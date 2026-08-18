use super::helpers::{emit_imm_shift, emit_reg_shift};
use crate::frontend::a32::decoder::{arm_expand_imm, arm_expand_imm_c, ArmInstId, DecodedArm};
use crate::frontend::a32::types::Reg;
use crate::ir::a32_emitter::A32IREmitter;
use crate::ir::terminal::Terminal;
use crate::ir::value::Value;

/// Instruction category — determines which operands are needed.
/// Matches upstream's per-function approach where each instruction
/// reads only the operands it requires.
#[derive(Clone, Copy, PartialEq)]
enum DpCategory {
    /// AND, EOR, SUB, RSB, ADD, ORR, BIC: result = f(Rn, operand2), writes Rd
    TwoOp,
    /// ADC, SBC, RSC: result = f(Rn, operand2, C), writes Rd
    TwoOpCarry,
    /// MOV: result = operand2, writes Rd (does NOT read Rn)
    MovOp,
    /// MVN: result = NOT(operand2), writes Rd (does NOT read Rn)
    MvnOp,
    /// TST, TEQ: result = f(Rn, operand2), sets flags, does NOT write Rd
    TestOp,
    /// CMP, CMN: result = f(Rn, operand2), sets flags (NZCV), does NOT write Rd
    CompareOp,
}

/// Operation type for the ALU computation.
#[derive(Clone, Copy, PartialEq)]
enum DpOp {
    And,
    Eor,
    Sub,
    Rsb,
    Add,
    Adc,
    Sbc,
    Rsc,
    Tst,
    Teq,
    Cmp,
    Cmn,
    Orr,
    Mov,
    Bic,
    Mvn,
}

fn classify(id: ArmInstId) -> Option<(DpOp, DpCategory)> {
    use ArmInstId::*;
    match id {
        AND_imm | AND_reg | AND_rsr => Some((DpOp::And, DpCategory::TwoOp)),
        EOR_imm | EOR_reg | EOR_rsr => Some((DpOp::Eor, DpCategory::TwoOp)),
        SUB_imm | SUB_reg | SUB_rsr => Some((DpOp::Sub, DpCategory::TwoOp)),
        RSB_imm | RSB_reg | RSB_rsr => Some((DpOp::Rsb, DpCategory::TwoOp)),
        ADD_imm | ADD_reg | ADD_rsr => Some((DpOp::Add, DpCategory::TwoOp)),
        ORR_imm | ORR_reg | ORR_rsr => Some((DpOp::Orr, DpCategory::TwoOp)),
        BIC_imm | BIC_reg | BIC_rsr => Some((DpOp::Bic, DpCategory::TwoOp)),
        ADC_imm | ADC_reg | ADC_rsr => Some((DpOp::Adc, DpCategory::TwoOpCarry)),
        SBC_imm | SBC_reg | SBC_rsr => Some((DpOp::Sbc, DpCategory::TwoOpCarry)),
        RSC_imm | RSC_reg | RSC_rsr => Some((DpOp::Rsc, DpCategory::TwoOpCarry)),
        MOV_imm | MOV_reg | MOV_rsr => Some((DpOp::Mov, DpCategory::MovOp)),
        MVN_imm | MVN_reg | MVN_rsr => Some((DpOp::Mvn, DpCategory::MvnOp)),
        TST_imm | TST_reg | TST_rsr => Some((DpOp::Tst, DpCategory::TestOp)),
        TEQ_imm | TEQ_reg | TEQ_rsr => Some((DpOp::Teq, DpCategory::TestOp)),
        CMP_imm | CMP_reg | CMP_rsr => Some((DpOp::Cmp, DpCategory::CompareOp)),
        CMN_imm | CMN_reg | CMN_rsr => Some((DpOp::Cmn, DpCategory::CompareOp)),
        _ => None,
    }
}

/// ARM data processing - immediate operand.
/// Matches upstream's per-instruction functions: only reads operands needed.
pub fn arm_dp_imm(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rd = inst.rd();
    let rn = inst.rn();
    let s = inst.s_flag();
    let rotate = inst.rotate();
    let imm8 = inst.imm8();

    let Some((op, cat)) = classify(inst.id) else {
        return true;
    };

    // Upstream: carry_in is needed for:
    // 1. Logic ops with S=1 — always, because SetCpsrNZC needs the C value.
    //    When rotate!=0, C comes from the barrel shifter rotation.
    //    When rotate==0, C is the old carry (carry_in from CPSR).
    // 2. ADC/SBC/RSC — always need carry for the computation.
    let is_logic_cat = match cat {
        DpCategory::TwoOp => !matches!(op, DpOp::Add | DpOp::Sub | DpOp::Rsb),
        DpCategory::MovOp | DpCategory::MvnOp | DpCategory::TestOp => true,
        _ => false,
    };
    let needs_carry = (s && is_logic_cat) || matches!(cat, DpCategory::TwoOpCarry);
    let carry_in = if needs_carry {
        ir.get_c_flag()
    } else {
        Value::ImmU1(false) // dummy, won't be used
    };

    let (imm_val, carry_bit) = arm_expand_imm_c(rotate, imm8, false);
    let carry = if rotate == 0 {
        carry_in
    } else {
        ir.ir().imm1(carry_bit)
    };
    let operand2 = Value::ImmU32(imm_val);

    // Upstream: MOV/MVN don't read Rn. TST/TEQ/CMP/CMN read Rn but not Rd.
    let operand1 = if matches!(cat, DpCategory::MovOp | DpCategory::MvnOp) {
        Value::ImmU32(0) // not used
    } else {
        ir.get_register(rn)
    };

    dp_emit(ir, op, cat, rd, s, operand1, operand2, carry)
}

/// ARM data processing - register operand (with immediate shift).
pub fn arm_dp_reg(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rd = inst.rd();
    let rn = inst.rn();
    let rm = inst.rm();
    let s = inst.s_flag();
    let shift_type = inst.shift_type();
    let imm5 = inst.imm5();

    let Some((op, cat)) = classify(inst.id) else {
        return true;
    };

    // Upstream: GetCFlag is called for the barrel shifter.
    // For MOV_reg with LSL#0 and S=0, upstream still calls GetCFlag
    // because EmitImmShift always takes carry_in. Match upstream.
    let carry_in = ir.get_c_flag();
    let rm_val = ir.get_register(rm);
    let (shifted, carry) = emit_imm_shift(ir, rm_val, shift_type, imm5, carry_in);

    // Only read Rn when the instruction actually uses it.
    // Upstream: arm_MOV_reg and arm_MVN_reg do NOT call ir.GetRegister(n).
    let operand1 = if matches!(cat, DpCategory::MovOp | DpCategory::MvnOp) {
        Value::ImmU32(0)
    } else {
        ir.get_register(rn)
    };

    dp_emit(ir, op, cat, rd, s, operand1, shifted, carry)
}

/// ARM data processing - register-shifted register operand.
pub fn arm_dp_rsr(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rd = inst.rd();
    let rn = inst.rn();
    let rm = inst.rm();
    let rs = inst.rs();
    let s = inst.s_flag();
    let shift_type = inst.shift_type();

    let Some((op, cat)) = classify(inst.id) else {
        return true;
    };

    let carry_in = ir.get_c_flag();
    let rm_val = ir.get_register(rm);
    let rs_val = ir.get_register(rs);
    let rs_amount = ir.ir().least_significant_byte(rs_val);
    let (shifted, carry) = emit_reg_shift(ir, rm_val, shift_type, rs_amount, carry_in);

    let operand1 = if matches!(cat, DpCategory::MovOp | DpCategory::MvnOp) {
        Value::ImmU32(0)
    } else {
        ir.get_register(rn)
    };

    dp_emit(ir, op, cat, rd, s, operand1, shifted, carry)
}

/// Emit the ALU operation, flags update, and result write.
/// Matches upstream's per-instruction handling but factored for reuse.
fn dp_emit(
    ir: &mut A32IREmitter,
    op: DpOp,
    cat: DpCategory,
    rd: Reg,
    s: bool,
    operand1: Value,
    operand2: Value,
    carry: Value,
) -> bool {
    // Compute the result. Matches upstream's per-instruction logic.
    let result = match op {
        DpOp::And | DpOp::Tst => ir.ir().and_32(operand1, operand2),
        DpOp::Eor | DpOp::Teq => ir.ir().eor_32(operand1, operand2),
        DpOp::Sub | DpOp::Cmp => ir.ir().sub_32(operand1, operand2, Value::ImmU1(true)),
        DpOp::Rsb => ir.ir().sub_32(operand2, operand1, Value::ImmU1(true)),
        DpOp::Add | DpOp::Cmn => ir.ir().add_32(operand1, operand2, Value::ImmU1(false)),
        DpOp::Adc => {
            let c = ir.get_c_flag();
            ir.ir().add_32(operand1, operand2, c)
        }
        DpOp::Sbc => {
            let c = ir.get_c_flag();
            ir.ir().sub_32(operand1, operand2, c)
        }
        DpOp::Rsc => {
            let c = ir.get_c_flag();
            ir.ir().sub_32(operand2, operand1, c)
        }
        DpOp::Orr => ir.ir().or_32(operand1, operand2),
        DpOp::Mov => operand2,
        DpOp::Bic => {
            let not_op2 = ir.ir().not_32(operand2);
            ir.ir().and_32(operand1, not_op2)
        }
        DpOp::Mvn => ir.ir().not_32(operand2),
    };

    // Update flags. Upstream: arithmetic ops set NZCV, logic ops set NZC.
    let update_flags = s || matches!(cat, DpCategory::TestOp | DpCategory::CompareOp);
    if update_flags {
        match cat {
            DpCategory::CompareOp | DpCategory::TwoOpCarry => {
                // Arithmetic: set all NZCV from the result.
                let nzcv = ir.ir().get_nzcv_from_op(result);
                ir.set_cpsr_nzcv(nzcv);
            }
            DpCategory::TwoOp if matches!(op, DpOp::Add | DpOp::Sub | DpOp::Rsb) => {
                let nzcv = ir.ir().get_nzcv_from_op(result);
                ir.set_cpsr_nzcv(nzcv);
            }
            _ => {
                // Logic ops: N,Z from result, C from barrel shifter, V unchanged.
                // For MOV/MVN the result is just the shifted value — emit OR with 0
                // to force flag setting (upstream uses the same pattern via
                // SetNZ on the result and SetC from the shifter carry).
                let flags_value = match op {
                    DpOp::Mov | DpOp::Mvn => ir.ir().or_32(result, Value::ImmU32(0)),
                    _ => result,
                };
                let nz = ir.ir().get_nz_from_op(flags_value);
                ir.set_cpsr_nzc(nz, carry);
            }
        }
    }

    // Write result to Rd (test/compare instructions don't write).
    let writes_rd = matches!(
        cat,
        DpCategory::TwoOp | DpCategory::TwoOpCarry | DpCategory::MovOp | DpCategory::MvnOp
    );
    if writes_rd {
        if rd == Reg::R15 {
            ir.alu_write_pc(result);
            ir.set_term(Terminal::ReturnToDispatch);
            return false;
        }
        ir.set_register(rd, result);
    }

    true
}
