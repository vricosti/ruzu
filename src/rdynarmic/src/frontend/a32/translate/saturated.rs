use super::helpers::{emit_imm_shift, most_significant_half, pack_2x16_to_1x32};
use crate::frontend::a32::decoder::DecodedArm;
use crate::frontend::a32::types::{Reg, ShiftType};
use crate::ir::a32_emitter::A32IREmitter;

/// ARM SSAT - signed saturate.
pub fn arm_ssat(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let d = inst.rd();
    let n = inst.rm();
    if d == Reg::PC || n == Reg::PC {
        return super::unpredictable_instruction(ir);
    }

    let saturate_to = (((inst.raw >> 16) & 0x1f) + 1) as usize;
    let shift = if inst.raw & (1 << 6) == 0 {
        ShiftType::LSL
    } else {
        ShiftType::ASR
    };
    let n_value = ir.get_register(n);
    let carry = ir.get_c_flag();
    let (operand, _) = emit_imm_shift(ir, n_value, shift, inst.imm5(), carry);
    let result = ir.ir().signed_saturation(operand, saturate_to);

    ir.set_register(d, result.result);
    ir.or_q_flag(result.overflow);
    true
}

/// ARM SSAT16 - signed saturate two halfwords.
pub fn arm_ssat16(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let d = inst.rd();
    let n = inst.rm();
    if d == Reg::PC || n == Reg::PC {
        return super::unpredictable_instruction(ir);
    }

    let saturate_to = (((inst.raw >> 16) & 0xf) + 1) as usize;
    let n_lo = ir.get_register(n);
    let lo_half = ir.ir().least_significant_half(n_lo);
    let lo_operand = ir.ir().sign_extend_half_to_word(lo_half);
    let n_hi = ir.get_register(n);
    let hi_half = most_significant_half(ir, n_hi);
    let hi_operand = ir.ir().sign_extend_half_to_word(hi_half);
    let lo_result = ir.ir().signed_saturation(lo_operand, saturate_to);
    let hi_result = ir.ir().signed_saturation(hi_operand, saturate_to);

    let result = pack_2x16_to_1x32(ir, lo_result.result, hi_result.result);
    ir.set_register(d, result);
    ir.or_q_flag(lo_result.overflow);
    ir.or_q_flag(hi_result.overflow);
    true
}

/// ARM USAT - unsigned saturate.
pub fn arm_usat(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let d = inst.rd();
    let n = inst.rm();
    if d == Reg::PC || n == Reg::PC {
        return super::unpredictable_instruction(ir);
    }

    let saturate_to = ((inst.raw >> 16) & 0x1f) as usize;
    let shift = if inst.raw & (1 << 6) == 0 {
        ShiftType::LSL
    } else {
        ShiftType::ASR
    };
    let n_value = ir.get_register(n);
    let carry = ir.get_c_flag();
    let (operand, _) = emit_imm_shift(ir, n_value, shift, inst.imm5(), carry);
    let result = ir.ir().unsigned_saturation(operand, saturate_to);

    ir.set_register(d, result.result);
    ir.or_q_flag(result.overflow);
    true
}

/// ARM USAT16 - unsigned saturate two signed halfword inputs.
pub fn arm_usat16(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let d = inst.rd();
    let n = inst.rm();
    if d == Reg::PC || n == Reg::PC {
        return super::unpredictable_instruction(ir);
    }

    let saturate_to = ((inst.raw >> 16) & 0xf) as usize;
    let n_lo = ir.get_register(n);
    let lo_half = ir.ir().least_significant_half(n_lo);
    let lo_operand = ir.ir().sign_extend_half_to_word(lo_half);
    let n_hi = ir.get_register(n);
    let hi_half = most_significant_half(ir, n_hi);
    let hi_operand = ir.ir().sign_extend_half_to_word(hi_half);
    let lo_result = ir.ir().unsigned_saturation(lo_operand, saturate_to);
    let hi_result = ir.ir().unsigned_saturation(hi_operand, saturate_to);

    let result = pack_2x16_to_1x32(ir, lo_result.result, hi_result.result);
    ir.set_register(d, result);
    ir.or_q_flag(lo_result.overflow);
    ir.or_q_flag(hi_result.overflow);
    true
}

/// ARM QADD - signed saturated add.
pub fn arm_qadd(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let n = inst.rn();
    let d = inst.rd();
    let m = inst.rm();
    if d == Reg::PC || n == Reg::PC || m == Reg::PC {
        return super::unpredictable_instruction(ir);
    }

    let a = ir.get_register(m);
    let b = ir.get_register(n);
    let result = ir.ir().signed_saturated_add_with_flag(a, b);

    ir.set_register(d, result.result);
    ir.or_q_flag(result.overflow);
    true
}

/// ARM QSUB - signed saturated subtract.
pub fn arm_qsub(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let n = inst.rn();
    let d = inst.rd();
    let m = inst.rm();
    if d == Reg::PC || n == Reg::PC || m == Reg::PC {
        return super::unpredictable_instruction(ir);
    }

    let a = ir.get_register(m);
    let b = ir.get_register(n);
    let result = ir.ir().signed_saturated_sub_with_flag(a, b);

    ir.set_register(d, result.result);
    ir.or_q_flag(result.overflow);
    true
}

/// ARM QDADD - saturating double followed by saturating add.
pub fn arm_qdadd(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let n = inst.rn();
    let d = inst.rd();
    let m = inst.rm();
    if d == Reg::PC || n == Reg::PC || m == Reg::PC {
        return super::unpredictable_instruction(ir);
    }

    let a = ir.get_register(m);
    let b = ir.get_register(n);
    let doubled = ir.ir().signed_saturated_add_with_flag(b, b);
    ir.or_q_flag(doubled.overflow);

    let result = ir.ir().signed_saturated_add_with_flag(a, doubled.result);
    ir.set_register(d, result.result);
    ir.or_q_flag(result.overflow);
    true
}

/// ARM QDSUB - saturating double followed by saturating subtract.
pub fn arm_qdsub(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let n = inst.rn();
    let d = inst.rd();
    let m = inst.rm();
    if d == Reg::PC || n == Reg::PC || m == Reg::PC {
        return super::unpredictable_instruction(ir);
    }

    let a = ir.get_register(m);
    let b = ir.get_register(n);
    let doubled = ir.ir().signed_saturated_add_with_flag(b, b);
    ir.or_q_flag(doubled.overflow);

    let result = ir.ir().signed_saturated_sub_with_flag(a, doubled.result);
    ir.set_register(d, result.result);
    ir.or_q_flag(result.overflow);
    true
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::a32::decoder::ArmInstId;
    use crate::ir::block::Block;
    use crate::ir::location::LocationDescriptor;
    use crate::ir::opcode::Opcode;

    fn translate(
        raw: u32,
        id: ArmInstId,
        translate: fn(&mut A32IREmitter, &DecodedArm) -> bool,
    ) -> Block {
        let mut block = Block::new(LocationDescriptor(0));
        {
            let mut ir = A32IREmitter::new(&mut block);
            assert!(translate(&mut ir, &DecodedArm { raw, id }));
        }
        block
    }

    #[test]
    fn ssat_and_usat_emit_saturation_result_and_q_flag() {
        let ssat = translate(0xe6a7_2051, ArmInstId::SSAT, arm_ssat);
        assert!(ssat
            .instructions
            .iter()
            .any(|inst| inst.opcode == Opcode::SignedSaturation));
        assert_eq!(
            ssat.instructions.last().expect("SSAT instruction").opcode,
            Opcode::A32OrQFlag
        );

        let usat = translate(0xe6e8_2051, ArmInstId::USAT, arm_usat);
        assert!(usat
            .instructions
            .iter()
            .any(|inst| inst.opcode == Opcode::UnsignedSaturation));
        assert_eq!(
            usat.instructions.last().expect("USAT instruction").opcode,
            Opcode::A32OrQFlag
        );
    }

    #[test]
    fn halfword_saturation_updates_q_for_both_lanes() {
        let block = translate(0xe6a7_2f31, ArmInstId::SSAT16, arm_ssat16);
        assert_eq!(
            block
                .instructions
                .iter()
                .filter(|inst| inst.opcode == Opcode::SignedSaturation)
                .count(),
            2
        );
        assert_eq!(
            block
                .instructions
                .iter()
                .filter(|inst| inst.opcode == Opcode::A32OrQFlag)
                .count(),
            2
        );
    }

    #[test]
    fn qdsub_preserves_double_then_subtract_order() {
        let block = translate(0xe161_2051, ArmInstId::QDSUB, arm_qdsub);
        let saturated: Vec<_> = block
            .instructions
            .iter()
            .filter(|inst| {
                matches!(
                    inst.opcode,
                    Opcode::SignedSaturatedAddWithFlag32 | Opcode::SignedSaturatedSubWithFlag32
                )
            })
            .map(|inst| inst.opcode)
            .collect();
        assert_eq!(
            saturated,
            vec![
                Opcode::SignedSaturatedAddWithFlag32,
                Opcode::SignedSaturatedSubWithFlag32
            ]
        );
    }

    #[test]
    fn pc_operands_are_unpredictable_before_register_reads() {
        let location = crate::ir::location::A32LocationDescriptor::at(0x1000);
        let mut block = Block::new(location.to_location());
        let mut ir = A32IREmitter::with_location(&mut block, location);
        assert!(!arm_qadd(
            &mut ir,
            &DecodedArm {
                raw: 0xe100_f055,
                id: ArmInstId::QADD,
            },
        ));
        assert!(!block
            .instructions
            .iter()
            .any(|inst| inst.opcode == Opcode::A32GetRegister));
    }
}
