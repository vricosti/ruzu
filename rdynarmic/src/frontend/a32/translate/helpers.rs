use crate::frontend::a32::types::ShiftType;
use crate::ir::a32_emitter::A32IREmitter;
use crate::ir::value::Value;

/// Apply an immediate shift to a register value, returning (result, carry_out).
pub fn emit_imm_shift(
    ir: &mut A32IREmitter,
    value: Value,
    shift_type: ShiftType,
    imm5: u32,
    carry_in: Value,
) -> (Value, Value) {
    match shift_type {
        ShiftType::LSL => {
            if imm5 == 0 {
                (value, carry_in)
            } else {
                let result =
                    ir.ir()
                        .logical_shift_left_32(value, Value::ImmU8(imm5 as u8), carry_in);
                let carry = ir.ir().get_carry_from_op(result);
                (result, carry)
            }
        }
        ShiftType::LSR => {
            let shift = if imm5 == 0 { 32 } else { imm5 };
            let result = ir
                .ir()
                .logical_shift_right_32(value, Value::ImmU8(shift as u8), carry_in);
            let carry = ir.ir().get_carry_from_op(result);
            (result, carry)
        }
        ShiftType::ASR => {
            let shift = if imm5 == 0 { 32 } else { imm5 };
            let result =
                ir.ir()
                    .arithmetic_shift_right_32(value, Value::ImmU8(shift as u8), carry_in);
            let carry = ir.ir().get_carry_from_op(result);
            (result, carry)
        }
        ShiftType::ROR => {
            if imm5 == 0 {
                // RRX: rotate right extended
                let result = ir.ir().rotate_right_extended(value, carry_in);
                let carry = ir.ir().get_carry_from_op(result);
                (result, carry)
            } else {
                let result = ir
                    .ir()
                    .rotate_right_32(value, Value::ImmU8(imm5 as u8), carry_in);
                let carry = ir.ir().get_carry_from_op(result);
                (result, carry)
            }
        }
    }
}

/// Apply a register-specified shift to a value, returning (result, carry_out).
pub fn emit_reg_shift(
    ir: &mut A32IREmitter,
    value: Value,
    shift_type: ShiftType,
    amount: Value,
    carry_in: Value,
) -> (Value, Value) {
    let result = match shift_type {
        ShiftType::LSL => ir.ir().logical_shift_left_32(value, amount, carry_in),
        ShiftType::LSR => ir.ir().logical_shift_right_32(value, amount, carry_in),
        ShiftType::ASR => ir.ir().arithmetic_shift_right_32(value, amount, carry_in),
        ShiftType::ROR => ir.ir().rotate_right_32(value, amount, carry_in),
    };
    let carry = ir.ir().get_carry_from_op(result);
    (result, carry)
}

/// Compute load/store address with P/U/W flags.
/// P = pre-index, U = add, W = writeback.
pub fn get_address(
    ir: &mut A32IREmitter,
    p: bool,
    u: bool,
    w: bool,
    base_reg: crate::frontend::a32::types::Reg,
    offset: Value,
) -> Value {
    let base = ir.get_register(base_reg);
    let carry = ir.ir().imm1(false);

    let offset_addr = if u {
        ir.ir().add_32(base, offset, carry)
    } else {
        ir.ir().sub_32(base, offset, Value::ImmU1(true))
    };

    let address = if p { offset_addr } else { base };

    // Writeback: update base register
    let wback = !p || w;
    if wback {
        ir.set_register(base_reg, offset_addr);
    }

    address
}
