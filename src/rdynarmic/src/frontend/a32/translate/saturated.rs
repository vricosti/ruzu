use crate::frontend::a32::decoder::DecodedArm;
use crate::ir::a32_emitter::A32IREmitter;
use crate::ir::value::Value;

/// ARM SSAT - signed saturate.
pub fn arm_ssat(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rd = inst.rd();
    let rn = inst.rm(); // Rn is the source in SSAT encoding
    let sat_imm = ((inst.raw >> 16) & 0x1F) + 1; // saturate_to = sat_imm + 1

    let rn_val = ir.get_register(rn);
    // Apply shift if specified
    let shift_type = (inst.raw >> 5) & 3;
    let imm5 = (inst.raw >> 7) & 0x1F;

    let shifted = if shift_type == 0 && imm5 != 0 {
        // LSL
        ir.ir()
            .logical_shift_left_32(rn_val, Value::ImmU8(imm5 as u8), Value::ImmU1(false))
    } else if shift_type == 2 {
        // ASR
        let amount = if imm5 == 0 { 32 } else { imm5 };
        ir.ir()
            .arithmetic_shift_right_32(rn_val, Value::ImmU8(amount as u8), Value::ImmU1(false))
    } else {
        rn_val
    };

    // For now, just write the shifted value - proper saturation requires
    // comparison and clamping which we'll implement with the backend
    ir.set_register(rd, shifted);
    true
}

/// ARM USAT - unsigned saturate.
pub fn arm_usat(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rd = inst.rd();
    let rn = inst.rm();
    let sat_imm = (inst.raw >> 16) & 0x1F;

    let rn_val = ir.get_register(rn);
    let shift_type = (inst.raw >> 5) & 3;
    let imm5 = (inst.raw >> 7) & 0x1F;

    let shifted = if shift_type == 0 && imm5 != 0 {
        ir.ir()
            .logical_shift_left_32(rn_val, Value::ImmU8(imm5 as u8), Value::ImmU1(false))
    } else if shift_type == 2 {
        let amount = if imm5 == 0 { 32 } else { imm5 };
        ir.ir()
            .arithmetic_shift_right_32(rn_val, Value::ImmU8(amount as u8), Value::ImmU1(false))
    } else {
        rn_val
    };

    ir.set_register(rd, shifted);
    true
}
