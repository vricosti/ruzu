use crate::frontend::a32::decoder::DecodedArm;
use crate::ir::a32_emitter::A32IREmitter;
use crate::ir::value::Value;

fn rotate_extension_operand(ir: &mut A32IREmitter, inst: &DecodedArm) -> Value {
    let rm_val = ir.get_register(inst.rm());
    let rotation = ((inst.raw >> 10) & 3) * 8;
    if rotation != 0 {
        ir.ir()
            .rotate_right_32(rm_val, Value::ImmU8(rotation as u8), Value::ImmU1(false))
    } else {
        rm_val
    }
}

fn pack_halfwords(ir: &mut A32IREmitter, lo: Value, hi: Value) -> Value {
    let lo = ir.ir().and_32(lo, Value::ImmU32(0xFFFF));
    let hi = ir
        .ir()
        .logical_shift_left_32(hi, Value::ImmU8(16), Value::ImmU1(false));
    ir.ir().or_32(lo, hi)
}

fn low_and_high_bytes(ir: &mut A32IREmitter, value: Value) -> (Value, Value) {
    let low = value;
    let high = ir
        .ir()
        .logical_shift_right_32(value, Value::ImmU8(16), Value::ImmU1(false));
    (low, high)
}

/// ARM SXTB - sign-extend byte.
pub fn arm_sxtb(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rd = inst.rd();
    let rotated = rotate_extension_operand(ir, inst);
    let result = ir.ir().sign_extend_byte_to_word(rotated);
    ir.set_register(rd, result);
    true
}

/// ARM SXTH - sign-extend halfword.
pub fn arm_sxth(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rd = inst.rd();
    let rotated = rotate_extension_operand(ir, inst);
    let result = ir.ir().sign_extend_half_to_word(rotated);
    ir.set_register(rd, result);
    true
}

/// ARM UXTB - zero-extend byte.
pub fn arm_uxtb(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rd = inst.rd();
    let rotated = rotate_extension_operand(ir, inst);
    let result = ir.ir().and_32(rotated, Value::ImmU32(0xFF));
    ir.set_register(rd, result);
    true
}

/// ARM UXTH - zero-extend halfword.
pub fn arm_uxth(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rd = inst.rd();
    let rotated = rotate_extension_operand(ir, inst);
    let result = ir.ir().and_32(rotated, Value::ImmU32(0xFFFF));
    ir.set_register(rd, result);
    true
}

/// ARM SXTAB - sign-extend byte and add.
pub fn arm_sxtab(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rd = inst.rd();
    let rn = inst.rn();

    let rn_val = ir.get_register(rn);
    let rotated = rotate_extension_operand(ir, inst);
    let extended = ir.ir().sign_extend_byte_to_word(rotated);
    let result = ir.ir().add_32(rn_val, extended, Value::ImmU1(false));
    ir.set_register(rd, result);
    true
}

/// ARM SXTAH - sign-extend halfword and add.
pub fn arm_sxtah(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rd = inst.rd();
    let rn = inst.rn();

    let rn_val = ir.get_register(rn);
    let rotated = rotate_extension_operand(ir, inst);
    let extended = ir.ir().sign_extend_half_to_word(rotated);
    let result = ir.ir().add_32(rn_val, extended, Value::ImmU1(false));
    ir.set_register(rd, result);
    true
}

/// ARM UXTAB - zero-extend byte and add.
pub fn arm_uxtab(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rd = inst.rd();
    let rn = inst.rn();

    let rn_val = ir.get_register(rn);
    let rotated = rotate_extension_operand(ir, inst);
    let masked = ir.ir().and_32(rotated, Value::ImmU32(0xFF));
    let result = ir.ir().add_32(rn_val, masked, Value::ImmU1(false));
    ir.set_register(rd, result);
    true
}

/// ARM UXTAH - zero-extend halfword and add.
pub fn arm_uxtah(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rd = inst.rd();
    let rn = inst.rn();

    let rn_val = ir.get_register(rn);
    let rotated = rotate_extension_operand(ir, inst);
    let masked = ir.ir().and_32(rotated, Value::ImmU32(0xFFFF));
    let result = ir.ir().add_32(rn_val, masked, Value::ImmU1(false));
    ir.set_register(rd, result);
    true
}

/// ARM SXTB16 - sign-extend two bytes into two halfwords.
pub fn arm_sxtb16(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rotated = rotate_extension_operand(ir, inst);
    let (low_byte, high_byte) = low_and_high_bytes(ir, rotated);
    let low = ir.ir().sign_extend_byte_to_word(low_byte);
    let high = ir.ir().sign_extend_byte_to_word(high_byte);
    let result = pack_halfwords(ir, low, high);
    ir.set_register(inst.rd(), result);
    true
}

/// ARM UXTB16 - zero-extend two bytes into two halfwords.
pub fn arm_uxtb16(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rotated = rotate_extension_operand(ir, inst);
    let (low_byte, high_byte) = low_and_high_bytes(ir, rotated);
    let low = ir.ir().and_32(low_byte, Value::ImmU32(0xFF));
    let high = ir.ir().and_32(high_byte, Value::ImmU32(0xFF));
    let result = pack_halfwords(ir, low, high);
    ir.set_register(inst.rd(), result);
    true
}

/// ARM SXTAB16 - sign-extend two bytes and add them to halfwords of Rn.
pub fn arm_sxtab16(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rn_val = ir.get_register(inst.rn());
    let rn_lo = ir.ir().and_32(rn_val, Value::ImmU32(0xFFFF));
    let rn_hi = ir
        .ir()
        .logical_shift_right_32(rn_val, Value::ImmU8(16), Value::ImmU1(false));
    let rotated = rotate_extension_operand(ir, inst);
    let (low_byte, high_byte) = low_and_high_bytes(ir, rotated);
    let ext_lo = ir.ir().sign_extend_byte_to_word(low_byte);
    let ext_hi = ir.ir().sign_extend_byte_to_word(high_byte);
    let res_lo = ir.ir().add_32(rn_lo, ext_lo, Value::ImmU1(false));
    let res_hi = ir.ir().add_32(rn_hi, ext_hi, Value::ImmU1(false));
    let result = pack_halfwords(ir, res_lo, res_hi);
    ir.set_register(inst.rd(), result);
    true
}

/// ARM UXTAB16 - zero-extend two bytes and add them to halfwords of Rn.
pub fn arm_uxtab16(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rn_val = ir.get_register(inst.rn());
    let rn_lo = ir.ir().and_32(rn_val, Value::ImmU32(0xFFFF));
    let rn_hi = ir
        .ir()
        .logical_shift_right_32(rn_val, Value::ImmU8(16), Value::ImmU1(false));
    let rotated = rotate_extension_operand(ir, inst);
    let (low_byte, high_byte) = low_and_high_bytes(ir, rotated);
    let ext_lo = ir.ir().and_32(low_byte, Value::ImmU32(0xFF));
    let ext_hi = ir.ir().and_32(high_byte, Value::ImmU32(0xFF));
    let res_lo = ir.ir().add_32(rn_lo, ext_lo, Value::ImmU1(false));
    let res_hi = ir.ir().add_32(rn_hi, ext_hi, Value::ImmU1(false));
    let result = pack_halfwords(ir, res_lo, res_hi);
    ir.set_register(inst.rd(), result);
    true
}
