use crate::frontend::a32::decoder::DecodedArm;
use crate::ir::a32_emitter::A32IREmitter;
use crate::ir::value::Value;

/// ARM REV - byte-reverse word.
pub fn arm_rev(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rd = inst.rd();
    let rm = inst.rm();

    let rm_val = ir.get_register(rm);
    let result = ir.ir().byte_reverse_word(rm_val);
    ir.set_register(rd, result);
    true
}

/// ARM REV16 - byte-reverse packed halfwords.
pub fn arm_rev16(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rd = inst.rd();
    let rm = inst.rm();

    let rm_val = ir.get_register(rm);
    // Swap bytes within each halfword:
    // [31:24] <-> [23:16], [15:8] <-> [7:0]
    let byte0 = ir.ir().and_32(rm_val, Value::ImmU32(0xFF));
    let byte1 = ir.ir().and_32(rm_val, Value::ImmU32(0xFF00));
    let byte2 = ir.ir().and_32(rm_val, Value::ImmU32(0xFF_0000));
    let byte3 = ir.ir().and_32(rm_val, Value::ImmU32(0xFF00_0000));

    let s0 = ir
        .ir()
        .logical_shift_left_32(byte0, Value::ImmU8(8), Value::ImmU1(false));
    let s1 = ir
        .ir()
        .logical_shift_right_32(byte1, Value::ImmU8(8), Value::ImmU1(false));
    let s2 = ir
        .ir()
        .logical_shift_left_32(byte2, Value::ImmU8(8), Value::ImmU1(false));
    let s3 = ir
        .ir()
        .logical_shift_right_32(byte3, Value::ImmU8(8), Value::ImmU1(false));

    let lo = ir.ir().or_32(s0, s1);
    let hi = ir.ir().or_32(s2, s3);
    let result = ir.ir().or_32(lo, hi);

    ir.set_register(rd, result);
    true
}

/// ARM REVSH - byte-reverse signed halfword.
pub fn arm_revsh(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rd = inst.rd();
    let rm = inst.rm();

    let rm_val = ir.get_register(rm);
    // Swap bottom two bytes, sign-extend from bit 15
    let byte0 = ir.ir().and_32(rm_val, Value::ImmU32(0xFF));
    let byte1 = ir.ir().and_32(rm_val, Value::ImmU32(0xFF00));

    let s0 = ir
        .ir()
        .logical_shift_left_32(byte0, Value::ImmU8(8), Value::ImmU1(false));
    let s1 = ir
        .ir()
        .logical_shift_right_32(byte1, Value::ImmU8(8), Value::ImmU1(false));
    let half = ir.ir().or_32(s0, s1);
    let result = ir.ir().sign_extend_half_to_word(half);

    ir.set_register(rd, result);
    true
}

/// ARM RBIT - reverse bits.
pub fn arm_rbit(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rd = inst.rd();
    let rm = inst.rm();

    let rm_val = ir.get_register(rm);
    // Use byte reverse + bit reverse within bytes
    // For simplicity, we'll do a byte reverse first, then reverse bits within each byte
    // Actually, we need a proper bit reversal. Use shifts and masks.
    // This is a well-known bit reversal pattern.
    let v = rm_val;

    // Swap adjacent bits
    let t1 = ir
        .ir()
        .logical_shift_right_32(v, Value::ImmU8(1), Value::ImmU1(false));
    let m1 = ir.ir().and_32(t1, Value::ImmU32(0x5555_5555));
    let t2 = ir.ir().and_32(v, Value::ImmU32(0x5555_5555));
    let t3 = ir
        .ir()
        .logical_shift_left_32(t2, Value::ImmU8(1), Value::ImmU1(false));
    let v1 = ir.ir().or_32(m1, t3);

    // Swap pairs
    let t4 = ir
        .ir()
        .logical_shift_right_32(v1, Value::ImmU8(2), Value::ImmU1(false));
    let m2 = ir.ir().and_32(t4, Value::ImmU32(0x3333_3333));
    let t5 = ir.ir().and_32(v1, Value::ImmU32(0x3333_3333));
    let t6 = ir
        .ir()
        .logical_shift_left_32(t5, Value::ImmU8(2), Value::ImmU1(false));
    let v2 = ir.ir().or_32(m2, t6);

    // Swap nibbles
    let t7 = ir
        .ir()
        .logical_shift_right_32(v2, Value::ImmU8(4), Value::ImmU1(false));
    let m3 = ir.ir().and_32(t7, Value::ImmU32(0x0F0F_0F0F));
    let t8 = ir.ir().and_32(v2, Value::ImmU32(0x0F0F_0F0F));
    let t9 = ir
        .ir()
        .logical_shift_left_32(t8, Value::ImmU8(4), Value::ImmU1(false));
    let v3 = ir.ir().or_32(m3, t9);

    // Byte reverse to finish
    let result = ir.ir().byte_reverse_word(v3);

    ir.set_register(rd, result);
    true
}
