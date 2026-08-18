use crate::frontend::a32::decoder::DecodedArm;
use crate::ir::a32_emitter::A32IREmitter;
use crate::ir::value::Value;

/// ARM CLZ - count leading zeros.
pub fn arm_clz(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rd = inst.rd();
    let rm = inst.rm();

    let rm_val = ir.get_register(rm);
    let result = ir.ir().count_leading_zeros_32(rm_val);
    ir.set_register(rd, result);
    true
}

/// ARM MOVW - move 16-bit immediate to bottom half.
pub fn arm_movw(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rd = inst.rd();
    // imm16 = imm4:imm12
    let imm4 = (inst.raw >> 16) & 0xF;
    let imm12 = inst.imm12();
    let imm16 = (imm4 << 12) | imm12;

    ir.set_register(rd, Value::ImmU32(imm16));
    true
}

/// ARM MOVT - move 16-bit immediate to top half.
pub fn arm_movt(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rd = inst.rd();
    let imm4 = (inst.raw >> 16) & 0xF;
    let imm12 = inst.imm12();
    let imm16 = (imm4 << 12) | imm12;

    let rd_val = ir.get_register(rd);
    let masked = ir.ir().and_32(rd_val, Value::ImmU32(0x0000_FFFF));
    let upper = Value::ImmU32(imm16 << 16);
    let result = ir.ir().or_32(masked, upper);
    ir.set_register(rd, result);
    true
}

/// ARM BFC - bit field clear.
pub fn arm_bfc(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rd = inst.rd();
    let lsb = (inst.raw >> 7) & 0x1F;
    let msb = (inst.raw >> 16) & 0x1F;
    let width = msb - lsb + 1;

    let rd_val = ir.get_register(rd);
    let mask = !(((1u32 << width) - 1) << lsb);
    let result = ir.ir().and_32(rd_val, Value::ImmU32(mask));
    ir.set_register(rd, result);
    true
}

/// ARM BFI - bit field insert.
pub fn arm_bfi(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rd = inst.rd();
    let rn = inst.rm();
    let lsb = (inst.raw >> 7) & 0x1F;
    let msb = (inst.raw >> 16) & 0x1F;
    let width = msb - lsb + 1;

    let rd_val = ir.get_register(rd);
    let rn_val = ir.get_register(rn);

    let field_mask = ((1u32 << width) - 1) << lsb;
    let src_mask = (1u32 << width) - 1;

    // Extract source bits, shift to position, insert into dest
    let src_bits = ir.ir().and_32(rn_val, Value::ImmU32(src_mask));
    let shifted = if lsb != 0 {
        ir.ir()
            .logical_shift_left_32(src_bits, Value::ImmU8(lsb as u8), Value::ImmU1(false))
    } else {
        src_bits
    };
    let cleared = ir.ir().and_32(rd_val, Value::ImmU32(!field_mask));
    let result = ir.ir().or_32(cleared, shifted);
    ir.set_register(rd, result);
    true
}

/// ARM SBFX - signed bit field extract.
pub fn arm_sbfx(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rd = inst.rd();
    let rn = inst.rm();
    let lsb = (inst.raw >> 7) & 0x1F;
    let widthm1 = (inst.raw >> 16) & 0x1F;
    let width = widthm1 + 1;

    let rn_val = ir.get_register(rn);

    // Shift right to get field at bit 0, then sign-extend
    let shifted = if lsb != 0 {
        ir.ir()
            .logical_shift_right_32(rn_val, Value::ImmU8(lsb as u8), Value::ImmU1(false))
    } else {
        rn_val
    };

    // Sign-extend from width bits
    let shift_amount = 32 - width;
    let shl = ir.ir().logical_shift_left_32(
        shifted,
        Value::ImmU8(shift_amount as u8),
        Value::ImmU1(false),
    );
    let result = ir.ir().arithmetic_shift_right_32(
        shl,
        Value::ImmU8(shift_amount as u8),
        Value::ImmU1(false),
    );
    ir.set_register(rd, result);
    true
}

/// ARM UBFX - unsigned bit field extract.
pub fn arm_ubfx(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rd = inst.rd();
    let rn = inst.rm();
    let lsb = (inst.raw >> 7) & 0x1F;
    let widthm1 = (inst.raw >> 16) & 0x1F;
    let width = widthm1 + 1;

    let rn_val = ir.get_register(rn);

    let shifted = if lsb != 0 {
        ir.ir()
            .logical_shift_right_32(rn_val, Value::ImmU8(lsb as u8), Value::ImmU1(false))
    } else {
        rn_val
    };

    let mask = if width >= 32 {
        u32::MAX
    } else {
        (1u32 << width) - 1
    };
    let result = ir.ir().and_32(shifted, Value::ImmU32(mask));
    ir.set_register(rd, result);
    true
}
