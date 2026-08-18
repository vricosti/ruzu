use crate::frontend::a32::decoder::DecodedArm;
use crate::ir::a32_emitter::A32IREmitter;
use crate::ir::value::Value;

/// ARM PKHBT - pack halfword bottom top.
pub fn arm_pkhbt(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rd = inst.rd();
    let rn = inst.rn();
    let rm = inst.rm();
    let imm5 = inst.imm5();

    let rn_val = ir.get_register(rn);
    let rm_val = ir.get_register(rm);

    let shifted = if imm5 != 0 {
        ir.ir()
            .logical_shift_left_32(rm_val, Value::ImmU8(imm5 as u8), Value::ImmU1(false))
    } else {
        rm_val
    };

    // Bottom halfword from Rn, top halfword from shifted Rm
    let lo = ir.ir().and_32(rn_val, Value::ImmU32(0xFFFF));
    let hi = ir.ir().and_32(shifted, Value::ImmU32(0xFFFF_0000));
    let result = ir.ir().or_32(lo, hi);

    ir.set_register(rd, result);
    true
}

/// ARM PKHTB - pack halfword top bottom.
pub fn arm_pkhtb(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rd = inst.rd();
    let rn = inst.rn();
    let rm = inst.rm();
    let imm5 = inst.imm5();

    let rn_val = ir.get_register(rn);
    let rm_val = ir.get_register(rm);

    let shift_amount = if imm5 == 0 { 32 } else { imm5 };
    let shifted = ir.ir().arithmetic_shift_right_32(
        rm_val,
        Value::ImmU8(shift_amount as u8),
        Value::ImmU1(false),
    );

    // Top halfword from Rn, bottom halfword from shifted Rm
    let hi = ir.ir().and_32(rn_val, Value::ImmU32(0xFFFF_0000));
    let lo = ir.ir().and_32(shifted, Value::ImmU32(0xFFFF));
    let result = ir.ir().or_32(hi, lo);

    ir.set_register(rd, result);
    true
}
