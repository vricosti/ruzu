use crate::frontend::a32::decoder::DecodedArm;
use crate::frontend::a32::types::Reg;
use crate::ir::a32_emitter::A32IREmitter;

/// ARM SDIV - signed divide.
pub fn arm_sdiv(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rd = Reg::from_u32((inst.raw >> 16) & 0xF);
    let rn = inst.rm(); // Rm is bits [3:0]
    let rm = Reg::from_u32((inst.raw >> 8) & 0xF);

    let rn_val = ir.get_register(rn);
    let rm_val = ir.get_register(rm);
    let result = ir.ir().signed_div_32(rn_val, rm_val);
    ir.set_register(rd, result);
    true
}

/// ARM UDIV - unsigned divide.
pub fn arm_udiv(ir: &mut A32IREmitter, inst: &DecodedArm) -> bool {
    let rd = Reg::from_u32((inst.raw >> 16) & 0xF);
    let rn = inst.rm(); // Rm is bits [3:0]
    let rm = Reg::from_u32((inst.raw >> 8) & 0xF);

    let rn_val = ir.get_register(rn);
    let rm_val = ir.get_register(rm);
    let result = ir.ir().unsigned_div_32(rn_val, rm_val);
    ir.set_register(rd, result);
    true
}
