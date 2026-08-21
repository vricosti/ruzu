use crate::backend::x64::emit_context::EmitContext;
use crate::backend::x64::host_feature::HostFeature;
use crate::backend::x64::hostloc::HostLoc;
use crate::backend::x64::reg_alloc::RegAlloc;
use crate::ir::inst::Inst;
use crate::ir::value::InstRef;

pub fn emit_sha256_hash(ctx: &EmitContext, ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let part1 = args[3].get_immediate_u1();
    assert!(ctx.has_host_feature(HostFeature::SHA));

    let x = ra.use_scratch_xmm(&mut args[0]);
    let y = ra.use_scratch_xmm(&mut args[1]);
    let w = ra.use_xmm(&mut args[2]);

    let xmm0 = ra.scratch_xmm_at(HostLoc::Xmm(0));
    ra.asm.movaps(xmm0, y).unwrap();
    ra.asm.shufps(xmm0, x, 0b1011_1011).unwrap();
    ra.asm.shufps(y, x, 0b0001_0001).unwrap();
    ra.asm.movaps(x, xmm0).unwrap();

    ra.asm.movaps(xmm0, w).unwrap();
    ra.asm.sha256rnds2(x, y).unwrap();
    ra.asm.punpckhqdq(xmm0, xmm0).unwrap();
    ra.asm.sha256rnds2(y, x).unwrap();
    ra.asm
        .shufps(y, x, if part1 { 0b1011_1011 } else { 0b0001_0001 })
        .unwrap();
    ra.release(xmm0);
    ra.define_value(inst_ref, y);
}

pub fn emit_sha256_message_schedule_0(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    assert!(ctx.has_host_feature(HostFeature::SHA));
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let x = ra.use_scratch_xmm(&mut args[0]);
    let y = ra.use_xmm(&mut args[1]);
    ra.asm.sha256msg1(x, y).unwrap();
    ra.define_value(inst_ref, x);
}

pub fn emit_sha256_message_schedule_1(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    assert!(ctx.has_host_feature(HostFeature::SHA));
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let x = ra.use_scratch_xmm(&mut args[0]);
    let y = ra.use_xmm(&mut args[1]);
    let z = ra.use_xmm(&mut args[2]);
    let xmm0 = ra.scratch_xmm_at(HostLoc::Xmm(0));
    ra.asm.movaps(xmm0, z).unwrap();
    ra.asm.palignr(xmm0, y, 4).unwrap();
    ra.asm.paddd(x, xmm0).unwrap();
    ra.asm.sha256msg2(x, z).unwrap();
    ra.release(xmm0);
    ra.define_value(inst_ref, x);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn emitter_signatures_match_the_sha_owner() {
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_sha256_hash;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_sha256_message_schedule_0;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_sha256_message_schedule_1;
    }
}
