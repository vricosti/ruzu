use crate::backend::x64::emit_context::EmitContext;
use crate::backend::x64::fp_helpers;
use crate::backend::x64::reg_alloc::RegAlloc;
use crate::ir::inst::Inst;
use crate::ir::value::InstRef;

// ---------------------------------------------------------------------------
// CRC32 Castagnoli (native x86 crc32 instruction)
// ---------------------------------------------------------------------------

fn emit_crc32_castagnoli(ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst, bitsize: usize) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let result = ra.use_scratch_gpr(&mut args[0]);
    let data = ra.use_gpr(&mut args[1]);

    match bitsize {
        8 => {
            ra.asm
                .crc32(result.cvt32().unwrap(), data.cvt8().unwrap())
                .unwrap();
        }
        16 => {
            ra.asm
                .crc32(result.cvt32().unwrap(), data.cvt16().unwrap())
                .unwrap();
        }
        32 => {
            ra.asm
                .crc32(result.cvt32().unwrap(), data.cvt32().unwrap())
                .unwrap();
        }
        64 => {
            ra.asm.crc32(result, data).unwrap();
        }
        _ => unreachable!(),
    }

    ra.define_value(inst_ref, result);
}

pub fn emit_crc32_castagnoli8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_crc32_castagnoli(ra, inst_ref, inst, 8);
}

pub fn emit_crc32_castagnoli16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_crc32_castagnoli(ra, inst_ref, inst, 16);
}

pub fn emit_crc32_castagnoli32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_crc32_castagnoli(ra, inst_ref, inst, 32);
}

pub fn emit_crc32_castagnoli64(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_crc32_castagnoli(ra, inst_ref, inst, 64);
}

// ---------------------------------------------------------------------------
// CRC32 ISO (software fallback — x86 crc32 is Castagnoli only)
// ---------------------------------------------------------------------------

fn emit_crc32_iso(ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst, func: usize) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let (first, rest) = args.split_at_mut(1);
    ra.host_call(
        Some(inst_ref),
        &mut [Some(&mut first[0]), Some(&mut rest[0]), None, None],
    );
    ra.asm.mov(rxbyak::RAX, func as i64).unwrap();
    ra.asm.call_reg(rxbyak::RAX).unwrap();
}

pub fn emit_crc32_iso8(_ctx: &EmitContext, ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst) {
    emit_crc32_iso(ra, inst_ref, inst, fp_helpers::crc32_iso8 as usize);
}

pub fn emit_crc32_iso16(_ctx: &EmitContext, ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst) {
    emit_crc32_iso(ra, inst_ref, inst, fp_helpers::crc32_iso16 as usize);
}

pub fn emit_crc32_iso32(_ctx: &EmitContext, ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst) {
    emit_crc32_iso(ra, inst_ref, inst, fp_helpers::crc32_iso32 as usize);
}

pub fn emit_crc32_iso64(_ctx: &EmitContext, ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst) {
    emit_crc32_iso(ra, inst_ref, inst, fp_helpers::crc32_iso64 as usize);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_crc32_fn_signatures() {
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_crc32_castagnoli8;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_crc32_castagnoli64;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_crc32_iso8;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_crc32_iso64;
    }
}
