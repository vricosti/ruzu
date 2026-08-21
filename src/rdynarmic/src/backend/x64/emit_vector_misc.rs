#![allow(
    clippy::missing_transmute_annotations,
    clippy::useless_transmute,
    unnecessary_transmutes
)]

use crate::backend::x64::abi;
use crate::backend::x64::emit_context::EmitContext;
use crate::backend::x64::emit_vector_helpers::*;
use crate::backend::x64::reg_alloc::RegAlloc;
use crate::common::math_util::{recip_estimate, recip_sqrt_estimate};
use crate::ir::inst::Inst;
use crate::ir::value::{InstRef, Value};

// ---------------------------------------------------------------------------
// VectorSignedAbsoluteDifference — fallback
// ---------------------------------------------------------------------------

macro_rules! define_signed_abs_diff {
    ($name:ident, $signed:ty, $unsigned:ty, $count:expr) => {
        extern "C" fn $name(result: *mut [u8; 16], a: *const [u8; 16], b: *const [u8; 16]) {
            unsafe {
                let va: [$signed; $count] = std::mem::transmute(*a);
                let vb: [$signed; $count] = std::mem::transmute(*b);
                let mut out = [0 as $unsigned; $count];
                for i in 0..$count {
                    out[i] = (va[i] as i64 - vb[i] as i64).unsigned_abs() as $unsigned;
                }
                *result = std::mem::transmute(out);
            }
        }
    };
}

define_signed_abs_diff!(fallback_signed_abs_diff8, i8, u8, 16);
define_signed_abs_diff!(fallback_signed_abs_diff16, i16, u16, 8);
define_signed_abs_diff!(fallback_signed_abs_diff32, i32, u32, 4);

pub fn emit_vector_signed_absolute_difference8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_signed_abs_diff8 as usize);
}
pub fn emit_vector_signed_absolute_difference16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_signed_abs_diff16 as usize);
}
pub fn emit_vector_signed_absolute_difference32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_signed_abs_diff32 as usize);
}

// ---------------------------------------------------------------------------
// VectorUnsignedAbsoluteDifference — fallback
// ---------------------------------------------------------------------------

macro_rules! define_unsigned_abs_diff {
    ($name:ident, $ty:ty, $count:expr) => {
        extern "C" fn $name(result: *mut [u8; 16], a: *const [u8; 16], b: *const [u8; 16]) {
            unsafe {
                let va: [$ty; $count] = std::mem::transmute(*a);
                let vb: [$ty; $count] = std::mem::transmute(*b);
                let mut out = [0 as $ty; $count];
                for i in 0..$count {
                    out[i] = if va[i] >= vb[i] {
                        va[i] - vb[i]
                    } else {
                        vb[i] - va[i]
                    };
                }
                *result = std::mem::transmute(out);
            }
        }
    };
}

define_unsigned_abs_diff!(fallback_unsigned_abs_diff8, u8, 16);
define_unsigned_abs_diff!(fallback_unsigned_abs_diff16, u16, 8);
define_unsigned_abs_diff!(fallback_unsigned_abs_diff32, u32, 4);

pub fn emit_vector_unsigned_absolute_difference8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_unsigned_abs_diff8 as usize);
}
pub fn emit_vector_unsigned_absolute_difference16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_unsigned_abs_diff16 as usize);
}
pub fn emit_vector_unsigned_absolute_difference32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_unsigned_abs_diff32 as usize);
}

// ---------------------------------------------------------------------------
// VectorRoundingHalvingAddSigned — fallback
// ---------------------------------------------------------------------------

macro_rules! define_rounding_halving_add_signed {
    ($name:ident, $sty:ty, $wide:ty, $count:expr) => {
        extern "C" fn $name(result: *mut [u8; 16], a: *const [u8; 16], b: *const [u8; 16]) {
            unsafe {
                let va: [$sty; $count] = std::mem::transmute(*a);
                let vb: [$sty; $count] = std::mem::transmute(*b);
                let mut out = [0 as $sty; $count];
                for i in 0..$count {
                    let sum = va[i] as $wide + vb[i] as $wide + 1;
                    out[i] = (sum >> 1) as $sty;
                }
                *result = std::mem::transmute(out);
            }
        }
    };
}

define_rounding_halving_add_signed!(fallback_rhadd_s8, i8, i16, 16);
define_rounding_halving_add_signed!(fallback_rhadd_s16, i16, i32, 8);
define_rounding_halving_add_signed!(fallback_rhadd_s32, i32, i64, 4);

pub fn emit_vector_rounding_halving_add_signed8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_rhadd_s8 as usize);
}
pub fn emit_vector_rounding_halving_add_signed16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_rhadd_s16 as usize);
}
pub fn emit_vector_rounding_halving_add_signed32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_rhadd_s32 as usize);
}

// ---------------------------------------------------------------------------
// VectorRoundingHalvingAddUnsigned — fallback
// ---------------------------------------------------------------------------

macro_rules! define_rounding_halving_add_unsigned {
    ($name:ident, $uty:ty, $wide:ty, $count:expr) => {
        extern "C" fn $name(result: *mut [u8; 16], a: *const [u8; 16], b: *const [u8; 16]) {
            unsafe {
                let va: [$uty; $count] = std::mem::transmute(*a);
                let vb: [$uty; $count] = std::mem::transmute(*b);
                let mut out = [0 as $uty; $count];
                for i in 0..$count {
                    let sum = va[i] as $wide + vb[i] as $wide + 1;
                    out[i] = (sum >> 1) as $uty;
                }
                *result = std::mem::transmute(out);
            }
        }
    };
}

define_rounding_halving_add_unsigned!(fallback_rhadd_u8, u8, u16, 16);
define_rounding_halving_add_unsigned!(fallback_rhadd_u16, u16, u32, 8);
define_rounding_halving_add_unsigned!(fallback_rhadd_u32, u32, u64, 4);

// RoundingHalvingAddUnsigned8: SSE2 pavgb = (a + b + 1) >> 1
pub fn emit_vector_rounding_halving_add_unsigned8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_vector_op(ra, inst_ref, inst, rxbyak::CodeAssembler::pavgb);
}
// RoundingHalvingAddUnsigned16: SSE2 pavgw
pub fn emit_vector_rounding_halving_add_unsigned16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_vector_op(ra, inst_ref, inst, rxbyak::CodeAssembler::pavgw);
}
// RoundingHalvingAddUnsigned32: no pavgd in SSE — keep fallback
pub fn emit_vector_rounding_halving_add_unsigned32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_rhadd_u32 as usize);
}

// ---------------------------------------------------------------------------
// VectorTable / VectorTableLookup
// ---------------------------------------------------------------------------

#[repr(C)]
struct TableLookupFrame {
    result: [u8; 16],
    defaults: [u8; 16],
    indices: [u8; 16],
    table: [[u8; 16]; 4],
    table_size: usize,
    lane_count: usize,
    table_entry_size: usize,
}

extern "C" fn fallback_vector_table_lookup(frame: *mut TableLookupFrame) {
    unsafe {
        let frame = &mut *frame;
        frame.result = frame.defaults;
        for i in 0..frame.lane_count {
            let index = frame.indices[i] as usize;
            let table_index = index / frame.table_entry_size;
            let element_index = index % frame.table_entry_size;
            if table_index < frame.table_size {
                frame.result[i] = frame.table[table_index][element_index];
            }
        }
    }
}

pub fn emit_vector_table(_ctx: &EmitContext, _ra: &mut RegAlloc, _inst_ref: InstRef, _inst: &Inst) {
    // Upstream: do nothing. The table pseudo-op keeps its operands alive for
    // the single following VectorTableLookup, which reads the pseudo-op args.
}

fn table_inst<'a>(ctx: &'a EmitContext<'a>, inst: &Inst) -> &'a Inst {
    let Value::Inst(table_ref) = inst.args[1] else {
        panic!("VectorTableLookup arg1 must be a VectorTable instruction");
    };
    let block = ctx
        .block
        .expect("EmitContext::block is required for VectorTableLookup");
    let table = block.get(table_ref);
    assert_eq!(table.opcode, crate::ir::opcode::Opcode::VectorTable);
    table
}

fn emit_vector_table_lookup(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
    lane_count: usize,
    table_entry_size: usize,
) {
    let table = table_inst(ctx, inst);
    let table_size = table
        .args
        .iter()
        .take(table.num_args())
        .filter(|value| !matches!(value, Value::Void))
        .count();

    let lookup_args = [inst.args[0], inst.args[2]];
    let mut args = ra.get_argument_info(inst_ref, &lookup_args, lookup_args.len());
    let mut table_args = ra.get_argument_info(inst_ref, &table.args, table.num_args());

    let defaults = ra.use_xmm(&mut args[0]);
    let indices = ra.use_xmm(&mut args[1]);
    let table_regs: Vec<_> = table_args
        .iter_mut()
        .take(table.num_args())
        .filter(|arg| !arg.is_void())
        .map(|arg| ra.use_xmm(arg))
        .collect();
    let result = ra.scratch_xmm();

    let frame_offset = abi::ABI_SHADOW_SPACE as i32;
    let frame_size = abi::ABI_SHADOW_SPACE + std::mem::size_of::<TableLookupFrame>();
    ra.alloc_stack_space(frame_size);

    let rsp = rxbyak::RegExp::from(rxbyak::RSP) + frame_offset;
    if table_entry_size == 8 {
        ra.asm.movq(rxbyak::qword_ptr(rsp + 16), defaults).unwrap();
        ra.asm.movq(rxbyak::qword_ptr(rsp + 32), indices).unwrap();
    } else {
        ra.asm
            .movaps(rxbyak::xmmword_ptr(rsp + 16), defaults)
            .unwrap();
        ra.asm
            .movaps(rxbyak::xmmword_ptr(rsp + 32), indices)
            .unwrap();
    }
    for (i, table_reg) in table_regs.iter().enumerate() {
        if table_entry_size == 8 {
            ra.asm
                .movq(rxbyak::qword_ptr(rsp + 48 + (i as i32 * 16)), *table_reg)
                .unwrap();
        } else {
            ra.asm
                .movaps(rxbyak::xmmword_ptr(rsp + 48 + (i as i32 * 16)), *table_reg)
                .unwrap();
        }
    }
    ra.asm
        .mov(
            rxbyak::qword_ptr(rsp + 112),
            i64::try_from(table_size).unwrap(),
        )
        .unwrap();
    ra.asm
        .mov(
            rxbyak::qword_ptr(rsp + 120),
            i64::try_from(lane_count).unwrap(),
        )
        .unwrap();
    ra.asm
        .mov(
            rxbyak::qword_ptr(rsp + 128),
            i64::try_from(table_entry_size).unwrap(),
        )
        .unwrap();

    ra.end_of_alloc_scope();
    ra.host_call(None, &mut [None, None, None, None]);

    ra.asm
        .lea(
            abi::ABI_PARAMS[0].to_reg64(),
            rxbyak::qword_ptr(rxbyak::RegExp::from(rxbyak::RSP) + frame_offset),
        )
        .unwrap();
    ra.asm
        .mov(rxbyak::RAX, fallback_vector_table_lookup as usize as i64)
        .unwrap();
    ra.asm.call_reg(rxbyak::RAX).unwrap();

    if table_entry_size == 8 {
        ra.asm
            .movq(
                result,
                rxbyak::qword_ptr(rxbyak::RegExp::from(rxbyak::RSP) + frame_offset),
            )
            .unwrap();
    } else {
        ra.asm
            .movaps(
                result,
                rxbyak::xmmword_ptr(rxbyak::RegExp::from(rxbyak::RSP) + frame_offset),
            )
            .unwrap();
    }
    ra.release_stack_space(frame_size);
    ra.define_value(inst_ref, result);
}

pub fn emit_vector_table_lookup64(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_vector_table_lookup(ctx, ra, inst_ref, inst, 8, 8);
}

pub fn emit_vector_table_lookup128(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_vector_table_lookup(ctx, ra, inst_ref, inst, 16, 16);
}

// ---------------------------------------------------------------------------
// VectorUnsignedRecipEstimate — fallback (1-arg, per-element u32)
// ---------------------------------------------------------------------------

fn unsigned_recip_estimate(a: u32) -> u32 {
    if (a & 0x8000_0000) == 0 {
        return 0xFFFF_FFFF;
    }
    let input = (a >> 23) & 0x1ff;
    let estimate = recip_estimate(input as u64) as u32;
    (0x100 | estimate) << 23
}

extern "C" fn fallback_unsigned_recip_estimate(result: *mut [u8; 16], a: *const [u8; 16]) {
    unsafe {
        let va: [u32; 4] = std::mem::transmute(*a);
        let out: [u32; 4] = [
            unsigned_recip_estimate(va[0]),
            unsigned_recip_estimate(va[1]),
            unsigned_recip_estimate(va[2]),
            unsigned_recip_estimate(va[3]),
        ];
        *result = std::mem::transmute(out);
    }
}

pub fn emit_vector_unsigned_recip_estimate(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_one_arg_fallback(
        ra,
        inst_ref,
        inst,
        fallback_unsigned_recip_estimate as usize,
    );
}

// ---------------------------------------------------------------------------
// VectorUnsignedRecipSqrtEstimate — fallback (1-arg, per-element u32)
// ---------------------------------------------------------------------------

fn unsigned_recip_sqrt_estimate(a: u32) -> u32 {
    if (a & 0xC000_0000) == 0 {
        return 0xFFFF_FFFF;
    }
    let input = (a >> 23) & 0x1ff;
    let estimate = recip_sqrt_estimate(input as u64) as u32;
    (0x100 | estimate) << 23
}

extern "C" fn fallback_unsigned_recip_sqrt_estimate(result: *mut [u8; 16], a: *const [u8; 16]) {
    unsafe {
        let va: [u32; 4] = std::mem::transmute(*a);
        let out: [u32; 4] = [
            unsigned_recip_sqrt_estimate(va[0]),
            unsigned_recip_sqrt_estimate(va[1]),
            unsigned_recip_sqrt_estimate(va[2]),
            unsigned_recip_sqrt_estimate(va[3]),
        ];
        *result = std::mem::transmute(out);
    }
}

pub fn emit_vector_unsigned_recip_sqrt_estimate(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_one_arg_fallback(
        ra,
        inst_ref,
        inst,
        fallback_unsigned_recip_sqrt_estimate as usize,
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fn_signatures() {
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) =
            emit_vector_signed_absolute_difference8;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) =
            emit_vector_unsigned_absolute_difference32;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) =
            emit_vector_rounding_halving_add_signed8;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) =
            emit_vector_rounding_halving_add_unsigned32;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_table;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_table_lookup64;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_table_lookup128;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) =
            emit_vector_unsigned_recip_estimate;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) =
            emit_vector_unsigned_recip_sqrt_estimate;
    }

    #[test]
    fn unsigned_estimates_match_upstream_reference_values() {
        assert_eq!(unsigned_recip_estimate(0x7fff_ffff), 0xffff_ffff);
        assert_eq!(unsigned_recip_estimate(0x8000_0000), 0xff80_0000);
        assert_eq!(unsigned_recip_sqrt_estimate(0x3fff_ffff), 0xffff_ffff);
        assert_eq!(unsigned_recip_sqrt_estimate(0x8000_0000), 0xb480_0000);
    }

    #[test]
    fn test_fallback_unsigned_abs_diff8() {
        let a: [u8; 16] = [
            10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 200, 255,
        ];
        let b: [u8; 16] = [
            5, 25, 30, 45, 50, 55, 75, 80, 95, 100, 115, 120, 125, 145, 100, 0,
        ];
        let mut result = [0u8; 16];
        fallback_unsigned_abs_diff8(&mut result, &a, &b);
        assert_eq!(result[0], 5);
        assert_eq!(result[1], 5);
        assert_eq!(result[2], 0);
        assert_eq!(result[14], 100);
        assert_eq!(result[15], 255);
    }

    #[test]
    fn test_fallback_table_lookup128() {
        let mut frame = TableLookupFrame {
            result: [0; 16],
            defaults: [0xAA; 16],
            indices: [0, 1, 15, 16, 17, 31, 32, 63, 64, 2, 3, 4, 5, 6, 7, 8],
            table: [[0; 16]; 4],
            table_size: 2,
            lane_count: 16,
            table_entry_size: 16,
        };
        for i in 0..16 {
            frame.table[0][i] = i as u8;
            frame.table[1][i] = 0x80 + i as u8;
        }

        fallback_vector_table_lookup(&mut frame);

        assert_eq!(
            frame.result,
            [0, 1, 15, 0x80, 0x81, 0x8F, 0xAA, 0xAA, 0xAA, 2, 3, 4, 5, 6, 7, 8,]
        );
    }

    #[test]
    fn test_fallback_table_lookup64_only_updates_low_half() {
        let mut frame = TableLookupFrame {
            result: [0; 16],
            defaults: [0xCC; 16],
            indices: [0, 1, 2, 3, 4, 5, 6, 20, 0, 1, 2, 3, 4, 5, 6, 7],
            table: [[0; 16]; 4],
            table_size: 1,
            lane_count: 8,
            table_entry_size: 8,
        };
        for i in 0..16 {
            frame.table[0][i] = 0x10 + i as u8;
        }

        fallback_vector_table_lookup(&mut frame);

        assert_eq!(
            &frame.result[..8],
            &[0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0xCC]
        );
        assert_eq!(&frame.result[8..], &[0xCC; 8]);
    }

    #[test]
    fn test_fallback_table_lookup64_uses_eight_byte_table_entries() {
        let mut frame = TableLookupFrame {
            result: [0; 16],
            defaults: [0xDD; 16],
            indices: [0, 1, 7, 8, 9, 15, 16, 31, 0, 0, 0, 0, 0, 0, 0, 0],
            table: [[0; 16]; 4],
            table_size: 2,
            lane_count: 8,
            table_entry_size: 8,
        };
        for i in 0..8 {
            frame.table[0][i] = 0x10 + i as u8;
            frame.table[1][i] = 0x80 + i as u8;
        }

        fallback_vector_table_lookup(&mut frame);

        assert_eq!(
            &frame.result[..8],
            &[0x10, 0x11, 0x17, 0x80, 0x81, 0x87, 0xDD, 0xDD]
        );
        assert_eq!(&frame.result[8..], &[0xDD; 8]);
    }

    #[test]
    fn test_fallback_rhadd_u8() {
        let a: [u8; 16] =
            unsafe { std::mem::transmute([3u8, 7, 0, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]) };
        let b: [u8; 16] =
            unsafe { std::mem::transmute([4u8, 8, 1, 254, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]) };
        let mut result = [0u8; 16];
        fallback_rhadd_u8(&mut result, &a, &b);
        let out: [u8; 16] = result;
        assert_eq!(out[0], 4); // (3+4+1)/2 = 4
        assert_eq!(out[1], 8); // (7+8+1)/2 = 8
        assert_eq!(out[2], 1); // (0+1+1)/2 = 1
        assert_eq!(out[3], 255); // (255+254+1)/2 = 255
    }
}
