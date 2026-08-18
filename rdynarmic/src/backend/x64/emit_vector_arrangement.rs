#![allow(
    clippy::missing_transmute_annotations,
    clippy::useless_transmute,
    unnecessary_transmutes
)]

use crate::backend::x64::emit_context::EmitContext;
use crate::backend::x64::emit_vector_helpers::*;
use crate::backend::x64::host_feature::HostFeature;
use crate::backend::x64::reg_alloc::RegAlloc;
use crate::ir::inst::Inst;
use crate::ir::value::InstRef;

// ---------------------------------------------------------------------------
// VectorGetElement — native SSE4.1: pextrb/pextrw/pextrd/pextrq
// ---------------------------------------------------------------------------

pub fn emit_vector_get_element8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let src = ra.use_xmm(&mut args[0]);
    let idx = args[1].get_immediate_u8();
    let result = ra.scratch_gpr();
    ra.asm.pextrb(result.cvt32().unwrap(), src, idx).unwrap();
    ra.release(src);
    ra.define_value(inst_ref, result);
}

pub fn emit_vector_get_element16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let src = ra.use_xmm(&mut args[0]);
    let idx = args[1].get_immediate_u8();
    let result = ra.scratch_gpr();
    ra.asm.pextrw(result.cvt32().unwrap(), src, idx).unwrap();
    ra.release(src);
    ra.define_value(inst_ref, result);
}

pub fn emit_vector_get_element32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let src = ra.use_xmm(&mut args[0]);
    let idx = args[1].get_immediate_u8();
    let result = ra.scratch_gpr();
    ra.asm.pextrd(result.cvt32().unwrap(), src, idx).unwrap();
    ra.release(src);
    ra.define_value(inst_ref, result);
}

pub fn emit_vector_get_element64(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let idx = args[1].get_immediate_u8();
    // Mirrors upstream `EmitVectorGetElement64` in
    // `emit_x64_vector.cpp:5181-5197`: use `movq` for index==0 (shorter
    // encoding, no immediate). Suspected bug in our pextrq path was
    // observed in STK's UMAXP+UMOV strchr loop where X3 stayed 0 even
    // when V17.D[0] was non-zero — switching to movq fixes the common
    // case and matches upstream byte-for-byte.
    if idx == 0 {
        let src = ra.use_xmm(&mut args[0]);
        let result = ra.scratch_gpr();
        ra.asm.movq(result, src).unwrap();
        ra.release(src);
        ra.define_value(inst_ref, result);
        return;
    }
    let src = ra.use_xmm(&mut args[0]);
    let result = ra.scratch_gpr();
    ra.asm.pextrq(result, src, idx).unwrap();
    ra.release(src);
    ra.define_value(inst_ref, result);
}

// ---------------------------------------------------------------------------
// VectorSetElement — native SSE4.1: pinsrb/pinsrw/pinsrd/pinsrq
// ---------------------------------------------------------------------------

// Upstream arg order: (vec: U128, idx: U8, elem: Uxx)
pub fn emit_vector_set_element8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let result = ra.use_scratch_xmm(&mut args[0]);
    let idx = args[1].get_immediate_u8();
    let val = ra.use_gpr(&mut args[2]);
    ra.asm.pinsrb(result, val.cvt32().unwrap(), idx).unwrap();
    ra.define_value(inst_ref, result);
}

pub fn emit_vector_set_element16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let result = ra.use_scratch_xmm(&mut args[0]);
    let idx = args[1].get_immediate_u8();
    let val = ra.use_gpr(&mut args[2]);
    ra.asm.pinsrw(result, val.cvt32().unwrap(), idx).unwrap();
    ra.define_value(inst_ref, result);
}

pub fn emit_vector_set_element32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let result = ra.use_scratch_xmm(&mut args[0]);
    let idx = args[1].get_immediate_u8();
    let val = ra.use_gpr(&mut args[2]);
    ra.asm.pinsrd(result, val.cvt32().unwrap(), idx).unwrap();
    ra.define_value(inst_ref, result);
}

pub fn emit_vector_set_element64(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let result = ra.use_scratch_xmm(&mut args[0]);
    let idx = args[1].get_immediate_u8();
    let val = ra.use_gpr(&mut args[2]);
    ra.asm.pinsrq(result, val, idx).unwrap();
    ra.define_value(inst_ref, result);
}

// ---------------------------------------------------------------------------
// VectorBroadcast — fallback
// ---------------------------------------------------------------------------

macro_rules! define_broadcast {
    ($name:ident, $ty:ty, $count:expr) => {
        extern "C" fn $name(result: *mut [u8; 16], a: *const [u8; 16]) {
            unsafe {
                let va: [$ty; $count] = std::mem::transmute(*a);
                let val = va[0];
                let out = [val; $count];
                *result = std::mem::transmute(out);
            }
        }
    };
}

define_broadcast!(fallback_broadcast8, u8, 16);
define_broadcast!(fallback_broadcast16, u16, 8);
define_broadcast!(fallback_broadcast32, u32, 4);
define_broadcast!(fallback_broadcast64, u64, 2);

// Broadcast8: pshufb with all-zero mask (broadcasts byte[0] to all 16 lanes)
pub fn emit_vector_broadcast8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let result = ra.use_scratch_xmm(&mut args[0]);
    let pool = ra.constant_pool.as_mut().expect("constant pool required");
    let zero_mask = pool.get_constant(0, 0); // all-zero shuffle mask = broadcast byte[0]
    ra.asm
        .pshufb(result, rxbyak::xmmword_ptr(zero_mask))
        .unwrap();
    ra.define_value(inst_ref, result);
}
// Broadcast16: pshuflw with 0x00 (broadcast word[0] to low 4 words), then punpcklqdq
pub fn emit_vector_broadcast16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let result = ra.use_scratch_xmm(&mut args[0]);
    ra.asm.pshuflw(result, result, 0x00).unwrap(); // broadcast word[0] to low 4 words
    ra.asm.punpcklqdq(result, result).unwrap(); // copy low 64 to high 64
    ra.define_value(inst_ref, result);
}
// Broadcast32: pshufd with 0x00 (broadcast dword[0] to all 4 lanes)
pub fn emit_vector_broadcast32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let src = ra.use_xmm(&mut args[0]);
    let result = ra.scratch_xmm();
    ra.asm.pshufd(result, src, 0x00).unwrap();
    ra.release(src);
    ra.define_value(inst_ref, result);
}
// Broadcast64: punpcklqdq to duplicate low qword
pub fn emit_vector_broadcast64(
    ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let result = ra.use_scratch_xmm(&mut args[0]);
    ra.asm.punpcklqdq(result, result).unwrap();
    // RUZU_BCAST64_MARK_XMM13=1 — set xmm13 = all FFs immediately after
    // BCAST64 emit. If xmm13 != all FFs at the W128 callback, BCAST64
    // emit's code did not execute (block jumped past it).
    if std::env::var("RUZU_BCAST64_MARK_XMM13").is_ok() {
        ra.asm.db(0x66).unwrap();
        ra.asm.db(0x45).unwrap();
        ra.asm.db(0x0F).unwrap();
        ra.asm.db(0x74).unwrap();
        ra.asm.db(0xED).unwrap();
    }
    // RUZU_BCAST64_MIRROR_XMM12=1 — copy result xmm into xmm12 right
    // after the BCAST64 emit. At the W128 callback, xmm12 should still
    // hold the BCAST64 result. Diff vs xmm1 reveals whether xmm1
    // specifically gets modified vs ambient XMM clobber.
    if std::env::var("RUZU_BCAST64_MIRROR_XMM12").is_ok() {
        // movaps xmm12, <result>
        ra.asm.movaps(rxbyak::XMM12, result).unwrap();
    }
    // RUZU_ASSERT_BCAST64_ZERO=1 — emit `ptest result, result; je +2; ud2`
    // right after the broadcast emit, ONLY when the source is ImmU64(0).
    // RUZU_ASSERT_BCAST64_ZERO_PC=0xPC — restrict to specific blocks.
    if std::env::var("RUZU_ASSERT_BCAST64_ZERO").is_ok()
        && matches!(inst.args[0], crate::ir::Value::ImmU64(0))
    {
        let pc_filter_ok = match std::env::var("RUZU_ASSERT_BCAST64_ZERO_PC") {
            Ok(spec) => {
                let block_pc = ctx.arch.extract_pc(ctx.location);
                let pcs: Vec<u64> = spec
                    .split(',')
                    .filter_map(|p| u64::from_str_radix(p.trim().trim_start_matches("0x"), 16).ok())
                    .collect();
                pcs.contains(&block_pc)
            }
            Err(_) => true,
        };
        if pc_filter_ok {
            ra.asm.ptest(result, result).unwrap();
            ra.asm.db(0x74).unwrap();
            ra.asm.db(0x02).unwrap();
            ra.asm.ud2().unwrap();
        }
    }
    ra.define_value(inst_ref, result);
}

// ---------------------------------------------------------------------------
// VectorBroadcastLower — broadcast element 0 to lower half only
// ---------------------------------------------------------------------------

macro_rules! define_broadcast_lower {
    ($name:ident, $ty:ty, $count:expr, $half:expr) => {
        extern "C" fn $name(result: *mut [u8; 16], a: *const [u8; 16]) {
            unsafe {
                let va: [$ty; $count] = std::mem::transmute(*a);
                let val = va[0];
                let mut out = [0 as $ty; $count];
                for i in 0..$half {
                    out[i] = val;
                }
                *result = std::mem::transmute(out);
            }
        }
    };
}

define_broadcast_lower!(fallback_broadcast_lower8, u8, 16, 8);
define_broadcast_lower!(fallback_broadcast_lower16, u16, 8, 4);
define_broadcast_lower!(fallback_broadcast_lower32, u32, 4, 2);

// BroadcastLower8: pshufb to broadcast byte[0] to lower 8 bytes, zero upper
pub fn emit_vector_broadcast_lower8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let result = ra.use_scratch_xmm(&mut args[0]);
    let pool = ra.constant_pool.as_mut().expect("constant pool required");
    // Mask: bytes 0-7 = 0x00 (select byte[0]), bytes 8-15 = 0x80 (zero)
    let mask = pool.get_constant(0x00_00_00_00_00_00_00_00u64, 0x80_80_80_80_80_80_80_80u64);
    ra.asm.pshufb(result, rxbyak::xmmword_ptr(mask)).unwrap();
    ra.define_value(inst_ref, result);
}
// BroadcastLower16: pshuflw with 0x00 then movq to zero upper
pub fn emit_vector_broadcast_lower16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let src = ra.use_xmm(&mut args[0]);
    let result = ra.scratch_xmm();
    ra.asm.pshuflw(result, src, 0x00).unwrap();
    // Zero upper 64 bits
    ra.asm.movq(result, result).unwrap();
    ra.release(src);
    ra.define_value(inst_ref, result);
}
// BroadcastLower32: pshufd with 0x00, then movq to zero upper
pub fn emit_vector_broadcast_lower32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let src = ra.use_xmm(&mut args[0]);
    let result = ra.scratch_xmm();
    ra.asm.pshufd(result, src, 0x00).unwrap();
    ra.asm.movq(result, result).unwrap(); // zero upper 64 bits
    ra.release(src);
    ra.define_value(inst_ref, result);
}

// ---------------------------------------------------------------------------
// VectorExtract — palignr (native SSE): extracts from concatenation
// ---------------------------------------------------------------------------

pub fn emit_vector_extract(_ctx: &EmitContext, ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let a = ra.use_xmm(&mut args[0]); // low part (Qn in ARM VEXT)
    let result = ra.use_scratch_xmm(&mut args[1]); // high part (Qm in ARM VEXT)
    let imm = args[2].get_immediate_u8();
    // PALIGNR(dest=high, src=low, bytes): extracts from [high:low] >> bytes
    // Position is in bits (upstream convention), PALIGNR takes bytes
    ra.asm.palignr(result, a, imm / 8).unwrap();
    ra.release(a);
    ra.define_value(inst_ref, result);
}

// ---------------------------------------------------------------------------
// VectorExtractLower — extract from two concatenated 64-bit vectors and zero
// the upper half, matching upstream EmitX64::EmitVectorExtractLower.
// ---------------------------------------------------------------------------

pub fn emit_vector_extract_lower(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let result = ra.use_scratch_xmm(&mut args[0]);
    let position = args[2].get_immediate_u8();
    assert_eq!(position % 8, 0);

    if position != 0 {
        let high = ra.use_xmm(&mut args[1]);
        ra.asm.punpcklqdq(result, high).unwrap();
        ra.asm.psrldq(result, position / 8).unwrap();
    }
    ra.asm.movq(result, result).unwrap();
    ra.define_value(inst_ref, result);
}

fn whole_vector_rotate_shuffle_imm(shift_amount: u8) -> u8 {
    assert_eq!(shift_amount % 32, 0);
    0b1110_0100_u8.rotate_right(u32::from(shift_amount / 32) * 2)
}

pub fn emit_vector_rotate_whole_vector_right(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let operand = ra.use_xmm(&mut args[0]);
    let result = ra.scratch_xmm();
    let shift_amount = args[1].get_immediate_u8();
    let shuffle_imm = whole_vector_rotate_shuffle_imm(shift_amount);
    ra.asm.pshufd(result, operand, shuffle_imm).unwrap();
    ra.define_value(inst_ref, result);
}

// ---------------------------------------------------------------------------
// VectorInterleaveLower — native SSE: punpcklbw/wd/dq/qdq
// ---------------------------------------------------------------------------

pub fn emit_vector_interleave_lower8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_vector_op(ra, inst_ref, inst, rxbyak::CodeAssembler::punpcklbw);
}
pub fn emit_vector_interleave_lower16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_vector_op(ra, inst_ref, inst, rxbyak::CodeAssembler::punpcklwd);
}
pub fn emit_vector_interleave_lower32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_vector_op(ra, inst_ref, inst, rxbyak::CodeAssembler::punpckldq);
}
pub fn emit_vector_interleave_lower64(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_vector_op(ra, inst_ref, inst, rxbyak::CodeAssembler::punpcklqdq);
}

// ---------------------------------------------------------------------------
// VectorInterleaveUpper — native SSE: punpckhbw/wd/dq/qdq
// ---------------------------------------------------------------------------

pub fn emit_vector_interleave_upper8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_vector_op(ra, inst_ref, inst, rxbyak::CodeAssembler::punpckhbw);
}
pub fn emit_vector_interleave_upper16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_vector_op(ra, inst_ref, inst, rxbyak::CodeAssembler::punpckhwd);
}
pub fn emit_vector_interleave_upper32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_vector_op(ra, inst_ref, inst, rxbyak::CodeAssembler::punpckhdq);
}
pub fn emit_vector_interleave_upper64(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_vector_op(ra, inst_ref, inst, rxbyak::CodeAssembler::punpckhqdq);
}

// ---------------------------------------------------------------------------
// VectorDeinterleaveEven/Odd — fallback
// ---------------------------------------------------------------------------

macro_rules! define_deinterleave {
    ($name:ident, $ty:ty, $count:expr, $even:expr) => {
        extern "C" fn $name(result: *mut [u8; 16], a: *const [u8; 16], b: *const [u8; 16]) {
            unsafe {
                let va: [$ty; $count] = std::mem::transmute(*a);
                let vb: [$ty; $count] = std::mem::transmute(*b);
                let mut out = [0 as $ty; $count];
                let half = $count / 2;
                let start = if $even { 0 } else { 1 };
                for i in 0..half {
                    out[i] = va[i * 2 + start];
                }
                for i in 0..half {
                    out[half + i] = vb[i * 2 + start];
                }
                *result = std::mem::transmute(out);
            }
        }
    };
}

define_deinterleave!(fallback_deinterleave_even8, u8, 16, true);
define_deinterleave!(fallback_deinterleave_even16, u16, 8, true);
define_deinterleave!(fallback_deinterleave_even32, u32, 4, true);
define_deinterleave!(fallback_deinterleave_even64, u64, 2, true);
define_deinterleave!(fallback_deinterleave_odd8, u8, 16, false);
define_deinterleave!(fallback_deinterleave_odd16, u16, 8, false);
define_deinterleave!(fallback_deinterleave_odd32, u32, 4, false);
define_deinterleave!(fallback_deinterleave_odd64, u64, 2, false);

// DeinterleaveEven8: pshufb to extract even bytes from each, then combine
// even bytes of [a0,a1,a2,...,a15] = [a0,a2,a4,...,a14] in lower 8 bytes
pub fn emit_vector_deinterleave_even8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let result = ra.use_scratch_xmm(&mut args[0]); // a
    let b = ra.use_scratch_xmm(&mut args[1]);
    let pool = ra.constant_pool.as_mut().expect("constant pool required");
    // Shuffle mask: pick bytes 0,2,4,6,8,10,12,14 then 0x80 for rest
    let even_mask = pool.get_constant(
        0x0E_0C_0A_08_06_04_02_00u64, // bytes 0-7: even indices
        0x80_80_80_80_80_80_80_80u64, // bytes 8-15: zero
    );
    ra.asm
        .pshufb(result, rxbyak::xmmword_ptr(even_mask))
        .unwrap(); // a evens in low 8
    ra.asm.pshufb(b, rxbyak::xmmword_ptr(even_mask)).unwrap(); // b evens in low 8
    ra.asm.punpcklqdq(result, b).unwrap(); // combine: a_evens | b_evens
    ra.define_value(inst_ref, result);
}
// DeinterleaveEven16: extract even words using pshufb
pub fn emit_vector_deinterleave_even16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let result = ra.use_scratch_xmm(&mut args[0]);
    let b = ra.use_scratch_xmm(&mut args[1]);
    let pool = ra.constant_pool.as_mut().expect("constant pool required");
    // Pick words 0,2,4,6 (bytes 0-1,4-5,8-9,12-13)
    let even_mask = pool.get_constant(0x0D_0C_09_08_05_04_01_00u64, 0x80_80_80_80_80_80_80_80u64);
    ra.asm
        .pshufb(result, rxbyak::xmmword_ptr(even_mask))
        .unwrap();
    ra.asm.pshufb(b, rxbyak::xmmword_ptr(even_mask)).unwrap();
    ra.asm.punpcklqdq(result, b).unwrap();
    ra.define_value(inst_ref, result);
}
// DeinterleaveEven32: shufps to pick dwords 0,2 from each
pub fn emit_vector_deinterleave_even32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let result = ra.use_scratch_xmm(&mut args[0]); // a
    let b = ra.use_xmm(&mut args[1]);
    // shufps(a, b, 0b_10_00_10_00) = {a[0], a[2], b[0], b[2]}
    ra.asm.shufps(result, b, 0x88).unwrap();
    ra.define_value(inst_ref, result);
}
// DeinterleaveEven64: shufpd to pick qwords 0 from each
pub fn emit_vector_deinterleave_even64(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let result = ra.use_scratch_xmm(&mut args[0]); // a
    let b = ra.use_xmm(&mut args[1]);
    // shufpd(a, b, 0b_00) = {a[0], b[0]}
    ra.asm.shufpd(result, b, 0x00).unwrap();
    ra.define_value(inst_ref, result);
}
// DeinterleaveOdd8: pshufb to extract odd bytes
pub fn emit_vector_deinterleave_odd8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let result = ra.use_scratch_xmm(&mut args[0]);
    let b = ra.use_scratch_xmm(&mut args[1]);
    let pool = ra.constant_pool.as_mut().expect("constant pool required");
    let odd_mask = pool.get_constant(0x0F_0D_0B_09_07_05_03_01u64, 0x80_80_80_80_80_80_80_80u64);
    ra.asm
        .pshufb(result, rxbyak::xmmword_ptr(odd_mask))
        .unwrap();
    ra.asm.pshufb(b, rxbyak::xmmword_ptr(odd_mask)).unwrap();
    ra.asm.punpcklqdq(result, b).unwrap();
    ra.define_value(inst_ref, result);
}
// DeinterleaveOdd16: extract odd words
pub fn emit_vector_deinterleave_odd16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let result = ra.use_scratch_xmm(&mut args[0]);
    let b = ra.use_scratch_xmm(&mut args[1]);
    let pool = ra.constant_pool.as_mut().expect("constant pool required");
    let odd_mask = pool.get_constant(0x0F_0E_0B_0A_07_06_03_02u64, 0x80_80_80_80_80_80_80_80u64);
    ra.asm
        .pshufb(result, rxbyak::xmmword_ptr(odd_mask))
        .unwrap();
    ra.asm.pshufb(b, rxbyak::xmmword_ptr(odd_mask)).unwrap();
    ra.asm.punpcklqdq(result, b).unwrap();
    ra.define_value(inst_ref, result);
}
// DeinterleaveOdd32: shufps to pick dwords 1,3 from each
pub fn emit_vector_deinterleave_odd32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let result = ra.use_scratch_xmm(&mut args[0]);
    let b = ra.use_xmm(&mut args[1]);
    // shufps(a, b, 0b_11_01_11_01) = {a[1], a[3], b[1], b[3]}
    ra.asm.shufps(result, b, 0xDD).unwrap();
    ra.define_value(inst_ref, result);
}
// DeinterleaveOdd64: shufpd to pick qwords 1 from each
pub fn emit_vector_deinterleave_odd64(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let result = ra.use_scratch_xmm(&mut args[0]);
    let b = ra.use_xmm(&mut args[1]);
    // shufpd(a, b, 0b_11) = {a[1], b[1]}
    ra.asm.shufpd(result, b, 0x03).unwrap();
    ra.define_value(inst_ref, result);
}

// ---------------------------------------------------------------------------
// VectorDeinterleaveEvenLower/OddLower — operates on lower 64 bits only
// For D-register (64-bit) paired operations (VPMAX, VPMIN, etc.)
// ---------------------------------------------------------------------------

macro_rules! define_deinterleave_lower {
    ($name:ident, $ty:ty, $count:expr, $even:expr) => {
        extern "C" fn $name(result: *mut [u8; 16], a: *const [u8; 16], b: *const [u8; 16]) {
            unsafe {
                // Only lower 64 bits of each input matter
                let va: [$ty; $count] = {
                    let bytes: [u8; 16] = *a;
                    let mut lower = [0u8; 16];
                    lower[..8].copy_from_slice(&bytes[..8]);
                    std::mem::transmute(lower)
                };
                let vb: [$ty; $count] = {
                    let bytes: [u8; 16] = *b;
                    let mut lower = [0u8; 16];
                    lower[..8].copy_from_slice(&bytes[..8]);
                    std::mem::transmute(lower)
                };
                let half = $count / 2; // elements in lower 64 bits
                let start = if $even { 0 } else { 1 };
                let mut out = [0 as $ty; $count];
                // Deinterleave from lower half of a
                let quarter = half / 2;
                for i in 0..quarter {
                    out[i] = va[i * 2 + start];
                }
                // Deinterleave from lower half of b
                for i in 0..quarter {
                    out[quarter + i] = vb[i * 2 + start];
                }
                // Upper half is zero
                *result = std::mem::transmute(out);
            }
        }
    };
}

define_deinterleave_lower!(fallback_deinterleave_even_lower8, u8, 16, true);
define_deinterleave_lower!(fallback_deinterleave_even_lower16, u16, 8, true);
define_deinterleave_lower!(fallback_deinterleave_even_lower32, u32, 4, true);
define_deinterleave_lower!(fallback_deinterleave_odd_lower8, u8, 16, false);
define_deinterleave_lower!(fallback_deinterleave_odd_lower16, u16, 8, false);
define_deinterleave_lower!(fallback_deinterleave_odd_lower32, u32, 4, false);

pub fn emit_vector_deinterleave_even_lower8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(
        ra,
        inst_ref,
        inst,
        fallback_deinterleave_even_lower8 as usize,
    );
}
pub fn emit_vector_deinterleave_even_lower16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(
        ra,
        inst_ref,
        inst,
        fallback_deinterleave_even_lower16 as usize,
    );
}
pub fn emit_vector_deinterleave_even_lower32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(
        ra,
        inst_ref,
        inst,
        fallback_deinterleave_even_lower32 as usize,
    );
}
pub fn emit_vector_deinterleave_odd_lower8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(
        ra,
        inst_ref,
        inst,
        fallback_deinterleave_odd_lower8 as usize,
    );
}
pub fn emit_vector_deinterleave_odd_lower16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(
        ra,
        inst_ref,
        inst,
        fallback_deinterleave_odd_lower16 as usize,
    );
}
pub fn emit_vector_deinterleave_odd_lower32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(
        ra,
        inst_ref,
        inst,
        fallback_deinterleave_odd_lower32 as usize,
    );
}

// ---------------------------------------------------------------------------
// VectorTranspose — native SSE2
// ---------------------------------------------------------------------------

pub fn emit_vector_transpose8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let lower = ra.use_scratch_xmm(&mut args[0]);
    let upper = ra.use_scratch_xmm(&mut args[1]);
    let part = args[2].get_immediate_u1();

    let pool = ra.constant_pool.as_mut().expect("constant pool required");
    if !part {
        let mask = pool.get_constant(0x00FF_00FF_00FF_00FF, 0x00FF_00FF_00FF_00FF);
        ra.asm.pand(lower, rxbyak::xmmword_ptr(mask)).unwrap();
        ra.asm.psllw_imm(upper, 8).unwrap();
    } else {
        ra.asm.psrlw_imm(lower, 8).unwrap();
        let mask = pool.get_constant(0xFF00_FF00_FF00_FF00, 0xFF00_FF00_FF00_FF00);
        ra.asm.pand(upper, rxbyak::xmmword_ptr(mask)).unwrap();
    }
    ra.asm.por(lower, upper).unwrap();

    ra.define_value(inst_ref, lower);
}

pub fn emit_vector_transpose16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let lower = ra.use_scratch_xmm(&mut args[0]);
    let upper = ra.use_scratch_xmm(&mut args[1]);
    let part = args[2].get_immediate_u1();

    let pool = ra.constant_pool.as_mut().expect("constant pool required");
    if !part {
        let mask = pool.get_constant(0x0000_FFFF_0000_FFFF, 0x0000_FFFF_0000_FFFF);
        ra.asm.pand(lower, rxbyak::xmmword_ptr(mask)).unwrap();
        ra.asm.pslld_imm(upper, 16).unwrap();
    } else {
        ra.asm.psrld_imm(lower, 16).unwrap();
        let mask = pool.get_constant(0xFFFF_0000_FFFF_0000, 0xFFFF_0000_FFFF_0000);
        ra.asm.pand(upper, rxbyak::xmmword_ptr(mask)).unwrap();
    }
    ra.asm.por(lower, upper).unwrap();

    ra.define_value(inst_ref, lower);
}

pub fn emit_vector_transpose32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let lower = ra.use_scratch_xmm(&mut args[0]);
    let upper = ra.use_xmm(&mut args[1]);
    let part = args[2].get_immediate_u1();

    ra.asm
        .shufps(lower, upper, if !part { 0x88 } else { 0xDD })
        .unwrap();
    ra.asm.pshufd(lower, lower, 0xD8).unwrap();

    ra.define_value(inst_ref, lower);
}

pub fn emit_vector_transpose64(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let lower = ra.use_scratch_xmm(&mut args[0]);
    let upper = ra.use_xmm(&mut args[1]);
    let part = args[2].get_immediate_u1();

    ra.asm
        .shufpd(lower, upper, if !part { 0x00 } else { 0x03 })
        .unwrap();

    ra.define_value(inst_ref, lower);
}

// ---------------------------------------------------------------------------
// VectorShuffle — native SSE: pshufd/pshufhw/pshuflw
// ---------------------------------------------------------------------------

pub fn emit_vector_shuffle_words(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_vector_shuffle_op(ra, inst_ref, inst, rxbyak::CodeAssembler::pshufd);
}
pub fn emit_vector_shuffle_high_halfwords(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_vector_shuffle_op(ra, inst_ref, inst, rxbyak::CodeAssembler::pshufhw);
}
pub fn emit_vector_shuffle_low_halfwords(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_vector_shuffle_op(ra, inst_ref, inst, rxbyak::CodeAssembler::pshuflw);
}

// ---------------------------------------------------------------------------
// VectorNarrow — fallback
// ---------------------------------------------------------------------------

extern "C" fn fallback_narrow16(result: *mut [u8; 16], a: *const [u8; 16]) {
    unsafe {
        let va: [u16; 8] = std::mem::transmute(*a);
        let dst = &mut *result;
        for i in 0..8 {
            dst[i] = va[i] as u8;
        }
        for byte in dst.iter_mut().skip(8) {
            *byte = 0;
        }
    }
}

extern "C" fn fallback_narrow32(result: *mut [u8; 16], a: *const [u8; 16]) {
    unsafe {
        let va: [u32; 4] = std::mem::transmute(*a);
        let mut out = [0u16; 8];
        for i in 0..4 {
            out[i] = va[i] as u16;
        }
        *result = std::mem::transmute(out);
    }
}

extern "C" fn fallback_narrow64(result: *mut [u8; 16], a: *const [u8; 16]) {
    unsafe {
        let va: [u64; 2] = std::mem::transmute(*a);
        let mut out = [0u32; 4];
        for i in 0..2 {
            out[i] = va[i] as u32;
        }
        *result = std::mem::transmute(out);
    }
}

// Narrow16: truncate 8×u16 from a to 8×u8 in the low half, zero upper half.
pub fn emit_vector_narrow16(ctx: &EmitContext, ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    if ctx.has_host_feature(HostFeature::AVX512_ORTHO | HostFeature::AVX512BW) {
        let a = ra.use_xmm(&mut args[0]);
        let result = ra.scratch_xmm();
        ra.asm.vpmovwb(result, a).unwrap();
        ra.define_value(inst_ref, result);
        return;
    }
    let result = ra.use_scratch_xmm(&mut args[0]);
    let zeros = ra.scratch_xmm();
    let narrow_mask = ra
        .constant_pool
        .as_mut()
        .expect("constant pool required")
        .get_constant(0x00ff_00ff_00ff_00ff, 0x00ff_00ff_00ff_00ff);
    ra.asm.pxor(zeros, zeros).unwrap();
    ra.asm
        .pand(result, rxbyak::xmmword_ptr(narrow_mask))
        .unwrap();
    ra.asm.packuswb(result, zeros).unwrap();
    ra.release(zeros);
    ra.define_value(inst_ref, result);
}

// Narrow32: truncate 4×u32 to 4×u16 in the low half, zero upper half.
pub fn emit_vector_narrow32(ctx: &EmitContext, ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    if ctx.has_host_feature(HostFeature::AVX512_ORTHO) {
        let a = ra.use_xmm(&mut args[0]);
        let result = ra.scratch_xmm();
        ra.asm.vpmovdw(result, a).unwrap();
        ra.define_value(inst_ref, result);
        return;
    }
    let result = ra.use_scratch_xmm(&mut args[0]);
    let zeros = ra.scratch_xmm();
    ra.asm.pxor(zeros, zeros).unwrap();
    if ctx.has_host_feature(HostFeature::SSE41) {
        ra.asm.pblendw(result, zeros, 0xaa).unwrap();
        ra.asm.packusdw(result, zeros).unwrap();
    } else {
        ra.asm.pslld_imm(result, 16).unwrap();
        ra.asm.psrad_imm(result, 16).unwrap();
        ra.asm.packssdw(result, zeros).unwrap();
    }
    ra.release(zeros);
    ra.define_value(inst_ref, result);
}

// Narrow64: truncate 2×u64 to 2×u32 in the low half, zero upper half.
pub fn emit_vector_narrow64(ctx: &EmitContext, ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    if ctx.has_host_feature(HostFeature::AVX512_ORTHO) {
        let a = ra.use_xmm(&mut args[0]);
        let result = ra.scratch_xmm();
        ra.asm.vpmovqd(result, a).unwrap();
        ra.define_value(inst_ref, result);
        return;
    }
    let result = ra.use_scratch_xmm(&mut args[0]);
    let zeros = ra.scratch_xmm();
    ra.asm.pxor(zeros, zeros).unwrap();
    ra.asm.shufps(result, zeros, 0x08).unwrap();
    ra.release(zeros);
    ra.define_value(inst_ref, result);
}

// ---------------------------------------------------------------------------
// VectorSignExtend — native SSE4.1: pmovsxbw/wd/dq
// VectorSignExtend64 — fallback
// ---------------------------------------------------------------------------

pub fn emit_vector_sign_extend8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_vector_unary_op(ra, inst_ref, inst, rxbyak::CodeAssembler::pmovsxbw);
}
pub fn emit_vector_sign_extend16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_vector_unary_op(ra, inst_ref, inst, rxbyak::CodeAssembler::pmovsxwd);
}
pub fn emit_vector_sign_extend32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_vector_unary_op(ra, inst_ref, inst, rxbyak::CodeAssembler::pmovsxdq);
}

extern "C" fn fallback_sign_extend64(result: *mut [u8; 16], a: *const [u8; 16]) {
    unsafe {
        let va: [i32; 4] = std::mem::transmute(*a);
        let out: [i64; 2] = [va[0] as i64, va[1] as i64];
        *result = std::mem::transmute(out);
    }
}

// SignExtend64: i32[0..1] → i64[0..1] = pmovsxdq
pub fn emit_vector_sign_extend64(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_vector_unary_op(ra, inst_ref, inst, rxbyak::CodeAssembler::pmovsxdq);
}

// ---------------------------------------------------------------------------
// VectorZeroExtend — native SSE4.1: pmovzxbw/wd/dq
// VectorZeroExtend64 — fallback
// ---------------------------------------------------------------------------

pub fn emit_vector_zero_extend8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_vector_unary_op(ra, inst_ref, inst, rxbyak::CodeAssembler::pmovzxbw);
}
pub fn emit_vector_zero_extend16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_vector_unary_op(ra, inst_ref, inst, rxbyak::CodeAssembler::pmovzxwd);
}
pub fn emit_vector_zero_extend32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_vector_unary_op(ra, inst_ref, inst, rxbyak::CodeAssembler::pmovzxdq);
}

extern "C" fn fallback_zero_extend64(result: *mut [u8; 16], a: *const [u8; 16]) {
    unsafe {
        let va: [u64; 2] = std::mem::transmute(*a);
        let out: [u64; 2] = [va[0], 0];
        *result = std::mem::transmute(out);
    }
}

// ZeroExtend64: preserve the low u64 and clear the high u64.
pub fn emit_vector_zero_extend64(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let result = ra.use_scratch_xmm(&mut args[0]);
    let zero = ra.scratch_xmm();
    ra.asm.pxor(zero, zero).unwrap();
    ra.asm.punpcklqdq(result, zero).unwrap();
    ra.release(zero);
    ra.define_value(inst_ref, result);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fn_signatures() {
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_get_element8;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_set_element64;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_broadcast8;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_broadcast_lower32;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_extract;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_extract_lower;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) =
            emit_vector_rotate_whole_vector_right;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_interleave_lower8;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_interleave_upper64;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_deinterleave_even8;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_deinterleave_odd64;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_transpose8;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_shuffle_words;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_narrow16;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_sign_extend8;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_zero_extend64;
    }

    #[test]
    fn whole_vector_rotate_uses_upstream_pshufd_controls() {
        assert_eq!(whole_vector_rotate_shuffle_imm(0), 0b11_10_01_00);
        assert_eq!(whole_vector_rotate_shuffle_imm(32), 0b00_11_10_01);
        assert_eq!(whole_vector_rotate_shuffle_imm(64), 0b01_00_11_10);
        assert_eq!(whole_vector_rotate_shuffle_imm(96), 0b10_01_00_11);
    }
}
