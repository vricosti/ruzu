#![allow(
    clippy::missing_transmute_annotations,
    clippy::useless_transmute,
    unnecessary_transmutes
)]

use crate::backend::x64::emit_context::EmitContext;
use crate::backend::x64::emit_vector_helpers::*;
use crate::backend::x64::reg_alloc::RegAlloc;
use crate::ir::inst::Inst;
use crate::ir::value::InstRef;

// ---------------------------------------------------------------------------
// VectorMultiply — native SSE for 16/32; fallback for 8/64
// ---------------------------------------------------------------------------

// VectorMultiply8: no pmullb in SSE, use pmullw on pairs then mask
// Upstream pattern:
//   tmp_a = a; tmp_b = b
//   pmullw(a, b)         — multiply even bytes as words
//   psrlw(tmp_a, 8); psrlw(tmp_b, 8)  — shift odd bytes to low position
//   pmullw(tmp_a, tmp_b) — multiply odd bytes
//   pand(a, mask_00FF)   — keep low byte of each word (even results)
//   psllw(tmp_a, 8)      — shift odd results to high byte
//   por(a, tmp_a)        — merge even and odd results
pub fn emit_vector_multiply8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let result = ra.use_scratch_xmm(&mut args[0]); // a
    let b = ra.use_xmm(&mut args[1]);
    let tmp_a = ra.scratch_xmm();
    let tmp_b = ra.scratch_xmm();
    // Save copies for odd-byte multiplication
    ra.asm.movaps(tmp_a, result).unwrap(); // tmp_a = a
    ra.asm.movaps(tmp_b, b).unwrap(); // tmp_b = b
                                      // Even bytes: pmullw(a, b) — multiplies pairs of bytes, low word contains lo byte product
    ra.asm.pmullw(result, b).unwrap();
    // Odd bytes: shift right by 8 to move odd bytes to even position
    ra.asm.psrlw_imm(tmp_a, 8).unwrap();
    ra.asm.psrlw_imm(tmp_b, 8).unwrap();
    ra.asm.pmullw(tmp_a, tmp_b).unwrap();
    // Mask even results to low bytes
    let pool = ra.constant_pool.as_mut().expect("constant pool required");
    let mask_addr = pool.get_constant(0x00FF_00FF_00FF_00FF, 0x00FF_00FF_00FF_00FF);
    ra.asm.pand(result, rxbyak::xmmword_ptr(mask_addr)).unwrap();
    // Shift odd results to high byte position
    ra.asm.psllw_imm(tmp_a, 8).unwrap();
    // Merge
    ra.asm.por(result, tmp_a).unwrap();
    ra.release(tmp_a);
    ra.release(tmp_b);
    ra.define_value(inst_ref, result);
}

pub fn emit_vector_multiply16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_vector_op(ra, inst_ref, inst, rxbyak::CodeAssembler::pmullw);
}
pub fn emit_vector_multiply32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_vector_op(ra, inst_ref, inst, rxbyak::CodeAssembler::pmulld);
}

// VectorMultiply64: use pmuludq chain for lower 64 bits of product
// Upstream SSE4.1: pextract/pinsert with imul. Keep fallback for now.
extern "C" fn fallback_multiply64(result: *mut [u8; 16], a: *const [u8; 16], b: *const [u8; 16]) {
    unsafe {
        let va: [u64; 2] = std::mem::transmute(*a);
        let vb: [u64; 2] = std::mem::transmute(*b);
        let out: [u64; 2] = [va[0].wrapping_mul(vb[0]), va[1].wrapping_mul(vb[1])];
        *result = std::mem::transmute(out);
    }
}

pub fn emit_vector_multiply64(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_multiply64 as usize);
}

// ---------------------------------------------------------------------------
// VectorMultiplySignedWiden — fallback (widening multiply)
// ---------------------------------------------------------------------------

extern "C" fn fallback_mul_signed_widen8(
    result: *mut [u8; 16],
    a: *const [u8; 16],
    b: *const [u8; 16],
) {
    unsafe {
        let va: [i8; 16] = std::mem::transmute(*a);
        let vb: [i8; 16] = std::mem::transmute(*b);
        let mut out = [0i16; 8];
        for i in 0..8 {
            out[i] = (va[i] as i16) * (vb[i] as i16);
        }
        *result = std::mem::transmute(out);
    }
}

extern "C" fn fallback_mul_signed_widen16(
    result: *mut [u8; 16],
    a: *const [u8; 16],
    b: *const [u8; 16],
) {
    unsafe {
        let va: [i16; 8] = std::mem::transmute(*a);
        let vb: [i16; 8] = std::mem::transmute(*b);
        let mut out = [0i32; 4];
        for i in 0..4 {
            out[i] = (va[i] as i32) * (vb[i] as i32);
        }
        *result = std::mem::transmute(out);
    }
}

extern "C" fn fallback_mul_signed_widen32(
    result: *mut [u8; 16],
    a: *const [u8; 16],
    b: *const [u8; 16],
) {
    unsafe {
        let va: [i32; 4] = std::mem::transmute(*a);
        let vb: [i32; 4] = std::mem::transmute(*b);
        let mut out = [0i64; 2];
        for i in 0..2 {
            out[i] = (va[i] as i64) * (vb[i] as i64);
        }
        *result = std::mem::transmute(out);
    }
}

pub fn emit_vector_multiply_signed_widen8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_mul_signed_widen8 as usize);
}
// VectorMultiplySignedWiden16: pmullw + pmulhw + punpcklwd
// IR: result = {a[0]*b[0], a[1]*b[1], a[2]*b[2], a[3]*b[3]} as i32x4
pub fn emit_vector_multiply_signed_widen16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let a = ra.use_xmm(&mut args[0]);
    let b = ra.use_xmm(&mut args[1]);
    let lo = ra.scratch_xmm();
    let result = ra.scratch_xmm();
    // lo = pmullw(a, b) — low 16 bits of each 16×16 product
    ra.asm.movaps(lo, a).unwrap();
    ra.asm.pmullw(lo, b).unwrap();
    // hi = pmulhw(a, b) — high 16 bits of each 16×16 product
    ra.asm.movaps(result, a).unwrap();
    ra.asm.pmulhw(result, b).unwrap();
    // Interleave low 4 words: punpcklwd(lo, hi) → {lo0,hi0,lo1,hi1,lo2,hi2,lo3,hi3}
    // This gives us 4 packed 32-bit products
    ra.asm.punpcklwd(lo, result).unwrap();
    ra.asm.movaps(result, lo).unwrap();
    ra.release(lo);
    ra.define_value(inst_ref, result);
}
// VectorMultiplySignedWiden32: SSE4.1 pmuldq
// IR: result = {a[0]*b[0], a[1]*b[1]} as i64x2
// pmuldq gives {a[0]*b[0], a[2]*b[2]} — need to shuffle a[1]→a[2], b[1]→b[2]
pub fn emit_vector_multiply_signed_widen32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let a = ra.use_xmm(&mut args[0]);
    let b = ra.use_xmm(&mut args[1]);
    let result = ra.scratch_xmm();
    let b_shuf = ra.scratch_xmm();
    // Shuffle: move dword[1] to dword[2]: pshufd(_, _, 0b_01_01_00_00) = 0x50
    // dword[0] stays at [0], dword[1] goes to [2]
    ra.asm.pshufd(result, a, 0x50).unwrap(); // result = {a[0], a[0], a[1], a[1]}
    ra.asm.pshufd(b_shuf, b, 0x50).unwrap(); // b_shuf = {b[0], b[0], b[1], b[1]}
    ra.asm.pmuldq(result, b_shuf).unwrap(); // result = {a[0]*b[0], a[1]*b[1]}
    ra.release(b_shuf);
    ra.define_value(inst_ref, result);
}

// ---------------------------------------------------------------------------
// VectorMultiplyUnsignedWiden — fallback
// ---------------------------------------------------------------------------

extern "C" fn fallback_mul_unsigned_widen8(
    result: *mut [u8; 16],
    a: *const [u8; 16],
    b: *const [u8; 16],
) {
    unsafe {
        let va: [u8; 16] = *a;
        let vb: [u8; 16] = *b;
        let mut out = [0u16; 8];
        for i in 0..8 {
            out[i] = (va[i] as u16) * (vb[i] as u16);
        }
        *result = std::mem::transmute(out);
    }
}

extern "C" fn fallback_mul_unsigned_widen16(
    result: *mut [u8; 16],
    a: *const [u8; 16],
    b: *const [u8; 16],
) {
    unsafe {
        let va: [u16; 8] = std::mem::transmute(*a);
        let vb: [u16; 8] = std::mem::transmute(*b);
        let mut out = [0u32; 4];
        for i in 0..4 {
            out[i] = (va[i] as u32) * (vb[i] as u32);
        }
        *result = std::mem::transmute(out);
    }
}

extern "C" fn fallback_mul_unsigned_widen32(
    result: *mut [u8; 16],
    a: *const [u8; 16],
    b: *const [u8; 16],
) {
    unsafe {
        let va: [u32; 4] = std::mem::transmute(*a);
        let vb: [u32; 4] = std::mem::transmute(*b);
        let mut out = [0u64; 2];
        for i in 0..2 {
            out[i] = (va[i] as u64) * (vb[i] as u64);
        }
        *result = std::mem::transmute(out);
    }
}

pub fn emit_vector_multiply_unsigned_widen8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_mul_unsigned_widen8 as usize);
}
// VectorMultiplyUnsignedWiden16: pmullw + pmulhuw + punpcklwd
pub fn emit_vector_multiply_unsigned_widen16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let a = ra.use_xmm(&mut args[0]);
    let b = ra.use_xmm(&mut args[1]);
    let lo = ra.scratch_xmm();
    let result = ra.scratch_xmm();
    // lo = pmullw(a, b) — low 16 bits of each unsigned 16×16 product
    ra.asm.movaps(lo, a).unwrap();
    ra.asm.pmullw(lo, b).unwrap();
    // hi = pmulhuw(a, b) — high 16 bits of each unsigned 16×16 product
    ra.asm.movaps(result, a).unwrap();
    ra.asm.pmulhuw(result, b).unwrap();
    // Interleave: punpcklwd(lo, hi) → {lo0,hi0,lo1,hi1,lo2,hi2,lo3,hi3}
    ra.asm.punpcklwd(lo, result).unwrap();
    ra.asm.movaps(result, lo).unwrap();
    ra.release(lo);
    ra.define_value(inst_ref, result);
}
// VectorMultiplyUnsignedWiden32: SSE2 pmuludq (same shuffle trick as signed)
pub fn emit_vector_multiply_unsigned_widen32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let a = ra.use_xmm(&mut args[0]);
    let b = ra.use_xmm(&mut args[1]);
    let result = ra.scratch_xmm();
    let b_shuf = ra.scratch_xmm();
    // pshufd with 0x50: {d[0], d[0], d[1], d[1]}
    ra.asm.pshufd(result, a, 0x50).unwrap();
    ra.asm.pshufd(b_shuf, b, 0x50).unwrap();
    ra.asm.pmuludq(result, b_shuf).unwrap();
    ra.release(b_shuf);
    ra.define_value(inst_ref, result);
}

// ---------------------------------------------------------------------------
// VectorSignedMultiplyLong — fallback
// ---------------------------------------------------------------------------

extern "C" fn fallback_signed_mul_long16(
    result: *mut [u8; 16],
    a: *const [u8; 16],
    b: *const [u8; 16],
) {
    unsafe {
        let va: [i16; 8] = std::mem::transmute(*a);
        let vb: [i16; 8] = std::mem::transmute(*b);
        let mut out = [0i32; 4];
        for i in 0..4 {
            out[i] = (va[i] as i32) * (vb[i] as i32);
        }
        *result = std::mem::transmute(out);
    }
}

extern "C" fn fallback_signed_mul_long32(
    result: *mut [u8; 16],
    a: *const [u8; 16],
    b: *const [u8; 16],
) {
    unsafe {
        let va: [i32; 4] = std::mem::transmute(*a);
        let vb: [i32; 4] = std::mem::transmute(*b);
        let mut out = [0i64; 2];
        for i in 0..2 {
            out[i] = (va[i] as i64) * (vb[i] as i64);
        }
        *result = std::mem::transmute(out);
    }
}

pub fn emit_vector_signed_multiply_long16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_signed_mul_long16 as usize);
}
pub fn emit_vector_signed_multiply_long32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_signed_mul_long32 as usize);
}

// ---------------------------------------------------------------------------
// VectorUnsignedMultiplyLong — fallback
// ---------------------------------------------------------------------------

extern "C" fn fallback_unsigned_mul_long16(
    result: *mut [u8; 16],
    a: *const [u8; 16],
    b: *const [u8; 16],
) {
    unsafe {
        let va: [u16; 8] = std::mem::transmute(*a);
        let vb: [u16; 8] = std::mem::transmute(*b);
        let mut out = [0u32; 4];
        for i in 0..4 {
            out[i] = (va[i] as u32) * (vb[i] as u32);
        }
        *result = std::mem::transmute(out);
    }
}

extern "C" fn fallback_unsigned_mul_long32(
    result: *mut [u8; 16],
    a: *const [u8; 16],
    b: *const [u8; 16],
) {
    unsafe {
        let va: [u32; 4] = std::mem::transmute(*a);
        let vb: [u32; 4] = std::mem::transmute(*b);
        let mut out = [0u64; 2];
        for i in 0..2 {
            out[i] = (va[i] as u64) * (vb[i] as u64);
        }
        *result = std::mem::transmute(out);
    }
}

pub fn emit_vector_unsigned_multiply_long16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_unsigned_mul_long16 as usize);
}
pub fn emit_vector_unsigned_multiply_long32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_unsigned_mul_long32 as usize);
}

// ---------------------------------------------------------------------------
// VectorPolynomialMultiply — fallback (GF(2) multiplication)
// ---------------------------------------------------------------------------

extern "C" fn fallback_poly_mul8(result: *mut [u8; 16], a: *const [u8; 16], b: *const [u8; 16]) {
    unsafe {
        let va = &*a;
        let vb = &*b;
        let dst = &mut *result;
        for i in 0..16 {
            let mut r = 0u8;
            for bit in 0..8 {
                if (vb[i] >> bit) & 1 != 0 {
                    r ^= va[i] << bit;
                }
            }
            dst[i] = r;
        }
    }
}

extern "C" fn fallback_poly_mul_long8(
    result: *mut [u8; 16],
    a: *const [u8; 16],
    b: *const [u8; 16],
) {
    unsafe {
        let va = &*a;
        let vb = &*b;
        let mut out = [0u16; 8];
        for i in 0..8 {
            let mut r = 0u16;
            for bit in 0..8 {
                if (vb[i] >> bit) & 1 != 0 {
                    r ^= (va[i] as u16) << bit;
                }
            }
            out[i] = r;
        }
        *result = std::mem::transmute(out);
    }
}

extern "C" fn fallback_poly_mul_long64(
    result: *mut [u8; 16],
    a: *const [u8; 16],
    b: *const [u8; 16],
) {
    unsafe {
        let va: [u64; 2] = std::mem::transmute(*a);
        let vb: [u64; 2] = std::mem::transmute(*b);
        let mut r = 0u128;
        for bit in 0..64 {
            if (vb[0] >> bit) & 1 != 0 {
                r ^= (va[0] as u128) << bit;
            }
        }
        *result = std::mem::transmute(r);
    }
}

pub fn emit_vector_polynomial_multiply8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_poly_mul8 as usize);
}
pub fn emit_vector_polynomial_multiply_long8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_poly_mul_long8 as usize);
}
pub fn emit_vector_polynomial_multiply_long64(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_poly_mul_long64 as usize);
}

// ---------------------------------------------------------------------------
// VectorPairedAdd — fallback
// ---------------------------------------------------------------------------

macro_rules! define_paired_add {
    ($name:ident, $ty:ty, $count:expr) => {
        extern "C" fn $name(result: *mut [u8; 16], a: *const [u8; 16], b: *const [u8; 16]) {
            unsafe {
                let va: [$ty; $count] = std::mem::transmute(*a);
                let vb: [$ty; $count] = std::mem::transmute(*b);
                let mut out = [0 as $ty; $count];
                let half = $count / 2;
                for i in 0..half {
                    out[i] = va[i * 2].wrapping_add(va[i * 2 + 1]);
                }
                for i in 0..half {
                    out[half + i] = vb[i * 2].wrapping_add(vb[i * 2 + 1]);
                }
                *result = std::mem::transmute(out);
            }
        }
    };
}

define_paired_add!(fallback_paired_add8, u8, 16);
define_paired_add!(fallback_paired_add16, u16, 8);
define_paired_add!(fallback_paired_add32, u32, 4);
define_paired_add!(fallback_paired_add64, u64, 2);

pub fn emit_vector_paired_add8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    if std::env::var_os("RUZU_FORCE_PAIRED_ADD8_FALLBACK").is_some() {
        emit_two_arg_fallback(ra, inst_ref, inst, fallback_paired_add8 as usize);
        return;
    }
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let a = ra.use_scratch_xmm(&mut args[0]);
    let b = ra.use_scratch_xmm(&mut args[1]);
    let c = ra.scratch_xmm();
    let d = ra.scratch_xmm();

    ra.asm.movaps(c, a).unwrap();
    ra.asm.movaps(d, b).unwrap();
    ra.asm.psllw_imm(a, 8).unwrap();
    ra.asm.psllw_imm(b, 8).unwrap();
    ra.asm.paddw(a, c).unwrap();
    ra.asm.paddw(b, d).unwrap();
    ra.asm.psrlw_imm(a, 8).unwrap();
    ra.asm.psrlw_imm(b, 8).unwrap();
    ra.asm.packuswb(a, b).unwrap();

    ra.release(c);
    ra.release(d);
    ra.define_value(inst_ref, a);
}
// VectorPairedAdd16: SSSE3 phaddw (horizontal add packed words)
pub fn emit_vector_paired_add16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let result = ra.use_scratch_xmm(&mut args[0]);
    let b = ra.use_xmm(&mut args[1]);
    ra.asm.phaddw(result, b).unwrap();
    ra.define_value(inst_ref, result);
}
// VectorPairedAdd32: SSSE3 phaddd (horizontal add packed dwords)
pub fn emit_vector_paired_add32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let result = ra.use_scratch_xmm(&mut args[0]);
    let b = ra.use_xmm(&mut args[1]);
    ra.asm.phaddd(result, b).unwrap();
    ra.define_value(inst_ref, result);
}
pub fn emit_vector_paired_add64(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_paired_add64 as usize);
}

// ---------------------------------------------------------------------------
// VectorPairedAddLower — fallback (lower half only)
// ---------------------------------------------------------------------------

// D-form (64-bit) paired add. Mirrors upstream `LowerPairedOperation`:
// pairs reduce HALF of each source vector (lower 64 bits) into HALF of the
// destination, with upper destination lanes zeroed. For u8 (count=16):
// range=4 pairs per input → 8 output lanes set, upper 8 zero.
macro_rules! define_paired_add_lower {
    ($name:ident, $ty:ty, $count:expr) => {
        extern "C" fn $name(result: *mut [u8; 16], a: *const [u8; 16], b: *const [u8; 16]) {
            unsafe {
                let va: [$ty; $count] = std::mem::transmute(*a);
                let vb: [$ty; $count] = std::mem::transmute(*b);
                let mut out = [0 as $ty; $count];
                let range = $count / 4;
                for i in 0..range {
                    out[i] = va[2 * i].wrapping_add(va[2 * i + 1]);
                }
                for i in 0..range {
                    out[range + i] = vb[2 * i].wrapping_add(vb[2 * i + 1]);
                }
                *result = std::mem::transmute(out);
            }
        }
    };
}

define_paired_add_lower!(fallback_paired_add_lower8, u8, 16);
define_paired_add_lower!(fallback_paired_add_lower16, u16, 8);
define_paired_add_lower!(fallback_paired_add_lower32, u32, 4);

pub fn emit_vector_paired_add_lower8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_paired_add_lower8 as usize);
}
pub fn emit_vector_paired_add_lower16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_paired_add_lower16 as usize);
}
pub fn emit_vector_paired_add_lower32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_paired_add_lower32 as usize);
}

// ---------------------------------------------------------------------------
// VectorPairedAddSignedWiden — fallback
// ---------------------------------------------------------------------------

extern "C" fn fallback_paired_add_signed_widen8(result: *mut [u8; 16], a: *const [u8; 16]) {
    unsafe {
        let va: [i8; 16] = std::mem::transmute(*a);
        let mut out = [0i16; 8];
        for i in 0..8 {
            out[i] = (va[i * 2] as i16) + (va[i * 2 + 1] as i16);
        }
        *result = std::mem::transmute(out);
    }
}

extern "C" fn fallback_paired_add_signed_widen16(result: *mut [u8; 16], a: *const [u8; 16]) {
    unsafe {
        let va: [i16; 8] = std::mem::transmute(*a);
        let mut out = [0i32; 4];
        for i in 0..4 {
            out[i] = (va[i * 2] as i32) + (va[i * 2 + 1] as i32);
        }
        *result = std::mem::transmute(out);
    }
}

extern "C" fn fallback_paired_add_signed_widen32(result: *mut [u8; 16], a: *const [u8; 16]) {
    unsafe {
        let va: [i32; 4] = std::mem::transmute(*a);
        let mut out = [0i64; 2];
        for i in 0..2 {
            out[i] = (va[i * 2] as i64) + (va[i * 2 + 1] as i64);
        }
        *result = std::mem::transmute(out);
    }
}

pub fn emit_vector_paired_add_signed_widen8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_one_arg_fallback(
        ra,
        inst_ref,
        inst,
        fallback_paired_add_signed_widen8 as usize,
    );
}
// PairedAddSignedWiden16: pmaddwd(a, ones) — multiply each word by 1, add pairs → i32
pub fn emit_vector_paired_add_signed_widen16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let result = ra.use_scratch_xmm(&mut args[0]);
    let pool = ra.constant_pool.as_mut().expect("constant pool required");
    let ones = pool.get_constant(0x0001_0001_0001_0001u64, 0x0001_0001_0001_0001u64);
    ra.asm.pmaddwd(result, rxbyak::xmmword_ptr(ones)).unwrap();
    ra.define_value(inst_ref, result);
}
pub fn emit_vector_paired_add_signed_widen32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_one_arg_fallback(
        ra,
        inst_ref,
        inst,
        fallback_paired_add_signed_widen32 as usize,
    );
}

// ---------------------------------------------------------------------------
// VectorPairedAddUnsignedWiden — fallback
// ---------------------------------------------------------------------------

extern "C" fn fallback_paired_add_unsigned_widen8(result: *mut [u8; 16], a: *const [u8; 16]) {
    unsafe {
        let va: [u8; 16] = *a;
        let mut out = [0u16; 8];
        for i in 0..8 {
            out[i] = (va[i * 2] as u16) + (va[i * 2 + 1] as u16);
        }
        *result = std::mem::transmute(out);
    }
}

extern "C" fn fallback_paired_add_unsigned_widen16(result: *mut [u8; 16], a: *const [u8; 16]) {
    unsafe {
        let va: [u16; 8] = std::mem::transmute(*a);
        let mut out = [0u32; 4];
        for i in 0..4 {
            out[i] = (va[i * 2] as u32) + (va[i * 2 + 1] as u32);
        }
        *result = std::mem::transmute(out);
    }
}

extern "C" fn fallback_paired_add_unsigned_widen32(result: *mut [u8; 16], a: *const [u8; 16]) {
    unsafe {
        let va: [u32; 4] = std::mem::transmute(*a);
        let mut out = [0u64; 2];
        for i in 0..2 {
            out[i] = (va[i * 2] as u64) + (va[i * 2 + 1] as u64);
        }
        *result = std::mem::transmute(out);
    }
}

// PairedAddUnsignedWiden8: pmaddubsw(a, ones) — treat a as unsigned, ones as signed 1
// pmaddubsw: result[i] = saturate(a[2i]*b[2i] + a[2i+1]*b[2i+1]) unsigned*signed→signed16
// With b=1: result[i] = a[2i] + a[2i+1] (unsigned sum fits in signed 16-bit since max = 510)
pub fn emit_vector_paired_add_unsigned_widen8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let result = ra.use_scratch_xmm(&mut args[0]);
    let pool = ra.constant_pool.as_mut().expect("constant pool required");
    let ones = pool.get_constant(0x01_01_01_01_01_01_01_01u64, 0x01_01_01_01_01_01_01_01u64);
    ra.asm.pmaddubsw(result, rxbyak::xmmword_ptr(ones)).unwrap();
    ra.define_value(inst_ref, result);
}
pub fn emit_vector_paired_add_unsigned_widen16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_one_arg_fallback(
        ra,
        inst_ref,
        inst,
        fallback_paired_add_unsigned_widen16 as usize,
    );
}
pub fn emit_vector_paired_add_unsigned_widen32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_one_arg_fallback(
        ra,
        inst_ref,
        inst,
        fallback_paired_add_unsigned_widen32 as usize,
    );
}

// ---------------------------------------------------------------------------
// VectorPairedMax/Min — fallback
// ---------------------------------------------------------------------------

macro_rules! define_paired_minmax {
    ($name:ident, $ty:ty, $count:expr, $op:ident) => {
        extern "C" fn $name(result: *mut [u8; 16], a: *const [u8; 16], b: *const [u8; 16]) {
            unsafe {
                let va: [$ty; $count] = std::mem::transmute(*a);
                let vb: [$ty; $count] = std::mem::transmute(*b);
                let mut out = [0 as $ty; $count];
                let half = $count / 2;
                for i in 0..half {
                    out[i] = va[i * 2].$op(va[i * 2 + 1]);
                }
                for i in 0..half {
                    out[half + i] = vb[i * 2].$op(vb[i * 2 + 1]);
                }
                *result = std::mem::transmute(out);
            }
        }
    };
}

define_paired_minmax!(fallback_paired_max_s8, i8, 16, max);
define_paired_minmax!(fallback_paired_max_s16, i16, 8, max);
define_paired_minmax!(fallback_paired_max_s32, i32, 4, max);
define_paired_minmax!(fallback_paired_max_u8, u8, 16, max);
define_paired_minmax!(fallback_paired_max_u16, u16, 8, max);
define_paired_minmax!(fallback_paired_max_u32, u32, 4, max);
define_paired_minmax!(fallback_paired_min_s8, i8, 16, min);
define_paired_minmax!(fallback_paired_min_s16, i16, 8, min);
define_paired_minmax!(fallback_paired_min_s32, i32, 4, min);
define_paired_minmax!(fallback_paired_min_u8, u8, 16, min);
define_paired_minmax!(fallback_paired_min_u16, u16, 8, min);
define_paired_minmax!(fallback_paired_min_u32, u32, 4, min);

// D-form (64-bit) paired min/max. Mirrors upstream `LowerPairedOperation`
// (emit_x64_vector.cpp:2750-2761): pairs reduce HALF of each source vector
// (the lower 64 bits) into HALF of the destination, with upper destination
// lanes zeroed. For u8 (count=16): range=4 pairs per input → 8 output lanes
// set (4 from a + 4 from b), upper 8 zero. The previous implementation only
// emitted 2 output lanes (one pair per input), producing wrong results for
// AArch64 `umaxp/uminp v.8b, v.8b, v.8b` which libnx string functions and
// fsdev path handling rely on.
macro_rules! define_paired_minmax_lower {
    ($name:ident, $ty:ty, $count:expr, $func:ident) => {
        extern "C" fn $name(result: *mut [u8; 16], a: *const [u8; 16], b: *const [u8; 16]) {
            unsafe {
                let va: [$ty; $count] = std::mem::transmute(*a);
                let vb: [$ty; $count] = std::mem::transmute(*b);
                let mut out = [0 as $ty; $count];
                let range = $count / 4;
                for i in 0..range {
                    out[i] = std::cmp::$func(va[2 * i], va[2 * i + 1]);
                }
                for i in 0..range {
                    out[range + i] = std::cmp::$func(vb[2 * i], vb[2 * i + 1]);
                }
                *result = std::mem::transmute(out);
            }
        }
    };
}

define_paired_minmax_lower!(fallback_paired_max_lower_s8, i8, 16, max);
define_paired_minmax_lower!(fallback_paired_max_lower_s16, i16, 8, max);
define_paired_minmax_lower!(fallback_paired_max_lower_s32, i32, 4, max);
define_paired_minmax_lower!(fallback_paired_max_lower_u8, u8, 16, max);
define_paired_minmax_lower!(fallback_paired_max_lower_u16, u16, 8, max);
define_paired_minmax_lower!(fallback_paired_max_lower_u32, u32, 4, max);
define_paired_minmax_lower!(fallback_paired_min_lower_s8, i8, 16, min);
define_paired_minmax_lower!(fallback_paired_min_lower_s16, i16, 8, min);
define_paired_minmax_lower!(fallback_paired_min_lower_s32, i32, 4, min);
define_paired_minmax_lower!(fallback_paired_min_lower_u8, u8, 16, min);
define_paired_minmax_lower!(fallback_paired_min_lower_u16, u16, 8, min);
define_paired_minmax_lower!(fallback_paired_min_lower_u32, u32, 4, min);

pub fn emit_vector_paired_max_signed8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_paired_max_s8 as usize);
}
pub fn emit_vector_paired_max_signed16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_paired_max_s16 as usize);
}
pub fn emit_vector_paired_max_signed32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_paired_max_s32 as usize);
}
pub fn emit_vector_paired_max_unsigned8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    if std::env::var_os("RUZU_FORCE_PAIRED_MAX_U8_FALLBACK").is_some() {
        emit_two_arg_fallback(ra, inst_ref, inst, fallback_paired_max_u8 as usize);
        return;
    }
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let x = ra.use_scratch_xmm(&mut args[0]);
    let y = ra.use_scratch_xmm(&mut args[1]);
    let tmp = ra.scratch_xmm();

    let pool = ra.constant_pool.as_mut().expect("constant pool required");
    let shuffle_mask =
        pool.get_constant(0x0E_0C_0A_08_06_04_02_00u64, 0x0F_0D_0B_09_07_05_03_01u64);

    ra.asm.pshufb(x, rxbyak::xmmword_ptr(shuffle_mask)).unwrap();
    ra.asm.pshufb(y, rxbyak::xmmword_ptr(shuffle_mask)).unwrap();
    ra.asm.movaps(tmp, x).unwrap();
    ra.asm.shufps(tmp, y, 0b01_00_01_00).unwrap();
    ra.asm.shufps(x, y, 0b11_10_11_10).unwrap();
    ra.asm.pmaxub(x, tmp).unwrap();

    ra.release(tmp);
    ra.define_value(inst_ref, x);
}
pub fn emit_vector_paired_max_unsigned16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_paired_max_u16 as usize);
}
pub fn emit_vector_paired_max_unsigned32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_paired_max_u32 as usize);
}
pub fn emit_vector_paired_max_signed_lower8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_paired_max_lower_s8 as usize);
}
pub fn emit_vector_paired_max_signed_lower16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_paired_max_lower_s16 as usize);
}
pub fn emit_vector_paired_max_signed_lower32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_paired_max_lower_s32 as usize);
}
pub fn emit_vector_paired_max_unsigned_lower8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_paired_max_lower_u8 as usize);
}
pub fn emit_vector_paired_max_unsigned_lower16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_paired_max_lower_u16 as usize);
}
pub fn emit_vector_paired_max_unsigned_lower32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_paired_max_lower_u32 as usize);
}
pub fn emit_vector_paired_min_signed8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_paired_min_s8 as usize);
}
pub fn emit_vector_paired_min_signed16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_paired_min_s16 as usize);
}
pub fn emit_vector_paired_min_signed32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_paired_min_s32 as usize);
}
pub fn emit_vector_paired_min_unsigned8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_paired_min_u8 as usize);
}
pub fn emit_vector_paired_min_unsigned16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_paired_min_u16 as usize);
}
pub fn emit_vector_paired_min_unsigned32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_paired_min_u32 as usize);
}
pub fn emit_vector_paired_min_signed_lower8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_paired_min_lower_s8 as usize);
}
pub fn emit_vector_paired_min_signed_lower16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_paired_min_lower_s16 as usize);
}
pub fn emit_vector_paired_min_signed_lower32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_paired_min_lower_s32 as usize);
}
pub fn emit_vector_paired_min_unsigned_lower8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_paired_min_lower_u8 as usize);
}
pub fn emit_vector_paired_min_unsigned_lower16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_paired_min_lower_u16 as usize);
}
pub fn emit_vector_paired_min_unsigned_lower32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_paired_min_lower_u32 as usize);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fn_signatures() {
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_multiply8;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_multiply16;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_multiply_signed_widen8;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) =
            emit_vector_multiply_unsigned_widen32;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_polynomial_multiply8;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_paired_add8;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_paired_add_lower32;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) =
            emit_vector_paired_add_signed_widen32;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_paired_max_signed8;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) =
            emit_vector_paired_max_signed_lower8;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_paired_min_unsigned32;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) =
            emit_vector_paired_min_unsigned_lower32;
    }

    // Test removed: fallback_multiply8 replaced with inline SSE (pmullw trick)
    // Correctness verified via a32_diff fuzzing

    #[test]
    fn fallback_paired_max_lower_u8_matches_upstream_lower_paired_operation() {
        // AArch64 `umaxp v.8b, v.8b, v.8b`: pairs the lower 64 bits of each
        // input. Upstream (emit_x64_vector.cpp:2750) uses range = count/4 = 4,
        // producing 4 pairs from `a` then 4 pairs from `b` in the lower 64
        // bits of the output, with the upper 64 bits zero.
        let mut a = [0u8; 16];
        let mut b = [0u8; 16];
        // a lower 8: 0x10..0x17
        for i in 0..8 {
            a[i] = 0x10 + i as u8;
        }
        // b lower 8: 0xA0..0xA7
        for i in 0..8 {
            b[i] = 0xA0 + i as u8;
        }
        let mut out = [0u8; 16];
        unsafe {
            fallback_paired_max_lower_u8(&mut out, &a, &b);
        }
        // Pairs from a: max(0x10,0x11)=0x11, max(0x12,0x13)=0x13,
        //               max(0x14,0x15)=0x15, max(0x16,0x17)=0x17
        // Pairs from b: max(0xA0,0xA1)=0xA1, ..., max(0xA6,0xA7)=0xA7
        let expected: [u8; 16] = [
            0x11, 0x13, 0x15, 0x17, 0xA1, 0xA3, 0xA5, 0xA7, 0, 0, 0, 0, 0, 0, 0, 0,
        ];
        assert_eq!(
            out, expected,
            "umaxp v.8b mismatch — only first 2 lanes set means the broken pre-fix implementation"
        );
    }

    #[test]
    fn fallback_paired_min_lower_u8_matches_upstream_lower_paired_operation() {
        let mut a = [0u8; 16];
        let mut b = [0u8; 16];
        for i in 0..8 {
            a[i] = 0x10 + i as u8;
        }
        for i in 0..8 {
            b[i] = 0xA0 + i as u8;
        }
        let mut out = [0u8; 16];
        unsafe {
            fallback_paired_min_lower_u8(&mut out, &a, &b);
        }
        let expected: [u8; 16] = [
            0x10, 0x12, 0x14, 0x16, 0xA0, 0xA2, 0xA4, 0xA6, 0, 0, 0, 0, 0, 0, 0, 0,
        ];
        assert_eq!(out, expected);
    }

    #[test]
    fn fallback_paired_max_lower_u16_produces_two_pairs_per_input() {
        // For u16 (count=8): range = 8/4 = 2 pairs from each.
        let mut a_bytes = [0u8; 16];
        let mut b_bytes = [0u8; 16];
        // a as u16 lower 4: [0x0001, 0x0002, 0x0003, 0x0004]
        let a_words: [u16; 4] = [0x0001, 0x0002, 0x0003, 0x0004];
        let b_words: [u16; 4] = [0x00A0, 0x00A1, 0x00A2, 0x00A3];
        a_bytes[..8].copy_from_slice(&unsafe { std::mem::transmute::<_, [u8; 8]>(a_words) });
        b_bytes[..8].copy_from_slice(&unsafe { std::mem::transmute::<_, [u8; 8]>(b_words) });
        let mut out = [0u8; 16];
        unsafe {
            fallback_paired_max_lower_u16(&mut out, &a_bytes, &b_bytes);
        }
        let out_words: [u16; 8] = unsafe { std::mem::transmute(out) };
        assert_eq!(
            out_words,
            [0x0002, 0x0004, 0x00A1, 0x00A3, 0, 0, 0, 0],
            "umaxp v.4h must produce 2 pairs from a then 2 pairs from b"
        );
    }
}
