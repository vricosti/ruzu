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
// VectorLogicalShiftLeft — native SSE for 16/32/64 (imm form)
// 8-bit has no native SSE instruction → fallback
// ---------------------------------------------------------------------------

// VectorLogicalShiftLeft8: no psllb; use psllw + mask to clear overflow bits
// Upstream pattern: psllw(data, shift), pand(data, mask) where mask clears the bits that
// overflowed from the low byte into the high byte of each word
pub fn emit_vector_logical_shift_left8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let result = ra.use_scratch_xmm(&mut args[0]);
    let shift = args[1].get_immediate_u8();
    if shift >= 8 {
        ra.asm.xorps(result, result).unwrap();
    } else if shift > 0 {
        ra.asm.psllw_imm(result, shift).unwrap();
        // Mask: for shift=1, valid bits = 0xFE per byte → mask = 0xFEFE...
        // For shift=n, mask = (0xFF << n) & 0xFF per byte
        let mask_byte = (0xFFu8 << shift) as u64;
        let mask_word = mask_byte | (mask_byte << 8);
        let mask_dword = mask_word | (mask_word << 16);
        let mask_qword = mask_dword | (mask_dword << 32);
        let pool = ra.constant_pool.as_mut().expect("constant pool required");
        let mask_addr = pool.get_constant(mask_qword, mask_qword);
        ra.asm.pand(result, rxbyak::xmmword_ptr(mask_addr)).unwrap();
    }
    // shift == 0: result is already data, no-op
    ra.define_value(inst_ref, result);
}
pub fn emit_vector_logical_shift_left16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_vector_op_imm(ra, inst_ref, inst, rxbyak::CodeAssembler::psllw_imm);
}
pub fn emit_vector_logical_shift_left32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_vector_op_imm(ra, inst_ref, inst, rxbyak::CodeAssembler::pslld_imm);
}
pub fn emit_vector_logical_shift_left64(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_vector_op_imm(ra, inst_ref, inst, rxbyak::CodeAssembler::psllq_imm);
}

// ---------------------------------------------------------------------------
// VectorLogicalShiftRight — native SSE for 16/32/64 (imm form)
// ---------------------------------------------------------------------------

// VectorLogicalShiftRight8: psrlw + mask (same pattern as LSL8)
pub fn emit_vector_logical_shift_right8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let result = ra.use_scratch_xmm(&mut args[0]);
    let shift = args[1].get_immediate_u8();
    if shift >= 8 {
        ra.asm.xorps(result, result).unwrap();
    } else if shift > 0 {
        ra.asm.psrlw_imm(result, shift).unwrap();
        // Mask: for shift=1, valid bits = 0x7F per byte
        // For shift=n, mask = 0xFF >> n per byte
        let mask_byte = (0xFFu8 >> shift) as u64;
        let mask_word = mask_byte | (mask_byte << 8);
        let mask_dword = mask_word | (mask_word << 16);
        let mask_qword = mask_dword | (mask_dword << 32);
        let pool = ra.constant_pool.as_mut().expect("constant pool required");
        let mask_addr = pool.get_constant(mask_qword, mask_qword);
        ra.asm.pand(result, rxbyak::xmmword_ptr(mask_addr)).unwrap();
    }
    ra.define_value(inst_ref, result);
}
pub fn emit_vector_logical_shift_right16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_vector_op_imm(ra, inst_ref, inst, rxbyak::CodeAssembler::psrlw_imm);
}
pub fn emit_vector_logical_shift_right32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_vector_op_imm(ra, inst_ref, inst, rxbyak::CodeAssembler::psrld_imm);
}
pub fn emit_vector_logical_shift_right64(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_vector_op_imm(ra, inst_ref, inst, rxbyak::CodeAssembler::psrlq_imm);
}

// ---------------------------------------------------------------------------
// VectorArithmeticShiftRight — native SSE for 16/32 (imm form)
// 8/64-bit have no native SSE → fallback
// ---------------------------------------------------------------------------

extern "C" fn fallback_asr8(result: *mut [u8; 16], a: *const [u8; 16], b: *const [u8; 16]) {
    unsafe {
        let src: [i8; 16] = std::mem::transmute(*a);
        let shift = (*b)[0].min(7);
        let mut out = [0i8; 16];
        for i in 0..16 {
            out[i] = src[i] >> shift;
        }
        *result = std::mem::transmute(out);
    }
}

extern "C" fn fallback_asr64(result: *mut [u8; 16], a: *const [u8; 16], b: *const [u8; 16]) {
    unsafe {
        let src: [i64; 2] = std::mem::transmute(*a);
        let shift = (*b)[0].min(63);
        let out: [i64; 2] = [src[0] >> shift, src[1] >> shift];
        *result = std::mem::transmute(out);
    }
}

// VectorArithmeticShiftRight8: psrlw + sign extension via pcmpgtb
// result = (data >> shift) | (sign_mask where data < 0)
pub fn emit_vector_arithmetic_shift_right8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let result = ra.use_scratch_xmm(&mut args[0]);
    let shift = args[1].get_immediate_u8().min(7);
    if shift == 0 {
        ra.define_value(inst_ref, result);
        return;
    }

    let data_sign = ra.scratch_xmm();
    let zero = ra.scratch_xmm();
    ra.asm.xorps(zero, zero).unwrap();

    // data_sign = 0xFF where result < 0, 0x00 where >= 0
    ra.asm.movaps(data_sign, zero).unwrap();
    ra.asm.pcmpgtb(data_sign, result).unwrap();

    // Logical shift right (word-level) then mask to byte boundaries
    ra.asm.psrlw_imm(result, shift).unwrap();
    let lsr_mask_byte = (0xFFu8 >> shift) as u64;
    let lsr_mask = lsr_mask_byte * 0x01_01_01_01_01_01_01_01u64;
    let pool = ra.constant_pool.as_mut().expect("constant pool required");
    let lsr_mask_addr = pool.get_constant(lsr_mask, lsr_mask);
    ra.asm
        .pand(result, rxbyak::xmmword_ptr(lsr_mask_addr))
        .unwrap();

    // Sign extension: OR in upper bits for negative bytes
    let sign_ext_byte = (!lsr_mask_byte) as u64 & 0xFF;
    let sign_ext = sign_ext_byte * 0x01_01_01_01_01_01_01_01u64;
    let pool = ra.constant_pool.as_mut().expect("constant pool required");
    let sign_ext_addr = pool.get_constant(sign_ext, sign_ext);
    ra.asm
        .pand(data_sign, rxbyak::xmmword_ptr(sign_ext_addr))
        .unwrap();
    ra.asm.por(result, data_sign).unwrap();

    ra.release(data_sign);
    ra.release(zero);
    ra.define_value(inst_ref, result);
}
pub fn emit_vector_arithmetic_shift_right16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_vector_op_imm(ra, inst_ref, inst, rxbyak::CodeAssembler::psraw_imm);
}
pub fn emit_vector_arithmetic_shift_right32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_vector_op_imm(ra, inst_ref, inst, rxbyak::CodeAssembler::psrad_imm);
}
pub fn emit_vector_arithmetic_shift_right64(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_asr64 as usize);
}

// ---------------------------------------------------------------------------
// VectorLogicalVShift — variable shift per element, fallback
// ---------------------------------------------------------------------------

macro_rules! define_logical_vshift {
    ($name:ident, $ty:ty, $count:expr) => {
        extern "C" fn $name(result: *mut [u8; 16], a: *const [u8; 16], b: *const [u8; 16]) {
            unsafe {
                let va: [$ty; $count] = std::mem::transmute(*a);
                let vb: [i8; 16] = std::mem::transmute(*b);
                let mut out = [0 as $ty; $count];
                let elem_bits = (std::mem::size_of::<$ty>() * 8) as i8;
                for i in 0..$count {
                    let shift = vb[i * std::mem::size_of::<$ty>()];
                    if shift >= elem_bits || shift <= -elem_bits {
                        out[i] = 0;
                    } else if shift >= 0 {
                        out[i] = va[i] << (shift as u32);
                    } else {
                        out[i] = va[i] >> ((-shift) as u32);
                    }
                }
                *result = std::mem::transmute(out);
            }
        }
    };
}

define_logical_vshift!(fallback_lvshift8, u8, 16);
define_logical_vshift!(fallback_lvshift16, u16, 8);
define_logical_vshift!(fallback_lvshift32, u32, 4);
define_logical_vshift!(fallback_lvshift64, u64, 2);

pub fn emit_vector_logical_vshift8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_lvshift8 as usize);
}
pub fn emit_vector_logical_vshift16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_lvshift16 as usize);
}
// LogicalVShift32: AVX2 vpsllvd/vpsrlvd with sign-based split, fallback without AVX2
pub fn emit_vector_logical_vshift32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    #[cfg(target_arch = "x86_64")]
    if std::is_x86_feature_detected!("avx2") {
        let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
        let a = ra.use_xmm(&mut args[0]);
        let shift = ra.use_xmm(&mut args[1]);
        let result = ra.scratch_xmm();
        let neg_shift = ra.scratch_xmm();
        let left = ra.scratch_xmm();
        let right = ra.scratch_xmm();
        let zero = ra.scratch_xmm();
        ra.asm.xorps(zero, zero).unwrap();
        // neg_shift = -shift (negate each dword: 0 - shift)
        ra.asm.movaps(neg_shift, zero).unwrap();
        ra.asm.psubd(neg_shift, shift).unwrap();
        // left = vpsllvd(a, shift) — left shift by positive amounts
        ra.asm.vpsllvd(left, a, shift).unwrap();
        // right = vpsrlvd(a, neg_shift) — right shift by negated amounts
        ra.asm.vpsrlvd(right, a, neg_shift).unwrap();
        // Select: where shift >= 0, use left; else use right
        // mask = pcmpgtd(shift, -1) → all 1s where shift >= 0
        // Actually: pcmpgtd(zero, shift) → 1s where shift < 0
        let mask = ra.scratch_xmm();
        ra.asm.movaps(mask, zero).unwrap();
        ra.asm.pcmpgtd(mask, shift).unwrap(); // mask = 0xFFFFFFFF where shift < 0
                                              // result = (left & ~mask) | (right & mask) = blendvps
        ra.asm.movaps(result, left).unwrap();
        ra.asm.movaps(rxbyak::XMM0, mask).unwrap();
        ra.asm.blendvps(result, right).unwrap();
        ra.release(neg_shift);
        ra.release(left);
        ra.release(right);
        ra.release(zero);
        ra.release(mask);
        ra.define_value(inst_ref, result);
        return;
    }
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_lvshift32 as usize);
}
// LogicalVShift64: AVX2 vpsllvq/vpsrlvq
pub fn emit_vector_logical_vshift64(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    #[cfg(target_arch = "x86_64")]
    if std::is_x86_feature_detected!("avx2") {
        let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
        let a = ra.use_xmm(&mut args[0]);
        let shift = ra.use_xmm(&mut args[1]);
        let result = ra.scratch_xmm();
        let neg_shift = ra.scratch_xmm();
        let left = ra.scratch_xmm();
        let right = ra.scratch_xmm();
        let zero = ra.scratch_xmm();
        ra.asm.xorps(zero, zero).unwrap();
        ra.asm.movaps(neg_shift, zero).unwrap();
        ra.asm.psubq(neg_shift, shift).unwrap();
        ra.asm.vpsllvq(left, a, shift).unwrap();
        ra.asm.vpsrlvq(right, a, neg_shift).unwrap();
        // mask where shift < 0: use pcmpgtq(zero, shift)
        let mask = ra.scratch_xmm();
        ra.asm.movaps(mask, zero).unwrap();
        ra.asm.pcmpgtq(mask, shift).unwrap();
        ra.asm.movaps(result, left).unwrap();
        ra.asm.movaps(rxbyak::XMM0, mask).unwrap();
        ra.asm.blendvpd(result, right).unwrap();
        ra.release(neg_shift);
        ra.release(left);
        ra.release(right);
        ra.release(zero);
        ra.release(mask);
        ra.define_value(inst_ref, result);
        return;
    }
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_lvshift64 as usize);
}

// ---------------------------------------------------------------------------
// VectorArithmeticVShift — variable arithmetic shift per element, fallback
// ---------------------------------------------------------------------------

macro_rules! define_arith_vshift {
    ($name:ident, $sty:ty, $uty:ty, $count:expr) => {
        extern "C" fn $name(result: *mut [u8; 16], a: *const [u8; 16], b: *const [u8; 16]) {
            unsafe {
                let va: [$sty; $count] = std::mem::transmute(*a);
                let vb: [i8; 16] = std::mem::transmute(*b);
                let mut out = [0 as $sty; $count];
                let elem_bits = (std::mem::size_of::<$sty>() * 8) as i8;
                for i in 0..$count {
                    let shift = vb[i * std::mem::size_of::<$sty>()];
                    if shift >= elem_bits {
                        out[i] = 0;
                    } else if shift >= 0 {
                        out[i] = ((va[i] as $uty) << (shift as u32)) as $sty;
                    } else if shift <= -elem_bits {
                        out[i] = va[i] >> (elem_bits as u32 - 1);
                    } else {
                        out[i] = va[i] >> ((-shift) as u32);
                    }
                }
                *result = std::mem::transmute(out);
            }
        }
    };
}

define_arith_vshift!(fallback_avshift8, i8, u8, 16);
define_arith_vshift!(fallback_avshift16, i16, u16, 8);
define_arith_vshift!(fallback_avshift32, i32, u32, 4);
define_arith_vshift!(fallback_avshift64, i64, u64, 2);

pub fn emit_vector_arithmetic_vshift8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_avshift8 as usize);
}
pub fn emit_vector_arithmetic_vshift16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_avshift16 as usize);
}
// ArithmeticVShift32: AVX2 vpsllvd/vpsravd with sign split
// Positive shift = left (logical), negative = right (arithmetic)
pub fn emit_vector_arithmetic_vshift32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    #[cfg(target_arch = "x86_64")]
    if std::is_x86_feature_detected!("avx2") {
        let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
        let a = ra.use_xmm(&mut args[0]);
        let shift = ra.use_xmm(&mut args[1]);
        let result = ra.scratch_xmm();
        let neg_shift = ra.scratch_xmm();
        let left = ra.scratch_xmm();
        let right = ra.scratch_xmm();
        let zero = ra.scratch_xmm();
        ra.asm.xorps(zero, zero).unwrap();
        ra.asm.movaps(neg_shift, zero).unwrap();
        ra.asm.psubd(neg_shift, shift).unwrap();
        ra.asm.vpsllvd(left, a, shift).unwrap();
        ra.asm.vpsravd(right, a, neg_shift).unwrap(); // arithmetic right shift
        let mask = ra.scratch_xmm();
        ra.asm.movaps(mask, zero).unwrap();
        ra.asm.pcmpgtd(mask, shift).unwrap();
        ra.asm.movaps(result, left).unwrap();
        ra.asm.movaps(rxbyak::XMM0, mask).unwrap();
        ra.asm.blendvps(result, right).unwrap();
        ra.release(neg_shift);
        ra.release(left);
        ra.release(right);
        ra.release(zero);
        ra.release(mask);
        ra.define_value(inst_ref, result);
        return;
    }
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_avshift32 as usize);
}
// ArithmeticVShift64: AVX512 vpsravq or fallback
pub fn emit_vector_arithmetic_vshift64(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    // vpsravq requires AVX512VL — fall back for now
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_avshift64 as usize);
}

// ---------------------------------------------------------------------------
// VectorRoundingShiftLeft — fallback
// ---------------------------------------------------------------------------

macro_rules! define_rounding_shift_signed {
    ($name:ident, $sty:ty, $uty:ty, $count:expr) => {
        extern "C" fn $name(result: *mut [u8; 16], a: *const [u8; 16], b: *const [u8; 16]) {
            unsafe {
                let va: [$sty; $count] = std::mem::transmute(*a);
                let vb: [i8; 16] = std::mem::transmute(*b);
                let mut out = [0 as $sty; $count];
                let elem_bits = std::mem::size_of::<$sty>() as i32 * 8;
                for i in 0..$count {
                    let shift = vb[i * std::mem::size_of::<$sty>()] as i32;
                    if shift >= elem_bits {
                        out[i] = 0;
                    } else if shift > 0 {
                        out[i] = ((va[i] as $uty) << shift as u32) as $sty;
                    } else if shift <= -elem_bits {
                        out[i] = va[i] >> (elem_bits as u32 - 1);
                    } else {
                        let neg = (-shift) as u32;
                        let round_bit = if neg > 0 { (va[i] >> (neg - 1)) & 1 } else { 0 };
                        out[i] = (va[i] >> neg) + round_bit;
                    }
                }
                *result = std::mem::transmute(out);
            }
        }
    };
}

macro_rules! define_rounding_shift_unsigned {
    ($name:ident, $ty:ty, $count:expr) => {
        extern "C" fn $name(result: *mut [u8; 16], a: *const [u8; 16], b: *const [u8; 16]) {
            unsafe {
                let va: [$ty; $count] = std::mem::transmute(*a);
                let vb: [i8; 16] = std::mem::transmute(*b);
                let mut out = [0 as $ty; $count];
                let elem_bits = std::mem::size_of::<$ty>() as i32 * 8;
                for i in 0..$count {
                    let shift = vb[i * std::mem::size_of::<$ty>()] as i32;
                    if shift >= elem_bits || shift <= -elem_bits {
                        out[i] = 0;
                    } else if shift >= 0 {
                        out[i] = va[i] << shift as u32;
                    } else {
                        let neg = (-shift) as u32;
                        let round_bit = if neg > 0 { (va[i] >> (neg - 1)) & 1 } else { 0 };
                        out[i] = (va[i] >> neg) + round_bit;
                    }
                }
                *result = std::mem::transmute(out);
            }
        }
    };
}

define_rounding_shift_signed!(fallback_rsl_s8, i8, u8, 16);
define_rounding_shift_signed!(fallback_rsl_s16, i16, u16, 8);
define_rounding_shift_signed!(fallback_rsl_s32, i32, u32, 4);
define_rounding_shift_signed!(fallback_rsl_s64, i64, u64, 2);
define_rounding_shift_unsigned!(fallback_rsl_u8, u8, 16);
define_rounding_shift_unsigned!(fallback_rsl_u16, u16, 8);
define_rounding_shift_unsigned!(fallback_rsl_u32, u32, 4);
define_rounding_shift_unsigned!(fallback_rsl_u64, u64, 2);

pub fn emit_vector_rounding_shift_left_signed8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_rsl_s8 as usize);
}
pub fn emit_vector_rounding_shift_left_signed16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_rsl_s16 as usize);
}
pub fn emit_vector_rounding_shift_left_signed32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_rsl_s32 as usize);
}
pub fn emit_vector_rounding_shift_left_signed64(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_rsl_s64 as usize);
}
pub fn emit_vector_rounding_shift_left_unsigned8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_rsl_u8 as usize);
}
pub fn emit_vector_rounding_shift_left_unsigned16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_rsl_u16 as usize);
}
pub fn emit_vector_rounding_shift_left_unsigned32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_rsl_u32 as usize);
}
pub fn emit_vector_rounding_shift_left_unsigned64(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    emit_two_arg_fallback(ra, inst_ref, inst, fallback_rsl_u64 as usize);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fn_signatures() {
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_logical_shift_left8;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_logical_shift_left16;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_logical_shift_right32;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) =
            emit_vector_arithmetic_shift_right64;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_logical_vshift8;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_arithmetic_vshift64;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) =
            emit_vector_rounding_shift_left_signed8;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) =
            emit_vector_rounding_shift_left_unsigned64;
    }
}
