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
// VectorAdd — native SSE: paddb/paddw/paddd/paddq
// ---------------------------------------------------------------------------

pub fn emit_vector_add8(_ctx: &EmitContext, ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst) {
    emit_vector_op(ra, inst_ref, inst, rxbyak::CodeAssembler::paddb);
}
pub fn emit_vector_add16(_ctx: &EmitContext, ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst) {
    emit_vector_op(ra, inst_ref, inst, rxbyak::CodeAssembler::paddw);
}
pub fn emit_vector_add32(_ctx: &EmitContext, ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst) {
    emit_vector_op(ra, inst_ref, inst, rxbyak::CodeAssembler::paddd);
}
pub fn emit_vector_add64(_ctx: &EmitContext, ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst) {
    emit_vector_op(ra, inst_ref, inst, rxbyak::CodeAssembler::paddq);
}

// ---------------------------------------------------------------------------
// VectorSub — native SSE: psubb/psubw/psubd/psubq
// ---------------------------------------------------------------------------

pub fn emit_vector_sub8(_ctx: &EmitContext, ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst) {
    emit_vector_op(ra, inst_ref, inst, rxbyak::CodeAssembler::psubb);
}
pub fn emit_vector_sub16(_ctx: &EmitContext, ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst) {
    emit_vector_op(ra, inst_ref, inst, rxbyak::CodeAssembler::psubw);
}
pub fn emit_vector_sub32(_ctx: &EmitContext, ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst) {
    emit_vector_op(ra, inst_ref, inst, rxbyak::CodeAssembler::psubd);
}
pub fn emit_vector_sub64(_ctx: &EmitContext, ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst) {
    emit_vector_op(ra, inst_ref, inst, rxbyak::CodeAssembler::psubq);
}

// ---------------------------------------------------------------------------
// Logical — native SSE: pand/pandn/por/pxor
// ---------------------------------------------------------------------------

pub fn emit_vector_and(_ctx: &EmitContext, ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst) {
    emit_vector_op(ra, inst_ref, inst, rxbyak::CodeAssembler::pand);
}
pub fn emit_vector_and_not(_ctx: &EmitContext, ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let a = ra.use_xmm(&mut args[0]);
    let result = ra.use_scratch_xmm(&mut args[1]);
    rxbyak::CodeAssembler::pandn(&mut *ra.asm, result, a).unwrap();
    ra.define_value(inst_ref, result);
}
pub fn emit_vector_or(_ctx: &EmitContext, ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst) {
    emit_vector_op(ra, inst_ref, inst, rxbyak::CodeAssembler::por);
}
pub fn emit_vector_eor(_ctx: &EmitContext, ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst) {
    emit_vector_op(ra, inst_ref, inst, rxbyak::CodeAssembler::pxor);
}

// ---------------------------------------------------------------------------
// VectorNot — pcmpeqd(tmp,tmp) to get all-ones, then pxor
// ---------------------------------------------------------------------------

pub fn emit_vector_not(_ctx: &EmitContext, ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let result = ra.use_scratch_xmm(&mut args[0]);
    let ones = ra.scratch_xmm();
    ra.asm.pcmpeqd(ones, ones).unwrap();
    ra.asm.pxor(result, ones).unwrap();
    ra.release(ones);
    ra.define_value(inst_ref, result);
}

// ---------------------------------------------------------------------------
// VectorAbs — native SSSE3: pabsb/pabsw/pabsd
// VectorAbs64 — fallback (no pabsq in SSE)
// ---------------------------------------------------------------------------

pub fn emit_vector_abs8(_ctx: &EmitContext, ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst) {
    emit_vector_unary_op(ra, inst_ref, inst, rxbyak::CodeAssembler::pabsb);
}
pub fn emit_vector_abs16(_ctx: &EmitContext, ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst) {
    emit_vector_unary_op(ra, inst_ref, inst, rxbyak::CodeAssembler::pabsw);
}
pub fn emit_vector_abs32(_ctx: &EmitContext, ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst) {
    emit_vector_unary_op(ra, inst_ref, inst, rxbyak::CodeAssembler::pabsd);
}

// VectorAbs64: no pabsq in SSE; use pxor+psubq+blendvpd
// Upstream: zero = pxor; neg = psubq(zero, data); mask = pcmpgtq(data, zero); blendvpd(neg, data, mask)
// Wait — upstream SSE4.1: pxor(zero); psubq(tmp=zero, data); pcmpgtq(zero2, data) for negative detection
// Simpler approach: sign = psrad(data, 31); pshufd with sign in 31 → mask; pxor + psubq
pub fn emit_vector_abs64(_ctx: &EmitContext, ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let data = ra.use_xmm(&mut args[0]);
    let result = ra.scratch_xmm();
    let zero = ra.scratch_xmm();
    let neg = ra.scratch_xmm();
    // zero = 0
    ra.asm.xorps(zero, zero).unwrap();
    // neg = 0 - data = two's complement negation
    ra.asm.movaps(neg, zero).unwrap();
    ra.asm.psubq(neg, data).unwrap();
    // mask where data >= 0: pcmpgtq(zero, data) gives 1s where 0 > data (i.e., data negative)
    // We want: where data >= 0 pick data, else pick neg
    // XMM0 = mask of negative elements (where zero > data)
    ra.asm.movaps(rxbyak::XMM0, zero).unwrap();
    ra.asm.pcmpgtq(rxbyak::XMM0, data).unwrap();
    // result = data; blendvpd(data, neg, XMM0): where negative, pick neg
    ra.asm.movaps(result, data).unwrap();
    ra.asm.blendvpd(result, neg).unwrap();
    ra.release(zero);
    ra.release(neg);
    ra.define_value(inst_ref, result);
}

// ---------------------------------------------------------------------------
// ZeroVector — xorps
// ---------------------------------------------------------------------------

pub fn emit_zero_vector(_ctx: &EmitContext, ra: &mut RegAlloc, inst_ref: InstRef, _inst: &Inst) {
    let result = ra.scratch_xmm();
    ra.asm.xorps(result, result).unwrap();
    ra.define_value(inst_ref, result);
}

// ---------------------------------------------------------------------------
// VectorZeroUpper — zero upper 64 bits, keep lower 64
// movq dst, src (SSE2 form: loads low 64, zeros high)
// ---------------------------------------------------------------------------

pub fn emit_vector_zero_upper(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let src = ra.use_xmm(&mut args[0]);
    let result = ra.scratch_xmm();
    ra.asm.movq(result, src).unwrap();
    ra.release(src);
    ra.define_value(inst_ref, result);
}

// ---------------------------------------------------------------------------
// VectorCountLeadingZeros8/16/32 — fallback
// ---------------------------------------------------------------------------

extern "C" fn fallback_vector_clz8(result: *mut [u8; 16], a: *const [u8; 16]) {
    unsafe {
        let src = &*a;
        let dst = &mut *result;
        for i in 0..16 {
            dst[i] = src[i].leading_zeros() as u8;
        }
    }
}

extern "C" fn fallback_vector_clz16(result: *mut [u8; 16], a: *const [u8; 16]) {
    unsafe {
        let src: [u16; 8] = std::mem::transmute(*a);
        let mut out = [0u16; 8];
        for i in 0..8 {
            out[i] = src[i].leading_zeros() as u16;
        }
        *result = std::mem::transmute(out);
    }
}

extern "C" fn fallback_vector_clz32(result: *mut [u8; 16], a: *const [u8; 16]) {
    unsafe {
        let src: [u32; 4] = std::mem::transmute(*a);
        let mut out = [0u32; 4];
        for i in 0..4 {
            out[i] = src[i].leading_zeros();
        }
        *result = std::mem::transmute(out);
    }
}

// VectorCountLeadingZeros8: pshufb nibble lookup
// clz8(x) = if hi_nibble != 0 { clz4(hi_nibble) } else { 4 + clz4(lo_nibble) }
// clz4 table: [4,3,2,2,1,1,1,1,0,0,0,0,0,0,0,0]
pub fn emit_vector_clz8(_ctx: &EmitContext, ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let data = ra.use_xmm(&mut args[0]);
    let result = ra.scratch_xmm();
    let lo_nibble = ra.scratch_xmm();
    let hi_nibble = ra.scratch_xmm();
    let clz4_table = ra.scratch_xmm();
    let clz4_hi = ra.scratch_xmm();

    let pool = ra.constant_pool.as_mut().expect("constant pool required");
    // clz4 = {4,3,2,2,1,1,1,1,0,0,0,0,0,0,0,0} as bytes
    let clz4_addr = pool.get_constant(
        0x01_01_01_01_02_02_03_04u64, // byte[0]=4, [1]=3, [2]=2, [3]=2, [4-7]=1
        0x00_00_00_00_00_00_00_00u64, // byte[8-15]=0
    );
    let nibble_mask_addr =
        pool.get_constant(0x0F_0F_0F_0F_0F_0F_0F_0Fu64, 0x0F_0F_0F_0F_0F_0F_0F_0Fu64);

    // lo_nibble = data & 0x0F
    ra.asm.movaps(lo_nibble, data).unwrap();
    ra.asm
        .pand(lo_nibble, rxbyak::xmmword_ptr(nibble_mask_addr))
        .unwrap();

    // hi_nibble = (data >> 4) & 0x0F
    ra.asm.movaps(hi_nibble, data).unwrap();
    ra.asm.psrlw_imm(hi_nibble, 4).unwrap();
    ra.asm
        .pand(hi_nibble, rxbyak::xmmword_ptr(nibble_mask_addr))
        .unwrap();

    // clz4_lo = clz4[lo_nibble]: load table, shuffle by indices
    ra.asm
        .movaps(result, rxbyak::xmmword_ptr(clz4_addr))
        .unwrap();
    ra.asm.pshufb(result, lo_nibble).unwrap();
    // result now has clz4(lo_nibble) per byte

    // clz4_hi = clz4[hi_nibble]
    ra.asm
        .movaps(clz4_table, rxbyak::xmmword_ptr(clz4_addr))
        .unwrap();
    ra.asm.pshufb(clz4_table, hi_nibble).unwrap();
    ra.asm.movaps(clz4_hi, clz4_table).unwrap();
    // clz4_hi now has clz4(hi_nibble) per byte

    // If hi_nibble != 0: clz = clz4_hi; else: clz = 4 + clz4_lo = result + 4
    // Method: result += 4 (add 4 to lo-nibble CLZ)
    // Then: if hi_nibble != 0, replace with clz4_hi
    // hi_is_zero = pcmpeqb(hi_nibble, zero)
    let zero = lo_nibble; // reuse
    ra.asm.xorps(zero, zero).unwrap();
    let hi_is_zero = hi_nibble; // reuse (hi_nibble no longer needed after clz4_hi computed)
    ra.asm.pcmpeqb(hi_is_zero, zero).unwrap();
    // hi_is_zero = 0xFF where hi_nibble was 0, 0x00 otherwise

    // result = result + 4 (for the lo-nibble case)
    let pool = ra.constant_pool.as_mut().expect("constant pool required");
    let four_addr = pool.get_constant(0x04_04_04_04_04_04_04_04u64, 0x04_04_04_04_04_04_04_04u64);
    ra.asm
        .paddb(result, rxbyak::xmmword_ptr(four_addr))
        .unwrap();

    // result = (hi_is_zero & result) | (~hi_is_zero & clz4_hi)
    // = blend: where hi_is_zero, keep result(=4+clz4_lo); else use clz4_hi
    // pand(result, hi_is_zero): keep result where hi was zero
    ra.asm.pand(result, hi_is_zero).unwrap();
    // pandn(hi_is_zero, clz4_hi): keep clz4_hi where hi was nonzero
    ra.asm.pandn(hi_is_zero, clz4_hi).unwrap();
    // por: merge
    ra.asm.por(result, hi_is_zero).unwrap();

    ra.release(data);
    ra.release(clz4_hi);
    ra.define_value(inst_ref, result);
}
pub fn emit_vector_clz16(_ctx: &EmitContext, ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst) {
    emit_one_arg_fallback(ra, inst_ref, inst, fallback_vector_clz16 as usize);
}
pub fn emit_vector_clz32(_ctx: &EmitContext, ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst) {
    emit_one_arg_fallback(ra, inst_ref, inst, fallback_vector_clz32 as usize);
}

// ---------------------------------------------------------------------------
// VectorPopulationCount — fallback
// ---------------------------------------------------------------------------

extern "C" fn fallback_vector_popcount(result: *mut [u8; 16], a: *const [u8; 16]) {
    unsafe {
        let src = &*a;
        let dst = &mut *result;
        for i in 0..16 {
            dst[i] = src[i].count_ones() as u8;
        }
    }
}

// VectorPopulationCount: pshufb nibble lookup, add halves
// popcount4 = {0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4}
pub fn emit_vector_popcount(_ctx: &EmitContext, ra: &mut RegAlloc, inst_ref: InstRef, inst: &Inst) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let data = ra.use_xmm(&mut args[0]);
    let lo_nibble = ra.scratch_xmm();
    let hi_nibble = ra.scratch_xmm();
    let result = ra.scratch_xmm();
    let table_copy = ra.scratch_xmm();

    let pool = ra.constant_pool.as_mut().expect("constant pool required");
    // popcount4 = {0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4}
    let pop4_addr = pool.get_constant(
        0x03_02_02_01_02_01_01_00u64, // byte[0]=0,[1]=1,[2]=1,[3]=2,[4]=1,[5]=2,[6]=2,[7]=3
        0x04_03_03_02_03_02_02_01u64, // byte[8]=1,[9]=2,[10]=2,[11]=3,[12]=2,[13]=3,[14]=3,[15]=4
    );
    let nibble_mask_addr =
        pool.get_constant(0x0F_0F_0F_0F_0F_0F_0F_0Fu64, 0x0F_0F_0F_0F_0F_0F_0F_0Fu64);

    // lo_nibble = data & 0x0F
    ra.asm.movaps(lo_nibble, data).unwrap();
    ra.asm
        .pand(lo_nibble, rxbyak::xmmword_ptr(nibble_mask_addr))
        .unwrap();

    // hi_nibble = (data >> 4) & 0x0F
    ra.asm.movaps(hi_nibble, data).unwrap();
    ra.asm.psrlw_imm(hi_nibble, 4).unwrap();
    ra.asm
        .pand(hi_nibble, rxbyak::xmmword_ptr(nibble_mask_addr))
        .unwrap();

    // pop_lo = pop4[lo_nibble]
    ra.asm
        .movaps(result, rxbyak::xmmword_ptr(pop4_addr))
        .unwrap();
    ra.asm.pshufb(result, lo_nibble).unwrap();

    // pop_hi = pop4[hi_nibble]
    ra.asm
        .movaps(table_copy, rxbyak::xmmword_ptr(pop4_addr))
        .unwrap();
    ra.asm.pshufb(table_copy, hi_nibble).unwrap();

    // result = pop_lo + pop_hi
    ra.asm.paddb(result, table_copy).unwrap();

    ra.release(data);
    ra.release(lo_nibble);
    ra.release(hi_nibble);
    ra.release(table_copy);
    ra.define_value(inst_ref, result);
}

// ---------------------------------------------------------------------------
// VectorReverseBits — fallback
// ---------------------------------------------------------------------------

extern "C" fn fallback_vector_reverse_bits(result: *mut [u8; 16], a: *const [u8; 16]) {
    unsafe {
        let src = &*a;
        let dst = &mut *result;
        for i in 0..16 {
            dst[i] = src[i].reverse_bits();
        }
    }
}

// VectorReverseBits: pshufb nibble bit-reverse lookup, swap nibbles
// reverse_bits(byte) = (reverse4(hi_nib) << 0) | (reverse4(lo_nib) << 4)
// reverse4 = {0,8,4,C,2,A,6,E,1,9,5,D,3,B,7,F}
pub fn emit_vector_reverse_bits(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let data = ra.use_xmm(&mut args[0]);
    let lo_nibble = ra.scratch_xmm();
    let hi_nibble = ra.scratch_xmm();
    let result = ra.scratch_xmm();
    let table_copy = ra.scratch_xmm();

    let pool = ra.constant_pool.as_mut().expect("constant pool required");
    // reverse4 = {0x0,0x8,0x4,0xC,0x2,0xA,0x6,0xE,0x1,0x9,0x5,0xD,0x3,0xB,0x7,0xF}
    let rev4_addr = pool.get_constant(0x0E_06_0A_02_0C_04_08_00u64, 0x0F_07_0B_03_0D_05_09_01u64);
    let nibble_mask_addr =
        pool.get_constant(0x0F_0F_0F_0F_0F_0F_0F_0Fu64, 0x0F_0F_0F_0F_0F_0F_0F_0Fu64);

    // lo_nibble = data & 0x0F
    ra.asm.movaps(lo_nibble, data).unwrap();
    ra.asm
        .pand(lo_nibble, rxbyak::xmmword_ptr(nibble_mask_addr))
        .unwrap();

    // hi_nibble = (data >> 4) & 0x0F
    ra.asm.movaps(hi_nibble, data).unwrap();
    ra.asm.psrlw_imm(hi_nibble, 4).unwrap();
    ra.asm
        .pand(hi_nibble, rxbyak::xmmword_ptr(nibble_mask_addr))
        .unwrap();

    // rev_lo = reverse4[lo_nibble] — goes to HIGH nibble of result
    ra.asm
        .movaps(result, rxbyak::xmmword_ptr(rev4_addr))
        .unwrap();
    ra.asm.pshufb(result, lo_nibble).unwrap();
    ra.asm.psllw_imm(result, 4).unwrap();
    // Mask to keep only high nibble per byte
    let pool = ra.constant_pool.as_mut().expect("constant pool required");
    let hi_mask_addr =
        pool.get_constant(0xF0_F0_F0_F0_F0_F0_F0_F0u64, 0xF0_F0_F0_F0_F0_F0_F0_F0u64);
    ra.asm
        .pand(result, rxbyak::xmmword_ptr(hi_mask_addr))
        .unwrap();

    // rev_hi = reverse4[hi_nibble] — goes to LOW nibble of result
    ra.asm
        .movaps(table_copy, rxbyak::xmmword_ptr(rev4_addr))
        .unwrap();
    ra.asm.pshufb(table_copy, hi_nibble).unwrap();
    // table_copy already has values in low nibble

    // result = rev_lo_shifted | rev_hi
    ra.asm.por(result, table_copy).unwrap();

    ra.release(data);
    ra.release(lo_nibble);
    ra.release(hi_nibble);
    ra.release(table_copy);
    ra.define_value(inst_ref, result);
}

// ---------------------------------------------------------------------------
// VectorReverseElementsIn*Groups* — native SSE, matching emit_x64_vector.cpp
// ---------------------------------------------------------------------------

pub fn emit_vector_reverse_half_groups_8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let data = ra.use_scratch_xmm(&mut args[0]);
    let tmp = ra.scratch_xmm();
    ra.asm.movdqa(tmp, data).unwrap();
    ra.asm.psllw_imm(tmp, 8).unwrap();
    ra.asm.psrlw_imm(data, 8).unwrap();
    ra.asm.por(data, tmp).unwrap();
    ra.release(tmp);
    ra.define_value(inst_ref, data);
}

pub fn emit_vector_reverse_word_groups_8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let data = ra.use_scratch_xmm(&mut args[0]);
    let tmp = ra.scratch_xmm();
    ra.asm.movdqa(tmp, data).unwrap();
    ra.asm.psllw_imm(tmp, 8).unwrap();
    ra.asm.psrlw_imm(data, 8).unwrap();
    ra.asm.por(data, tmp).unwrap();
    ra.asm.pshuflw(data, data, 0b1011_0001).unwrap();
    ra.asm.pshufhw(data, data, 0b1011_0001).unwrap();
    ra.release(tmp);
    ra.define_value(inst_ref, data);
}

pub fn emit_vector_reverse_word_groups_16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let data = ra.use_scratch_xmm(&mut args[0]);
    ra.asm.pshuflw(data, data, 0b1011_0001).unwrap();
    ra.asm.pshufhw(data, data, 0b1011_0001).unwrap();
    ra.define_value(inst_ref, data);
}

pub fn emit_vector_reverse_long_groups_8(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let data = ra.use_scratch_xmm(&mut args[0]);
    let tmp = ra.scratch_xmm();
    ra.asm.movdqa(tmp, data).unwrap();
    ra.asm.psllw_imm(tmp, 8).unwrap();
    ra.asm.psrlw_imm(data, 8).unwrap();
    ra.asm.por(data, tmp).unwrap();
    ra.asm.pshuflw(data, data, 0b0001_1011).unwrap();
    ra.asm.pshufhw(data, data, 0b0001_1011).unwrap();
    ra.release(tmp);
    ra.define_value(inst_ref, data);
}

pub fn emit_vector_reverse_long_groups_16(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let data = ra.use_scratch_xmm(&mut args[0]);
    ra.asm.pshuflw(data, data, 0b0001_1011).unwrap();
    ra.asm.pshufhw(data, data, 0b0001_1011).unwrap();
    ra.define_value(inst_ref, data);
}

pub fn emit_vector_reverse_long_groups_32(
    _ctx: &EmitContext,
    ra: &mut RegAlloc,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let mut args = ra.get_argument_info(inst_ref, &inst.args, inst.num_args());
    let data = ra.use_scratch_xmm(&mut args[0]);
    ra.asm.pshuflw(data, data, 0b0100_1110).unwrap();
    ra.asm.pshufhw(data, data, 0b0100_1110).unwrap();
    ra.define_value(inst_ref, data);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fn_signatures() {
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_add8;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_sub64;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_and;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_not;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_abs8;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_abs64;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_zero_vector;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_zero_upper;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_clz8;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_popcount;
        let _: fn(&EmitContext, &mut RegAlloc, InstRef, &Inst) = emit_vector_reverse_bits;
    }

    #[test]
    fn test_fallback_vector_clz8() {
        let input: [u8; 16] = [0, 1, 2, 4, 8, 16, 32, 64, 128, 255, 3, 7, 15, 31, 63, 127];
        let mut output = [0u8; 16];
        fallback_vector_clz8(&mut output, &input);
        assert_eq!(output[0], 8); // clz(0) = 8
        assert_eq!(output[1], 7); // clz(1) = 7
        assert_eq!(output[8], 0); // clz(128) = 0
        assert_eq!(output[9], 0); // clz(255) = 0
    }

    #[test]
    fn test_fallback_vector_popcount() {
        let input: [u8; 16] = [
            0, 1, 3, 7, 15, 31, 63, 127, 255, 0x80, 0xAA, 0x55, 0xFF, 0, 0, 0,
        ];
        let mut output = [0u8; 16];
        fallback_vector_popcount(&mut output, &input);
        assert_eq!(output[0], 0);
        assert_eq!(output[1], 1);
        assert_eq!(output[2], 2);
        assert_eq!(output[8], 8);
    }
}
