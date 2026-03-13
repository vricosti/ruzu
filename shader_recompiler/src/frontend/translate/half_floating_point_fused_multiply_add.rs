// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/half_floating_point_fused_multiply_add.cpp

use super::{bit, field, TranslatorVisitor};
use super::half_floating_point_helper::{
    extract, half_precision_to_fmz_mode, merge_result, HalfPrecision, Merge, Swizzle,
};
use crate::ir::value::Value;

/// Core HFMA2 implementation (inner `HFMA2` overloads from upstream).
#[allow(clippy::too_many_arguments)]
fn hfma2_inner(
    tv: &mut TranslatorVisitor,
    insn: u64,
    merge: Merge,
    swizzle_a: Swizzle,
    neg_b: bool,
    neg_c: bool,
    swizzle_b: Swizzle,
    swizzle_c: Swizzle,
    src_b: Value,
    src_c: Value,
    sat: bool,
    precision: HalfPrecision,
) {
    let dest_reg  = field(insn, 0, 8);
    let src_a_reg = field(insn, 8, 8);
    let src_a_val = tv.x(src_a_reg);

    let (mut lhs_a, mut rhs_a) = extract(tv, src_a_val, swizzle_a);
    let (mut lhs_b, mut rhs_b) = extract(tv, src_b, swizzle_b);
    let (mut lhs_c, mut rhs_c) = extract(tv, src_c, swizzle_c);

    let a_is_f32 = swizzle_a == Swizzle::F32;
    let b_is_f32 = swizzle_b == Swizzle::F32;
    let c_is_f32 = swizzle_c == Swizzle::F32;
    let promotion = (a_is_f32 != b_is_f32) || (a_is_f32 != c_is_f32);
    if promotion {
        if !a_is_f32 {
            lhs_a = tv.ir.convert_f32_from_f16(lhs_a);
            rhs_a = tv.ir.convert_f32_from_f16(rhs_a);
        }
        if !b_is_f32 {
            lhs_b = tv.ir.convert_f32_from_f16(lhs_b);
            rhs_b = tv.ir.convert_f32_from_f16(rhs_b);
        }
        if !c_is_f32 {
            lhs_c = tv.ir.convert_f32_from_f16(lhs_c);
            rhs_c = tv.ir.convert_f32_from_f16(rhs_c);
        }
    }

    let use_f32 = a_is_f32 || promotion;

    // Apply neg modifiers to B and C operands.
    if use_f32 {
        lhs_b = tv.ir.fp_abs_neg_32(lhs_b, false, neg_b);
        rhs_b = tv.ir.fp_abs_neg_32(rhs_b, false, neg_b);
        lhs_c = tv.ir.fp_abs_neg_32(lhs_c, false, neg_c);
        rhs_c = tv.ir.fp_abs_neg_32(rhs_c, false, neg_c);
    } else {
        lhs_b = tv.ir.fp_abs_neg_16(lhs_b, false, neg_b);
        rhs_b = tv.ir.fp_abs_neg_16(rhs_b, false, neg_b);
        lhs_c = tv.ir.fp_abs_neg_16(lhs_c, false, neg_c);
        rhs_c = tv.ir.fp_abs_neg_16(rhs_c, false, neg_c);
    }

    let _ = half_precision_to_fmz_mode(precision);
    let (mut lhs, mut rhs) = if use_f32 {
        (tv.ir.fp_fma_32(lhs_a, lhs_b, lhs_c), tv.ir.fp_fma_32(rhs_a, rhs_b, rhs_c))
    } else {
        (tv.ir.fp_fma_16(lhs_a, lhs_b, lhs_c), tv.ir.fp_fma_16(rhs_a, rhs_b, rhs_c))
    };

    // FMZ mode: when A or B is zero, collapse to C (the addend).
    if precision == HalfPrecision::Fmz && !sat {
        if use_f32 {
            let zero = tv.ir.imm_f32(0.0);
            let lhs_zero_a = tv.ir.fp_ord_equal_32(lhs_a, zero);
            let lhs_zero_b = tv.ir.fp_ord_equal_32(lhs_b, zero);
            let lhs_any_zero = tv.ir.logical_or(lhs_zero_a, lhs_zero_b);
            lhs = tv.ir.select_f32(lhs_any_zero, lhs_c, lhs);

            let rhs_zero_a = tv.ir.fp_ord_equal_32(rhs_a, zero);
            let rhs_zero_b = tv.ir.fp_ord_equal_32(rhs_b, zero);
            let rhs_any_zero = tv.ir.logical_or(rhs_zero_a, rhs_zero_b);
            rhs = tv.ir.select_f32(rhs_any_zero, rhs_c, rhs);
        }
    }

    if sat {
        if use_f32 {
            lhs = tv.ir.fp_saturate_32(lhs);
            rhs = tv.ir.fp_saturate_32(rhs);
        } else {
            lhs = tv.ir.fp_saturate_16(lhs);
            rhs = tv.ir.fp_saturate_16(rhs);
        }
    }

    if promotion {
        lhs = tv.ir.convert_f16_from_f32(lhs);
        rhs = tv.ir.convert_f16_from_f32(rhs);
    }

    let result_is_f32 = a_is_f32 && !promotion;
    let result = merge_result(tv, dest_reg, lhs, rhs, merge, result_is_f32);
    tv.set_x(dest_reg, result);
}

/// HFMA2_reg — src_b from register [20:8], src_c from register [39:8].
pub fn hfma2_reg(tv: &mut TranslatorVisitor, insn: u64) {
    let swizzle_b = Swizzle::from_u32(field(insn, 28, 2));
    let sat       = bit(insn, 32);
    let neg_b     = bit(insn, 31);
    let neg_c     = bit(insn, 30);
    let swizzle_c = Swizzle::from_u32(field(insn, 35, 2));
    let precision = HalfPrecision::from_u32(field(insn, 37, 2));
    let swizzle_a = Swizzle::from_u32(field(insn, 47, 2));
    let merge     = Merge::from_u32(field(insn, 49, 2));
    let src_b     = tv.get_reg20(insn);
    let src_c     = tv.get_reg39(insn);
    hfma2_inner(tv, insn, merge, swizzle_a, neg_b, neg_c, swizzle_b, swizzle_c, src_b, src_c, sat, precision);
}

/// HFMA2_rc — src_b from register [39:8], src_c from constant buffer.
pub fn hfma2_rc(tv: &mut TranslatorVisitor, insn: u64) {
    let neg_c     = bit(insn, 51);
    let sat       = bit(insn, 52);
    let swizzle_b = Swizzle::from_u32(field(insn, 53, 2));
    let neg_b     = bit(insn, 56);
    let precision = HalfPrecision::from_u32(field(insn, 57, 2));
    let swizzle_a = Swizzle::from_u32(field(insn, 47, 2));
    let merge     = Merge::from_u32(field(insn, 49, 2));
    let src_b     = tv.get_reg39(insn);
    let src_c     = tv.get_cbuf(insn);
    hfma2_inner(tv, insn, merge, swizzle_a, neg_b, neg_c, swizzle_b, Swizzle::F32, src_b, src_c, sat, precision);
}

/// HFMA2_cr — src_b from constant buffer, src_c from register [39:8].
pub fn hfma2_cr(tv: &mut TranslatorVisitor, insn: u64) {
    let neg_c     = bit(insn, 51);
    let sat       = bit(insn, 52);
    let swizzle_c = Swizzle::from_u32(field(insn, 53, 2));
    let neg_b     = bit(insn, 56);
    let precision = HalfPrecision::from_u32(field(insn, 57, 2));
    let swizzle_a = Swizzle::from_u32(field(insn, 47, 2));
    let merge     = Merge::from_u32(field(insn, 49, 2));
    let src_b     = tv.get_cbuf(insn);
    let src_c     = tv.get_reg39(insn);
    hfma2_inner(tv, insn, merge, swizzle_a, neg_b, neg_c, Swizzle::F32, swizzle_c, src_b, src_c, sat, precision);
}

/// HFMA2_imm — src_b from 16-bit immediate pair, src_c from register [39:8].
pub fn hfma2_imm(tv: &mut TranslatorVisitor, insn: u64) {
    let neg_c     = bit(insn, 51);
    let sat       = bit(insn, 52);
    let swizzle_c = Swizzle::from_u32(field(insn, 53, 2));
    let neg_high  = bit(insn, 56);
    let high      = field(insn, 30, 9);
    let neg_low   = bit(insn, 29);
    let low       = field(insn, 20, 9);
    let precision = HalfPrecision::from_u32(field(insn, 57, 2));
    let imm: u32  = (low << 6)
        | (if neg_low  { 1u32 } else { 0u32 } << 15)
        | (high << 22)
        | (if neg_high { 1u32 } else { 0u32 } << 31);
    let swizzle_a = Swizzle::from_u32(field(insn, 47, 2));
    let merge     = Merge::from_u32(field(insn, 49, 2));
    let src_b     = Value::ImmU32(imm);
    let src_c     = tv.get_reg39(insn);
    hfma2_inner(tv, insn, merge, swizzle_a, false, neg_c, Swizzle::H1H0, swizzle_c, src_b, src_c, sat, precision);
}

/// HFMA2_32I — src_b from 32-bit immediate, src_c from register [0:8].
pub fn hfma2_32i(tv: &mut TranslatorVisitor, insn: u64) {
    let src_c_reg = field(insn, 0, 8);
    let imm: u32  = field(insn, 20, 32);
    let neg_c     = bit(insn, 52);
    let swizzle_a = Swizzle::from_u32(field(insn, 53, 2));
    let precision = HalfPrecision::from_u32(field(insn, 55, 2));
    let src_b     = Value::ImmU32(imm);
    let src_c     = tv.x(src_c_reg);
    hfma2_inner(tv, insn, Merge::H1H0, swizzle_a, false, neg_c, Swizzle::H1H0, Swizzle::H1H0, src_b, src_c, false, precision);
}

/// Public entry-point stub used when a single dispatch opcode covers HFMA2.
pub fn hfma2(tv: &mut TranslatorVisitor, insn: u64) {
    hfma2_reg(tv, insn);
}
