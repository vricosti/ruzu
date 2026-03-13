// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/half_floating_point_add.cpp

use super::{bit, field, TranslatorVisitor};
use super::half_floating_point_helper::{extract, merge_result, Merge, Swizzle};
use crate::ir::value::Value;

/// Core HADD2 implementation (both inner `HADD2` overloads from upstream).
fn hadd2_inner(
    tv: &mut TranslatorVisitor,
    insn: u64,
    merge: Merge,
    ftz: bool,
    sat: bool,
    abs_a: bool,
    neg_a: bool,
    swizzle_a: Swizzle,
    abs_b: bool,
    neg_b: bool,
    swizzle_b: Swizzle,
    src_b: Value,
) {
    let dest_reg  = field(insn, 0, 8);
    let src_a_reg = field(insn, 8, 8);
    let src_a_val = tv.x(src_a_reg);

    let (mut lhs_a, mut rhs_a) = extract(tv, src_a_val, swizzle_a);
    let (mut lhs_b, mut rhs_b) = extract(tv, src_b, swizzle_b);

    // Promotion: if exactly one operand side is F32 (from Swizzle::F32), upgrade all F16 to F32.
    let a_is_f32 = swizzle_a == Swizzle::F32;
    let b_is_f32 = swizzle_b == Swizzle::F32;
    let promotion = a_is_f32 != b_is_f32;
    if promotion {
        if !a_is_f32 {
            lhs_a = tv.ir.convert_f32_from_f16(lhs_a);
            rhs_a = tv.ir.convert_f32_from_f16(rhs_a);
        }
        if !b_is_f32 {
            lhs_b = tv.ir.convert_f32_from_f16(lhs_b);
            rhs_b = tv.ir.convert_f32_from_f16(rhs_b);
        }
    }

    let use_f32 = a_is_f32 || promotion;

    // Apply abs/neg modifiers.
    if use_f32 {
        lhs_a = tv.ir.fp_abs_neg_32(lhs_a, abs_a, neg_a);
        rhs_a = tv.ir.fp_abs_neg_32(rhs_a, abs_a, neg_a);
        lhs_b = tv.ir.fp_abs_neg_32(lhs_b, abs_b, neg_b);
        rhs_b = tv.ir.fp_abs_neg_32(rhs_b, abs_b, neg_b);
    } else {
        lhs_a = tv.ir.fp_abs_neg_16(lhs_a, abs_a, neg_a);
        rhs_a = tv.ir.fp_abs_neg_16(rhs_a, abs_a, neg_a);
        lhs_b = tv.ir.fp_abs_neg_16(lhs_b, abs_b, neg_b);
        rhs_b = tv.ir.fp_abs_neg_16(rhs_b, abs_b, neg_b);
    }

    let _ = ftz; // ftz is encoded in FpControl in upstream; we approximate without it here
    let (mut lhs, mut rhs) = if use_f32 {
        (tv.ir.fp_add_32(lhs_a, lhs_b), tv.ir.fp_add_32(rhs_a, rhs_b))
    } else {
        (tv.ir.fp_add_16(lhs_a, lhs_b), tv.ir.fp_add_16(rhs_a, rhs_b))
    };

    if sat {
        if use_f32 {
            lhs = tv.ir.fp_saturate_32(lhs);
            rhs = tv.ir.fp_saturate_32(rhs);
        } else {
            lhs = tv.ir.fp_saturate_16(lhs);
            rhs = tv.ir.fp_saturate_16(rhs);
        }
    }

    // If promoted, convert back to F16 before merging.
    if promotion {
        lhs = tv.ir.convert_f16_from_f32(lhs);
        rhs = tv.ir.convert_f16_from_f32(rhs);
    }

    // After conversion: values are F32 only if a_is_f32 && !promotion.
    let result_is_f32 = a_is_f32 && !promotion;
    let result = merge_result(tv, dest_reg, lhs, rhs, merge, result_is_f32);
    tv.set_x(dest_reg, result);
}

/// Decode common modifier fields shared by reg/cbuf variants.
fn hadd2_mods(tv: &mut TranslatorVisitor, insn: u64) -> (Merge, bool, bool, bool, bool, Swizzle) {
    let merge     = Merge::from_u32(field(insn, 49, 2));
    let ftz       = bit(insn, 39);
    let neg_a     = bit(insn, 43);
    let abs_a     = bit(insn, 44);
    let sat       = false; // sat decoded per-variant
    let swizzle_a = Swizzle::from_u32(field(insn, 47, 2));
    let _ = sat;
    (merge, ftz, neg_a, abs_a, false, swizzle_a)
}

/// HADD2_reg — source B from register (bit [20:8]).
pub fn hadd2_reg(tv: &mut TranslatorVisitor, insn: u64) {
    let sat       = bit(insn, 32);
    let neg_b     = bit(insn, 31);
    let abs_b     = bit(insn, 30);
    let swizzle_b = Swizzle::from_u32(field(insn, 28, 2));
    let (merge, ftz, neg_a, abs_a, _, swizzle_a) = hadd2_mods(tv, insn);
    let src_b = tv.get_reg20(insn);
    hadd2_inner(tv, insn, merge, ftz, sat, abs_a, neg_a, swizzle_a, abs_b, neg_b, swizzle_b, src_b);
}

/// HADD2_cbuf — source B from constant buffer.
pub fn hadd2_cbuf(tv: &mut TranslatorVisitor, insn: u64) {
    let sat   = bit(insn, 52);
    let neg_b = bit(insn, 56);
    let abs_b = bit(insn, 54);
    let (merge, ftz, neg_a, abs_a, _, swizzle_a) = hadd2_mods(tv, insn);
    let src_b = tv.get_cbuf(insn);
    hadd2_inner(tv, insn, merge, ftz, sat, abs_a, neg_a, swizzle_a, abs_b, neg_b, Swizzle::F32, src_b);
}

/// HADD2_imm — source B from 16-bit immediate pair.
pub fn hadd2_imm(tv: &mut TranslatorVisitor, insn: u64) {
    let sat      = bit(insn, 52);
    let neg_high = bit(insn, 56);
    let high     = field(insn, 30, 9);
    let neg_low  = bit(insn, 29);
    let low      = field(insn, 20, 9);
    let imm: u32 = (low << 6)
        | (if neg_low  { 1u32 } else { 0u32 } << 15)
        | (high << 22)
        | (if neg_high { 1u32 } else { 0u32 } << 31);
    let (merge, ftz, neg_a, abs_a, _, swizzle_a) = hadd2_mods(tv, insn);
    let src_b = Value::ImmU32(imm);
    hadd2_inner(tv, insn, merge, ftz, sat, abs_a, neg_a, swizzle_a, false, false, Swizzle::H1H0, src_b);
}

/// HADD2_32I — source B from 32-bit immediate; uses explicit Merge::H1H0 and individual fields.
pub fn hadd2_32i(tv: &mut TranslatorVisitor, insn: u64) {
    let ftz       = bit(insn, 55);
    let sat       = bit(insn, 52);
    let neg_a     = bit(insn, 56);
    let swizzle_a = Swizzle::from_u32(field(insn, 53, 2));
    let imm: u32  = field(insn, 20, 32);
    let src_b     = Value::ImmU32(imm);
    hadd2_inner(tv, insn, Merge::H1H0, ftz, sat, false, neg_a, swizzle_a, false, false, Swizzle::H1H0, src_b);
}

/// Public entry-point stub used when a single dispatch opcode covers HADD2.
///
/// Delegates to `hadd2_reg` as the default variant.
pub fn hadd2(tv: &mut TranslatorVisitor, insn: u64) {
    hadd2_reg(tv, insn);
}
