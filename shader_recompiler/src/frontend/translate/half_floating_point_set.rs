// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/half_floating_point_set.cpp

use super::half_floating_point_helper::{extract, Swizzle};
use super::{bit, field, TranslatorVisitor};
use crate::ir::value::{Pred, Value};

/// Perform a floating-point comparison for HSET2/HSETP2.
///
/// `cmp` is the 4-bit FPCompareOp encoding (same as FSET2).
/// Works on both F16 and F32 values — the opcode is selected based on whether the operand is F32.
fn fp_compare_hset(
    tv: &mut TranslatorVisitor,
    cmp: u32,
    a: Value,
    b: Value,
    is_f32: bool,
) -> Value {
    if is_f32 {
        match cmp {
            0 => tv.ir.imm_u1(false),
            1 => tv.ir.fp_ord_less_than_32(a, b),
            2 => tv.ir.fp_ord_equal_32(a, b),
            3 => tv.ir.fp_ord_less_than_equal_32(a, b),
            4 => tv.ir.fp_ord_greater_than_32(a, b),
            5 => tv.ir.fp_ord_not_equal_32(a, b),
            6 => tv.ir.fp_ord_greater_than_equal_32(a, b),
            7 => {
                let na = tv.ir.fp_is_nan_32(a);
                let nb = tv.ir.fp_is_nan_32(b);
                let e = tv.ir.logical_or(na, nb);
                tv.ir.logical_not(e)
            }
            8 => {
                let na = tv.ir.fp_is_nan_32(a);
                let nb = tv.ir.fp_is_nan_32(b);
                tv.ir.logical_or(na, nb)
            }
            9 => tv.ir.fp_unord_less_than_32(a, b),
            10 => tv.ir.fp_unord_equal_32(a, b),
            11 => tv.ir.fp_unord_less_than_32(a, b),
            12 => tv.ir.fp_unord_greater_than_32(a, b),
            13 => tv.ir.fp_unord_not_equal_32(a, b),
            14 => tv.ir.fp_unord_greater_than_32(a, b),
            15 => tv.ir.imm_u1(true),
            _ => tv.ir.imm_u1(false),
        }
    } else {
        match cmp {
            0 => tv.ir.imm_u1(false),
            1 => tv.ir.fp_ord_less_than_16(a, b),
            2 => tv.ir.fp_ord_equal_16(a, b),
            3 => tv.ir.fp_ord_less_than_equal_16(a, b),
            4 => tv.ir.fp_ord_greater_than_16(a, b),
            5 => tv.ir.fp_ord_not_equal_16(a, b),
            6 => tv.ir.fp_ord_greater_than_equal_16(a, b),
            7 => {
                let na = tv.ir.fp_is_nan_16(a);
                let nb = tv.ir.fp_is_nan_16(b);
                let e = tv.ir.logical_or(na, nb);
                tv.ir.logical_not(e)
            }
            8 => {
                let na = tv.ir.fp_is_nan_16(a);
                let nb = tv.ir.fp_is_nan_16(b);
                tv.ir.logical_or(na, nb)
            }
            9 => tv.ir.fp_unord_less_than_16(a, b),
            10 => tv.ir.fp_unord_equal_16(a, b),
            11 => tv.ir.fp_unord_less_than_equal_16(a, b),
            12 => tv.ir.fp_unord_greater_than_16(a, b),
            13 => tv.ir.fp_unord_not_equal_16(a, b),
            14 => tv.ir.fp_unord_greater_than_equal_16(a, b),
            15 => tv.ir.imm_u1(true),
            _ => tv.ir.imm_u1(false),
        }
    }
}

/// Apply a boolean operation to two predicates (AND=0, OR=1, XOR=2).
fn pred_combine(tv: &mut TranslatorVisitor, a: Value, b: Value, bop: u32) -> Value {
    match bop {
        0 => tv.ir.logical_and(a, b),
        1 => tv.ir.logical_or(a, b),
        2 => tv.ir.logical_xor(a, b),
        _ => a,
    }
}

/// Core HSET2 implementation.
fn hset2_inner(
    tv: &mut TranslatorVisitor,
    insn: u64,
    src_b: Value,
    bf: bool,
    ftz: bool,
    neg_b: bool,
    abs_b: bool,
    compare_op: u32,
    swizzle_b: Swizzle,
) {
    let dest_reg = field(insn, 0, 8);
    let src_a_reg = field(insn, 8, 8);
    let pred_idx = field(insn, 39, 3);
    let neg_pred = bit(insn, 42);
    let neg_a = bit(insn, 43);
    let bop = field(insn, 45, 2);
    let abs_a = bit(insn, 44);
    let swizzle_a = Swizzle::from_u32(field(insn, 47, 2));

    let src_a_val = tv.x(src_a_reg);
    let (mut lhs_a, mut rhs_a) = extract(tv, src_a_val, swizzle_a);
    let (mut lhs_b, mut rhs_b) = extract(tv, src_b, swizzle_b);

    let a_is_f32 = swizzle_a == Swizzle::F32;
    let b_is_f32 = swizzle_b == Swizzle::F32;
    if a_is_f32 != b_is_f32 {
        if !a_is_f32 {
            lhs_a = tv.ir.convert_f32_from_f16(lhs_a);
            rhs_a = tv.ir.convert_f32_from_f16(rhs_a);
        }
        if !b_is_f32 {
            lhs_b = tv.ir.convert_f32_from_f16(lhs_b);
            rhs_b = tv.ir.convert_f32_from_f16(rhs_b);
        }
    }

    let use_f32 = a_is_f32 || b_is_f32;
    let _ = ftz;

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

    let mut pred = tv.ir.get_pred(Pred(pred_idx as u8), false);
    if neg_pred {
        pred = tv.ir.logical_not(pred);
    }

    let cmp_lhs = fp_compare_hset(tv, compare_op, lhs_a, lhs_b, use_f32);
    let cmp_rhs = fp_compare_hset(tv, compare_op, rhs_a, rhs_b, use_f32);
    let bop_lhs = pred_combine(tv, cmp_lhs, pred, bop);
    let bop_rhs = pred_combine(tv, cmp_rhs, pred, bop);

    // true_value: if bf mode, 0x3c00 (F16 1.0), else 0xffff.
    let true_value = if bf { 0x3c00u32 } else { 0xffffu32 };
    let true_val_lhs = Value::ImmU32(true_value);
    let true_val_rhs = Value::ImmU32(true_value << 16);
    let fail = Value::ImmU32(0);
    let result_lhs = tv.ir.select_u32(bop_lhs, true_val_lhs, fail);
    let result_rhs = tv.ir.select_u32(bop_rhs, true_val_rhs, fail);
    let result = tv.ir.bitwise_or_32(result_lhs, result_rhs);
    tv.set_x(dest_reg, result);
}

/// HSET2_reg — source B from register.
pub fn hset2_reg(tv: &mut TranslatorVisitor, insn: u64) {
    let abs_b = bit(insn, 30);
    let bf = bit(insn, 49);
    let neg_b = bit(insn, 31);
    let ftz = bit(insn, 50);
    let compare_op = field(insn, 35, 4);
    let swizzle_b = Swizzle::from_u32(field(insn, 28, 2));
    let src_b = tv.get_reg20(insn);
    hset2_inner(
        tv, insn, src_b, bf, ftz, neg_b, abs_b, compare_op, swizzle_b,
    );
}

/// HSET2_cbuf — source B from constant buffer.
pub fn hset2_cbuf(tv: &mut TranslatorVisitor, insn: u64) {
    let bf = bit(insn, 53);
    let neg_b = bit(insn, 56);
    let ftz = bit(insn, 54);
    let compare_op = field(insn, 49, 4);
    let src_b = tv.get_cbuf(insn);
    hset2_inner(
        tv,
        insn,
        src_b,
        bf,
        ftz,
        neg_b,
        false,
        compare_op,
        Swizzle::F32,
    );
}

/// HSET2_imm — source B from 16-bit immediate pair.
pub fn hset2_imm(tv: &mut TranslatorVisitor, insn: u64) {
    let bf = bit(insn, 53);
    let ftz = bit(insn, 54);
    let compare_op = field(insn, 49, 4);
    let neg_high = bit(insn, 56);
    let high = field(insn, 30, 9);
    let neg_low = bit(insn, 29);
    let low = field(insn, 20, 9);
    let imm: u32 = (low << 6)
        | (if neg_low { 1u32 } else { 0u32 } << 15)
        | (high << 22)
        | (if neg_high { 1u32 } else { 0u32 } << 31);
    let src_b = Value::ImmU32(imm);
    hset2_inner(
        tv,
        insn,
        src_b,
        bf,
        ftz,
        false,
        false,
        compare_op,
        Swizzle::H1H0,
    );
}

/// Public entry-point stub used when a single dispatch opcode covers HSET2.
pub fn hset2(tv: &mut TranslatorVisitor, insn: u64) {
    hset2_reg(tv, insn);
}
