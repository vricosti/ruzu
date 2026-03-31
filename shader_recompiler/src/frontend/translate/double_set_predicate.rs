// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/double_set_predicate.cpp

use super::{bit, field, TranslatorVisitor};
use crate::ir::value::{Pred, Value};

/// FP64 comparison helper (same as in double_compare_and_set).
fn fp64_compare(tv: &mut TranslatorVisitor, cmp: u32, a: Value, b: Value) -> Value {
    match cmp {
        0 => tv.ir.imm_u1(false),
        1 => tv.ir.fp_ord_less_than_64(a, b),
        2 => tv.ir.fp_ord_equal_64(a, b),
        3 => tv.ir.fp_ord_less_than_equal_64(a, b),
        4 => tv.ir.fp_ord_greater_than_64(a, b),
        5 => tv.ir.fp_ord_not_equal_64(a, b),
        6 => tv.ir.fp_ord_greater_than_equal_64(a, b),
        7 => {
            let na = tv.ir.fp_is_nan_64(a);
            let nb = tv.ir.fp_is_nan_64(b);
            let e = tv.ir.logical_or(na, nb);
            tv.ir.logical_not(e)
        }
        8 => {
            let na = tv.ir.fp_is_nan_64(a);
            let nb = tv.ir.fp_is_nan_64(b);
            tv.ir.logical_or(na, nb)
        }
        9 => tv.ir.fp_unord_less_than_64(a, b),
        10 => tv.ir.fp_unord_equal_64(a, b),
        11 => tv.ir.fp_unord_less_than_64(a, b),
        12 => tv.ir.fp_unord_greater_than_64(a, b),
        13 => tv.ir.fp_unord_not_equal_64(a, b),
        14 => tv.ir.fp_unord_greater_than_64(a, b),
        15 => tv.ir.imm_u1(true),
        _ => tv.ir.imm_u1(false),
    }
}

fn combine_pred(tv: &mut TranslatorVisitor, r: Value, p: Value, bop: u32) -> Value {
    match bop {
        0 => tv.ir.logical_and(r, p),
        1 => tv.ir.logical_or(r, p),
        2 => tv.ir.logical_xor(r, p),
        _ => r,
    }
}

fn dsetp_impl(tv: &mut TranslatorVisitor, insn: u64, src_b: Value) {
    let dst_pred_b = field(insn, 0, 3);
    let dst_pred_a = field(insn, 3, 3);
    let neg_b = bit(insn, 6);
    let abs_a = bit(insn, 7);
    let src_a_reg = field(insn, 8, 8);
    let bop_pred_idx = field(insn, 39, 3);
    let neg_bop_pred = bit(insn, 42);
    let neg_a = bit(insn, 43);
    let abs_b = bit(insn, 44);
    let bop = field(insn, 45, 2);
    let cmp_op = field(insn, 48, 4);

    let src_a_val = tv.d(src_a_reg);
    let op_a = tv.ir.fp_abs_neg_64(src_a_val, abs_a, neg_a);
    let op_b = tv.ir.fp_abs_neg_64(src_b, abs_b, neg_b);

    let bop_pred = tv.ir.get_pred(Pred(bop_pred_idx as u8), neg_bop_pred);
    let cmp = fp64_compare(tv, cmp_op, op_a, op_b);

    let result_a = combine_pred(tv, cmp.clone(), bop_pred.clone(), bop);
    let neg_cmp = tv.ir.logical_not(cmp);
    let result_b = combine_pred(tv, neg_cmp, bop_pred, bop);

    tv.ir.set_pred(Pred(dst_pred_a as u8), result_a);
    tv.ir.set_pred(Pred(dst_pred_b as u8), result_b);
}

/// DSETP_reg.
pub fn dsetp_reg(tv: &mut TranslatorVisitor, insn: u64) {
    let src_b = tv.get_double_reg20(insn);
    dsetp_impl(tv, insn, src_b);
}

/// DSETP_cbuf.
pub fn dsetp_cbuf(tv: &mut TranslatorVisitor, insn: u64) {
    let src_b = tv.get_double_cbuf(insn);
    dsetp_impl(tv, insn, src_b);
}

/// DSETP_imm.
pub fn dsetp_imm(tv: &mut TranslatorVisitor, insn: u64) {
    let src_b = tv.get_double_imm20(insn);
    dsetp_impl(tv, insn, src_b);
}

/// DSETP — dispatch wrapper.
pub fn dsetp(tv: &mut TranslatorVisitor, insn: u64) {
    dsetp_reg(tv, insn);
}
