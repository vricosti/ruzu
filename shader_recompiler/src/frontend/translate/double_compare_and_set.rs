// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/double_compare_and_set.cpp

use super::{bit, field, TranslatorVisitor};
use crate::ir::value::{Pred, Value};

/// FP64 comparison.  Corresponds to `FloatingPointCompare` for F64 operands.
fn fp64_compare(tv: &mut TranslatorVisitor, cmp: u32, a: Value, b: Value) -> Value {
    match cmp {
        0  => tv.ir.imm_u1(false),
        1  => tv.ir.fp_ord_less_than_64(a, b),
        2  => tv.ir.fp_ord_equal_64(a, b),
        3  => tv.ir.fp_ord_less_than_equal_64(a, b),
        4  => tv.ir.fp_ord_greater_than_64(a, b),
        5  => tv.ir.fp_ord_not_equal_64(a, b),
        6  => tv.ir.fp_ord_greater_than_equal_64(a, b),
        7  => { let na = tv.ir.fp_is_nan_64(a); let nb = tv.ir.fp_is_nan_64(b); let e = tv.ir.logical_or(na, nb); tv.ir.logical_not(e) }
        8  => { let na = tv.ir.fp_is_nan_64(a); let nb = tv.ir.fp_is_nan_64(b); tv.ir.logical_or(na, nb) }
        9  => tv.ir.fp_unord_less_than_64(a, b),
        10 => tv.ir.fp_unord_equal_64(a, b),
        11 => tv.ir.fp_unord_less_than_64(a, b),
        12 => tv.ir.fp_unord_greater_than_64(a, b),
        13 => tv.ir.fp_unord_not_equal_64(a, b),
        14 => tv.ir.fp_unord_greater_than_64(a, b),
        15 => tv.ir.imm_u1(true),
        _  => tv.ir.imm_u1(false),
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

fn dset_impl(tv: &mut TranslatorVisitor, insn: u64, src_b: Value) {
    let dst       = field(insn, 0, 8);
    let src_a_reg = field(insn, 8, 8);
    let pred_idx  = field(insn, 39, 3);
    let neg_pred  = bit(insn, 42);
    let neg_a     = bit(insn, 43);
    let abs_b     = bit(insn, 44);
    let bop       = field(insn, 45, 2);
    let cmp_op    = field(insn, 48, 4);
    let bf_mode   = bit(insn, 52);
    let neg_b     = bit(insn, 53);
    let abs_a     = bit(insn, 54);

    let src_a = tv.d(src_a_reg);
    let op_a = tv.ir.fp_abs_neg_64(src_a, abs_a, neg_a);
    let op_b = tv.ir.fp_abs_neg_64(src_b, abs_b, neg_b);

    let pred = tv.ir.get_pred(Pred(pred_idx as u8), neg_pred);
    let cmp  = fp64_compare(tv, cmp_op, op_a, op_b);
    let bop_result = combine_pred(tv, cmp, pred, bop);

    let true_val = if bf_mode { Value::ImmU32(0x3F800000) } else { Value::ImmU32(0xFFFFFFFF) };
    let result = tv.ir.select_u32(bop_result, true_val, Value::ImmU32(0));
    tv.set_x(dst, result);
}

/// DSET_reg.
pub fn dset_reg(tv: &mut TranslatorVisitor, insn: u64) {
    let src_b = tv.get_double_reg20(insn);
    dset_impl(tv, insn, src_b);
}

/// DSET_cbuf.
pub fn dset_cbuf(tv: &mut TranslatorVisitor, insn: u64) {
    let src_b = tv.get_double_cbuf(insn);
    dset_impl(tv, insn, src_b);
}

/// DSET_imm.
pub fn dset_imm(tv: &mut TranslatorVisitor, insn: u64) {
    let src_b = tv.get_double_imm20(insn);
    dset_impl(tv, insn, src_b);
}

/// DSET — dispatch wrapper.
pub fn dset(tv: &mut TranslatorVisitor, insn: u64) {
    dset_reg(tv, insn);
}
