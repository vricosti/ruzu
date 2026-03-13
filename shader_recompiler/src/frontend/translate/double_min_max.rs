// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/double_min_max.cpp

use super::{bit, field, TranslatorVisitor};
use crate::ir::value::{Pred, Value};

fn dmnmx_impl(tv: &mut TranslatorVisitor, insn: u64, src_b: Value) {
    let dst       = field(insn, 0, 8);
    let src_a_reg = field(insn, 8, 8);
    let pred_idx  = field(insn, 39, 3);
    let neg_pred  = bit(insn, 42);
    let neg_b     = bit(insn, 45);
    let abs_a     = bit(insn, 46);
    let neg_a     = bit(insn, 48);
    let abs_b     = bit(insn, 49);

    let pred = tv.ir.get_pred(Pred(pred_idx as u8), neg_pred);
    let src_a = tv.d(src_a_reg);
    let op_a = tv.ir.fp_abs_neg_64(src_a, abs_a, neg_a);
    let op_b = tv.ir.fp_abs_neg_64(src_b, abs_b, neg_b);

    let max_val = tv.ir.fp_max_64(op_a.clone(), op_b.clone());
    let min_val = tv.ir.fp_min_64(op_a, op_b);

    // pred=true → pick min; pred=false → pick max.
    // When neg_pred is set the pred was already negated, so we swap min/max selection.
    let result = tv.ir.select_u32(pred, min_val, max_val);
    tv.set_d(dst, result);
}

/// DMNMX_reg — register operand.
pub fn dmnmx_reg(tv: &mut TranslatorVisitor, insn: u64) {
    let src_b = tv.get_double_reg20(insn);
    dmnmx_impl(tv, insn, src_b);
}

/// DMNMX_cbuf — constant buffer operand.
pub fn dmnmx_cbuf(tv: &mut TranslatorVisitor, insn: u64) {
    let src_b = tv.get_double_cbuf(insn);
    dmnmx_impl(tv, insn, src_b);
}

/// DMNMX_imm — immediate operand.
pub fn dmnmx_imm(tv: &mut TranslatorVisitor, insn: u64) {
    let src_b = tv.get_double_imm20(insn);
    dmnmx_impl(tv, insn, src_b);
}

/// DMNMX — dispatch wrapper.
pub fn dmnmx(tv: &mut TranslatorVisitor, insn: u64) {
    dmnmx_reg(tv, insn);
}
