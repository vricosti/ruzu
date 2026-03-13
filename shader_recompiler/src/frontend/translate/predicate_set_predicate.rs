// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/predicate_set_predicate.cpp

use super::{bit, field, TranslatorVisitor};
use crate::ir::value::Pred;

/// PSETP — Predicate set predicate.
///
/// Computes two boolean results from three source predicates using two boolean operations,
/// writing both results (and their complement) to destination predicate registers.
pub fn psetp(tv: &mut TranslatorVisitor, insn: u64) {
    // BitField layout (upstream):
    //   [0:3]  dest_pred_b
    //   [3:3]  dest_pred_a
    //   [12:3] pred_a
    //   [15:1] neg_pred_a
    //   [24:2] bop_1
    //   [29:3] pred_b
    //   [32:1] neg_pred_b
    //   [39:3] pred_c
    //   [42:1] neg_pred_c
    //   [45:2] bop_2
    let dest_pred_b_idx = field(insn, 0, 3);
    let dest_pred_a_idx = field(insn, 3, 3);
    let pred_a_idx = field(insn, 12, 3);
    let neg_pred_a = bit(insn, 15);
    let bop_1 = field(insn, 24, 2);
    let pred_b_idx = field(insn, 29, 3);
    let neg_pred_b = bit(insn, 32);
    let pred_c_idx = field(insn, 39, 3);
    let neg_pred_c = bit(insn, 42);
    let bop_2 = field(insn, 45, 2);

    let pred_a = tv.ir.get_pred(Pred(pred_a_idx as u8), neg_pred_a);
    let pred_b = tv.ir.get_pred(Pred(pred_b_idx as u8), neg_pred_b);
    let pred_c = tv.ir.get_pred(Pred(pred_c_idx as u8), neg_pred_c);

    // lhs_a = PredicateCombine(pred_a, pred_b, bop_1)
    let lhs_a = predicate_combine(tv, pred_a, pred_b, bop_1);
    // lhs_b = PredicateCombine(NOT pred_a, pred_b, bop_1)
    let not_pred_a = tv.ir.logical_not(pred_a);
    let lhs_b = predicate_combine(tv, not_pred_a, pred_b, bop_1);
    // result_a = PredicateCombine(lhs_a, pred_c, bop_2)
    let result_a = predicate_combine(tv, lhs_a, pred_c, bop_2);
    // result_b = PredicateCombine(lhs_b, pred_c, bop_2)
    let result_b = predicate_combine(tv, lhs_b, pred_c, bop_2);

    tv.ir.set_pred(Pred(dest_pred_a_idx as u8), result_a);
    tv.ir.set_pred(Pred(dest_pred_b_idx as u8), result_b);
}

/// Apply a boolean operation to two predicates (AND=0, OR=1, XOR=2).
fn predicate_combine(
    tv: &mut TranslatorVisitor,
    a: crate::ir::value::Value,
    b: crate::ir::value::Value,
    op: u32,
) -> crate::ir::value::Value {
    match op {
        0 => tv.ir.logical_and(a, b),
        1 => tv.ir.logical_or(a, b),
        2 => tv.ir.logical_xor(a, b),
        _ => a,
    }
}
