// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Predicate translation: PSETP, PSET.

use super::{bit, field, TranslatorVisitor};
use crate::ir::value::{Pred, Value};

pub fn psetp(tv: &mut TranslatorVisitor, insn: u64) {
    let pred_a_idx = field(insn, 12, 3);
    let pred_a_neg = bit(insn, 15);
    let pred_b_idx = field(insn, 29, 3);
    let pred_b_neg = bit(insn, 32);
    let pred_c_idx = field(insn, 39, 3);
    let pred_c_neg = bit(insn, 42);

    let bool_op_ab = field(insn, 24, 2);
    let bool_op_c = field(insn, 45, 2);

    let a = tv.ir.get_pred(Pred(pred_a_idx as u8), pred_a_neg);
    let b = tv.ir.get_pred(Pred(pred_b_idx as u8), pred_b_neg);
    let c = tv.ir.get_pred(Pred(pred_c_idx as u8), pred_c_neg);

    let ab = match bool_op_ab {
        0 => tv.ir.logical_and(a, b),
        1 => tv.ir.logical_or(a, b),
        2 => tv.ir.logical_xor(a, b),
        _ => a,
    };

    let result = match bool_op_c {
        0 => tv.ir.logical_and(ab, c),
        1 => tv.ir.logical_or(ab, c),
        2 => tv.ir.logical_xor(ab, c),
        _ => ab,
    };

    let dst_p = tv.dst_pred(insn);
    tv.ir.set_pred(Pred(dst_p as u8), result);

    let dst_p2 = field(insn, 3, 3);
    if dst_p2 != 7 {
        let neg_result = tv.ir.logical_not(result);
        tv.ir.set_pred(Pred(dst_p2 as u8), neg_result);
    }
}

pub fn pset(tv: &mut TranslatorVisitor, insn: u64) {
    let dst = tv.dst_reg(insn);
    let pred_a_idx = field(insn, 12, 3);
    let pred_a_neg = bit(insn, 15);
    let pred_b_idx = field(insn, 29, 3);
    let pred_b_neg = bit(insn, 32);
    let pred_c_idx = field(insn, 39, 3);
    let pred_c_neg = bit(insn, 42);

    let bool_op_ab = field(insn, 24, 2);
    let bool_op_c = field(insn, 45, 2);
    let bf_mode = bit(insn, 44);

    let a = tv.ir.get_pred(Pred(pred_a_idx as u8), pred_a_neg);
    let b = tv.ir.get_pred(Pred(pred_b_idx as u8), pred_b_neg);
    let c = tv.ir.get_pred(Pred(pred_c_idx as u8), pred_c_neg);

    let ab = match bool_op_ab {
        0 => tv.ir.logical_and(a, b),
        1 => tv.ir.logical_or(a, b),
        2 => tv.ir.logical_xor(a, b),
        _ => a,
    };

    let result = match bool_op_c {
        0 => tv.ir.logical_and(ab, c),
        1 => tv.ir.logical_or(ab, c),
        2 => tv.ir.logical_xor(ab, c),
        _ => ab,
    };

    let true_val = if bf_mode {
        Value::ImmU32(0x3F800000)
    } else {
        Value::ImmU32(0xFFFFFFFF)
    };
    let output = tv.ir.select_u32(result, true_val, Value::ImmU32(0));

    tv.set_x(dst, output);
}
