// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/floating_point_set_predicate.cpp

use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::value::{Pred, Value};

/// FP comparison predicate (from bits).
fn fp_compare(tv: &mut TranslatorVisitor, cmp: u32, a: Value, b: Value) -> Value {
    match cmp {
        0 => tv.ir.imm_u1(false),                      // F (false)
        1 => tv.ir.fp_ord_less_than_32(a, b),          // LT
        2 => tv.ir.fp_ord_equal_32(a, b),              // EQ
        3 => tv.ir.fp_ord_less_than_equal_32(a, b),    // LE
        4 => tv.ir.fp_ord_greater_than_32(a, b),       // GT
        5 => tv.ir.fp_ord_not_equal_32(a, b),          // NE
        6 => tv.ir.fp_ord_greater_than_equal_32(a, b), // GE
        7 => {
            // NUM (ordered)
            let nan_a = tv.ir.fp_is_nan_32(a);
            let nan_b = tv.ir.fp_is_nan_32(b);
            let either_nan = tv.ir.logical_or(nan_a, nan_b);
            tv.ir.logical_not(either_nan)
        }
        8 => {
            // NAN (unordered)
            let nan_a = tv.ir.fp_is_nan_32(a);
            let nan_b = tv.ir.fp_is_nan_32(b);
            tv.ir.logical_or(nan_a, nan_b)
        }
        9 => tv.ir.fp_unord_less_than_32(a, b),     // LTU
        10 => tv.ir.fp_unord_equal_32(a, b),        // EQU
        11 => tv.ir.fp_unord_less_than_32(a, b),    // LEU (approximation)
        12 => tv.ir.fp_unord_greater_than_32(a, b), // GTU
        13 => tv.ir.fp_unord_not_equal_32(a, b),    // NEU
        14 => tv.ir.fp_unord_greater_than_32(a, b), // GEU (approximation)
        15 => tv.ir.imm_u1(true),                   // T (true)
        _ => tv.ir.imm_u1(false),
    }
}

/// Combine comparison result with predicate via boolean op.
fn combine_pred(tv: &mut TranslatorVisitor, result: Value, pred: Value, bool_op: u32) -> Value {
    match bool_op {
        0 => tv.ir.logical_and(result, pred), // AND
        1 => tv.ir.logical_or(result, pred),  // OR
        2 => tv.ir.logical_xor(result, pred), // XOR
        _ => result,
    }
}

pub fn fsetp(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let src_a = tv.f(tv.src_a_reg(insn));
    let src_b = tv.decode_src_b_f32(insn, opcode);

    let abs_a = bit(insn, 7);
    let abs_b = bit(insn, 44);
    let neg_a = bit(insn, 43);
    let neg_b = bit(insn, 6);

    let a = tv.ir.fp_abs_neg_32(src_a, abs_a, neg_a);
    let b = tv.ir.fp_abs_neg_32(src_b, abs_b, neg_b);

    let cmp_op = field(insn, 48, 4);
    let bool_op = field(insn, 45, 2);
    let pred_idx = field(insn, 39, 3);
    let pred39 = tv.ir.get_pred(Pred(pred_idx as u8), false);

    let cmp_result = fp_compare(tv, cmp_op, a, b);
    let result = combine_pred(tv, cmp_result, pred39, bool_op);

    let dst_p = tv.dst_pred(insn);
    tv.ir.set_pred(Pred(dst_p as u8), result);

    // Secondary predicate destination
    let dst_p2 = field(insn, 3, 3);
    if dst_p2 != 7 {
        let neg_result = tv.ir.logical_not(result);
        tv.ir.set_pred(Pred(dst_p2 as u8), neg_result);
    }
}
