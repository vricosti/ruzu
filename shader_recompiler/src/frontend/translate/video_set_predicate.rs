// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/video_set_predicate.cpp

use super::video_helper::{extract_video_operand_value, get_video_source_width, VideoWidth};
use super::{bit, field, TranslatorVisitor};
use crate::ir::value::{Pred, Value};

/// VSETP compare operation codes (from upstream `VsetpCompareOp`).
///
/// Values match the field encoding in the instruction:
/// 0–7 map to False/LT/EQ/LE, then 16–23 map to GT/NE/GE/True.
fn integer_compare(tv: &mut TranslatorVisitor, a: Value, b: Value, op: u32, signed: bool) -> Value {
    match op {
        0 => tv.ir.imm_u1(false), // False
        1 => {
            if signed {
                tv.ir.s_less_than(a, b)
            } else {
                tv.ir.u_less_than(a, b)
            }
        } // LT
        2 => tv.ir.i_equal(a, b), // EQ
        3 => {
            if signed {
                tv.ir.s_less_than_equal(a, b)
            } else {
                tv.ir.u_less_than_equal(a, b)
            }
        } // LE
        // 4–15 unused; 16 onward for GT/NE/GE/True
        16 => {
            if signed {
                tv.ir.s_greater_than(a, b)
            } else {
                tv.ir.u_greater_than(a, b)
            }
        } // GT
        17 => tv.ir.i_not_equal(a, b), // NE
        18 => {
            if signed {
                tv.ir.s_greater_than_equal(a, b)
            } else {
                tv.ir.u_greater_than_equal(a, b)
            }
        } // GE
        19 => tv.ir.imm_u1(true),      // True
        _ => tv.ir.imm_u1(false),
    }
}

/// Apply a boolean operation to two predicates.
fn pred_combine(tv: &mut TranslatorVisitor, a: Value, b: Value, bop: u32) -> Value {
    match bop {
        0 => tv.ir.logical_and(a, b),
        1 => tv.ir.logical_or(a, b),
        2 => tv.ir.logical_xor(a, b),
        _ => a,
    }
}

/// VSETP — Video set predicate.
///
/// Extracts video-width sub-word operands from src_a and src_b, performs an integer comparison,
/// combines the result with a third predicate via a boolean op, and writes two result predicates.
pub fn vsetp(tv: &mut TranslatorVisitor, insn: u64) {
    let dest_pred_b = Pred(field(insn, 0, 3) as u8);
    let dest_pred_a = Pred(field(insn, 3, 3) as u8);
    let src_b_imm = field(insn, 20, 16);
    let src_b_sel = field(insn, 28, 2);
    let src_b_width = VideoWidth::from_u32(field(insn, 29, 2));
    let src_a_sel = field(insn, 36, 2);
    let src_a_width = VideoWidth::from_u32(field(insn, 37, 2));
    let bop_pred_idx = field(insn, 39, 3);
    let neg_bop_pred = bit(insn, 42);
    let compare_op = field(insn, 43, 5);
    let bop = field(insn, 45, 2);
    let src_a_sign = bit(insn, 48);
    let src_b_sign = bit(insn, 49);
    let is_src_b_reg = bit(insn, 50);

    let is_b_imm = !is_src_b_reg;
    let src_a_reg = field(insn, 8, 8);
    let src_a_val = tv.x(src_a_reg);
    let src_b_val = if is_b_imm {
        Value::ImmU32(src_b_imm)
    } else {
        tv.get_reg20(insn)
    };

    let a_sel = src_a_sel;
    let b_sel = src_b_sel;
    let b_width = get_video_source_width(src_b_width, is_b_imm);

    let op_a = extract_video_operand_value(tv, src_a_val, src_a_width, a_sel, src_a_sign);
    let op_b = extract_video_operand_value(tv, src_b_val, b_width, b_sel, src_b_sign);

    // Compare sign is only dependent on operand b's sign (matching upstream).
    let compare_signed = src_b_sign;
    let comparison = integer_compare(tv, op_a, op_b, compare_op, compare_signed);
    let not_comparison = tv.ir.logical_not(comparison);

    let bop_pred = tv.ir.get_pred(Pred(bop_pred_idx as u8), neg_bop_pred);
    let result_a = pred_combine(tv, comparison, bop_pred, bop);
    let result_b = pred_combine(tv, not_comparison, bop_pred, bop);

    tv.ir.set_pred(dest_pred_a, result_a);
    tv.ir.set_pred(dest_pred_b, result_b);
}
