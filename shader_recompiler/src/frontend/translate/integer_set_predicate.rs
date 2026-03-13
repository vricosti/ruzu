// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/integer_set_predicate.cpp

use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::value::{Pred, Value};

/// Integer comparison.
fn int_compare(tv: &mut TranslatorVisitor, cmp: u32, a: Value, b: Value, is_signed: bool) -> Value {
    match cmp {
        1 => {
            if is_signed {
                tv.ir.s_less_than(a, b)
            } else {
                tv.ir.u_less_than(a, b)
            }
        }
        2 => tv.ir.i_equal(a, b),
        3 => {
            if is_signed {
                tv.ir.s_less_than_equal(a, b)
            } else {
                tv.ir.u_less_than_equal(a, b)
            }
        }
        4 => {
            if is_signed {
                tv.ir.s_greater_than(a, b)
            } else {
                tv.ir.u_greater_than(a, b)
            }
        }
        5 => tv.ir.i_not_equal(a, b),
        6 => {
            if is_signed {
                tv.ir.s_greater_than_equal(a, b)
            } else {
                tv.ir.u_greater_than_equal(a, b)
            }
        }
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

pub fn isetp(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let src_a = tv.x(tv.src_a_reg(insn));
    let src_b = tv.decode_src_b(insn, opcode);

    let cmp_op = field(insn, 49, 3);
    let bool_op = field(insn, 45, 2);
    let is_signed = bit(insn, 48);
    let pred_idx = field(insn, 39, 3);
    let pred39 = tv.ir.get_pred(Pred(pred_idx as u8), false);

    let cmp_result = int_compare(tv, cmp_op, src_a, src_b, is_signed);
    let result = combine_pred(tv, cmp_result, pred39, bool_op);

    let dst_p = tv.dst_pred(insn);
    tv.ir.set_pred(Pred(dst_p as u8), result);

    let dst_p2 = field(insn, 3, 3);
    if dst_p2 != 7 {
        let neg_result = tv.ir.logical_not(result);
        tv.ir.set_pred(Pred(dst_p2 as u8), neg_result);
    }
}
