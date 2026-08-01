// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/predicate_set_register.cpp

use super::common_funcs::predicate_combine;
use super::{bit, field, TranslatorVisitor};
use crate::ir::value::{Pred, Value};

/// PSET — Predicate set register.
///
/// Combines three source predicates through two boolean operations and writes
/// the result as 0xFFFFFFFF / 0x3F800000 (fp one) or 0 into a destination register.
///
/// Upstream: `TranslatorVisitor::PSET(u64 insn)`
pub fn pset(tv: &mut TranslatorVisitor, insn: u64) {
    let dst = field(insn, 0, 8);
    let pred_a_idx = field(insn, 12, 3);
    let neg_a = bit(insn, 15);
    let bop1 = field(insn, 24, 2);
    let pred_b_idx = field(insn, 29, 3);
    let neg_b = bit(insn, 32);
    let pred_c_idx = field(insn, 39, 3);
    let neg_c = bit(insn, 42);
    let bf = bit(insn, 44);
    let bop2 = field(insn, 45, 2);
    let cc = bit(insn, 47);

    let pred_a = tv.ir.get_pred(Pred(pred_a_idx as u8), neg_a);
    let pred_b = tv.ir.get_pred(Pred(pred_b_idx as u8), neg_b);
    let pred_c = tv.ir.get_pred(Pred(pred_c_idx as u8), neg_c);

    let res1 = predicate_combine(tv, pred_a, pred_b, bop1);
    let res2 = predicate_combine(tv, res1, pred_c, bop2);

    let true_result = if bf {
        Value::ImmU32(0x3F800000)
    } else {
        Value::ImmU32(0xFFFFFFFF)
    };
    let result = tv.ir.select_u32(res2, true_result, Value::ImmU32(0));
    tv.set_x(dst, result.clone());

    if cc {
        let zero = Value::ImmU32(0);
        let is_zero = tv.ir.i_equal(result, zero);
        tv.ir.set_z_flag(is_zero.clone());
        let sign = if bf {
            tv.ir.imm_u1(false)
        } else {
            tv.ir.logical_not(is_zero)
        };
        tv.ir.set_s_flag(sign);
        tv.ir.set_o_flag(Value::ImmU1(false));
        tv.ir.set_c_flag(Value::ImmU1(false));
    }
}
