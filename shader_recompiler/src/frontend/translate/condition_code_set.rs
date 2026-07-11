// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/condition_code_set.cpp
//!
//! CSET and CSETP test the Maxwell condition code flags (Z, S, C, O) via a
//! flow-test selector.

use super::{bit, field, TranslatorVisitor};
use crate::ir::{value::Pred, FlowTest};

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
        _ => panic!("Invalid boolean operation {op}"),
    }
}

/// CSET — Condition code set register.
pub fn cset(tv: &mut TranslatorVisitor, insn: u64) {
    let dest_reg = field(insn, 0, 8);
    let cc_test = FlowTest::from_u64(field(insn, 8, 5) as u64).unwrap();
    let bop_pred = Pred(field(insn, 39, 3) as u8);
    let neg_bop_pred = bit(insn, 42);
    let bf = bit(insn, 44);
    let bop = field(insn, 45, 2);
    let cc = bit(insn, 47);

    let pass_result = if bf { 0x3f80_0000 } else { u32::MAX };
    let cc_test_result = tv.ir.get_flow_test_result(cc_test);
    let bop_pred = tv.ir.get_pred(bop_pred, neg_bop_pred);
    let pred_result = predicate_combine(tv, cc_test_result, bop_pred, bop);
    let result = tv
        .ir
        .select_u32(pred_result, tv.ir.imm_u32(pass_result), tv.ir.imm_u32(0));
    tv.set_x(dest_reg, result);
    if cc {
        let is_zero = tv.ir.i_equal(result, tv.ir.imm_u32(0));
        tv.ir.set_z_flag(is_zero);
        if bf {
            tv.ir.set_s_flag(tv.ir.imm_u1(false));
        } else {
            let is_nonzero = tv.ir.logical_not(is_zero);
            tv.ir.set_s_flag(is_nonzero);
        }
        tv.ir.set_o_flag(tv.ir.imm_u1(false));
        tv.ir.set_c_flag(tv.ir.imm_u1(false));
    }
}

/// CSETP — Condition code set predicate.
pub fn csetp(tv: &mut TranslatorVisitor, insn: u64) {
    let dest_pred_b = Pred(field(insn, 0, 3) as u8);
    let dest_pred_a = Pred(field(insn, 3, 3) as u8);
    let cc_test = FlowTest::from_u64(field(insn, 8, 5) as u64).unwrap();
    let bop_pred = Pred(field(insn, 39, 3) as u8);
    let neg_bop_pred = bit(insn, 42);
    let bop = field(insn, 45, 2);

    let bop_pred = tv.ir.get_pred(bop_pred, neg_bop_pred);
    let cc_test_result = tv.ir.get_flow_test_result(cc_test);
    let result_a = predicate_combine(tv, cc_test_result, bop_pred, bop);
    let not_cc_test_result = tv.ir.logical_not(cc_test_result);
    let result_b = predicate_combine(tv, not_cc_test_result, bop_pred, bop);
    tv.ir.set_pred(dest_pred_a, result_a);
    tv.ir.set_pred(dest_pred_b, result_b);
}
