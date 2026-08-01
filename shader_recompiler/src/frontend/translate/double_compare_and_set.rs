// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/double_compare_and_set.cpp

use super::common_funcs::{floating_point_compare_64, predicate_combine};
use super::{bit, field, TranslatorVisitor};
use crate::ir::types::FpControl;
use crate::ir::value::{Pred, Value};

fn dset_impl(tv: &mut TranslatorVisitor, insn: u64, src_b: Value) {
    let dst = field(insn, 0, 8);
    let src_a_reg = field(insn, 8, 8);
    let pred_idx = field(insn, 39, 3);
    let neg_pred = bit(insn, 42);
    let neg_a = bit(insn, 43);
    let abs_b = bit(insn, 44);
    let bop = field(insn, 45, 2);
    let cc = bit(insn, 47);
    let cmp_op = field(insn, 48, 4);
    let bf_mode = bit(insn, 52);
    let neg_b = bit(insn, 53);
    let abs_a = bit(insn, 54);

    let src_a = tv.d(src_a_reg);
    let op_a = tv.ir.fp_abs_neg_64(src_a, abs_a, neg_a);
    let op_b = tv.ir.fp_abs_neg_64(src_b, abs_b, neg_b);

    let pred = tv.ir.get_pred(Pred(pred_idx as u8), neg_pred);
    let cmp = floating_point_compare_64(tv, op_a, op_b, cmp_op, FpControl::default());
    let bop_result = predicate_combine(tv, cmp, pred, bop);

    let true_val = if bf_mode {
        Value::ImmU32(0x3F800000)
    } else {
        Value::ImmU32(0xFFFFFFFF)
    };
    let result = tv.ir.select_u32(bop_result, true_val, Value::ImmU32(0));
    tv.set_x(dst, result.clone());

    if cc {
        let zero = Value::ImmU32(0);
        let is_zero = tv.ir.i_equal(result, zero);
        tv.ir.set_z_flag(is_zero.clone());
        let sign = if bf_mode {
            tv.ir.imm_u1(false)
        } else {
            tv.ir.logical_not(is_zero)
        };
        tv.ir.set_s_flag(sign);
        tv.ir.set_c_flag(Value::ImmU1(false));
        tv.ir.set_o_flag(Value::ImmU1(false));
    }
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
