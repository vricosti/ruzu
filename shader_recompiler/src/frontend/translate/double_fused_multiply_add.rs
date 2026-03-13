// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/double_fused_multiply_add.cpp

use super::{bit, field, TranslatorVisitor};
use crate::ir::value::Value;

fn dfma_impl(tv: &mut TranslatorVisitor, insn: u64, src_b: Value, src_c: Value) {
    let dst       = field(insn, 0, 8);
    let src_a_reg = field(insn, 8, 8);
    let neg_b     = bit(insn, 48);
    let neg_c     = bit(insn, 49);

    let src_a = tv.d(src_a_reg);
    let op_b  = tv.ir.fp_abs_neg_64(src_b, false, neg_b);
    let op_c  = tv.ir.fp_abs_neg_64(src_c, false, neg_c);

    let result = tv.ir.fp_fma_64(src_a, op_b, op_c);
    tv.set_d(dst, result);
}

/// DFMA_reg — reg, reg, reg.
pub fn dfma_reg(tv: &mut TranslatorVisitor, insn: u64) {
    let src_b = tv.get_double_reg20(insn);
    let src_c = tv.get_double_reg39(insn);
    dfma_impl(tv, insn, src_b, src_c);
}

/// DFMA_cr — cbuf, reg.
pub fn dfma_cr(tv: &mut TranslatorVisitor, insn: u64) {
    let src_b = tv.get_double_cbuf(insn);
    let src_c = tv.get_double_reg39(insn);
    dfma_impl(tv, insn, src_b, src_c);
}

/// DFMA_rc — reg, cbuf.
pub fn dfma_rc(tv: &mut TranslatorVisitor, insn: u64) {
    let src_b = tv.get_double_reg39(insn);
    let src_c = tv.get_double_cbuf(insn);
    dfma_impl(tv, insn, src_b, src_c);
}

/// DFMA_imm — imm, reg.
pub fn dfma_imm(tv: &mut TranslatorVisitor, insn: u64) {
    let src_b = tv.get_double_imm20(insn);
    let src_c = tv.get_double_reg39(insn);
    dfma_impl(tv, insn, src_b, src_c);
}

/// DFMA — dispatch wrapper.
pub fn dfma(tv: &mut TranslatorVisitor, insn: u64) {
    dfma_reg(tv, insn);
}
