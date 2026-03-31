// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/double_multiply.cpp

use super::{bit, field, TranslatorVisitor};
use crate::ir::value::Value;

fn dmul_impl(tv: &mut TranslatorVisitor, insn: u64, src_b: Value) {
    let dst = field(insn, 0, 8);
    let src_a_reg = field(insn, 8, 8);
    let neg = bit(insn, 48);

    let src_a = tv.d(src_a_reg);
    let op_a = tv.ir.fp_abs_neg_64(src_a, false, neg);

    let result = tv.ir.fp_mul_64(op_a, src_b);
    tv.set_d(dst, result);
}

/// DMUL_reg — Double-precision floating-point multiply, register operand.
pub fn dmul_reg(tv: &mut TranslatorVisitor, insn: u64) {
    let src_b = tv.get_double_reg20(insn);
    dmul_impl(tv, insn, src_b);
}

/// DMUL_cbuf — Double-precision floating-point multiply, constant buffer operand.
pub fn dmul_cbuf(tv: &mut TranslatorVisitor, insn: u64) {
    let src_b = tv.get_double_cbuf(insn);
    dmul_impl(tv, insn, src_b);
}

/// DMUL_imm — Double-precision floating-point multiply, immediate operand.
pub fn dmul_imm(tv: &mut TranslatorVisitor, insn: u64) {
    let src_b = tv.get_double_imm20(insn);
    dmul_impl(tv, insn, src_b);
}

/// DMUL — dispatch wrapper (used by the opcode table when variant not split).
pub fn dmul(tv: &mut TranslatorVisitor, insn: u64) {
    dmul_reg(tv, insn);
}
