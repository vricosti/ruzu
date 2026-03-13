// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/double_add.cpp

use super::{bit, field, TranslatorVisitor};

fn dadd_impl(tv: &mut TranslatorVisitor, insn: u64, src_b: crate::ir::value::Value) {
    let dst  = field(insn, 0, 8);
    let src_a_reg = field(insn, 8, 8);

    let abs_a = bit(insn, 46);
    let neg_a = bit(insn, 48);
    let abs_b = bit(insn, 49);
    let neg_b = bit(insn, 45);

    let src_a = tv.d(src_a_reg);
    let op_a  = tv.ir.fp_abs_neg_64(src_a, abs_a, neg_a);
    let op_b  = tv.ir.fp_abs_neg_64(src_b, abs_b, neg_b);

    let result = tv.ir.fp_add_64(op_a, op_b);
    tv.set_d(dst, result);
}

/// DADD_reg — Double-precision floating-point add, register operand.
pub fn dadd_reg(tv: &mut TranslatorVisitor, insn: u64) {
    let src_b = tv.get_double_reg20(insn);
    dadd_impl(tv, insn, src_b);
}

/// DADD_cbuf — Double-precision floating-point add, constant buffer operand.
pub fn dadd_cbuf(tv: &mut TranslatorVisitor, insn: u64) {
    let src_b = tv.get_double_cbuf(insn);
    dadd_impl(tv, insn, src_b);
}

/// DADD_imm — Double-precision floating-point add, immediate operand.
pub fn dadd_imm(tv: &mut TranslatorVisitor, insn: u64) {
    let src_b = tv.get_double_imm20(insn);
    dadd_impl(tv, insn, src_b);
}

/// DADD — dispatch wrapper (single entry point used by the opcode table).
pub fn dadd(tv: &mut TranslatorVisitor, insn: u64) {
    // Dispatch based on opcode bits — for now route to reg form as default.
    // The real dispatch happens in translate_instruction via separate opcodes.
    dadd_reg(tv, insn);
}
