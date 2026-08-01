// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/floating_point_compare.cpp

use super::common_funcs::floating_point_compare_32;
use super::{bit, field, TranslatorVisitor};
use crate::ir::types::{FmzMode, FpControl};
use crate::ir::value::Value;

fn fcmp_impl(tv: &mut TranslatorVisitor, insn: u64, src_a: Value, operand: Value) {
    let dst = field(insn, 0, 8);
    let src_reg = field(insn, 8, 8);
    let ftz = bit(insn, 47);
    let cmp_op = field(insn, 48, 4);

    let zero = Value::ImmF32(0.0f32);
    let control = FpControl {
        no_contraction: false,
        rounding: Default::default(),
        fmz_mode: if ftz { FmzMode::FTZ } else { FmzMode::None },
    };
    let cmp_result = floating_point_compare_32(tv, operand, zero, cmp_op, control);
    let src_reg_val = tv.x(src_reg);
    let result = tv.ir.select_u32(cmp_result, src_reg_val, src_a);
    tv.set_x(dst, result);
}

/// FCMP_reg — both src_a and operand from registers.
pub fn fcmp_reg(tv: &mut TranslatorVisitor, insn: u64) {
    let src_a = tv.get_reg20(insn);
    let operand = tv.get_float_reg39(insn);
    fcmp_impl(tv, insn, src_a, operand);
}

/// FCMP_rc — src_a from reg39, operand from cbuf.
pub fn fcmp_rc(tv: &mut TranslatorVisitor, insn: u64) {
    let src_a = tv.get_reg39(insn);
    let operand = tv.get_float_cbuf(insn);
    fcmp_impl(tv, insn, src_a, operand);
}

/// FCMP_cr — src_a from cbuf, operand from reg39.
pub fn fcmp_cr(tv: &mut TranslatorVisitor, insn: u64) {
    let src_a = tv.get_cbuf(insn);
    let operand = tv.get_float_reg39(insn);
    fcmp_impl(tv, insn, src_a, operand);
}

/// FCMP_imm — src_a from immediate, operand from reg39.
pub fn fcmp_imm(tv: &mut TranslatorVisitor, insn: u64) {
    let imm_bits = (field(insn, 20, 19) << 12) | if bit(insn, 56) { 1u32 << 31 } else { 0 };
    let src_a = Value::ImmU32(imm_bits);
    let operand = tv.get_float_reg39(insn);
    fcmp_impl(tv, insn, src_a, operand);
}

/// FCMP — dispatch wrapper.
pub fn fcmp(tv: &mut TranslatorVisitor, insn: u64) {
    fcmp_reg(tv, insn);
}
