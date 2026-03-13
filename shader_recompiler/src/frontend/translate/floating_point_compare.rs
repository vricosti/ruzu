// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/floating_point_compare.cpp

use super::{bit, field, TranslatorVisitor};
use crate::ir::value::Value;

/// FP32 comparison predicate helper.
fn fp_compare(tv: &mut TranslatorVisitor, cmp: u32, a: Value, b: Value) -> Value {
    match cmp {
        0  => tv.ir.imm_u1(false),
        1  => tv.ir.fp_ord_less_than_32(a, b),
        2  => tv.ir.fp_ord_equal_32(a, b),
        3  => tv.ir.fp_ord_less_than_equal_32(a, b),
        4  => tv.ir.fp_ord_greater_than_32(a, b),
        5  => tv.ir.fp_ord_not_equal_32(a, b),
        6  => tv.ir.fp_ord_greater_than_equal_32(a, b),
        7  => { let na = tv.ir.fp_is_nan_32(a); let nb = tv.ir.fp_is_nan_32(b); let e = tv.ir.logical_or(na, nb); tv.ir.logical_not(e) }
        8  => { let na = tv.ir.fp_is_nan_32(a); let nb = tv.ir.fp_is_nan_32(b); tv.ir.logical_or(na, nb) }
        9  => tv.ir.fp_unord_less_than_32(a, b),
        10 => tv.ir.fp_unord_equal_32(a, b),
        11 => tv.ir.fp_unord_less_than_32(a, b),
        12 => tv.ir.fp_unord_greater_than_32(a, b),
        13 => tv.ir.fp_unord_not_equal_32(a, b),
        14 => tv.ir.fp_unord_greater_than_32(a, b),
        15 => tv.ir.imm_u1(true),
        _  => tv.ir.imm_u1(false),
    }
}

fn fcmp_impl(tv: &mut TranslatorVisitor, insn: u64, src_a: Value, operand: Value) {
    let dst       = field(insn, 0, 8);
    let src_reg   = field(insn, 8, 8);
    let _ftz      = bit(insn, 47);
    let cmp_op    = field(insn, 48, 4);

    // Compare operand against zero; select src_reg or src_a based on result.
    let zero = Value::ImmF32(0.0f32);
    let cmp_result = fp_compare(tv, cmp_op, operand, zero);
    let src_reg_val = tv.x(src_reg);
    let result = tv.ir.select_u32(cmp_result, src_reg_val, src_a);
    tv.set_x(dst, result);
}

/// FCMP_reg — both src_a and operand from registers.
pub fn fcmp_reg(tv: &mut TranslatorVisitor, insn: u64) {
    let src_a   = tv.get_reg20(insn);
    let operand = tv.get_float_reg39(insn);
    fcmp_impl(tv, insn, src_a, operand);
}

/// FCMP_rc — src_a from reg39, operand from cbuf.
pub fn fcmp_rc(tv: &mut TranslatorVisitor, insn: u64) {
    let src_a   = tv.get_reg39(insn);
    let operand = tv.get_float_cbuf(insn);
    fcmp_impl(tv, insn, src_a, operand);
}

/// FCMP_cr — src_a from cbuf, operand from reg39.
pub fn fcmp_cr(tv: &mut TranslatorVisitor, insn: u64) {
    let src_a   = tv.get_cbuf(insn);
    let operand = tv.get_float_reg39(insn);
    fcmp_impl(tv, insn, src_a, operand);
}

/// FCMP_imm — src_a from immediate, operand from reg39.
pub fn fcmp_imm(tv: &mut TranslatorVisitor, insn: u64) {
    let imm_bits = (field(insn, 20, 19) << 12) | if bit(insn, 56) { 1u32 << 31 } else { 0 };
    let src_a   = Value::ImmU32(imm_bits);
    let operand = tv.get_float_reg39(insn);
    fcmp_impl(tv, insn, src_a, operand);
}

/// FCMP — dispatch wrapper.
pub fn fcmp(tv: &mut TranslatorVisitor, insn: u64) {
    fcmp_reg(tv, insn);
}
