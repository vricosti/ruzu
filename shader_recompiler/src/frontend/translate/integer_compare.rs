// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/integer_compare.cpp

use super::{bit, field, TranslatorVisitor};
use crate::ir::value::Value;

/// Integer comparison helper.
fn int_compare(tv: &mut TranslatorVisitor, cmp: u32, a: Value, b: Value, is_signed: bool) -> Value {
    match cmp {
        1 => if is_signed { tv.ir.s_less_than(a, b) } else { tv.ir.u_less_than(a, b) },
        2 => tv.ir.i_equal(a, b),
        3 => if is_signed { tv.ir.s_less_than_equal(a, b) } else { tv.ir.u_less_than_equal(a, b) },
        4 => if is_signed { tv.ir.s_greater_than(a, b) } else { tv.ir.u_greater_than(a, b) },
        5 => tv.ir.i_not_equal(a, b),
        6 => if is_signed { tv.ir.s_greater_than_equal(a, b) } else { tv.ir.u_greater_than_equal(a, b) },
        _ => tv.ir.imm_u1(false),
    }
}

fn icmp_impl(tv: &mut TranslatorVisitor, insn: u64, src_a: Value, operand: Value) {
    let dst       = field(insn, 0, 8);
    let src_reg   = field(insn, 8, 8);
    let is_signed = bit(insn, 48);
    let cmp_op    = field(insn, 49, 3);

    let zero = Value::ImmU32(0);
    let cmp_result = int_compare(tv, cmp_op, operand, zero, is_signed);
    let src_reg_val = tv.x(src_reg);
    let result = tv.ir.select_u32(cmp_result, src_reg_val, src_a);
    tv.set_x(dst, result);
}

/// ICMP_reg.
pub fn icmp_reg(tv: &mut TranslatorVisitor, insn: u64) {
    let src_a   = tv.get_reg20(insn);
    let operand = tv.get_reg39(insn);
    icmp_impl(tv, insn, src_a, operand);
}

/// ICMP_rc.
pub fn icmp_rc(tv: &mut TranslatorVisitor, insn: u64) {
    let src_a   = tv.get_reg39(insn);
    let operand = tv.get_cbuf(insn);
    icmp_impl(tv, insn, src_a, operand);
}

/// ICMP_cr.
pub fn icmp_cr(tv: &mut TranslatorVisitor, insn: u64) {
    let src_a   = tv.get_cbuf(insn);
    let operand = tv.get_reg39(insn);
    icmp_impl(tv, insn, src_a, operand);
}

/// ICMP_imm.
pub fn icmp_imm(tv: &mut TranslatorVisitor, insn: u64) {
    let src_a   = tv.get_imm20(insn);
    let operand = tv.get_reg39(insn);
    icmp_impl(tv, insn, src_a, operand);
}

/// ICMP — dispatch wrapper.
pub fn icmp(tv: &mut TranslatorVisitor, insn: u64) {
    icmp_reg(tv, insn);
}
