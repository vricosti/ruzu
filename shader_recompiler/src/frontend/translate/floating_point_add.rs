// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/floating_point_add.cpp

use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::value::Value;

pub fn fadd(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.f(tv.src_a_reg(insn));
    let src_b = tv.decode_src_b_f32(insn, opcode);

    let abs_a = bit(insn, 46);
    let neg_a = bit(insn, 48);
    let abs_b = bit(insn, 44);
    let neg_b = bit(insn, 45);
    let sat = bit(insn, 50);

    let a = tv.ir.fp_abs_neg_32(src_a, abs_a, neg_a);
    let b = tv.ir.fp_abs_neg_32(src_b, abs_b, neg_b);
    let mut result = tv.ir.fp_add_32(a, b);

    if sat {
        result = tv.ir.fp_saturate_32(result);
    }

    tv.set_f(dst, result);
}

pub fn fadd32i(tv: &mut TranslatorVisitor, insn: u64) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.f(tv.src_a_reg(insn));
    let imm = tv.decode_imm32(insn);
    let src_b = Value::ImmF32(f32::from_bits(imm));

    let abs_a = bit(insn, 54);
    let neg_a = bit(insn, 56);
    let abs_b = bit(insn, 52);
    let neg_b = bit(insn, 53);

    let a = tv.ir.fp_abs_neg_32(src_a, abs_a, neg_a);
    let b = tv.ir.fp_abs_neg_32(src_b, abs_b, neg_b);
    let result = tv.ir.fp_add_32(a, b);

    tv.set_f(dst, result);
}
