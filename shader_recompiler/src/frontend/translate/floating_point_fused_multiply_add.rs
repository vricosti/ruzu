// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/floating_point_fused_multiply_add.cpp

use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::value::Value;

pub fn ffma(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.f(tv.src_a_reg(insn));
    let src_b = tv.decode_src_b_f32(insn, opcode);

    // src_c is in register field bits [47:39] for reg/rc/cr, or derived for imm
    let src_c_reg = field(insn, 39, 8);
    let src_c = tv.f(src_c_reg);

    let neg_a = bit(insn, 48);
    let neg_c = bit(insn, 49);
    let sat = bit(insn, 50);

    let a = if neg_a {
        tv.ir.fp_neg_32(src_a)
    } else {
        src_a
    };
    let c = if neg_c {
        tv.ir.fp_neg_32(src_c)
    } else {
        src_c
    };

    let mut result = tv.ir.fp_fma_32(a, src_b, c);

    if sat {
        result = tv.ir.fp_saturate_32(result);
    }

    tv.set_f(dst, result);
}

pub fn ffma32i(tv: &mut TranslatorVisitor, insn: u64) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.f(tv.src_a_reg(insn));
    let imm = tv.decode_imm32(insn);
    let src_b = Value::ImmF32(f32::from_bits(imm));

    // For FFMA32I, src_c comes from the destination register
    let src_c = tv.f(dst);

    let result = tv.ir.fp_fma_32(src_a, src_b, src_c);

    tv.set_f(dst, result);
}
