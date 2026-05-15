// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/floating_point_fused_multiply_add.cpp

use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::value::Value;

pub fn ffma(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let (src_b, src_c) = match opcode {
        MaxwellOpcode::FFMA_reg => (tv.get_float_reg20(insn), tv.get_float_reg39(insn)),
        MaxwellOpcode::FFMA_rc => (tv.get_float_reg39(insn), tv.get_float_cbuf(insn)),
        MaxwellOpcode::FFMA_cr => (tv.get_float_cbuf(insn), tv.get_float_reg39(insn)),
        MaxwellOpcode::FFMA_imm => (tv.get_float_imm20(insn), tv.get_float_reg39(insn)),
        _ => unreachable!("invalid opcode for FFMA: {:?}", opcode),
    };

    ffma_inner(
        tv,
        insn,
        src_b,
        src_c,
        false,
        bit(insn, 48),
        bit(insn, 49),
        bit(insn, 50),
    );
}

fn ffma_inner(
    tv: &mut TranslatorVisitor,
    insn: u64,
    src_b: Value,
    src_c: Value,
    neg_a: bool,
    neg_b: bool,
    neg_c: bool,
    sat: bool,
) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.f(tv.src_a_reg(insn));

    let a = if neg_a { tv.ir.fp_neg_32(src_a) } else { src_a };
    let b = if neg_b { tv.ir.fp_neg_32(src_b) } else { src_b };
    let c = if neg_c { tv.ir.fp_neg_32(src_c) } else { src_c };

    let mut result = tv.ir.fp_fma_32(a, b, c);

    if sat {
        result = tv.ir.fp_saturate_32(result);
    }

    tv.set_f(dst, result);
}

pub fn ffma32i(tv: &mut TranslatorVisitor, insn: u64) {
    let imm = tv.decode_imm32(insn);
    let src_b = Value::ImmF32(f32::from_bits(imm));
    let src_c = tv.f(field(insn, 0, 8));

    ffma_inner(
        tv,
        insn,
        src_b,
        src_c,
        bit(insn, 56),
        false,
        bit(insn, 57),
        bit(insn, 55),
    );
}
