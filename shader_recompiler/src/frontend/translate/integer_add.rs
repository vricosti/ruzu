// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/integer_add.cpp

use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::value::Value;

/// IADD / IADD_reg / IADD_cbuf / IADD_imm — Integer add (two-input).
pub fn iadd(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.x(tv.src_a_reg(insn));
    let src_b = tv.decode_src_b(insn, opcode);

    let neg_a = bit(insn, 49);
    let neg_b = bit(insn, 48);

    let a = if neg_a { tv.ir.ineg_32(src_a) } else { src_a };
    let b = if neg_b { tv.ir.ineg_32(src_b) } else { src_b };

    let result = tv.ir.iadd_32(a, b);

    tv.set_x(dst, result);
}

/// IADD32I — Integer add with 32-bit immediate.
pub fn iadd32i(tv: &mut TranslatorVisitor, insn: u64) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.x(tv.src_a_reg(insn));
    let imm = tv.decode_imm32(insn);
    let neg_a = bit(insn, 54);

    let a = if neg_a { tv.ir.ineg_32(src_a) } else { src_a };
    let result = tv.ir.iadd_32(a, Value::ImmU32(imm));

    tv.set_x(dst, result);
}
