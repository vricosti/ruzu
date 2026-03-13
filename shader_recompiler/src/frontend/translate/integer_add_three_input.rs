// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/integer_add_three_input.cpp

use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;

/// IADD3 / IADD3_reg / IADD3_cbuf / IADD3_imm — Integer three-input add.
pub fn iadd3(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.x(tv.src_a_reg(insn));
    let src_b = tv.decode_src_b(insn, opcode);
    let src_c_reg = field(insn, 39, 8);
    let src_c = tv.x(src_c_reg);

    let neg_a = bit(insn, 51);
    let neg_b = bit(insn, 50);
    let neg_c = bit(insn, 49);

    let a = if neg_a { tv.ir.ineg_32(src_a) } else { src_a };
    let b = if neg_b { tv.ir.ineg_32(src_b) } else { src_b };
    let c = if neg_c { tv.ir.ineg_32(src_c) } else { src_c };

    let ab = tv.ir.iadd_32(a, b);
    let result = tv.ir.iadd_32(ab, c);

    tv.set_x(dst, result);
}
