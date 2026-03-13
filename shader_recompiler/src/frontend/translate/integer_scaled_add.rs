// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/integer_scaled_add.cpp

use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::value::Value;

/// ISCADD / ISCADD_reg / ISCADD_cbuf / ISCADD_imm / ISCADD32I — Integer scaled add.
pub fn iscadd(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.x(tv.src_a_reg(insn));
    let src_b = tv.decode_src_b(insn, opcode);
    let shift = field(insn, 39, 5);

    let neg_a = bit(insn, 49);
    let neg_b = bit(insn, 48);

    let a = if neg_a { tv.ir.ineg_32(src_a) } else { src_a };
    let b = if neg_b { tv.ir.ineg_32(src_b) } else { src_b };

    let shifted = tv.ir.shift_left_logical_32(a, Value::ImmU32(shift));
    let result = tv.ir.iadd_32(shifted, b);

    tv.set_x(dst, result);
}
