// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/logic_operation.cpp

use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::value::Value;

pub fn lop(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.x(tv.src_a_reg(insn));
    let src_b = tv.decode_src_b(insn, opcode);

    let inv_a = bit(insn, 39);
    let inv_b = bit(insn, 40);
    let lop_op = field(insn, 41, 2);

    let a = if inv_a { tv.ir.bitwise_not_32(src_a) } else { src_a };
    let b = if inv_b { tv.ir.bitwise_not_32(src_b) } else { src_b };

    let result = match lop_op {
        0 => tv.ir.bitwise_and_32(a, b),
        1 => tv.ir.bitwise_or_32(a, b),
        2 => tv.ir.bitwise_xor_32(a, b),
        3 => b, // PASS_B — pass src_b through (with possible inversion)
        _ => b,
    };

    tv.set_x(dst, result);
}

pub fn lop32i(tv: &mut TranslatorVisitor, insn: u64) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.x(tv.src_a_reg(insn));
    let imm = tv.decode_imm32(insn);

    let lop_op = field(insn, 53, 2);
    let inv_a = bit(insn, 55);

    let a = if inv_a { tv.ir.bitwise_not_32(src_a) } else { src_a };
    let b = Value::ImmU32(imm);

    let result = match lop_op {
        0 => tv.ir.bitwise_and_32(a, b),
        1 => tv.ir.bitwise_or_32(a, b),
        2 => tv.ir.bitwise_xor_32(a, b),
        3 => b,
        _ => b,
    };

    tv.set_x(dst, result);
}
