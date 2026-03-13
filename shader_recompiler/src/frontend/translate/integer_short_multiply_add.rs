// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/integer_short_multiply_add.cpp

use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::value::Value;

/// IMAD — Integer multiply-add.
pub fn imad(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.x(tv.src_a_reg(insn));
    let src_b = tv.decode_src_b(insn, opcode);
    let src_c_reg = field(insn, 39, 8);
    let src_c = tv.x(src_c_reg);

    let product = tv.ir.imul_32(src_a, src_b);
    let result = tv.ir.iadd_32(product, src_c);

    tv.set_x(dst, result);
}

/// IMAD32I — Integer multiply-add with 32-bit immediate.
pub fn imad32i(tv: &mut TranslatorVisitor, insn: u64) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.x(tv.src_a_reg(insn));
    let imm = tv.decode_imm32(insn);

    let product = tv.ir.imul_32(src_a, Value::ImmU32(imm));
    let src_c = tv.x(dst); // IMAD32I uses dst as src_c
    let result = tv.ir.iadd_32(product, src_c);

    tv.set_x(dst, result);
}

/// XMAD — Extended multiply-add (16×16+32).
///
/// Implements XMAD_reg, XMAD_rc, XMAD_cr, XMAD_imm.
pub fn xmad(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.x(tv.src_a_reg(insn));
    let src_b = tv.decode_src_b(insn, opcode);
    let src_c_reg = field(insn, 39, 8);
    let src_c = tv.x(src_c_reg);

    // XMAD mode: bits [52:50]
    let mode = field(insn, 50, 3);
    let _is_signed_a = bit(insn, 48);
    let _is_signed_b = bit(insn, 49);
    let is_high_a = bit(insn, 53);
    let is_high_b = bit(insn, 35);

    // Extract 16-bit halves
    let half_a = if is_high_a {
        tv.ir.shift_right_logical_32(src_a, Value::ImmU32(16))
    } else {
        tv.ir.bitwise_and_32(src_a, Value::ImmU32(0xFFFF))
    };

    let half_b = if is_high_b {
        tv.ir.shift_right_logical_32(src_b, Value::ImmU32(16))
    } else {
        tv.ir.bitwise_and_32(src_b, Value::ImmU32(0xFFFF))
    };

    // Multiply 16×16
    let product = tv.ir.imul_32(half_a, half_b);

    // Mode-specific accumulation (SelectMode: Default=0, CLO=1, CHI=2, CSFU=3, CBCC=4)
    let result = match mode {
        0 => {
            // Default: product + src_c
            tv.ir.iadd_32(product, src_c)
        }
        1 => {
            // CLO: product + low half of src_c
            let c_lo = tv.ir.bitwise_and_32(src_c, Value::ImmU32(0xFFFF));
            tv.ir.iadd_32(product, c_lo)
        }
        2 => {
            // CHI: product + high half of src_c
            let c_hi = tv.ir.shift_right_logical_32(src_c, Value::ImmU32(16));
            tv.ir.iadd_32(product, c_hi)
        }
        4 => {
            // CBCC: product + (src_b << 16) + src_c
            let b_shifted = tv.ir.shift_left_logical_32(src_b, Value::ImmU32(16));
            let acc = tv.ir.iadd_32(b_shifted, src_c);
            tv.ir.iadd_32(product, acc)
        }
        _ => {
            // Fallback: product + src_c
            tv.ir.iadd_32(product, src_c)
        }
    };

    tv.set_x(dst, result);
}
