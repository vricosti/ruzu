// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Integer arithmetic translation: IADD, IADD3, IMAD, IMUL, ISCADD, XMAD, IMNMX.

use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::value::{Pred, Value};

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

pub fn iadd32i(tv: &mut TranslatorVisitor, insn: u64) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.x(tv.src_a_reg(insn));
    let imm = tv.decode_imm32(insn);
    let neg_a = bit(insn, 54);

    let a = if neg_a { tv.ir.ineg_32(src_a) } else { src_a };
    let result = tv.ir.iadd_32(a, Value::ImmU32(imm));

    tv.set_x(dst, result);
}

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

pub fn imad32i(tv: &mut TranslatorVisitor, insn: u64) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.x(tv.src_a_reg(insn));
    let imm = tv.decode_imm32(insn);

    let product = tv.ir.imul_32(src_a, Value::ImmU32(imm));
    let src_c = tv.x(dst); // IMAD32I uses dst as src_c
    let result = tv.ir.iadd_32(product, src_c);

    tv.set_x(dst, result);
}

pub fn imul(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.x(tv.src_a_reg(insn));
    let src_b = tv.decode_src_b(insn, opcode);

    let result = tv.ir.imul_32(src_a, src_b);

    tv.set_x(dst, result);
}

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

/// XMAD — Extended Multiply-Add (16×16+32).
pub fn xmad(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.x(tv.src_a_reg(insn));
    let src_b = tv.decode_src_b(insn, opcode);
    let src_c_reg = field(insn, 39, 8);
    let src_c = tv.x(src_c_reg);

    // XMAD mode: bits [52:50]
    let mode = field(insn, 50, 3);
    let is_signed_a = bit(insn, 48);
    let is_signed_b = bit(insn, 49);
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

    // Mode-specific accumulation
    let result = match mode {
        0 => {
            // CLO: product + src_c
            tv.ir.iadd_32(product, src_c)
        }
        1 => {
            // CHI: product + (src_c << 16)
            let c_shifted = tv.ir.shift_left_logical_32(src_c, Value::ImmU32(16));
            tv.ir.iadd_32(product, c_shifted)
        }
        2 => {
            // CBCC: product + src_c (with carry — simplified)
            tv.ir.iadd_32(product, src_c)
        }
        4 => {
            // PSL: (product << 16) + src_c
            let p_shifted = tv.ir.shift_left_logical_32(product, Value::ImmU32(16));
            tv.ir.iadd_32(p_shifted, src_c)
        }
        _ => {
            // Default: product + src_c
            tv.ir.iadd_32(product, src_c)
        }
    };

    tv.set_x(dst, result);
}

pub fn imnmx(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.x(tv.src_a_reg(insn));
    let src_b = tv.decode_src_b(insn, opcode);

    let is_signed = bit(insn, 48);
    let pred_idx = field(insn, 42, 3);

    // Predicate selects min vs max
    let pred = tv.ir.get_pred(Pred(pred_idx as u8), false);

    let min_val = if is_signed {
        tv.ir.s_min_32(src_a, src_b)
    } else {
        tv.ir.u_min_32(src_a, src_b)
    };

    let max_val = if is_signed {
        tv.ir.s_max_32(src_a, src_b)
    } else {
        tv.ir.u_max_32(src_a, src_b)
    };

    let result = tv.ir.select_u32(pred, min_val, max_val);

    tv.set_x(dst, result);
}
