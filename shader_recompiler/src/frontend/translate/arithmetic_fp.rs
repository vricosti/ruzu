// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! FP32 arithmetic translation: FADD, FMUL, FFMA, FMNMX, MUFU.

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

pub fn fmul(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.f(tv.src_a_reg(insn));
    let src_b = tv.decode_src_b_f32(insn, opcode);

    let neg_a = bit(insn, 48);
    let sat = bit(insn, 50);

    let a = if neg_a {
        tv.ir.fp_neg_32(src_a)
    } else {
        src_a
    };

    let mut result = tv.ir.fp_mul_32(a, src_b);

    if sat {
        result = tv.ir.fp_saturate_32(result);
    }

    tv.set_f(dst, result);
}

pub fn fmul32i(tv: &mut TranslatorVisitor, insn: u64) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.f(tv.src_a_reg(insn));
    let imm = tv.decode_imm32(insn);
    let src_b = Value::ImmF32(f32::from_bits(imm));

    let result = tv.ir.fp_mul_32(src_a, src_b);

    tv.set_f(dst, result);
}

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

pub fn fmnmx(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.f(tv.src_a_reg(insn));
    let src_b = tv.decode_src_b_f32(insn, opcode);

    let abs_a = bit(insn, 46);
    let neg_a = bit(insn, 48);
    let abs_b = bit(insn, 44);
    let neg_b = bit(insn, 45);

    let a = tv.ir.fp_abs_neg_32(src_a, abs_a, neg_a);
    let b = tv.ir.fp_abs_neg_32(src_b, abs_b, neg_b);

    // Predicate bit 42 selects min vs max
    let pred = field(insn, 42, 1);
    let result = if pred == 0 {
        tv.ir.fp_min_32(a, b)
    } else {
        tv.ir.fp_max_32(a, b)
    };

    tv.set_f(dst, result);
}

/// MUFU — Multi-Function Unit (transcendentals).
pub fn mufu(tv: &mut TranslatorVisitor, insn: u64) {
    let dst = tv.dst_reg(insn);
    let src = tv.f(tv.src_a_reg(insn));
    let abs_src = bit(insn, 46);
    let neg_src = bit(insn, 48);
    let sat = bit(insn, 50);
    let op = field(insn, 20, 4);

    let a = tv.ir.fp_abs_neg_32(src, abs_src, neg_src);

    let mut result = match op {
        0 => tv.ir.fp_cos(a),           // COS
        1 => tv.ir.fp_sin(a),           // SIN
        2 => tv.ir.fp_exp2(a),          // EX2
        3 => tv.ir.fp_log2(a),          // LG2
        4 => tv.ir.fp_recip_32(a),      // RCP
        5 => tv.ir.fp_recip_sqrt_32(a), // RSQ
        6 => {
            let sqrt = tv.ir.fp_sqrt_32(a);
            tv.ir.fp_recip_32(sqrt)
        } // RCP64H (approximate)
        7 => tv.ir.fp_sqrt_32(a),       // SQRT
        _ => {
            log::warn!("Unknown MUFU op: {}", op);
            a
        }
    };

    if sat {
        result = tv.ir.fp_saturate_32(result);
    }

    tv.set_f(dst, result);
}
