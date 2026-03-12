// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Bitwise operation translation: LOP, LOP3, SHL, SHR, SHF, BFE, BFI, POPC, FLO, PRMT.

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
        3 => a, // PASS_B  — pass src_a through (with possible inversion)
        _ => a,
    };

    tv.set_x(dst, result);
}

pub fn lop3(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.x(tv.src_a_reg(insn));
    let src_b = tv.decode_src_b(insn, opcode);
    let src_c_reg = field(insn, 39, 8);
    let src_c = tv.x(src_c_reg);

    // LOP3 truth table in bits [47:40]
    let lut = field(insn, 28, 8) as u8;

    // Evaluate the 3-input LUT per bit
    // For simplicity, decompose common LUT values:
    let result = match lut {
        0x00 => Value::ImmU32(0),                              // FALSE
        0xFF => Value::ImmU32(0xFFFFFFFF),                     // TRUE
        0xC0 => tv.ir.bitwise_and_32(src_a, src_b),           // A & B
        0xFC => tv.ir.bitwise_or_32(src_a, src_b),            // A | B
        0x3C => tv.ir.bitwise_xor_32(src_a, src_b),           // A ^ B
        0xF0 => src_a,                                          // A
        0xCC => src_b,                                          // B
        0xAA => src_c,                                          // C
        0x0F => tv.ir.bitwise_not_32(src_a),                   // ~A
        0x33 => tv.ir.bitwise_not_32(src_b),                   // ~B
        0x55 => tv.ir.bitwise_not_32(src_c),                   // ~C
        0xA0 => tv.ir.bitwise_and_32(src_a, src_c),           // A & C
        0x88 => tv.ir.bitwise_and_32(src_b, src_c),           // B & C
        0x80 => {                                               // A & B & C
            let ab = tv.ir.bitwise_and_32(src_a, src_b);
            tv.ir.bitwise_and_32(ab, src_c)
        }
        0xFE => {                                               // A | B | C
            let ab = tv.ir.bitwise_or_32(src_a, src_b);
            tv.ir.bitwise_or_32(ab, src_c)
        }
        0x96 => {                                               // A ^ B ^ C
            let ab = tv.ir.bitwise_xor_32(src_a, src_b);
            tv.ir.bitwise_xor_32(ab, src_c)
        }
        0xE8 => {                                               // (A & B) | (A & C) | (B & C) — majority
            let ab = tv.ir.bitwise_and_32(src_a, src_b);
            let ac = tv.ir.bitwise_and_32(src_a, src_c);
            let bc = tv.ir.bitwise_and_32(src_b, src_c);
            let ab_or_ac = tv.ir.bitwise_or_32(ab, ac);
            tv.ir.bitwise_or_32(ab_or_ac, bc)
        }
        _ => {
            // General case: evaluate LUT bitwise
            // (A & B & C) selects bit 7, (~A & ~B & ~C) selects bit 0, etc.
            // For now, implement as a series of selects
            // This is correct but could be optimized
            let mut result = Value::ImmU32(0);
            for i in 0..8u32 {
                if lut & (1 << i) != 0 {
                    let bit_a = if i & 4 != 0 { src_a } else { tv.ir.bitwise_not_32(src_a) };
                    let bit_b = if i & 2 != 0 { src_b } else { tv.ir.bitwise_not_32(src_b) };
                    let bit_c = if i & 1 != 0 { src_c } else { tv.ir.bitwise_not_32(src_c) };
                    let term = tv.ir.bitwise_and_32(bit_a, bit_b);
                    let term = tv.ir.bitwise_and_32(term, bit_c);
                    result = tv.ir.bitwise_or_32(result, term);
                }
            }
            result
        }
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
        3 => a,
        _ => a,
    };

    tv.set_x(dst, result);
}

pub fn shl(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.x(tv.src_a_reg(insn));
    let src_b = tv.decode_src_b(insn, opcode);

    let result = tv.ir.shift_left_logical_32(src_a, src_b);

    tv.set_x(dst, result);
}

pub fn shr(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.x(tv.src_a_reg(insn));
    let src_b = tv.decode_src_b(insn, opcode);
    let is_signed = bit(insn, 48);

    let result = if is_signed {
        tv.ir.shift_right_arithmetic_32(src_a, src_b)
    } else {
        tv.ir.shift_right_logical_32(src_a, src_b)
    };

    tv.set_x(dst, result);
}

pub fn bfe(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.x(tv.src_a_reg(insn));
    let src_b = tv.decode_src_b(insn, opcode);
    let is_signed = bit(insn, 48);

    // src_b packs offset in low 8 bits, count in bits [15:8]
    let offset = tv.ir.bitwise_and_32(src_b, Value::ImmU32(0xFF));
    let shifted = tv.ir.shift_right_logical_32(src_b, Value::ImmU32(8));
    let count = tv.ir.bitwise_and_32(shifted, Value::ImmU32(0xFF));

    let result = if is_signed {
        tv.ir.bit_field_s_extract(src_a, offset, count)
    } else {
        tv.ir.bit_field_u_extract(src_a, offset, count)
    };

    tv.set_x(dst, result);
}

pub fn bfi(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.x(tv.src_a_reg(insn));
    let src_b = tv.decode_src_b(insn, opcode);
    let src_c_reg = field(insn, 39, 8);
    let src_c = tv.x(src_c_reg);

    // src_a packs offset and count
    let offset = tv.ir.bitwise_and_32(src_a, Value::ImmU32(0xFF));
    let shifted = tv.ir.shift_right_logical_32(src_a, Value::ImmU32(8));
    let count = tv.ir.bitwise_and_32(shifted, Value::ImmU32(0xFF));

    let result = tv.ir.bit_field_insert(src_c, src_b, offset, count);

    tv.set_x(dst, result);
}

pub fn popc(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src = tv.decode_src_b(insn, opcode);
    let inv = bit(insn, 40);

    let a = if inv { tv.ir.bitwise_not_32(src) } else { src };
    let result = tv.ir.bit_count_32(a);

    tv.set_x(dst, result);
}

pub fn flo(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src = tv.decode_src_b(insn, opcode);
    let is_signed = bit(insn, 48);
    let inv = bit(insn, 40);

    let a = if inv { tv.ir.bitwise_not_32(src) } else { src };
    let result = if is_signed {
        tv.ir.find_s_msb_32(a)
    } else {
        tv.ir.find_u_msb_32(a)
    };

    tv.set_x(dst, result);
}

pub fn prmt(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.x(tv.src_a_reg(insn));
    let src_b = tv.decode_src_b(insn, opcode);
    let src_c_reg = field(insn, 39, 8);
    let src_c = tv.x(src_c_reg);

    // PRMT permutes bytes from src_a and src_b using src_c as selector.
    // Each 4-bit nibble in src_c selects a byte:
    //   0-3: bytes of src_a
    //   4-7: bytes of src_b
    // Simplified: for common PRMT modes, we do byte-level manipulation.
    // Full implementation would need per-nibble extraction.

    // For now, implement basic byte permutation
    let mut result = Value::ImmU32(0);
    // Simplified: just pass through src_a for now.
    // Full PRMT byte permutation would need per-nibble extraction from src_c
    // to select bytes from src_a (indices 0-3) or src_b (indices 4-7).
    // TODO: Full PRMT byte permutation
    let _ = (src_b, src_c, result);
    tv.set_x(dst, src_a);
}
