// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/logic_operation_three_input.cpp

use super::{field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::value::Value;

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
