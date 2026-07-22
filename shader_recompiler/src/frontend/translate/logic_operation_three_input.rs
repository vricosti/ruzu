// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/logic_operation_three_input.cpp

use super::{bit, common_funcs::predicate_operation, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::value::{Pred, Value};

fn apply_lut(
    tv: &mut TranslatorVisitor<'_>,
    src_a: Value,
    src_b: Value,
    src_c: Value,
    lut: u8,
) -> Value {
    match lut {
        0x00 => Value::ImmU32(0),                   // FALSE
        0xFF => Value::ImmU32(0xFFFFFFFF),          // TRUE
        0xC0 => tv.ir.bitwise_and_32(src_a, src_b), // A & B
        0xFC => tv.ir.bitwise_or_32(src_a, src_b),  // A | B
        0x3C => tv.ir.bitwise_xor_32(src_a, src_b), // A ^ B
        0xF0 => src_a,                              // A
        0xCC => src_b,                              // B
        0xAA => src_c,                              // C
        0x0F => tv.ir.bitwise_not_32(src_a),        // ~A
        0x33 => tv.ir.bitwise_not_32(src_b),        // ~B
        0x55 => tv.ir.bitwise_not_32(src_c),        // ~C
        0xA0 => tv.ir.bitwise_and_32(src_a, src_c), // A & C
        0x88 => tv.ir.bitwise_and_32(src_b, src_c), // B & C
        0x80 => {
            // A & B & C
            let ab = tv.ir.bitwise_and_32(src_a, src_b);
            tv.ir.bitwise_and_32(ab, src_c)
        }
        0xFE => {
            // A | B | C
            let ab = tv.ir.bitwise_or_32(src_a, src_b);
            tv.ir.bitwise_or_32(ab, src_c)
        }
        0x96 => {
            // A ^ B ^ C
            let ab = tv.ir.bitwise_xor_32(src_a, src_b);
            tv.ir.bitwise_xor_32(ab, src_c)
        }
        0xE8 => {
            // (A & B) | (A & C) | (B & C) — majority
            let ab = tv.ir.bitwise_and_32(src_a, src_b);
            let ac = tv.ir.bitwise_and_32(src_a, src_c);
            let bc = tv.ir.bitwise_and_32(src_b, src_c);
            let ab_or_ac = tv.ir.bitwise_or_32(ab, ac);
            tv.ir.bitwise_or_32(ab_or_ac, bc)
        }
        0xF8 => {
            // Upstream ApplyLUT case 248: a | (b & c).
            let bc = tv.ir.bitwise_and_32(src_b, src_c);
            tv.ir.bitwise_or_32(src_a, bc)
        }
        _ => {
            // General case: evaluate LUT bitwise
            // (A & B & C) selects bit 7, (~A & ~B & ~C) selects bit 0, etc.
            let mut result = Value::ImmU32(0);
            for i in 0..8u32 {
                if lut & (1 << i) != 0 {
                    let bit_a = if i & 4 != 0 {
                        src_a
                    } else {
                        tv.ir.bitwise_not_32(src_a)
                    };
                    let bit_b = if i & 2 != 0 {
                        src_b
                    } else {
                        tv.ir.bitwise_not_32(src_b)
                    };
                    let bit_c = if i & 1 != 0 {
                        src_c
                    } else {
                        tv.ir.bitwise_not_32(src_c)
                    };
                    let term = tv.ir.bitwise_and_32(bit_a, bit_b);
                    let term = tv.ir.bitwise_and_32(term, bit_c);
                    result = tv.ir.bitwise_or_32(result, term);
                }
            }
            result
        }
    }
}

fn lut(insn: u64, opcode: MaxwellOpcode) -> u8 {
    match opcode {
        MaxwellOpcode::LOP3_reg => field(insn, 28, 8) as u8,
        MaxwellOpcode::LOP3_cbuf | MaxwellOpcode::LOP3_imm => field(insn, 48, 8) as u8,
        _ => unreachable!("non-LOP3 opcode {opcode:?}"),
    }
}

pub fn lop3(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    if bit(insn, 47) {
        panic!("LOP3 CC");
    }
    if opcode == MaxwellOpcode::LOP3_reg && bit(insn, 38) {
        panic!("LOP3 X");
    }

    let dst = tv.dst_reg(insn);
    let src_a = tv.x(tv.src_a_reg(insn));
    let src_b = tv.decode_src_b(insn, opcode);
    let src_c = tv.x(field(insn, 39, 8));
    let result = apply_lut(tv, src_a, src_b, src_c, lut(insn, opcode));

    tv.set_x(dst, result);

    if opcode == MaxwellOpcode::LOP3_reg {
        let pred_result = predicate_operation(tv, result, field(insn, 36, 2));
        tv.ir.set_pred(Pred(field(insn, 48, 3) as u8), pred_result);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::opcodes::Opcode;
    use crate::ir::program::Program;
    use crate::ir::types::ShaderStage;

    #[test]
    fn mk8d_packed_output_immediates_use_lut_48() {
        let words = [
            0x3CF8_218F_FFF7_0003,
            0x3CF8_210F_FFF7_0102,
            0x3CF8_208F_FFF7_0401,
            0x3CF8_200F_FFF7_0500,
        ];
        for word in words {
            assert_eq!(lut(word, MaxwellOpcode::LOP3_imm), 0xF8);
            assert_eq!(field(word, 28, 8), 0xFF);
        }
    }

    #[test]
    fn mk8d_packed_output_emits_a_or_b_and_c() {
        let mut program = Program::new(ShaderStage::Fragment);
        let block = program.add_block();
        {
            let mut visitor = TranslatorVisitor::new(&mut program, block);
            lop3(&mut visitor, 0x3CF8_218F_FFF7_0003, MaxwellOpcode::LOP3_imm);
        }

        let instructions: Vec<_> = program.block(block).iter().collect();
        assert!(instructions
            .iter()
            .any(|inst| inst.opcode == Opcode::BitwiseAnd32));
        assert!(instructions
            .iter()
            .any(|inst| inst.opcode == Opcode::BitwiseOr32));
        let set = instructions
            .iter()
            .find(|inst| inst.opcode == Opcode::SetRegister)
            .expect("LOP3 must write its destination register");
        assert_ne!(set.args[1], Value::ImmU32(u32::MAX));
    }
}
