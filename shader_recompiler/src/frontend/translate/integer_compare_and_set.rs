// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/integer_compare_and_set.cpp

use super::common_funcs::{extended_integer_compare, integer_compare, predicate_combine};
use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::value::{Pred, Value};

pub fn iset(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.x(tv.src_a_reg(insn));
    let src_b = tv.decode_src_b(insn, opcode);

    let cmp_op = field(insn, 49, 3);
    let bool_op = field(insn, 45, 2);
    let is_signed = bit(insn, 48);
    let pred_idx = field(insn, 39, 3);
    let neg_pred = bit(insn, 42);
    let x = bit(insn, 43);
    let bf_mode = bit(insn, 44);
    let cc = bit(insn, 47);
    let pred39 = tv.ir.get_pred(Pred(pred_idx as u8), neg_pred);

    let cmp_result = if x {
        extended_integer_compare(tv, src_a, src_b, cmp_op, is_signed)
    } else {
        integer_compare(tv, src_a, src_b, cmp_op, is_signed)
    };
    let result = predicate_combine(tv, cmp_result, pred39, bool_op);

    let true_val = if bf_mode {
        Value::ImmU32(0x3F800000) // 1.0f
    } else {
        Value::ImmU32(0xFFFFFFFF)
    };
    let output = tv.ir.select_u32(result, true_val, Value::ImmU32(0));

    tv.set_x(dst, output);
    if cc {
        if x {
            panic!("ISET.CC + X");
        }
        let zero = tv.ir.imm_u32(0);
        let is_zero = tv.ir.i_equal(output, zero);
        tv.ir.set_z_flag(is_zero);
        if bf_mode {
            tv.ir.set_s_flag(tv.ir.imm_u1(false));
        } else {
            let is_nonzero = tv.ir.logical_not(is_zero);
            tv.ir.set_s_flag(is_nonzero);
        }
        tv.ir.set_c_flag(tv.ir.imm_u1(false));
        tv.ir.set_o_flag(tv.ir.imm_u1(false));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::basic_block::Block;
    use crate::ir::opcodes::Opcode;
    use crate::ir::program::Program;
    use crate::ir::types::ShaderStage;

    fn translated_select_true_value(insn: u64) -> Value {
        let mut program = Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());
        let mut tv = TranslatorVisitor::new(&mut program, 0);

        iset(&mut tv, insn, MaxwellOpcode::ISET_reg);

        let true_value = tv
            .ir
            .program
            .block(0)
            .iter()
            .find_map(|inst| (inst.opcode == Opcode::SelectU32).then(|| inst.args[1]))
            .expect("ISET must emit SelectU32");
        true_value
    }

    #[test]
    fn mk8d_transition_iset_words_use_integer_one_mask() {
        for insn in [0x5B5A_0380_0087_0D05, 0x5B5A_0380_0087_0508] {
            assert_eq!(translated_select_true_value(insn), Value::ImmU32(u32::MAX));
        }
    }

    #[test]
    fn iset_bf_is_bit_44_not_bit_52() {
        let base = 0x5B4A_0380_0087_0D05;
        assert_eq!(translated_select_true_value(base), Value::ImmU32(u32::MAX));
        assert_eq!(
            translated_select_true_value(base | (1 << 44)),
            Value::ImmU32(0x3f80_0000)
        );
    }
}
