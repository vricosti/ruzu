// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/integer_set_predicate.cpp

use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::value::{Pred, Value};

/// Integer comparison.
fn int_compare(tv: &mut TranslatorVisitor, cmp: u32, a: Value, b: Value, is_signed: bool) -> Value {
    match cmp {
        1 => {
            if is_signed {
                tv.ir.s_less_than(a, b)
            } else {
                tv.ir.u_less_than(a, b)
            }
        }
        2 => tv.ir.i_equal(a, b),
        3 => {
            if is_signed {
                tv.ir.s_less_than_equal(a, b)
            } else {
                tv.ir.u_less_than_equal(a, b)
            }
        }
        4 => {
            if is_signed {
                tv.ir.s_greater_than(a, b)
            } else {
                tv.ir.u_greater_than(a, b)
            }
        }
        5 => tv.ir.i_not_equal(a, b),
        6 => {
            if is_signed {
                tv.ir.s_greater_than_equal(a, b)
            } else {
                tv.ir.u_greater_than_equal(a, b)
            }
        }
        _ => tv.ir.imm_u1(false),
    }
}

/// Combine comparison result with predicate via boolean op.
fn combine_pred(tv: &mut TranslatorVisitor, result: Value, pred: Value, bool_op: u32) -> Value {
    match bool_op {
        0 => tv.ir.logical_and(result, pred), // AND
        1 => tv.ir.logical_or(result, pred),  // OR
        2 => tv.ir.logical_xor(result, pred), // XOR
        _ => result,
    }
}

pub fn isetp(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let src_a = tv.x(tv.src_a_reg(insn));
    let src_b = tv.decode_src_b(insn, opcode);

    let dest_pred_b = Pred(field(insn, 0, 3) as u8);
    let dest_pred_a = Pred(field(insn, 3, 3) as u8);
    let bop_pred = Pred(field(insn, 39, 3) as u8);
    let neg_bop_pred = bit(insn, 42);
    let x = bit(insn, 43);
    let cmp_op = field(insn, 49, 3);
    let bool_op = field(insn, 45, 2);
    let is_signed = bit(insn, 48);
    if x {
        panic!("ISETP.X extended integer compare not implemented");
    }
    let pred39 = tv.ir.get_pred(bop_pred, neg_bop_pred);

    let cmp_result = int_compare(tv, cmp_op, src_a, src_b, is_signed);
    let result_a = combine_pred(tv, cmp_result, pred39, bool_op);
    let not_cmp = tv.ir.logical_not(cmp_result);
    let result_b = combine_pred(tv, not_cmp, pred39, bool_op);

    tv.ir.set_pred(dest_pred_a, result_a);
    tv.ir.set_pred(dest_pred_b, result_b);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::basic_block::Block;
    use crate::ir::opcodes::Opcode;
    use crate::ir::program::Program;
    use crate::ir::types::ShaderStage;

    #[test]
    fn isetp_writes_upstream_predicate_destinations_and_negates_bop_pred() {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        let mut tv = TranslatorVisitor::new(&mut program, 0);
        let insn = (2u64) // dest_pred_b
            | (5u64 << 3) // dest_pred_a
            | (1u64 << 8) // src_reg_a
            | (4u64 << 39) // bop_pred
            | (1u64 << 42) // neg_bop_pred
            | (0u64 << 45) // AND
            | (2u64 << 49); // Equal

        isetp(&mut tv, insn, MaxwellOpcode::ISETP_reg);

        let block = &tv.ir.program.blocks[0];
        let set_preds: Vec<_> = block
            .iter()
            .filter(|inst| inst.opcode == Opcode::SetPred)
            .collect();
        assert_eq!(set_preds.len(), 2);
        assert_eq!(set_preds[0].args[0], Value::Pred(Pred(5)));
        assert_eq!(set_preds[1].args[0], Value::Pred(Pred(2)));
        assert!(block.iter().any(|inst| inst.opcode == Opcode::LogicalNot));
    }
}
