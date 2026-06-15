// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/floating_point_set_predicate.cpp

use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::value::{Pred, Value};

/// FP comparison predicate (from bits).
fn fp_compare(tv: &mut TranslatorVisitor, cmp: u32, a: Value, b: Value) -> Value {
    match cmp {
        0 => tv.ir.imm_u1(false),                      // F (false)
        1 => tv.ir.fp_ord_less_than_32(a, b),          // LT
        2 => tv.ir.fp_ord_equal_32(a, b),              // EQ
        3 => tv.ir.fp_ord_less_than_equal_32(a, b),    // LE
        4 => tv.ir.fp_ord_greater_than_32(a, b),       // GT
        5 => tv.ir.fp_ord_not_equal_32(a, b),          // NE
        6 => tv.ir.fp_ord_greater_than_equal_32(a, b), // GE
        7 => {
            // NUM (ordered)
            let nan_a = tv.ir.fp_is_nan_32(a);
            let nan_b = tv.ir.fp_is_nan_32(b);
            let either_nan = tv.ir.logical_or(nan_a, nan_b);
            tv.ir.logical_not(either_nan)
        }
        8 => {
            // NAN (unordered)
            let nan_a = tv.ir.fp_is_nan_32(a);
            let nan_b = tv.ir.fp_is_nan_32(b);
            tv.ir.logical_or(nan_a, nan_b)
        }
        9 => tv.ir.fp_unord_less_than_32(a, b),     // LTU
        10 => tv.ir.fp_unord_equal_32(a, b),        // EQU
        11 => tv.ir.fp_unord_less_than_32(a, b),    // LEU (approximation)
        12 => tv.ir.fp_unord_greater_than_32(a, b), // GTU
        13 => tv.ir.fp_unord_not_equal_32(a, b),    // NEU
        14 => tv.ir.fp_unord_greater_than_32(a, b), // GEU (approximation)
        15 => tv.ir.imm_u1(true),                   // T (true)
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

pub fn fsetp(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let src_a = tv.f(tv.src_a_reg(insn));
    let src_b = tv.decode_src_b_f32(insn, opcode);

    let dest_pred_b = Pred(field(insn, 0, 3) as u8);
    let dest_pred_a = Pred(field(insn, 3, 3) as u8);
    let abs_a = bit(insn, 7);
    let abs_b = bit(insn, 44);
    let neg_a = bit(insn, 43);
    let neg_b = bit(insn, 6);

    let a = tv.ir.fp_abs_neg_32(src_a, abs_a, neg_a);
    let b = tv.ir.fp_abs_neg_32(src_b, abs_b, neg_b);

    let cmp_op = field(insn, 48, 4);
    let bool_op = field(insn, 45, 2);
    let pred_idx = Pred(field(insn, 39, 3) as u8);
    let neg_bop_pred = bit(insn, 42);
    let pred39 = tv.ir.get_pred(pred_idx, neg_bop_pred);

    let cmp_result = fp_compare(tv, cmp_op, a, b);
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
    fn fsetp_writes_upstream_predicate_destinations_and_negates_bop_pred() {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        let mut tv = TranslatorVisitor::new(&mut program, 0);
        let insn = (1u64) // dest_pred_b
            | (6u64 << 3) // dest_pred_a
            | (1u64 << 8) // src_a_reg
            | (3u64 << 39) // bop_pred
            | (1u64 << 42) // neg_bop_pred
            | (0u64 << 45) // AND
            | (2u64 << 48); // Equal

        fsetp(&mut tv, insn, MaxwellOpcode::FSETP_reg);

        let block = &tv.ir.program.blocks[0];
        let set_preds: Vec<_> = block
            .iter()
            .filter(|inst| inst.opcode == Opcode::SetPred)
            .collect();
        assert_eq!(set_preds.len(), 2);
        assert_eq!(set_preds[0].args[0], Value::Pred(Pred(6)));
        assert_eq!(set_preds[1].args[0], Value::Pred(Pred(1)));
        assert!(block.iter().any(|inst| inst.opcode == Opcode::LogicalNot));
    }
}
