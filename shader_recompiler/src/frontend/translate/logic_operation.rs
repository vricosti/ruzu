// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/logic_operation.cpp

use super::common_funcs::predicate_operation;
use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::value::{Pred, Value};

fn logical_operation(
    tv: &mut TranslatorVisitor,
    operand_1: Value,
    operand_2: Value,
    op: u32,
) -> Value {
    match op {
        0 => tv.ir.bitwise_and_32(operand_1, operand_2),
        1 => tv.ir.bitwise_or_32(operand_1, operand_2),
        2 => tv.ir.bitwise_xor_32(operand_1, operand_2),
        3 => operand_2,
        _ => panic!("invalid logical operation {op}"),
    }
}

#[allow(clippy::too_many_arguments)]
fn lop_impl(
    tv: &mut TranslatorVisitor,
    insn: u64,
    mut op_b: Value,
    x: bool,
    cc: bool,
    inv_a: bool,
    inv_b: bool,
    bit_op: u32,
    pred: Option<(u32, u32)>,
) {
    if x {
        panic!("LOP X not implemented upstream");
    }
    let dest_reg = field(insn, 0, 8);
    let src_reg = field(insn, 8, 8);
    let mut op_a = tv.x(src_reg);
    if inv_a {
        op_a = tv.ir.bitwise_not_32(op_a);
    }
    if inv_b {
        op_b = tv.ir.bitwise_not_32(op_b);
    }
    let result = logical_operation(tv, op_a, op_b, bit_op);
    if let Some((pred_op, dest_pred)) = pred {
        let pred_result = predicate_operation(tv, result, pred_op);
        tv.ir.set_pred(Pred(dest_pred as u8), pred_result);
    }
    if cc {
        let zero = if bit_op == 3 {
            tv.ir.i_equal(result, Value::ImmU32(0))
        } else {
            tv.ir.get_zero_from_op(result)
        };
        let sign = if bit_op == 3 {
            tv.ir.s_less_than(result, Value::ImmU32(0))
        } else {
            tv.ir.get_sign_from_op(result)
        };
        tv.ir.set_z_flag(zero);
        tv.ir.set_s_flag(sign);
        tv.ir.set_c_flag(Value::ImmU1(false));
        tv.ir.set_o_flag(Value::ImmU1(false));
    }
    tv.set_x(dest_reg, result);
}

pub fn lop(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let op_b = match opcode {
        MaxwellOpcode::LOP_reg => tv.get_reg20(insn),
        MaxwellOpcode::LOP_cbuf => tv.get_cbuf(insn),
        MaxwellOpcode::LOP_imm => tv.get_imm20(insn),
        _ => unreachable!("invalid LOP opcode {opcode:?}"),
    };
    lop_impl(
        tv,
        insn,
        op_b,
        bit(insn, 43),
        bit(insn, 47),
        bit(insn, 39),
        bit(insn, 40),
        field(insn, 41, 2),
        Some((field(insn, 44, 2), field(insn, 48, 3))),
    );
}

pub fn lop32i(tv: &mut TranslatorVisitor, insn: u64) {
    lop_impl(
        tv,
        insn,
        Value::ImmU32(field(insn, 20, 32)),
        bit(insn, 57),
        bit(insn, 52),
        bit(insn, 55),
        bit(insn, 56),
        field(insn, 53, 2),
        None,
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::basic_block::Block;
    use crate::ir::opcodes::Opcode;
    use crate::ir::program::Program;
    use crate::ir::types::ShaderStage;

    #[test]
    fn lop_reg_emits_predicate_and_condition_code_outputs() {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        let mut visitor = TranslatorVisitor::new(&mut program, 0);
        let insn =
            1u64 | 2u64 << 8 | 3u64 << 20 | 2u64 << 41 | 3u64 << 44 | 1u64 << 47 | 4u64 << 48;

        lop(&mut visitor, insn, MaxwellOpcode::LOP_reg);

        let opcodes: Vec<_> = visitor.ir.program.blocks[0]
            .iter()
            .map(|inst| inst.opcode)
            .collect();
        assert!(opcodes.contains(&Opcode::SetPred));
        assert!(opcodes.contains(&Opcode::SetZFlag));
        assert!(opcodes.contains(&Opcode::SetSFlag));
        assert!(opcodes.contains(&Opcode::SetCFlag));
        assert!(opcodes.contains(&Opcode::SetOFlag));
    }
}
