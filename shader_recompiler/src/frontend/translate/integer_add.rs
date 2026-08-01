// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/integer_add.cpp

use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::value::Value;

#[allow(clippy::too_many_arguments)]
fn iadd_impl(
    tv: &mut TranslatorVisitor,
    insn: u64,
    op_b: Value,
    neg_a: bool,
    po: bool,
    sat: bool,
    x: bool,
    cc: bool,
) {
    if sat {
        panic!("IADD SAT not implemented upstream");
    }
    if x && po {
        panic!("IADD X+PO not implemented upstream");
    }
    let dest_reg = field(insn, 0, 8);
    let src_a = field(insn, 8, 8);
    let mut op_a = tv.x(src_a);
    if neg_a {
        op_a = tv.ir.ineg_32(op_a);
    }
    let mut result = tv.ir.iadd_32(op_a, op_b);
    if x {
        let carry_flag = tv.ir.get_c_flag();
        let carry = tv
            .ir
            .select_u32(carry_flag, Value::ImmU32(1), Value::ImmU32(0));
        result = tv.ir.iadd_32(result, carry);
    }
    if po {
        result = tv.ir.iadd_32(result, Value::ImmU32(1));
    }
    if cc {
        if po {
            panic!("IADD CC+PO not implemented upstream");
        }
        if x {
            panic!("IADD X+CC not implemented upstream");
        }
        let zero = tv.ir.get_zero_from_op(result);
        let sign = tv.ir.get_sign_from_op(result);
        let carry = tv.ir.get_carry_from_op(result);
        let overflow = tv.ir.get_overflow_from_op(result);
        tv.ir.set_z_flag(zero);
        tv.ir.set_s_flag(sign);
        tv.ir.set_c_flag(carry);
        tv.ir.set_o_flag(overflow);
    }
    tv.set_x(dest_reg, result);
}

pub fn iadd(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let mut op_b = match opcode {
        MaxwellOpcode::IADD_reg => tv.get_reg20(insn),
        MaxwellOpcode::IADD_cbuf => tv.get_cbuf(insn),
        MaxwellOpcode::IADD_imm => tv.get_imm20(insn),
        _ => unreachable!("invalid IADD opcode {opcode:?}"),
    };
    let po = field(insn, 48, 2) == 3;
    if !po && bit(insn, 48) {
        op_b = tv.ir.ineg_32(op_b);
    }
    iadd_impl(
        tv,
        insn,
        op_b,
        bit(insn, 49),
        po,
        bit(insn, 50),
        bit(insn, 43),
        bit(insn, 47),
    );
}

pub fn iadd32i(tv: &mut TranslatorVisitor, insn: u64) {
    let po = field(insn, 55, 2) == 3;
    let neg_a = !po && bit(insn, 56);
    iadd_impl(
        tv,
        insn,
        Value::ImmU32(field(insn, 20, 32)),
        neg_a,
        po,
        bit(insn, 54),
        bit(insn, 53),
        bit(insn, 52),
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
    fn iadd_po_negates_only_a_and_adds_one() {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        let mut visitor = TranslatorVisitor::new(&mut program, 0);
        let insn = 1u64 | 2u64 << 8 | 3u64 << 20 | 3u64 << 48;

        iadd(&mut visitor, insn, MaxwellOpcode::IADD_reg);

        let opcodes: Vec<_> = visitor.ir.program.blocks[0]
            .iter()
            .map(|inst| inst.opcode)
            .collect();
        assert_eq!(
            opcodes.iter().filter(|&&op| op == Opcode::IAdd32).count(),
            2
        );
        assert_eq!(
            opcodes.iter().filter(|&&op| op == Opcode::INeg32).count(),
            1
        );
    }
}
