// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/integer_add_three_input.cpp

use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::value::Value;

#[derive(Clone, Copy, PartialEq, Eq)]
enum Shift {
    None,
    Right,
    Left,
}

impl Shift {
    fn decode(value: u32) -> Self {
        match value {
            0 => Self::None,
            1 => Self::Right,
            2 => Self::Left,
            _ => panic!("invalid IADD3 shift {value}"),
        }
    }
}

fn integer_half(tv: &mut TranslatorVisitor, value: Value, half: u32) -> Value {
    match half {
        0 => value,
        1 => tv
            .ir
            .bit_field_u_extract(value, Value::ImmU32(0), Value::ImmU32(16)),
        2 => tv
            .ir
            .bit_field_u_extract(value, Value::ImmU32(16), Value::ImmU32(16)),
        _ => panic!("invalid IADD3 half {half}"),
    }
}

fn integer_shift(tv: &mut TranslatorVisitor, value: Value, shift: Shift) -> Value {
    match shift {
        Shift::None => value,
        Shift::Right => {
            let edge_case = tv.ir.get_carry_from_op(value);
            let shifted = tv.ir.shift_right_logical_32(value, Value::ImmU32(16));
            let extended = tv.ir.iadd_32(shifted, Value::ImmU32(0x1_0000));
            tv.ir.select_u32(edge_case, extended, shifted)
        }
        Shift::Left => tv.ir.shift_left_logical_32(value, Value::ImmU32(16)),
    }
}

fn iadd3_impl(
    tv: &mut TranslatorVisitor,
    insn: u64,
    mut op_a: Value,
    mut op_b: Value,
    mut op_c: Value,
    shift: Shift,
) {
    let dest_reg = field(insn, 0, 8);
    let cc = bit(insn, 47);
    let x = bit(insn, 48);
    let neg_c = bit(insn, 49);
    let neg_b = bit(insn, 50);
    let neg_a = bit(insn, 51);

    if neg_a {
        op_a = tv.ir.ineg_32(op_a);
    }
    if neg_b {
        op_b = tv.ir.ineg_32(op_b);
    }
    if neg_c {
        op_c = tv.ir.ineg_32(op_c);
    }
    let mut lhs_1 = tv.ir.iadd_32(op_a, op_b);
    if x {
        if shift == Shift::Right {
            panic!("IADD3 X+RS not implemented upstream");
        }
        let carry_flag = tv.ir.get_c_flag();
        let carry = tv
            .ir
            .select_u32(carry_flag, Value::ImmU32(1), Value::ImmU32(0));
        lhs_1 = tv.ir.iadd_32(lhs_1, carry);
    }
    let lhs_2 = integer_shift(tv, lhs_1, shift);
    let result = tv.ir.iadd_32(lhs_2, op_c);
    tv.set_x(dest_reg, result);

    if cc {
        if x {
            panic!("IADD3 X+CC not implemented upstream");
        }
        let zero = tv.ir.get_zero_from_op(result);
        let sign = tv.ir.get_sign_from_op(result);
        let carry = tv.ir.get_carry_from_op(result);
        let result_overflow = tv.ir.get_overflow_from_op(result);
        let first_add_overflow = tv.ir.u_less_than(lhs_1, op_a);
        let overflow = tv.ir.logical_or(result_overflow, first_add_overflow);
        tv.ir.set_z_flag(zero);
        tv.ir.set_s_flag(sign);
        tv.ir.set_c_flag(carry);
        tv.ir.set_o_flag(overflow);
    }
}

pub fn iadd3(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    match opcode {
        MaxwellOpcode::IADD3_reg => {
            let op_a = tv.get_reg8(insn);
            let op_b = tv.get_reg20(insn);
            let op_c = tv.get_reg39(insn);
            let op_a = integer_half(tv, op_a, field(insn, 35, 2));
            let op_b = integer_half(tv, op_b, field(insn, 33, 2));
            let op_c = integer_half(tv, op_c, field(insn, 31, 2));
            iadd3_impl(
                tv,
                insn,
                op_a,
                op_b,
                op_c,
                Shift::decode(field(insn, 37, 2)),
            );
        }
        MaxwellOpcode::IADD3_cbuf => {
            let op_a = tv.get_reg8(insn);
            let op_b = tv.get_cbuf(insn);
            let op_c = tv.get_reg39(insn);
            iadd3_impl(tv, insn, op_a, op_b, op_c, Shift::None);
        }
        MaxwellOpcode::IADD3_imm => {
            let op_a = tv.get_reg8(insn);
            let op_b = tv.get_imm20(insn);
            let op_c = tv.get_reg39(insn);
            iadd3_impl(tv, insn, op_a, op_b, op_c, Shift::None);
        }
        _ => unreachable!("invalid IADD3 opcode {opcode:?}"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::basic_block::Block;
    use crate::ir::opcodes::Opcode;
    use crate::ir::program::Program;
    use crate::ir::types::ShaderStage;

    #[test]
    fn iadd3_reg_preserves_half_shift_and_condition_codes() {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        let mut visitor = TranslatorVisitor::new(&mut program, 0);
        let insn = 1u64
            | 2u64 << 8
            | 3u64 << 20
            | 4u64 << 39
            | 1u64 << 31
            | 2u64 << 33
            | 1u64 << 35
            | 1u64 << 37
            | 1u64 << 47;

        iadd3(&mut visitor, insn, MaxwellOpcode::IADD3_reg);

        let opcodes: Vec<_> = visitor.ir.program.blocks[0]
            .iter()
            .map(|inst| inst.opcode)
            .collect();
        assert_eq!(
            opcodes
                .iter()
                .filter(|&&op| op == Opcode::BitFieldUExtract)
                .count(),
            3
        );
        assert!(opcodes.contains(&Opcode::ShiftRightLogical32));
        assert!(opcodes.contains(&Opcode::GetCarryFromOp));
        assert!(opcodes.contains(&Opcode::SetZFlag));
        assert!(opcodes.contains(&Opcode::SetOFlag));
    }
}
