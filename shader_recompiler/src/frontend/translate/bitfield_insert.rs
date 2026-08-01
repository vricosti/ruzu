// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/bitfield_insert.cpp

use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::value::Value;

fn bfi_impl(tv: &mut TranslatorVisitor, insn: u64, src_a: Value, base: Value) {
    let dest_reg = field(insn, 0, 8);
    let insert_reg = field(insn, 8, 8);
    let cc = bit(insn, 47);

    let zero = Value::ImmU32(0);
    let max_size = Value::ImmU32(32);
    let offset = tv.ir.bit_field_u_extract(src_a, zero, Value::ImmU32(8));
    let unsafe_count = tv
        .ir
        .bit_field_u_extract(src_a, Value::ImmU32(8), Value::ImmU32(8));
    let exceed_offset = tv.ir.u_greater_than_equal(offset, max_size);
    let exceed_count = tv.ir.u_greater_than(unsafe_count, max_size);
    let remaining_size = tv.ir.isub_32(max_size, offset);
    let safe_count = tv.ir.select_u32(exceed_count, remaining_size, unsafe_count);

    let insert = tv.x(insert_reg);
    let inserted = tv.ir.bit_field_insert(base, insert, offset, safe_count);
    let result = tv.ir.select_u32(exceed_offset, base, inserted);
    tv.set_x(dest_reg, result);

    if cc {
        let is_zero = tv.ir.i_equal(result, zero);
        let is_negative = tv.ir.s_less_than(result, zero);
        tv.ir.set_z_flag(is_zero);
        tv.ir.set_s_flag(is_negative);
        tv.ir.set_c_flag(Value::ImmU1(false));
        tv.ir.set_o_flag(Value::ImmU1(false));
    }
}

pub fn bfi(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let (src_a, base) = match opcode {
        MaxwellOpcode::BFI_reg => (tv.get_reg20(insn), tv.get_reg39(insn)),
        MaxwellOpcode::BFI_rc => (tv.get_reg39(insn), tv.get_cbuf(insn)),
        MaxwellOpcode::BFI_cr => (tv.get_cbuf(insn), tv.get_reg39(insn)),
        MaxwellOpcode::BFI_imm => (tv.get_imm20(insn), tv.get_reg39(insn)),
        _ => unreachable!("invalid BFI opcode {opcode:?}"),
    };
    bfi_impl(tv, insn, src_a, base);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::basic_block::Block;
    use crate::ir::opcodes::Opcode;
    use crate::ir::program::Program;
    use crate::ir::types::ShaderStage;

    #[test]
    fn bfi_clamps_unsafe_count_and_updates_condition_codes() {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        let mut visitor = TranslatorVisitor::new(&mut program, 0);
        let insn = 1u64 | 2u64 << 8 | 3u64 << 20 | 4u64 << 39 | 1u64 << 47;

        bfi(&mut visitor, insn, MaxwellOpcode::BFI_reg);

        let opcodes: Vec<_> = visitor.ir.program.blocks[0]
            .iter()
            .map(|inst| inst.opcode)
            .collect();
        assert!(opcodes.contains(&Opcode::UGreaterThan));
        assert!(
            opcodes
                .iter()
                .filter(|&&op| op == Opcode::SelectU32)
                .count()
                >= 2
        );
        assert!(opcodes.contains(&Opcode::SetZFlag));
        assert!(opcodes.contains(&Opcode::SetSFlag));
    }
}
