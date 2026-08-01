// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/bitfield_extract.cpp

use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::value::Value;

fn bfe_impl(tv: &mut TranslatorVisitor, insn: u64, src: Value) {
    let dest_reg = field(insn, 0, 8);
    let offset_reg = field(insn, 8, 8);
    let brev = bit(insn, 40);
    let cc = bit(insn, 47);
    let is_signed = bit(insn, 48);

    let zero = Value::ImmU32(0);
    let one = Value::ImmU32(1);
    let max_size = Value::ImmU32(32);
    let offset = tv.ir.bit_field_u_extract(src, zero, Value::ImmU32(8));
    let count = tv
        .ir
        .bit_field_u_extract(src, Value::ImmU32(8), Value::ImmU32(8));

    let zero_count = tv.ir.i_equal(count, zero);
    let offset_plus_count = tv.ir.iadd_32(offset, count);
    let exceed_count = tv.ir.u_greater_than_equal(offset_plus_count, max_size);
    let replicate = tv.ir.u_greater_than_equal(offset, max_size);

    let mut base = tv.x(offset_reg);
    if brev {
        base = tv.ir.bit_reverse_32(base);
    }
    let mut result = if is_signed {
        tv.ir.bit_field_s_extract(base, offset, count)
    } else {
        tv.ir.bit_field_u_extract(base, offset, count)
    };
    if is_signed {
        let is_negative = tv.ir.s_less_than(base, zero);
        let replicated_bit = tv.ir.select_u32(is_negative, Value::ImmU32(u32::MAX), zero);
        let exceed_bit = tv.ir.bit_field_u_extract(base, Value::ImmU32(31), one);
        result = tv.ir.select_u32(replicate, replicated_bit, result);
        let exceed_result = tv
            .ir
            .bit_field_insert(result, exceed_bit, Value::ImmU32(31), one);
        result = tv.ir.select_u32(exceed_count, exceed_result, result);
    }
    result = tv.ir.select_u32(zero_count, zero, result);

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

pub fn bfe(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let src = match opcode {
        MaxwellOpcode::BFE_reg => tv.get_reg20(insn),
        MaxwellOpcode::BFE_cbuf => tv.get_cbuf(insn),
        MaxwellOpcode::BFE_imm => tv.get_imm20(insn),
        _ => unreachable!("invalid BFE opcode {opcode:?}"),
    };
    bfe_impl(tv, insn, src);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::basic_block::Block;
    use crate::ir::opcodes::Opcode;
    use crate::ir::program::Program;
    use crate::ir::types::ShaderStage;

    #[test]
    fn bfe_emits_brev_edge_cases_and_condition_codes() {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        let mut visitor = TranslatorVisitor::new(&mut program, 0);
        let insn = 1u64 | 2u64 << 8 | 3u64 << 20 | 1u64 << 40 | 1u64 << 47 | 1u64 << 48;

        bfe(&mut visitor, insn, MaxwellOpcode::BFE_reg);

        let opcodes: Vec<_> = visitor.ir.program.blocks[0]
            .iter()
            .map(|inst| inst.opcode)
            .collect();
        assert!(opcodes.contains(&Opcode::BitReverse32));
        assert!(
            opcodes
                .iter()
                .filter(|&&op| op == Opcode::SelectU32)
                .count()
                >= 3
        );
        assert!(opcodes.contains(&Opcode::SetZFlag));
        assert!(opcodes.contains(&Opcode::SetSFlag));
        assert!(opcodes.contains(&Opcode::SetCFlag));
        assert!(opcodes.contains(&Opcode::SetOFlag));
    }
}
