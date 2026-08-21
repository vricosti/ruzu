// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/integer_minimum_maximum.cpp

use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::value::Pred;

/// IMNMX / IMNMX_reg / IMNMX_cbuf / IMNMX_imm — Integer minimum/maximum.
pub fn imnmx(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_b = tv.decode_src_b(insn, opcode);

    let pred = Pred(field(insn, 39, 3) as u8);
    let neg_pred = bit(insn, 42);
    let mode = field(insn, 43, 2);
    let cc = bit(insn, 47);
    let is_signed = bit(insn, 48);

    if cc {
        panic!("IMNMX CC");
    }
    if mode != 0 {
        panic!("IMNMX.MODE");
    }

    let pred = tv.ir.get_pred(pred, false);
    let src_a = tv.x(tv.src_a_reg(insn));
    let min_val = if is_signed {
        tv.ir.s_min_32(src_a, src_b)
    } else {
        tv.ir.u_min_32(src_a, src_b)
    };

    let max_val = if is_signed {
        tv.ir.s_max_32(src_a, src_b)
    } else {
        tv.ir.u_max_32(src_a, src_b)
    };

    let (min_val, max_val) = if neg_pred {
        (max_val, min_val)
    } else {
        (min_val, max_val)
    };
    let result = tv.ir.select_u32(pred, min_val, max_val);

    tv.set_x(dst, result);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::basic_block::Block;
    use crate::ir::opcodes::Opcode;
    use crate::ir::program::Program;
    use crate::ir::types::ShaderStage;
    use crate::ir::value::Value;

    fn translate(insn: u64) -> Program {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        let mut tv = TranslatorVisitor::new(&mut program, 0);
        imnmx(&mut tv, insn, MaxwellOpcode::IMNMX_reg);
        program
    }

    #[test]
    fn imnmx_uses_bits_39_to_41_for_predicate() {
        let program = translate((Pred::PT.0 as u64) << 39);
        let select = program.blocks[0]
            .iter()
            .find(|inst| inst.opcode == Opcode::SelectU32)
            .expect("IMNMX must select between min and max");

        assert_eq!(select.args[0], Value::ImmU1(true));
        assert!(!program.blocks[0]
            .iter()
            .any(|inst| inst.opcode == Opcode::GetPred));
    }

    #[test]
    fn imnmx_bit_42_negates_the_min_max_selection() {
        let program = translate(((Pred::PT.0 as u64) << 39) | (1u64 << 42));
        let block = &program.blocks[0];
        let min_ref = block
            .iter()
            .position(|inst| inst.opcode == Opcode::UMin32)
            .expect("IMNMX must emit the minimum");
        let max_ref = block
            .iter()
            .position(|inst| inst.opcode == Opcode::UMax32)
            .expect("IMNMX must emit the maximum");
        let select = block
            .iter()
            .find(|inst| inst.opcode == Opcode::SelectU32)
            .expect("IMNMX must select between min and max");

        assert_eq!(
            select.args[1],
            Value::Inst(crate::ir::value::InstRef {
                block: 0,
                inst: max_ref as u32,
            })
        );
        assert_eq!(
            select.args[2],
            Value::Inst(crate::ir::value::InstRef {
                block: 0,
                inst: min_ref as u32,
            })
        );
    }

    #[test]
    #[should_panic(expected = "IMNMX CC")]
    fn imnmx_rejects_cc_like_upstream() {
        let _ = translate(1u64 << 47);
    }

    #[test]
    #[should_panic(expected = "IMNMX.MODE")]
    fn imnmx_rejects_nonzero_mode_like_upstream() {
        let _ = translate(1u64 << 43);
    }
}
