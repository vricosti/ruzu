// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/floating_point_min_max.cpp

use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::types::{FmzMode, FpControl};
use crate::ir::value::Pred;

pub fn fmnmx(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.f(tv.src_a_reg(insn));
    let src_b = tv.decode_src_b_f32(insn, opcode);

    let pred = Pred(field(insn, 39, 3) as u8);
    let neg_pred = bit(insn, 42);
    let ftz = bit(insn, 44);
    let neg_b = bit(insn, 45);
    let abs_a = bit(insn, 46);
    let cc = bit(insn, 47);
    let neg_a = bit(insn, 48);
    let abs_b = bit(insn, 49);

    if cc {
        panic!("FMNMX CC");
    }

    let a = tv.ir.fp_abs_neg_32(src_a, abs_a, neg_a);
    let b = tv.ir.fp_abs_neg_32(src_b, abs_b, neg_b);

    let control = FpControl {
        fmz_mode: if ftz { FmzMode::FTZ } else { FmzMode::None },
        ..FpControl::default()
    };
    let max = tv.ir.fp_max_32_with_control(a.clone(), b.clone(), control);
    let min = tv.ir.fp_min_32_with_control(a, b, control);
    let (min, max) = if neg_pred { (max, min) } else { (min, max) };
    let pred = tv.ir.get_pred(pred, false);
    let result = tv.ir.select_f32(pred, min, max);

    tv.set_f(dst, result);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::basic_block::Block;
    use crate::ir::opcodes::Opcode;
    use crate::ir::program::Program;
    use crate::ir::types::ShaderStage;

    #[test]
    fn fmnmx_mk8d_particle_word_keeps_ftz_separate_from_abs_b() {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        let mut tv = TranslatorVisitor::new(&mut program, 0);

        // Live MK8D flare vertex shader instruction at 0x6754d0.
        fmnmx(&mut tv, 0x5C60_1380_0057_FF03, MaxwellOpcode::FMNMX_reg);

        let block = &tv.ir.program.blocks[0];
        assert!(!block.iter().any(|inst| inst.opcode == Opcode::FPAbs32));
        for opcode in [Opcode::FPMin32, Opcode::FPMax32] {
            let inst = block
                .iter()
                .find(|inst| inst.opcode == opcode)
                .expect("FMNMX must emit both min and max like upstream");
            assert_eq!(FpControl::from_u32(inst.flags).fmz_mode, FmzMode::FTZ);
        }
        assert!(block.iter().any(|inst| inst.opcode == Opcode::GetPred));
        assert!(block.iter().any(|inst| inst.opcode == Opcode::SelectF32));
    }

    #[test]
    #[should_panic(expected = "FMNMX CC")]
    fn fmnmx_rejects_cc_like_upstream() {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        let mut tv = TranslatorVisitor::new(&mut program, 0);
        fmnmx(&mut tv, 1u64 << 47, MaxwellOpcode::FMNMX_reg);
    }
}
