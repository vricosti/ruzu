// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/floating_point_add.cpp

use super::common_encoding::{cast_fp_rounding, MaxwellFpRounding};
use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::types::{FmzMode, FpControl};
use crate::ir::value::Value;

pub fn fadd(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.f(tv.src_a_reg(insn));
    let src_b = tv.decode_src_b_f32(insn, opcode);

    let abs_a = bit(insn, 46);
    let neg_a = bit(insn, 48);
    let abs_b = bit(insn, 49);
    let neg_b = bit(insn, 45);
    let sat = bit(insn, 50);
    let cc = bit(insn, 47);
    let ftz = bit(insn, 44);
    let fp_rounding = MaxwellFpRounding::from_field(field(insn, 39, 2) as u32);

    if cc {
        panic!("FADD CC");
    }

    let a = tv.ir.fp_abs_neg_32(src_a, abs_a, neg_a);
    let b = tv.ir.fp_abs_neg_32(src_b, abs_b, neg_b);
    let mut result = tv.ir.fp_add_32_with_control(
        a,
        b,
        FpControl {
            no_contraction: true,
            rounding: cast_fp_rounding(fp_rounding),
            fmz_mode: if ftz { FmzMode::FTZ } else { FmzMode::None },
        },
    );

    if sat {
        result = tv.ir.fp_saturate_32(result);
    }

    tv.set_f(dst, result);
}

pub fn fadd32i(tv: &mut TranslatorVisitor, insn: u64) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.f(tv.src_a_reg(insn));
    let imm = tv.decode_imm32(insn);
    let src_b = Value::ImmF32(f32::from_bits(imm));

    let abs_a = bit(insn, 54);
    let neg_a = bit(insn, 56);
    let abs_b = bit(insn, 57);
    let neg_b = bit(insn, 53);
    let cc = bit(insn, 52);
    let ftz = bit(insn, 55);

    if cc {
        panic!("FADD CC");
    }

    let a = tv.ir.fp_abs_neg_32(src_a, abs_a, neg_a);
    let b = tv.ir.fp_abs_neg_32(src_b, abs_b, neg_b);
    let result = tv.ir.fp_add_32_with_control(
        a,
        b,
        FpControl {
            no_contraction: true,
            rounding: cast_fp_rounding(MaxwellFpRounding::Rn),
            fmz_mode: if ftz { FmzMode::FTZ } else { FmzMode::None },
        },
    );

    tv.set_f(dst, result);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::basic_block::Block;
    use crate::ir::opcodes::Opcode;
    use crate::ir::program::Program;
    use crate::ir::types::{FpRounding, ShaderStage};

    fn fresh_program() -> Program {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        program
    }

    #[test]
    fn fadd_preserves_upstream_fp_control() {
        let mut program = fresh_program();
        let mut tv = TranslatorVisitor::new(&mut program, 0);
        let insn = 1u64 | (2u64 << 20) | (1u64 << 39) | (1u64 << 44);

        fadd(&mut tv, insn, MaxwellOpcode::FADD_reg);

        let add = tv.ir.program.blocks[0]
            .iter()
            .find(|inst| inst.opcode == Opcode::FPAdd32)
            .expect("FADD must emit FPAdd32");
        let control = FpControl::from_u32(add.flags);
        assert!(control.no_contraction);
        assert_eq!(control.rounding, FpRounding::RM);
        assert_eq!(control.fmz_mode, FmzMode::FTZ);
    }

    #[test]
    fn fadd32i_uses_rn_and_no_contraction() {
        let mut program = fresh_program();
        let mut tv = TranslatorVisitor::new(&mut program, 0);
        let insn = 1u64 | (0x3f80_0000u64 << 20);

        fadd32i(&mut tv, insn);

        let add = tv.ir.program.blocks[0]
            .iter()
            .find(|inst| inst.opcode == Opcode::FPAdd32)
            .expect("FADD32I must emit FPAdd32");
        let control = FpControl::from_u32(add.flags);
        assert!(control.no_contraction);
        assert_eq!(control.rounding, FpRounding::RN);
        assert_eq!(control.fmz_mode, FmzMode::None);
    }
}
