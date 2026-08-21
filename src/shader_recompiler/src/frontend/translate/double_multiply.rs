// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/double_multiply.cpp

use super::common_encoding::{cast_fp_rounding, MaxwellFpRounding};
use super::{bit, field, TranslatorVisitor};
use crate::ir::types::{FmzMode, FpControl};
use crate::ir::value::Value;

fn dmul_impl(tv: &mut TranslatorVisitor, insn: u64, src_b: Value) {
    let dst = field(insn, 0, 8);
    let src_a_reg = field(insn, 8, 8);
    let neg = bit(insn, 48);
    let cc = bit(insn, 47);
    let fp_rounding = MaxwellFpRounding::from_field(field(insn, 39, 2) as u32);

    if cc {
        panic!("DMUL CC");
    }

    let src_a = tv.d(src_a_reg);
    let op_a = tv.ir.fp_abs_neg_64(src_a, false, neg);

    let result = tv.ir.fp_mul_64_with_control(
        op_a,
        src_b,
        FpControl {
            no_contraction: true,
            rounding: cast_fp_rounding(fp_rounding),
            fmz_mode: FmzMode::None,
        },
    );
    tv.set_d(dst, result);
}

/// DMUL_reg — Double-precision floating-point multiply, register operand.
pub fn dmul_reg(tv: &mut TranslatorVisitor, insn: u64) {
    let src_b = tv.get_double_reg20(insn);
    dmul_impl(tv, insn, src_b);
}

/// DMUL_cbuf — Double-precision floating-point multiply, constant buffer operand.
pub fn dmul_cbuf(tv: &mut TranslatorVisitor, insn: u64) {
    let src_b = tv.get_double_cbuf(insn);
    dmul_impl(tv, insn, src_b);
}

/// DMUL_imm — Double-precision floating-point multiply, immediate operand.
pub fn dmul_imm(tv: &mut TranslatorVisitor, insn: u64) {
    let src_b = tv.get_double_imm20(insn);
    dmul_impl(tv, insn, src_b);
}

/// DMUL — dispatch wrapper (used by the opcode table when variant not split).
pub fn dmul(tv: &mut TranslatorVisitor, insn: u64) {
    dmul_reg(tv, insn);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{basic_block::Block, opcodes::Opcode, program::Program, types::ShaderStage};

    #[test]
    fn dmul_preserves_upstream_fp_control() {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        let mut tv = TranslatorVisitor::new(&mut program, 0);
        dmul_reg(&mut tv, 3u64 << 39);
        let inst = tv.ir.program.blocks[0]
            .iter()
            .find(|inst| inst.opcode == Opcode::FPMul64)
            .unwrap();
        let control = FpControl::from_u32(inst.flags);
        assert!(control.no_contraction);
        assert_eq!(control.rounding, crate::ir::types::FpRounding::RZ);
        assert_eq!(control.fmz_mode, FmzMode::None);
    }
}
