// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/double_add.cpp

use super::common_encoding::{cast_fp_rounding, MaxwellFpRounding};
use super::{bit, field, TranslatorVisitor};
use crate::ir::types::{FmzMode, FpControl};

fn dadd_impl(tv: &mut TranslatorVisitor, insn: u64, src_b: crate::ir::value::Value) {
    let dst = field(insn, 0, 8);
    let src_a_reg = field(insn, 8, 8);

    let abs_a = bit(insn, 46);
    let neg_a = bit(insn, 48);
    let abs_b = bit(insn, 49);
    let neg_b = bit(insn, 45);
    let cc = bit(insn, 47);
    let fp_rounding = MaxwellFpRounding::from_field(field(insn, 39, 2) as u32);

    if cc {
        panic!("DADD CC");
    }

    let src_a = tv.d(src_a_reg);
    let op_a = tv.ir.fp_abs_neg_64(src_a, abs_a, neg_a);
    let op_b = tv.ir.fp_abs_neg_64(src_b, abs_b, neg_b);

    let result = tv.ir.fp_add_64_with_control(
        op_a,
        op_b,
        FpControl {
            no_contraction: true,
            rounding: cast_fp_rounding(fp_rounding),
            fmz_mode: FmzMode::None,
        },
    );
    tv.set_d(dst, result);
}

/// DADD_reg — Double-precision floating-point add, register operand.
pub fn dadd_reg(tv: &mut TranslatorVisitor, insn: u64) {
    let src_b = tv.get_double_reg20(insn);
    dadd_impl(tv, insn, src_b);
}

/// DADD_cbuf — Double-precision floating-point add, constant buffer operand.
pub fn dadd_cbuf(tv: &mut TranslatorVisitor, insn: u64) {
    let src_b = tv.get_double_cbuf(insn);
    dadd_impl(tv, insn, src_b);
}

/// DADD_imm — Double-precision floating-point add, immediate operand.
pub fn dadd_imm(tv: &mut TranslatorVisitor, insn: u64) {
    let src_b = tv.get_double_imm20(insn);
    dadd_impl(tv, insn, src_b);
}

/// DADD — dispatch wrapper (single entry point used by the opcode table).
pub fn dadd(tv: &mut TranslatorVisitor, insn: u64) {
    // Dispatch based on opcode bits — for now route to reg form as default.
    // The real dispatch happens in translate_instruction via separate opcodes.
    dadd_reg(tv, insn);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{basic_block::Block, opcodes::Opcode, program::Program, types::ShaderStage};

    #[test]
    fn dadd_preserves_upstream_fp_control() {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        let mut tv = TranslatorVisitor::new(&mut program, 0);
        dadd_reg(&mut tv, 2u64 << 39);
        let inst = tv.ir.program.blocks[0]
            .iter()
            .find(|inst| inst.opcode == Opcode::FPAdd64)
            .unwrap();
        let control = FpControl::from_u32(inst.flags);
        assert!(control.no_contraction);
        assert_eq!(control.rounding, crate::ir::types::FpRounding::RP);
        assert_eq!(control.fmz_mode, FmzMode::None);
    }
}
