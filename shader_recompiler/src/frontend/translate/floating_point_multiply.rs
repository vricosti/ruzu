// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/floating_point_multiply.cpp

use super::common_encoding::{cast_fmz_mode, cast_fp_rounding, MaxwellFmzMode, MaxwellFpRounding};
use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::types::FpControl;
use crate::ir::value::Value;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
enum Scale {
    None = 0,
    D2 = 1,
    D4 = 2,
    D8 = 3,
    M8 = 4,
    M4 = 5,
    M2 = 6,
    InvalidScale37 = 7,
}

impl Scale {
    fn from_field(value: u32) -> Self {
        match value & 0x7 {
            0 => Self::None,
            1 => Self::D2,
            2 => Self::D4,
            3 => Self::D8,
            4 => Self::M8,
            5 => Self::M4,
            6 => Self::M2,
            7 => Self::InvalidScale37,
            _ => unreachable!("masked to 3 bits"),
        }
    }
}

fn scale_factor(scale: Scale) -> f32 {
    match scale {
        Scale::None => 1.0,
        Scale::D2 => 1.0 / 2.0,
        Scale::D4 => 1.0 / 4.0,
        Scale::D8 => 1.0 / 8.0,
        Scale::M8 => 8.0,
        Scale::M4 => 4.0,
        Scale::M2 => 2.0,
        Scale::InvalidScale37 => panic!("Invalid FMUL scale {:?}", scale),
    }
}

pub fn fmul(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let mut src_a = tv.f(tv.src_a_reg(insn));
    let src_b = tv.decode_src_b_f32(insn, opcode);

    let neg_b = bit(insn, 48);
    let sat = bit(insn, 50);
    let fp_rounding = MaxwellFpRounding::from_field(field(insn, 39, 2) as u32);
    let scale = Scale::from_field(field(insn, 41, 3) as u32);
    let fmz_mode = maxwell_fmz_mode(field(insn, 44, 2) as u32);

    if bit(insn, 47) {
        panic!("FMUL CC");
    }
    if scale != Scale::None {
        if fmz_mode != MaxwellFmzMode::Ftz || fp_rounding != MaxwellFpRounding::Rn {
            panic!("FMUL scale with non-FMZ or non-RN modifiers");
        }
        src_a = tv.ir.fp_mul_32(src_a, Value::ImmF32(scale_factor(scale)));
    }

    let b = tv.ir.fp_abs_neg_32(src_b, false, neg_b);

    let mut result = tv.ir.fp_mul_32_with_control(
        src_a,
        b,
        FpControl {
            no_contraction: true,
            rounding: cast_fp_rounding(fp_rounding),
            fmz_mode: cast_fmz_mode(fmz_mode),
        },
    );

    if fmz_mode == MaxwellFmzMode::Fmz && !sat {
        let zero = Value::ImmF32(0.0);
        let zero_a = tv.ir.fp_ord_equal_32(src_a, zero);
        let zero_b = tv.ir.fp_ord_equal_32(b, zero);
        let any_zero = tv.ir.logical_or(zero_a, zero_b);
        result = tv.ir.select_f32(any_zero, zero, result);
    }

    if sat {
        result = tv.ir.fp_saturate_32(result);
    }

    tv.set_f(dst, result);
}

pub fn fmul32i(tv: &mut TranslatorVisitor, insn: u64) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.f(tv.src_a_reg(insn));
    let imm = tv.decode_imm32(insn);
    let src_b = Value::ImmF32(f32::from_bits(imm));

    let fmz_mode = maxwell_fmz_mode(field(insn, 53, 2) as u32);
    if bit(insn, 52) {
        panic!("FMUL CC");
    }

    let mut result = tv.ir.fp_mul_32_with_control(
        src_a,
        src_b,
        FpControl {
            no_contraction: true,
            rounding: cast_fp_rounding(MaxwellFpRounding::Rn),
            fmz_mode: cast_fmz_mode(fmz_mode),
        },
    );

    if fmz_mode == MaxwellFmzMode::Fmz && !bit(insn, 55) {
        let zero = Value::ImmF32(0.0);
        let zero_a = tv.ir.fp_ord_equal_32(src_a, zero);
        let zero_b = tv.ir.fp_ord_equal_32(src_b, zero);
        let any_zero = tv.ir.logical_or(zero_a, zero_b);
        result = tv.ir.select_f32(any_zero, zero, result);
    }

    if bit(insn, 55) {
        result = tv.ir.fp_saturate_32(result);
    }

    tv.set_f(dst, result);
}

fn maxwell_fmz_mode(value: u32) -> MaxwellFmzMode {
    match value & 0x3 {
        0 => MaxwellFmzMode::None,
        1 => MaxwellFmzMode::Ftz,
        2 => MaxwellFmzMode::Fmz,
        3 => MaxwellFmzMode::InvalidFmz3,
        _ => unreachable!("masked to 2 bits"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::basic_block::Block;
    use crate::ir::opcodes::Opcode;
    use crate::ir::program::Program;
    use crate::ir::types::ShaderStage;

    fn fresh_program() -> Program {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        program
    }

    #[test]
    fn fmul32i_fmz_emits_zero_operand_select_like_upstream() {
        let mut program = fresh_program();
        let mut tv = TranslatorVisitor::new(&mut program, 0);

        let insn = 1u64 | (0x3f80_0000u64 << 20) | (2u64 << 53);
        fmul32i(&mut tv, insn);

        let opcodes: Vec<_> = tv
            .ir
            .program
            .blocks[0]
            .iter()
            .map(|inst| inst.opcode)
            .collect();
        assert!(opcodes.contains(&Opcode::FPOrdEqual32));
        assert!(opcodes.contains(&Opcode::LogicalOr));
        assert!(opcodes.contains(&Opcode::SelectF32));
    }

    #[test]
    fn fmul_scale_multiplies_src_a_before_main_multiply() {
        let mut program = fresh_program();
        let mut tv = TranslatorVisitor::new(&mut program, 0);

        let insn = 1u64 | (2u64 << 20) | (Scale::D2 as u64) << 41 | (1u64 << 44);
        fmul(&mut tv, insn, MaxwellOpcode::FMUL_reg);

        let fpmul_count = tv
            .ir
            .program
            .blocks[0]
            .iter()
            .filter(|inst| inst.opcode == Opcode::FPMul32)
            .count();
        assert_eq!(fpmul_count, 2);
    }
}
