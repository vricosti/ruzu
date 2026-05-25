// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/floating_point_fused_multiply_add.cpp

use super::{bit, field, TranslatorVisitor};
use super::common_encoding::{cast_fmz_mode, cast_fp_rounding, MaxwellFmzMode, MaxwellFpRounding};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::types::FpControl;
use crate::ir::value::Value;

pub fn ffma(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let (src_b, src_c) = match opcode {
        MaxwellOpcode::FFMA_reg => (tv.get_float_reg20(insn), tv.get_float_reg39(insn)),
        MaxwellOpcode::FFMA_rc => (tv.get_float_reg39(insn), tv.get_float_cbuf(insn)),
        MaxwellOpcode::FFMA_cr => (tv.get_float_cbuf(insn), tv.get_float_reg39(insn)),
        MaxwellOpcode::FFMA_imm => (tv.get_float_imm20(insn), tv.get_float_reg39(insn)),
        _ => unreachable!("invalid opcode for FFMA: {:?}", opcode),
    };

    let fp_rounding = MaxwellFpRounding::from_field(field(insn, 51, 2) as u32);
    let fmz_mode = maxwell_fmz_mode(field(insn, 53, 2) as u32);

    ffma_inner(
        tv,
        insn,
        src_b,
        src_c,
        false,
        bit(insn, 48),
        bit(insn, 49),
        bit(insn, 50),
        bit(insn, 47),
        fp_rounding,
        fmz_mode,
    );
}

fn ffma_inner(
    tv: &mut TranslatorVisitor,
    insn: u64,
    src_b: Value,
    src_c: Value,
    neg_a: bool,
    neg_b: bool,
    neg_c: bool,
    sat: bool,
    cc: bool,
    fp_rounding: MaxwellFpRounding,
    fmz_mode: MaxwellFmzMode,
) {
    let dst = tv.dst_reg(insn);
    let src_a = tv.f(tv.src_a_reg(insn));

    if cc {
        panic!("FFMA CC");
    }

    let a = tv.ir.fp_abs_neg_32(src_a, false, neg_a);
    let b = tv.ir.fp_abs_neg_32(src_b, false, neg_b);
    let c = tv.ir.fp_abs_neg_32(src_c, false, neg_c);

    let fp_control = FpControl {
        no_contraction: true,
        rounding: cast_fp_rounding(fp_rounding),
        fmz_mode: cast_fmz_mode(fmz_mode),
    };
    let mut result = tv.ir.fp_fma_32_with_control(a, b, c, fp_control);

    if fmz_mode == MaxwellFmzMode::Fmz && !sat {
        let zero = Value::ImmF32(0.0);
        let zero_a = tv.ir.fp_ord_equal_32(a, zero);
        let zero_b = tv.ir.fp_ord_equal_32(b, zero);
        let any_zero = tv.ir.logical_or(zero_a, zero_b);
        result = tv.ir.select_f32(any_zero, c, result);
    }

    if sat {
        result = tv.ir.fp_saturate_32(result);
    }

    tv.set_f(dst, result);
}

pub fn ffma32i(tv: &mut TranslatorVisitor, insn: u64) {
    let imm = tv.decode_imm32(insn);
    let src_b = Value::ImmF32(f32::from_bits(imm));
    let src_c = tv.f(field(insn, 0, 8));

    ffma_inner(
        tv,
        insn,
        src_b,
        src_c,
        bit(insn, 56),
        false,
        bit(insn, 57),
        bit(insn, 55),
        bit(insn, 52),
        MaxwellFpRounding::Rn,
        maxwell_fmz_mode(field(insn, 53, 2) as u32),
    );
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

    #[test]
    fn ffma32i_fmz_emits_zero_operand_select_like_upstream() {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        let mut tv = TranslatorVisitor::new(&mut program, 0);

        let insn = 1u64 | (0x3f80_0000u64 << 20) | (2u64 << 53);
        ffma32i(&mut tv, insn);

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
}
