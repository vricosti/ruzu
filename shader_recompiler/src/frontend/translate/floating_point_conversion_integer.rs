// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/floating_point_conversion_integer.cpp

use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::value::Value;

const DEST_FORMAT_I16: u32 = 1;
const DEST_FORMAT_I32: u32 = 2;
const DEST_FORMAT_I64: u32 = 3;
const SRC_FORMAT_F32: u32 = 2;

const ROUNDING_ROUND: u32 = 0;
const ROUNDING_FLOOR: u32 = 1;
const ROUNDING_CEIL: u32 = 2;
const ROUNDING_TRUNC: u32 = 3;

pub fn f2i(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src = tv.decode_src_b_f32(insn, opcode);

    let dest_format = field(insn, 8, 2);
    let src_format = field(insn, 10, 2);
    let is_signed = bit(insn, 12);
    let rounding = field(insn, 39, 2);
    let abs_src = bit(insn, 45);
    let neg_src = bit(insn, 49);
    let cc = bit(insn, 47);

    let a = tv.ir.fp_abs_neg_32(src, abs_src, neg_src);
    let rounded = match rounding {
        ROUNDING_ROUND => tv.ir.fp_round_even_32(a),
        ROUNDING_FLOOR => tv.ir.fp_floor_32(a),
        ROUNDING_CEIL => tv.ir.fp_ceil_32(a),
        ROUNDING_TRUNC => tv.ir.fp_trunc_32(a),
        _ => unreachable!("2-bit F2I rounding field"),
    };

    if src_format != SRC_FORMAT_F32 {
        log::warn!("F2I source format {} not fully ported; treating as F32", src_format);
    }
    if dest_format == DEST_FORMAT_I64 {
        log::warn!("F2I I64 destination not fully ported; writing low 32 bits");
    }

    let (min_bound, max_bound) = clamp_bounds(dest_format, is_signed);
    let min = Value::ImmF32(min_bound);
    let max = Value::ImmF32(max_bound);
    let clamped = tv.ir.fp_clamp_32(rounded, min, max);

    let result = if is_signed {
        let converted = tv.ir.convert_s32_from_f32(clamped);
        let is_nan = tv.ir.fp_is_nan_32(a);
        tv.ir.select_u32(is_nan, Value::ImmU32(0), converted)
    } else {
        tv.ir.convert_u32_from_f32(clamped)
    };

    tv.set_x(dst, result);

    if cc {
        log::warn!("F2I CC is not implemented");
    }
}

fn clamp_bounds(dest_format: u32, is_signed: bool) -> (f32, f32) {
    match (dest_format, is_signed) {
        (DEST_FORMAT_I16, true) => (i16::MIN as f32, i16::MAX as f32),
        (DEST_FORMAT_I16, false) => (u16::MIN as f32, u16::MAX as f32),
        (DEST_FORMAT_I32, true) => (i32::MIN as f32, i32::MAX as f32),
        (DEST_FORMAT_I32, false) => (u32::MIN as f32, u32::MAX as f32),
        (DEST_FORMAT_I64, true) => (i32::MIN as f32, i32::MAX as f32),
        (DEST_FORMAT_I64, false) => (u32::MIN as f32, u32::MAX as f32),
        _ => {
            log::warn!("Invalid F2I destination format {}; using I32 bounds", dest_format);
            (i32::MIN as f32, i32::MAX as f32)
        }
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
    fn f2i_uses_upstream_abs_neg_and_rounding_fields() {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        let mut tv = TranslatorVisitor::new(&mut program, 0);

        let insn = 1u64
            | (DEST_FORMAT_I32 as u64) << 8
            | (SRC_FORMAT_F32 as u64) << 10
            | 1u64 << 12
            | 2u64 << 20
            | (ROUNDING_FLOOR as u64) << 39
            | 1u64 << 45
            | 1u64 << 49;

        f2i(&mut tv, insn, MaxwellOpcode::F2I_reg);
        let opcodes: Vec<_> = tv
            .ir
            .program
            .blocks[0]
            .iter()
            .map(|inst| inst.opcode)
            .collect();

        assert!(opcodes.contains(&Opcode::FPAbs32));
        assert!(opcodes.contains(&Opcode::FPNeg32));
        assert!(opcodes.contains(&Opcode::FPFloor32));
        assert!(opcodes.contains(&Opcode::FPClamp32));
        assert!(opcodes.contains(&Opcode::ConvertS32F32));
        assert!(opcodes.contains(&Opcode::FPIsNan32));
        assert!(opcodes.contains(&Opcode::SelectU32));
    }
}
