// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `ir_opt/lower_fp16_to_fp32.cpp`
//!
//! Lowers FP16 operations to FP32 equivalents for GPUs that do not
//! support native 16-bit floating-point operations.

use crate::ir::opcodes::Opcode;
use crate::ir::program::Program;

fn replacement(opcode: Opcode) -> Opcode {
    match opcode {
        Opcode::FPAbs16 => Opcode::FPAbs32,
        Opcode::FPAdd16 => Opcode::FPAdd32,
        Opcode::FPCeil16 => Opcode::FPCeil32,
        Opcode::FPFloor16 => Opcode::FPFloor32,
        Opcode::FPFma16 => Opcode::FPFma32,
        Opcode::FPMul16 => Opcode::FPMul32,
        Opcode::FPNeg16 => Opcode::FPNeg32,
        Opcode::FPRoundEven16 => Opcode::FPRoundEven32,
        Opcode::FPSaturate16 => Opcode::FPSaturate32,
        Opcode::FPClamp16 => Opcode::FPClamp32,
        Opcode::FPTrunc16 => Opcode::FPTrunc32,
        Opcode::CompositeConstructF16x2 => Opcode::CompositeConstructF32x2,
        Opcode::CompositeConstructF16x3 => Opcode::CompositeConstructF32x3,
        Opcode::CompositeConstructF16x4 => Opcode::CompositeConstructF32x4,
        Opcode::CompositeExtractF16x2 => Opcode::CompositeExtractF32x2,
        Opcode::CompositeExtractF16x3 => Opcode::CompositeExtractF32x3,
        Opcode::CompositeExtractF16x4 => Opcode::CompositeExtractF32x4,
        Opcode::CompositeInsertF16x2 => Opcode::CompositeInsertF32x2,
        Opcode::CompositeInsertF16x3 => Opcode::CompositeInsertF32x3,
        Opcode::CompositeInsertF16x4 => Opcode::CompositeInsertF32x4,
        Opcode::FPOrdEqual16 => Opcode::FPOrdEqual32,
        Opcode::FPUnordEqual16 => Opcode::FPUnordEqual32,
        Opcode::FPOrdNotEqual16 => Opcode::FPOrdNotEqual32,
        Opcode::FPUnordNotEqual16 => Opcode::FPUnordNotEqual32,
        Opcode::FPOrdLessThan16 => Opcode::FPOrdLessThan32,
        Opcode::FPUnordLessThan16 => Opcode::FPUnordLessThan32,
        Opcode::FPOrdGreaterThan16 => Opcode::FPOrdGreaterThan32,
        Opcode::FPUnordGreaterThan16 => Opcode::FPUnordGreaterThan32,
        Opcode::FPOrdLessThanEqual16 => Opcode::FPOrdLessThanEqual32,
        Opcode::FPUnordLessThanEqual16 => Opcode::FPUnordLessThanEqual32,
        Opcode::FPOrdGreaterThanEqual16 => Opcode::FPOrdGreaterThanEqual32,
        Opcode::FPUnordGreaterThanEqual16 => Opcode::FPUnordGreaterThanEqual32,
        Opcode::FPIsNan16 => Opcode::FPIsNan32,
        Opcode::ConvertS16F16 => Opcode::ConvertS16F32,
        Opcode::ConvertS32F16 => Opcode::ConvertS32F32,
        Opcode::ConvertS64F16 => Opcode::ConvertS64F32,
        Opcode::ConvertU16F16 => Opcode::ConvertU16F32,
        Opcode::ConvertU32F16 => Opcode::ConvertU32F32,
        Opcode::ConvertU64F16 => Opcode::ConvertU64F32,
        Opcode::PackFloat2x16 => Opcode::PackHalf2x16,
        Opcode::UnpackFloat2x16 => Opcode::UnpackHalf2x16,
        Opcode::ConvertF32F16 | Opcode::ConvertF16F32 => Opcode::Identity,
        Opcode::ConvertF16S8 => Opcode::ConvertF32S8,
        Opcode::ConvertF16S16 => Opcode::ConvertF32S16,
        Opcode::ConvertF16S32 => Opcode::ConvertF32S32,
        Opcode::ConvertF16S64 => Opcode::ConvertF32S64,
        Opcode::ConvertF16U8 => Opcode::ConvertF32U8,
        Opcode::ConvertF16U16 => Opcode::ConvertF32U16,
        Opcode::ConvertF16U32 => Opcode::ConvertF32U32,
        Opcode::ConvertF16U64 => Opcode::ConvertF32U64,
        Opcode::GlobalAtomicAddF16x2 => Opcode::GlobalAtomicAddF32x2,
        Opcode::StorageAtomicAddF16x2 => Opcode::StorageAtomicAddF32x2,
        Opcode::GlobalAtomicMinF16x2 => Opcode::GlobalAtomicMinF32x2,
        Opcode::StorageAtomicMinF16x2 => Opcode::StorageAtomicMinF32x2,
        Opcode::GlobalAtomicMaxF16x2 => Opcode::GlobalAtomicMaxF32x2,
        Opcode::StorageAtomicMaxF16x2 => Opcode::StorageAtomicMaxF32x2,
        _ => opcode,
    }
}

/// Port of upstream `LowerFp16ToFp32`.
pub fn lower_fp16_to_fp32(program: &mut Program) {
    for block in &mut program.blocks {
        for instruction in block.iter_mut() {
            instruction.opcode = replacement(instruction.opcode);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::basic_block::Block;
    use crate::ir::instruction::Inst;
    use crate::ir::types::ShaderStage;
    use crate::ir::value::Value;

    #[test]
    fn replaces_fp16_and_atomic_opcodes_like_upstream() {
        let mut program = Program::new(ShaderStage::Compute);
        let mut block = Block::new();
        block.append_inst(Inst::new(
            Opcode::FPAdd16,
            vec![Value::ImmF16(0), Value::ImmF16(0)],
        ));
        block.append_inst(Inst::new(
            Opcode::StorageAtomicAddF16x2,
            vec![Value::ImmU32(0), Value::ImmU32(0), Value::ImmU32(0)],
        ));
        program.blocks.push(block);

        lower_fp16_to_fp32(&mut program);

        assert_eq!(program.block(0).inst(0).opcode, Opcode::FPAdd32);
        assert_eq!(
            program.block(0).inst(1).opcode,
            Opcode::StorageAtomicAddF32x2
        );
    }
}
