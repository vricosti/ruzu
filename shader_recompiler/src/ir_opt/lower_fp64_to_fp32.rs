// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `ir_opt/lower_fp64_to_fp32.cpp`.

use crate::ir::basic_block::Block;
use crate::ir::instruction::Inst;
use crate::ir::opcodes::Opcode;
use crate::ir::program::{Program, SyntaxNode};
use crate::ir::value::{InstRef, Value};

const F64_TO_F32_EXP: i32 = 1023 - 127;
const F32_TO_F64_EXP: i32 = 127 - 1023;

fn insert_before(
    block: &mut Block,
    block_index: u32,
    before: u32,
    opcode: Opcode,
    args: Vec<Value>,
) -> Value {
    let inst = block.insert_inst_before(before, Inst::new(opcode, args));
    Value::Inst(InstRef {
        block: block_index,
        inst,
    })
}

fn replace_uses_with(program: &mut Program, old: InstRef, replacement: Value) {
    let old_value = Value::Inst(old);
    for block in &mut program.blocks {
        for inst in block.iter_mut() {
            for arg in &mut inst.args {
                if *arg == old_value {
                    *arg = replacement;
                }
            }
            for (_, value) in &mut inst.phi_args {
                if *value == old_value {
                    *value = replacement;
                }
            }
        }
    }
    for node in &mut program.syntax_list {
        match node {
            SyntaxNode::If { cond, .. }
            | SyntaxNode::Repeat { cond, .. }
            | SyntaxNode::Break { cond, .. } => {
                if *cond == old_value {
                    *cond = replacement;
                }
            }
            _ => {}
        }
    }
}

fn packed_f64_to_f32(program: &mut Program, inst_ref: InstRef, packed: Value) -> Value {
    let block = program.block_mut(inst_ref.block);
    let emit = |block: &mut Block, opcode, args| {
        insert_before(block, inst_ref.block, inst_ref.inst, opcode, args)
    };
    let lo = emit(
        block,
        Opcode::CompositeExtractU32x2,
        vec![packed, Value::ImmU32(0)],
    );
    let hi = emit(
        block,
        Opcode::CompositeExtractU32x2,
        vec![packed, Value::ImmU32(1)],
    );
    let sign = emit(
        block,
        Opcode::BitFieldUExtract,
        vec![hi, Value::ImmU32(31), Value::ImmU32(1)],
    );
    let exp = emit(
        block,
        Opcode::BitFieldUExtract,
        vec![hi, Value::ImmU32(20), Value::ImmU32(11)],
    );
    let mantissa_hi = emit(
        block,
        Opcode::BitFieldUExtract,
        vec![hi, Value::ImmU32(0), Value::ImmU32(20)],
    );
    let mantissa_lo = emit(
        block,
        Opcode::BitFieldUExtract,
        vec![lo, Value::ImmU32(29), Value::ImmU32(3)],
    );
    let mantissa_hi = emit(
        block,
        Opcode::ShiftLeftLogical32,
        vec![mantissa_hi, Value::ImmU32(3)],
    );
    let mantissa = emit(block, Opcode::BitwiseOr32, vec![mantissa_hi, mantissa_lo]);
    let exp_is_zero = emit(block, Opcode::IEqual, vec![exp, Value::ImmU32(0)]);
    let adjusted_exp = emit(
        block,
        Opcode::IAdd32,
        vec![exp, Value::ImmU32(F64_TO_F32_EXP as u32)],
    );
    let exp_if_subnorm = emit(
        block,
        Opcode::SelectU32,
        vec![exp_is_zero, Value::ImmU32(0), adjusted_exp],
    );
    let exp_is_infnan = emit(block, Opcode::IEqual, vec![exp, Value::ImmU32(0x7ff)]);
    let exp_if_infnan = emit(
        block,
        Opcode::SelectU32,
        vec![exp_is_infnan, Value::ImmU32(0xff), exp_if_subnorm],
    );
    let sign = emit(
        block,
        Opcode::ShiftLeftLogical32,
        vec![sign, Value::ImmU32(31)],
    );
    let exp = emit(
        block,
        Opcode::ShiftLeftLogical32,
        vec![exp_if_infnan, Value::ImmU32(23)],
    );
    let exp_mantissa = emit(block, Opcode::BitwiseOr32, vec![exp, mantissa]);
    let result = emit(block, Opcode::BitwiseOr32, vec![sign, exp_mantissa]);
    emit(block, Opcode::BitCastF32U32, vec![result])
}

fn f32_to_packed_f64(program: &mut Program, inst_ref: InstRef, raw: Value) -> Value {
    let block = program.block_mut(inst_ref.block);
    let emit = |block: &mut Block, opcode, args| {
        insert_before(block, inst_ref.block, inst_ref.inst, opcode, args)
    };
    let value = emit(block, Opcode::BitCastU32F32, vec![raw]);
    let sign = emit(
        block,
        Opcode::BitFieldUExtract,
        vec![value, Value::ImmU32(31), Value::ImmU32(1)],
    );
    let exp = emit(
        block,
        Opcode::BitFieldUExtract,
        vec![value, Value::ImmU32(23), Value::ImmU32(8)],
    );
    let mantissa = emit(
        block,
        Opcode::BitFieldUExtract,
        vec![value, Value::ImmU32(0), Value::ImmU32(23)],
    );
    let mantissa_hi = emit(
        block,
        Opcode::BitFieldUExtract,
        vec![mantissa, Value::ImmU32(3), Value::ImmU32(20)],
    );
    let mantissa_lo = emit(
        block,
        Opcode::BitFieldUExtract,
        vec![mantissa, Value::ImmU32(0), Value::ImmU32(3)],
    );
    let exp_is_zero = emit(block, Opcode::IEqual, vec![exp, Value::ImmU32(0)]);
    let adjusted_exp = emit(
        block,
        Opcode::IAdd32,
        vec![exp, Value::ImmU32(F32_TO_F64_EXP as u32)],
    );
    let exp_if_subnorm = emit(
        block,
        Opcode::SelectU32,
        vec![exp_is_zero, Value::ImmU32(0), adjusted_exp],
    );
    let exp_is_infnan = emit(block, Opcode::IEqual, vec![exp, Value::ImmU32(0xff)]);
    let exp_if_infnan = emit(
        block,
        Opcode::SelectU32,
        vec![exp_is_infnan, Value::ImmU32(0x7ff), exp_if_subnorm],
    );
    let lo = emit(
        block,
        Opcode::ShiftLeftLogical32,
        vec![mantissa_lo, Value::ImmU32(29)],
    );
    let sign = emit(
        block,
        Opcode::ShiftLeftLogical32,
        vec![sign, Value::ImmU32(31)],
    );
    let exp = emit(
        block,
        Opcode::ShiftLeftLogical32,
        vec![exp_if_infnan, Value::ImmU32(20)],
    );
    let exp_mantissa = emit(block, Opcode::BitwiseOr32, vec![exp, mantissa_hi]);
    let hi = emit(block, Opcode::BitwiseOr32, vec![sign, exp_mantissa]);
    emit(block, Opcode::CompositeConstructU32x2, vec![lo, hi])
}

fn replacement(opcode: Opcode) -> Opcode {
    match opcode {
        Opcode::FPAbs64 => Opcode::FPAbs32,
        Opcode::FPAdd64 => Opcode::FPAdd32,
        Opcode::FPCeil64 => Opcode::FPCeil32,
        Opcode::FPFloor64 => Opcode::FPFloor32,
        Opcode::FPFma64 => Opcode::FPFma32,
        Opcode::FPMul64 => Opcode::FPMul32,
        Opcode::FPNeg64 => Opcode::FPNeg32,
        Opcode::FPRoundEven64 => Opcode::FPRoundEven32,
        Opcode::FPSaturate64 => Opcode::FPSaturate32,
        Opcode::FPClamp64 => Opcode::FPClamp32,
        Opcode::FPTrunc64 => Opcode::FPTrunc32,
        Opcode::CompositeConstructF64x2 => Opcode::CompositeConstructF32x2,
        Opcode::CompositeConstructF64x3 => Opcode::CompositeConstructF32x3,
        Opcode::CompositeConstructF64x4 => Opcode::CompositeConstructF32x4,
        Opcode::CompositeExtractF64x2 => Opcode::CompositeExtractF32x2,
        Opcode::CompositeExtractF64x3 => Opcode::CompositeExtractF32x3,
        Opcode::CompositeExtractF64x4 => Opcode::CompositeExtractF32x4,
        Opcode::CompositeInsertF64x2 => Opcode::CompositeInsertF32x2,
        Opcode::CompositeInsertF64x3 => Opcode::CompositeInsertF32x3,
        Opcode::CompositeInsertF64x4 => Opcode::CompositeInsertF32x4,
        Opcode::FPOrdEqual64 => Opcode::FPOrdEqual32,
        Opcode::FPUnordEqual64 => Opcode::FPUnordEqual32,
        Opcode::FPOrdNotEqual64 => Opcode::FPOrdNotEqual32,
        Opcode::FPUnordNotEqual64 => Opcode::FPUnordNotEqual32,
        Opcode::FPOrdLessThan64 => Opcode::FPOrdLessThan32,
        Opcode::FPUnordLessThan64 => Opcode::FPUnordLessThan32,
        Opcode::FPOrdGreaterThan64 => Opcode::FPOrdGreaterThan32,
        Opcode::FPUnordGreaterThan64 => Opcode::FPUnordGreaterThan32,
        Opcode::FPOrdLessThanEqual64 => Opcode::FPOrdLessThanEqual32,
        Opcode::FPUnordLessThanEqual64 => Opcode::FPUnordLessThanEqual32,
        Opcode::FPOrdGreaterThanEqual64 => Opcode::FPOrdGreaterThanEqual32,
        Opcode::FPUnordGreaterThanEqual64 => Opcode::FPUnordGreaterThanEqual32,
        Opcode::FPIsNan64 => Opcode::FPIsNan32,
        Opcode::ConvertS16F64 => Opcode::ConvertS16F32,
        Opcode::ConvertS32F64 => Opcode::ConvertS32F32,
        Opcode::ConvertS64F64 => Opcode::ConvertS64F32,
        Opcode::ConvertU16F64 => Opcode::ConvertU16F32,
        Opcode::ConvertU32F64 => Opcode::ConvertU32F32,
        Opcode::ConvertU64F64 => Opcode::ConvertU64F32,
        Opcode::ConvertF32F64 | Opcode::ConvertF64F32 => Opcode::Identity,
        Opcode::ConvertF64S8 => Opcode::ConvertF32S8,
        Opcode::ConvertF64S16 => Opcode::ConvertF32S16,
        Opcode::ConvertF64S32 => Opcode::ConvertF32S32,
        Opcode::ConvertF64S64 => Opcode::ConvertF32S64,
        Opcode::ConvertF64U8 => Opcode::ConvertF32U8,
        Opcode::ConvertF64U16 => Opcode::ConvertF32U16,
        Opcode::ConvertF64U32 => Opcode::ConvertF32U32,
        Opcode::ConvertF64U64 => Opcode::ConvertF32U64,
        _ => opcode,
    }
}

fn lower(program: &mut Program, inst_ref: InstRef) {
    let inst = program.block(inst_ref.block).inst(inst_ref.inst).clone();
    match inst.opcode {
        Opcode::PackDouble2x32 => {
            let value = packed_f64_to_f32(program, inst_ref, inst.args[0]);
            replace_uses_with(program, inst_ref, value);
        }
        Opcode::UnpackDouble2x32 => {
            let value = f32_to_packed_f64(program, inst_ref, inst.args[0]);
            replace_uses_with(program, inst_ref, value);
        }
        opcode => {
            program
                .block_mut(inst_ref.block)
                .inst_mut(inst_ref.inst)
                .opcode = replacement(opcode)
        }
    }
}

/// Port of upstream `LowerFp64ToFp32`.
pub fn lower_fp64_to_fp32(program: &mut Program) {
    let instructions = program
        .blocks
        .iter()
        .enumerate()
        .flat_map(|(block, contents)| {
            contents.indexed_iter().map(move |(inst, _)| InstRef {
                block: block as u32,
                inst,
            })
        })
        .collect::<Vec<_>>();
    for inst in instructions {
        lower(program, inst);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::types::ShaderStage;

    #[test]
    fn replaces_fp64_opcode_table_like_upstream() {
        let mut program = Program::new(ShaderStage::Fragment);
        let mut block = Block::new();
        block.append_inst(Inst::new(
            Opcode::FPAdd64,
            vec![Value::ImmF64(1.0), Value::ImmF64(2.0)],
        ));
        block.append_inst(Inst::new(Opcode::ConvertF64U32, vec![Value::ImmU32(1)]));
        program.blocks.push(block);

        lower_fp64_to_fp32(&mut program);

        assert_eq!(program.block(0).inst(0).opcode, Opcode::FPAdd32);
        assert_eq!(program.block(0).inst(1).opcode, Opcode::ConvertF32U32);
    }

    #[test]
    fn pack_double_uses_upstream_bit_reconstruction() {
        let mut program = Program::new(ShaderStage::Fragment);
        let mut block = Block::new();
        let packed = block.append_inst(Inst::new(
            Opcode::PackDouble2x32,
            vec![Value::ImmU64(1.0f64.to_bits())],
        ));
        let user = block.append_inst(Inst::new(
            Opcode::Identity,
            vec![Value::Inst(InstRef {
                block: 0,
                inst: packed,
            })],
        ));
        program.blocks.push(block);

        lower_fp64_to_fp32(&mut program);

        assert_ne!(
            program.block(0).inst(user).args[0],
            Value::Inst(InstRef {
                block: 0,
                inst: packed
            })
        );
        assert!(program
            .block(0)
            .iter()
            .any(|inst| inst.opcode == Opcode::BitCastF32U32));
    }
}
