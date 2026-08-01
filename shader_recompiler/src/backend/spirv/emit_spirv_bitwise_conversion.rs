// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V bitwise conversion emission — maps to zuyu's
//! `backend/spirv/emit_spirv_bitwise_conversion.cpp`.

use super::spirv_emit_context::SpirvEmitContext;
use rspirv::{dr::Operand, spirv::Word};

/// Emit `OpBitcast` U32 <- F32.
///
/// Matches upstream `EmitBitCastU32F32(EmitContext&, Id)`.
pub fn emit_bit_cast_u32_f32(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    ctx.builder.bitcast(ctx.u32_type, None, value).unwrap()
}

/// Emit `OpBitcast` F32 <- U32.
///
/// Matches upstream `EmitBitCastF32U32(EmitContext&, Id)`.
pub fn emit_bit_cast_f32_u32(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    ctx.builder.bitcast(ctx.f32_type, None, value).unwrap()
}

/// Emit `OpBitcast` U64 <- U32x2 (PackUint2x32).
///
/// Matches upstream `EmitPackUint2x32(EmitContext&, Id)`.
pub fn emit_pack_uint2x32(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    ctx.builder.bitcast(ctx.u64_type, None, value).unwrap()
}

/// Emit `OpBitcast` U32x2 <- U64 (UnpackUint2x32).
///
/// Matches upstream `EmitUnpackUint2x32(EmitContext&, Id)`.
pub fn emit_unpack_uint2x32(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    ctx.builder.bitcast(ctx.u32_vec2_type, None, value).unwrap()
}

/// Emit `OpBitcast` U32 <- F16x2 (PackFloat2x16).
///
/// Matches upstream `EmitPackFloat2x16(EmitContext&, Id)`.
pub fn emit_pack_float2x16(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    ctx.builder.bitcast(ctx.u32_type, None, value).unwrap()
}

/// Emit `OpBitcast` F16x2 <- U32 (UnpackFloat2x16).
///
/// Matches upstream `EmitUnpackFloat2x16(EmitContext&, Id)`.
pub fn emit_unpack_float2x16(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    ctx.builder.bitcast(ctx.f16_vec2_type, None, value).unwrap()
}

/// Emit `GLSL.std.450 PackHalf2x16` U32 <- F32x2.
///
/// Matches upstream `EmitPackHalf2x16(EmitContext&, Id)`.
pub fn emit_pack_half2x16(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    ctx.builder
        .ext_inst(
            ctx.u32_type,
            None,
            ctx.glsl_ext,
            58,
            vec![Operand::IdRef(value)],
        )
        .unwrap()
}

/// Emit `GLSL.std.450 UnpackHalf2x16` F32x2 <- U32.
///
/// Matches upstream `EmitUnpackHalf2x16(EmitContext&, Id)`.
pub fn emit_unpack_half2x16(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    ctx.builder
        .ext_inst(
            ctx.f32_vec2_type,
            None,
            ctx.glsl_ext,
            62,
            vec![Operand::IdRef(value)],
        )
        .unwrap()
}

/// Emit `OpBitcast` F64 <- U32x2 (PackDouble2x32).
///
/// Matches upstream `EmitPackDouble2x32(EmitContext&, Id)`.
pub fn emit_pack_double2x32(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    ctx.builder.bitcast(ctx.f64_type, None, value).unwrap()
}

/// Emit `OpBitcast` U32x2 <- F64 (UnpackDouble2x32).
///
/// Matches upstream `EmitUnpackDouble2x32(EmitContext&, Id)`.
pub fn emit_unpack_double2x32(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    ctx.builder.bitcast(ctx.u32_vec2_type, None, value).unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{
        basic_block::Block,
        instruction::Inst,
        opcodes::Opcode,
        program::{Program, SyntaxNode},
        types::ShaderStage,
        value::{InstRef, Value},
    };
    use crate::{profile::Profile, runtime_info::RuntimeInfo};
    use rspirv::spirv::{self, Op};

    #[test]
    fn half_pack_unpack_emit_upstream_glsl_ext_instructions() {
        let program = Program::new(ShaderStage::Fragment);
        let mut ctx = SpirvEmitContext::new(&program, &Profile::default(), &RuntimeInfo::default());
        ctx.builder
            .begin_function(
                ctx.void_type,
                None,
                spirv::FunctionControl::NONE,
                ctx.void_fn_type,
            )
            .unwrap();
        ctx.builder.begin_block(None).unwrap();

        let pair = ctx
            .builder
            .composite_construct(
                ctx.f32_vec2_type,
                None,
                vec![ctx.const_zero_f32, ctx.const_one_f32],
            )
            .unwrap();
        let packed = emit_pack_half2x16(&mut ctx, pair);
        let _unpacked = emit_unpack_half2x16(&mut ctx, packed);

        let instructions = &ctx.builder.module_ref().functions[0].blocks[0].instructions;
        for expected in [58, 62] {
            assert!(instructions.iter().any(|inst| {
                inst.class.opcode == Op::ExtInst
                    && matches!(
                        inst.operands.get(1),
                        Some(Operand::LiteralExtInstInteger(opcode)) if *opcode == expected
                    )
            }));
        }
    }

    #[test]
    fn pack_half_result_can_feed_bitcast_during_program_emission() {
        let mut program = Program::new(ShaderStage::Fragment);
        let mut block = Block::new();
        let pair = block.append_inst(Inst::new(
            Opcode::CompositeConstructF32x2,
            vec![Value::ImmF32(0.25), Value::ImmF32(0.75)],
        ));
        let packed = block.append_inst(Inst::new(
            Opcode::PackHalf2x16,
            vec![Value::Inst(InstRef {
                block: 0,
                inst: pair,
            })],
        ));
        block.append_inst(Inst::new(
            Opcode::BitCastF32U32,
            vec![Value::Inst(InstRef {
                block: 0,
                inst: packed,
            })],
        ));
        program.blocks.push(block);
        program.syntax_list = vec![SyntaxNode::Block(0), SyntaxNode::Return];

        let words = crate::backend::spirv::emit_spirv::emit_spirv(
            &program,
            &Profile::default(),
            &RuntimeInfo::default(),
        );
        let module = rspirv::dr::load_words(words).unwrap();
        assert!(module.functions.iter().any(|function| {
            function.blocks.iter().any(|block| {
                block.instructions.iter().any(|inst| {
                    inst.class.opcode == Op::ExtInst
                        && matches!(
                            inst.operands.get(1),
                            Some(Operand::LiteralExtInstInteger(58))
                        )
                })
            })
        }));
    }
}
