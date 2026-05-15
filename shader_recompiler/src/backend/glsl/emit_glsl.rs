// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Main GLSL emission entry point.
//!
//! Maps to upstream `backend/glsl/emit_glsl.cpp`.

use crate::ir;
use crate::ir::instruction::Inst;
use crate::ir::opcodes::Opcode;
use crate::ir::program::SyntaxNode;
use crate::ir::types::Type;
use crate::ir::value::{InstRef, Value};

use super::emit_glsl_context_get_set;
use super::glsl_emit_context::EmitContext;
use super::var_alloc::GlslVarType;

/// Walk an IR program and emit GLSL code.
pub fn emit_program(ctx: &mut EmitContext, program: &mut ir::Program) {
    precolor(program);
    if program.syntax_list.is_empty() {
        for block_index in 0..program.blocks.len() as u32 {
            emit_block(ctx, program, block_index);
        }
        return;
    }

    for node in program.syntax_list.clone() {
        match node {
            SyntaxNode::Block(block_index) => emit_block(ctx, program, block_index),
            SyntaxNode::If { cond, .. } => {
                let cond = ctx.var_alloc.consume(program, &cond);
                ctx.add_fmt(format!("if({}){{", cond));
            }
            SyntaxNode::EndIf { .. } => ctx.add_line("}"),
            SyntaxNode::Break { cond, .. } => {
                if let Value::ImmU1(true) = cond {
                    ctx.add_line("break;");
                } else if !matches!(cond, Value::ImmU1(false)) {
                    let cond = ctx.var_alloc.consume(program, &cond);
                    ctx.add_fmt(format!("if({}){{break;}}", cond));
                }
            }
            SyntaxNode::Return | SyntaxNode::Unreachable => ctx.add_line("return;"),
            SyntaxNode::Loop { .. } => ctx.add_line("for(;;){"),
            SyntaxNode::Repeat { cond, .. } => {
                let cond = ctx.var_alloc.consume(program, &cond);
                ctx.add_fmt(format!(
                    "if(--loop{}<0 || !{}){{break;}}}}",
                    ctx.num_safety_loop_vars, cond
                ));
                ctx.num_safety_loop_vars += 1;
            }
        }
    }
}

fn emit_block(ctx: &mut EmitContext, program: &mut ir::Program, block_index: u32) {
    let inst_refs: Vec<InstRef> = program
        .block(block_index)
        .indexed_iter()
        .map(|(inst_index, _)| InstRef {
            block: block_index,
            inst: inst_index,
        })
        .collect();
    for inst_ref in inst_refs {
        emit_inst(ctx, program, inst_ref);
    }
}

/// Emit a single IR instruction as GLSL.
fn emit_inst(ctx: &mut EmitContext, program: &mut ir::Program, inst_ref: InstRef) {
    let inst_snapshot = program.block(inst_ref.block).inst(inst_ref.inst).clone();
    match inst_snapshot.opcode {
        // Arithmetic - float
        Opcode::FPAdd32 => emit_binary(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F32,
            "+",
        ),
        Opcode::FPMul32 => emit_binary(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F32,
            "*",
        ),
        Opcode::FPFma32 => emit_ternary_call(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F32,
            "fma",
        ),
        Opcode::FPMax32 => emit_binary_call(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F32,
            "max",
        ),
        Opcode::FPMin32 => emit_binary_call(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F32,
            "min",
        ),
        Opcode::FPNeg32 => emit_unary_prefix(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F32,
            "-",
        ),
        Opcode::FPAbs32 => emit_unary_call(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F32,
            "abs",
        ),
        Opcode::FPSaturate32 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F32,
            |v| format!("clamp({},0.0f,1.0f)", v),
        ),
        Opcode::FPRoundEven32 => emit_unary_call(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F32,
            "roundEven",
        ),
        Opcode::FPFloor32 => emit_unary_call(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F32,
            "floor",
        ),
        Opcode::FPCeil32 => emit_unary_call(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F32,
            "ceil",
        ),
        Opcode::FPTrunc32 => emit_unary_call(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F32,
            "trunc",
        ),
        Opcode::FPSin => emit_unary_call(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F32,
            "sin",
        ),
        Opcode::FPCos => emit_unary_call(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F32,
            "cos",
        ),
        Opcode::FPExp2 => emit_unary_call(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F32,
            "exp2",
        ),
        Opcode::FPLog2 => emit_unary_call(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F32,
            "log2",
        ),
        Opcode::FPRecip32 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F32,
            |v| format!("1.0f/({})", v),
        ),
        Opcode::FPRecipSqrt32 => emit_unary_call(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F32,
            "inversesqrt",
        ),
        Opcode::FPSqrt32 => emit_unary_call(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F32,
            "sqrt",
        ),

        // Arithmetic - integer
        Opcode::IAdd32 => emit_binary(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U32,
            "+",
        ),
        Opcode::ISub32 => emit_binary(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U32,
            "-",
        ),
        Opcode::IMul32 => emit_binary(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U32,
            "*",
        ),
        Opcode::INeg32 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U32,
            |v| format!("uint(-int({}))", v),
        ),
        Opcode::IAbs32 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U32,
            |v| format!("uint(abs(int({})))", v),
        ),
        Opcode::ShiftLeftLogical32 => emit_binary(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U32,
            "<<",
        ),
        Opcode::ShiftRightLogical32 => emit_binary(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U32,
            ">>",
        ),
        Opcode::ShiftRightArithmetic32 => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U32,
            |a, b| format!("uint(int({})>>int({}))", a, b),
        ),
        Opcode::BitwiseAnd32 => emit_binary(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U32,
            "&",
        ),
        Opcode::BitwiseOr32 => emit_binary(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U32,
            "|",
        ),
        Opcode::BitwiseXor32 => emit_binary(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U32,
            "^",
        ),
        Opcode::BitwiseNot32 => emit_unary_prefix(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U32,
            "~",
        ),
        Opcode::BitReverse32 => emit_unary_call(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U32,
            "bitfieldReverse",
        ),
        Opcode::BitCount32 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U32,
            |v| format!("uint(bitCount({}))", v),
        ),
        Opcode::SMin32 => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U32,
            |a, b| format!("uint(min(int({}),int({})))", a, b),
        ),
        Opcode::UMin32 => emit_binary_call(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U32,
            "min",
        ),
        Opcode::SMax32 => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U32,
            |a, b| format!("uint(max(int({}),int({})))", a, b),
        ),
        Opcode::UMax32 => emit_binary_call(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U32,
            "max",
        ),
        Opcode::BitFieldInsert => emit_quaternary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U32,
            |a, b, c, d| format!("bitfieldInsert({},{},int({}),int({}))", a, b, c, d),
        ),
        Opcode::BitFieldSExtract => emit_ternary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U32,
            |a, b, c| format!("uint(bitfieldExtract(int({}),int({}),int({})))", a, b, c),
        ),
        Opcode::BitFieldUExtract => emit_ternary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U32,
            |a, b, c| format!("bitfieldExtract({},int({}),int({}))", a, b, c),
        ),

        // Composite vector operations. Upstream owns these in
        // `backend/glsl/emit_glsl_composite.cpp`.
        Opcode::CompositeConstructU32x2 => emit_composite_construct(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U32x2,
            "uvec2",
            2,
        ),
        Opcode::CompositeConstructU32x3 => emit_composite_construct(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U32x3,
            "uvec3",
            3,
        ),
        Opcode::CompositeConstructU32x4 => emit_composite_construct(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U32x4,
            "uvec4",
            4,
        ),
        Opcode::CompositeExtractU32x2
        | Opcode::CompositeExtractU32x3
        | Opcode::CompositeExtractU32x4 => {
            emit_composite_extract(ctx, program, inst_ref, &inst_snapshot, GlslVarType::U32)
        }
        Opcode::CompositeConstructF32x2 => emit_composite_construct(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F32x2,
            "vec2",
            2,
        ),
        Opcode::CompositeConstructF32x3 => emit_composite_construct(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F32x3,
            "vec3",
            3,
        ),
        Opcode::CompositeConstructF32x4 => emit_composite_construct(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F32x4,
            "vec4",
            4,
        ),
        Opcode::CompositeExtractF32x2
        | Opcode::CompositeExtractF32x3
        | Opcode::CompositeExtractF32x4 => {
            emit_composite_extract(ctx, program, inst_ref, &inst_snapshot, GlslVarType::F32)
        }

        // Comparisons - float
        Opcode::FPOrdLessThan32 => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("{}<{}", a, b),
        ),
        Opcode::FPOrdEqual32 => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("{}=={}", a, b),
        ),
        Opcode::FPOrdLessThanEqual32 => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("{}<={}", a, b),
        ),
        Opcode::FPOrdGreaterThan32 => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("{}>{}", a, b),
        ),
        Opcode::FPOrdGreaterThanEqual32 => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("{}>={}", a, b),
        ),
        Opcode::FPOrdNotEqual32 => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("{}!={}", a, b),
        ),
        Opcode::FPUnordLessThan32 => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("!({}>={})", a, b),
        ),
        Opcode::FPUnordEqual32 => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("!({}!={})", a, b),
        ),
        Opcode::FPUnordLessThanEqual32 => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("!({}>{})", a, b),
        ),
        Opcode::FPUnordGreaterThan32 => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("!({}<={})", a, b),
        ),
        Opcode::FPUnordGreaterThanEqual32 => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("!({}<{})", a, b),
        ),
        Opcode::FPUnordNotEqual32 => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("!({}=={})", a, b),
        ),
        Opcode::FPIsNan32 => emit_unary_call(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            "isnan",
        ),

        // Comparisons - integer
        Opcode::SLessThan => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("int({})<int({})", a, b),
        ),
        Opcode::ULessThan => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("{}<{}", a, b),
        ),
        Opcode::IEqual => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("{}=={}", a, b),
        ),
        Opcode::SLessThanEqual => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("int({})<=int({})", a, b),
        ),
        Opcode::ULessThanEqual => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("{}<={}", a, b),
        ),
        Opcode::SGreaterThan => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("int({})>int({})", a, b),
        ),
        Opcode::UGreaterThan => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("{}>{}", a, b),
        ),
        Opcode::INotEqual => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("{}!={}", a, b),
        ),
        Opcode::SGreaterThanEqual => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("int({})>=int({})", a, b),
        ),
        Opcode::UGreaterThanEqual => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("{}>={}", a, b),
        ),

        // Logical
        Opcode::LogicalOr => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("{}||{}", a, b),
        ),
        Opcode::LogicalAnd => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("{}&&{}", a, b),
        ),
        Opcode::LogicalXor => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("{}^^{}", a, b),
        ),
        Opcode::LogicalNot => {
            emit_unary_prefix(ctx, program, inst_ref, &inst_snapshot, GlslVarType::U1, "!")
        }

        // Select
        Opcode::SelectU32 => emit_select(ctx, program, inst_ref, &inst_snapshot, GlslVarType::U32),
        Opcode::SelectF32 => emit_select(ctx, program, inst_ref, &inst_snapshot, GlslVarType::F32),

        // Undefined
        Opcode::UndefU1 => add_assign(ctx, program, inst_ref, GlslVarType::U1, "false".to_string()),
        Opcode::UndefU8 | Opcode::UndefU16 | Opcode::UndefU32 => {
            add_assign(ctx, program, inst_ref, GlslVarType::U32, "0u".to_string())
        }

        // Barriers
        Opcode::Barrier => ctx.add_line("barrier();"),
        Opcode::WorkgroupMemoryBarrier => ctx.add_line("groupMemoryBarrier();"),
        Opcode::DeviceMemoryBarrier => ctx.add_line("memoryBarrier();"),

        // Control flow
        Opcode::DemoteToHelperInvocation => ctx.add_line("discard;"),

        // Bitwise conversion
        Opcode::BitCastU32F32 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U32,
            |v| format!("floatBitsToUint({})", v),
        ),
        Opcode::BitCastF32U32 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F32,
            |v| format!("uintBitsToFloat({})", v),
        ),
        Opcode::Identity => {}
        Opcode::BitCastU16F16 | Opcode::BitCastF16U16 => {}
        Opcode::BitCastU64F64 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U64,
            |v| format!("doubleBitsToUint64({})", v),
        ),
        Opcode::BitCastF64U64 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F64,
            |v| format!("uint64BitsToDouble({})", v),
        ),

        // Pack/unpack
        Opcode::PackHalf2x16 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U32,
            |v| format!("packHalf2x16({})", v),
        ),
        Opcode::UnpackHalf2x16 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F32x2,
            |v| format!("unpackHalf2x16({})", v),
        ),

        // Conversion
        Opcode::ConvertF32S32 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F32,
            |v| format!("float(int({}))", v),
        ),
        Opcode::ConvertF32U32 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F32,
            |v| format!("float({})", v),
        ),
        Opcode::ConvertS32F32 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U32,
            |v| format!("uint(int({}))", v),
        ),
        Opcode::ConvertU32F32 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U32,
            |v| format!("uint({})", v),
        ),
        Opcode::ConvertF32F64 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F32,
            |v| format!("float({})", v),
        ),
        Opcode::ConvertF64F32 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F64,
            |v| format!("double({})", v),
        ),

        // Context getters/setters
        Opcode::GetCbufU8
        | Opcode::GetCbufS8
        | Opcode::GetCbufU16
        | Opcode::GetCbufS16
        | Opcode::GetCbufU32
        | Opcode::GetCbufF32 => {
            emit_glsl_context_get_set::emit_get_cbuf(
                ctx,
                program,
                inst_ref,
                inst_snapshot.opcode,
                &inst_snapshot.args[0],
                &inst_snapshot.args[1],
            );
        }
        Opcode::GetAttribute => {
            let attr = inst_snapshot.args[0].attribute();
            let vertex = ctx.var_alloc.consume(program, &inst_snapshot.args[1]);
            emit_glsl_context_get_set::emit_get_attribute(ctx, program, inst_ref, attr, &vertex);
        }
        Opcode::GetAttributeU32 => {
            let attr = inst_snapshot.args[0].attribute();
            emit_glsl_context_get_set::emit_get_attribute_u32(ctx, program, inst_ref, attr);
        }
        Opcode::SetAttribute => {
            let attr = inst_snapshot.args[0].attribute();
            let value = ctx.var_alloc.consume(program, &inst_snapshot.args[1]);
            emit_glsl_context_get_set::emit_set_attribute(ctx, attr.0, &value);
        }
        Opcode::SetFragColor => {
            let render_target = inst_snapshot.args[0].imm_u32();
            let component = inst_snapshot.args[1].imm_u32();
            let value = ctx.var_alloc.consume(program, &inst_snapshot.args[2]);
            emit_glsl_context_get_set::emit_set_frag_color(ctx, render_target, component, &value);
        }
        Opcode::ImageSampleImplicitLod => {
            crate::backend::glsl::emit_glsl_image::emit_image_sample_implicit_lod_inst(
                ctx,
                program,
                inst_ref,
                &inst_snapshot,
            );
        }

        // Special
        Opcode::Prologue | Opcode::Epilogue | Opcode::Void | Opcode::Phi => {}
        Opcode::PhiMove => emit_phi_move(ctx, program, &inst_snapshot),
        Opcode::Reference => {
            let _ = ctx.var_alloc.consume(program, &inst_snapshot.args[0]);
        }
        Opcode::EmitVertex => ctx.add_line("EmitVertex();"),
        Opcode::EndPrimitive => ctx.add_line("EndPrimitive();"),

        // Everything else
        _ => {
            ctx.add_fmt(format!(
                "// {} (not yet emitted)",
                inst_snapshot.opcode.name()
            ));
        }
    }
}

fn precolor(program: &mut ir::Program) {
    let block_count = program.blocks.len() as u32;
    for block_index in 0..block_count {
        let phi_indices: Vec<u32> = program
            .block(block_index)
            .indexed_iter()
            .filter_map(|(index, inst)| (inst.opcode == Opcode::Phi).then_some(index))
            .collect();
        for phi_index in phi_indices {
            let phi_ref = InstRef {
                block: block_index,
                inst: phi_index,
            };
            let phi_args = program.block(block_index).inst(phi_index).phi_args.clone();
            for (pred, value) in phi_args {
                program.block_mut(pred).append_inst(Inst::new(
                    Opcode::PhiMove,
                    vec![Value::Inst(phi_ref), value],
                ));
                program
                    .block_mut(pred)
                    .append_inst(Inst::new(Opcode::Reference, vec![Value::Inst(phi_ref)]));
            }
        }
    }
}

fn emit_phi_move(ctx: &mut EmitContext, program: &mut ir::Program, inst: &Inst) {
    let phi_ref = inst.args[0].inst_ref();
    let needs_define = {
        let phi = program.block(phi_ref.block).inst(phi_ref.inst);
        super::var_alloc::Id {
            raw: phi.definition,
        }
        .is_valid()
            == false
    };
    if needs_define {
        let phi_type = {
            let phi = program.block(phi_ref.block).inst(phi_ref.inst);
            type_from_phi_flags(phi.flags)
        };
        ctx.var_alloc
            .phi_define(inst_mut(program, phi_ref), phi_type);
    }
    let phi_reg = {
        let phi = program.block(phi_ref.block).inst(phi_ref.inst);
        ctx.var_alloc.definition_repr(phi)
    };
    let val_reg = ctx.var_alloc.consume(program, &inst.args[1]);
    if phi_reg != val_reg {
        ctx.add_fmt(format!("{}={};", phi_reg, val_reg));
    }
}

fn type_from_phi_flags(flags: u32) -> Type {
    match flags {
        x if x == Type::U1 as u32 => Type::U1,
        x if x == Type::U32 as u32 => Type::U32,
        x if x == Type::F32 as u32 => Type::F32,
        x if x == Type::U64 as u32 => Type::U64,
        x if x == Type::F64 as u32 => Type::F64,
        x if x == Type::U32x2 as u32 => Type::U32x2,
        x if x == Type::F32x2 as u32 => Type::F32x2,
        x if x == Type::U32x3 as u32 => Type::U32x3,
        x if x == Type::F32x3 as u32 => Type::F32x3,
        x if x == Type::U32x4 as u32 => Type::U32x4,
        x if x == Type::F32x4 as u32 => Type::F32x4,
        _ => Type::U32,
    }
}

fn inst_mut<'a>(program: &'a mut ir::Program, inst_ref: InstRef) -> &'a mut Inst {
    program.block_mut(inst_ref.block).inst_mut(inst_ref.inst)
}

fn add_assign(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    ty: GlslVarType,
    expr: String,
) {
    let dst = ctx.var_alloc.add_define(inst_mut(program, inst_ref), ty);
    if dst.is_empty() {
        return;
    }
    ctx.add_fmt(format!("{}={};", dst, expr));
}

fn consume_args(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst: &Inst,
    count: usize,
) -> Vec<String> {
    inst.args
        .iter()
        .take(count)
        .map(|arg| ctx.var_alloc.consume(program, arg))
        .collect()
}

fn emit_unary_expr(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    inst: &Inst,
    ty: GlslVarType,
    f: impl FnOnce(String) -> String,
) {
    let args = consume_args(ctx, program, inst, 1);
    add_assign(ctx, program, inst_ref, ty, f(args[0].clone()));
}

fn emit_unary_call(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    inst: &Inst,
    ty: GlslVarType,
    func: &str,
) {
    emit_unary_expr(ctx, program, inst_ref, inst, ty, |v| {
        format!("{}({})", func, v)
    });
}

fn emit_unary_prefix(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    inst: &Inst,
    ty: GlslVarType,
    op: &str,
) {
    emit_unary_expr(ctx, program, inst_ref, inst, ty, |v| {
        format!("{}({})", op, v)
    });
}

fn emit_binary_expr(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    inst: &Inst,
    ty: GlslVarType,
    f: impl FnOnce(String, String) -> String,
) {
    let args = consume_args(ctx, program, inst, 2);
    add_assign(
        ctx,
        program,
        inst_ref,
        ty,
        f(args[0].clone(), args[1].clone()),
    );
}

fn emit_binary(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    inst: &Inst,
    ty: GlslVarType,
    op: &str,
) {
    emit_binary_expr(ctx, program, inst_ref, inst, ty, |a, b| {
        format!("({}){}({})", a, op, b)
    });
}

fn emit_binary_call(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    inst: &Inst,
    ty: GlslVarType,
    func: &str,
) {
    emit_binary_expr(ctx, program, inst_ref, inst, ty, |a, b| {
        format!("{}({},{})", func, a, b)
    });
}

fn emit_ternary_expr(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    inst: &Inst,
    ty: GlslVarType,
    f: impl FnOnce(String, String, String) -> String,
) {
    let args = consume_args(ctx, program, inst, 3);
    add_assign(
        ctx,
        program,
        inst_ref,
        ty,
        f(args[0].clone(), args[1].clone(), args[2].clone()),
    );
}

fn emit_ternary_call(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    inst: &Inst,
    ty: GlslVarType,
    func: &str,
) {
    emit_ternary_expr(ctx, program, inst_ref, inst, ty, |a, b, c| {
        format!("{}({},{},{})", func, a, b, c)
    });
}

fn emit_composite_construct(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    inst: &Inst,
    ty: GlslVarType,
    constructor: &str,
    count: usize,
) {
    let args = consume_args(ctx, program, inst, count);
    add_assign(
        ctx,
        program,
        inst_ref,
        ty,
        format!("{}({})", constructor, args.join(",")),
    );
}

fn emit_composite_extract(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    inst: &Inst,
    ty: GlslVarType,
) {
    let composite = ctx.var_alloc.consume(program, &inst.args[0]);
    let index = inst.args[1].imm_u32() as usize;
    const SWIZZLE: [&str; 4] = ["x", "y", "z", "w"];
    add_assign(
        ctx,
        program,
        inst_ref,
        ty,
        format!("{}.{}", composite, SWIZZLE[index]),
    );
}

fn emit_quaternary_expr(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    inst: &Inst,
    ty: GlslVarType,
    f: impl FnOnce(String, String, String, String) -> String,
) {
    let args = consume_args(ctx, program, inst, 4);
    add_assign(
        ctx,
        program,
        inst_ref,
        ty,
        f(
            args[0].clone(),
            args[1].clone(),
            args[2].clone(),
            args[3].clone(),
        ),
    );
}

fn emit_select(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    inst: &Inst,
    ty: GlslVarType,
) {
    let args = consume_args(ctx, program, inst, 3);
    add_assign(
        ctx,
        program,
        inst_ref,
        ty,
        format!("{}?{}:{}", args[0], args[1], args[2]),
    );
}
