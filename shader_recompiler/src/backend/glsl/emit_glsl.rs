// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Main GLSL emission entry point.
//!
//! Maps to upstream `backend/glsl/emit_glsl.cpp`.

use crate::ir;
use crate::ir::instruction::Inst;
use crate::ir::opcodes::Opcode;
use crate::ir::program::SyntaxNode;
use crate::ir::types::{FpControl, Type};
use crate::ir::value::{InstRef, Value};

use super::emit_glsl_context_get_set;
use super::emit_glsl_image;
use super::emit_glsl_special;
use super::glsl_emit_context::EmitContext;
use super::var_alloc::GlslVarType;

/// Walk an IR program and emit GLSL code.
pub fn emit_program(ctx: &mut EmitContext, program: &mut ir::Program) {
    precolor(program);
    recompute_emit_use_counts(program);
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
            precise_type(&inst_snapshot, GlslVarType::F32, GlslVarType::PrecF32),
            "+",
        ),
        Opcode::FPMul32 => emit_binary(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            precise_type(&inst_snapshot, GlslVarType::F32, GlslVarType::PrecF32),
            "*",
        ),
        Opcode::FPFma32 => emit_ternary_call(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            precise_type(&inst_snapshot, GlslVarType::F32, GlslVarType::PrecF32),
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

        // ── Composite F16x2/3/4 and F64x2/3/4 (ports of upstream
        // EmitCompositeConstructF16/F64 / EmitCompositeExtractF16/F64)
        // F16 is packed via F16x2 storage in GLSL.
        Opcode::CompositeConstructF16x2 => emit_composite_construct(
            ctx, program, inst_ref, &inst_snapshot, GlslVarType::F16x2, "f16vec2", 2,
        ),
        Opcode::CompositeConstructF16x3 => {
            panic!("CompositeConstructF16x3 not implemented in GLSL backend (upstream NotImplemented)");
        }
        Opcode::CompositeConstructF16x4 => {
            panic!("CompositeConstructF16x4 not implemented in GLSL backend (upstream NotImplemented)");
        }
        Opcode::CompositeExtractF16x2 => {
            emit_composite_extract(ctx, program, inst_ref, &inst_snapshot, GlslVarType::F16x2)
        }
        Opcode::CompositeExtractF16x3 | Opcode::CompositeExtractF16x4 => {
            panic!(
                "CompositeExtractF16x3/x4 not implemented in GLSL backend (upstream NotImplemented)"
            );
        }
        Opcode::CompositeInsertF16x2
        | Opcode::CompositeInsertF16x3
        | Opcode::CompositeInsertF16x4
        | Opcode::CompositeInsertU32x2
        | Opcode::CompositeInsertU32x3
        | Opcode::CompositeInsertU32x4
        | Opcode::CompositeInsertF32x2
        | Opcode::CompositeInsertF32x3
        | Opcode::CompositeInsertF32x4 => {
            // Port of upstream CompositeInsert (backend/glsl/emit_glsl_composite.cpp):
            // value[index] = inserted_value. GLSL handles via .x/.y/.z/.w accessors.
            emit_composite_insert(ctx, program, inst_ref, &inst_snapshot)
        }
        Opcode::CompositeConstructF64x2 => emit_composite_construct(
            ctx, program, inst_ref, &inst_snapshot, GlslVarType::F64, "dvec2", 2,
        ),
        Opcode::CompositeConstructF64x3 => {
            panic!("CompositeConstructF64x3 not implemented in GLSL backend (upstream NotImplemented)");
        }
        Opcode::CompositeConstructF64x4 => {
            panic!("CompositeConstructF64x4 not implemented in GLSL backend (upstream NotImplemented)");
        }
        Opcode::CompositeExtractF64x2
        | Opcode::CompositeExtractF64x3
        | Opcode::CompositeExtractF64x4 => {
            emit_composite_extract(ctx, program, inst_ref, &inst_snapshot, GlslVarType::F64)
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

        // ── FP64 arithmetic ────────────────────────────────────────────
        // Ports of upstream `EmitFP*64` from
        // `backend/glsl/emit_glsl_floating_point.cpp`.
        Opcode::FPAdd64 => emit_binary(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            precise_type(&inst_snapshot, GlslVarType::F64, GlslVarType::PrecF64),
            "+",
        ),
        Opcode::FPMul64 => emit_binary(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            precise_type(&inst_snapshot, GlslVarType::F64, GlslVarType::PrecF64),
            "*",
        ),
        Opcode::FPFma64 => emit_ternary_call(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            precise_type(&inst_snapshot, GlslVarType::F64, GlslVarType::PrecF64),
            "fma",
        ),
        Opcode::FPMax64 => emit_binary_call(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F64,
            "max",
        ),
        Opcode::FPMin64 => emit_binary_call(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F64,
            "min",
        ),
        // Upstream uses `0.f-({})` / `double(0.)-({})` rather than `-({})`
        // to avoid driver-specific NaN handling on the unary minus.
        Opcode::FPNeg64 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F64,
            |v| format!("double(0.)-({})", v),
        ),
        Opcode::FPAbs64 => emit_unary_call(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F64,
            "abs",
        ),
        Opcode::FPRecip64 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F64,
            |v| format!("1.0/{}", v),
        ),
        Opcode::FPSaturate64 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F64,
            |v| format!("min(max({},0.0),1.0)", v),
        ),
        Opcode::FPClamp64 => emit_ternary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F64,
            |v, lo, hi| {
                format!("min(max({},double({})),double({}))", v, lo, hi)
            },
        ),
        Opcode::FPRoundEven64 => emit_unary_call(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F64,
            "roundEven",
        ),
        Opcode::FPFloor64 => emit_unary_call(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F64,
            "floor",
        ),
        Opcode::FPCeil64 => emit_unary_call(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F64,
            "ceil",
        ),
        Opcode::FPTrunc64 => emit_unary_call(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F64,
            "trunc",
        ),
        Opcode::FPIsNan64 => emit_unary_call(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            "isnan",
        ),

        // ── FP64 comparisons ───────────────────────────────────────────
        Opcode::FPOrdLessThan64 => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("{}<{}", a, b),
        ),
        Opcode::FPOrdEqual64 => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("{}=={}", a, b),
        ),
        Opcode::FPOrdLessThanEqual64 => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("{}<={}", a, b),
        ),
        Opcode::FPOrdGreaterThan64 => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("{}>{}", a, b),
        ),
        Opcode::FPOrdGreaterThanEqual64 => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("{}>={}", a, b),
        ),
        Opcode::FPOrdNotEqual64 => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("{}!={}", a, b),
        ),
        Opcode::FPUnordLessThan64 => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("({}<{})||isnan({})||isnan({})", a, b, a, b),
        ),
        Opcode::FPUnordEqual64 => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("({}=={})||isnan({})||isnan({})", a, b, a, b),
        ),
        Opcode::FPUnordLessThanEqual64 => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("({}<={})||isnan({})||isnan({})", a, b, a, b),
        ),
        Opcode::FPUnordGreaterThan64 => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("({}>{})||isnan({})||isnan({})", a, b, a, b),
        ),
        Opcode::FPUnordGreaterThanEqual64 => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("({}>={})||isnan({})||isnan({})", a, b, a, b),
        ),
        Opcode::FPUnordNotEqual64 => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U1,
            |a, b| format!("({}!={})||isnan({})||isnan({})", a, b, a, b),
        ),

        // ── FP16 (all `NotImplemented` in upstream) ───────────────────
        // Upstream `EmitFP{Add,Mul,Fma,Neg,Abs,Saturate,Clamp,RoundEven,
        // Floor,Ceil,Trunc}16` and `EmitFP{Recip,RecipSqrt,Sqrt}16`
        // throw `NotImplemented()`. We panic with a parity-faithful
        // message so the wrong path crashes loudly instead of silently
        // emitting wrong GLSL.
        Opcode::FPAdd16
        | Opcode::FPMul16
        | Opcode::FPFma16
        | Opcode::FPNeg16
        | Opcode::FPAbs16
        | Opcode::FPSaturate16
        | Opcode::FPClamp16
        | Opcode::FPRoundEven16
        | Opcode::FPFloor16
        | Opcode::FPCeil16
        | Opcode::FPTrunc16
        | Opcode::FPRecipSqrt64
        | Opcode::FPSqrt64
        | Opcode::FPIsNan16
        | Opcode::FPOrdEqual16
        | Opcode::FPOrdNotEqual16
        | Opcode::FPOrdLessThan16
        | Opcode::FPOrdLessThanEqual16
        | Opcode::FPOrdGreaterThan16
        | Opcode::FPOrdGreaterThanEqual16
        | Opcode::FPUnordEqual16
        | Opcode::FPUnordNotEqual16
        | Opcode::FPUnordLessThan16
        | Opcode::FPUnordLessThanEqual16
        | Opcode::FPUnordGreaterThan16
        | Opcode::FPUnordGreaterThanEqual16 => {
            panic!(
                "FP16 opcode {:?} not implemented in GLSL backend (upstream throws NotImplemented)",
                inst_snapshot.opcode
            );
        }

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

        // Select — port of upstream EmitSelect{U1,U8,U16,U32,U64,F16,F32,F64}.
        Opcode::SelectU1 => emit_select(ctx, program, inst_ref, &inst_snapshot, GlslVarType::U1),
        Opcode::SelectU8 | Opcode::SelectU16 => {
            panic!(
                "{:?} not implemented in GLSL backend (upstream NotImplemented)",
                inst_snapshot.opcode
            );
        }
        Opcode::SelectU32 => emit_select(ctx, program, inst_ref, &inst_snapshot, GlslVarType::U32),
        Opcode::SelectU64 => emit_select(ctx, program, inst_ref, &inst_snapshot, GlslVarType::U64),
        Opcode::SelectF16 => {
            panic!("SelectF16 not implemented in GLSL backend (upstream NotImplemented)");
        }
        Opcode::SelectF32 => emit_select(ctx, program, inst_ref, &inst_snapshot, GlslVarType::F32),
        Opcode::SelectF64 => emit_select(ctx, program, inst_ref, &inst_snapshot, GlslVarType::F64),

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
        Opcode::ConvertF16F32 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F16x2,
            |v| format!("float16BitsToUint16(float16_t({}))", v),
        ),
        Opcode::ConvertF32F16 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F32,
            |v| format!("float(uint16BitsToFloat16(uint16_t({})))", v),
        ),
        // ── Additional Convert*F* / F*<-Int ports (upstream
        // backend/glsl/emit_glsl_convert.cpp). U16/F16/U8 paths panic
        // exactly like upstream (`NotImplemented()`).
        Opcode::ConvertS32F64 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U32,
            |v| format!("uint(int({}))", v),
        ),
        Opcode::ConvertS64F32 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U64,
            |v| format!("uint64_t(int64_t({}))", v),
        ),
        Opcode::ConvertS64F64 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U64,
            |v| format!("uint64_t(int64_t({}))", v),
        ),
        Opcode::ConvertU32F64 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U32,
            |v| format!("uint({})", v),
        ),
        Opcode::ConvertU64F32 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U64,
            |v| format!("uint64_t({})", v),
        ),
        Opcode::ConvertU64F64 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U64,
            |v| format!("uint64_t({})", v),
        ),
        // F16-source converts: upstream throws NotImplemented for ALL of
        // S16F16/S32F16/S64F16/U16F16/U32F16/U64F16. Match parity.
        Opcode::ConvertS16F16
        | Opcode::ConvertS32F16
        | Opcode::ConvertU16F16
        | Opcode::ConvertU32F16 => {
            panic!(
                "Convert from F16 ({:?}) not implemented in GLSL backend (upstream NotImplemented)",
                inst_snapshot.opcode
            );
        }

        // Context getters/setters
        Opcode::GetCbufU8
        | Opcode::GetCbufS8
        | Opcode::GetCbufU16
        | Opcode::GetCbufS16
        | Opcode::GetCbufU32
        | Opcode::GetCbufF32
        | Opcode::GetCbufU32x2 => {
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
        // Image / texture sampling — ports of upstream
        // `EmitImage*` (backend/glsl/emit_glsl_image.cpp).
        Opcode::ImageSampleImplicitLod => {
            emit_glsl_image::emit_image_sample_implicit_lod_inst(
                ctx, program, inst_ref, &inst_snapshot,
            );
        }
        Opcode::ImageSampleExplicitLod => {
            emit_glsl_image::emit_image_sample_explicit_lod_inst(
                ctx, program, inst_ref, &inst_snapshot,
            );
        }
        Opcode::ImageSampleDrefImplicitLod => {
            emit_glsl_image::emit_image_sample_dref_implicit_lod_inst(
                ctx, program, inst_ref, &inst_snapshot,
            );
        }
        Opcode::ImageSampleDrefExplicitLod => {
            emit_glsl_image::emit_image_sample_dref_explicit_lod_inst(
                ctx, program, inst_ref, &inst_snapshot,
            );
        }
        Opcode::ImageGather => {
            emit_glsl_image::emit_image_gather_inst(ctx, program, inst_ref, &inst_snapshot);
        }
        Opcode::ImageGatherDref => {
            emit_glsl_image::emit_image_gather_dref_inst(ctx, program, inst_ref, &inst_snapshot);
        }
        Opcode::ImageFetch => {
            emit_glsl_image::emit_image_fetch_inst(ctx, program, inst_ref, &inst_snapshot);
        }
        Opcode::ImageQueryDimensions => {
            emit_glsl_image::emit_image_query_dimensions_inst(
                ctx, program, inst_ref, &inst_snapshot,
            );
        }
        Opcode::ImageQueryLod => {
            emit_glsl_image::emit_image_query_lod_inst(ctx, program, inst_ref, &inst_snapshot);
        }
        Opcode::ImageGradient => {
            emit_glsl_image::emit_image_gradient_inst(ctx, program, inst_ref, &inst_snapshot);
        }
        Opcode::ImageRead => {
            emit_glsl_image::emit_image_read_inst(ctx, program, inst_ref, &inst_snapshot);
        }
        Opcode::ImageWrite => {
            emit_glsl_image::emit_image_write_inst(ctx, program, inst_ref, &inst_snapshot);
        }
        Opcode::ImageAtomicIAdd32 => {
            emit_glsl_image::emit_image_atomic_iadd32_inst(
                ctx, program, inst_ref, &inst_snapshot,
            );
        }
        Opcode::ImageAtomicSMin32 => {
            emit_glsl_image::emit_image_atomic_smin32_inst(
                ctx, program, inst_ref, &inst_snapshot,
            );
        }
        Opcode::ImageAtomicUMin32 => {
            emit_glsl_image::emit_image_atomic_umin32_inst(
                ctx, program, inst_ref, &inst_snapshot,
            );
        }
        Opcode::ImageAtomicSMax32 => {
            emit_glsl_image::emit_image_atomic_smax32_inst(
                ctx, program, inst_ref, &inst_snapshot,
            );
        }
        Opcode::ImageAtomicUMax32 => {
            emit_glsl_image::emit_image_atomic_umax32_inst(
                ctx, program, inst_ref, &inst_snapshot,
            );
        }
        Opcode::ImageAtomicInc32 => {
            emit_glsl_image::emit_image_atomic_inc32_inst(
                ctx, program, inst_ref, &inst_snapshot,
            );
        }
        Opcode::ImageAtomicDec32 => {
            emit_glsl_image::emit_image_atomic_dec32_inst(
                ctx, program, inst_ref, &inst_snapshot,
            );
        }
        Opcode::ImageAtomicAnd32 => {
            emit_glsl_image::emit_image_atomic_and32_inst(
                ctx, program, inst_ref, &inst_snapshot,
            );
        }
        Opcode::ImageAtomicOr32 => {
            emit_glsl_image::emit_image_atomic_or32_inst(
                ctx, program, inst_ref, &inst_snapshot,
            );
        }
        Opcode::ImageAtomicXor32 => {
            emit_glsl_image::emit_image_atomic_xor32_inst(
                ctx, program, inst_ref, &inst_snapshot,
            );
        }
        Opcode::ImageAtomicExchange32 => {
            emit_glsl_image::emit_image_atomic_exchange32_inst(
                ctx, program, inst_ref, &inst_snapshot,
            );
        }

        // Special
        Opcode::Prologue => emit_glsl_special::emit_prologue(ctx),
        Opcode::Epilogue | Opcode::Void | Opcode::Phi => {}
        Opcode::PhiMove => emit_phi_move(ctx, program, &inst_snapshot),
        Opcode::Reference => {
            let _ = ctx.var_alloc.consume(program, &inst_snapshot.args[0]);
        }
        Opcode::EmitVertex => ctx.add_line("EmitVertex();"),
        Opcode::EndPrimitive => ctx.add_line("EndPrimitive();"),

        // ── Bound/Bindless image fold guards ──────────────────────────
        // Upstream all `NotImplemented()` because an earlier IR pass
        // folds these into the non-prefixed `Image*` variants. Direct
        // dispatch here would indicate a pass-ordering bug.
        Opcode::BoundImageSampleImplicitLod
        | Opcode::BoundImageSampleExplicitLod
        | Opcode::BoundImageSampleDrefImplicitLod
        | Opcode::BoundImageSampleDrefExplicitLod
        | Opcode::BoundImageGather
        | Opcode::BoundImageGatherDref
        | Opcode::BoundImageFetch
        | Opcode::BoundImageQueryDimensions
        | Opcode::BoundImageQueryLod
        | Opcode::BoundImageGradient
        | Opcode::BoundImageRead
        | Opcode::BoundImageWrite
        | Opcode::BoundImageAtomicIAdd32
        | Opcode::BoundImageAtomicSMin32
        | Opcode::BoundImageAtomicUMin32
        | Opcode::BoundImageAtomicSMax32
        | Opcode::BoundImageAtomicUMax32
        | Opcode::BoundImageAtomicInc32
        | Opcode::BoundImageAtomicDec32
        | Opcode::BoundImageAtomicAnd32
        | Opcode::BoundImageAtomicOr32
        | Opcode::BoundImageAtomicXor32
        | Opcode::BoundImageAtomicExchange32
        | Opcode::BindlessImageSampleImplicitLod
        | Opcode::BindlessImageSampleExplicitLod
        | Opcode::BindlessImageSampleDrefImplicitLod
        | Opcode::BindlessImageSampleDrefExplicitLod
        | Opcode::BindlessImageGather
        | Opcode::BindlessImageGatherDref
        | Opcode::BindlessImageFetch
        | Opcode::BindlessImageQueryDimensions
        | Opcode::BindlessImageQueryLod
        | Opcode::BindlessImageGradient
        | Opcode::BindlessImageRead
        | Opcode::BindlessImageWrite
        | Opcode::BindlessImageAtomicIAdd32
        | Opcode::BindlessImageAtomicSMin32
        | Opcode::BindlessImageAtomicUMin32
        | Opcode::BindlessImageAtomicSMax32
        | Opcode::BindlessImageAtomicUMax32
        | Opcode::BindlessImageAtomicInc32
        | Opcode::BindlessImageAtomicDec32
        | Opcode::BindlessImageAtomicAnd32
        | Opcode::BindlessImageAtomicOr32
        | Opcode::BindlessImageAtomicXor32
        | Opcode::BindlessImageAtomicExchange32 => {
            panic!(
                "Bound/Bindless image opcode {:?} reached GLSL backend; \
                 expected to be folded to Image* by an earlier IR pass",
                inst_snapshot.opcode
            );
        }

        // ── F16-destination converts — all `NotImplemented()` upstream
        Opcode::ConvertF16S8
        | Opcode::ConvertF16S16
        | Opcode::ConvertF16S32
        | Opcode::ConvertF16S64
        | Opcode::ConvertF16U8
        | Opcode::ConvertF16U16
        | Opcode::ConvertF16U32
        | Opcode::ConvertF16U64 => {
            panic!(
                "Convert to F16 ({:?}) not implemented in GLSL backend (upstream NotImplemented)",
                inst_snapshot.opcode
            );
        }

        // ── F32 from integer (signed/unsigned 8/16/64) ────────────────
        // Upstream: F32S8/F32U8 throw NotImplemented; F32U16 emits
        // `float({}&0xffff)`; F32S64/F32U64 emit `float(int64_t({}))` /
        // `float({})`.
        Opcode::ConvertF32S8 | Opcode::ConvertF32U8 => {
            panic!(
                "{:?} not implemented in GLSL backend (upstream NotImplemented)",
                inst_snapshot.opcode
            );
        }
        Opcode::ConvertF32S16 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F32,
            |v| format!("float(int({}&0xffff))", v),
        ),
        Opcode::ConvertF32U16 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F32,
            |v| format!("float({}&0xffff)", v),
        ),
        Opcode::ConvertF32S64 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F32,
            |v| format!("float(int64_t({}))", v),
        ),
        Opcode::ConvertF32U64 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F32,
            |v| format!("float({})", v),
        ),
        // ── F64 from integer ──────────────────────────────────────────
        Opcode::ConvertF64S8 | Opcode::ConvertF64U8 | Opcode::ConvertF64S16 | Opcode::ConvertF64U16 => {
            panic!(
                "{:?} not implemented in GLSL backend (upstream NotImplemented)",
                inst_snapshot.opcode
            );
        }
        Opcode::ConvertF64S32 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F64,
            |v| format!("double(int({}))", v),
        ),
        Opcode::ConvertF64S64 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F64,
            |v| format!("double(int64_t({}))", v),
        ),
        Opcode::ConvertF64U32 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F64,
            |v| format!("double({})", v),
        ),
        Opcode::ConvertF64U64 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F64,
            |v| format!("double({})", v),
        ),

        // ── 64-bit integer arithmetic ─────────────────────────────────
        Opcode::IAdd64 => emit_binary(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U64,
            "+",
        ),
        Opcode::ISub64 => emit_binary(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U64,
            "-",
        ),
        Opcode::INeg64 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U64,
            |v| format!("uint64_t(-int64_t({}))", v),
        ),
        Opcode::IAbs64 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U64,
            |v| format!("uint64_t(abs(int64_t({})))", v),
        ),
        Opcode::ShiftLeftLogical64 => emit_binary(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U64,
            "<<",
        ),
        Opcode::ShiftRightLogical64 => emit_binary(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U64,
            ">>",
        ),
        Opcode::ShiftRightArithmetic64 => emit_binary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U64,
            |a, b| format!("uint64_t(int64_t({})>>int64_t({}))", a, b),
        ),

        // ── Pack/Unpack ───────────────────────────────────────────────
        Opcode::PackFloat2x16 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U32,
            |v| format!("packFloat2x16({})", v),
        ),
        Opcode::UnpackFloat2x16 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F16x2,
            |v| format!("unpackFloat2x16({})", v),
        ),
        Opcode::PackUint2x32 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U64,
            |v| format!("packUint2x32({})", v),
        ),
        Opcode::UnpackUint2x32 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U32x2,
            |v| format!("unpackUint2x32({})", v),
        ),
        Opcode::PackDouble2x32 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F64,
            |v| format!("packDouble2x32({})", v),
        ),
        Opcode::UnpackDouble2x32 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U32x2,
            |v| format!("unpackDouble2x32({})", v),
        ),

        // ── FP misc ───────────────────────────────────────────────────
        Opcode::FPClamp32 => emit_ternary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F32,
            |v, lo, hi| format!("min(max({},float({})),float({}))", v, lo, hi),
        ),
        Opcode::FPSub32 => emit_binary(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F32,
            "-",
        ),
        Opcode::FPSub64 => emit_binary(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F64,
            "-",
        ),
        Opcode::FPDiv32 => emit_binary(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F32,
            "/",
        ),
        Opcode::FPDiv64 => emit_binary(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::F64,
            "/",
        ),
        Opcode::FPMin16 | Opcode::FPMax16 => {
            panic!(
                "{:?} not implemented in GLSL backend (upstream NotImplemented for FP16)",
                inst_snapshot.opcode
            );
        }
        Opcode::FindUMsb32 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U32,
            |v| format!("findMSB({})", v),
        ),
        Opcode::FindSMsb32 => emit_unary_expr(
            ctx,
            program,
            inst_ref,
            &inst_snapshot,
            GlslVarType::U32,
            |v| format!("findMSB(int({}))", v),
        ),

        // ── Atomic ops on shared / global / storage buffers ────────────
        // Each atomic emits `dst=imageAtomicOp(buf, offset, value);`-like
        // pattern. Upstream `emit_glsl_atomic.cpp` keeps separate impls
        // per buffer/source; the GLSL functions used are the same
        // (`atomicAdd`, `atomicMin`, etc.) so we factor through a
        // shared helper.
        Opcode::SharedAtomicIAdd32 => emit_shared_atomic_binop(
            ctx, program, inst_ref, &inst_snapshot, "atomicAdd", None,
        ),
        Opcode::SharedAtomicSMin32 => emit_shared_atomic_binop(
            ctx, program, inst_ref, &inst_snapshot, "atomicMin", Some("int"),
        ),
        Opcode::SharedAtomicUMin32 => emit_shared_atomic_binop(
            ctx, program, inst_ref, &inst_snapshot, "atomicMin", Some("uint"),
        ),
        Opcode::SharedAtomicSMax32 => emit_shared_atomic_binop(
            ctx, program, inst_ref, &inst_snapshot, "atomicMax", Some("int"),
        ),
        Opcode::SharedAtomicUMax32 => emit_shared_atomic_binop(
            ctx, program, inst_ref, &inst_snapshot, "atomicMax", Some("uint"),
        ),
        Opcode::SharedAtomicAnd32 => emit_shared_atomic_binop(
            ctx, program, inst_ref, &inst_snapshot, "atomicAnd", None,
        ),
        Opcode::SharedAtomicOr32 => emit_shared_atomic_binop(
            ctx, program, inst_ref, &inst_snapshot, "atomicOr", None,
        ),
        Opcode::SharedAtomicXor32 => emit_shared_atomic_binop(
            ctx, program, inst_ref, &inst_snapshot, "atomicXor", None,
        ),
        Opcode::SharedAtomicExchange32 => emit_shared_atomic_binop(
            ctx, program, inst_ref, &inst_snapshot, "atomicExchange", None,
        ),
        // ── Global atomics — upstream throws NotImplemented for all ───
        // (see upstream `EmitGlobalAtomic*` — all `throw NotImplementedException`).
        Opcode::GlobalAtomicIAdd32
        | Opcode::GlobalAtomicSMin32
        | Opcode::GlobalAtomicUMin32
        | Opcode::GlobalAtomicSMax32
        | Opcode::GlobalAtomicUMax32
        | Opcode::GlobalAtomicAnd32
        | Opcode::GlobalAtomicOr32
        | Opcode::GlobalAtomicXor32
        | Opcode::GlobalAtomicExchange32 => {
            panic!(
                "Global atomic {:?} not implemented in GLSL backend (upstream NotImplemented)",
                inst_snapshot.opcode
            );
        }

        // ── Storage atomics ───────────────────────────────────────────
        // Port of upstream `EmitStorageAtomic*` (emit_glsl_atomic.cpp).
        // Pattern: `dst = atomicOp({stage}_ssbo{binding}[offset>>2], value);`.
        // SMin/SMax need a CasFunction in upstream because GLSL's
        // atomicMin/Max are unsigned-only — we still panic for those
        // because the CAS helper isn't ported (matches the upstream
        // codepath that requires it).
        Opcode::StorageAtomicIAdd32 => {
            emit_storage_atomic_native(ctx, program, inst_ref, &inst_snapshot, "atomicAdd", None);
        }
        Opcode::StorageAtomicUMin32 => {
            emit_storage_atomic_native(ctx, program, inst_ref, &inst_snapshot, "atomicMin", None);
        }
        Opcode::StorageAtomicUMax32 => {
            emit_storage_atomic_native(ctx, program, inst_ref, &inst_snapshot, "atomicMax", None);
        }
        Opcode::StorageAtomicAnd32 => {
            emit_storage_atomic_native(ctx, program, inst_ref, &inst_snapshot, "atomicAnd", None);
        }
        Opcode::StorageAtomicOr32 => {
            emit_storage_atomic_native(ctx, program, inst_ref, &inst_snapshot, "atomicOr", None);
        }
        Opcode::StorageAtomicXor32 => {
            emit_storage_atomic_native(ctx, program, inst_ref, &inst_snapshot, "atomicXor", None);
        }
        Opcode::StorageAtomicExchange32 => {
            emit_storage_atomic_native(
                ctx, program, inst_ref, &inst_snapshot, "atomicExchange", None,
            );
        }
        Opcode::StorageAtomicSMin32 | Opcode::StorageAtomicSMax32 => {
            // Upstream uses `SsboCasFunction(ctx, inst, binding, offset,
            // uint(value), "CasMinS32" | "CasMaxS32")` — a CAS-loop
            // helper that emulates signed min/max via unsigned atomics.
            // The CAS helper isn't ported; panic to mirror "missing".
            panic!(
                "Storage atomic {:?} requires CAS helper (not ported)",
                inst_snapshot.opcode
            );
        }

        // ── Load/Store global / local / shared / storage ──────────────
        // Upstream `emit_glsl_memory.cpp` and shared/storage variants.
        // Local has its own emit; Shared U32 emit; Storage 32/64/128 emit.
        // For ports we route through `consume + add_fmt` patterns
        // matching upstream's `ctx.AddU32("{}=u32[{}>>2];", inst, ofs)` etc.
        // ── Shared memory load/store (1-arg / 2-arg) ──────────────────
        // Port of upstream `EmitLoadShared*` / `EmitWriteShared*`
        // (emit_glsl_shared_memory.cpp). Pattern:
        //   load_u8:  `dst = bitfieldExtract(smem[ofs>>2],int(ofs%4)*8,8);`
        //   load_s8:  `dst = bitfieldExtract(int(smem[ofs>>2]),int(ofs%4)*8,8);`
        //   load_u16: `dst = bitfieldExtract(smem[ofs>>2],int((ofs>>1)%2)*16,16);`
        //   load_s16: `dst = bitfieldExtract(int(smem[ofs>>2]),int((ofs>>1)%2)*16,16);`
        //   load_32:  `dst = smem[ofs>>2];`
        //   load_64:  `dst = packUint2x32(uvec2(smem[ofs>>2],smem[(ofs>>2)+1]));`
        //   load_128: `dst = uvec4(smem[ofs>>2],smem[(ofs>>2)+1],smem[(ofs>>2)+2],smem[(ofs>>2)+3]);`
        Opcode::LoadSharedU8 => emit_load_shared_8(ctx, program, inst_ref, &inst_snapshot, false),
        Opcode::LoadSharedS8 => emit_load_shared_8(ctx, program, inst_ref, &inst_snapshot, true),
        Opcode::LoadSharedU16 => emit_load_shared_16(ctx, program, inst_ref, &inst_snapshot, false),
        Opcode::LoadSharedS16 => emit_load_shared_16(ctx, program, inst_ref, &inst_snapshot, true),
        Opcode::LoadSharedU32 => emit_load_shared_u32(ctx, program, inst_ref, &inst_snapshot),
        Opcode::LoadSharedU64 => emit_load_shared_u64(ctx, program, inst_ref, &inst_snapshot),
        Opcode::LoadSharedU128 => emit_load_shared_u128(ctx, program, inst_ref, &inst_snapshot),
        Opcode::WriteSharedU8 => emit_write_shared_8(ctx, program, &inst_snapshot),
        Opcode::WriteSharedU16 => emit_write_shared_16(ctx, program, &inst_snapshot),
        Opcode::WriteSharedU32 => emit_write_shared_u32(ctx, program, &inst_snapshot),
        Opcode::WriteSharedU64 => emit_write_shared_u64(ctx, program, &inst_snapshot),
        Opcode::WriteSharedU128 => emit_write_shared_u128(ctx, program, &inst_snapshot),

        // ── Storage SSBO load/store ───────────────────────────────────
        // Port of upstream `EmitLoadStorage*` / `EmitWriteStorage*`
        // (emit_glsl_memory.cpp). Format: `{stage}_ssbo{binding}[offset>>2]`.
        Opcode::LoadStorageU8 => emit_load_storage_8(ctx, program, inst_ref, &inst_snapshot, false),
        Opcode::LoadStorageS8 => emit_load_storage_8(ctx, program, inst_ref, &inst_snapshot, true),
        Opcode::LoadStorageU16 => emit_load_storage_16(ctx, program, inst_ref, &inst_snapshot, false),
        Opcode::LoadStorageS16 => emit_load_storage_16(ctx, program, inst_ref, &inst_snapshot, true),
        Opcode::LoadStorage32 => emit_load_storage_32(ctx, program, inst_ref, &inst_snapshot),
        Opcode::LoadStorage64 => emit_load_storage_64(ctx, program, inst_ref, &inst_snapshot),
        Opcode::LoadStorage128 => emit_load_storage_128(ctx, program, inst_ref, &inst_snapshot),
        Opcode::WriteStorage32 => emit_write_storage_32(ctx, program, &inst_snapshot),
        Opcode::WriteStorage64 => emit_write_storage_64(ctx, program, &inst_snapshot),
        Opcode::WriteStorage128 => emit_write_storage_128(ctx, program, &inst_snapshot),
        Opcode::WriteStorageS8 | Opcode::WriteStorageU8 | Opcode::WriteStorageS16 | Opcode::WriteStorageU16 => {
            // Upstream `EmitWriteStorageU8/S8/U16/S16` are NotImplemented.
            panic!(
                "Storage write op {:?} not implemented (upstream NotImplemented)",
                inst_snapshot.opcode
            );
        }

        // ── Global memory load/store (require int64 + GLSL helper decl) ─
        // Port of upstream `EmitLoadGlobal32/64/128` and `EmitWriteGlobal*`.
        // Upstream calls a `LoadGlobal32(address)` / `WriteGlobal32(...)`
        // GLSL helper declared in the shader header (gated on
        // `profile.support_int64`); the !support_int64 branch warns and
        // emits zero / no-op. We follow the !support_int64 fallback
        // since the helper declaration isn't yet wired by the ruzu
        // EmitContext header builder.
        Opcode::LoadGlobal32 => {
            log::warn!("Int64 not supported (helper not declared), emitting zero");
            add_assign(ctx, program, inst_ref, GlslVarType::U32, "0u".to_string());
        }
        Opcode::LoadGlobal64 => {
            log::warn!("Int64 not supported (helper not declared), emitting zero");
            add_assign(ctx, program, inst_ref, GlslVarType::U32x2, "uvec2(0)".to_string());
        }
        Opcode::LoadGlobal128 => {
            log::warn!("Int64 not supported (helper not declared), emitting zero");
            add_assign(ctx, program, inst_ref, GlslVarType::U32x4, "uvec4(0)".to_string());
        }
        Opcode::WriteGlobal32 | Opcode::WriteGlobal64 | Opcode::WriteGlobal128 => {
            log::warn!("Int64 not supported (helper not declared), ignoring write");
        }
        // 8/16-bit Global / Local — upstream NotImplemented.
        Opcode::LoadGlobalS8
        | Opcode::LoadGlobalU8
        | Opcode::LoadGlobalS16
        | Opcode::LoadGlobalU16
        | Opcode::WriteGlobalS8
        | Opcode::WriteGlobalU8
        | Opcode::WriteGlobalS16
        | Opcode::WriteGlobalU16
        | Opcode::LoadLocal
        | Opcode::WriteLocal => {
            panic!(
                "Memory op {:?} not implemented (upstream NotImplemented)",
                inst_snapshot.opcode
            );
        }

        // ── Subgroup / warp ops ───────────────────────────────────────
        Opcode::LaneId => {
            // Port of upstream `IREmitter::LaneId()` (emit_glsl_warp.cpp).
            add_assign(
                ctx, program, inst_ref, GlslVarType::U32,
                "gl_SubGroupInvocationARB".to_string(),
            );
        }
        Opcode::SubgroupEqMask => emit_unary_expr(
            ctx, program, inst_ref, &inst_snapshot, GlslVarType::U32x4,
            |_| "uvec4(gl_SubGroupEqMaskARB)".to_string(),
        ),
        Opcode::SubgroupLtMask => emit_unary_expr(
            ctx, program, inst_ref, &inst_snapshot, GlslVarType::U32x4,
            |_| "uvec4(gl_SubGroupLtMaskARB)".to_string(),
        ),
        Opcode::SubgroupLeMask => emit_unary_expr(
            ctx, program, inst_ref, &inst_snapshot, GlslVarType::U32x4,
            |_| "uvec4(gl_SubGroupLeMaskARB)".to_string(),
        ),
        Opcode::SubgroupGtMask => emit_unary_expr(
            ctx, program, inst_ref, &inst_snapshot, GlslVarType::U32x4,
            |_| "uvec4(gl_SubGroupGtMaskARB)".to_string(),
        ),
        Opcode::SubgroupGeMask => emit_unary_expr(
            ctx, program, inst_ref, &inst_snapshot, GlslVarType::U32x4,
            |_| "uvec4(gl_SubGroupGeMaskARB)".to_string(),
        ),
        // Port of upstream `EmitVoteAll/Any/Equal` (emit_glsl_warp.cpp).
        // Small-warp path (warp_size_potentially_larger_than_guest=false).
        // The big-warp path uses ballotARB+masking and isn't ported.
        Opcode::VoteAll => emit_unary_expr(
            ctx, program, inst_ref, &inst_snapshot, GlslVarType::U1,
            |p| format!("allInvocationsEqualARB({})", p),
        ),
        Opcode::VoteAny => emit_unary_expr(
            ctx, program, inst_ref, &inst_snapshot, GlslVarType::U1,
            |p| format!("anyInvocationARB({})", p),
        ),
        Opcode::VoteEqual => emit_unary_expr(
            ctx, program, inst_ref, &inst_snapshot, GlslVarType::U1,
            |p| format!("allInvocationsEqualARB({})", p),
        ),
        // Port of upstream `EmitSubgroupBallot`.
        Opcode::SubgroupBallot => emit_unary_expr(
            ctx, program, inst_ref, &inst_snapshot, GlslVarType::U32,
            |p| format!("uvec2(ballotARB({})).x", p),
        ),
        // Port of upstream `EmitShuffle{Index,Up,Down,Butterfly}` small-warp
        // path. The Rust IR's 3-arg variant omits upstream's `clamp` —
        // we synthesise it as 31 (full warp) since MK8D shaders use the
        // standard 32-thread warp. `readInvocationARB` returns the value
        // from another lane.
        Opcode::ShuffleIndex => {
            let args = consume_args(ctx, program, &inst_snapshot, 3);
            // src_thread_id = (index & ~seg_mask) | (gl_SubGroupInvocationARB & seg_mask)
            let value = &args[0];
            let index = &args[1];
            let seg_mask = &args[2];
            let src_id = format!(
                "((({})&(~({})))|(gl_SubGroupInvocationARB&({})))",
                index, seg_mask, seg_mask
            );
            add_assign(
                ctx, program, inst_ref, GlslVarType::U32,
                format!("readInvocationARB({},{})", value, src_id),
            );
        }
        Opcode::ShuffleUp => {
            let args = consume_args(ctx, program, &inst_snapshot, 3);
            let value = &args[0];
            let index = &args[1];
            let _seg_mask = &args[2];
            add_assign(
                ctx, program, inst_ref, GlslVarType::U32,
                format!(
                    "readInvocationARB({},uint(int(gl_SubGroupInvocationARB)-int({})))",
                    value, index
                ),
            );
        }
        Opcode::ShuffleDown => {
            let args = consume_args(ctx, program, &inst_snapshot, 3);
            let value = &args[0];
            let index = &args[1];
            let _seg_mask = &args[2];
            add_assign(
                ctx, program, inst_ref, GlslVarType::U32,
                format!(
                    "readInvocationARB({},uint(gl_SubGroupInvocationARB+{}))",
                    value, index
                ),
            );
        }
        Opcode::ShuffleButterfly => {
            let args = consume_args(ctx, program, &inst_snapshot, 3);
            let value = &args[0];
            let index = &args[1];
            let _seg_mask = &args[2];
            add_assign(
                ctx, program, inst_ref, GlslVarType::U32,
                format!(
                    "readInvocationARB({},uint(gl_SubGroupInvocationARB^{}))",
                    value, index
                ),
            );
        }

        // ── Special variables (port of upstream EmitContext getters) ──
        Opcode::SampleId => {
            add_assign(ctx, program, inst_ref, GlslVarType::U32, "uint(gl_SampleID)".to_string());
        }
        Opcode::InvocationId => {
            add_assign(ctx, program, inst_ref, GlslVarType::U32, "uint(gl_InvocationID)".to_string());
        }
        Opcode::InvocationInfo => {
            // Vertex info bits (per upstream emit_glsl_context_get_set.cpp).
            add_assign(ctx, program, inst_ref, GlslVarType::U32, "0u".to_string());
        }
        Opcode::IsHelperInvocation => {
            add_assign(ctx, program, inst_ref, GlslVarType::U1, "gl_HelperInvocation".to_string());
        }
        Opcode::LocalInvocationId => {
            add_assign(
                ctx, program, inst_ref, GlslVarType::U32x4,
                "uvec4(gl_LocalInvocationID,0)".to_string(),
            );
        }
        Opcode::WorkgroupId => {
            add_assign(
                ctx, program, inst_ref, GlslVarType::U32x4,
                "uvec4(gl_WorkGroupID,0)".to_string(),
            );
        }
        Opcode::YDirection => {
            ctx.uses_y_direction = true;
            add_assign(
                ctx,
                program,
                inst_ref,
                GlslVarType::F32,
                "gl_FrontMaterial.ambient.a".to_string(),
            );
        }
        Opcode::RenderArea => {
            // Upstream: gl_FragCoord-style render-area uniform.
            add_assign(ctx, program, inst_ref, GlslVarType::F32x4, "vec4(0.0)".to_string());
        }
        Opcode::ResolutionDownFactor => {
            add_assign(ctx, program, inst_ref, GlslVarType::F32, "1.0".to_string());
        }
        Opcode::SetFragDepth => {
            let value = ctx.var_alloc.consume(program, &inst_snapshot.args[0]);
            ctx.add_fmt(format!("gl_FragDepth={};", value));
        }
        Opcode::SetSampleMask => {
            let value = ctx.var_alloc.consume(program, &inst_snapshot.args[0]);
            ctx.add_fmt(format!("gl_SampleMask[0]=int({});", value));
        }
        Opcode::FSwizzleAdd => emit_ternary_call(
            ctx, program, inst_ref, &inst_snapshot, GlslVarType::F32, "FSwizzleAdd",
        ),

        // ── Control-flow markers (handled by structurer) ──────────────
        // `Branch`/`BranchConditional`/`Return`/`Join`/`SelectionMerge`/
        // `LoopMerge`/`Unreachable` come from the SPIR-V structurer and
        // are consumed at structure-CFG time, not emitted as text.
        Opcode::ConditionRef => {
            // Upstream `EmitConditionRef` forces a real boolean variable for
            // structured control-flow conditions. Syntax-node conditions are
            // not counted by the normal instruction-use walk, so add one
            // synthetic use before defining the result.
            let ret = {
                let inst = program.block_mut(inst_ref.block).inst_mut(inst_ref.inst);
                inst.use_count = inst.use_count.saturating_add(1);
                ctx.var_alloc.add_define(inst, GlslVarType::U1)
            };
            let input = ctx.var_alloc.consume(program, &inst_snapshot.args[0]);
            let suffix = if ctx.profile.has_gl_bool_ref_bug {
                "?true:false"
            } else {
                ""
            };
            if !ret.is_empty() && ret != input {
                ctx.add_fmt(format!("{}={}{};", ret, input, suffix));
            }
        }
        Opcode::Branch
        | Opcode::BranchConditional
        | Opcode::Return
        | Opcode::Join
        | Opcode::SelectionMerge
        | Opcode::LoopMerge
        | Opcode::Unreachable => {
            // No GLSL emission — the structurer produces if/else/while/
            // break statements directly. Reaching the dispatcher means
            // the structurer didn't consume the instruction.
        }

        // ── GetReg/Pred/Patch/Flag (SSA-rewritten away) ───────────────
        Opcode::GetRegister
        | Opcode::GetPred
        | Opcode::GetPatch
        | Opcode::GetCFlag
        | Opcode::GetSFlag
        | Opcode::GetZFlag
        | Opcode::GetOFlag
        | Opcode::SetRegister
        | Opcode::SetPred
        | Opcode::SetPatch
        | Opcode::SetCFlag
        | Opcode::SetSFlag
        | Opcode::SetZFlag
        | Opcode::SetOFlag
        | Opcode::GetGotoVariable
        | Opcode::SetGotoVariable
        | Opcode::GetIndirectBranchVariable
        | Opcode::SetIndirectBranchVariable
        | Opcode::GetAttributeIndexed
        | Opcode::SetAttributeIndexed => {
            // These ops are removed by the SSA rewrite pass and should
            // never reach the GLSL backend. Reaching here is a pass-
            // ordering bug; emit an inert comment to keep going.
            ctx.add_fmt(format!(
                "// {} should have been SSA-rewritten",
                inst_snapshot.opcode.name()
            ));
        }

        // ── Misc ──────────────────────────────────────────────────────
        Opcode::UndefU64 => {
            add_assign(ctx, program, inst_ref, GlslVarType::U64, "0ul".to_string())
        }

        // ── Reference / sparse / zero pseudo-ops (no output) ───────────
        Opcode::GetSparseFromOp
        | Opcode::GetZeroFromOp
        | Opcode::GetSignFromOp
        | Opcode::GetCarryFromOp
        | Opcode::GetOverflowFromOp
        | Opcode::GetInBoundsFromOp => {
            // These pseudo-ops are consumed inline by their parent's
            // emit handler (see `prepare_sparse` in emit_glsl_image).
            // Reaching the dispatcher here means the parent didn't
            // invalidate the pseudo — emit a comment marker so the GLSL
            // still compiles instead of crashing.
            ctx.add_fmt(format!(
                "// {} consumed inline by parent",
                inst_snapshot.opcode.name()
            ));
        }

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
    let mut phi_moves: Vec<(u32, Inst)> = Vec::new();
    let mut references: Vec<(u32, Inst)> = Vec::new();
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
                phi_moves.push((
                    pred,
                    Inst::new(Opcode::PhiMove, vec![Value::Inst(phi_ref), value]),
                ));
                references.push((
                    pred,
                    Inst::new(Opcode::Reference, vec![Value::Inst(phi_ref)]),
                ));
            }
        }
    }

    // Upstream inserts PhiMove before trailing Reference instructions. Stable
    // instruction slots cannot be shifted, so append all moves before refs.
    for (block_index, inst) in phi_moves {
        program.block_mut(block_index).append_inst(inst);
    }
    for (block_index, inst) in references {
        program.block_mut(block_index).append_inst(inst);
    }
}

fn recompute_emit_use_counts(program: &mut ir::Program) {
    for block in &mut program.blocks {
        for inst in block.iter_mut() {
            inst.use_count = 0;
        }
    }

    let mut use_counts: Vec<Vec<u32>> = program
        .blocks
        .iter()
        .map(|block| vec![0u32; block.instructions.len()])
        .collect();

    for block in &program.blocks {
        for inst in block.iter() {
            for arg in &inst.args {
                if let Value::Inst(inst_ref) = arg {
                    if let Some(block_counts) = use_counts.get_mut(inst_ref.block as usize) {
                        if let Some(count) = block_counts.get_mut(inst_ref.inst as usize) {
                            *count += 1;
                        }
                    }
                }
            }
        }
    }

    for (block_index, block) in program.blocks.iter_mut().enumerate() {
        for (inst_index, inst) in block.indexed_iter_mut() {
            if let Some(count) = use_counts
                .get(block_index)
                .and_then(|counts| counts.get(inst_index as usize))
            {
                inst.use_count = *count;
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
    let phi_reg = ctx.var_alloc.consume(program, &inst.args[0]);
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

fn precise_type(inst: &Inst, normal: GlslVarType, precise: GlslVarType) -> GlslVarType {
    if FpControl::from_u32(inst.flags).no_contraction {
        precise
    } else {
        normal
    }
}

#[cfg(test)]
mod tests {
    use crate::backend::bindings::Bindings;
    use crate::backend::glsl::emit_glsl;
    use crate::ir::basic_block::Block;
    use crate::ir::emitter::Emitter;
    use crate::ir::instruction::Inst;
    use crate::ir::opcodes::Opcode;
    use crate::ir::program::Program;
    use crate::ir::types::{FpControl, ShaderStage, Type};
    use crate::ir::value::{Attribute, InstRef, Value};
    use crate::profile::Profile;
    use crate::runtime_info::RuntimeInfo;

    use super::{precolor, recompute_emit_use_counts};

    #[test]
    fn precolor_appends_all_phi_moves_before_references_and_recounts_uses() {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        program.blocks.push(Block::new());

        let src0 = program
            .block_mut(0)
            .append_inst(Inst::new(Opcode::UndefU32, Vec::new()));
        let src1 = program
            .block_mut(0)
            .append_inst(Inst::new(Opcode::UndefU32, Vec::new()));
        let phi0 = program.block_mut(1).append_new_inst(Opcode::Phi, Vec::new());
        let phi1 = program.block_mut(1).append_new_inst(Opcode::Phi, Vec::new());
        program.block_mut(1).inst_mut(phi0).flags = Type::U32 as u32;
        program.block_mut(1).inst_mut(phi1).flags = Type::U32 as u32;
        program
            .block_mut(1)
            .inst_mut(phi0)
            .add_phi_operand(0, Value::Inst(InstRef { block: 0, inst: src0 }));
        program
            .block_mut(1)
            .inst_mut(phi1)
            .add_phi_operand(0, Value::Inst(InstRef { block: 0, inst: src1 }));

        precolor(&mut program);
        recompute_emit_use_counts(&mut program);

        let opcodes: Vec<Opcode> = program.block(0).iter().map(|inst| inst.opcode).collect();
        assert_eq!(
            opcodes,
            vec![
                Opcode::UndefU32,
                Opcode::UndefU32,
                Opcode::PhiMove,
                Opcode::PhiMove,
                Opcode::Reference,
                Opcode::Reference,
            ]
        );
        assert_eq!(program.block(0).inst(src0).use_count, 1);
        assert_eq!(program.block(0).inst(src1).use_count, 1);
        assert_eq!(program.block(1).inst(phi0).use_count, 2);
        assert_eq!(program.block(1).inst(phi1).use_count, 2);
    }

    #[test]
    fn glsl_precise_float_ops_use_precise_variables() {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        {
            let mut emitter = Emitter::new(&mut program, 0);
            let value = emitter.fp_fma_32_with_control(
                Value::ImmF32(1.0),
                Value::ImmF32(2.0),
                Value::ImmF32(3.0),
                FpControl {
                    no_contraction: true,
                    ..FpControl::default()
                },
            );
            emitter.set_attribute(Attribute::generic(0, 0), value, Value::ImmU32(0));
        }

        let mut bindings = Bindings::default();
        let source = emit_glsl(
            &Profile::default(),
            &RuntimeInfo::default(),
            &mut program,
            &mut bindings,
        );

        assert!(source.contains("precise float pf_0=float(0);"));
        assert!(source.contains("pf_0=fma(1.f,2.f,3.f);"));
        assert!(source.contains("out_attr0.x=pf_0;"));
    }

    #[test]
    #[should_panic(expected = "SelectF16 not implemented")]
    fn glsl_select_f16_matches_upstream_not_implemented() {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        program.block_mut(0).append_inst(Inst::new(
            Opcode::SelectF16,
            vec![Value::ImmU1(true), Value::ImmF32(1.0), Value::ImmF32(0.0)],
        ));

        let mut bindings = Bindings::default();
        let _ = emit_glsl(
            &Profile::default(),
            &RuntimeInfo::default(),
            &mut program,
            &mut bindings,
        );
    }

    #[test]
    #[should_panic(expected = "SelectU8 not implemented")]
    fn glsl_select_u8_matches_upstream_not_implemented() {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        program.block_mut(0).append_inst(Inst::new(
            Opcode::SelectU8,
            vec![Value::ImmU1(true), Value::ImmU32(1), Value::ImmU32(0)],
        ));

        let mut bindings = Bindings::default();
        let _ = emit_glsl(
            &Profile::default(),
            &RuntimeInfo::default(),
            &mut program,
            &mut bindings,
        );
    }

    #[test]
    fn glsl_y_direction_uses_fixed_function_material_state() {
        let mut program = Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());
        {
            let mut emitter = Emitter::new(&mut program, 0);
            let value = emitter.y_direction();
            emitter.set_attribute(Attribute::generic(0, 0), value, Value::ImmU32(0));
        }

        let mut bindings = Bindings::default();
        let source = emit_glsl(
            &Profile::default(),
            &RuntimeInfo::default(),
            &mut program,
            &mut bindings,
        );

        assert!(source.contains("gl_FrontMaterial.ambient.a"));
        assert!(!source.contains("y_direction"));
    }
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

/// Port of upstream `EmitCompositeInsert{U32,F16,F32,F64}x{2,3,4}`.
///
/// Each `CompositeInsert` is a 3-arg op: (composite, value, index). It
/// produces a new composite identical to the input but with the
/// `index`-th element replaced by `value`. GLSL implements this through
/// an SSA temporary that copies the input and overwrites one swizzle
/// component:
///
/// ```glsl
/// vec4 tmp = composite_in;
/// tmp.y = inserted_value;
/// // tmp is the result
/// ```
///
/// Upstream emits the equivalent through `ctx.AddU32x2("{}=...;{}.x=...",
/// inst, composite, inst, value)` — three statements collapsed into one
/// emission. We reproduce that pattern.
fn emit_composite_insert(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let composite = ctx.var_alloc.consume(program, &inst.args[0]);
    let value = ctx.var_alloc.consume(program, &inst.args[1]);
    let index = inst.args[2].imm_u32() as usize;
    const SWIZZLE: [&str; 4] = ["x", "y", "z", "w"];
    // Pick GLSL output type matching the input opcode. Composite inserts
    // preserve width × element type of the input composite.
    let ty = match inst.opcode {
        Opcode::CompositeInsertU32x2 => GlslVarType::U32x2,
        Opcode::CompositeInsertU32x3 => GlslVarType::U32x3,
        Opcode::CompositeInsertU32x4 => GlslVarType::U32x4,
        Opcode::CompositeInsertF32x2 => GlslVarType::F32x2,
        Opcode::CompositeInsertF32x3 => GlslVarType::F32x3,
        Opcode::CompositeInsertF32x4 => GlslVarType::F32x4,
        Opcode::CompositeInsertF16x2 => GlslVarType::F16x2,
        // F16x3/F16x4 and F64x* inserts are NotImplemented in upstream.
        _ => panic!("Composite insert {:?} not supported", inst.opcode),
    };
    // Single fused emission: define dst, copy composite into it, then
    // overwrite one component. Matches upstream's multi-statement chain.
    let dst = ctx
        .var_alloc
        .define(program.block_mut(inst_ref.block).inst_mut(inst_ref.inst), ty);
    ctx.add_fmt(format!(
        "{}={};{}.{}={};",
        dst,
        composite,
        dst,
        SWIZZLE[index],
        value
    ));
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

// ── Shared-memory atomic helper ────────────────────────────────────────
//
// Port of upstream `EmitSharedAtomic*` (backend/glsl/emit_glsl_atomic.cpp).
// Args: (offset, value). Emits `dst=atomicOp(shared_buf[offset>>2], cast(value));`.
fn emit_shared_atomic_binop(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    inst: &Inst,
    glsl_fn: &str,
    cast_value: Option<&str>,
) {
    let offset = ctx.var_alloc.consume(program, &inst.args[0]);
    let value = ctx.var_alloc.consume(program, &inst.args[1]);
    let value_expr = match cast_value {
        Some(c) => format!("{}({})", c, value),
        None => value,
    };
    add_assign(
        ctx,
        program,
        inst_ref,
        GlslVarType::U32,
        format!("{}(smem[{}>>2],{})", glsl_fn, offset, value_expr),
    );
}

// ── Shared-memory load/store helpers ───────────────────────────────────
//
// Port of upstream `EmitLoadSharedU32` / `EmitWriteSharedU32` (and U64).
// Upstream emits `dst=smem[ofs>>2];` and `smem[ofs>>2]=value;` against
// the shared workgroup buffer. The Rust port mirrors that text.

fn emit_load_shared_u32(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let offset = ctx.var_alloc.consume(program, &inst.args[0]);
    add_assign(
        ctx,
        program,
        inst_ref,
        GlslVarType::U32,
        format!("smem[{}>>2]", offset),
    );
}

fn emit_load_shared_u64(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let offset = ctx.var_alloc.consume(program, &inst.args[0]);
    add_assign(
        ctx,
        program,
        inst_ref,
        GlslVarType::U64,
        format!(
            "packUint2x32(uvec2(smem[{}>>2],smem[({}>>2)+1u]))",
            offset, offset
        ),
    );
}

fn emit_write_shared_u32(ctx: &mut EmitContext, program: &mut ir::Program, inst: &Inst) {
    let offset = ctx.var_alloc.consume(program, &inst.args[0]);
    let value = ctx.var_alloc.consume(program, &inst.args[1]);
    ctx.add_fmt(format!("smem[{}>>2]={};", offset, value));
}

fn emit_write_shared_u64(ctx: &mut EmitContext, program: &mut ir::Program, inst: &Inst) {
    let offset = ctx.var_alloc.consume(program, &inst.args[0]);
    let value = ctx.var_alloc.consume(program, &inst.args[1]);
    ctx.add_fmt(format!(
        "{{uvec2 _w=unpackUint2x32({});smem[{}>>2]=_w.x;smem[({}>>2)+1u]=_w.y;}}",
        value, offset, offset
    ));
}

// ── Storage SSBO atomic helper ────────────────────────────────────────
//
// Port of upstream `EmitStorageAtomic*32` (emit_glsl_atomic.cpp).
// Args: (binding: imm u32, offset: consumed string, value: consumed string).
// Emits:
//   `dst = atomicOp({stage_name}_ssbo{binding}[offset>>2], value);`
// matching upstream's `ctx.AddU32("{}=atomicAdd(...);", ...)` format.
fn emit_storage_atomic_native(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    inst: &Inst,
    glsl_fn: &str,
    cast_value: Option<&str>,
) {
    let binding = inst.args[0].imm_u32();
    let offset = ctx.var_alloc.consume(program, &inst.args[1]);
    let value = ctx.var_alloc.consume(program, &inst.args[2]);
    let value_expr = match cast_value {
        Some(c) => format!("{}({})", c, value),
        None => value,
    };
    let stage_name = ctx.stage_name;
    add_assign(
        ctx,
        program,
        inst_ref,
        GlslVarType::U32,
        format!(
            "{}({}_ssbo{}[{}>>2],{})",
            glsl_fn, stage_name, binding, offset, value_expr
        ),
    );
}

// ── Shared memory load/store helpers (8/16/128-bit variants) ─────────

fn emit_load_shared_8(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    inst: &Inst,
    signed: bool,
) {
    let offset = ctx.var_alloc.consume(program, &inst.args[0]);
    let cast = if signed { "int" } else { "" };
    let target = if cast.is_empty() {
        format!("smem[{}>>2]", offset)
    } else {
        format!("{}(smem[{}>>2])", cast, offset)
    };
    add_assign(
        ctx, program, inst_ref, GlslVarType::U32,
        format!("bitfieldExtract({},int({}%4)*8,8)", target, offset),
    );
}

fn emit_load_shared_16(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    inst: &Inst,
    signed: bool,
) {
    let offset = ctx.var_alloc.consume(program, &inst.args[0]);
    let cast = if signed { "int" } else { "" };
    let target = if cast.is_empty() {
        format!("smem[{}>>2]", offset)
    } else {
        format!("{}(smem[{}>>2])", cast, offset)
    };
    add_assign(
        ctx, program, inst_ref, GlslVarType::U32,
        format!("bitfieldExtract({},int(({}>>1)%2)*16,16)", target, offset),
    );
}

fn emit_load_shared_u128(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let offset = ctx.var_alloc.consume(program, &inst.args[0]);
    add_assign(
        ctx, program, inst_ref, GlslVarType::U32x4,
        format!(
            "uvec4(smem[{0}>>2],smem[({0}>>2)+1u],smem[({0}>>2)+2u],smem[({0}>>2)+3u])",
            offset
        ),
    );
}

fn emit_write_shared_8(ctx: &mut EmitContext, program: &mut ir::Program, inst: &Inst) {
    let offset = ctx.var_alloc.consume(program, &inst.args[0]);
    let value = ctx.var_alloc.consume(program, &inst.args[1]);
    ctx.add_fmt(format!(
        "smem[{0}>>2]=bitfieldInsert(smem[{0}>>2],{1},int({0}%4)*8,8);",
        offset, value
    ));
}

fn emit_write_shared_16(ctx: &mut EmitContext, program: &mut ir::Program, inst: &Inst) {
    let offset = ctx.var_alloc.consume(program, &inst.args[0]);
    let value = ctx.var_alloc.consume(program, &inst.args[1]);
    ctx.add_fmt(format!(
        "smem[{0}>>2]=bitfieldInsert(smem[{0}>>2],{1},int(({0}>>1)%2)*16,16);",
        offset, value
    ));
}

fn emit_write_shared_u128(ctx: &mut EmitContext, program: &mut ir::Program, inst: &Inst) {
    let offset = ctx.var_alloc.consume(program, &inst.args[0]);
    let value = ctx.var_alloc.consume(program, &inst.args[1]);
    ctx.add_fmt(format!(
        "smem[{0}>>2]={1}.x;smem[({0}>>2)+1u]={1}.y;smem[({0}>>2)+2u]={1}.z;smem[({0}>>2)+3u]={1}.w;",
        offset, value
    ));
}

// ── Storage SSBO load/store helpers ───────────────────────────────────

fn emit_load_storage_8(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    inst: &Inst,
    signed: bool,
) {
    let binding = inst.args[0].imm_u32();
    let offset = ctx.var_alloc.consume(program, &inst.args[1]);
    let stage = ctx.stage_name;
    let target = if signed {
        format!("int({}_ssbo{}[{}>>2])", stage, binding, offset)
    } else {
        format!("{}_ssbo{}[{}>>2]", stage, binding, offset)
    };
    add_assign(
        ctx, program, inst_ref, GlslVarType::U32,
        format!("bitfieldExtract({},int({}%4)*8,8)", target, offset),
    );
}

fn emit_load_storage_16(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    inst: &Inst,
    signed: bool,
) {
    let binding = inst.args[0].imm_u32();
    let offset = ctx.var_alloc.consume(program, &inst.args[1]);
    let stage = ctx.stage_name;
    let target = if signed {
        format!("int({}_ssbo{}[{}>>2])", stage, binding, offset)
    } else {
        format!("{}_ssbo{}[{}>>2]", stage, binding, offset)
    };
    add_assign(
        ctx, program, inst_ref, GlslVarType::U32,
        format!("bitfieldExtract({},int(({}>>1)%2)*16,16)", target, offset),
    );
}

fn emit_load_storage_32(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let binding = inst.args[0].imm_u32();
    let offset = ctx.var_alloc.consume(program, &inst.args[1]);
    let stage = ctx.stage_name;
    add_assign(
        ctx, program, inst_ref, GlslVarType::U32,
        format!("{}_ssbo{}[{}>>2]", stage, binding, offset),
    );
}

fn emit_load_storage_64(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let binding = inst.args[0].imm_u32();
    let offset = ctx.var_alloc.consume(program, &inst.args[1]);
    let stage = ctx.stage_name;
    add_assign(
        ctx, program, inst_ref, GlslVarType::U32x2,
        format!(
            "uvec2({0}_ssbo{1}[{2}>>2],{0}_ssbo{1}[({2}+4)>>2])",
            stage, binding, offset
        ),
    );
}

fn emit_load_storage_128(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let binding = inst.args[0].imm_u32();
    let offset = ctx.var_alloc.consume(program, &inst.args[1]);
    let stage = ctx.stage_name;
    add_assign(
        ctx, program, inst_ref, GlslVarType::U32x4,
        format!(
            "uvec4({0}_ssbo{1}[{2}>>2],{0}_ssbo{1}[({2}+4)>>2],{0}_ssbo{1}[({2}+8)>>2],{0}_ssbo{1}[({2}+12)>>2])",
            stage, binding, offset
        ),
    );
}

fn emit_write_storage_32(ctx: &mut EmitContext, program: &mut ir::Program, inst: &Inst) {
    let binding = inst.args[0].imm_u32();
    let offset = ctx.var_alloc.consume(program, &inst.args[1]);
    let value = ctx.var_alloc.consume(program, &inst.args[2]);
    let stage = ctx.stage_name;
    ctx.add_fmt(format!(
        "{}_ssbo{}[{}>>2]={};",
        stage, binding, offset, value
    ));
}

fn emit_write_storage_64(ctx: &mut EmitContext, program: &mut ir::Program, inst: &Inst) {
    let binding = inst.args[0].imm_u32();
    let offset = ctx.var_alloc.consume(program, &inst.args[1]);
    let value = ctx.var_alloc.consume(program, &inst.args[2]);
    let stage = ctx.stage_name;
    ctx.add_fmt(format!(
        "{0}_ssbo{1}[{2}>>2]={3}.x;{0}_ssbo{1}[({2}+4)>>2]={3}.y;",
        stage, binding, offset, value
    ));
}

fn emit_write_storage_128(ctx: &mut EmitContext, program: &mut ir::Program, inst: &Inst) {
    let binding = inst.args[0].imm_u32();
    let offset = ctx.var_alloc.consume(program, &inst.args[1]);
    let value = ctx.var_alloc.consume(program, &inst.args[2]);
    let stage = ctx.stage_name;
    ctx.add_fmt(format!(
        "{0}_ssbo{1}[{2}>>2]={3}.x;{0}_ssbo{1}[({2}+4)>>2]={3}.y;{0}_ssbo{1}[({2}+8)>>2]={3}.z;{0}_ssbo{1}[({2}+12)>>2]={3}.w;",
        stage, binding, offset, value
    ));
}
