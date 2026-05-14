// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Main GLSL emission entry point.
//!
//! Maps to upstream `backend/glsl/emit_glsl.cpp`.

use crate::ir;
use crate::ir::instruction::Inst;
use crate::ir::opcodes::Opcode;
use crate::ir::program::SyntaxNode;
use crate::ir::value::{InstRef, Value};

use super::glsl_emit_context::EmitContext;
use super::var_alloc::GlslVarType;

/// Walk an IR program and emit GLSL code.
pub fn emit_program(ctx: &mut EmitContext, program: &mut ir::Program) {
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
    let inst_count = program.block(block_index).instructions.len() as u32;
    for inst_index in 0..inst_count {
        emit_inst(
            ctx,
            program,
            InstRef {
                block: block_index,
                inst: inst_index,
            },
        );
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

        // Special
        Opcode::Prologue | Opcode::Epilogue | Opcode::Void | Opcode::Phi | Opcode::PhiMove => {}
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
