// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLSL undefined value emission.
//!
//! Maps to upstream `backend/glsl/emit_glsl_undefined.cpp`.

use super::glsl_emit_context::EmitContext;
use super::var_alloc::GlslVarType;
use crate::ir;
use crate::ir::value::InstRef;

pub fn emit_undef_u1(ctx: &mut EmitContext, program: &mut ir::Program, inst_ref: InstRef) {
    add_assign(ctx, program, inst_ref, GlslVarType::U1, "false");
}

pub fn emit_undef_u8(ctx: &mut EmitContext, program: &mut ir::Program, inst_ref: InstRef) {
    add_assign(ctx, program, inst_ref, GlslVarType::U32, "0u");
}

pub fn emit_undef_u16(ctx: &mut EmitContext, program: &mut ir::Program, inst_ref: InstRef) {
    add_assign(ctx, program, inst_ref, GlslVarType::U32, "0u");
}

pub fn emit_undef_u32(ctx: &mut EmitContext, program: &mut ir::Program, inst_ref: InstRef) {
    add_assign(ctx, program, inst_ref, GlslVarType::U32, "0u");
}

pub fn emit_undef_u64(ctx: &mut EmitContext, program: &mut ir::Program, inst_ref: InstRef) {
    add_assign(ctx, program, inst_ref, GlslVarType::U64, "0u");
}

fn add_assign(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    ty: GlslVarType,
    expr: &str,
) {
    let dst = ctx.var_alloc.add_define(
        program.block_mut(inst_ref.block).inst_mut(inst_ref.inst),
        ty,
    );
    if dst.is_empty() {
        return;
    }
    ctx.add_fmt(format!("{}={};", dst, expr));
}
