// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLSL memory operation emission.
//!
//! Maps to upstream `backend/glsl/emit_glsl_memory.cpp`.

use super::glsl_emit_context::EmitContext;
use super::var_alloc::GlslVarType;
use crate::ir;
use crate::ir::instruction::Inst;
use crate::ir::opcodes::Opcode;
use crate::ir::value::InstRef;

pub fn global_address_expr(address: String) -> String {
    format!("uint64_t({})", address)
}

pub fn emit_load_global_32(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let address = global_address_expr(ctx.var_alloc.consume(program, &inst.args[0]));
    if ctx.profile.support_int64 {
        add_assign(
            ctx,
            program,
            inst_ref,
            GlslVarType::U32,
            format!("LoadGlobal32({})", address),
        );
    } else {
        log::warn!("Int64 not supported, ignoring memory operation");
        add_assign(ctx, program, inst_ref, GlslVarType::U32, "0u".to_string());
    }
}

pub fn emit_load_global_64(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let address = global_address_expr(ctx.var_alloc.consume(program, &inst.args[0]));
    if ctx.profile.support_int64 {
        add_assign(
            ctx,
            program,
            inst_ref,
            GlslVarType::U32x2,
            format!("LoadGlobal64({})", address),
        );
    } else {
        log::warn!("Int64 not supported, ignoring memory operation");
        add_assign(
            ctx,
            program,
            inst_ref,
            GlslVarType::U32x2,
            "uvec2(0)".to_string(),
        );
    }
}

pub fn emit_load_global_128(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let address = global_address_expr(ctx.var_alloc.consume(program, &inst.args[0]));
    if ctx.profile.support_int64 {
        add_assign(
            ctx,
            program,
            inst_ref,
            GlslVarType::U32x4,
            format!("LoadGlobal128({})", address),
        );
    } else {
        log::warn!("Int64 not supported, ignoring memory operation");
        add_assign(
            ctx,
            program,
            inst_ref,
            GlslVarType::U32x4,
            "uvec4(0)".to_string(),
        );
    }
}

pub fn emit_write_global_32(ctx: &mut EmitContext, program: &mut ir::Program, inst: &Inst) {
    if ctx.profile.support_int64 {
        let address = global_address_expr(ctx.var_alloc.consume(program, &inst.args[0]));
        let value = ctx.var_alloc.consume(program, &inst.args[1]);
        ctx.add_fmt(format!("WriteGlobal32({},{});", address, value));
    } else {
        log::warn!("Int64 not supported, ignoring memory operation");
    }
}

pub fn emit_write_global_64(ctx: &mut EmitContext, program: &mut ir::Program, inst: &Inst) {
    if ctx.profile.support_int64 {
        let address = global_address_expr(ctx.var_alloc.consume(program, &inst.args[0]));
        let value = ctx.var_alloc.consume(program, &inst.args[1]);
        ctx.add_fmt(format!("WriteGlobal64({},{});", address, value));
    } else {
        log::warn!("Int64 not supported, ignoring memory operation");
    }
}

pub fn emit_write_global_128(ctx: &mut EmitContext, program: &mut ir::Program, inst: &Inst) {
    if ctx.profile.support_int64 {
        let address = global_address_expr(ctx.var_alloc.consume(program, &inst.args[0]));
        let value = ctx.var_alloc.consume(program, &inst.args[1]);
        ctx.add_fmt(format!("WriteGlobal128({},{});", address, value));
    } else {
        log::warn!("Int64 not supported, ignoring memory operation");
    }
}

pub fn emit_global_narrow_not_implemented(opcode: Opcode) -> ! {
    panic!(
        "Memory op {:?} not implemented (upstream NotImplemented)",
        opcode
    );
}

fn add_assign(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    ty: GlslVarType,
    expr: String,
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
