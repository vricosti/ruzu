// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLSL composite (vector construct/extract/insert) emission.
//!
//! Maps to upstream `backend/glsl/emit_glsl_composite.cpp`.

use crate::ir;
use crate::ir::instruction::Inst;
use crate::ir::opcodes::Opcode;
use crate::ir::value::InstRef;

use super::glsl_emit_context::EmitContext;
use super::var_alloc::GlslVarType;

const SWIZZLE: [&str; 4] = ["x", "y", "z", "w"];

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

pub fn emit_composite_construct_inst(
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

pub fn emit_composite_extract_inst(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    inst: &Inst,
    ty: GlslVarType,
) {
    let composite = ctx.var_alloc.consume(program, &inst.args[0]);
    let index = inst.args[1].imm_u32() as usize;
    add_assign(
        ctx,
        program,
        inst_ref,
        ty,
        format!("{}.{}", composite, SWIZZLE[index]),
    );
}

pub fn emit_composite_insert_inst(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let composite = ctx.var_alloc.consume(program, &inst.args[0]);
    let value = ctx.var_alloc.consume(program, &inst.args[1]);
    let index = inst.args[2].imm_u32() as usize;
    let ty = match inst.opcode {
        Opcode::CompositeInsertU32x2 => GlslVarType::U32x2,
        Opcode::CompositeInsertU32x3 => GlslVarType::U32x3,
        Opcode::CompositeInsertU32x4 => GlslVarType::U32x4,
        Opcode::CompositeInsertF32x2 => GlslVarType::F32x2,
        Opcode::CompositeInsertF32x3 => GlslVarType::F32x3,
        Opcode::CompositeInsertF32x4 => GlslVarType::F32x4,
        _ => panic!("Composite insert {:?} not supported", inst.opcode),
    };
    let dst = ctx.var_alloc.define(inst_mut(program, inst_ref), ty);
    ctx.add_fmt(format!(
        "{}={};{}.{}={};",
        dst, composite, dst, SWIZZLE[index], value
    ));
}

pub fn emit_composite_construct_u32x2(ctx: &mut EmitContext, e1: &str, e2: &str) {
    ctx.add_fmt(format!("u2_0=uvec2({},{});", e1, e2));
}
pub fn emit_composite_construct_u32x3(ctx: &mut EmitContext, e1: &str, e2: &str, e3: &str) {
    ctx.add_fmt(format!("u3_0=uvec3({},{},{});", e1, e2, e3));
}
pub fn emit_composite_construct_u32x4(
    ctx: &mut EmitContext,
    e1: &str,
    e2: &str,
    e3: &str,
    e4: &str,
) {
    ctx.add_fmt(format!("u4_0=uvec4({},{},{},{});", e1, e2, e3, e4));
}
pub fn emit_composite_extract_u32x2(ctx: &mut EmitContext, composite: &str, index: u32) {
    let swz = ["x", "y"][index as usize];
    ctx.add_fmt(format!("u_0={}.{};", composite, swz));
}
pub fn emit_composite_extract_u32x3(ctx: &mut EmitContext, composite: &str, index: u32) {
    let swz = ["x", "y", "z"][index as usize];
    ctx.add_fmt(format!("u_0={}.{};", composite, swz));
}
pub fn emit_composite_extract_u32x4(ctx: &mut EmitContext, composite: &str, index: u32) {
    let swz = ["x", "y", "z", "w"][index as usize];
    ctx.add_fmt(format!("u_0={}.{};", composite, swz));
}
pub fn emit_composite_construct_f32x2(ctx: &mut EmitContext, e1: &str, e2: &str) {
    ctx.add_fmt(format!("f2_0=vec2({},{});", e1, e2));
}
pub fn emit_composite_construct_f32x3(ctx: &mut EmitContext, e1: &str, e2: &str, e3: &str) {
    ctx.add_fmt(format!("f3_0=vec3({},{},{});", e1, e2, e3));
}
pub fn emit_composite_construct_f32x4(
    ctx: &mut EmitContext,
    e1: &str,
    e2: &str,
    e3: &str,
    e4: &str,
) {
    ctx.add_fmt(format!("f4_0=vec4({},{},{},{});", e1, e2, e3, e4));
}
pub fn emit_composite_extract_f32x2(ctx: &mut EmitContext, composite: &str, index: u32) {
    let swz = ["x", "y"][index as usize];
    ctx.add_fmt(format!("f_0={}.{};", composite, swz));
}
pub fn emit_composite_extract_f32x3(ctx: &mut EmitContext, composite: &str, index: u32) {
    let swz = ["x", "y", "z"][index as usize];
    ctx.add_fmt(format!("f_0={}.{};", composite, swz));
}
pub fn emit_composite_extract_f32x4(ctx: &mut EmitContext, composite: &str, index: u32) {
    let swz = ["x", "y", "z", "w"][index as usize];
    ctx.add_fmt(format!("f_0={}.{};", composite, swz));
}
pub fn emit_composite_insert_f32x2(
    ctx: &mut EmitContext,
    composite: &str,
    object: &str,
    index: u32,
) {
    let swz = ["x", "y"][index as usize];
    ctx.add_fmt(format!("f2_0={};f2_0.{}={};", composite, swz, object));
}
pub fn emit_composite_insert_f32x3(
    ctx: &mut EmitContext,
    composite: &str,
    object: &str,
    index: u32,
) {
    let swz = ["x", "y", "z"][index as usize];
    ctx.add_fmt(format!("f3_0={};f3_0.{}={};", composite, swz, object));
}
pub fn emit_composite_insert_f32x4(
    ctx: &mut EmitContext,
    composite: &str,
    object: &str,
    index: u32,
) {
    let swz = ["x", "y", "z", "w"][index as usize];
    ctx.add_fmt(format!("f4_0={};f4_0.{}={};", composite, swz, object));
}
