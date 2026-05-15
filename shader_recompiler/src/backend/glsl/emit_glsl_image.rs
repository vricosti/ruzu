// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLSL image/texture operation emission.
//!
//! Maps to upstream `backend/glsl/emit_glsl_image.cpp`.

use crate::ir;
use crate::ir::instruction::Inst;
use crate::ir::value::InstRef;

use super::glsl_emit_context::EmitContext;
use super::var_alloc::GlslVarType;

fn inst_mut<'a>(program: &'a mut ir::Program, inst_ref: InstRef) -> &'a mut Inst {
    program.block_mut(inst_ref.block).inst_mut(inst_ref.inst)
}

pub fn emit_image_sample_implicit_lod_inst(
    ctx: &mut EmitContext,
    program: &mut ir::Program,
    inst_ref: InstRef,
    inst: &Inst,
) {
    let texture_index = inst.args[0].imm_u32();
    let coords = ctx.var_alloc.consume(program, &inst.args[1]);
    let dst = ctx
        .var_alloc
        .define(inst_mut(program, inst_ref), GlslVarType::F32x4);
    if ctx.stage == crate::stage::Stage::Fragment {
        ctx.add_fmt(format!(
            "{}=texture({}_tex{},{});",
            dst, ctx.stage_name, texture_index, coords
        ));
    } else {
        ctx.add_fmt(format!(
            "{}=textureLod({}_tex{}, {}, 0.0);",
            dst, ctx.stage_name, texture_index, coords
        ));
    }
}

pub fn emit_image_sample_implicit_lod(ctx: &mut EmitContext) {
    ctx.add_line("// ImageSampleImplicitLod (complex)");
}
pub fn emit_image_sample_explicit_lod(ctx: &mut EmitContext) {
    ctx.add_line("// ImageSampleExplicitLod (complex)");
}
pub fn emit_image_sample_dref_implicit_lod(ctx: &mut EmitContext) {
    ctx.add_line("// ImageSampleDrefImplicitLod (complex)");
}
pub fn emit_image_sample_dref_explicit_lod(ctx: &mut EmitContext) {
    ctx.add_line("// ImageSampleDrefExplicitLod (complex)");
}
pub fn emit_image_gather(ctx: &mut EmitContext) {
    ctx.add_line("// ImageGather (complex)");
}
pub fn emit_image_gather_dref(ctx: &mut EmitContext) {
    ctx.add_line("// ImageGatherDref (complex)");
}
pub fn emit_image_fetch(ctx: &mut EmitContext) {
    ctx.add_line("// ImageFetch (complex)");
}
pub fn emit_image_query_dimensions(ctx: &mut EmitContext) {
    ctx.add_line("// ImageQueryDimensions (complex)");
}
pub fn emit_image_read(ctx: &mut EmitContext) {
    ctx.add_line("// ImageRead (complex)");
}
pub fn emit_image_write(ctx: &mut EmitContext) {
    ctx.add_line("// ImageWrite (complex)");
}
