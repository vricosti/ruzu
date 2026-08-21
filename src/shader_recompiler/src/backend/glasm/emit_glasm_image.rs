// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLASM image/texture operation emission.
//!
//! Maps to upstream `backend/glasm/emit_glasm_image.cpp`.
//!
//! Image and texture operations in GLASM use the TEX, TXD, TXB, TXL, TXF,
//! TXQ, and ATOM instructions with texture/image bindings.

use super::glasm_emit_context::EmitContext;
use crate::ir::{instruction::Inst, value::Value};

pub fn emit_is_texture_scaled(ctx: &mut EmitContext, inst: &Inst) {
    let Value::ImmU32(index) = inst.args[0] else {
        panic!("Non-constant texture rescaling");
    };
    ctx.add_line(&format!("AND.U RC.x,scaling[0].x,{};", 1u32 << index));
    ctx.add_line("SNE.S RC.x,RC.x,0;");
}

pub fn emit_is_image_scaled(ctx: &mut EmitContext, inst: &Inst) {
    let Value::ImmU32(index) = inst.args[0] else {
        panic!("Non-constant texture rescaling");
    };
    ctx.add_line(&format!("AND.U RC.x,scaling[0].y,{};", 1u32 << index));
    ctx.add_line("SNE.S RC.x,RC.x,0;");
}

pub fn emit_image_sample_implicit_lod(ctx: &mut EmitContext) {
    ctx.add_line("; ImageSampleImplicitLod (complex, texture binding required)");
}

pub fn emit_image_sample_explicit_lod(ctx: &mut EmitContext) {
    ctx.add_line("; ImageSampleExplicitLod (complex, texture binding required)");
}

pub fn emit_image_sample_dref_implicit_lod(ctx: &mut EmitContext) {
    ctx.add_line("; ImageSampleDrefImplicitLod (complex, texture binding required)");
}

pub fn emit_image_sample_dref_explicit_lod(ctx: &mut EmitContext) {
    ctx.add_line("; ImageSampleDrefExplicitLod (complex, texture binding required)");
}

pub fn emit_image_gather(ctx: &mut EmitContext) {
    ctx.add_line("; ImageGather (complex, texture binding required)");
}

pub fn emit_image_gather_dref(ctx: &mut EmitContext) {
    ctx.add_line("; ImageGatherDref (complex, texture binding required)");
}

pub fn emit_image_fetch(ctx: &mut EmitContext) {
    ctx.add_line("; ImageFetch (complex, texture binding required)");
}

pub fn emit_image_query_dimensions(ctx: &mut EmitContext) {
    ctx.add_line("; ImageQueryDimensions (complex, texture binding required)");
}

pub fn emit_image_read(ctx: &mut EmitContext) {
    ctx.add_line("; ImageRead (complex, image binding required)");
}

pub fn emit_image_write(ctx: &mut EmitContext) {
    ctx.add_line("; ImageWrite (complex, image binding required)");
}
