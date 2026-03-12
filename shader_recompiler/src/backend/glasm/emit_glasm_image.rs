// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLASM image/texture operation emission.
//!
//! Maps to upstream `backend/glasm/emit_glasm_image.cpp`.
//!
//! Image and texture operations in GLASM use the TEX, TXD, TXB, TXL, TXF,
//! TXQ, and ATOM instructions with texture/image bindings.

use super::glasm_emit_context::EmitContext;

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
