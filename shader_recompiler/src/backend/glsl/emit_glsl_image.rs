// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLSL image/texture operation emission.
//!
//! Maps to upstream `backend/glsl/emit_glsl_image.cpp`.

use super::glsl_emit_context::EmitContext;

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
