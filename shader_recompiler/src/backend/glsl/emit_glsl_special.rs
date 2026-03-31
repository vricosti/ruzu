// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLSL special operation emission (phi, void, prologue/epilogue).
//!
//! Maps to upstream `backend/glsl/emit_glsl_special.cpp`.

use super::glsl_emit_context::EmitContext;

pub fn emit_phi(_ctx: &mut EmitContext) {}
pub fn emit_void(_ctx: &mut EmitContext) {}
pub fn emit_reference(_ctx: &mut EmitContext) {}
pub fn emit_phi_move(ctx: &mut EmitContext) {
    ctx.add_line("// phi move");
}
pub fn emit_prologue(_ctx: &mut EmitContext) {}
pub fn emit_epilogue(_ctx: &mut EmitContext) {}
pub fn emit_emit_vertex(ctx: &mut EmitContext) {
    ctx.add_line("EmitVertex();");
}
pub fn emit_end_primitive(ctx: &mut EmitContext) {
    ctx.add_line("EndPrimitive();");
}
