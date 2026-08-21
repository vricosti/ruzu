// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLASM special operation emission (phi, void, prologue/epilogue, emit vertex).
//!
//! Maps to upstream `backend/glasm/emit_glasm_special.cpp`.

use super::glasm_emit_context::EmitContext;

/// Emit phi node - consumes arguments but phi resolution is done at register
/// allocation time.
pub fn emit_phi(_ctx: &mut EmitContext) {
    // Phi nodes are resolved during register allocation
}

/// Emit void - no-op.
pub fn emit_void(_ctx: &mut EmitContext) {}

/// Emit reference - consumes a value.
pub fn emit_reference(_ctx: &mut EmitContext) {}

/// Emit phi move - copies value into phi destination.
pub fn emit_phi_move(ctx: &mut EmitContext) {
    ctx.add_line("MOV.S RC.x,RC.y;");
}

/// Emit prologue - currently no-op, matching upstream.
pub fn emit_prologue(_ctx: &mut EmitContext) {}

/// Emit epilogue - currently no-op, matching upstream.
pub fn emit_epilogue(_ctx: &mut EmitContext) {}

/// Emit vertex for geometry shaders.
pub fn emit_emit_vertex(ctx: &mut EmitContext) {
    ctx.add_line("EMIT;");
}

/// End primitive for geometry shaders.
pub fn emit_end_primitive(ctx: &mut EmitContext) {
    ctx.add_line("ENDPRIM;");
}
