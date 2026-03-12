// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLASM undefined value emission.
//!
//! Maps to upstream `backend/glasm/emit_glasm_undefined.cpp`.

use super::glasm_emit_context::EmitContext;

/// Emit undefined U1 (defaults to 0).
pub fn emit_undef_u1(ctx: &mut EmitContext) {
    ctx.add_line("MOV.S RC.x,0;");
}

/// Emit undefined U8 (defaults to 0).
pub fn emit_undef_u8(ctx: &mut EmitContext) {
    ctx.add_line("MOV.S RC.x,0;");
}

/// Emit undefined U16 (defaults to 0).
pub fn emit_undef_u16(ctx: &mut EmitContext) {
    ctx.add_line("MOV.S RC.x,0;");
}

/// Emit undefined U32 (defaults to 0).
pub fn emit_undef_u32(ctx: &mut EmitContext) {
    ctx.add_line("MOV.S RC.x,0;");
}

/// Emit undefined U64 (defaults to 0).
pub fn emit_undef_u64(ctx: &mut EmitContext) {
    ctx.add_line("MOV.S64 DC.x,0;");
}
