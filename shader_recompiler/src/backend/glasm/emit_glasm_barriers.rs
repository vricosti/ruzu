// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLASM barrier emission.
//!
//! Maps to upstream `backend/glasm/emit_glasm_barriers.cpp`.

use super::glasm_emit_context::EmitContext;

/// Emit a workgroup barrier.
pub fn emit_barrier(ctx: &mut EmitContext) {
    ctx.add_line("BAR;");
}

/// Emit a workgroup memory barrier.
pub fn emit_workgroup_memory_barrier(ctx: &mut EmitContext) {
    ctx.add_line("MEMBAR.CTA;");
}

/// Emit a device-wide memory barrier.
pub fn emit_device_memory_barrier(ctx: &mut EmitContext) {
    ctx.add_line("MEMBAR;");
}
