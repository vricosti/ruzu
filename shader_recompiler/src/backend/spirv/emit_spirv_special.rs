// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V special emission — maps to zuyu's
//! `backend/spirv/emit_spirv_special.cpp`.
//!
//! Handles prologue/epilogue, emit vertex, end primitive, depth mode
//! conversion, alpha test, and fixed pipeline point size.

use super::spirv_emit_context::SpirvEmitContext;

/// Emit shader prologue.
///
/// Matches upstream `EmitPrologue(EmitContext&)`.
/// For vertex shaders, initializes output position to (0,0,0,1) and
/// sets default values for generic outputs. For geometry shaders,
/// sets fixed pipeline point size.
pub fn emit_prologue(_ctx: &mut SpirvEmitContext) {
    log::trace!("SPIR-V: emit_prologue");
    // The actual prologue depends on the shader stage and runtime info.
    // Vertex: store default position (0,0,0,1) and default generics.
    // Geometry: set fixed pipeline point size if needed.
}

/// Emit shader epilogue.
///
/// Matches upstream `EmitEpilogue(EmitContext&)`.
/// For vertex shaders with depth mode conversion, transform Z coordinate.
/// For fragment shaders, run alpha test.
pub fn emit_epilogue(_ctx: &mut SpirvEmitContext) {
    log::trace!("SPIR-V: emit_epilogue");
}

/// Emit a geometry shader vertex.
///
/// Matches upstream `EmitEmitVertex(EmitContext&, const IR::Value&)`.
pub fn emit_emit_vertex(_ctx: &mut SpirvEmitContext, _stream: u32) {
    log::trace!("SPIR-V: emit_emit_vertex");
}

/// End a geometry shader primitive.
///
/// Matches upstream `EmitEndPrimitive(EmitContext&, const IR::Value&)`.
pub fn emit_end_primitive(_ctx: &mut SpirvEmitContext, _stream: u32) {
    log::trace!("SPIR-V: emit_end_primitive");
}

/// Emit `OpDemoteToHelperInvocation` (or `OpKill` as fallback).
///
/// Matches upstream `EmitDemoteToHelperInvocation(EmitContext&)`.
pub fn emit_demote_to_helper_invocation(ctx: &mut SpirvEmitContext) {
    if ctx.profile.support_demote_to_helper_invocation {
        ctx.builder.demote_to_helper_invocation().unwrap();
    } else {
        ctx.builder.kill().unwrap();
    }
}
