// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `backend/spirv/emit_spirv_special.cpp`
//!
//! SPIR-V emission for special operations: phi nodes, identity,
//! prologue/epilogue, demote, and other control operations.

use super::spirv_context::EmitContext;

/// Emit SPIR-V phi node.
pub fn emit_phi(_ctx: &mut EmitContext) {
    todo!("EmitPhi")
}

/// Emit SPIR-V void (no-op).
pub fn emit_void(_ctx: &mut EmitContext) {
    // No SPIR-V instruction needed for void
}

/// Emit SPIR-V identity (pass-through).
pub fn emit_identity(_ctx: &mut EmitContext) {
    todo!("EmitIdentity")
}

/// Emit SPIR-V prologue.
pub fn emit_prologue(_ctx: &mut EmitContext) {
    todo!("EmitPrologue")
}

/// Emit SPIR-V epilogue.
pub fn emit_epilogue(_ctx: &mut EmitContext) {
    todo!("EmitEpilogue")
}

/// Emit SPIR-V DemoteToHelperInvocation.
pub fn emit_demote_to_helper_invocation(_ctx: &mut EmitContext) {
    todo!("EmitDemoteToHelperInvocation")
}

/// Emit SPIR-V EmitVertex.
pub fn emit_emit_vertex(_ctx: &mut EmitContext) {
    todo!("EmitEmitVertex")
}

/// Emit SPIR-V EndPrimitive.
pub fn emit_end_primitive(_ctx: &mut EmitContext) {
    todo!("EmitEndPrimitive")
}

/// Emit SPIR-V for reading the invocation ID.
pub fn emit_invocation_id(_ctx: &mut EmitContext) {
    todo!("EmitInvocationId")
}

/// Emit SPIR-V for reading the sample ID.
pub fn emit_sample_id(_ctx: &mut EmitContext) {
    todo!("EmitSampleId")
}

/// Emit SPIR-V for is_helper_invocation check.
pub fn emit_is_helper_invocation(_ctx: &mut EmitContext) {
    todo!("EmitIsHelperInvocation")
}
