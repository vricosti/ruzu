// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V control flow emission — maps to zuyu's
//! `backend/spirv/emit_spirv_control_flow.cpp`.

use super::spirv_emit_context::SpirvEmitContext;

/// Emit a demote-to-helper-invocation.
///
/// Matches upstream `EmitDemoteToHelperInvocation(EmitContext&)`.
/// If the profile supports `OpDemoteToHelperInvocation`, use it directly.
/// Otherwise, fall back to a conditional `OpKill` pattern.
pub fn emit_demote_to_helper_invocation(ctx: &mut SpirvEmitContext) {
    if ctx.profile.support_demote_to_helper_invocation {
        ctx.builder.demote_to_helper_invocation().unwrap();
    } else {
        // Fallback: conditional kill
        // Create labels for the kill path and the merge path
        let kill_label = ctx.builder.id();
        let impossible_label = ctx.builder.id();

        ctx.builder
            .selection_merge(impossible_label, rspirv::spirv::SelectionControl::NONE)
            .unwrap();
        ctx.builder
            .branch_conditional(ctx.const_true, kill_label, impossible_label, vec![])
            .unwrap();

        ctx.builder.begin_block(Some(kill_label)).unwrap();
        ctx.builder.kill().unwrap();

        ctx.builder.begin_block(Some(impossible_label)).unwrap();
    }
}

/// Emit a Join instruction — this should never be emitted in SPIR-V.
///
/// Matches upstream `EmitJoin(EmitContext&)` which throws NotImplementedException.
pub fn emit_join(_ctx: &mut SpirvEmitContext) {
    panic!("Join shouldn't be emitted to SPIR-V");
}
