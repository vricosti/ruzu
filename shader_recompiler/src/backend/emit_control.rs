// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V emission for control flow and special operations.

use rspirv::spirv;

use super::spirv_context::EmitContext;

pub fn emit_demote(ctx: &mut EmitContext) {
    // OpDemoteToHelperInvocation (SPIR-V 1.6+ / SPV_EXT_demote_to_helper_invocation)
    if ctx.profile.support_demote_to_helper {
        ctx.builder.demote_to_helper_invocation().unwrap();
    } else {
        // Fallback: OpKill
        ctx.builder.kill().unwrap();
    }
}

pub fn emit_barrier(ctx: &mut EmitContext) {
    // OpControlBarrier for workgroup synchronization
    let workgroup = ctx
        .builder
        .constant_bit32(ctx.u32_type, spirv::Scope::Workgroup as u32);
    let workgroup2 = ctx
        .builder
        .constant_bit32(ctx.u32_type, spirv::Scope::Workgroup as u32);
    let acquire_release = ctx.builder.constant_bit32(
        ctx.u32_type,
        spirv::MemorySemantics::WORKGROUP_MEMORY.bits()
            | spirv::MemorySemantics::ACQUIRE_RELEASE.bits(),
    );
    ctx.builder
        .control_barrier(workgroup, workgroup2, acquire_release)
        .unwrap();
}
