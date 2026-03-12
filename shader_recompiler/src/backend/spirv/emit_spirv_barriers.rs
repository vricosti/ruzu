// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V barrier emission — maps to zuyu's `backend/spirv/emit_spirv_barriers.cpp`.

use rspirv::spirv;
use super::spirv_emit_context::SpirvEmitContext;

/// Emit a memory barrier at the given scope.
///
/// Matches upstream anonymous `MemoryBarrier(EmitContext&, spv::Scope)`.
fn memory_barrier(ctx: &mut SpirvEmitContext, scope: spirv::Scope) {
    let semantics = spirv::MemorySemantics::ACQUIRE_RELEASE
        | spirv::MemorySemantics::UNIFORM_MEMORY
        | spirv::MemorySemantics::WORKGROUP_MEMORY
        | spirv::MemorySemantics::ATOMIC_COUNTER_MEMORY
        | spirv::MemorySemantics::IMAGE_MEMORY;
    let scope_id = ctx.constant_u32(scope as u32);
    let semantics_id = ctx.constant_u32(semantics.bits());
    ctx.builder.memory_barrier(scope_id, semantics_id).unwrap();
}

/// Emit a workgroup control barrier.
///
/// Matches upstream `EmitBarrier(EmitContext&)`.
pub fn emit_barrier(ctx: &mut SpirvEmitContext) {
    let execution = spirv::Scope::Workgroup;
    let memory = spirv::Scope::Workgroup;
    let memory_semantics =
        spirv::MemorySemantics::ACQUIRE_RELEASE | spirv::MemorySemantics::WORKGROUP_MEMORY;

    let execution_id = ctx.constant_u32(execution as u32);
    let memory_id = ctx.constant_u32(memory as u32);
    let semantics_id = ctx.constant_u32(memory_semantics.bits());
    ctx.builder
        .control_barrier(execution_id, memory_id, semantics_id)
        .unwrap();
}

/// Emit a workgroup memory barrier.
///
/// Matches upstream `EmitWorkgroupMemoryBarrier(EmitContext&)`.
pub fn emit_workgroup_memory_barrier(ctx: &mut SpirvEmitContext) {
    memory_barrier(ctx, spirv::Scope::Workgroup);
}

/// Emit a device memory barrier.
///
/// Matches upstream `EmitDeviceMemoryBarrier(EmitContext&)`.
pub fn emit_device_memory_barrier(ctx: &mut SpirvEmitContext) {
    memory_barrier(ctx, spirv::Scope::Device);
}
