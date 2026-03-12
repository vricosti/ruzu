// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `backend/spirv/emit_spirv_barriers.cpp`
//!
//! SPIR-V emission for barrier and memory fence instructions.

use super::spirv_context::EmitContext;

/// Emit SPIR-V barrier instruction.
pub fn emit_barrier(_ctx: &mut EmitContext) {
    todo!("EmitBarrier")
}

/// Emit SPIR-V workgroup memory barrier.
pub fn emit_workgroup_memory_barrier(_ctx: &mut EmitContext) {
    todo!("EmitWorkgroupMemoryBarrier")
}

/// Emit SPIR-V device memory barrier.
pub fn emit_device_memory_barrier(_ctx: &mut EmitContext) {
    todo!("EmitDeviceMemoryBarrier")
}
