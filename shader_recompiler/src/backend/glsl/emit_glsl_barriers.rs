// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLSL barrier emission.
//!
//! Maps to upstream `backend/glsl/emit_glsl_barriers.cpp`.

use super::glsl_emit_context::EmitContext;

pub fn emit_barrier(ctx: &mut EmitContext) { ctx.add_line("barrier();"); }
pub fn emit_workgroup_memory_barrier(ctx: &mut EmitContext) { ctx.add_line("groupMemoryBarrier();"); }
pub fn emit_device_memory_barrier(ctx: &mut EmitContext) { ctx.add_line("memoryBarrier();"); }
