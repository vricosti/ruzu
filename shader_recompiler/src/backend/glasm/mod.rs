// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLASM (GL Assembly) backend: emit NV_gpu_program assembly from IR.
//!
//! Maps to upstream `backend/glasm/`.
//!
//! Entry point is [`emit_glasm()`] which takes an IR program and returns
//! an assembly string suitable for `glProgramStringARB`.

pub mod reg_alloc;
pub mod glasm_emit_context;
pub mod emit_glasm;
pub mod emit_glasm_barriers;
pub mod emit_glasm_bitwise_conversion;
pub mod emit_glasm_composite;
pub mod emit_glasm_context_get_set;
pub mod emit_glasm_control_flow;
pub mod emit_glasm_convert;
pub mod emit_glasm_floating_point;
pub mod emit_glasm_integer;
pub mod emit_glasm_logical;
pub mod emit_glasm_memory;
pub mod emit_glasm_not_implemented;
pub mod emit_glasm_select;
pub mod emit_glasm_shared_memory;
pub mod emit_glasm_special;
pub mod emit_glasm_undefined;
pub mod emit_glasm_warp;
pub mod emit_glasm_image;

use crate::ir;
use crate::profile::Profile;
use crate::runtime_info::RuntimeInfo;
use crate::backend::bindings::Bindings;

/// Constant matching upstream `PROGRAM_LOCAL_PARAMETER_STORAGE_BUFFER_BASE`.
pub const PROGRAM_LOCAL_PARAMETER_STORAGE_BUFFER_BASE: u32 = 1;

/// Emit GLASM assembly from an IR program.
///
/// Returns the assembly string ready for `glProgramStringARB`.
pub fn emit_glasm(
    profile: &Profile,
    runtime_info: &RuntimeInfo,
    program: &ir::Program,
    bindings: &mut Bindings,
) -> String {
    let mut ctx = glasm_emit_context::EmitContext::new(program, bindings, profile, runtime_info);
    emit_glasm::emit_program(&mut ctx, program);
    ctx.code
}

/// Convenience overload without explicit bindings.
pub fn emit_glasm_default(
    profile: &Profile,
    runtime_info: &RuntimeInfo,
    program: &ir::Program,
) -> String {
    let mut bindings = Bindings::default();
    emit_glasm(profile, runtime_info, program, &mut bindings)
}
