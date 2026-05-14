// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLSL backend: emit GLSL source from IR.
//!
//! Maps to upstream `backend/glsl/`.
//!
//! Entry point is [`emit_glsl()`] which takes an IR program and returns
//! a GLSL source string.

pub mod emit_glsl;
pub mod emit_glsl_atomic;
pub mod emit_glsl_barriers;
pub mod emit_glsl_bitwise_conversion;
pub mod emit_glsl_composite;
pub mod emit_glsl_context_get_set;
pub mod emit_glsl_control_flow;
pub mod emit_glsl_convert;
pub mod emit_glsl_floating_point;
pub mod emit_glsl_image;
pub mod emit_glsl_integer;
pub mod emit_glsl_logical;
pub mod emit_glsl_memory;
pub mod emit_glsl_not_implemented;
pub mod emit_glsl_select;
pub mod emit_glsl_shared_memory;
pub mod emit_glsl_special;
pub mod emit_glsl_undefined;
pub mod emit_glsl_warp;
pub mod glsl_emit_context;
pub mod var_alloc;

use crate::backend::bindings::Bindings;
use crate::ir;
use crate::profile::Profile;
use crate::runtime_info::RuntimeInfo;

/// Emit GLSL source from an IR program.
///
/// Returns the GLSL source string.
pub fn emit_glsl(
    profile: &Profile,
    runtime_info: &RuntimeInfo,
    program: &mut ir::Program,
    bindings: &mut Bindings,
) -> String {
    let mut ctx = glsl_emit_context::EmitContext::new(program, bindings, profile, runtime_info);
    emit_glsl::emit_program(&mut ctx, program);
    let mut header = std::mem::take(&mut ctx.header);
    header.push_str("void main(){\n");
    if program.local_memory_size > 0 {
        header.push_str(&format!(
            "uint lmem[{}];\n",
            program.local_memory_size.div_ceil(4)
        ));
    }
    ctx.define_variables(&mut header);
    let code = std::mem::take(&mut ctx.code);
    format!("{}{}{}", header, code, "}\n")
}

/// Convenience overload without explicit bindings.
pub fn emit_glsl_default(profile: &Profile, program: &ir::Program) -> String {
    let mut bindings = Bindings::default();
    let runtime_info = RuntimeInfo::default();
    let mut program = program.clone();
    emit_glsl(profile, &runtime_info, &mut program, &mut bindings)
}
