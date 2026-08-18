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
    if matches!(ctx.stage, crate::stage::Stage::Fragment) && program.info.stores_frag_color[0] {
        emit_glsl_special::emit_fragment_alpha_test(&mut ctx);
    }
    // Upstream chooses the version specifier after emitting the program,
    // because only then is `uses_y_direction` known. Fixed-function material
    // state is available only in the compatibility profile; all other
    // shaders use the unsuffixed GLSL version directive.
    ctx.header.insert_str(
        0,
        if ctx.uses_y_direction {
            "#version 460 compatibility\n"
        } else {
            "#version 460\n"
        },
    );
    let mut header = std::mem::take(&mut ctx.header);
    if program.shared_memory_size > 0 {
        let max_size = profile.gl_max_compute_smem_size;
        let smem_size = if max_size > 0 {
            program.shared_memory_size.min(max_size)
        } else {
            program.shared_memory_size
        };
        header.push_str(&format!("shared uint smem[{}];", smem_size.div_ceil(4)));
    }
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::types::ShaderStage;

    #[test]
    fn glsl_declares_shared_memory_smem_like_upstream() {
        let mut profile = Profile::default();
        profile.gl_max_compute_smem_size = 0x20;
        let runtime_info = RuntimeInfo::default();
        let mut bindings = Bindings::default();
        let mut program = ir::Program::new(ShaderStage::Compute);
        program.shared_memory_size = 0x24;

        let source = emit_glsl(&profile, &runtime_info, &mut program, &mut bindings);

        assert!(source.contains("shared uint smem[8];"));
        assert!(source.starts_with("#version 460\n"));
    }
}
