// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Shader backend: emits target-specific binary from IR.
//!
//! The primary backend is SPIR-V via the `spirv/` subdirectory, which contains
//! one Rust file per upstream `backend/spirv/emit_spirv_*.cpp` source file.
//!
//! The entry point is `emit_spirv()`, which takes an `IR::Program` and a
//! `Profile` and returns a `Vec<u32>` of SPIR-V words.

pub mod bindings;
pub mod glasm;
pub mod glsl;
pub mod spirv;

use crate::ir;
use crate::profile::Profile;
use crate::runtime_info::RuntimeInfo;

/// Emit SPIR-V binary from an IR program.
///
/// Returns the SPIR-V words ready to be loaded into a VkShaderModule.
/// Delegates to `spirv::emit_spirv::emit_spirv`.
///
/// `profile` is the upstream-faithful `Shader::Profile` (in
/// `crate::profile`). The previous duplicate `backend::Profile` was
/// removed when the SPIR-V and GLSL backends were unified onto a
/// single Profile type.
pub fn emit_spirv(
    program: &ir::Program,
    profile: &Profile,
    runtime_info: &RuntimeInfo,
) -> Vec<u32> {
    spirv::emit_spirv::emit_spirv(program, profile, runtime_info)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::basic_block::Block;
    use crate::ir::emitter::Emitter;
    use crate::ir::types::ShaderStage;
    use crate::ir::value::Value;
    use crate::runtime_info::RuntimeInfo;

    #[test]
    fn test_emit_empty_vertex_shader() {
        let mut program = ir::Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());

        let profile = Profile::default();
        let words = emit_spirv(&program, &profile, &RuntimeInfo::default());

        // SPIR-V magic number is 0x07230203
        assert!(words.len() >= 5, "SPIR-V should have at least a header");
        assert_eq!(words[0], 0x07230203, "SPIR-V magic number mismatch");
    }

    #[test]
    fn test_emit_empty_fragment_shader() {
        let mut program = ir::Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());

        let profile = Profile::default();
        let words = emit_spirv(&program, &profile, &RuntimeInfo::default());

        assert!(words.len() >= 5);
        assert_eq!(words[0], 0x07230203);
    }

    #[test]
    fn test_emit_vertex_with_arithmetic() {
        let mut program = ir::Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());

        {
            let mut emitter = Emitter::new(&mut program, 0);
            // Create a simple FP add
            let a = emitter.fp_add_32(Value::ImmF32(1.0), Value::ImmF32(2.0));
            let b = emitter.fp_mul_32(a, Value::ImmF32(3.0));
            let _ = b; // Result is not stored but instructions should emit
        }

        let profile = Profile::default();
        let words = emit_spirv(&program, &profile, &RuntimeInfo::default());

        assert!(words.len() > 5, "Should have more than just a header");
        assert_eq!(words[0], 0x07230203);
    }

    #[test]
    fn test_emit_fragment_with_output() {
        let mut program = ir::Program::new(ShaderStage::Fragment);
        // Fragment doesn't store generics (VaryingState defaults to all-zero).
        program.blocks.push(Block::new());

        {
            let mut emitter = Emitter::new(&mut program, 0);
            let _ = emitter.iadd_32(Value::ImmU32(1), Value::ImmU32(2));
        }

        let profile = Profile::default();
        let words = emit_spirv(&program, &profile, &RuntimeInfo::default());

        assert!(words.len() >= 5);
        assert_eq!(words[0], 0x07230203);
    }

    #[test]
    fn test_emit_with_cbuf_descriptors() {
        let mut program = ir::Program::new(ShaderStage::VertexB);
        program
            .info
            .constant_buffer_descriptors
            .push(crate::ir::program::CbufDescriptor { index: 0, count: 1 });
        program.blocks.push(Block::new());

        {
            let mut emitter = Emitter::new(&mut program, 0);
            let _ = emitter.get_cbuf_u32(Value::ImmU32(0), Value::ImmU32(0));
        }

        let profile = Profile::default();
        let words = emit_spirv(&program, &profile, &RuntimeInfo::default());

        assert!(words.len() > 5);
        assert_eq!(words[0], 0x07230203);
    }

    #[test]
    fn test_profile_default() {
        let profile = Profile::default();
        assert_eq!(profile.supported_spirv, 0x00010000);
        assert!(profile.support_demote_to_helper_invocation);
    }
}
