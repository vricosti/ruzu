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
use crate::runtime_info::RuntimeInfo;

/// GPU/driver capability profile.
#[derive(Debug, Clone)]
pub struct Profile {
    pub spirv_version: u32,
    pub unified_descriptor_binding: bool,
    pub support_fp16: bool,
    pub support_fp64: bool,
    pub support_int8: bool,
    pub support_int16: bool,
    pub support_int64: bool,
    pub support_subgroup: bool,
    pub support_demote_to_helper: bool,
    pub has_broken_fp_clamp: bool,
}

impl Default for Profile {
    fn default() -> Self {
        Self {
            spirv_version: 0x00010500, // SPIR-V 1.5
            unified_descriptor_binding: false,
            support_fp16: false,
            support_fp64: false,
            support_int8: false,
            support_int16: false,
            support_int64: false,
            support_subgroup: true,
            support_demote_to_helper: true,
            has_broken_fp_clamp: false,
        }
    }
}

/// Emit SPIR-V binary from an IR program.
///
/// Returns the SPIR-V words ready to be loaded into a VkShaderModule.
/// Delegates to `spirv::emit_spirv::emit_spirv`.
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
        let mut program = ir::Program::new(ShaderStage::Vertex);
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
        let mut program = ir::Program::new(ShaderStage::Vertex);
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
        program.info.stores_generics = 0; // Fragment doesn't store generics
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
        let mut program = ir::Program::new(ShaderStage::Vertex);
        program
            .info
            .constant_buffer_descriptors
            .push(crate::ir::program::CbufDescriptor {
                index: 0,
                size: 0x10000,
            });
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
        assert_eq!(profile.spirv_version, 0x00010500);
        assert!(!profile.support_fp16);
        assert!(profile.support_subgroup);
        assert!(profile.support_demote_to_helper);
    }
}
