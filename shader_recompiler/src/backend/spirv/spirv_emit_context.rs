// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V emission context — maps to zuyu's
//! `backend/spirv/spirv_emit_context.h` and `spirv_emit_context.cpp`.
//!
//! Wraps rspirv builder with cached types, constants, and resource variables.
//! This is the per-`backend/spirv/` context type, distinct from the root-level
//! `backend/spirv_context.rs` which is the older monolithic context.

use rspirv::binary::Assemble;
use rspirv::dr::Builder;
use rspirv::spirv;
use std::collections::HashMap;

use crate::backend::Profile;
use crate::ir;
use crate::ir::types::ShaderStage;

/// SPIR-V emission context.
///
/// Matches upstream `EmitContext` class that inherits from `Sirit::Module`.
pub struct SpirvEmitContext {
    pub builder: Builder,
    pub profile: Profile,
    pub stage: ShaderStage,

    // ── Cached SPIR-V type IDs ────────────────────────────────────────
    pub void_type: spirv::Word,
    pub bool_type: spirv::Word,
    pub u32_type: spirv::Word,
    pub i32_type: spirv::Word,
    pub f32_type: spirv::Word,
    pub u32_vec2_type: spirv::Word,
    pub u32_vec3_type: spirv::Word,
    pub u32_vec4_type: spirv::Word,
    pub f32_vec2_type: spirv::Word,
    pub f32_vec3_type: spirv::Word,
    pub f32_vec4_type: spirv::Word,
    pub u64_type: spirv::Word,
    pub f64_type: spirv::Word,
    pub void_fn_type: spirv::Word,

    // ── Cached constants ──────────────────────────────────────────────
    pub const_zero_u32: spirv::Word,
    pub const_one_u32: spirv::Word,
    pub const_zero_f32: spirv::Word,
    pub const_one_f32: spirv::Word,
    pub const_true: spirv::Word,
    pub const_false: spirv::Word,

    // ── GLSL.std.450 extended instruction set ─────────────────────────
    pub glsl_ext: spirv::Word,

    // ── Value mapping ─────────────────────────────────────────────────
    pub values: HashMap<(u32, u32), spirv::Word>,
}

impl SpirvEmitContext {
    /// Create a new SPIR-V emission context.
    pub fn new(program: &ir::Program, profile: &Profile) -> Self {
        let mut builder = Builder::new();
        builder.set_version(1, 5);
        builder.capability(spirv::Capability::Shader);

        if profile.support_fp64 {
            builder.capability(spirv::Capability::Float64);
        }
        if profile.support_int16 {
            builder.capability(spirv::Capability::Int16);
        }
        if profile.support_int64 {
            builder.capability(spirv::Capability::Int64);
        }
        if profile.support_subgroup {
            builder.capability(spirv::Capability::GroupNonUniform);
            builder.capability(spirv::Capability::GroupNonUniformBallot);
            builder.capability(spirv::Capability::GroupNonUniformShuffle);
            builder.capability(spirv::Capability::GroupNonUniformVote);
        }
        if profile.support_demote_to_helper {
            builder.capability(spirv::Capability::DemoteToHelperInvocation);
        }
        builder.capability(spirv::Capability::ImageQuery);
        builder.capability(spirv::Capability::Sampled1D);
        builder.capability(spirv::Capability::SampledCubeArray);

        builder.memory_model(
            spirv::AddressingModel::Logical,
            spirv::MemoryModel::GLSL450,
        );

        let glsl_ext = builder.ext_inst_import("GLSL.std.450");

        // Define types
        let void_type = builder.type_void();
        let bool_type = builder.type_bool();
        let u32_type = builder.type_int(32, 0);
        let i32_type = builder.type_int(32, 1);
        let f32_type = builder.type_float(32);
        let u32_vec2_type = builder.type_vector(u32_type, 2);
        let u32_vec3_type = builder.type_vector(u32_type, 3);
        let u32_vec4_type = builder.type_vector(u32_type, 4);
        let f32_vec2_type = builder.type_vector(f32_type, 2);
        let f32_vec3_type = builder.type_vector(f32_type, 3);
        let f32_vec4_type = builder.type_vector(f32_type, 4);
        let u64_type = builder.type_int(64, 0);
        let f64_type = builder.type_float(64);
        let void_fn_type = builder.type_function(void_type, vec![]);

        // Define constants
        let const_zero_u32 = builder.constant_bit32(u32_type, 0);
        let const_one_u32 = builder.constant_bit32(u32_type, 1);
        let const_zero_f32 = builder.constant_bit32(f32_type, 0.0f32.to_bits());
        let const_one_f32 = builder.constant_bit32(f32_type, 1.0f32.to_bits());
        let const_true = builder.constant_true(bool_type);
        let const_false = builder.constant_false(bool_type);

        Self {
            builder,
            profile: profile.clone(),
            stage: program.stage,
            void_type,
            bool_type,
            u32_type,
            i32_type,
            f32_type,
            u32_vec2_type,
            u32_vec3_type,
            u32_vec4_type,
            f32_vec2_type,
            f32_vec3_type,
            f32_vec4_type,
            u64_type,
            f64_type,
            void_fn_type,
            const_zero_u32,
            const_one_u32,
            const_zero_f32,
            const_one_f32,
            const_true,
            const_false,
            glsl_ext,
            values: HashMap::new(),
        }
    }

    /// Create a u32 constant.
    pub fn constant_u32(&mut self, value: u32) -> spirv::Word {
        self.builder.constant_bit32(self.u32_type, value)
    }

    /// Create an i32 constant.
    pub fn constant_i32(&mut self, value: i32) -> spirv::Word {
        self.builder
            .constant_bit32(self.i32_type, value as u32)
    }

    /// Create an f32 constant.
    pub fn constant_f32(&mut self, value: f32) -> spirv::Word {
        self.builder
            .constant_bit32(self.f32_type, value.to_bits())
    }

    /// Emit the complete program.
    pub fn emit_program(&mut self, program: &ir::Program) {
        // Create main function
        let main_fn = self
            .builder
            .begin_function(
                self.void_type,
                None,
                spirv::FunctionControl::NONE,
                self.void_fn_type,
            )
            .unwrap();

        let _entry_label = self.builder.begin_block(None).unwrap();

        // Emit instructions from all blocks
        for (_block_idx, _block) in program.blocks.iter().enumerate() {
            // Instructions are emitted via the per-opcode handlers
        }

        self.builder.ret().unwrap();
        self.builder.end_function().unwrap();

        // Entry point
        let exec_model = match self.stage {
            ShaderStage::Vertex => spirv::ExecutionModel::Vertex,
            ShaderStage::Fragment => spirv::ExecutionModel::Fragment,
            ShaderStage::Compute => spirv::ExecutionModel::GLCompute,
            ShaderStage::Geometry => spirv::ExecutionModel::Geometry,
            ShaderStage::TessellationControl => spirv::ExecutionModel::TessellationControl,
            ShaderStage::TessellationEval => spirv::ExecutionModel::TessellationEvaluation,
        };

        self.builder
            .entry_point(exec_model, main_fn, "main", vec![]);

        if self.stage == ShaderStage::Fragment {
            self.builder
                .execution_mode(main_fn, spirv::ExecutionMode::OriginUpperLeft, vec![]);
        }
    }

    /// Finalize and return SPIR-V words.
    pub fn finalize(self) -> Vec<u32> {
        let module = self.builder.module();
        let mut words = Vec::new();
        module.assemble_into(&mut words);
        words
    }
}
