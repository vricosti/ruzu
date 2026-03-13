// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V emission context — maps to zuyu's
//! `backend/spirv/spirv_emit_context.h` and `spirv_emit_context.cpp`.
//!
//! Wraps rspirv builder with cached types, constants, and resource variables.

use rspirv::binary::Assemble;
use rspirv::dr::{Builder, Operand};
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

    // ── Pointer types ─────────────────────────────────────────────────
    pub input_f32_ptr: spirv::Word,
    pub output_f32_ptr: spirv::Word,
    pub uniform_u32_ptr: spirv::Word,
    pub input_u32_ptr: spirv::Word,
    pub output_u32_ptr: spirv::Word,
    pub uniform_f32_ptr: spirv::Word,

    // ── Cached constants ──────────────────────────────────────────────
    pub const_zero_u32: spirv::Word,
    pub const_one_u32: spirv::Word,
    pub const_zero_f32: spirv::Word,
    pub const_one_f32: spirv::Word,
    pub const_true: spirv::Word,
    pub const_false: spirv::Word,

    // ── GLSL.std.450 extended instruction set ─────────────────────────
    pub glsl_ext: spirv::Word,

    // ── Resources ─────────────────────────────────────────────────────
    /// Constant buffer UBO variables, indexed by CB index.
    pub cbuf_vars: HashMap<u32, spirv::Word>,
    /// Texture combined image sampler variables, indexed by descriptor index.
    pub texture_vars: HashMap<u32, spirv::Word>,
    /// Input variables (vertex attributes / fragment varyings).
    pub input_vars: HashMap<u32, spirv::Word>,
    /// Output variables (vertex outputs / fragment colors).
    pub output_vars: HashMap<u32, spirv::Word>,

    // ── Value mapping ─────────────────────────────────────────────────
    /// Maps IR instruction references (block, inst) to SPIR-V result IDs.
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

        // Define scalar types
        let void_type = builder.type_void();
        let bool_type = builder.type_bool();
        let u32_type = builder.type_int(32, 0);
        let i32_type = builder.type_int(32, 1);
        let f32_type = builder.type_float(32);

        // Define vector types
        let u32_vec2_type = builder.type_vector(u32_type, 2);
        let u32_vec3_type = builder.type_vector(u32_type, 3);
        let u32_vec4_type = builder.type_vector(u32_type, 4);
        let f32_vec2_type = builder.type_vector(f32_type, 2);
        let f32_vec3_type = builder.type_vector(f32_type, 3);
        let f32_vec4_type = builder.type_vector(f32_type, 4);

        // 64-bit types
        let u64_type = builder.type_int(64, 0);
        let f64_type = builder.type_float(64);

        // Function type: void(void)
        let void_fn_type = builder.type_function(void_type, vec![]);

        // Pointer types
        let input_f32_ptr = builder.type_pointer(None, spirv::StorageClass::Input, f32_type);
        let output_f32_ptr = builder.type_pointer(None, spirv::StorageClass::Output, f32_type);
        let input_u32_ptr = builder.type_pointer(None, spirv::StorageClass::Input, u32_type);
        let output_u32_ptr = builder.type_pointer(None, spirv::StorageClass::Output, u32_type);
        let uniform_u32_ptr = builder.type_pointer(None, spirv::StorageClass::Uniform, u32_type);
        let uniform_f32_ptr = builder.type_pointer(None, spirv::StorageClass::Uniform, f32_type);

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
            input_f32_ptr,
            output_f32_ptr,
            uniform_u32_ptr,
            input_u32_ptr,
            output_u32_ptr,
            uniform_f32_ptr,
            const_zero_u32,
            const_one_u32,
            const_zero_f32,
            const_one_f32,
            const_true,
            const_false,
            glsl_ext,
            cbuf_vars: HashMap::new(),
            texture_vars: HashMap::new(),
            input_vars: HashMap::new(),
            output_vars: HashMap::new(),
            values: HashMap::new(),
        }
    }

    /// Create a u32 constant.
    pub fn constant_u32(&mut self, value: u32) -> spirv::Word {
        self.builder.constant_bit32(self.u32_type, value)
    }

    /// Create an i32 constant.
    pub fn constant_i32(&mut self, value: i32) -> spirv::Word {
        self.builder.constant_bit32(self.i32_type, value as u32)
    }

    /// Create an f32 constant.
    pub fn constant_f32(&mut self, value: f32) -> spirv::Word {
        self.builder.constant_bit32(self.f32_type, value.to_bits())
    }

    /// Define global variables (inputs, outputs, UBOs, textures) from shader info.
    pub fn define_global_variables(&mut self, program: &ir::Program) {
        let info = &program.info;

        // Input variables
        match self.stage {
            ShaderStage::Vertex => {
                for i in 0..32u32 {
                    if info.loads_generics & (1 << i) != 0 {
                        let vec4_ptr = self.builder.type_pointer(
                            None,
                            spirv::StorageClass::Input,
                            self.f32_vec4_type,
                        );
                        let var = self.builder.variable(
                            vec4_ptr,
                            None,
                            spirv::StorageClass::Input,
                            None,
                        );
                        self.builder.decorate(
                            var,
                            spirv::Decoration::Location,
                            vec![Operand::LiteralBit32(i)],
                        );
                        self.input_vars.insert(i, var);
                    }
                }
            }
            ShaderStage::Fragment => {
                for i in 0..32u32 {
                    if info.loads_generics & (1 << i) != 0 {
                        let vec4_ptr = self.builder.type_pointer(
                            None,
                            spirv::StorageClass::Input,
                            self.f32_vec4_type,
                        );
                        let var = self.builder.variable(
                            vec4_ptr,
                            None,
                            spirv::StorageClass::Input,
                            None,
                        );
                        self.builder.decorate(
                            var,
                            spirv::Decoration::Location,
                            vec![Operand::LiteralBit32(i)],
                        );
                        self.input_vars.insert(i, var);
                    }
                }
            }
            _ => {}
        }

        // Output variables
        match self.stage {
            ShaderStage::Vertex => {
                if info.stores_position {
                    let vec4_ptr = self.builder.type_pointer(
                        None,
                        spirv::StorageClass::Output,
                        self.f32_vec4_type,
                    );
                    let var = self.builder.variable(
                        vec4_ptr,
                        None,
                        spirv::StorageClass::Output,
                        None,
                    );
                    self.builder.decorate(
                        var,
                        spirv::Decoration::BuiltIn,
                        vec![Operand::BuiltIn(spirv::BuiltIn::Position)],
                    );
                    self.output_vars.insert(0xFFFF_0000, var); // Special key for gl_Position
                }
                for i in 0..32u32 {
                    if info.stores_generics & (1 << i) != 0 {
                        let vec4_ptr = self.builder.type_pointer(
                            None,
                            spirv::StorageClass::Output,
                            self.f32_vec4_type,
                        );
                        let var = self.builder.variable(
                            vec4_ptr,
                            None,
                            spirv::StorageClass::Output,
                            None,
                        );
                        self.builder.decorate(
                            var,
                            spirv::Decoration::Location,
                            vec![Operand::LiteralBit32(i)],
                        );
                        self.output_vars.insert(i, var);
                    }
                }
            }
            ShaderStage::Fragment => {
                let vec4_ptr = self.builder.type_pointer(
                    None,
                    spirv::StorageClass::Output,
                    self.f32_vec4_type,
                );
                let var = self.builder.variable(
                    vec4_ptr,
                    None,
                    spirv::StorageClass::Output,
                    None,
                );
                self.builder.decorate(
                    var,
                    spirv::Decoration::Location,
                    vec![Operand::LiteralBit32(0)],
                );
                self.output_vars.insert(0, var);
            }
            _ => {}
        }

        // Constant buffers (UBOs)
        for desc in &info.constant_buffer_descriptors {
            let array_len = 4096u32; // 0x10000 bytes / 4 = 4096 u32s
            let array_len_const = self.builder.constant_bit32(self.u32_type, array_len);
            let array_type = self.builder.type_array(self.u32_type, array_len_const);
            self.builder.decorate(
                array_type,
                spirv::Decoration::ArrayStride,
                vec![Operand::LiteralBit32(4)],
            );

            let struct_type = self.builder.type_struct(vec![array_type]);
            self.builder.decorate(struct_type, spirv::Decoration::Block, vec![]);
            self.builder.member_decorate(
                struct_type,
                0,
                spirv::Decoration::Offset,
                vec![Operand::LiteralBit32(0)],
            );

            let ptr_type =
                self.builder.type_pointer(None, spirv::StorageClass::Uniform, struct_type);
            let var =
                self.builder.variable(ptr_type, None, spirv::StorageClass::Uniform, None);
            self.builder.decorate(
                var,
                spirv::Decoration::DescriptorSet,
                vec![Operand::LiteralBit32(0)],
            );
            self.builder.decorate(
                var,
                spirv::Decoration::Binding,
                vec![Operand::LiteralBit32(desc.index)],
            );

            self.cbuf_vars.insert(desc.index, var);
        }

        // Textures (combined image samplers)
        for desc in &info.texture_descriptors {
            let image_type = self.builder.type_image(
                self.f32_type,
                spirv::Dim::Dim2D,
                0,
                0,
                0,
                1,
                spirv::ImageFormat::Unknown,
                None,
            );
            let sampled_image = self.builder.type_sampled_image(image_type);
            let ptr_type = self.builder.type_pointer(
                None,
                spirv::StorageClass::UniformConstant,
                sampled_image,
            );
            let var = self.builder.variable(
                ptr_type,
                None,
                spirv::StorageClass::UniformConstant,
                None,
            );
            self.builder.decorate(
                var,
                spirv::Decoration::DescriptorSet,
                vec![Operand::LiteralBit32(1)],
            );
            self.builder.decorate(
                var,
                spirv::Decoration::Binding,
                vec![Operand::LiteralBit32(desc.index)],
            );

            self.texture_vars.insert(desc.index, var);
        }
    }

    /// Define the main() function and emit IR instructions as SPIR-V.
    pub fn define_main_function(&mut self, program: &ir::Program) {
        // Collect interface variables for the entry point
        let mut interface: Vec<spirv::Word> = Vec::new();
        for &var in self.input_vars.values() {
            interface.push(var);
        }
        for &var in self.output_vars.values() {
            interface.push(var);
        }

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

        // Create entry block label
        let _entry_label = self.builder.begin_block(None).unwrap();

        // Emit instructions from all blocks
        for (block_idx, block) in program.blocks.iter().enumerate() {
            for (inst_idx, inst) in block.instructions.iter().enumerate() {
                self.emit_instruction(inst, block_idx as u32, inst_idx as u32);
            }
        }

        // Return
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

        self.builder.entry_point(exec_model, main_fn, "main", interface);

        if self.stage == ShaderStage::Fragment {
            self.builder
                .execution_mode(main_fn, spirv::ExecutionMode::OriginUpperLeft, vec![]);
        }
    }

    /// Emit a single IR instruction as SPIR-V.
    fn emit_instruction(&mut self, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
        use ir::Opcode;
        match inst.opcode {
            // ── FP32 arithmetic ───────────────────────────────────────
            Opcode::FPAdd32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_floating_point::emit_fp_add_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPSub32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_floating_point::emit_fp_sub_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPMul32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_floating_point::emit_fp_mul_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPDiv32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_floating_point::emit_fp_div_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPFma32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let c = self.resolve_value(inst.arg(2));
                let id = super::emit_spirv_floating_point::emit_fp_fma_32(self, a, b, c);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPNeg32 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_floating_point::emit_fp_neg_32(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPAbs32 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_floating_point::emit_fp_abs_32(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPSaturate32 => {
                let a = self.resolve_value(inst.arg(0));
                let zero = self.const_zero_f32;
                let one = self.const_one_f32;
                let id = super::emit_spirv_floating_point::emit_fp_clamp_32(self, a, zero, one);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPMin32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_floating_point::emit_fp_min_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPMax32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_floating_point::emit_fp_max_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPSin => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_floating_point::emit_fp_sin(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPCos => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_floating_point::emit_fp_cos(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPExp2 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_floating_point::emit_fp_exp2(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPLog2 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_floating_point::emit_fp_log2(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPSqrt32 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_floating_point::emit_fp_sqrt_32(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPRecip32 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_floating_point::emit_fp_recip_32(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPRecipSqrt32 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_floating_point::emit_fp_recip_sqrt_32(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPFloor32 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_floating_point::emit_fp_floor_32(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPCeil32 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_floating_point::emit_fp_ceil_32(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPTrunc32 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_floating_point::emit_fp_trunc_32(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPRoundEven32 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_floating_point::emit_fp_round_even_32(self, a);
                self.set_value(block_idx, inst_idx, id);
            }

            // ── FP comparison ─────────────────────────────────────────
            Opcode::FPOrdEqual32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_floating_point::emit_fp_ord_equal_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPOrdNotEqual32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_floating_point::emit_fp_ord_not_equal_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPOrdLessThan32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_floating_point::emit_fp_ord_less_than_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPOrdGreaterThan32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id =
                    super::emit_spirv_floating_point::emit_fp_ord_greater_than_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPOrdLessThanEqual32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id =
                    super::emit_spirv_floating_point::emit_fp_ord_less_than_equal_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPOrdGreaterThanEqual32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_floating_point::emit_fp_ord_greater_than_equal_32(
                    self, a, b,
                );
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPUnordEqual32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_floating_point::emit_fp_unord_equal_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPUnordNotEqual32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id =
                    super::emit_spirv_floating_point::emit_fp_unord_not_equal_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPUnordLessThan32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                // Use FPOrdLessThan for now (unord variant falls back)
                let id = super::emit_spirv_floating_point::emit_fp_ord_less_than_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPUnordGreaterThan32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id =
                    super::emit_spirv_floating_point::emit_fp_ord_greater_than_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPIsNan32 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_floating_point::emit_fp_is_nan_32(self, a);
                self.set_value(block_idx, inst_idx, id);
            }

            // ── Integer arithmetic ────────────────────────────────────
            Opcode::IAdd32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_integer::emit_iadd_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::ISub32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_integer::emit_isub_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::IMul32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_integer::emit_imul_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::INeg32 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_integer::emit_ineg_32(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::IAbs32 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_integer::emit_iabs_32(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::ShiftLeftLogical32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_integer::emit_shift_left_logical_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::ShiftRightLogical32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_integer::emit_shift_right_logical_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::ShiftRightArithmetic32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_integer::emit_shift_right_arithmetic_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::BitwiseAnd32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_integer::emit_bitwise_and_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::BitwiseOr32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_integer::emit_bitwise_or_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::BitwiseXor32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_integer::emit_bitwise_xor_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::BitwiseNot32 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_integer::emit_bitwise_not_32(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::BitFieldInsert => {
                let base = self.resolve_value(inst.arg(0));
                let insert = self.resolve_value(inst.arg(1));
                let offset = self.resolve_value(inst.arg(2));
                let count = self.resolve_value(inst.arg(3));
                let id = super::emit_spirv_integer::emit_bit_field_insert(
                    self, base, insert, offset, count,
                );
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::BitFieldSExtract => {
                let base = self.resolve_value(inst.arg(0));
                let offset = self.resolve_value(inst.arg(1));
                let count = self.resolve_value(inst.arg(2));
                let id =
                    super::emit_spirv_integer::emit_bit_field_s_extract(self, base, offset, count);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::BitFieldUExtract => {
                let base = self.resolve_value(inst.arg(0));
                let offset = self.resolve_value(inst.arg(1));
                let count = self.resolve_value(inst.arg(2));
                let id =
                    super::emit_spirv_integer::emit_bit_field_u_extract(self, base, offset, count);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::BitCount32 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_integer::emit_bit_count_32(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FindSMsb32 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_integer::emit_find_s_msb_32(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FindUMsb32 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_integer::emit_find_u_msb_32(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::SMin32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_integer::emit_s_min_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::UMin32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_integer::emit_u_min_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::SMax32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_integer::emit_s_max_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::UMax32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_integer::emit_u_max_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }

            // ── Integer comparison ────────────────────────────────────
            Opcode::IEqual => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_integer::emit_i_equal(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::INotEqual => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_integer::emit_i_not_equal(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::SLessThan => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_integer::emit_s_less_than(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::ULessThan => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_integer::emit_u_less_than(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::SLessThanEqual => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_integer::emit_s_less_than_equal(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::ULessThanEqual => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_integer::emit_u_less_than_equal(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::SGreaterThan => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_integer::emit_s_greater_than(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::UGreaterThan => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_integer::emit_u_greater_than(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::SGreaterThanEqual => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_integer::emit_s_greater_than_equal(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::UGreaterThanEqual => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_integer::emit_u_greater_than_equal(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }

            // ── Logic ─────────────────────────────────────────────────
            Opcode::LogicalOr => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_logical::emit_logical_or(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::LogicalAnd => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_logical::emit_logical_and(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::LogicalXor => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_logical::emit_logical_xor(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::LogicalNot => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_logical::emit_logical_not(self, a);
                self.set_value(block_idx, inst_idx, id);
            }

            // ── Select ────────────────────────────────────────────────
            Opcode::SelectU32 => {
                let cond = self.resolve_value(inst.arg(0));
                let t = self.resolve_value(inst.arg(1));
                let f = self.resolve_value(inst.arg(2));
                let id = super::emit_spirv_select::emit_select_u32(self, cond, t, f);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::SelectF32 => {
                let cond = self.resolve_value(inst.arg(0));
                let t = self.resolve_value(inst.arg(1));
                let f = self.resolve_value(inst.arg(2));
                let id = super::emit_spirv_select::emit_select_f32(self, cond, t, f);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::SelectU1 => {
                let cond = self.resolve_value(inst.arg(0));
                let t = self.resolve_value(inst.arg(1));
                let f = self.resolve_value(inst.arg(2));
                let id = super::emit_spirv_select::emit_select_u1(self, cond, t, f);
                self.set_value(block_idx, inst_idx, id);
            }

            // ── Conversion ────────────────────────────────────────────
            Opcode::ConvertS32F32 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_convert::emit_convert_s32_f32(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::ConvertU32F32 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_convert::emit_convert_u32_f32(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::ConvertF32S32 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_convert::emit_convert_f32_s32(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::ConvertF32U32 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_convert::emit_convert_f32_u32(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::BitCastU32F32 => {
                let a = self.resolve_value(inst.arg(0));
                let id =
                    super::emit_spirv_bitwise_conversion::emit_bit_cast_u32_f32(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::BitCastF32U32 => {
                let a = self.resolve_value(inst.arg(0));
                let id =
                    super::emit_spirv_bitwise_conversion::emit_bit_cast_f32_u32(self, a);
                self.set_value(block_idx, inst_idx, id);
            }

            // ── Composite ─────────────────────────────────────────────
            Opcode::CompositeConstructF32x2 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id =
                    super::emit_spirv_composite::emit_composite_construct_f32x2(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::CompositeConstructF32x4 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let c = self.resolve_value(inst.arg(2));
                let d = self.resolve_value(inst.arg(3));
                let id =
                    super::emit_spirv_composite::emit_composite_construct_f32x4(self, a, b, c, d);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::CompositeConstructU32x2 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id =
                    super::emit_spirv_composite::emit_composite_construct_u32x2(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::CompositeConstructU32x4 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let c = self.resolve_value(inst.arg(2));
                let d = self.resolve_value(inst.arg(3));
                let id =
                    super::emit_spirv_composite::emit_composite_construct_u32x4(self, a, b, c, d);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::CompositeExtractF32x4 => {
                let composite = self.resolve_value(inst.arg(0));
                let index = inst.arg(1).imm_u32();
                let id = super::emit_spirv_composite::emit_composite_extract_f32x4(
                    self, composite, index,
                );
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::CompositeExtractU32x2 => {
                let composite = self.resolve_value(inst.arg(0));
                let index = inst.arg(1).imm_u32();
                let id = super::emit_spirv_composite::emit_composite_extract_u32x2(
                    self, composite, index,
                );
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::CompositeExtractU32x4 => {
                let composite = self.resolve_value(inst.arg(0));
                let index = inst.arg(1).imm_u32();
                let id = super::emit_spirv_composite::emit_composite_extract_u32x4(
                    self, composite, index,
                );
                self.set_value(block_idx, inst_idx, id);
            }

            // ── Context (registers, attributes, cbufs) ────────────────
            Opcode::GetCbufU32 | Opcode::GetCbufF32 => {
                super::emit_spirv_context_get_set::emit_get_cbuf(self, inst, block_idx, inst_idx);
            }
            Opcode::GetAttribute | Opcode::GetAttributeU32 => {
                super::emit_spirv_context_get_set::emit_get_attribute_inst(
                    self, inst, block_idx, inst_idx,
                );
            }
            Opcode::SetAttribute => {
                super::emit_spirv_context_get_set::emit_set_attribute_inst(
                    self, inst, block_idx, inst_idx,
                );
            }
            Opcode::SetFragColor => {
                super::emit_spirv_context_get_set::emit_set_frag_color_inst(
                    self, inst, block_idx, inst_idx,
                );
            }
            Opcode::SetFragDepth => {
                super::emit_spirv_context_get_set::emit_set_frag_depth_inst(
                    self, inst, block_idx, inst_idx,
                );
            }

            // ── Image (texture) ───────────────────────────────────────
            Opcode::ImageSampleImplicitLod | Opcode::ImageSampleExplicitLod => {
                super::emit_spirv_image::emit_image_sample(self, inst, block_idx, inst_idx);
            }
            Opcode::ImageFetch => {
                super::emit_spirv_image::emit_image_fetch_inst(self, inst, block_idx, inst_idx);
            }
            Opcode::ImageQueryDimensions => {
                super::emit_spirv_image::emit_image_query(self, inst, block_idx, inst_idx);
            }
            Opcode::ImageGather | Opcode::ImageGatherDref => {
                super::emit_spirv_image::emit_image_gather_inst(self, inst, block_idx, inst_idx);
            }

            // ── Memory ────────────────────────────────────────────────
            Opcode::LoadGlobal32 | Opcode::LoadLocal | Opcode::LoadStorage32 => {
                super::emit_spirv_memory::emit_load(self, inst, block_idx, inst_idx);
            }
            Opcode::WriteGlobal32 | Opcode::WriteLocal | Opcode::WriteStorage32 => {
                super::emit_spirv_memory::emit_store(self, inst, block_idx, inst_idx);
            }

            // ── Control ───────────────────────────────────────────────
            Opcode::DemoteToHelperInvocation => {
                super::emit_spirv_special::emit_demote_to_helper_invocation(self);
            }
            Opcode::Barrier => {
                super::emit_spirv_barriers::emit_barrier(self);
            }

            // ── Register/predicate access — these are handled during
            //    SSA construction and don't emit SPIR-V directly ───────
            Opcode::GetRegister | Opcode::SetRegister | Opcode::GetPred | Opcode::SetPred
            | Opcode::Phi | Opcode::Identity | Opcode::Void | Opcode::Prologue
            | Opcode::Epilogue | Opcode::GetZeroFromOp | Opcode::GetSignFromOp
            | Opcode::GetCarryFromOp | Opcode::GetOverflowFromOp | Opcode::GetZFlag
            | Opcode::SetZFlag | Opcode::GetSFlag | Opcode::SetSFlag | Opcode::GetCFlag
            | Opcode::SetCFlag | Opcode::GetOFlag | Opcode::SetOFlag | Opcode::ConditionRef
            | Opcode::Reference | Opcode::PhiMove | Opcode::GetGotoVariable
            | Opcode::SetGotoVariable | Opcode::GetIndirectBranchVariable
            | Opcode::SetIndirectBranchVariable | Opcode::Join => {
                // No SPIR-V emission needed
            }

            // System values
            Opcode::WorkgroupId | Opcode::LocalInvocationId | Opcode::InvocationId
            | Opcode::InvocationInfo | Opcode::IsHelperInvocation | Opcode::SampleId
            | Opcode::YDirection | Opcode::ResolutionDownFactor | Opcode::RenderArea => {
                // TODO: System value emission
            }

            // Undefined values
            Opcode::UndefU1 | Opcode::UndefU8 | Opcode::UndefU16 | Opcode::UndefU32
            | Opcode::UndefU64 => {
                let result_type = match inst.opcode {
                    Opcode::UndefU1 => self.bool_type,
                    Opcode::UndefU32 | Opcode::UndefU8 | Opcode::UndefU16 => self.u32_type,
                    Opcode::UndefU64 => self.u64_type,
                    _ => self.u32_type,
                };
                let id = self.builder.undef(result_type, None);
                self.set_value(block_idx, inst_idx, id);
            }

            // Everything else — skip with a trace
            _ => {
                log::trace!("SPIR-V: unhandled opcode {:?}", inst.opcode);
            }
        }
    }

    /// Get or create a SPIR-V constant for an IR value.
    pub fn resolve_value(&mut self, value: &ir::Value) -> spirv::Word {
        match value {
            ir::Value::Inst(r) => {
                *self.values.get(&(r.block, r.inst)).unwrap_or(&self.const_zero_u32)
            }
            ir::Value::ImmU32(v) => self.builder.constant_bit32(self.u32_type, *v),
            ir::Value::ImmF32(v) => self.builder.constant_bit32(self.f32_type, v.to_bits()),
            ir::Value::ImmU1(v) => {
                if *v {
                    self.const_true
                } else {
                    self.const_false
                }
            }
            ir::Value::ImmU64(v) => self.builder.constant_bit64(self.u64_type, *v),
            ir::Value::ImmF64(v) => self.builder.constant_bit64(self.f64_type, v.to_bits()),
            _ => self.const_zero_u32,
        }
    }

    /// Store a result ID for an IR instruction.
    pub fn set_value(&mut self, block_idx: u32, inst_idx: u32, id: spirv::Word) {
        self.values.insert((block_idx, inst_idx), id);
    }

    /// Emit the complete program (entry point used by emit_spirv.rs).
    pub fn emit_program(&mut self, program: &ir::Program) {
        self.define_global_variables(program);
        self.define_main_function(program);
    }

    /// Finalize and return SPIR-V words.
    pub fn finalize(self) -> Vec<u32> {
        let module = self.builder.module();
        let mut words = Vec::new();
        module.assemble_into(&mut words);
        words
    }
}
