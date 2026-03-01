// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V EmitContext — wraps rspirv builder with cached types and resources.

use rspirv::binary::Assemble;
use rspirv::dr::{Builder, Operand};
use rspirv::spirv;
use std::collections::HashMap;

use super::Profile;
use crate::shader_recompiler::ir;
use crate::shader_recompiler::ir::types::ShaderStage;

/// Mapping from IR instruction reference to SPIR-V result ID.
pub type ValueMap = HashMap<(u32, u32), spirv::Word>;

/// SPIR-V emission context.
pub struct EmitContext {
    pub builder: Builder,
    pub profile: Profile,

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

    // ── Function types ────────────────────────────────────────────────
    pub void_fn_type: spirv::Word,

    // ── Pointer types ─────────────────────────────────────────────────
    pub input_f32_ptr: spirv::Word,
    pub output_f32_ptr: spirv::Word,
    pub uniform_u32_ptr: spirv::Word,
    pub input_u32_ptr: spirv::Word,
    pub output_u32_ptr: spirv::Word,
    pub uniform_f32_ptr: spirv::Word,

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
    pub values: ValueMap,

    // ── Shader stage ──────────────────────────────────────────────────
    pub stage: ShaderStage,

    // ── Cached constants ──────────────────────────────────────────────
    pub const_zero_u32: spirv::Word,
    pub const_one_u32: spirv::Word,
    pub const_zero_f32: spirv::Word,
    pub const_one_f32: spirv::Word,
    pub const_true: spirv::Word,
    pub const_false: spirv::Word,
}

impl EmitContext {
    pub fn new(program: &ir::Program, profile: &Profile) -> Self {
        let mut builder = Builder::new();

        // Set SPIR-V version and capabilities
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

        builder.memory_model(spirv::AddressingModel::Logical, spirv::MemoryModel::GLSL450);

        // Extension
        builder.ext_inst_import("GLSL.std.450");

        // Placeholder values — will be initialized in define_types_and_constants
        Self {
            builder,
            profile: profile.clone(),
            void_type: 0,
            bool_type: 0,
            u32_type: 0,
            i32_type: 0,
            f32_type: 0,
            u32_vec2_type: 0,
            u32_vec3_type: 0,
            u32_vec4_type: 0,
            f32_vec2_type: 0,
            f32_vec3_type: 0,
            f32_vec4_type: 0,
            u64_type: 0,
            f64_type: 0,
            void_fn_type: 0,
            input_f32_ptr: 0,
            output_f32_ptr: 0,
            uniform_u32_ptr: 0,
            input_u32_ptr: 0,
            output_u32_ptr: 0,
            uniform_f32_ptr: 0,
            cbuf_vars: HashMap::new(),
            texture_vars: HashMap::new(),
            input_vars: HashMap::new(),
            output_vars: HashMap::new(),
            values: HashMap::new(),
            stage: program.stage,
            const_zero_u32: 0,
            const_one_u32: 0,
            const_zero_f32: 0,
            const_one_f32: 0,
            const_true: 0,
            const_false: 0,
        }
    }

    /// Define all SPIR-V types and frequently-used constants.
    pub fn define_types_and_constants(&mut self) {
        // Scalar types
        self.void_type = self.builder.type_void();
        self.bool_type = self.builder.type_bool();
        self.u32_type = self.builder.type_int(32, 0);
        self.i32_type = self.builder.type_int(32, 1);
        self.f32_type = self.builder.type_float(32);

        // Vector types
        self.u32_vec2_type = self.builder.type_vector(self.u32_type, 2);
        self.u32_vec3_type = self.builder.type_vector(self.u32_type, 3);
        self.u32_vec4_type = self.builder.type_vector(self.u32_type, 4);
        self.f32_vec2_type = self.builder.type_vector(self.f32_type, 2);
        self.f32_vec3_type = self.builder.type_vector(self.f32_type, 3);
        self.f32_vec4_type = self.builder.type_vector(self.f32_type, 4);

        // 64-bit types
        self.u64_type = self.builder.type_int(64, 0);
        self.f64_type = self.builder.type_float(64);

        // Function type: void(void)
        self.void_fn_type = self.builder.type_function(self.void_type, vec![]);

        // Pointer types
        self.input_f32_ptr =
            self.builder.type_pointer(None, spirv::StorageClass::Input, self.f32_type);
        self.output_f32_ptr =
            self.builder.type_pointer(None, spirv::StorageClass::Output, self.f32_type);
        self.input_u32_ptr =
            self.builder.type_pointer(None, spirv::StorageClass::Input, self.u32_type);
        self.output_u32_ptr =
            self.builder.type_pointer(None, spirv::StorageClass::Output, self.u32_type);
        self.uniform_u32_ptr =
            self.builder.type_pointer(None, spirv::StorageClass::Uniform, self.u32_type);
        self.uniform_f32_ptr =
            self.builder.type_pointer(None, spirv::StorageClass::Uniform, self.f32_type);

        // Constants
        self.const_zero_u32 = self.builder.constant_bit32(self.u32_type, 0);
        self.const_one_u32 = self.builder.constant_bit32(self.u32_type, 1);
        self.const_zero_f32 = self.builder.constant_bit32(self.f32_type, 0.0f32.to_bits());
        self.const_one_f32 = self.builder.constant_bit32(self.f32_type, 1.0f32.to_bits());
        self.const_true = self.builder.constant_true(self.bool_type);
        self.const_false = self.builder.constant_false(self.bool_type);
    }

    /// Define global variables (inputs, outputs, UBOs, textures) from shader info.
    pub fn define_global_variables(&mut self, program: &ir::Program) {
        let info = &program.info;

        // Input variables
        match self.stage {
            ShaderStage::Vertex => {
                // Vertex inputs come from vertex attributes
                for i in 0..32u32 {
                    if info.loads_generics & (1 << i) != 0 {
                        let vec4_ptr = self
                            .builder
                            .type_pointer(None, spirv::StorageClass::Input, self.f32_vec4_type);
                        let var = self
                            .builder
                            .variable(vec4_ptr, None, spirv::StorageClass::Input, None);
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
                // Fragment inputs are varyings from the previous stage
                for i in 0..32u32 {
                    if info.loads_generics & (1 << i) != 0 {
                        let vec4_ptr = self
                            .builder
                            .type_pointer(None, spirv::StorageClass::Input, self.f32_vec4_type);
                        let var = self
                            .builder
                            .variable(vec4_ptr, None, spirv::StorageClass::Input, None);
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
                    let vec4_ptr = self
                        .builder
                        .type_pointer(None, spirv::StorageClass::Output, self.f32_vec4_type);
                    let var = self
                        .builder
                        .variable(vec4_ptr, None, spirv::StorageClass::Output, None);
                    self.builder.decorate(
                        var,
                        spirv::Decoration::BuiltIn,
                        vec![Operand::BuiltIn(spirv::BuiltIn::Position)],
                    );
                    self.output_vars.insert(0xFFFF_0000, var); // Special key for gl_Position
                }
                for i in 0..32u32 {
                    if info.stores_generics & (1 << i) != 0 {
                        let vec4_ptr = self
                            .builder
                            .type_pointer(None, spirv::StorageClass::Output, self.f32_vec4_type);
                        let var = self
                            .builder
                            .variable(vec4_ptr, None, spirv::StorageClass::Output, None);
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
                // Fragment outputs = render target colors
                let vec4_ptr = self
                    .builder
                    .type_pointer(None, spirv::StorageClass::Output, self.f32_vec4_type);
                let var = self
                    .builder
                    .variable(vec4_ptr, None, spirv::StorageClass::Output, None);
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
            let array_type = self
                .builder
                .type_array(self.u32_type, array_len_const);
            self.builder.decorate(
                array_type,
                spirv::Decoration::ArrayStride,
                vec![Operand::LiteralBit32(4)],
            );

            let struct_type = self.builder.type_struct(vec![array_type]);
            self.builder.decorate(
                struct_type,
                spirv::Decoration::Block,
                vec![],
            );
            self.builder.member_decorate(
                struct_type,
                0,
                spirv::Decoration::Offset,
                vec![Operand::LiteralBit32(0)],
            );

            let ptr_type = self
                .builder
                .type_pointer(None, spirv::StorageClass::Uniform, struct_type);
            let var = self
                .builder
                .variable(ptr_type, None, spirv::StorageClass::Uniform, None);
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
        let entry_label = self.builder.begin_block(None).unwrap();

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

        self.builder
            .entry_point(exec_model, main_fn, "main", interface);

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
                super::emit_float::emit_fp_add_32(self, inst, block_idx, inst_idx);
            }
            Opcode::FPSub32 => {
                super::emit_float::emit_fp_sub_32(self, inst, block_idx, inst_idx);
            }
            Opcode::FPMul32 => {
                super::emit_float::emit_fp_mul_32(self, inst, block_idx, inst_idx);
            }
            Opcode::FPDiv32 => {
                super::emit_float::emit_fp_div_32(self, inst, block_idx, inst_idx);
            }
            Opcode::FPFma32 => {
                super::emit_float::emit_fp_fma_32(self, inst, block_idx, inst_idx);
            }
            Opcode::FPNeg32 => {
                super::emit_float::emit_fp_neg_32(self, inst, block_idx, inst_idx);
            }
            Opcode::FPAbs32 => {
                super::emit_float::emit_fp_abs_32(self, inst, block_idx, inst_idx);
            }
            Opcode::FPSaturate32 => {
                super::emit_float::emit_fp_saturate_32(self, inst, block_idx, inst_idx);
            }
            Opcode::FPMin32 => {
                super::emit_float::emit_fp_min_32(self, inst, block_idx, inst_idx);
            }
            Opcode::FPMax32 => {
                super::emit_float::emit_fp_max_32(self, inst, block_idx, inst_idx);
            }
            Opcode::FPSin => {
                super::emit_float::emit_fp_sin(self, inst, block_idx, inst_idx);
            }
            Opcode::FPCos => {
                super::emit_float::emit_fp_cos(self, inst, block_idx, inst_idx);
            }
            Opcode::FPExp2 => {
                super::emit_float::emit_fp_exp2(self, inst, block_idx, inst_idx);
            }
            Opcode::FPLog2 => {
                super::emit_float::emit_fp_log2(self, inst, block_idx, inst_idx);
            }
            Opcode::FPSqrt32 => {
                super::emit_float::emit_fp_sqrt_32(self, inst, block_idx, inst_idx);
            }
            Opcode::FPRecip32 => {
                super::emit_float::emit_fp_recip_32(self, inst, block_idx, inst_idx);
            }
            Opcode::FPRecipSqrt32 => {
                super::emit_float::emit_fp_recip_sqrt_32(self, inst, block_idx, inst_idx);
            }
            Opcode::FPFloor32 => {
                super::emit_float::emit_fp_floor_32(self, inst, block_idx, inst_idx);
            }
            Opcode::FPCeil32 => {
                super::emit_float::emit_fp_ceil_32(self, inst, block_idx, inst_idx);
            }
            Opcode::FPTrunc32 => {
                super::emit_float::emit_fp_trunc_32(self, inst, block_idx, inst_idx);
            }
            Opcode::FPRoundEven32 => {
                super::emit_float::emit_fp_round_even_32(self, inst, block_idx, inst_idx);
            }

            // ── FP comparison ─────────────────────────────────────────
            Opcode::FPOrdEqual32 | Opcode::FPOrdNotEqual32 | Opcode::FPOrdLessThan32
            | Opcode::FPOrdGreaterThan32 | Opcode::FPOrdLessThanEqual32
            | Opcode::FPOrdGreaterThanEqual32 | Opcode::FPUnordEqual32
            | Opcode::FPUnordNotEqual32 | Opcode::FPUnordLessThan32
            | Opcode::FPUnordGreaterThan32 => {
                super::emit_float::emit_fp_compare(self, inst, block_idx, inst_idx);
            }
            Opcode::FPIsNan32 => {
                super::emit_float::emit_fp_is_nan_32(self, inst, block_idx, inst_idx);
            }

            // ── Integer arithmetic ────────────────────────────────────
            Opcode::IAdd32 => {
                super::emit_integer::emit_iadd_32(self, inst, block_idx, inst_idx);
            }
            Opcode::ISub32 => {
                super::emit_integer::emit_isub_32(self, inst, block_idx, inst_idx);
            }
            Opcode::IMul32 => {
                super::emit_integer::emit_imul_32(self, inst, block_idx, inst_idx);
            }
            Opcode::INeg32 => {
                super::emit_integer::emit_ineg_32(self, inst, block_idx, inst_idx);
            }
            Opcode::IAbs32 => {
                super::emit_integer::emit_iabs_32(self, inst, block_idx, inst_idx);
            }
            Opcode::ShiftLeftLogical32 => {
                super::emit_integer::emit_shl_32(self, inst, block_idx, inst_idx);
            }
            Opcode::ShiftRightLogical32 => {
                super::emit_integer::emit_shr_logical_32(self, inst, block_idx, inst_idx);
            }
            Opcode::ShiftRightArithmetic32 => {
                super::emit_integer::emit_shr_arith_32(self, inst, block_idx, inst_idx);
            }
            Opcode::BitwiseAnd32 => {
                super::emit_integer::emit_bitwise_and_32(self, inst, block_idx, inst_idx);
            }
            Opcode::BitwiseOr32 => {
                super::emit_integer::emit_bitwise_or_32(self, inst, block_idx, inst_idx);
            }
            Opcode::BitwiseXor32 => {
                super::emit_integer::emit_bitwise_xor_32(self, inst, block_idx, inst_idx);
            }
            Opcode::BitwiseNot32 => {
                super::emit_integer::emit_bitwise_not_32(self, inst, block_idx, inst_idx);
            }
            Opcode::BitFieldInsert => {
                super::emit_integer::emit_bfi(self, inst, block_idx, inst_idx);
            }
            Opcode::BitFieldSExtract => {
                super::emit_integer::emit_bfe_s(self, inst, block_idx, inst_idx);
            }
            Opcode::BitFieldUExtract => {
                super::emit_integer::emit_bfe_u(self, inst, block_idx, inst_idx);
            }
            Opcode::BitCount32 => {
                super::emit_integer::emit_bit_count_32(self, inst, block_idx, inst_idx);
            }
            Opcode::FindSMsb32 | Opcode::FindUMsb32 => {
                super::emit_integer::emit_find_msb_32(self, inst, block_idx, inst_idx);
            }
            Opcode::SMin32 | Opcode::UMin32 | Opcode::SMax32 | Opcode::UMax32 => {
                super::emit_integer::emit_min_max_32(self, inst, block_idx, inst_idx);
            }

            // ── Integer comparison ────────────────────────────────────
            Opcode::IEqual | Opcode::INotEqual | Opcode::SLessThan | Opcode::ULessThan
            | Opcode::SLessThanEqual | Opcode::ULessThanEqual | Opcode::SGreaterThan
            | Opcode::UGreaterThan | Opcode::SGreaterThanEqual | Opcode::UGreaterThanEqual => {
                super::emit_integer::emit_int_compare(self, inst, block_idx, inst_idx);
            }

            // ── Logic ─────────────────────────────────────────────────
            Opcode::LogicalOr | Opcode::LogicalAnd | Opcode::LogicalXor | Opcode::LogicalNot => {
                super::emit_integer::emit_logical(self, inst, block_idx, inst_idx);
            }

            // ── Select ────────────────────────────────────────────────
            Opcode::SelectU32 | Opcode::SelectF32 | Opcode::SelectU1 => {
                super::emit_integer::emit_select(self, inst, block_idx, inst_idx);
            }

            // ── Conversion ────────────────────────────────────────────
            Opcode::ConvertS32F32 | Opcode::ConvertU32F32 | Opcode::ConvertF32S32
            | Opcode::ConvertF32U32 | Opcode::BitCastU32F32 | Opcode::BitCastF32U32 => {
                super::emit_convert::emit_convert(self, inst, block_idx, inst_idx);
            }

            // ── Composite ─────────────────────────────────────────────
            Opcode::CompositeConstructF32x2
            | Opcode::CompositeConstructF32x4
            | Opcode::CompositeConstructU32x2
            | Opcode::CompositeConstructU32x4
            | Opcode::CompositeExtractF32x4
            | Opcode::CompositeExtractU32x2
            | Opcode::CompositeExtractU32x4 => {
                super::emit_composite::emit_composite(self, inst, block_idx, inst_idx);
            }

            // ── Context (registers, attributes, cbufs) ────────────────
            Opcode::GetCbufU32 | Opcode::GetCbufF32 => {
                super::emit_context::emit_get_cbuf(self, inst, block_idx, inst_idx);
            }
            Opcode::GetAttribute | Opcode::GetAttributeU32 => {
                super::emit_context::emit_get_attribute(self, inst, block_idx, inst_idx);
            }
            Opcode::SetAttribute => {
                super::emit_context::emit_set_attribute(self, inst, block_idx, inst_idx);
            }
            Opcode::SetFragColor => {
                super::emit_context::emit_set_frag_color(self, inst, block_idx, inst_idx);
            }
            Opcode::SetFragDepth => {
                super::emit_context::emit_set_frag_depth(self, inst, block_idx, inst_idx);
            }

            // ── Texture ───────────────────────────────────────────────
            Opcode::ImageSampleImplicitLod | Opcode::ImageSampleExplicitLod => {
                super::emit_texture::emit_image_sample(self, inst, block_idx, inst_idx);
            }
            Opcode::ImageFetch => {
                super::emit_texture::emit_image_fetch(self, inst, block_idx, inst_idx);
            }
            Opcode::ImageQueryDimensions => {
                super::emit_texture::emit_image_query(self, inst, block_idx, inst_idx);
            }
            Opcode::ImageGather | Opcode::ImageGatherDref => {
                super::emit_texture::emit_image_gather(self, inst, block_idx, inst_idx);
            }

            // ── Memory ────────────────────────────────────────────────
            Opcode::LoadGlobal32 | Opcode::LoadLocal | Opcode::LoadStorage32 => {
                super::emit_memory::emit_load(self, inst, block_idx, inst_idx);
            }
            Opcode::WriteGlobal32 | Opcode::WriteLocal | Opcode::WriteStorage32 => {
                super::emit_memory::emit_store(self, inst, block_idx, inst_idx);
            }

            // ── Control ───────────────────────────────────────────────
            Opcode::DemoteToHelperInvocation => {
                super::emit_control::emit_demote(self);
            }
            Opcode::Barrier => {
                super::emit_control::emit_barrier(self);
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
                self.values.insert((block_idx, inst_idx), id);
            }

            // Everything else — skip with a trace
            _ => {
                log::trace!("SPIR-V: unhandled opcode {:?}", inst.opcode);
            }
        }
    }

    /// Get the SPIR-V ID for an IR value.
    pub fn get_value(&self, value: &ir::Value) -> spirv::Word {
        match value {
            ir::Value::Inst(r) => *self.values.get(&(r.block, r.inst)).unwrap_or(&0),
            ir::Value::ImmU32(v) => {
                // We can't create constants here (need mut), use 0 as fallback
                // In practice, we pre-resolve immediates during emission
                0
            }
            ir::Value::ImmF32(v) => 0,
            ir::Value::ImmU1(v) => {
                if *v {
                    self.const_true
                } else {
                    self.const_false
                }
            }
            _ => 0,
        }
    }

    /// Get or create a SPIR-V constant for an IR value.
    pub fn resolve_value(&mut self, value: &ir::Value) -> spirv::Word {
        match value {
            ir::Value::Inst(r) => *self.values.get(&(r.block, r.inst)).unwrap_or(&self.const_zero_u32),
            ir::Value::ImmU32(v) => self.builder.constant_bit32(self.u32_type, *v),
            ir::Value::ImmF32(v) => self.builder.constant_bit32(self.f32_type, v.to_bits()),
            ir::Value::ImmU1(v) => {
                if *v {
                    self.const_true
                } else {
                    self.const_false
                }
            }
            ir::Value::ImmU64(v) => {
                self.builder.constant_bit64(self.u64_type, *v)
            }
            ir::Value::ImmF64(v) => self.builder.constant_bit64(self.f64_type, v.to_bits()),
            _ => self.const_zero_u32,
        }
    }

    /// Store a result ID for an IR instruction.
    pub fn set_value(&mut self, block_idx: u32, inst_idx: u32, id: spirv::Word) {
        self.values.insert((block_idx, inst_idx), id);
    }

    /// Finalize the module and return SPIR-V words.
    pub fn finalize(self) -> Vec<u32> {
        let module = self.builder.module();
        let mut words = Vec::new();
        module.assemble_into(&mut words);
        words
    }
}
