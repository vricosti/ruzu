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

use crate::backend::bindings::Bindings;
use crate::ir;
use crate::ir::program::ShaderInfo;
use crate::ir::types::{ShaderStage, Type};
use crate::profile::Profile;
use crate::runtime_info::{AttributeType, RuntimeInfo};
use crate::shader_info::{ConstantBufferDescriptor, TextureDescriptor, TextureType};

struct DeferredPhi {
    result_id: spirv::Word,
    values: Vec<ir::Value>,
}

/// Port of upstream `TextureDefinition` in `spirv_emit_context.h`.
#[derive(Clone, Copy)]
pub(crate) struct TextureDefinition {
    pub id: spirv::Word,
    pub sampled_type: spirv::Word,
    pub pointer_type: spirv::Word,
    pub image_type: spirv::Word,
    pub count: u32,
    pub is_multisample: bool,
}

#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct UniformDefinitions {
    pub u32_scalar: spirv::Word,
    pub f32_scalar: spirv::Word,
    pub u32x2: spirv::Word,
    pub u32x4: spirv::Word,
}

#[derive(Debug, Clone, Copy)]
enum UniformDefinitionKind {
    U32,
    F32,
    U32x2,
    U32x4,
}

/// Port of upstream `ImageType(EmitContext&, const TextureDescriptor&)`.
fn texture_image_type(ctx: &mut SpirvEmitContext, desc: &TextureDescriptor) -> spirv::Word {
    let depth = u32::from(desc.is_depth);
    let multisampled = u32::from(desc.is_multisample);
    let (dim, arrayed, ms) = match desc.texture_type {
        TextureType::Color1D => (spirv::Dim::Dim1D, 0, 0),
        TextureType::ColorArray1D => (spirv::Dim::Dim1D, 1, 0),
        TextureType::Color2D | TextureType::Color2DRect => (spirv::Dim::Dim2D, 0, multisampled),
        TextureType::ColorArray2D => (spirv::Dim::Dim2D, 1, multisampled),
        TextureType::Color3D => (spirv::Dim::Dim3D, 0, 0),
        TextureType::ColorCube => (spirv::Dim::DimCube, 0, 0),
        TextureType::ColorArrayCube => (spirv::Dim::DimCube, 1, 0),
        TextureType::Buffer => panic!("SPIR-V: buffer texture in sampled texture descriptors"),
    };
    ctx.builder.type_image(
        ctx.f32_type,
        dim,
        depth,
        arrayed,
        ms,
        1,
        spirv::ImageFormat::Unknown,
        None,
    )
}

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
    pub f16_type: spirv::Word,
    pub f32_type: spirv::Word,
    pub f16_vec2_type: spirv::Word,
    pub u32_vec2_type: spirv::Word,
    pub u32_vec3_type: spirv::Word,
    pub u32_vec4_type: spirv::Word,
    pub i32_vec4_type: spirv::Word,
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
    pub input_i32_ptr: spirv::Word,
    pub output_u32_ptr: spirv::Word,
    pub output_i32_ptr: spirv::Word,
    pub uniform_f32_ptr: spirv::Word,
    pub uniform_u32_vec2_ptr: spirv::Word,
    pub uniform_u32_vec4_ptr: spirv::Word,

    // ── Cached constants ──────────────────────────────────────────────
    pub const_zero_u32: spirv::Word,
    pub const_one_u32: spirv::Word,
    pub const_zero_f32: spirv::Word,
    pub const_one_f32: spirv::Word,
    pub const_true: spirv::Word,
    pub const_false: spirv::Word,

    // ── GLSL.std.450 extended instruction set ─────────────────────────
    pub glsl_ext: spirv::Word,

    // ── Runtime info ──────────────────────────────────────────────────
    pub runtime_info: RuntimeInfo,

    // ── System value input variables ─────────────────────────────────
    pub workgroup_id: spirv::Word,
    pub local_invocation_id: spirv::Word,
    pub invocation_id: spirv::Word,
    pub patch_vertices_in: spirv::Word,
    pub sample_id: spirv::Word,
    pub is_helper_invocation: spirv::Word,
    pub primitive_id: spirv::Word,
    pub layer: spirv::Word,
    pub instance_id: spirv::Word,
    pub instance_index: spirv::Word,
    pub base_instance: spirv::Word,
    pub vertex_id: spirv::Word,
    pub vertex_index: spirv::Word,
    pub base_vertex: spirv::Word,
    pub draw_index: spirv::Word,
    pub front_face: spirv::Word,
    pub point_coord: spirv::Word,
    pub tess_coord: spirv::Word,
    pub input_position: spirv::Word,
    pub output_point_size: spirv::Word,
    pub frag_depth: spirv::Word,

    // ── Rescaling / render area push constants ───────────────────────
    pub rescaling_uniform_constant: spirv::Word,
    pub rescaling_push_constants: spirv::Word,
    pub rescaling_downfactor_member_index: u32,
    pub render_area_push_constant: spirv::Word,
    pub render_are_member_index: u32,

    // ── Resources ─────────────────────────────────────────────────────
    /// Typed constant-buffer views indexed by CB index. The views alias the
    /// same descriptor bindings when the device supports descriptor aliasing.
    pub(crate) cbufs: HashMap<u32, UniformDefinitions>,
    /// Texture combined image sampler variables, indexed by descriptor index.
    pub(crate) textures: Vec<TextureDefinition>,
    /// Storage buffer variables, indexed by compact SSBO descriptor index.
    pub ssbo_vars: HashMap<u32, spirv::Word>,
    /// Pointer to a u32 element inside an SSBO runtime array.
    pub storage_u32_ptr: spirv::Word,
    /// Input variables (vertex attributes / fragment varyings).
    pub input_vars: HashMap<u32, spirv::Word>,
    /// Output variables (vertex outputs / fragment colors).
    pub output_vars: HashMap<u32, spirv::Word>,
    /// Entry-point interface variables.
    ///
    /// Upstream keeps this list on `EmitContext` and appends variables as they
    /// are defined. Do not reconstruct it later, because resources are part of
    /// the interface for SPIR-V 1.4+.
    pub interfaces: Vec<spirv::Word>,

    // ── Value mapping ─────────────────────────────────────────────────
    /// Maps IR instruction references (block, inst) to SPIR-V result IDs.
    pub values: HashMap<(u32, u32), spirv::Word>,
    /// Maps IR block indices to SPIR-V label IDs.
    pub block_labels: Vec<spirv::Word>,
    /// Phi values are patched after all blocks have been emitted, matching
    /// upstream Sirit's `DeferredOpPhi` / `PatchDeferredPhi` lifecycle.
    deferred_phis: Vec<DeferredPhi>,
}

impl SpirvEmitContext {
    /// Create a new SPIR-V emission context.
    pub fn new(program: &ir::Program, profile: &Profile, runtime_info: &RuntimeInfo) -> Self {
        let mut builder = Builder::new();
        builder.set_version(
            (profile.supported_spirv >> 16) as u8,
            ((profile.supported_spirv >> 8) & 0xff) as u8,
        );
        builder.capability(spirv::Capability::Shader);

        // Upstream gates Float16/Float64/Int* capabilities on
        // `program.info.uses_fp16/fp64/int8/int16/int64` — the program-
        // info flags populated during translation.
        if program.info.uses_fp16 {
            builder.capability(spirv::Capability::Float16);
        }
        if program.info.uses_fp64 {
            builder.capability(spirv::Capability::Float64);
        }
        if program.info.uses_int8 && profile.support_int8 {
            builder.capability(spirv::Capability::Int8);
        }
        if program.info.uses_int16 && profile.support_int16 {
            builder.capability(spirv::Capability::Int16);
        }
        if program.info.uses_int64 && profile.support_int64 {
            builder.capability(spirv::Capability::Int64);
        }
        // Upstream gates subgroup capabilities on individual usage flags.
        if program.info.uses_subgroup_vote
            || program.info.uses_subgroup_mask
            || program.info.uses_subgroup_shuffles
        {
            builder.capability(spirv::Capability::GroupNonUniform);
            builder.capability(spirv::Capability::GroupNonUniformBallot);
            builder.capability(spirv::Capability::GroupNonUniformShuffle);
            builder.capability(spirv::Capability::GroupNonUniformVote);
        }
        if program.info.uses_demote_to_helper_invocation
            && profile.support_demote_to_helper_invocation
        {
            if profile.supported_spirv < 0x0001_0600 {
                builder.extension("SPV_EXT_demote_to_helper_invocation");
            }
            builder.capability(spirv::Capability::DemoteToHelperInvocation);
        }
        builder.capability(spirv::Capability::DrawParameters);
        builder.capability(spirv::Capability::ImageQuery);
        builder.capability(spirv::Capability::Sampled1D);
        builder.capability(spirv::Capability::SampledCubeArray);

        builder.memory_model(spirv::AddressingModel::Logical, spirv::MemoryModel::GLSL450);

        let glsl_ext = builder.ext_inst_import("GLSL.std.450");

        // Define scalar types
        let void_type = builder.type_void();
        let bool_type = builder.type_bool();
        let u32_type = builder.type_int(32, 0);
        let i32_type = builder.type_int(32, 1);
        let f32_type = builder.type_float(32);
        let f16_type = if program.info.uses_fp16 {
            builder.type_float(16)
        } else {
            f32_type
        };

        // Define vector types
        let u32_vec2_type = builder.type_vector(u32_type, 2);
        let u32_vec3_type = builder.type_vector(u32_type, 3);
        let u32_vec4_type = builder.type_vector(u32_type, 4);
        let i32_vec4_type = builder.type_vector(i32_type, 4);
        let f32_vec2_type = builder.type_vector(f32_type, 2);
        let f32_vec3_type = builder.type_vector(f32_type, 3);
        let f32_vec4_type = builder.type_vector(f32_type, 4);
        let f16_vec2_type = if program.info.uses_fp16 {
            builder.type_vector(f16_type, 2)
        } else {
            f32_vec2_type
        };

        // Upstream only defines 64-bit scalar types when the program uses
        // them. Declaring OpTypeInt/OpTypeFloat 64 without the corresponding
        // capability makes otherwise 32-bit shaders invalid SPIR-V.
        let u64_type = if program.info.uses_int64 && profile.support_int64 {
            builder.type_int(64, 0)
        } else {
            u32_type
        };
        let f64_type = if program.info.uses_fp64 {
            builder.type_float(64)
        } else {
            f32_type
        };

        // Function type: void(void)
        let void_fn_type = builder.type_function(void_type, vec![]);

        // Pointer types
        let input_f32_ptr = builder.type_pointer(None, spirv::StorageClass::Input, f32_type);
        let output_f32_ptr = builder.type_pointer(None, spirv::StorageClass::Output, f32_type);
        let input_u32_ptr = builder.type_pointer(None, spirv::StorageClass::Input, u32_type);
        let input_i32_ptr = builder.type_pointer(None, spirv::StorageClass::Input, i32_type);
        let output_u32_ptr = builder.type_pointer(None, spirv::StorageClass::Output, u32_type);
        let output_i32_ptr = builder.type_pointer(None, spirv::StorageClass::Output, i32_type);
        let uniform_u32_ptr = builder.type_pointer(None, spirv::StorageClass::Uniform, u32_type);
        let uniform_f32_ptr = builder.type_pointer(None, spirv::StorageClass::Uniform, f32_type);
        let uniform_u32_vec2_ptr =
            builder.type_pointer(None, spirv::StorageClass::Uniform, u32_vec2_type);
        let uniform_u32_vec4_ptr =
            builder.type_pointer(None, spirv::StorageClass::Uniform, u32_vec4_type);
        let storage_u32_ptr =
            builder.type_pointer(None, spirv::StorageClass::StorageBuffer, u32_type);

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
            runtime_info: runtime_info.clone(),
            workgroup_id: 0,
            local_invocation_id: 0,
            invocation_id: 0,
            patch_vertices_in: 0,
            sample_id: 0,
            is_helper_invocation: 0,
            primitive_id: 0,
            layer: 0,
            instance_id: 0,
            instance_index: 0,
            base_instance: 0,
            vertex_id: 0,
            vertex_index: 0,
            base_vertex: 0,
            draw_index: 0,
            front_face: 0,
            point_coord: 0,
            tess_coord: 0,
            input_position: 0,
            output_point_size: 0,
            frag_depth: 0,
            rescaling_uniform_constant: 0,
            rescaling_push_constants: 0,
            rescaling_downfactor_member_index: 0,
            render_area_push_constant: 0,
            render_are_member_index: 0,
            void_type,
            bool_type,
            u32_type,
            i32_type,
            f16_type,
            f32_type,
            f16_vec2_type,
            u32_vec2_type,
            u32_vec3_type,
            u32_vec4_type,
            i32_vec4_type,
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
            input_i32_ptr,
            output_u32_ptr,
            output_i32_ptr,
            uniform_f32_ptr,
            uniform_u32_vec2_ptr,
            uniform_u32_vec4_ptr,
            const_zero_u32,
            const_one_u32,
            const_zero_f32,
            const_one_f32,
            const_true,
            const_false,
            glsl_ext,
            cbufs: HashMap::new(),
            textures: Vec::new(),
            ssbo_vars: HashMap::new(),
            storage_u32_ptr,
            input_vars: HashMap::new(),
            output_vars: HashMap::new(),
            interfaces: Vec::new(),
            values: HashMap::new(),
            block_labels: Vec::new(),
            deferred_phis: Vec::new(),
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

    fn define_constant_buffer_view(
        &mut self,
        descriptors: &[ConstantBufferDescriptor],
        mut binding: u32,
        element_type: spirv::Word,
        element_size: u32,
        kind: UniformDefinitionKind,
    ) {
        let array_len = self
            .builder
            .constant_bit32(self.u32_type, 0x1_0000 / element_size);
        let array_type = self.builder.type_array(element_type, array_len);
        self.builder.decorate(
            array_type,
            spirv::Decoration::ArrayStride,
            vec![Operand::LiteralBit32(element_size)],
        );
        let struct_type = self.builder.type_struct(vec![array_type]);
        self.builder
            .decorate(struct_type, spirv::Decoration::Block, vec![]);
        self.builder.member_decorate(
            struct_type,
            0,
            spirv::Decoration::Offset,
            vec![Operand::LiteralBit32(0)],
        );
        let pointer_type =
            self.builder
                .type_pointer(None, spirv::StorageClass::Uniform, struct_type);

        for desc in descriptors {
            let var = self
                .builder
                .variable(pointer_type, None, spirv::StorageClass::Uniform, None);
            self.builder.decorate(
                var,
                spirv::Decoration::DescriptorSet,
                vec![Operand::LiteralBit32(0)],
            );
            self.builder.decorate(
                var,
                spirv::Decoration::Binding,
                vec![Operand::LiteralBit32(binding)],
            );
            for index in 0..desc.count {
                let definitions = self.cbufs.entry(desc.index + index).or_default();
                match kind {
                    UniformDefinitionKind::U32 => definitions.u32_scalar = var,
                    UniformDefinitionKind::F32 => definitions.f32_scalar = var,
                    UniformDefinitionKind::U32x2 => definitions.u32x2 = var,
                    UniformDefinitionKind::U32x4 => definitions.u32x4 = var,
                }
            }
            if self.profile.supported_spirv >= 0x0001_0400 {
                self.interfaces.push(var);
            }
            binding += desc.count;
        }
    }

    /// Define global variables (inputs, outputs, UBOs, textures) from shader info.
    pub fn define_global_variables(&mut self, program: &ir::Program, bindings: &mut Bindings) {
        let info = &program.info;

        // Input variables — upstream reads `info.loads` VaryingState.
        match self.stage {
            ShaderStage::VertexB | ShaderStage::Fragment => {
                for i in 0..32u32 {
                    if info.loads.generic_any(i as usize) {
                        let input_type = self.runtime_info.generic_input_types[i as usize];
                        if input_type == AttributeType::Disabled {
                            continue;
                        }
                        let vector_type = match input_type {
                            AttributeType::Float => self.f32_vec4_type,
                            AttributeType::SignedInt => self.i32_vec4_type,
                            AttributeType::UnsignedInt => self.u32_vec4_type,
                            AttributeType::SignedScaled => {
                                if self.profile.support_scaled_attributes {
                                    self.f32_vec4_type
                                } else {
                                    self.i32_vec4_type
                                }
                            }
                            AttributeType::UnsignedScaled => {
                                if self.profile.support_scaled_attributes {
                                    self.f32_vec4_type
                                } else {
                                    self.u32_vec4_type
                                }
                            }
                            AttributeType::Disabled => unreachable!(),
                        };
                        let vec4_ptr = self.builder.type_pointer(
                            None,
                            spirv::StorageClass::Input,
                            vector_type,
                        );
                        let var =
                            self.builder
                                .variable(vec4_ptr, None, spirv::StorageClass::Input, None);
                        self.builder.decorate(
                            var,
                            spirv::Decoration::Location,
                            vec![Operand::LiteralBit32(i)],
                        );
                        self.input_vars.insert(i, var);
                        self.interfaces.push(var);
                    }
                }
            }
            _ => {}
        }

        // Output variables — upstream reads `info.stores` VaryingState.
        use crate::ir::value::Attribute;
        if info.stores.get(Attribute::POINT_SIZE.0 as usize)
            || self.runtime_info.fixed_state_point_size.is_some()
        {
            assert_ne!(
                self.stage,
                ShaderStage::Fragment,
                "storing PointSize in fragment stage is unsupported upstream"
            );
            let ptr_type =
                self.builder
                    .type_pointer(None, spirv::StorageClass::Output, self.f32_type);
            let var = self
                .builder
                .variable(ptr_type, None, spirv::StorageClass::Output, None);
            self.builder.decorate(
                var,
                spirv::Decoration::BuiltIn,
                vec![Operand::BuiltIn(spirv::BuiltIn::PointSize)],
            );
            self.output_point_size = var;
            self.interfaces.push(var);
        }
        match self.stage {
            ShaderStage::VertexB => {
                // Upstream always declares Position for VertexB.
                let vec4_ptr = self.builder.type_pointer(
                    None,
                    spirv::StorageClass::Output,
                    self.f32_vec4_type,
                );
                let var = self
                    .builder
                    .variable(vec4_ptr, None, spirv::StorageClass::Output, None);
                self.builder.decorate(
                    var,
                    spirv::Decoration::BuiltIn,
                    vec![Operand::BuiltIn(spirv::BuiltIn::Position)],
                );
                self.output_vars.insert(0xFFFF_0000, var);
                self.interfaces.push(var);
                for i in 0..32u32 {
                    if info.stores.generic_any(i as usize) {
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
                        self.interfaces.push(var);
                    }
                }
            }
            ShaderStage::Fragment => {
                for (index, &stores) in info.stores_frag_color.iter().enumerate() {
                    if !stores && !self.profile.need_declared_frag_colors {
                        continue;
                    }
                    let output_type = match self.runtime_info.frag_color_types[index] {
                        AttributeType::UnsignedInt => self.u32_vec4_type,
                        AttributeType::SignedInt => self.i32_vec4_type,
                        _ => self.f32_vec4_type,
                    };
                    let vec4_ptr =
                        self.builder
                            .type_pointer(None, spirv::StorageClass::Output, output_type);
                    let var =
                        self.builder
                            .variable(vec4_ptr, None, spirv::StorageClass::Output, None);
                    self.builder.decorate(
                        var,
                        spirv::Decoration::Location,
                        vec![Operand::LiteralBit32(index as u32)],
                    );
                    self.output_vars.insert(index as u32, var);
                    self.interfaces.push(var);
                }
                if info.stores_frag_depth {
                    let ptr_type =
                        self.builder
                            .type_pointer(None, spirv::StorageClass::Output, self.f32_type);
                    let var =
                        self.builder
                            .variable(ptr_type, None, spirv::StorageClass::Output, None);
                    self.builder.decorate(
                        var,
                        spirv::Decoration::BuiltIn,
                        vec![Operand::BuiltIn(spirv::BuiltIn::FragDepth)],
                    );
                    self.frag_depth = var;
                    self.interfaces.push(var);
                }
            }
            _ => {}
        }

        // Constant buffers (UBOs), matching upstream DefineConstantBuffers.
        if !info.constant_buffer_descriptors.is_empty() {
            let binding = if self.profile.unified_descriptor_binding {
                &mut bindings.unified
            } else {
                &mut bindings.uniform_buffer
            };
            let first_binding = *binding;
            if !self.profile.support_descriptor_aliasing {
                self.define_constant_buffer_view(
                    &info.constant_buffer_descriptors,
                    first_binding,
                    self.u32_vec4_type,
                    16,
                    UniformDefinitionKind::U32x4,
                );
                *binding += info
                    .constant_buffer_descriptors
                    .iter()
                    .map(|desc| desc.count)
                    .sum::<u32>();
            } else {
                let mut types = info.used_constant_buffer_types | info.used_indirect_cbuf_types;
                if types & Type::U8 as u32 != 0 && !self.profile.support_int8 {
                    types |= Type::U32 as u32;
                }
                if types & Type::U16 as u32 != 0 && !self.profile.support_int16 {
                    types |= Type::U32 as u32;
                }
                if types & Type::U32 as u32 != 0 {
                    self.define_constant_buffer_view(
                        &info.constant_buffer_descriptors,
                        first_binding,
                        self.u32_type,
                        4,
                        UniformDefinitionKind::U32,
                    );
                }
                if types & Type::F32 as u32 != 0 {
                    self.define_constant_buffer_view(
                        &info.constant_buffer_descriptors,
                        first_binding,
                        self.f32_type,
                        4,
                        UniformDefinitionKind::F32,
                    );
                }
                if types & Type::U32x2 as u32 != 0 {
                    self.define_constant_buffer_view(
                        &info.constant_buffer_descriptors,
                        first_binding,
                        self.u32_vec2_type,
                        8,
                        UniformDefinitionKind::U32x2,
                    );
                }
                if types & Type::U32x4 as u32 != 0 {
                    self.define_constant_buffer_view(
                        &info.constant_buffer_descriptors,
                        first_binding,
                        self.u32_vec4_type,
                        16,
                        UniformDefinitionKind::U32x4,
                    );
                }
                *binding += info.constant_buffer_descriptors.len() as u32;
            }
        }

        // Storage buffers (SSBOs). This mirrors upstream's non-descriptor-
        // aliasing storage path: one `uint[]` block per compact descriptor,
        // consumed by LoadStorage32/64/128 in emit_spirv_memory.rs.
        if !info.storage_buffers_descriptors.is_empty() {
            self.builder
                .extension("SPV_KHR_storage_buffer_storage_class");
            let array_type = self.builder.type_runtime_array(self.u32_type);
            self.builder.decorate(
                array_type,
                spirv::Decoration::ArrayStride,
                vec![Operand::LiteralBit32(4)],
            );

            let struct_type = self.builder.type_struct(vec![array_type]);
            self.builder
                .decorate(struct_type, spirv::Decoration::Block, vec![]);
            self.builder.member_decorate(
                struct_type,
                0,
                spirv::Decoration::Offset,
                vec![Operand::LiteralBit32(0)],
            );
            let ptr_type =
                self.builder
                    .type_pointer(None, spirv::StorageClass::StorageBuffer, struct_type);

            let binding_counter = if self.profile.unified_descriptor_binding {
                &mut bindings.unified
            } else {
                &mut bindings.storage_buffer
            };
            let mut descriptor_index = 0u32;
            for desc in &info.storage_buffers_descriptors {
                for _ in 0..desc.count {
                    let var = self.builder.variable(
                        ptr_type,
                        None,
                        spirv::StorageClass::StorageBuffer,
                        None,
                    );
                    self.builder.decorate(
                        var,
                        spirv::Decoration::DescriptorSet,
                        vec![Operand::LiteralBit32(0)],
                    );
                    self.builder.decorate(
                        var,
                        spirv::Decoration::Binding,
                        vec![Operand::LiteralBit32(*binding_counter)],
                    );
                    self.ssbo_vars.insert(descriptor_index, var);
                    if self.profile.supported_spirv >= 0x0001_0400 {
                        self.interfaces.push(var);
                    }
                    descriptor_index += 1;
                    *binding_counter += 1;
                }
            }
        }

        // Textures (combined image samplers)
        self.textures.reserve(info.texture_descriptors.len());
        for desc in &info.texture_descriptors {
            let binding = if self.profile.unified_descriptor_binding {
                &mut bindings.unified
            } else {
                &mut bindings.texture
            };
            let image_type = texture_image_type(self, desc);
            let sampled_image = self.builder.type_sampled_image(image_type);
            let pointer_type = self.builder.type_pointer(
                None,
                spirv::StorageClass::UniformConstant,
                sampled_image,
            );
            let descriptor_type = if desc.count > 1 {
                let count = self.builder.constant_bit32(self.u32_type, desc.count);
                self.builder.type_array(sampled_image, count)
            } else {
                sampled_image
            };
            let descriptor_pointer_type = self.builder.type_pointer(
                None,
                spirv::StorageClass::UniformConstant,
                descriptor_type,
            );
            let var = self.builder.variable(
                descriptor_pointer_type,
                None,
                spirv::StorageClass::UniformConstant,
                None,
            );
            self.builder.decorate(
                var,
                spirv::Decoration::DescriptorSet,
                vec![Operand::LiteralBit32(0)],
            );
            self.builder.decorate(
                var,
                spirv::Decoration::Binding,
                vec![Operand::LiteralBit32(*binding)],
            );

            self.textures.push(TextureDefinition {
                id: var,
                sampled_type: sampled_image,
                pointer_type,
                image_type,
                count: desc.count,
                is_multisample: desc.is_multisample,
            });
            if self.profile.supported_spirv >= 0x0001_0400 {
                self.interfaces.push(var);
            }
            *binding += 1;
        }

        // System value input variables
        if info.uses_workgroup_id {
            let ptr_type =
                self.builder
                    .type_pointer(None, spirv::StorageClass::Input, self.u32_vec3_type);
            let var = self
                .builder
                .variable(ptr_type, None, spirv::StorageClass::Input, None);
            self.builder.decorate(
                var,
                spirv::Decoration::BuiltIn,
                vec![Operand::BuiltIn(spirv::BuiltIn::WorkgroupId)],
            );
            self.workgroup_id = var;
            self.interfaces.push(var);
        }
        if info.uses_local_invocation_id {
            let ptr_type =
                self.builder
                    .type_pointer(None, spirv::StorageClass::Input, self.u32_vec3_type);
            let var = self
                .builder
                .variable(ptr_type, None, spirv::StorageClass::Input, None);
            self.builder.decorate(
                var,
                spirv::Decoration::BuiltIn,
                vec![Operand::BuiltIn(spirv::BuiltIn::LocalInvocationId)],
            );
            self.local_invocation_id = var;
            self.interfaces.push(var);
        }
        if info.uses_invocation_id {
            let ptr_type =
                self.builder
                    .type_pointer(None, spirv::StorageClass::Input, self.u32_type);
            let var = self
                .builder
                .variable(ptr_type, None, spirv::StorageClass::Input, None);
            self.builder.decorate(
                var,
                spirv::Decoration::BuiltIn,
                vec![Operand::BuiltIn(spirv::BuiltIn::InvocationId)],
            );
            self.invocation_id = var;
            self.interfaces.push(var);
        }
        if info.uses_invocation_info
            && (self.stage == ShaderStage::TessellationControl
                || self.stage == ShaderStage::TessellationEval)
        {
            let ptr_type =
                self.builder
                    .type_pointer(None, spirv::StorageClass::Input, self.u32_type);
            let var = self
                .builder
                .variable(ptr_type, None, spirv::StorageClass::Input, None);
            self.builder.decorate(
                var,
                spirv::Decoration::BuiltIn,
                vec![Operand::BuiltIn(spirv::BuiltIn::PatchVertices)],
            );
            self.patch_vertices_in = var;
            self.interfaces.push(var);
        }
        if info.uses_sample_id {
            let ptr_type =
                self.builder
                    .type_pointer(None, spirv::StorageClass::Input, self.u32_type);
            let var = self
                .builder
                .variable(ptr_type, None, spirv::StorageClass::Input, None);
            self.builder.decorate(
                var,
                spirv::Decoration::BuiltIn,
                vec![Operand::BuiltIn(spirv::BuiltIn::SampleId)],
            );
            self.sample_id = var;
            self.interfaces.push(var);
        }
        if info.uses_is_helper_invocation {
            let ptr_type =
                self.builder
                    .type_pointer(None, spirv::StorageClass::Input, self.bool_type);
            let var = self
                .builder
                .variable(ptr_type, None, spirv::StorageClass::Input, None);
            self.builder.decorate(
                var,
                spirv::Decoration::BuiltIn,
                vec![Operand::BuiltIn(spirv::BuiltIn::HelperInvocation)],
            );
            self.is_helper_invocation = var;
            self.interfaces.push(var);
        }

        use crate::ir::value::Attribute as IrAttribute;
        if info.loads.get(IrAttribute::PRIMITIVE_ID.0 as usize) {
            self.primitive_id = self.define_builtin_u32_input(spirv::BuiltIn::PrimitiveId);
        }
        if info.loads.get(IrAttribute::LAYER.0 as usize) {
            self.layer = self.define_builtin_u32_input(spirv::BuiltIn::Layer);
        }
        if info.loads.get(IrAttribute::INSTANCE_ID.0 as usize) {
            if self.profile.support_vertex_instance_id {
                self.instance_id = self.define_builtin_u32_input(spirv::BuiltIn::InstanceId);
                if info.loads.get(IrAttribute::BASE_INSTANCE.0 as usize) {
                    self.base_instance =
                        self.define_builtin_u32_input(spirv::BuiltIn::BaseInstance);
                }
            } else {
                self.instance_index = self.define_builtin_u32_input(spirv::BuiltIn::InstanceIndex);
                self.base_instance = self.define_builtin_u32_input(spirv::BuiltIn::BaseInstance);
            }
        } else if info.loads.get(IrAttribute::BASE_INSTANCE.0 as usize) {
            self.base_instance = self.define_builtin_u32_input(spirv::BuiltIn::BaseInstance);
        }
        if info.loads.get(IrAttribute::VERTEX_ID.0 as usize) {
            if self.profile.support_vertex_instance_id {
                self.vertex_id = self.define_builtin_u32_input(spirv::BuiltIn::VertexId);
                if info.loads.get(IrAttribute::BASE_VERTEX.0 as usize) {
                    self.base_vertex = self.define_builtin_u32_input(spirv::BuiltIn::BaseVertex);
                }
            } else {
                self.vertex_index = self.define_builtin_u32_input(spirv::BuiltIn::VertexIndex);
                self.base_vertex = self.define_builtin_u32_input(spirv::BuiltIn::BaseVertex);
            }
        } else if info.loads.get(IrAttribute::BASE_VERTEX.0 as usize) {
            self.base_vertex = self.define_builtin_u32_input(spirv::BuiltIn::BaseVertex);
        }
        if info.loads.get(IrAttribute::DRAW_ID.0 as usize) {
            self.draw_index = self.define_builtin_u32_input(spirv::BuiltIn::DrawIndex);
        }
        if info.loads.any_component(IrAttribute::POSITION_X.0 as usize) {
            let built_in = if self.stage == ShaderStage::Fragment {
                spirv::BuiltIn::FragCoord
            } else {
                spirv::BuiltIn::Position
            };
            self.input_position = self.define_builtin_f32_vec4_input(built_in);
        }
        if info.loads.get(IrAttribute::FRONT_FACE.0 as usize) {
            let ptr_type =
                self.builder
                    .type_pointer(None, spirv::StorageClass::Input, self.bool_type);
            let var = self
                .builder
                .variable(ptr_type, None, spirv::StorageClass::Input, None);
            self.builder.decorate(
                var,
                spirv::Decoration::BuiltIn,
                vec![Operand::BuiltIn(spirv::BuiltIn::FrontFacing)],
            );
            self.front_face = var;
            self.interfaces.push(var);
        }
        if info.loads.get(IrAttribute::POINT_SPRITE_S.0 as usize)
            || info.loads.get(IrAttribute::POINT_SPRITE_T.0 as usize)
        {
            self.point_coord = self.define_builtin_f32_vec2_input(spirv::BuiltIn::PointCoord);
        }
        if info
            .loads
            .get(IrAttribute::TESSELLATION_EVALUATION_POINT_U.0 as usize)
            || info
                .loads
                .get(IrAttribute::TESSELLATION_EVALUATION_POINT_V.0 as usize)
        {
            self.tess_coord = self.define_builtin_f32_vec3_input(spirv::BuiltIn::TessCoord);
        }

        self.define_rescaling_input(info);
        self.define_render_area(info);
    }

    fn define_builtin_u32_input(&mut self, built_in: spirv::BuiltIn) -> spirv::Word {
        let ptr_type = self
            .builder
            .type_pointer(None, spirv::StorageClass::Input, self.u32_type);
        let var = self
            .builder
            .variable(ptr_type, None, spirv::StorageClass::Input, None);
        self.builder.decorate(
            var,
            spirv::Decoration::BuiltIn,
            vec![Operand::BuiltIn(built_in)],
        );
        self.interfaces.push(var);
        var
    }

    fn define_builtin_f32_vec2_input(&mut self, built_in: spirv::BuiltIn) -> spirv::Word {
        let ptr_type =
            self.builder
                .type_pointer(None, spirv::StorageClass::Input, self.f32_vec2_type);
        let var = self
            .builder
            .variable(ptr_type, None, spirv::StorageClass::Input, None);
        self.builder.decorate(
            var,
            spirv::Decoration::BuiltIn,
            vec![Operand::BuiltIn(built_in)],
        );
        self.interfaces.push(var);
        var
    }

    fn define_builtin_f32_vec3_input(&mut self, built_in: spirv::BuiltIn) -> spirv::Word {
        let ptr_type =
            self.builder
                .type_pointer(None, spirv::StorageClass::Input, self.f32_vec3_type);
        let var = self
            .builder
            .variable(ptr_type, None, spirv::StorageClass::Input, None);
        self.builder.decorate(
            var,
            spirv::Decoration::BuiltIn,
            vec![Operand::BuiltIn(built_in)],
        );
        self.interfaces.push(var);
        var
    }

    fn define_builtin_f32_vec4_input(&mut self, built_in: spirv::BuiltIn) -> spirv::Word {
        let ptr_type =
            self.builder
                .type_pointer(None, spirv::StorageClass::Input, self.f32_vec4_type);
        let var = self
            .builder
            .variable(ptr_type, None, spirv::StorageClass::Input, None);
        self.builder.decorate(
            var,
            spirv::Decoration::BuiltIn,
            vec![Operand::BuiltIn(built_in)],
        );
        self.interfaces.push(var);
        var
    }

    fn define_rescaling_input(&mut self, info: &ShaderInfo) {
        if !info.uses_rescaling_uniform {
            return;
        }
        if self.profile.unified_descriptor_binding {
            self.define_rescaling_input_push_constant();
        }
    }

    fn define_rescaling_input_push_constant(&mut self) {
        use super::emit_spirv::{
            NUM_IMAGE_SCALING_WORDS, NUM_TEXTURE_SCALING_WORDS,
            RESCALING_LAYOUT_DOWN_FACTOR_OFFSET, RESCALING_LAYOUT_WORDS_OFFSET,
        };

        let textures_len = self
            .builder
            .constant_bit32(self.u32_type, NUM_TEXTURE_SCALING_WORDS);
        let textures_type = self.builder.type_array(self.u32_type, textures_len);
        self.builder.decorate(
            textures_type,
            spirv::Decoration::ArrayStride,
            vec![Operand::LiteralBit32(4)],
        );

        let images_len = self
            .builder
            .constant_bit32(self.u32_type, NUM_IMAGE_SCALING_WORDS);
        let images_type = self.builder.type_array(self.u32_type, images_len);
        self.builder.decorate(
            images_type,
            spirv::Decoration::ArrayStride,
            vec![Operand::LiteralBit32(4)],
        );

        let mut members = vec![textures_type, images_type];
        if self.stage != ShaderStage::Compute {
            self.rescaling_downfactor_member_index = members.len() as u32;
            members.push(self.f32_type);
        }

        let push_constant_struct = self.builder.type_struct(members);
        self.builder
            .decorate(push_constant_struct, spirv::Decoration::Block, vec![]);
        self.builder.member_decorate(
            push_constant_struct,
            0,
            spirv::Decoration::Offset,
            vec![Operand::LiteralBit32(RESCALING_LAYOUT_WORDS_OFFSET)],
        );
        self.builder.member_decorate(
            push_constant_struct,
            1,
            spirv::Decoration::Offset,
            vec![Operand::LiteralBit32(16)],
        );
        if self.stage != ShaderStage::Compute {
            self.builder.member_decorate(
                push_constant_struct,
                self.rescaling_downfactor_member_index,
                spirv::Decoration::Offset,
                vec![Operand::LiteralBit32(RESCALING_LAYOUT_DOWN_FACTOR_OFFSET)],
            );
        }

        let pointer_type = self.builder.type_pointer(
            None,
            spirv::StorageClass::PushConstant,
            push_constant_struct,
        );
        self.rescaling_push_constants =
            self.builder
                .variable(pointer_type, None, spirv::StorageClass::PushConstant, None);
        if self.profile.supported_spirv >= 0x0001_0400 {
            self.interfaces.push(self.rescaling_push_constants);
        }
    }

    fn define_render_area(&mut self, info: &ShaderInfo) {
        if !info.uses_render_area || !self.profile.unified_descriptor_binding {
            return;
        }

        self.render_are_member_index = 0;
        let push_constant_struct = self.builder.type_struct(vec![self.f32_vec4_type]);
        self.builder
            .decorate(push_constant_struct, spirv::Decoration::Block, vec![]);
        self.builder.member_decorate(
            push_constant_struct,
            self.render_are_member_index,
            spirv::Decoration::Offset,
            vec![Operand::LiteralBit32(0)],
        );

        let pointer_type = self.builder.type_pointer(
            None,
            spirv::StorageClass::PushConstant,
            push_constant_struct,
        );
        self.render_area_push_constant =
            self.builder
                .variable(pointer_type, None, spirv::StorageClass::PushConstant, None);
        if self.profile.supported_spirv >= 0x0001_0400 {
            self.interfaces.push(self.render_area_push_constant);
        }
    }

    fn phi_type_id(&self, inst: &ir::Inst) -> spirv::Word {
        use crate::ir::types::Type;
        match inst.flags {
            x if x == Type::U1 as u32 => self.bool_type,
            x if x == Type::U8 as u32 || x == Type::U16 as u32 || x == Type::U32 as u32 => {
                self.u32_type
            }
            x if x == Type::U64 as u32 => self.u64_type,
            x if x == Type::F32 as u32 => self.f32_type,
            x if x == Type::F64 as u32 => self.f64_type,
            flags => panic!("SPIR-V: unimplemented Phi result type flags {flags:#x}"),
        }
    }

    fn begin_ir_block(&mut self, block_idx: u32) {
        let label = self
            .block_labels
            .get(block_idx as usize)
            .copied()
            .unwrap_or_else(|| panic!("SPIR-V: missing label for block {block_idx}"));
        self.builder.begin_block(Some(label)).unwrap();
    }

    fn add_float_execution_mode(
        &mut self,
        main_fn: spirv::Word,
        capability: spirv::Capability,
        mode: spirv::ExecutionMode,
        width: u32,
    ) {
        self.builder.capability(capability);
        self.builder.execution_mode(main_fn, mode, vec![width]);
    }

    fn setup_denorm_control(&mut self, program: &ir::Program, main_fn: spirv::Word) {
        let info = &program.info;
        if !(info.uses_fp32_denorms_flush && info.uses_fp32_denorms_preserve) {
            if info.uses_fp32_denorms_flush && self.profile.support_fp32_denorm_flush {
                self.add_float_execution_mode(
                    main_fn,
                    spirv::Capability::DenormFlushToZero,
                    spirv::ExecutionMode::DenormFlushToZero,
                    32,
                );
            } else if info.uses_fp32_denorms_preserve && self.profile.support_fp32_denorm_preserve {
                self.add_float_execution_mode(
                    main_fn,
                    spirv::Capability::DenormPreserve,
                    spirv::ExecutionMode::DenormPreserve,
                    32,
                );
            }
        }
        if !self.profile.support_separate_denorm_behavior
            || self.profile.has_broken_fp16_float_controls
        {
            return;
        }
        if !(info.uses_fp16_denorms_flush && info.uses_fp16_denorms_preserve) {
            if info.uses_fp16_denorms_flush && self.profile.support_fp16_denorm_flush {
                self.add_float_execution_mode(
                    main_fn,
                    spirv::Capability::DenormFlushToZero,
                    spirv::ExecutionMode::DenormFlushToZero,
                    16,
                );
            } else if info.uses_fp16_denorms_preserve && self.profile.support_fp16_denorm_preserve {
                self.add_float_execution_mode(
                    main_fn,
                    spirv::Capability::DenormPreserve,
                    spirv::ExecutionMode::DenormPreserve,
                    16,
                );
            }
        }
    }

    fn setup_signed_nan_capabilities(&mut self, program: &ir::Program, main_fn: spirv::Word) {
        let info = &program.info;
        if self.profile.has_broken_fp16_float_controls && info.uses_fp16 {
            return;
        }
        if info.uses_fp16 && self.profile.support_fp16_signed_zero_nan_preserve {
            self.add_float_execution_mode(
                main_fn,
                spirv::Capability::SignedZeroInfNanPreserve,
                spirv::ExecutionMode::SignedZeroInfNanPreserve,
                16,
            );
        }
        if self.profile.support_fp32_signed_zero_nan_preserve {
            self.add_float_execution_mode(
                main_fn,
                spirv::Capability::SignedZeroInfNanPreserve,
                spirv::ExecutionMode::SignedZeroInfNanPreserve,
                32,
            );
        }
        if info.uses_fp64 && self.profile.support_fp64_signed_zero_nan_preserve {
            self.add_float_execution_mode(
                main_fn,
                spirv::Capability::SignedZeroInfNanPreserve,
                spirv::ExecutionMode::SignedZeroInfNanPreserve,
                64,
            );
        }
    }

    fn setup_float_controls(&mut self, program: &ir::Program, main_fn: spirv::Word) {
        if !self.profile.support_float_controls {
            return;
        }
        self.builder.extension("SPV_KHR_float_controls");
        self.setup_denorm_control(program, main_fn);
        self.setup_signed_nan_capabilities(program, main_fn);
    }

    fn emit_block_instructions(&mut self, program: &ir::Program, block_idx: u32) {
        let block = program
            .blocks
            .get(block_idx as usize)
            .unwrap_or_else(|| panic!("SPIR-V: syntax references missing block {block_idx}"));
        for (inst_idx, inst) in block.indexed_iter() {
            if matches!(inst.opcode, ir::Opcode::Phi) {
                self.emit_instruction(program, inst, block_idx, inst_idx);
            }
        }
        for (inst_idx, inst) in block.indexed_iter() {
            if matches!(
                inst.opcode,
                ir::Opcode::UndefU1
                    | ir::Opcode::UndefU8
                    | ir::Opcode::UndefU16
                    | ir::Opcode::UndefU32
                    | ir::Opcode::UndefU64
            ) {
                self.emit_instruction(program, inst, block_idx, inst_idx);
            }
        }
        for (inst_idx, inst) in block.indexed_iter() {
            if matches!(
                inst.opcode,
                ir::Opcode::Phi
                    | ir::Opcode::UndefU1
                    | ir::Opcode::UndefU8
                    | ir::Opcode::UndefU16
                    | ir::Opcode::UndefU32
                    | ir::Opcode::UndefU64
            ) {
                continue;
            }
            self.emit_instruction(program, inst, block_idx, inst_idx);
        }
    }

    /// Define the main() function and emit IR instructions as SPIR-V.
    pub fn define_main_function(&mut self, program: &ir::Program) {
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

        self.block_labels = (0..program.blocks.len())
            .map(|_| self.builder.id())
            .collect();

        let syntax_list = if program.syntax_list.is_empty() {
            let mut list = Vec::with_capacity(program.blocks.len() + 1);
            for block_idx in 0..program.blocks.len() as u32 {
                list.push(ir::SyntaxNode::Block(block_idx));
            }
            list.push(ir::SyntaxNode::Return);
            list
        } else {
            program.syntax_list.clone()
        };

        let mut current_block: Option<u32> = None;
        for node in &syntax_list {
            match *node {
                ir::SyntaxNode::Block(block_idx) => {
                    let label = self.block_labels[block_idx as usize];
                    if current_block.is_some() {
                        self.builder.branch(label).unwrap();
                    }
                    current_block = Some(block_idx);
                    self.begin_ir_block(block_idx);
                    self.emit_block_instructions(program, block_idx);
                }
                ir::SyntaxNode::If { cond, body, merge } => {
                    let if_label = self.block_labels[body as usize];
                    let endif_label = self.block_labels[merge as usize];
                    let cond = self.resolve_value(&cond);
                    self.builder
                        .selection_merge(endif_label, spirv::SelectionControl::NONE)
                        .unwrap();
                    self.builder
                        .branch_conditional(cond, if_label, endif_label, std::iter::empty())
                        .unwrap();
                    current_block = None;
                }
                ir::SyntaxNode::Loop {
                    body,
                    continue_block,
                    merge,
                } => {
                    let body_label = self.block_labels[body as usize];
                    let continue_label = self.block_labels[continue_block as usize];
                    let endloop_label = self.block_labels[merge as usize];
                    self.builder
                        .loop_merge(
                            endloop_label,
                            continue_label,
                            spirv::LoopControl::NONE,
                            std::iter::empty(),
                        )
                        .unwrap();
                    self.builder.branch(body_label).unwrap();
                    current_block = None;
                }
                ir::SyntaxNode::Break { cond, merge, skip } => {
                    let break_label = self.block_labels[merge as usize];
                    let skip_label = self.block_labels[skip as usize];
                    let cond = self.resolve_value(&cond);
                    self.builder
                        .branch_conditional(cond, break_label, skip_label, std::iter::empty())
                        .unwrap();
                    current_block = None;
                }
                ir::SyntaxNode::EndIf { merge } => {
                    if current_block.is_some() {
                        self.builder
                            .branch(self.block_labels[merge as usize])
                            .unwrap();
                    }
                    current_block = None;
                }
                ir::SyntaxNode::Repeat {
                    cond,
                    loop_header,
                    merge,
                } => {
                    let cond = self.resolve_value(&cond);
                    let loop_header_label = self.block_labels[loop_header as usize];
                    let merge_label = self.block_labels[merge as usize];
                    self.builder
                        .branch_conditional(
                            cond,
                            loop_header_label,
                            merge_label,
                            std::iter::empty(),
                        )
                        .unwrap();
                    current_block = None;
                }
                ir::SyntaxNode::Return => {
                    self.builder.ret().unwrap();
                    current_block = None;
                }
                ir::SyntaxNode::Unreachable => {
                    self.builder.unreachable().unwrap();
                    current_block = None;
                }
            }
        }

        self.builder.end_function().unwrap();

        // Entry point
        let exec_model = match self.stage {
            ShaderStage::VertexB => spirv::ExecutionModel::Vertex,
            ShaderStage::Fragment => spirv::ExecutionModel::Fragment,
            ShaderStage::Compute => spirv::ExecutionModel::GLCompute,
            ShaderStage::Geometry => spirv::ExecutionModel::Geometry,
            ShaderStage::TessellationControl => spirv::ExecutionModel::TessellationControl,
            ShaderStage::TessellationEval => spirv::ExecutionModel::TessellationEvaluation,
            ShaderStage::VertexA => {
                unreachable!("VertexA must be merged into VertexB before SPIR-V emission")
            }
        };

        self.builder
            .entry_point(exec_model, main_fn, "main", self.interfaces.clone());

        if self.stage == ShaderStage::Fragment {
            self.builder
                .execution_mode(main_fn, spirv::ExecutionMode::OriginUpperLeft, vec![]);
            if program.info.stores_frag_depth {
                self.builder
                    .execution_mode(main_fn, spirv::ExecutionMode::DepthReplacing, vec![]);
            }
        }
        self.setup_float_controls(program, main_fn);
    }

    /// Emit a single IR instruction as SPIR-V.
    fn emit_instruction(
        &mut self,
        program: &ir::Program,
        inst: &ir::Inst,
        block_idx: u32,
        inst_idx: u32,
    ) {
        use ir::Opcode;
        match inst.opcode {
            // ── FP32 arithmetic ───────────────────────────────────────
            Opcode::FPAdd32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_floating_point::emit_fp_add_32(self, inst, a, b);
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
                let id = super::emit_spirv_floating_point::emit_fp_mul_32(self, inst, a, b);
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
                let id = super::emit_spirv_floating_point::emit_fp_fma_32(self, inst, a, b, c);
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
            Opcode::FPClamp32 => {
                let value = self.resolve_value(inst.arg(0));
                let min = self.resolve_value(inst.arg(1));
                let max = self.resolve_value(inst.arg(2));
                let id = super::emit_spirv_floating_point::emit_fp_clamp_32(self, value, min, max);
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

            // ── FP64 arithmetic ───────────────────────────────────────
            Opcode::FPAdd64 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_floating_point::emit_fp_add_64(self, inst, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPMul64 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_floating_point::emit_fp_mul_64(self, inst, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPFma64 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let c = self.resolve_value(inst.arg(2));
                let id = super::emit_spirv_floating_point::emit_fp_fma_64(self, inst, a, b, c);
                self.set_value(block_idx, inst_idx, id);
            }

            // ── FP comparison ─────────────────────────────────────────
            Opcode::FPOrdEqual16 | Opcode::FPOrdEqual64 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_floating_point::emit_fp_ord_equal(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPOrdEqual32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_floating_point::emit_fp_ord_equal_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPOrdNotEqual16 | Opcode::FPOrdNotEqual64 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_floating_point::emit_fp_ord_not_equal(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPOrdNotEqual32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_floating_point::emit_fp_ord_not_equal_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPOrdLessThan16 | Opcode::FPOrdLessThan64 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_floating_point::emit_fp_ord_less_than(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPOrdLessThan32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_floating_point::emit_fp_ord_less_than_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPOrdGreaterThan16 | Opcode::FPOrdGreaterThan64 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_floating_point::emit_fp_ord_greater_than(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPOrdGreaterThan32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_floating_point::emit_fp_ord_greater_than_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPOrdLessThanEqual16 | Opcode::FPOrdLessThanEqual64 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_floating_point::emit_fp_ord_less_than_equal(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPOrdLessThanEqual32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id =
                    super::emit_spirv_floating_point::emit_fp_ord_less_than_equal_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPOrdGreaterThanEqual16 | Opcode::FPOrdGreaterThanEqual64 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id =
                    super::emit_spirv_floating_point::emit_fp_ord_greater_than_equal(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPOrdGreaterThanEqual32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id =
                    super::emit_spirv_floating_point::emit_fp_ord_greater_than_equal_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPUnordEqual16 | Opcode::FPUnordEqual64 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_floating_point::emit_fp_unord_equal(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPUnordEqual32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_floating_point::emit_fp_unord_equal_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPUnordNotEqual16 | Opcode::FPUnordNotEqual64 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_floating_point::emit_fp_unord_not_equal(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPUnordNotEqual32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_floating_point::emit_fp_unord_not_equal_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPUnordLessThan16 | Opcode::FPUnordLessThan64 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_floating_point::emit_fp_unord_less_than(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPUnordLessThan32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_floating_point::emit_fp_unord_less_than_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPUnordGreaterThan16 | Opcode::FPUnordGreaterThan64 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_floating_point::emit_fp_unord_greater_than(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPUnordGreaterThan32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id =
                    super::emit_spirv_floating_point::emit_fp_unord_greater_than_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPUnordLessThanEqual16 | Opcode::FPUnordLessThanEqual64 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id =
                    super::emit_spirv_floating_point::emit_fp_unord_less_than_equal(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPUnordLessThanEqual32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id =
                    super::emit_spirv_floating_point::emit_fp_unord_less_than_equal_32(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPUnordGreaterThanEqual16 | Opcode::FPUnordGreaterThanEqual64 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id =
                    super::emit_spirv_floating_point::emit_fp_unord_greater_than_equal(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPUnordGreaterThanEqual32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_floating_point::emit_fp_unord_greater_than_equal_32(
                    self, a, b,
                );
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::FPIsNan16 | Opcode::FPIsNan64 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_floating_point::emit_fp_is_nan(self, a);
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
                let id = super::emit_spirv_integer::emit_iadd_32(self, inst, a, b);
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
            Opcode::INeg64 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_integer::emit_ineg_64(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::IAbs32 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_integer::emit_iabs_32(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::IAbs64 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_integer::emit_iabs_64(self, a);
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
                let id = super::emit_spirv_integer::emit_bitwise_and_32(self, inst, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::BitwiseOr32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_integer::emit_bitwise_or_32(self, inst, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::BitwiseXor32 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_integer::emit_bitwise_xor_32(self, inst, a, b);
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
                let id = super::emit_spirv_integer::emit_bit_field_s_extract(
                    self, inst, base, offset, count,
                );
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::BitFieldUExtract => {
                let base = self.resolve_value(inst.arg(0));
                let offset = self.resolve_value(inst.arg(1));
                let count = self.resolve_value(inst.arg(2));
                let id = super::emit_spirv_integer::emit_bit_field_u_extract(
                    self, inst, base, offset, count,
                );
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
            Opcode::SClamp32 => {
                let value = self.resolve_value(inst.arg(0));
                let min = self.resolve_value(inst.arg(1));
                let max = self.resolve_value(inst.arg(2));
                let id = super::emit_spirv_integer::emit_s_clamp_32(self, inst, value, min, max);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::UClamp32 => {
                let value = self.resolve_value(inst.arg(0));
                let min = self.resolve_value(inst.arg(1));
                let max = self.resolve_value(inst.arg(2));
                let id = super::emit_spirv_integer::emit_u_clamp_32(self, inst, value, min, max);
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
            Opcode::SelectU64 => {
                let cond = self.resolve_value(inst.arg(0));
                let t = self.resolve_value(inst.arg(1));
                let f = self.resolve_value(inst.arg(2));
                let id = super::emit_spirv_select::emit_select_u64(self, cond, t, f);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::SelectF16 => {
                let cond = self.resolve_value(inst.arg(0));
                let t = self.resolve_value(inst.arg(1));
                let f = self.resolve_value(inst.arg(2));
                let id = super::emit_spirv_select::emit_select_f16(self, cond, t, f);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::SelectF32 => {
                let cond = self.resolve_value(inst.arg(0));
                let t = self.resolve_value(inst.arg(1));
                let f = self.resolve_value(inst.arg(2));
                let id = super::emit_spirv_select::emit_select_f32(self, cond, t, f);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::SelectF64 => {
                let cond = self.resolve_value(inst.arg(0));
                let t = self.resolve_value(inst.arg(1));
                let f = self.resolve_value(inst.arg(2));
                let id = super::emit_spirv_select::emit_select_f64(self, cond, t, f);
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
            Opcode::ConvertF16S8 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_convert::emit_convert_f16_s8(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::ConvertF16S16 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_convert::emit_convert_f16_s16(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::ConvertF16S32 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_convert::emit_convert_f16_s32(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::ConvertF16S64 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_convert::emit_convert_f16_s64(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::ConvertF16U8 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_convert::emit_convert_f16_u8(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::ConvertF16U16 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_convert::emit_convert_f16_u16(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::ConvertF16U32 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_convert::emit_convert_f16_u32(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::ConvertF16U64 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_convert::emit_convert_f16_u64(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::ConvertF32S8 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_convert::emit_convert_f32_s8(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::ConvertF32S16 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_convert::emit_convert_f32_s16(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::ConvertF32S64 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_convert::emit_convert_f32_s64(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::ConvertF32U8 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_convert::emit_convert_f32_u8(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::ConvertF32U16 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_convert::emit_convert_f32_u16(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::ConvertF32U64 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_convert::emit_convert_f32_u64(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::ConvertF64S8 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_convert::emit_convert_f64_s8(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::ConvertF64S16 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_convert::emit_convert_f64_s16(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::ConvertF64S32 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_convert::emit_convert_f64_s32(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::ConvertF64S64 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_convert::emit_convert_f64_s64(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::ConvertF64U8 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_convert::emit_convert_f64_u8(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::ConvertF64U16 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_convert::emit_convert_f64_u16(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::ConvertF64U32 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_convert::emit_convert_f64_u32(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::ConvertF64U64 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_convert::emit_convert_f64_u64(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::BitCastU32F32 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_bitwise_conversion::emit_bit_cast_u32_f32(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::BitCastF32U32 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_bitwise_conversion::emit_bit_cast_f32_u32(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::PackUint2x32 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_bitwise_conversion::emit_pack_uint2x32(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::UnpackUint2x32 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_bitwise_conversion::emit_unpack_uint2x32(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::PackDouble2x32 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_bitwise_conversion::emit_pack_double2x32(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::UnpackDouble2x32 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_bitwise_conversion::emit_unpack_double2x32(self, a);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::PackFloat2x16 => {
                let a = self.resolve_value(inst.arg(0));
                let id = super::emit_spirv_bitwise_conversion::emit_pack_float2x16(self, a);
                self.set_value(block_idx, inst_idx, id);
            }

            // ── Composite ─────────────────────────────────────────────
            Opcode::CompositeConstructF16x2 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_composite::emit_composite_construct_f16x2(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::CompositeConstructF32x2 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let id = super::emit_spirv_composite::emit_composite_construct_f32x2(self, a, b);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::CompositeConstructF32x3 => {
                let a = self.resolve_value(inst.arg(0));
                let b = self.resolve_value(inst.arg(1));
                let c = self.resolve_value(inst.arg(2));
                let id = super::emit_spirv_composite::emit_composite_construct_f32x3(self, a, b, c);
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
                let id = super::emit_spirv_composite::emit_composite_construct_u32x2(self, a, b);
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
            Opcode::GetCbufU8
            | Opcode::GetCbufS8
            | Opcode::GetCbufU16
            | Opcode::GetCbufS16
            | Opcode::GetCbufU32
            | Opcode::GetCbufF32
            | Opcode::GetCbufU32x2 => {
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
                super::emit_spirv_image::emit_image_sample(
                    self, program, inst, block_idx, inst_idx,
                );
            }
            Opcode::ImageSampleDrefImplicitLod | Opcode::ImageSampleDrefExplicitLod => {
                super::emit_spirv_image::emit_image_sample_dref(
                    self, program, inst, block_idx, inst_idx,
                );
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
            Opcode::LoadGlobal32
            | Opcode::LoadLocal
            | Opcode::LoadStorage32
            | Opcode::LoadStorage64
            | Opcode::LoadStorage128 => {
                super::emit_spirv_memory::emit_load(self, inst, block_idx, inst_idx);
            }
            Opcode::WriteGlobal32 | Opcode::WriteLocal | Opcode::WriteStorage32 => {
                super::emit_spirv_memory::emit_store(self, inst, block_idx, inst_idx);
            }

            // ── Control ───────────────────────────────────────────────
            Opcode::DemoteToHelperInvocation => {
                super::emit_spirv_control_flow::emit_demote_to_helper_invocation(self);
            }
            Opcode::Barrier => {
                super::emit_spirv_barriers::emit_barrier(self);
            }

            // ── Register/predicate access — these are handled during
            //    SSA construction and don't emit SPIR-V directly ───────
            Opcode::Phi => {
                if inst.phi_args.is_empty() {
                    let id = self.builder.undef(self.phi_type_id(inst), None);
                    self.set_value(block_idx, inst_idx, id);
                    return;
                }
                let result_id = self.builder.id();
                self.set_value(block_idx, inst_idx, result_id);
                let incoming: Vec<_> = inst
                    .phi_args
                    .iter()
                    .map(|(block, _)| {
                        let label = self
                            .block_labels
                            .get(*block as usize)
                            .copied()
                            .unwrap_or_else(|| {
                                panic!("SPIR-V: Phi references missing block {block}")
                            });
                        (0, label)
                    })
                    .collect();
                self.builder
                    .phi(self.phi_type_id(inst), Some(result_id), incoming)
                    .unwrap();
                self.deferred_phis.push(DeferredPhi {
                    result_id,
                    values: inst.phi_args.iter().map(|(_, value)| *value).collect(),
                });
            }
            Opcode::Identity | Opcode::ConditionRef => {
                let id = self.resolve_value(inst.arg(0));
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::Prologue => {
                super::emit_spirv_special::emit_prologue(self);
            }
            Opcode::Epilogue => {
                super::emit_spirv_special::emit_epilogue(self);
            }
            Opcode::GetRegister
            | Opcode::SetRegister
            | Opcode::GetPred
            | Opcode::SetPred
            | Opcode::Void
            | Opcode::GetZeroFromOp
            | Opcode::GetSignFromOp
            | Opcode::GetCarryFromOp
            | Opcode::GetOverflowFromOp
            | Opcode::GetZFlag
            | Opcode::SetZFlag
            | Opcode::GetSFlag
            | Opcode::SetSFlag
            | Opcode::GetCFlag
            | Opcode::SetCFlag
            | Opcode::GetOFlag
            | Opcode::SetOFlag
            | Opcode::Reference
            | Opcode::PhiMove
            | Opcode::GetGotoVariable
            | Opcode::SetGotoVariable
            | Opcode::GetIndirectBranchVariable
            | Opcode::SetIndirectBranchVariable
            | Opcode::Join => {
                // No SPIR-V emission needed
            }

            // System values
            Opcode::WorkgroupId => {
                let id = super::emit_spirv_context_get_set::emit_workgroup_id(self);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::LocalInvocationId => {
                let id = super::emit_spirv_context_get_set::emit_local_invocation_id(self);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::InvocationId => {
                let id = super::emit_spirv_context_get_set::emit_invocation_id(self);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::InvocationInfo => {
                let id = super::emit_spirv_context_get_set::emit_invocation_info(self);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::IsHelperInvocation => {
                let id = super::emit_spirv_context_get_set::emit_is_helper_invocation(self);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::SampleId => {
                let id = super::emit_spirv_context_get_set::emit_sample_id(self);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::YDirection => {
                let id = super::emit_spirv_context_get_set::emit_y_direction(self);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::ResolutionDownFactor => {
                let id = super::emit_spirv_context_get_set::emit_resolution_down_factor(self);
                self.set_value(block_idx, inst_idx, id);
            }
            Opcode::RenderArea => {
                let id = super::emit_spirv_context_get_set::emit_render_area(self);
                self.set_value(block_idx, inst_idx, id);
            }

            // Undefined values
            Opcode::UndefU1
            | Opcode::UndefU8
            | Opcode::UndefU16
            | Opcode::UndefU32
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
            ir::Value::Inst(r) => *self.values.get(&(r.block, r.inst)).unwrap_or_else(|| {
                panic!(
                    "SPIR-V: unresolved IR value reference block={} inst={}",
                    r.block, r.inst
                )
            }),
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
            other => panic!("SPIR-V: unsupported immediate/reference value {other:?}"),
        }
    }

    /// Store a result ID for an IR instruction.
    pub fn set_value(&mut self, block_idx: u32, inst_idx: u32, id: spirv::Word) {
        self.values.insert((block_idx, inst_idx), id);
    }

    /// Port of upstream `PatchPhiNodes` + Sirit's `PatchDeferredPhi`.
    fn patch_deferred_phis(&mut self) {
        for deferred in std::mem::take(&mut self.deferred_phis) {
            let values: Vec<_> = deferred
                .values
                .iter()
                .map(|value| self.resolve_value(value))
                .collect();
            let phi = self
                .builder
                .module_mut()
                .functions
                .iter_mut()
                .flat_map(|function| function.blocks.iter_mut())
                .flat_map(|block| block.instructions.iter_mut())
                .find(|inst| {
                    inst.class.opcode == spirv::Op::Phi
                        && inst.result_id == Some(deferred.result_id)
                })
                .unwrap_or_else(|| {
                    panic!(
                        "SPIR-V: deferred Phi result {} was not emitted",
                        deferred.result_id
                    )
                });
            assert_eq!(
                phi.operands.len(),
                values.len() * 2,
                "SPIR-V: deferred Phi operand count changed before patching"
            );
            for (index, value) in values.into_iter().enumerate() {
                phi.operands[index * 2] = Operand::IdRef(value);
            }
        }
    }

    /// Emit the complete program (entry point used by emit_spirv.rs).
    pub fn emit_program(&mut self, program: &ir::Program) {
        let mut bindings = Bindings::default();
        self.emit_program_with_bindings(program, &mut bindings);
    }

    pub fn emit_program_with_bindings(&mut self, program: &ir::Program, bindings: &mut Bindings) {
        self.define_global_variables(program, bindings);
        self.define_main_function(program);
        self.patch_deferred_phis();
    }

    /// Finalize and return SPIR-V words.
    pub fn finalize(self) -> Vec<u32> {
        let module = self.builder.module();
        let mut words = Vec::new();
        module.assemble_into(&mut words);
        words
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::bindings::Bindings;
    use crate::ir::basic_block::Block;
    use crate::ir::emitter::Emitter;
    use crate::ir::instruction::Inst;
    use crate::ir::opcodes::Opcode;
    use crate::ir::types::ShaderStage;
    use crate::ir::types::Type;
    use crate::ir::value::{Attribute, InstRef, Value};

    #[test]
    fn fragment_depth_declares_builtin_mode_and_store() {
        let mut program = ir::Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());
        {
            let mut emitter = Emitter::new(&mut program, 0);
            emitter.set_frag_depth(Value::ImmF32(0.25));
        }
        program.info.stores_frag_depth = true;
        program.syntax_list = vec![ir::SyntaxNode::Block(0), ir::SyntaxNode::Return];

        let profile = Profile::default();
        let runtime_info = RuntimeInfo {
            convert_depth_mode: true,
            ..RuntimeInfo::default()
        };
        let mut ctx = SpirvEmitContext::new(&program, &profile, &runtime_info);
        ctx.emit_program(&program);

        let frag_depth = ctx.frag_depth;
        let module = ctx.builder.module_ref();
        assert_ne!(frag_depth, 0);
        assert!(module.annotations.iter().any(|inst| {
            matches!(
                inst.operands.as_slice(),
                [
                    Operand::IdRef(id),
                    Operand::Decoration(spirv::Decoration::BuiltIn),
                    Operand::BuiltIn(spirv::BuiltIn::FragDepth)
                ] if *id == frag_depth
            )
        }));
        assert!(module.execution_modes.iter().any(|inst| {
            matches!(
                inst.operands.as_slice(),
                [
                    Operand::IdRef(_),
                    Operand::ExecutionMode(spirv::ExecutionMode::DepthReplacing)
                ]
            )
        }));
        assert!(module.functions.iter().any(|function| {
            function.blocks.iter().any(|block| {
                block.instructions.iter().any(|inst| {
                    inst.class.opcode == spirv::Op::Store
                        && matches!(inst.operands.first(), Some(Operand::IdRef(id)) if *id == frag_depth)
                })
            })
        }));
        assert!(module.functions.iter().any(|function| {
            function.blocks.iter().any(|block| {
                block
                    .instructions
                    .iter()
                    .any(|inst| inst.class.opcode == spirv::Op::ExtInst)
            })
        }));
    }

    #[test]
    fn descriptor_aliasing_uses_typed_scalar_cbuf_view() {
        let mut program = ir::Program::new(ShaderStage::Fragment);
        program.info.used_constant_buffer_types = Type::F32 as u32;
        program
            .info
            .constant_buffer_descriptors
            .push(ConstantBufferDescriptor { index: 3, count: 1 });
        let profile = Profile {
            unified_descriptor_binding: true,
            support_descriptor_aliasing: true,
            ..Profile::default()
        };
        let mut ctx = SpirvEmitContext::new(&program, &profile, &RuntimeInfo::default());
        let mut bindings = Bindings::default();

        ctx.define_global_variables(&program, &mut bindings);

        let definitions = ctx.cbufs.get(&3).expect("CB3 must be declared");
        assert_ne!(definitions.f32_scalar, 0);
        assert_eq!(definitions.u32x4, 0);
        assert_eq!(bindings.unified, 1);
        assert!(ctx.builder.module_ref().annotations.iter().any(|inst| {
            inst.class.opcode == spirv::Op::Decorate
                && matches!(
                    inst.operands.as_slice(),
                    [
                        Operand::IdRef(_),
                        Operand::Decoration(spirv::Decoration::ArrayStride),
                        Operand::LiteralBit32(4)
                    ]
                )
        }));
    }

    #[test]
    fn demote_capability_and_extension_are_usage_gated() {
        let mut unused = ir::Program::new(ShaderStage::Fragment);
        let profile = Profile {
            supported_spirv: 0x0001_0500,
            support_demote_to_helper_invocation: true,
            ..Profile::default()
        };
        let runtime_info = RuntimeInfo::default();
        let unused_ctx = SpirvEmitContext::new(&unused, &profile, &runtime_info);
        assert!(!unused_ctx
            .builder
            .module_ref()
            .capabilities
            .iter()
            .any(|inst| {
                matches!(
                    inst.operands.as_slice(),
                    [Operand::Capability(
                        spirv::Capability::DemoteToHelperInvocation
                    )]
                )
            }));

        unused.info.uses_demote_to_helper_invocation = true;
        let used_ctx = SpirvEmitContext::new(&unused, &profile, &runtime_info);
        let module = used_ctx.builder.module_ref();
        assert!(module.capabilities.iter().any(|inst| {
            matches!(
                inst.operands.as_slice(),
                [Operand::Capability(
                    spirv::Capability::DemoteToHelperInvocation
                )]
            )
        }));
        assert!(module.extensions.iter().any(|inst| {
            matches!(inst.operands.as_slice(), [Operand::LiteralString(name)]
                if name == "SPV_EXT_demote_to_helper_invocation")
        }));
    }

    #[test]
    fn float_control_extension_and_signed_zero_mode_follow_profile() {
        let mut program = ir::Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());
        program.syntax_list = vec![ir::SyntaxNode::Block(0), ir::SyntaxNode::Return];
        let profile = Profile {
            supported_spirv: 0x0001_0600,
            support_float_controls: true,
            support_fp32_signed_zero_nan_preserve: true,
            ..Profile::default()
        };
        let runtime_info = RuntimeInfo::default();
        let mut ctx = SpirvEmitContext::new(&program, &profile, &runtime_info);
        ctx.emit_program(&program);
        let module = ctx.builder.module_ref();

        assert!(module.extensions.iter().any(|inst| {
            matches!(inst.operands.as_slice(), [Operand::LiteralString(name)]
                if name == "SPV_KHR_float_controls")
        }));
        assert!(module.capabilities.iter().any(|inst| {
            matches!(
                inst.operands.as_slice(),
                [Operand::Capability(
                    spirv::Capability::SignedZeroInfNanPreserve
                )]
            )
        }));
        assert!(module.execution_modes.iter().any(|inst| {
            matches!(
                inst.operands.as_slice(),
                [
                    Operand::IdRef(_),
                    Operand::ExecutionMode(spirv::ExecutionMode::SignedZeroInfNanPreserve),
                    Operand::LiteralBit32(32)
                ]
            )
        }));
    }

    #[test]
    fn vertex_id_declares_vertex_index_and_base_vertex_without_vertex_id_support() {
        let mut program = ir::Program::new(ShaderStage::VertexB);
        program
            .info
            .loads
            .set(Attribute::VERTEX_ID.0 as usize, true);

        let profile = Profile {
            support_vertex_instance_id: false,
            ..Profile::default()
        };
        let runtime_info = RuntimeInfo::default();
        let mut ctx = SpirvEmitContext::new(&program, &profile, &runtime_info);
        let mut bindings = Bindings::default();

        ctx.define_global_variables(&program, &mut bindings);

        assert_eq!(ctx.vertex_id, 0);
        assert_ne!(ctx.vertex_index, 0);
        assert_ne!(ctx.base_vertex, 0);
        assert!(ctx.interfaces.contains(&ctx.vertex_index));
        assert!(ctx.interfaces.contains(&ctx.base_vertex));
    }

    #[test]
    fn vertex_id_declares_vertex_id_when_supported() {
        let mut program = ir::Program::new(ShaderStage::VertexB);
        program
            .info
            .loads
            .set(Attribute::VERTEX_ID.0 as usize, true);

        let profile = Profile {
            support_vertex_instance_id: true,
            ..Profile::default()
        };
        let runtime_info = RuntimeInfo::default();
        let mut ctx = SpirvEmitContext::new(&program, &profile, &runtime_info);
        let mut bindings = Bindings::default();

        ctx.define_global_variables(&program, &mut bindings);

        assert_ne!(ctx.vertex_id, 0);
        assert_eq!(ctx.vertex_index, 0);
        assert_eq!(ctx.base_vertex, 0);
        assert!(ctx.interfaces.contains(&ctx.vertex_id));
    }

    #[test]
    fn fragment_position_load_declares_frag_coord_input() {
        let mut program = ir::Program::new(ShaderStage::Fragment);
        program
            .info
            .loads
            .set(Attribute::POSITION_W.0 as usize, true);

        let profile = Profile::default();
        let runtime_info = RuntimeInfo::default();
        let mut ctx = SpirvEmitContext::new(&program, &profile, &runtime_info);
        let mut bindings = Bindings::default();

        ctx.define_global_variables(&program, &mut bindings);

        assert_ne!(ctx.input_position, 0);
        assert!(ctx.interfaces.contains(&ctx.input_position));

        let input_position = ctx.input_position;
        let module = ctx.builder.module();
        let has_frag_coord = module.annotations.iter().any(|inst| {
            inst.class.opcode == spirv::Op::Decorate
                && matches!(inst.operands.as_slice(), [
                    Operand::IdRef(id),
                    Operand::Decoration(spirv::Decoration::BuiltIn),
                    Operand::BuiltIn(spirv::BuiltIn::FragCoord),
                ] if *id == input_position)
        });
        let has_position = module.annotations.iter().any(|inst| {
            inst.class.opcode == spirv::Op::Decorate
                && matches!(inst.operands.as_slice(), [
                    Operand::IdRef(id),
                    Operand::Decoration(spirv::Decoration::BuiltIn),
                    Operand::BuiltIn(spirv::BuiltIn::Position),
                ] if *id == input_position)
        });

        assert!(has_frag_coord);
        assert!(!has_position);
    }

    #[test]
    fn sampled_array_texture_preserves_upstream_image_type_flags() {
        let mut program = ir::Program::new(ShaderStage::Fragment);
        program.info.texture_descriptors.push(TextureDescriptor {
            texture_type: TextureType::ColorArray2D,
            is_depth: true,
            is_multisample: true,
            has_secondary: false,
            cbuf_index: 0,
            cbuf_offset: 0,
            shift_left: 0,
            secondary_cbuf_index: 0,
            secondary_cbuf_offset: 0,
            secondary_shift_left: 0,
            count: 1,
            size_shift: 0,
        });

        let profile = Profile::default();
        let runtime_info = RuntimeInfo::default();
        let mut ctx = SpirvEmitContext::new(&program, &profile, &runtime_info);
        let mut bindings = Bindings::default();
        ctx.define_global_variables(&program, &mut bindings);

        let image_type = ctx
            .builder
            .module_ref()
            .types_global_values
            .iter()
            .find(|inst| inst.class.opcode == spirv::Op::TypeImage)
            .expect("sampled texture must declare OpTypeImage");
        assert!(matches!(
            image_type.operands.as_slice(),
            [
                Operand::IdRef(_),
                Operand::Dim(spirv::Dim::Dim2D),
                Operand::LiteralBit32(1),
                Operand::LiteralBit32(1),
                Operand::LiteralBit32(1),
                Operand::LiteralBit32(1),
                Operand::ImageFormat(spirv::ImageFormat::Unknown),
            ]
        ));
    }

    #[test]
    fn phi_values_are_patched_after_forward_definitions() {
        let mut program = ir::Program::new(ShaderStage::VertexB);
        let phi_block = program.add_block();
        let value_block = program.add_block();

        let value_inst = program.block_mut(value_block).append_inst(Inst::new(
            Opcode::IAdd32,
            vec![Value::ImmU32(3), Value::ImmU32(4)],
        ));
        let mut phi = Inst::phi();
        phi.flags = Type::U32 as u32;
        phi.add_phi_operand(
            value_block,
            Value::Inst(InstRef {
                block: value_block,
                inst: value_inst,
            }),
        );
        let phi_inst = program.block_mut(phi_block).append_inst(phi);

        let profile = Profile::default();
        let runtime_info = RuntimeInfo::default();
        let mut ctx = SpirvEmitContext::new(&program, &profile, &runtime_info);
        ctx.emit_program(&program);

        let phi_id = ctx.values[&(phi_block, phi_inst)];
        let value_id = ctx.values[&(value_block, value_inst)];
        let emitted_phi = ctx
            .builder
            .module_ref()
            .functions
            .iter()
            .flat_map(|function| function.blocks.iter())
            .flat_map(|block| block.instructions.iter())
            .find(|inst| inst.result_id == Some(phi_id))
            .expect("Phi result was not emitted");

        assert_eq!(emitted_phi.class.opcode, spirv::Op::Phi);
        assert_eq!(emitted_phi.operands[0], Operand::IdRef(value_id));
        assert_ne!(emitted_phi.operands[0], Operand::IdRef(0));
    }
}
