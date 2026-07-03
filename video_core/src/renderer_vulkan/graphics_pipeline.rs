// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Graphics pipeline compilation helpers.
//!
//! Ref: zuyu `vk_graphics_pipeline.h` and `vk_pipeline_cache.h` — this file is
//! the reduced compilation leaf, while `pipeline_cache.rs` owns the matching
//! top-level pipeline-cache state and lookup flow.

use ash::vk;
use log::{debug, warn};
use std::collections::BTreeMap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::Instant;

use crate::engines::maxwell_3d::{DrawCall, ShaderStageType, VertexAttribSize, VertexAttribType};
use crate::shader;
use crate::shader_cache::{GraphicsEnvironments, NUM_PROGRAMS};
use crate::surface::{
    is_pixel_format_integer, is_pixel_format_signed_integer, pixel_format_from_render_target_format,
};
use shader_recompiler::backend::bindings::Bindings;
use shader_recompiler::host_translate_info::HostTranslateInfo;
use shader_recompiler::runtime_info::AttributeType;
use shader_recompiler::shader_info::{Info as ShaderInfo, TextureType};
use shader_recompiler::{CompiledShader, PipelineCache, Profile, RuntimeInfo, ShaderStage};

use super::descriptor_pool::DescriptorBankInfo;
use super::fixed_pipeline_state::FixedPipelineState;
use super::maxwell_to_vk;
use super::pipeline_helper::{
    RENDERAREA_LAYOUT_SIZE, RESCALING_LAYOUT_SIZE, RESCALING_LAYOUT_WORDS_OFFSET,
};

#[derive(Debug, Clone, Copy)]
pub struct GraphicsDescriptorBinding {
    pub binding: u32,
    pub descriptor_type: vk::DescriptorType,
    pub descriptor_count: u32,
    pub stage_flags: vk::ShaderStageFlags,
    pub uniform_stage: Option<u32>,
    pub uniform_index: Option<u32>,
    pub texture: Option<GraphicsTextureBinding>,
}

#[derive(Debug, Clone, Copy)]
pub struct GraphicsTextureBinding {
    pub texture_type: TextureType,
    pub graphics_stage: u32,
    pub cbuf_index: u32,
    pub cbuf_offset: u32,
    pub shift_left: u32,
    pub has_secondary: bool,
    pub secondary_cbuf_index: u32,
    pub secondary_cbuf_offset: u32,
    pub secondary_shift_left: u32,
    pub size_shift: u32,
}

type DescriptorBindingRecord = (
    vk::DescriptorType,
    u32,
    vk::ShaderStageFlags,
    Option<u32>,
    Option<u32>,
    Option<GraphicsTextureBinding>,
);

/// Cache key for graphics pipeline lookup.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GraphicsPipelineKey {
    /// Upstream shader unique hashes, indexed by Maxwell shader program slot.
    pub unique_hashes: [u64; NUM_PROGRAMS],
    /// Fixed (non-dynamic) pipeline state.
    pub fixed_state: FixedPipelineState,
}

impl Default for GraphicsPipelineKey {
    fn default() -> Self {
        Self {
            unique_hashes: [0; NUM_PROGRAMS],
            fixed_state: FixedPipelineState::default(),
        }
    }
}

/// A compiled Vulkan graphics pipeline.
pub struct GraphicsPipeline {
    device: ash::Device,
    key: GraphicsPipelineKey,
    transition_keys: Vec<GraphicsPipelineKey>,
    pub pipeline: vk::Pipeline,
    pub pipeline_layout: vk::PipelineLayout,
    pub descriptor_set_layout: vk::DescriptorSetLayout,
    pub descriptor_bindings: Vec<GraphicsDescriptorBinding>,
    pub descriptor_bank_info: DescriptorBankInfo,
    pub stage_infos: [Option<ShaderInfo>; 5],
    pub uses_render_area: bool,
    pub uses_rescaling_uniform: bool,
    pub vs_module: vk::ShaderModule,
    pub fs_module: vk::ShaderModule,
    is_built: AtomicBool,
}

impl Drop for GraphicsPipeline {
    fn drop(&mut self) {
        unsafe {
            self.device.destroy_pipeline(self.pipeline, None);
            self.device
                .destroy_pipeline_layout(self.pipeline_layout, None);
            self.device
                .destroy_descriptor_set_layout(self.descriptor_set_layout, None);
            self.device.destroy_shader_module(self.vs_module, None);
            self.device.destroy_shader_module(self.fs_module, None);
        }
    }
}

impl GraphicsPipeline {
    /// Port of `GraphicsPipeline::AddTransition`.
    pub fn add_transition(&mut self, transition_key: GraphicsPipelineKey) {
        self.transition_keys.push(transition_key);
    }

    /// Port of `GraphicsPipeline::Next`.
    pub fn next<'a>(
        &'a self,
        current_key: &GraphicsPipelineKey,
    ) -> Option<&'a GraphicsPipelineKey> {
        if &self.key == current_key {
            return Some(&self.key);
        }
        self.transition_keys.iter().find(|key| *key == current_key)
    }

    /// Port of `GraphicsPipeline::IsBuilt`.
    pub fn is_built(&self) -> bool {
        self.is_built.load(Ordering::Relaxed)
    }
}

/// Reduced graphics pipeline compilation leaf.
///
/// Ref: zuyu `CreateGraphicsPipeline(...)` helpers — `PipelineCache` owns the
/// lookup/cache state, while this file provides key construction and the
/// concrete compile/build path.
pub struct GraphicsPipelineCache {
    device: ash::Device,
    shader_cache: PipelineCache,
    profile: Profile,
    host_info: HostTranslateInfo,
    extended_dynamic_state_supported: bool,
    extended_dynamic_state2_supported: bool,
    topology_list_primitive_restart_supported: bool,
    patch_list_primitive_restart_supported: bool,
    max_viewports: u32,
}

impl GraphicsPipelineCache {
    pub fn new(
        device: ash::Device,
        shader_cache: PipelineCache,
        profile: Profile,
        extended_dynamic_state_supported: bool,
        extended_dynamic_state2_supported: bool,
        topology_list_primitive_restart_supported: bool,
        patch_list_primitive_restart_supported: bool,
        max_viewports: u32,
    ) -> Self {
        Self {
            device,
            shader_cache,
            profile,
            host_info: HostTranslateInfo::default(),
            extended_dynamic_state_supported,
            extended_dynamic_state2_supported,
            topology_list_primitive_restart_supported,
            patch_list_primitive_restart_supported,
            max_viewports: max_viewports.min(crate::engines::maxwell_3d::NUM_VIEWPORTS as u32),
        }
    }

    /// Build the current graphics pipeline cache key for a draw.
    pub fn make_key(
        &mut self,
        draw: &DrawCall,
        read_gpu: &dyn Fn(u64, &mut [u8]),
    ) -> Option<(GraphicsPipelineKey, FixedPipelineState)> {
        let mut fixed_state = FixedPipelineState::default();
        fixed_state.refresh(draw);
        fixed_state.set_extended_dynamic_state(self.extended_dynamic_state_supported);
        fixed_state.set_extended_dynamic_state_2(self.extended_dynamic_state2_supported);

        let vs_compiled = self.compile_vertex_shader(draw, &fixed_state, read_gpu)?;
        let fs_compiled = self.compile_fragment_shader(draw, &fixed_state, read_gpu);

        let vs_hash = hash_spirv(&vs_compiled.spirv_words);
        let fs_hash = fs_compiled
            .as_ref()
            .map(|f| hash_spirv(&f.spirv_words))
            .unwrap_or(0);

        let key = GraphicsPipelineKey {
            unique_hashes: [0, vs_hash, 0, 0, 0, fs_hash],
            fixed_state: fixed_state.clone(),
        };
        Some((key, fixed_state))
    }

    /// Build the current graphics pipeline key from the shared shader-cache
    /// hashes. This matches upstream's `PipelineCache::RefreshStages` owner
    /// boundary more closely than the legacy direct GPU read fallback.
    pub fn make_key_from_unique_hashes(
        &mut self,
        draw: &DrawCall,
        unique_hashes: [u64; NUM_PROGRAMS],
    ) -> (GraphicsPipelineKey, FixedPipelineState) {
        let mut fixed_state = FixedPipelineState::default();
        fixed_state.refresh(draw);
        fixed_state.set_extended_dynamic_state(self.extended_dynamic_state_supported);
        fixed_state.set_extended_dynamic_state_2(self.extended_dynamic_state2_supported);
        (
            GraphicsPipelineKey {
                unique_hashes,
                fixed_state: fixed_state.clone(),
            },
            fixed_state,
        )
    }

    /// Build a graphics pipeline for the given draw/key pair.
    ///
    /// Returns an owned pipeline object for insertion into the matching
    /// `PipelineCache` owner.
    pub fn build_pipeline_keyed(
        &mut self,
        draw: &DrawCall,
        render_pass: vk::RenderPass,
        read_gpu: &dyn Fn(u64, &mut [u8]),
        key: &GraphicsPipelineKey,
        fixed_state: &FixedPipelineState,
    ) -> Option<GraphicsPipeline> {
        let vs_compiled = self.compile_vertex_shader(draw, fixed_state, read_gpu)?;
        let fs_compiled = self.compile_fragment_shader(draw, fixed_state, read_gpu);

        // Create shader modules
        let vs_module = self.create_shader_module(&vs_compiled.spirv_words)?;
        let fs_module = if let Some(ref fs) = fs_compiled {
            self.create_shader_module(&fs.spirv_words)?
        } else {
            // Use VS module as FS fallback (will produce undefined output but won't crash)
            self.create_shader_module(&vs_compiled.spirv_words)?
        };

        // Create pipeline layout and pipeline
        let descriptor_layout = self.create_pipeline_layout(&vs_compiled, fs_compiled.as_ref())?;
        let pipeline = self.create_graphics_pipeline(
            vs_module,
            fs_module,
            descriptor_layout.pipeline_layout,
            render_pass,
            draw,
            fixed_state,
            &vs_compiled,
        )?;

        debug!(
            "GraphicsPipelineCache: compiled new pipeline (vs_hash=0x{:016X}, fs_hash=0x{:016X})",
            key.unique_hashes[1], key.unique_hashes[5]
        );

        Some(GraphicsPipeline {
            device: self.device.clone(),
            key: key.clone(),
            transition_keys: Vec::new(),
            pipeline,
            pipeline_layout: descriptor_layout.pipeline_layout,
            descriptor_set_layout: descriptor_layout.descriptor_set_layout,
            descriptor_bindings: descriptor_layout.bindings,
            descriptor_bank_info: descriptor_layout.bank_info,
            stage_infos: {
                let mut infos: [Option<ShaderInfo>; 5] = Default::default();
                infos[0] = Some(vs_compiled.info.clone());
                if let Some(fs) = fs_compiled.as_ref() {
                    infos[4] = Some(fs.info.clone());
                }
                infos
            },
            uses_render_area: vs_compiled.info.uses_render_area
                || fs_compiled
                    .as_ref()
                    .is_some_and(|shader| shader.info.uses_render_area),
            uses_rescaling_uniform: vs_compiled.info.uses_rescaling_uniform
                || fs_compiled
                    .as_ref()
                    .is_some_and(|shader| shader.info.uses_rescaling_uniform),
            vs_module,
            fs_module,
            is_built: AtomicBool::new(true),
        })
    }

    /// Build a Vulkan graphics pipeline from shared shader environments.
    pub fn build_pipeline_keyed_from_environments(
        &mut self,
        draw: &DrawCall,
        render_pass: vk::RenderPass,
        environments: &mut GraphicsEnvironments,
        key: &GraphicsPipelineKey,
        fixed_state: &FixedPipelineState,
    ) -> Option<GraphicsPipeline> {
        let mut bindings = Bindings::default();
        let vertex_stage = if environment_has_stage(environments, 1) {
            1
        } else if environment_has_stage(environments, 0) {
            0
        } else {
            return None;
        };
        let vertex_runtime_info = make_runtime_info(draw, fixed_state, ShaderStage::VertexB, None);
        let vs_compiled = self.compile_stage_from_environment(
            environments,
            vertex_stage,
            &mut bindings,
            &vertex_runtime_info,
        )?;
        let fs_compiled = if environment_has_stage(environments, 5) {
            let fragment_runtime_info = make_runtime_info(
                draw,
                fixed_state,
                ShaderStage::Fragment,
                Some(&vs_compiled.info),
            );
            self.compile_stage_from_environment(
                environments,
                5,
                &mut bindings,
                &fragment_runtime_info,
            )
        } else {
            None
        };

        let vs_module = self.create_shader_module(&vs_compiled.spirv_words)?;
        let fs_module = if let Some(ref fs) = fs_compiled {
            self.create_shader_module(&fs.spirv_words)?
        } else {
            self.create_shader_module(&vs_compiled.spirv_words)?
        };

        let descriptor_layout = self.create_pipeline_layout(&vs_compiled, fs_compiled.as_ref())?;
        let pipeline = self.create_graphics_pipeline(
            vs_module,
            fs_module,
            descriptor_layout.pipeline_layout,
            render_pass,
            draw,
            fixed_state,
            &vs_compiled,
        )?;

        debug!(
            "GraphicsPipelineCache: compiled env pipeline (vb_hash=0x{:016X}, fs_hash=0x{:016X})",
            key.unique_hashes[1], key.unique_hashes[5]
        );

        Some(GraphicsPipeline {
            device: self.device.clone(),
            key: key.clone(),
            transition_keys: Vec::new(),
            pipeline,
            pipeline_layout: descriptor_layout.pipeline_layout,
            descriptor_set_layout: descriptor_layout.descriptor_set_layout,
            descriptor_bindings: descriptor_layout.bindings,
            descriptor_bank_info: descriptor_layout.bank_info,
            stage_infos: {
                let mut infos: [Option<ShaderInfo>; 5] = Default::default();
                infos[0] = Some(vs_compiled.info.clone());
                if let Some(fs) = fs_compiled.as_ref() {
                    infos[4] = Some(fs.info.clone());
                }
                infos
            },
            uses_render_area: vs_compiled.info.uses_render_area
                || fs_compiled
                    .as_ref()
                    .is_some_and(|shader| shader.info.uses_render_area),
            uses_rescaling_uniform: vs_compiled.info.uses_rescaling_uniform
                || fs_compiled
                    .as_ref()
                    .is_some_and(|shader| shader.info.uses_rescaling_uniform),
            vs_module,
            fs_module,
            is_built: AtomicBool::new(true),
        })
    }

    fn compile_stage_from_environment(
        &self,
        environments: &mut GraphicsEnvironments,
        stage_index: usize,
        bindings: &mut Bindings,
        runtime_info: &RuntimeInfo,
    ) -> Option<CompiledShader> {
        if stage_index >= NUM_PROGRAMS {
            return None;
        }
        if !environment_has_stage(environments, stage_index) {
            return None;
        }
        let env = &mut environments.envs[stage_index];
        let trace = std::env::var_os("RUZU_TRACE_SHADER_WORDS").is_some();
        let start = Instant::now();
        if env.generic_environment().cached_code_slice().is_empty()
            && (!env.generic_environment().has_runtime_gpu_memory_owner()
                || env.generic_environment_mut().analyze().is_none())
        {
            if trace {
                eprintln!(
                    "[VK_SHADER_ENV_COMPILE] stage_index={} analyze_failed elapsed_ms={}",
                    stage_index,
                    start.elapsed().as_millis(),
                );
            }
            return None;
        }
        if trace {
            eprintln!(
                "[VK_SHADER_ENV_COMPILE] stage_index={} analyze_done elapsed_ms={} code_words={} instruction_start=0x{:X}",
                stage_index,
                start.elapsed().as_millis(),
                env.generic_environment().cached_code_slice().len(),
                env.generic_environment().cached_instruction_start(),
            );
        }
        let code = env
            .generic_environment()
            .cached_instruction_slice()
            .to_vec();
        if code.is_empty() {
            return None;
        }
        let base_offset = env.generic_environment().cached_instruction_start();
        let compile_start = Instant::now();
        let compiled = shader_recompiler::compile_shader_from_env_with_bindings_and_host_info(
            &code,
            base_offset,
            env,
            &self.profile,
            runtime_info,
            bindings,
            &self.host_info,
        );
        if trace {
            eprintln!(
                "[VK_SHADER_ENV_COMPILE] stage_index={} compile_done elapsed_ms={} spirv_words={} cbufs={} textures={}",
                stage_index,
                compile_start.elapsed().as_millis(),
                compiled.spirv_words.len(),
                compiled.info.constant_buffer_descriptors.len(),
                compiled.info.texture_descriptors.len(),
            );
        }
        dump_spirv_if_requested(stage_index, base_offset, &compiled.spirv_words);
        Some(compiled)
    }

    fn compile_vertex_shader(
        &mut self,
        draw: &DrawCall,
        fixed_state: &FixedPipelineState,
        read_gpu: &dyn Fn(u64, &mut [u8]),
    ) -> Option<CompiledShader> {
        let stage_info = &draw.shader_stages[1]; // VertexB = index 1
        if !stage_info.enabled || stage_info.program_type != ShaderStageType::VertexB {
            return None;
        }

        let shader_addr = draw.program_base_address + stage_info.offset as u64;
        let (code, _header) = shader::load_shader_code(shader_addr, read_gpu, 4096);
        if code.is_empty() {
            return None;
        }

        let runtime_info = make_runtime_info(draw, fixed_state, ShaderStage::VertexB, None);
        let compiled = self
            .shader_cache
            .get_or_compile(&code, ShaderStage::VertexB, &runtime_info);
        Some(compiled.clone())
    }

    fn compile_fragment_shader(
        &mut self,
        draw: &DrawCall,
        fixed_state: &FixedPipelineState,
        read_gpu: &dyn Fn(u64, &mut [u8]),
    ) -> Option<CompiledShader> {
        let stage_info = &draw.shader_stages[5]; // Fragment = index 5
        if !stage_info.enabled || stage_info.program_type != ShaderStageType::Fragment {
            return None;
        }

        let shader_addr = draw.program_base_address + stage_info.offset as u64;
        let (code, _header) = shader::load_shader_code(shader_addr, read_gpu, 4096);
        if code.is_empty() {
            return None;
        }

        let runtime_info = make_runtime_info(draw, fixed_state, ShaderStage::Fragment, None);
        let compiled =
            self.shader_cache
                .get_or_compile(&code, ShaderStage::Fragment, &runtime_info);
        Some(compiled.clone())
    }

    fn create_shader_module(&self, spirv_words: &[u32]) -> Option<vk::ShaderModule> {
        let create_info = vk::ShaderModuleCreateInfo::builder()
            .code(spirv_words)
            .build();
        match unsafe { self.device.create_shader_module(&create_info, None) } {
            Ok(m) => Some(m),
            Err(e) => {
                warn!(
                    "GraphicsPipelineCache: failed to create shader module: {:?}",
                    e
                );
                None
            }
        }
    }

    fn create_pipeline_layout(
        &self,
        vs: &CompiledShader,
        fs: Option<&CompiledShader>,
    ) -> Option<GraphicsDescriptorLayout> {
        let mut bindings = BTreeMap::new();
        let mut binding = 0u32;
        add_shader_descriptor_bindings(
            &mut bindings,
            vs,
            vk::ShaderStageFlags::VERTEX,
            0,
            &mut binding,
        );
        if let Some(fs) = fs {
            add_shader_descriptor_bindings(
                &mut bindings,
                fs,
                vk::ShaderStageFlags::FRAGMENT,
                4,
                &mut binding,
            );
        }
        let descriptor_bindings: Vec<_> = bindings
            .into_iter()
            .map(
                |(
                    binding,
                    (
                        descriptor_type,
                        descriptor_count,
                        stage_flags,
                        uniform_stage,
                        uniform_index,
                        texture,
                    ),
                )| {
                    GraphicsDescriptorBinding {
                        binding,
                        descriptor_type,
                        descriptor_count,
                        stage_flags,
                        uniform_stage,
                        uniform_index,
                        texture,
                    }
                },
            )
            .collect();
        let bank_info = descriptor_bank_info(&descriptor_bindings);
        let layout_bindings: Vec<_> = descriptor_bindings
            .iter()
            .map(|binding| {
                vk::DescriptorSetLayoutBinding::builder()
                    .binding(binding.binding)
                    .descriptor_type(binding.descriptor_type)
                    .descriptor_count(binding.descriptor_count)
                    .stage_flags(binding.stage_flags)
                    .build()
            })
            .collect();
        let layout_info = vk::DescriptorSetLayoutCreateInfo::builder()
            .bindings(&layout_bindings)
            .build();
        let desc_layout = unsafe {
            self.device
                .create_descriptor_set_layout(&layout_info, None)
                .ok()?
        };

        let push_constant_range = vk::PushConstantRange {
            stage_flags: vk::ShaderStageFlags::ALL_GRAPHICS,
            offset: RESCALING_LAYOUT_WORDS_OFFSET,
            size: RESCALING_LAYOUT_SIZE + RENDERAREA_LAYOUT_SIZE,
        };
        let layouts = [desc_layout];
        let pipeline_layout_info = vk::PipelineLayoutCreateInfo::builder()
            .set_layouts(&layouts)
            .push_constant_ranges(std::slice::from_ref(&push_constant_range))
            .build();
        let pipeline_layout = unsafe {
            self.device
                .create_pipeline_layout(&pipeline_layout_info, None)
                .ok()?
        };

        Some(GraphicsDescriptorLayout {
            pipeline_layout,
            descriptor_set_layout: desc_layout,
            bindings: descriptor_bindings,
            bank_info,
        })
    }

    fn create_graphics_pipeline(
        &self,
        vs_module: vk::ShaderModule,
        fs_module: vk::ShaderModule,
        pipeline_layout: vk::PipelineLayout,
        render_pass: vk::RenderPass,
        draw: &DrawCall,
        fixed_state: &FixedPipelineState,
        vs: &CompiledShader,
    ) -> Option<vk::Pipeline> {
        let entry_name = std::ffi::CString::new("main").unwrap();

        let shader_stages = [
            vk::PipelineShaderStageCreateInfo::builder()
                .stage(vk::ShaderStageFlags::VERTEX)
                .module(vs_module)
                .name(&entry_name)
                .build(),
            vk::PipelineShaderStageCreateInfo::builder()
                .stage(vk::ShaderStageFlags::FRAGMENT)
                .module(fs_module)
                .name(&entry_name)
                .build(),
        ];

        let (vertex_bindings, vertex_attributes) = build_vertex_input_state(draw, vs);
        let vertex_input = vk::PipelineVertexInputStateCreateInfo::builder()
            .vertex_binding_descriptions(&vertex_bindings)
            .vertex_attribute_descriptions(&vertex_attributes)
            .build();

        let input_assembly_topology = super::map_topology(draw.topology);
        let primitive_restart_enable = primitive_restart_enable_for_pipeline(
            fixed_state.dynamic_state.primitive_restart_enable(),
            input_assembly_topology,
            self.topology_list_primitive_restart_supported,
            self.patch_list_primitive_restart_supported,
        );
        let input_assembly = vk::PipelineInputAssemblyStateCreateInfo::builder()
            .topology(input_assembly_topology)
            .primitive_restart_enable(primitive_restart_enable)
            .build();

        let num_viewports = self.max_viewports.max(1);
        let viewport_state = vk::PipelineViewportStateCreateInfo::builder()
            .viewport_count(num_viewports)
            .scissor_count(num_viewports)
            .build();

        let rasterization = vk::PipelineRasterizationStateCreateInfo::builder()
            .depth_clamp_enable(false)
            .rasterizer_discard_enable(false)
            .polygon_mode(vk::PolygonMode::FILL)
            .cull_mode(super::map_cull_mode(&draw.rasterizer))
            .front_face(super::map_front_face(draw.rasterizer.front_face))
            .depth_bias_enable(draw.rasterizer.depth_bias != 0.0)
            .depth_bias_constant_factor(draw.rasterizer.depth_bias)
            .depth_bias_slope_factor(draw.rasterizer.slope_scale_depth_bias)
            .depth_bias_clamp(draw.rasterizer.depth_bias_clamp)
            .line_width(draw.rasterizer.line_width_smooth.max(1.0))
            .build();

        let multisample = vk::PipelineMultisampleStateCreateInfo::builder()
            .rasterization_samples(vk::SampleCountFlags::TYPE_1)
            .sample_shading_enable(false)
            .build();

        let depth_stencil = vk::PipelineDepthStencilStateCreateInfo::builder()
            .depth_test_enable(draw.depth_stencil.depth_test_enable)
            .depth_write_enable(draw.depth_stencil.depth_write_enable)
            .depth_compare_op(super::map_compare_op(draw.depth_stencil.depth_func))
            .depth_bounds_test_enable(false)
            .stencil_test_enable(draw.depth_stencil.stencil_enable)
            .build();

        let num_attachments = draw.rt_control.count.clamp(1, 8) as usize;
        let blend_attachments = (0..num_attachments)
            .map(|index| {
                let blend = draw.blend[index];
                let mask = draw.color_masks[index];
                let mut write_mask = vk::ColorComponentFlags::empty();
                if mask.r {
                    write_mask |= vk::ColorComponentFlags::R;
                }
                if mask.g {
                    write_mask |= vk::ColorComponentFlags::G;
                }
                if mask.b {
                    write_mask |= vk::ColorComponentFlags::B;
                }
                if mask.a {
                    write_mask |= vk::ColorComponentFlags::A;
                }
                vk::PipelineColorBlendAttachmentState::builder()
                    .blend_enable(blend.enabled)
                    .src_color_blend_factor(super::map_blend_factor(blend.color_src))
                    .dst_color_blend_factor(super::map_blend_factor(blend.color_dst))
                    .color_blend_op(super::map_blend_equation(blend.color_op))
                    .src_alpha_blend_factor(super::map_blend_factor(blend.alpha_src))
                    .dst_alpha_blend_factor(super::map_blend_factor(blend.alpha_dst))
                    .alpha_blend_op(super::map_blend_equation(blend.alpha_op))
                    .color_write_mask(write_mask)
                    .build()
            })
            .collect::<Vec<_>>();

        let color_blend = vk::PipelineColorBlendStateCreateInfo::builder()
            .logic_op_enable(draw.logic_op.enabled)
            .logic_op(vk::LogicOp::from_raw(draw.logic_op.op as i32))
            .attachments(&blend_attachments)
            .blend_constants([
                draw.blend_color.r,
                draw.blend_color.g,
                draw.blend_color.b,
                draw.blend_color.a,
            ])
            .build();

        let mut dynamic_states = vec![
            vk::DynamicState::VIEWPORT,
            vk::DynamicState::SCISSOR,
            vk::DynamicState::DEPTH_BIAS,
            vk::DynamicState::BLEND_CONSTANTS,
            vk::DynamicState::DEPTH_BOUNDS,
            vk::DynamicState::STENCIL_COMPARE_MASK,
            vk::DynamicState::STENCIL_WRITE_MASK,
            vk::DynamicState::STENCIL_REFERENCE,
            vk::DynamicState::LINE_WIDTH,
        ];
        if fixed_state.extended_dynamic_state() {
            dynamic_states.extend([
                vk::DynamicState::CULL_MODE,
                vk::DynamicState::FRONT_FACE,
                vk::DynamicState::DEPTH_TEST_ENABLE,
                vk::DynamicState::DEPTH_WRITE_ENABLE,
                vk::DynamicState::DEPTH_COMPARE_OP,
                vk::DynamicState::DEPTH_BOUNDS_TEST_ENABLE,
                vk::DynamicState::STENCIL_TEST_ENABLE,
                vk::DynamicState::STENCIL_OP,
            ]);
        }
        if fixed_state.extended_dynamic_state_2() {
            dynamic_states.extend([
                vk::DynamicState::DEPTH_BIAS_ENABLE,
                vk::DynamicState::PRIMITIVE_RESTART_ENABLE,
                vk::DynamicState::RASTERIZER_DISCARD_ENABLE,
            ]);
        }
        let dynamic_state = vk::PipelineDynamicStateCreateInfo::builder()
            .dynamic_states(&dynamic_states)
            .build();

        let pipeline_info = vk::GraphicsPipelineCreateInfo::builder()
            .stages(&shader_stages)
            .vertex_input_state(&vertex_input)
            .input_assembly_state(&input_assembly)
            .viewport_state(&viewport_state)
            .rasterization_state(&rasterization)
            .multisample_state(&multisample)
            .depth_stencil_state(&depth_stencil)
            .color_blend_state(&color_blend)
            .dynamic_state(&dynamic_state)
            .layout(pipeline_layout)
            .render_pass(render_pass)
            .subpass(0)
            .build();

        match unsafe {
            self.device
                .create_graphics_pipelines(vk::PipelineCache::null(), &[pipeline_info], None)
        } {
            Ok(pipelines) => Some(pipelines[0]),
            Err((_, e)) => {
                warn!("GraphicsPipelineCache: pipeline creation failed: {:?}", e);
                None
            }
        }
    }
}

fn environment_has_stage(environments: &GraphicsEnvironments, stage_index: usize) -> bool {
    environments
        .env_ptrs
        .iter()
        .flatten()
        .any(|&index| index == stage_index)
}

/// Port of `SupportsPrimitiveRestart` in upstream `vk_graphics_pipeline.cpp`.
fn supports_primitive_restart(topology: vk::PrimitiveTopology) -> bool {
    !matches!(
        topology,
        vk::PrimitiveTopology::POINT_LIST
            | vk::PrimitiveTopology::LINE_LIST
            | vk::PrimitiveTopology::TRIANGLE_LIST
            | vk::PrimitiveTopology::LINE_LIST_WITH_ADJACENCY
            | vk::PrimitiveTopology::TRIANGLE_LIST_WITH_ADJACENCY
            | vk::PrimitiveTopology::PATCH_LIST
    )
}

fn primitive_restart_supported_for_topology(
    topology: vk::PrimitiveTopology,
    topology_list_primitive_restart_supported: bool,
    patch_list_primitive_restart_supported: bool,
) -> bool {
    (topology != vk::PrimitiveTopology::PATCH_LIST && topology_list_primitive_restart_supported)
        || supports_primitive_restart(topology)
        || (topology == vk::PrimitiveTopology::PATCH_LIST && patch_list_primitive_restart_supported)
}

/// Port of `input_assembly_ci.primitiveRestartEnable` selection in
/// `vk_graphics_pipeline.cpp`. Upstream does not force this to false when
/// `VK_EXT_extended_dynamic_state2` is active; the dynamic state is added
/// separately to the pipeline dynamic-state list.
fn primitive_restart_enable_for_pipeline(
    dynamic_primitive_restart_enable: bool,
    topology: vk::PrimitiveTopology,
    topology_list_primitive_restart_supported: bool,
    patch_list_primitive_restart_supported: bool,
) -> bool {
    dynamic_primitive_restart_enable
        && primitive_restart_supported_for_topology(
            topology,
            topology_list_primitive_restart_supported,
            patch_list_primitive_restart_supported,
        )
}

fn add_shader_descriptor_bindings(
    bindings: &mut BTreeMap<u32, DescriptorBindingRecord>,
    shader: &CompiledShader,
    stage: vk::ShaderStageFlags,
    graphics_stage: u32,
    binding: &mut u32,
) {
    for desc in &shader.info.constant_buffer_descriptors {
        merge_descriptor_binding(
            bindings,
            *binding,
            vk::DescriptorType::UNIFORM_BUFFER,
            desc.count,
            stage,
            Some(graphics_stage),
            Some(desc.index),
            None,
        );
        *binding += 1;
    }
    for desc in &shader.info.texture_descriptors {
        merge_descriptor_binding(
            bindings,
            *binding,
            vk::DescriptorType::COMBINED_IMAGE_SAMPLER,
            desc.count,
            stage,
            None,
            None,
            Some(GraphicsTextureBinding {
                texture_type: desc.texture_type,
                graphics_stage,
                cbuf_index: desc.cbuf_index,
                cbuf_offset: desc.cbuf_offset,
                shift_left: desc.shift_left,
                has_secondary: desc.has_secondary,
                secondary_cbuf_index: desc.secondary_cbuf_index,
                secondary_cbuf_offset: desc.secondary_cbuf_offset,
                secondary_shift_left: desc.secondary_shift_left,
                size_shift: desc.size_shift,
            }),
        );
        *binding += 1;
    }
}

fn merge_descriptor_binding(
    bindings: &mut BTreeMap<u32, DescriptorBindingRecord>,
    binding: u32,
    descriptor_type: vk::DescriptorType,
    descriptor_count: u32,
    stage: vk::ShaderStageFlags,
    uniform_stage: Option<u32>,
    uniform_index: Option<u32>,
    texture: Option<GraphicsTextureBinding>,
) {
    bindings
        .entry(binding)
        .and_modify(
            |(
                existing_type,
                existing_count,
                existing_stages,
                existing_uniform_stage,
                existing_uniform_index,
                existing_texture,
            )| {
                if *existing_type != descriptor_type {
                    warn!(
                        "GraphicsPipelineCache: descriptor binding {} type conflict {:?} vs {:?}",
                        binding, *existing_type, descriptor_type
                    );
                }
                if *existing_count != descriptor_count {
                    warn!(
                        "GraphicsPipelineCache: descriptor binding {} count conflict {} vs {}",
                        binding, *existing_count, descriptor_count
                    );
                    *existing_count = (*existing_count).max(descriptor_count);
                }
                *existing_stages |= stage;
                if existing_uniform_stage.is_none() {
                    *existing_uniform_stage = uniform_stage;
                }
                if existing_uniform_index.is_none() {
                    *existing_uniform_index = uniform_index;
                }
                if existing_texture.is_none() {
                    *existing_texture = texture;
                }
            },
        )
        .or_insert((
            descriptor_type,
            descriptor_count,
            stage,
            uniform_stage,
            uniform_index,
            texture,
        ));
}

struct GraphicsDescriptorLayout {
    pipeline_layout: vk::PipelineLayout,
    descriptor_set_layout: vk::DescriptorSetLayout,
    bindings: Vec<GraphicsDescriptorBinding>,
    bank_info: DescriptorBankInfo,
}

fn descriptor_bank_info(bindings: &[GraphicsDescriptorBinding]) -> DescriptorBankInfo {
    let mut info = DescriptorBankInfo::default();
    for binding in bindings {
        match binding.descriptor_type {
            vk::DescriptorType::UNIFORM_BUFFER => info.uniform_buffers += binding.descriptor_count,
            vk::DescriptorType::STORAGE_BUFFER => info.storage_buffers += binding.descriptor_count,
            vk::DescriptorType::UNIFORM_TEXEL_BUFFER => {
                info.texture_buffers += binding.descriptor_count
            }
            vk::DescriptorType::STORAGE_TEXEL_BUFFER => {
                info.image_buffers += binding.descriptor_count
            }
            vk::DescriptorType::COMBINED_IMAGE_SAMPLER => info.textures += binding.descriptor_count,
            vk::DescriptorType::STORAGE_IMAGE => info.images += binding.descriptor_count,
            _ => {}
        }
        info.score += binding.descriptor_count as i32;
    }
    info
}

/// Port-facing subset of upstream `MakeRuntimeInfo`.
fn make_runtime_info(
    draw: &DrawCall,
    fixed_state: &FixedPipelineState,
    stage: ShaderStage,
    previous_program: Option<&ShaderInfo>,
) -> RuntimeInfo {
    let mut info = RuntimeInfo::default();
    if let Some(previous_program) = previous_program {
        info.previous_stage_stores = previous_program.stores.clone();
        info.previous_stage_legacy_stores_mapping = previous_program.legacy_stores_mapping.clone();
    } else {
        info.previous_stage_stores.mask.fill(u64::MAX);
    }
    if stage == ShaderStage::VertexB {
        info.convert_depth_mode = fixed_state.ndc_minus_one_to_one();
        for (index, attrib) in draw.vertex_attribs.iter().take(32).enumerate() {
            info.generic_input_types[index] = cast_attribute_type(attrib);
        }
    } else if stage == ShaderStage::Fragment {
        for (index, rt) in draw.render_targets.iter().take(8).enumerate() {
            if rt.format == 0 {
                info.frag_color_types[index] = AttributeType::Float;
                continue;
            }
            let pixel_format = pixel_format_from_render_target_format(rt.format);
            info.frag_color_types[index] = if is_pixel_format_signed_integer(pixel_format) {
                AttributeType::SignedInt
            } else if is_pixel_format_integer(pixel_format) {
                AttributeType::UnsignedInt
            } else {
                AttributeType::Float
            };
        }
    }
    info
}

/// Port of `CastAttributeType` in `vk_pipeline_cache.cpp`.
fn cast_attribute_type(attrib: &crate::engines::maxwell_3d::VertexAttribInfo) -> AttributeType {
    if attrib.constant {
        return AttributeType::Disabled;
    }
    match attrib.attrib_type {
        VertexAttribType::Invalid => AttributeType::Disabled,
        VertexAttribType::SNorm | VertexAttribType::UNorm | VertexAttribType::Float => {
            AttributeType::Float
        }
        VertexAttribType::SInt => AttributeType::SignedInt,
        VertexAttribType::UInt => AttributeType::UnsignedInt,
        VertexAttribType::UScaled => AttributeType::UnsignedScaled,
        VertexAttribType::SScaled => AttributeType::SignedScaled,
    }
}

fn dump_spirv_if_requested(stage_index: usize, base_offset: u32, words: &[u32]) {
    let Some(dir) = std::env::var_os("RUZU_DUMP_SPIRV_DIR") else {
        return;
    };
    let dir = std::path::PathBuf::from(dir);
    if let Err(err) = std::fs::create_dir_all(&dir) {
        warn!(
            "GraphicsPipelineCache: failed to create SPIR-V dump dir {}: {}",
            dir.display(),
            err
        );
        return;
    }
    let path = dir.join(format!(
        "stage{}_base_{:06X}_{:016X}.spv",
        stage_index,
        base_offset,
        hash_spirv(words)
    ));
    let mut bytes = Vec::with_capacity(words.len() * std::mem::size_of::<u32>());
    for word in words {
        bytes.extend_from_slice(&word.to_le_bytes());
    }
    if let Err(err) = std::fs::write(&path, bytes) {
        warn!(
            "GraphicsPipelineCache: failed to write SPIR-V dump {}: {}",
            path.display(),
            err
        );
    }
}

fn build_vertex_input_state(
    draw: &DrawCall,
    vs: &CompiledShader,
) -> (
    Vec<vk::VertexInputBindingDescription>,
    Vec<vk::VertexInputAttributeDescription>,
) {
    let mut binding_descs = BTreeMap::<u32, vk::VertexInputBindingDescription>::new();
    for stream in &draw.vertex_streams {
        if !stream.enabled {
            continue;
        }
        let input_rate = if stream.frequency != 0 {
            vk::VertexInputRate::INSTANCE
        } else {
            vk::VertexInputRate::VERTEX
        };
        binding_descs.insert(
            stream.index,
            vk::VertexInputBindingDescription {
                binding: stream.index,
                stride: stream.stride,
                input_rate,
            },
        );
    }

    let mut attributes = Vec::new();
    for (location, attrib) in draw.vertex_attribs.iter().enumerate() {
        if attrib.constant
            || attrib.size == VertexAttribSize::Invalid
            || attrib.attrib_type == VertexAttribType::Invalid
            || !vs.info.loads.generic_any(location)
        {
            continue;
        }
        let format = maxwell_to_vk::vertex_format(false, attrib.attrib_type, attrib.size);
        if format == vk::Format::UNDEFINED {
            warn!(
                "GraphicsPipelineCache: unsupported vertex format location={} type={:?} size={:?}",
                location, attrib.attrib_type, attrib.size
            );
            continue;
        }
        if std::env::var_os("RUZU_TRACE_VK_VERTEX_INPUT").is_some() {
            log::info!(
                "[VK_VERTEX_INPUT] attr location={} binding={} offset={} type={:?} size={:?} format={:?}",
                location,
                attrib.buffer_index,
                attrib.offset,
                attrib.attrib_type,
                attrib.size,
                format
            );
        }
        binding_descs.entry(attrib.buffer_index).or_insert_with(|| {
            vk::VertexInputBindingDescription {
                binding: attrib.buffer_index,
                stride: attrib.size.size_bytes(),
                input_rate: vk::VertexInputRate::VERTEX,
            }
        });
        attributes.push(vk::VertexInputAttributeDescription {
            location: location as u32,
            binding: attrib.buffer_index,
            format,
            offset: attrib.offset,
        });
    }

    (binding_descs.into_values().collect(), attributes)
}

fn hash_spirv(words: &[u32]) -> u64 {
    // FNV-1a hash
    let mut hash: u64 = 0xcbf29ce484222325;
    for word in words {
        let bytes = word.to_le_bytes();
        for &b in &bytes {
            hash ^= b as u64;
            hash = hash.wrapping_mul(0x100000001b3);
        }
    }
    hash
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem::ManuallyDrop;

    unsafe extern "system" fn dummy_vk_function() {}

    fn make_test_pipeline(key: GraphicsPipelineKey) -> ManuallyDrop<GraphicsPipeline> {
        // Test-only placeholder. These methods do not touch the Vulkan device,
        // and `ManuallyDrop` avoids running the real destruction path.
        let instance_fn =
            vk::InstanceFnV1_0::load(|_| dummy_vk_function as *const () as *const std::ffi::c_void);
        let device = unsafe { ash::Device::load(&instance_fn, vk::Device::null()) };
        ManuallyDrop::new(GraphicsPipeline {
            device,
            key,
            transition_keys: Vec::new(),
            pipeline: vk::Pipeline::null(),
            pipeline_layout: vk::PipelineLayout::null(),
            descriptor_set_layout: vk::DescriptorSetLayout::null(),
            descriptor_bindings: Vec::new(),
            descriptor_bank_info: DescriptorBankInfo::default(),
            stage_infos: Default::default(),
            uses_render_area: false,
            uses_rescaling_uniform: false,
            vs_module: vk::ShaderModule::null(),
            fs_module: vk::ShaderModule::null(),
            is_built: AtomicBool::new(true),
        })
    }

    #[test]
    fn test_hash_spirv_deterministic() {
        let words = vec![0x07230203u32, 0x00010000, 0x000d000a];
        assert_eq!(hash_spirv(&words), hash_spirv(&words));
    }

    #[test]
    fn test_hash_spirv_different_for_different_input() {
        let a = vec![0x07230203u32, 0x00010000];
        let b = vec![0x07230203u32, 0x00020000];
        assert_ne!(hash_spirv(&a), hash_spirv(&b));
    }

    #[test]
    fn test_pipeline_key_equality() {
        let key_a = GraphicsPipelineKey {
            unique_hashes: [0, 123, 0, 0, 0, 456],
            fixed_state: FixedPipelineState::default(),
        };
        let key_b = GraphicsPipelineKey {
            unique_hashes: [0, 123, 0, 0, 0, 456],
            fixed_state: FixedPipelineState::default(),
        };
        assert_eq!(key_a, key_b);
    }

    #[test]
    fn test_transition_graph_returns_self_and_added_transition() {
        let key_a = GraphicsPipelineKey {
            unique_hashes: [0, 123, 0, 0, 0, 456],
            fixed_state: FixedPipelineState::default(),
        };
        let key_b = GraphicsPipelineKey {
            unique_hashes: [0, 789, 0, 0, 0, 321],
            fixed_state: FixedPipelineState::default(),
        };

        let mut pipeline = make_test_pipeline(key_a.clone());
        pipeline.add_transition(key_b.clone());

        assert_eq!(pipeline.next(&key_a), Some(&key_a));
        assert_eq!(pipeline.next(&key_b), Some(&key_b));
        assert!(pipeline.is_built());
    }

    #[test]
    fn primitive_restart_topology_gate_matches_upstream() {
        assert!(primitive_restart_supported_for_topology(
            vk::PrimitiveTopology::TRIANGLE_STRIP,
            false,
            false,
        ));
        assert!(!primitive_restart_supported_for_topology(
            vk::PrimitiveTopology::TRIANGLE_LIST,
            false,
            false,
        ));
        assert!(primitive_restart_supported_for_topology(
            vk::PrimitiveTopology::TRIANGLE_LIST,
            true,
            false,
        ));
        assert!(!primitive_restart_supported_for_topology(
            vk::PrimitiveTopology::PATCH_LIST,
            true,
            false,
        ));
        assert!(primitive_restart_supported_for_topology(
            vk::PrimitiveTopology::PATCH_LIST,
            false,
            true,
        ));
    }

    #[test]
    fn primitive_restart_pipeline_state_matches_upstream_dynamic_state2_behavior() {
        assert!(primitive_restart_enable_for_pipeline(
            true,
            vk::PrimitiveTopology::TRIANGLE_STRIP,
            false,
            false,
        ));
        assert!(primitive_restart_enable_for_pipeline(
            true,
            vk::PrimitiveTopology::TRIANGLE_LIST,
            true,
            false,
        ));
        assert!(!primitive_restart_enable_for_pipeline(
            false,
            vk::PrimitiveTopology::TRIANGLE_STRIP,
            true,
            true,
        ));
    }

    #[test]
    fn runtime_info_convert_depth_mode_tracks_fixed_pipeline_ndc_mode() {
        let draw = DrawCall {
            topology: crate::engines::maxwell_3d::PrimitiveTopology::Triangles,
            vertex_first: 0,
            vertex_count: 0,
            indexed: false,
            index_buffer_addr: 0,
            index_buffer_count: 0,
            index_buffer_first: 0,
            index_format: crate::engines::maxwell_3d::IndexFormat::UnsignedShort,
            vertex_streams: Vec::new(),
            vertex_stream_limits: Default::default(),
            viewports: [crate::engines::maxwell_3d::ViewportInfo::default();
                crate::engines::maxwell_3d::NUM_VIEWPORTS],
            viewport_transforms: Default::default(),
            scissors: [crate::engines::maxwell_3d::ScissorInfo::default();
                crate::engines::maxwell_3d::NUM_VIEWPORTS],
            viewport_scale_offset_enabled: false,
            window_origin_lower_left: false,
            window_origin_flip_y: false,
            surface_clip: Default::default(),
            blend: [crate::engines::maxwell_3d::BlendInfo::default(); 8],
            blend_color: Default::default(),
            depth_stencil: Default::default(),
            rasterizer: Default::default(),
            rasterize_enable: true,
            primitive_restart: Default::default(),
            logic_op: Default::default(),
            depth_clamp_enabled: false,
            program_base_address: 0,
            cb_bindings: Default::default(),
            vertex_attribs: Vec::new(),
            shader_stages: Default::default(),
            color_masks: Default::default(),
            rt_control: Default::default(),
            tex_header_pool_addr: 0,
            tex_header_pool_limit: 0,
            tex_sampler_pool_addr: 0,
            tex_sampler_pool_limit: 0,
            instance_count: 1,
            base_instance: 0,
            base_vertex: 0,
            inline_index_data: Vec::new(),
            sampler_binding: crate::engines::maxwell_3d::SamplerBinding::Independently,
            render_targets: Default::default(),
            zeta: Default::default(),
            dirty_flags: [false; 256],
        };
        let mut fixed_state = FixedPipelineState::default();
        fixed_state.set_ndc_minus_one_to_one(true);
        assert!(
            make_runtime_info(&draw, &fixed_state, ShaderStage::VertexB, None).convert_depth_mode
        );

        fixed_state.set_ndc_minus_one_to_one(false);
        assert!(
            !make_runtime_info(&draw, &fixed_state, ShaderStage::VertexB, None).convert_depth_mode
        );
    }
}
