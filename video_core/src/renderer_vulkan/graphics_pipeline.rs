// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Graphics pipeline compilation helpers.
//!
//! Ref: zuyu `vk_graphics_pipeline.h` and `vk_pipeline_cache.h` — this file is
//! the reduced compilation leaf, while `pipeline_cache.rs` owns the matching
//! top-level pipeline-cache state and lookup flow.

use ash::vk;
use log::{debug, warn};
use std::sync::atomic::{AtomicBool, Ordering};

use crate::engines::maxwell_3d::{DrawCall, ShaderStageType};
use crate::shader;
use shader_recompiler::{CompiledShader, PipelineCache, RuntimeInfo, ShaderStage};

use super::fixed_pipeline_state::FixedPipelineState;

/// Cache key for graphics pipeline lookup.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GraphicsPipelineKey {
    /// Shader hashes: [VS, FS] (expand for GS/TCS/TES later).
    pub shader_hashes: [u64; 2],
    /// Fixed (non-dynamic) pipeline state.
    pub fixed_state: FixedPipelineState,
}

/// A compiled Vulkan graphics pipeline.
pub struct GraphicsPipeline {
    device: ash::Device,
    key: GraphicsPipelineKey,
    transition_keys: Vec<GraphicsPipelineKey>,
    pub pipeline: vk::Pipeline,
    pub pipeline_layout: vk::PipelineLayout,
    pub descriptor_set_layout: vk::DescriptorSetLayout,
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
}

impl GraphicsPipelineCache {
    pub fn new(device: ash::Device, shader_cache: PipelineCache) -> Self {
        Self {
            device,
            shader_cache,
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

        let vs_compiled = self.compile_vertex_shader(draw, read_gpu)?;
        let fs_compiled = self.compile_fragment_shader(draw, read_gpu);

        let vs_hash = hash_spirv(&vs_compiled.spirv_words);
        let fs_hash = fs_compiled
            .as_ref()
            .map(|f| hash_spirv(&f.spirv_words))
            .unwrap_or(0);

        let key = GraphicsPipelineKey {
            shader_hashes: [vs_hash, fs_hash],
            fixed_state: fixed_state.clone(),
        };
        Some((key, fixed_state))
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
        _fixed_state: &FixedPipelineState,
    ) -> Option<GraphicsPipeline> {
        let vs_compiled = self.compile_vertex_shader(draw, read_gpu)?;
        let fs_compiled = self.compile_fragment_shader(draw, read_gpu);

        // Create shader modules
        let vs_module = self.create_shader_module(&vs_compiled.spirv_words)?;
        let fs_module = if let Some(ref fs) = fs_compiled {
            self.create_shader_module(&fs.spirv_words)?
        } else {
            // Use VS module as FS fallback (will produce undefined output but won't crash)
            self.create_shader_module(&vs_compiled.spirv_words)?
        };

        // Create pipeline layout and pipeline
        let (pipeline_layout, descriptor_set_layout) = self.create_pipeline_layout()?;
        let pipeline = self.create_graphics_pipeline(
            vs_module,
            fs_module,
            pipeline_layout,
            render_pass,
            draw,
        )?;

        debug!(
            "GraphicsPipelineCache: compiled new pipeline (vs_hash=0x{:016X}, fs_hash=0x{:016X})",
            key.shader_hashes[0], key.shader_hashes[1]
        );

        Some(GraphicsPipeline {
            device: self.device.clone(),
            key: key.clone(),
            transition_keys: Vec::new(),
            pipeline,
            pipeline_layout,
            descriptor_set_layout,
            vs_module,
            fs_module,
            is_built: AtomicBool::new(true),
        })
    }

    fn compile_vertex_shader(
        &mut self,
        draw: &DrawCall,
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

        let runtime_info = RuntimeInfo::default();
        let compiled = self
            .shader_cache
            .get_or_compile(&code, ShaderStage::VertexB, &runtime_info);
        Some(compiled.clone())
    }

    fn compile_fragment_shader(
        &mut self,
        draw: &DrawCall,
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

        let runtime_info = RuntimeInfo::default();
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

    fn create_pipeline_layout(&self) -> Option<(vk::PipelineLayout, vk::DescriptorSetLayout)> {
        // Create descriptor set layout (empty for now — will add UBO/sampler bindings)
        let layout_info = vk::DescriptorSetLayoutCreateInfo::builder().build();
        let desc_layout = unsafe {
            self.device
                .create_descriptor_set_layout(&layout_info, None)
                .ok()?
        };

        let layouts = [desc_layout];
        let pipeline_layout_info = vk::PipelineLayoutCreateInfo::builder()
            .set_layouts(&layouts)
            .build();
        let pipeline_layout = unsafe {
            self.device
                .create_pipeline_layout(&pipeline_layout_info, None)
                .ok()?
        };

        Some((pipeline_layout, desc_layout))
    }

    fn create_graphics_pipeline(
        &self,
        vs_module: vk::ShaderModule,
        fs_module: vk::ShaderModule,
        pipeline_layout: vk::PipelineLayout,
        render_pass: vk::RenderPass,
        draw: &DrawCall,
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

        let vertex_input = vk::PipelineVertexInputStateCreateInfo::builder().build();

        let input_assembly = vk::PipelineInputAssemblyStateCreateInfo::builder()
            .topology(super::map_topology(draw.topology))
            .primitive_restart_enable(false)
            .build();

        let viewport_state = vk::PipelineViewportStateCreateInfo::builder()
            .viewport_count(1)
            .scissor_count(1)
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

        let blend_attachment = vk::PipelineColorBlendAttachmentState::builder()
            .blend_enable(draw.blend[0].enabled)
            .src_color_blend_factor(super::map_blend_factor(draw.blend[0].color_src))
            .dst_color_blend_factor(super::map_blend_factor(draw.blend[0].color_dst))
            .color_blend_op(super::map_blend_equation(draw.blend[0].color_op))
            .src_alpha_blend_factor(super::map_blend_factor(draw.blend[0].alpha_src))
            .dst_alpha_blend_factor(super::map_blend_factor(draw.blend[0].alpha_dst))
            .alpha_blend_op(super::map_blend_equation(draw.blend[0].alpha_op))
            .color_write_mask(
                vk::ColorComponentFlags::R
                    | vk::ColorComponentFlags::G
                    | vk::ColorComponentFlags::B
                    | vk::ColorComponentFlags::A,
            )
            .build();

        let color_blend = vk::PipelineColorBlendStateCreateInfo::builder()
            .logic_op_enable(false)
            .attachments(std::slice::from_ref(&blend_attachment))
            .blend_constants([
                draw.blend_color.r,
                draw.blend_color.g,
                draw.blend_color.b,
                draw.blend_color.a,
            ])
            .build();

        let dynamic_states = [vk::DynamicState::VIEWPORT, vk::DynamicState::SCISSOR];
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
            shader_hashes: [123, 456],
            fixed_state: FixedPipelineState::default(),
        };
        let key_b = GraphicsPipelineKey {
            shader_hashes: [123, 456],
            fixed_state: FixedPipelineState::default(),
        };
        assert_eq!(key_a, key_b);
    }

    #[test]
    fn test_transition_graph_returns_self_and_added_transition() {
        let key_a = GraphicsPipelineKey {
            shader_hashes: [123, 456],
            fixed_state: FixedPipelineState::default(),
        };
        let key_b = GraphicsPipelineKey {
            shader_hashes: [789, 321],
            fixed_state: FixedPipelineState::default(),
        };

        let mut pipeline = make_test_pipeline(key_a.clone());
        pipeline.add_transition(key_b.clone());

        assert_eq!(pipeline.next(&key_a), Some(&key_a));
        assert_eq!(pipeline.next(&key_b), Some(&key_b));
        assert!(pipeline.is_built());
    }
}
