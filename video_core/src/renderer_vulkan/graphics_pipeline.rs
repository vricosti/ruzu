// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Graphics pipeline compilation helpers.
//!
//! Ref: zuyu `vk_graphics_pipeline.h` and `vk_pipeline_cache.h` — this file is
//! the reduced compilation leaf, while `pipeline_cache.rs` owns the matching
//! top-level pipeline-cache state and lookup flow.

use ash::vk;
use common::cityhash::city_hash64;
use common::thread_worker::ThreadWorker;
use log::{debug, warn};
use std::collections::BTreeMap;
use std::panic::{catch_unwind, take_hook, AssertUnwindSafe};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Condvar, Mutex};
use std::time::Instant;

use crate::buffer_cache::buffer_cache_base::{
    UniformBufferSizes, NUM_GRAPHICS_UNIFORM_BUFFERS, NUM_STAGES,
};
use crate::engines::maxwell_3d::{
    ComparisonOp, CullFace, DrawCall, PrimitiveTopology, ShaderStageType, VertexAttribSize,
    VertexAttribType,
};
use crate::shader;
use crate::shader_cache::{GraphicsEnvironments, NUM_PROGRAMS};
use crate::surface::{
    is_pixel_format_integer, is_pixel_format_signed_integer, pixel_format_from_render_target_format,
};
use shader_recompiler::backend::bindings::Bindings;
use shader_recompiler::host_translate_info::HostTranslateInfo;
use shader_recompiler::runtime_info::{
    AttributeType, CompareFunction, InputTopology, TessPrimitive, TessSpacing,
};
use shader_recompiler::shader_info::{Info as ShaderInfo, TextureType};
use shader_recompiler::{CompiledShader, PipelineCache, Profile, RuntimeInfo, ShaderStage};

use super::descriptor_pool::DescriptorBankInfo;
use super::fixed_pipeline_state::FixedPipelineState;
use super::maxwell_to_vk;
use super::pipeline_helper::{
    RENDERAREA_LAYOUT_SIZE, RESCALING_LAYOUT_SIZE, RESCALING_LAYOUT_WORDS_OFFSET,
};

const NUM_VK_GRAPHICS_STAGES: usize = 5;

/// One-time installation of the shader-compile panic-hook filter used by
/// `catch_shader_exception`. See that function for the rationale.
static SHADER_EXCEPTION_HOOK_INSTALL: std::sync::Once = std::sync::Once::new();

thread_local! {
    /// True while the current thread is inside `catch_shader_exception`.
    /// The process-wide panic hook stays silent for such threads.
    static IN_SHADER_EXCEPTION_SCOPE: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

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

impl GraphicsPipelineKey {
    /// Port of upstream `GraphicsPipelineCacheKey::Size`.
    pub fn serialized_size(&self) -> usize {
        std::mem::size_of::<[u64; NUM_PROGRAMS]>() + self.fixed_state.serialized_size()
    }

    pub fn to_cache_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::with_capacity(self.serialized_size());
        for hash in self.unique_hashes {
            bytes.extend_from_slice(&hash.to_le_bytes());
        }
        self.fixed_state.write_prefix_bytes(&mut bytes);
        debug_assert_eq!(bytes.len(), self.serialized_size());
        bytes
    }

    pub fn read_from_file(file: &mut std::fs::File) -> std::io::Result<Self> {
        use std::io::Read;

        let mut unique_hashes = [0u64; NUM_PROGRAMS];
        for hash in &mut unique_hashes {
            let mut buf = [0u8; 8];
            file.read_exact(&mut buf)?;
            *hash = u64::from_le_bytes(buf);
        }
        let fixed_state = FixedPipelineState::read_from_file(file)?;
        Ok(Self {
            unique_hashes,
            fixed_state,
        })
    }
}

/// A compiled Vulkan graphics pipeline.
pub struct GraphicsPipeline {
    device: ash::Device,
    key: GraphicsPipelineKey,
    transition_keys: Vec<GraphicsPipelineKey>,
    pipeline: Arc<Mutex<vk::Pipeline>>,
    pub pipeline_layout: vk::PipelineLayout,
    pub descriptor_set_layout: vk::DescriptorSetLayout,
    pub descriptor_bindings: Vec<GraphicsDescriptorBinding>,
    pub descriptor_bank_info: DescriptorBankInfo,
    pub stage_infos: [Option<ShaderInfo>; 5],
    pub enabled_uniform_buffer_masks: [u32; NUM_STAGES as usize],
    pub uniform_buffer_sizes: UniformBufferSizes,
    pub uses_render_area: bool,
    pub uses_rescaling_uniform: bool,
    /// Upstream `std::array<vk::ShaderModule, NUM_STAGES> spv_modules`.
    pub shader_modules: [vk::ShaderModule; NUM_VK_GRAPHICS_STAGES],
    build_condvar: Arc<Condvar>,
    build_mutex: Arc<Mutex<()>>,
    is_built: Arc<AtomicBool>,
}

// Vulkan pipeline objects are opaque device handles. Upstream builds them on
// worker threads and transfers the resulting `unique_ptr<GraphicsPipeline>` to
// the pipeline cache; the Rust preload path mirrors that ownership transfer.
unsafe impl Send for GraphicsPipeline {}

impl Drop for GraphicsPipeline {
    fn drop(&mut self) {
        self.wait_for_build();
        unsafe {
            let pipeline = *self.pipeline.lock().unwrap();
            if pipeline != vk::Pipeline::null() {
                self.device.destroy_pipeline(pipeline, None);
            }
            self.device
                .destroy_pipeline_layout(self.pipeline_layout, None);
            self.device
                .destroy_descriptor_set_layout(self.descriptor_set_layout, None);
            for module in self.shader_modules {
                if module != vk::ShaderModule::null() {
                    self.device.destroy_shader_module(module, None);
                }
            }
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

    /// Upstream `GraphicsPipeline::ConfigureDraw` waits for async pipeline
    /// creation before binding the handle. The current Rust draw path still
    /// performs configure/bind outside this object, so expose the same wait at
    /// the handle boundary until `ConfigureDraw` ownership is ported.
    pub fn pipeline_handle(&self) -> Option<vk::Pipeline> {
        self.wait_for_build();
        let pipeline = *self.pipeline.lock().unwrap();
        (pipeline != vk::Pipeline::null()).then_some(pipeline)
    }

    fn wait_for_build(&self) {
        if self.is_built.load(Ordering::Acquire) {
            return;
        }
        let lock = self.build_mutex.lock().unwrap();
        let _guard = self
            .build_condvar
            .wait_while(lock, |_| !self.is_built.load(Ordering::Relaxed))
            .unwrap();
    }
}

fn buffer_cache_metadata(
    stage_infos: &[Option<ShaderInfo>; 5],
) -> ([u32; NUM_STAGES as usize], UniformBufferSizes) {
    let mut masks = [0u32; NUM_STAGES as usize];
    let mut sizes = [[0u32; NUM_GRAPHICS_UNIFORM_BUFFERS as usize]; NUM_STAGES as usize];
    for stage in 0..NUM_STAGES as usize {
        let Some(info) = stage_infos.get(stage).and_then(Option::as_ref) else {
            continue;
        };
        masks[stage] = info.constant_buffer_mask;
        sizes[stage].copy_from_slice(&info.constant_buffer_used_sizes);
    }
    (masks, sizes)
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
    extended_dynamic_state2_extra_supported: bool,
    extended_dynamic_state3_blend_supported: bool,
    extended_dynamic_state3_enables_supported: bool,
    dynamic_vertex_input_supported: bool,
    must_emulate_scaled_formats: bool,
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
        extended_dynamic_state2_extra_supported: bool,
        extended_dynamic_state3_blend_supported: bool,
        extended_dynamic_state3_enables_supported: bool,
        dynamic_vertex_input_supported: bool,
        must_emulate_scaled_formats: bool,
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
            extended_dynamic_state2_extra_supported,
            extended_dynamic_state3_blend_supported,
            extended_dynamic_state3_enables_supported,
            dynamic_vertex_input_supported,
            must_emulate_scaled_formats,
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
        fixed_state
            .set_extended_dynamic_state_2_extra(self.extended_dynamic_state2_extra_supported);
        fixed_state
            .set_extended_dynamic_state_3_blend(self.extended_dynamic_state3_blend_supported);
        fixed_state
            .set_extended_dynamic_state_3_enables(self.extended_dynamic_state3_enables_supported);
        fixed_state.set_dynamic_vertex_input(self.dynamic_vertex_input_supported);

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

    pub fn extended_dynamic_state_supported(&self) -> bool {
        self.extended_dynamic_state_supported
    }

    pub fn extended_dynamic_state2_supported(&self) -> bool {
        self.extended_dynamic_state2_supported
    }

    pub fn extended_dynamic_state2_extra_supported(&self) -> bool {
        self.extended_dynamic_state2_extra_supported
    }

    pub fn extended_dynamic_state3_blend_supported(&self) -> bool {
        self.extended_dynamic_state3_blend_supported
    }

    pub fn extended_dynamic_state3_enables_supported(&self) -> bool {
        self.extended_dynamic_state3_enables_supported
    }

    pub fn dynamic_vertex_input_supported(&self) -> bool {
        self.dynamic_vertex_input_supported
    }

    pub fn clone_for_disk_worker(&self) -> Self {
        Self {
            device: self.device.clone(),
            shader_cache: PipelineCache::new(self.profile.clone()),
            profile: self.profile.clone(),
            host_info: self.host_info.clone(),
            extended_dynamic_state_supported: self.extended_dynamic_state_supported,
            extended_dynamic_state2_supported: self.extended_dynamic_state2_supported,
            extended_dynamic_state2_extra_supported: self.extended_dynamic_state2_extra_supported,
            extended_dynamic_state3_blend_supported: self.extended_dynamic_state3_blend_supported,
            extended_dynamic_state3_enables_supported: self
                .extended_dynamic_state3_enables_supported,
            dynamic_vertex_input_supported: self.dynamic_vertex_input_supported,
            must_emulate_scaled_formats: self.must_emulate_scaled_formats,
            topology_list_primitive_restart_supported: self
                .topology_list_primitive_restart_supported,
            patch_list_primitive_restart_supported: self.patch_list_primitive_restart_supported,
            max_viewports: self.max_viewports,
        }
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
        fixed_state
            .set_extended_dynamic_state_2_extra(self.extended_dynamic_state2_extra_supported);
        fixed_state
            .set_extended_dynamic_state_3_blend(self.extended_dynamic_state3_blend_supported);
        fixed_state
            .set_extended_dynamic_state_3_enables(self.extended_dynamic_state3_enables_supported);
        fixed_state.set_dynamic_vertex_input(self.dynamic_vertex_input_supported);
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
        pipeline_cache: vk::PipelineCache,
        read_gpu: &dyn Fn(u64, &mut [u8]),
        key: &GraphicsPipelineKey,
        fixed_state: &FixedPipelineState,
    ) -> Option<GraphicsPipeline> {
        let vs_compiled = self.compile_vertex_shader(draw, fixed_state, read_gpu)?;
        let fs_compiled = self.compile_fragment_shader(draw, fixed_state, read_gpu);
        let mut compiled_stages: [Option<CompiledShader>; NUM_VK_GRAPHICS_STAGES] =
            Default::default();
        compiled_stages[0] = Some(vs_compiled);
        compiled_stages[4] = fs_compiled;

        // Create pipeline layout and pipeline
        let shader_modules = self.create_shader_modules(&compiled_stages)?;
        let descriptor_layout = self.create_pipeline_layout(&compiled_stages)?;
        let vs_compiled = compiled_stages[0]
            .as_ref()
            .expect("vertex stage was compiled before module creation");
        let pipeline = self.create_graphics_pipeline(
            &shader_modules,
            descriptor_layout.pipeline_layout,
            render_pass,
            pipeline_cache,
            draw,
            fixed_state,
            vs_compiled,
        )?;

        debug!(
            "GraphicsPipelineCache: compiled new pipeline (vs_hash=0x{:016X}, fs_hash=0x{:016X})",
            key.unique_hashes[1], key.unique_hashes[5]
        );

        let stage_infos = {
            let mut infos: [Option<ShaderInfo>; 5] = Default::default();
            for (index, compiled) in compiled_stages.iter().enumerate() {
                if let Some(compiled) = compiled {
                    infos[index] = Some(compiled.info.clone());
                }
            }
            infos
        };
        let (enabled_uniform_buffer_masks, uniform_buffer_sizes) =
            buffer_cache_metadata(&stage_infos);
        let uses_render_area = stage_infos
            .iter()
            .flatten()
            .any(|info| info.uses_render_area);
        let uses_rescaling_uniform = stage_infos
            .iter()
            .flatten()
            .any(|info| info.uses_rescaling_uniform);

        Some(GraphicsPipeline {
            device: self.device.clone(),
            key: key.clone(),
            transition_keys: Vec::new(),
            pipeline: Arc::new(Mutex::new(pipeline)),
            pipeline_layout: descriptor_layout.pipeline_layout,
            descriptor_set_layout: descriptor_layout.descriptor_set_layout,
            descriptor_bindings: descriptor_layout.bindings,
            descriptor_bank_info: descriptor_layout.bank_info,
            stage_infos,
            enabled_uniform_buffer_masks,
            uniform_buffer_sizes,
            uses_render_area,
            uses_rescaling_uniform,
            shader_modules,
            build_condvar: Arc::new(Condvar::new()),
            build_mutex: Arc::new(Mutex::new(())),
            is_built: Arc::new(AtomicBool::new(true)),
        })
    }

    /// Build a Vulkan graphics pipeline from shared shader environments.
    pub fn build_pipeline_keyed_from_environments(
        &mut self,
        draw: &DrawCall,
        render_pass: vk::RenderPass,
        pipeline_cache: vk::PipelineCache,
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
        let mut compiled_stages: [Option<CompiledShader>; NUM_VK_GRAPHICS_STAGES] =
            Default::default();
        compiled_stages[0] = Some(vs_compiled);
        compiled_stages[4] = fs_compiled;

        let shader_modules = self.create_shader_modules(&compiled_stages)?;
        let descriptor_layout = self.create_pipeline_layout(&compiled_stages)?;
        let vs_compiled = compiled_stages[0]
            .as_ref()
            .expect("vertex stage was compiled before module creation");
        let pipeline = self.create_graphics_pipeline(
            &shader_modules,
            descriptor_layout.pipeline_layout,
            render_pass,
            pipeline_cache,
            draw,
            fixed_state,
            vs_compiled,
        )?;

        debug!(
            "GraphicsPipelineCache: compiled env pipeline (vb_hash=0x{:016X}, fs_hash=0x{:016X})",
            key.unique_hashes[1], key.unique_hashes[5]
        );

        let stage_infos = {
            let mut infos: [Option<ShaderInfo>; 5] = Default::default();
            for (index, compiled) in compiled_stages.iter().enumerate() {
                if let Some(compiled) = compiled {
                    infos[index] = Some(compiled.info.clone());
                }
            }
            infos
        };
        let (enabled_uniform_buffer_masks, uniform_buffer_sizes) =
            buffer_cache_metadata(&stage_infos);
        let uses_render_area = stage_infos
            .iter()
            .flatten()
            .any(|info| info.uses_render_area);
        let uses_rescaling_uniform = stage_infos
            .iter()
            .flatten()
            .any(|info| info.uses_rescaling_uniform);

        Some(GraphicsPipeline {
            device: self.device.clone(),
            key: key.clone(),
            transition_keys: Vec::new(),
            pipeline: Arc::new(Mutex::new(pipeline)),
            pipeline_layout: descriptor_layout.pipeline_layout,
            descriptor_set_layout: descriptor_layout.descriptor_set_layout,
            descriptor_bindings: descriptor_layout.bindings,
            descriptor_bank_info: descriptor_layout.bank_info,
            stage_infos,
            enabled_uniform_buffer_masks,
            uniform_buffer_sizes,
            uses_render_area,
            uses_rescaling_uniform,
            shader_modules,
            build_condvar: Arc::new(Condvar::new()),
            build_mutex: Arc::new(Mutex::new(())),
            is_built: Arc::new(AtomicBool::new(true)),
        })
    }

    /// Runtime async variant of `build_pipeline_keyed_from_environments`.
    ///
    /// This mirrors the upstream split where shader translation/module/layout
    /// setup happens before the `GraphicsPipeline` object is inserted, while
    /// final `vkCreateGraphicsPipelines` work can run on `ThreadWorker`.
    pub fn build_pipeline_keyed_from_environments_async(
        &mut self,
        draw: &DrawCall,
        render_pass: vk::RenderPass,
        pipeline_cache: vk::PipelineCache,
        environments: &mut GraphicsEnvironments,
        key: &GraphicsPipelineKey,
        fixed_state: &FixedPipelineState,
        worker: &ThreadWorker,
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
        let mut compiled_stages: [Option<CompiledShader>; NUM_VK_GRAPHICS_STAGES] =
            Default::default();
        compiled_stages[0] = Some(vs_compiled);
        compiled_stages[4] = fs_compiled;

        let shader_modules = self.create_shader_modules(&compiled_stages)?;
        let descriptor_layout = self.create_pipeline_layout(&compiled_stages)?;
        let vs_compiled = compiled_stages[0]
            .as_ref()
            .expect("vertex stage was compiled before module creation")
            .clone();

        let stage_infos = {
            let mut infos: [Option<ShaderInfo>; 5] = Default::default();
            for (index, compiled) in compiled_stages.iter().enumerate() {
                if let Some(compiled) = compiled {
                    infos[index] = Some(compiled.info.clone());
                }
            }
            infos
        };
        let (enabled_uniform_buffer_masks, uniform_buffer_sizes) =
            buffer_cache_metadata(&stage_infos);
        let uses_render_area = stage_infos
            .iter()
            .flatten()
            .any(|info| info.uses_render_area);
        let uses_rescaling_uniform = stage_infos
            .iter()
            .flatten()
            .any(|info| info.uses_rescaling_uniform);

        let pipeline = Arc::new(Mutex::new(vk::Pipeline::null()));
        let build_condvar = Arc::new(Condvar::new());
        let build_mutex = Arc::new(Mutex::new(()));
        let is_built = Arc::new(AtomicBool::new(false));

        let mut builder = self.clone_for_disk_worker();
        let shader_modules_for_worker = shader_modules;
        let draw_for_worker = draw.clone();
        let fixed_state_for_worker = fixed_state.clone();
        let pipeline_for_worker = pipeline.clone();
        let build_condvar_for_worker = build_condvar.clone();
        let build_mutex_for_worker = build_mutex.clone();
        let is_built_for_worker = is_built.clone();
        worker.queue_stateless_work(move || {
            let created = builder.create_graphics_pipeline(
                &shader_modules_for_worker,
                descriptor_layout.pipeline_layout,
                render_pass,
                pipeline_cache,
                &draw_for_worker,
                &fixed_state_for_worker,
                &vs_compiled,
            );
            if let Some(created) = created {
                *pipeline_for_worker.lock().unwrap() = created;
            }
            {
                let _lock = build_mutex_for_worker.lock().unwrap();
                is_built_for_worker.store(true, Ordering::Release);
            }
            build_condvar_for_worker.notify_one();
        });

        Some(GraphicsPipeline {
            device: self.device.clone(),
            key: key.clone(),
            transition_keys: Vec::new(),
            pipeline,
            pipeline_layout: descriptor_layout.pipeline_layout,
            descriptor_set_layout: descriptor_layout.descriptor_set_layout,
            descriptor_bindings: descriptor_layout.bindings,
            descriptor_bank_info: descriptor_layout.bank_info,
            stage_infos,
            enabled_uniform_buffer_masks,
            uniform_buffer_sizes,
            uses_render_area,
            uses_rescaling_uniform,
            shader_modules,
            build_condvar,
            build_mutex,
            is_built,
        })
    }

    pub fn build_pipeline_keyed_from_file_environments(
        &mut self,
        render_pass: vk::RenderPass,
        pipeline_cache: vk::PipelineCache,
        environments: &mut [crate::shader_environment::FileEnvironment],
        key: &GraphicsPipelineKey,
    ) -> Option<GraphicsPipeline> {
        match catch_shader_exception(|| {
            self.build_pipeline_keyed_from_file_environments_impl(
                render_pass,
                pipeline_cache,
                environments,
                key,
            )
        }) {
            Ok(pipeline) => pipeline,
            Err(payload) => {
                let reason = payload
                    .downcast_ref::<&str>()
                    .copied()
                    .or_else(|| payload.downcast_ref::<String>().map(String::as_str))
                    .unwrap_or("unknown shader compiler panic");
                log::error!(
                    "Skipping cached graphics pipeline 0x{:016X}: {}",
                    graphics_pipeline_key_cache_hash(key),
                    reason
                );
                None
            }
        }
    }

    fn build_pipeline_keyed_from_file_environments_impl(
        &mut self,
        render_pass: vk::RenderPass,
        pipeline_cache: vk::PipelineCache,
        environments: &mut [crate::shader_environment::FileEnvironment],
        key: &GraphicsPipelineKey,
    ) -> Option<GraphicsPipeline> {
        let mut bindings = Bindings::default();
        let mut compiled_stages: [Option<CompiledShader>; 5] = Default::default();
        let mut previous_stage_info: Option<ShaderInfo> = None;

        let vertex_a_env = environments
            .iter()
            .position(|env| env.shader_stage() == ShaderStage::VertexA);
        let vertex_b_env = environments
            .iter()
            .position(|env| env.shader_stage() == ShaderStage::VertexB);
        if let (Some(vertex_a_index), Some(vertex_b_index)) = (vertex_a_env, vertex_b_env) {
            let runtime_info = make_runtime_info_from_state(key, ShaderStage::VertexB, None);
            let (vertex_a, vertex_b) = two_mut(environments, vertex_a_index, vertex_b_index)?;
            let vertex_a_code = vertex_a.cached_instruction_slice().to_vec();
            let vertex_b_code = vertex_b.cached_instruction_slice().to_vec();
            if vertex_a_code.is_empty() || vertex_b_code.is_empty() {
                return None;
            }
            let compiled =
                shader_recompiler::compile_dual_vertex_shader_from_env_with_bindings_and_host_info(
                    &vertex_a_code,
                    vertex_a.start_address(),
                    vertex_a,
                    &vertex_b_code,
                    vertex_b.start_address(),
                    vertex_b,
                    &self.profile,
                    &runtime_info,
                    &mut bindings,
                    &self.host_info,
                );
            previous_stage_info = Some(compiled.info.clone());
            compiled_stages[0] = Some(compiled);
        }

        for (env_index, env) in environments.iter_mut().enumerate() {
            let stage = env.shader_stage();
            if matches!(stage, ShaderStage::VertexA) {
                continue;
            }
            if matches!(stage, ShaderStage::VertexB) && compiled_stages[0].is_some() {
                continue;
            }
            let Some(stage_index) = shader_stage_to_graphics_info_index(stage) else {
                continue;
            };
            let runtime_info =
                make_runtime_info_from_state(key, stage, previous_stage_info.as_ref());
            let code = env.cached_instruction_slice().to_vec();
            if code.is_empty() {
                continue;
            }
            let compiled = shader_recompiler::compile_shader_from_env_with_bindings_and_host_info(
                &code,
                env.start_address(),
                env,
                &self.profile,
                &runtime_info,
                &mut bindings,
                &self.host_info,
            );
            if std::env::var_os("RUZU_TRACE_SHADER_WORDS").is_some() {
                log::info!(
                    "[VK_SHADER_DISK_COMPILE] env_index={} stage={:?} spirv_words={}",
                    env_index,
                    stage,
                    compiled.spirv_words.len()
                );
            }
            previous_stage_info = Some(compiled.info.clone());
            compiled_stages[stage_index] = Some(compiled);
        }

        let vs_compiled = compiled_stages[0].as_ref()?;
        let shader_modules = self.create_shader_modules(&compiled_stages)?;
        let descriptor_layout = self.create_pipeline_layout(&compiled_stages)?;
        let pipeline = self.create_graphics_pipeline_from_state(
            &shader_modules,
            descriptor_layout.pipeline_layout,
            render_pass,
            pipeline_cache,
            &key.fixed_state,
            vs_compiled,
        )?;

        let stage_infos = {
            let mut infos: [Option<ShaderInfo>; 5] = Default::default();
            for (index, compiled) in compiled_stages.iter().enumerate() {
                if let Some(compiled) = compiled {
                    infos[index] = Some(compiled.info.clone());
                }
            }
            infos
        };
        let (enabled_uniform_buffer_masks, uniform_buffer_sizes) =
            buffer_cache_metadata(&stage_infos);
        let uses_render_area = stage_infos
            .iter()
            .flatten()
            .any(|info| info.uses_render_area);
        let uses_rescaling_uniform = stage_infos
            .iter()
            .flatten()
            .any(|info| info.uses_rescaling_uniform);

        Some(GraphicsPipeline {
            device: self.device.clone(),
            key: key.clone(),
            transition_keys: Vec::new(),
            pipeline: Arc::new(Mutex::new(pipeline)),
            pipeline_layout: descriptor_layout.pipeline_layout,
            descriptor_set_layout: descriptor_layout.descriptor_set_layout,
            descriptor_bindings: descriptor_layout.bindings,
            descriptor_bank_info: descriptor_layout.bank_info,
            stage_infos,
            enabled_uniform_buffer_masks,
            uniform_buffer_sizes,
            uses_render_area,
            uses_rescaling_uniform,
            shader_modules,
            build_condvar: Arc::new(Condvar::new()),
            build_mutex: Arc::new(Mutex::new(())),
            is_built: Arc::new(AtomicBool::new(true)),
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

    fn create_shader_modules(
        &self,
        compiled_stages: &[Option<CompiledShader>; NUM_VK_GRAPHICS_STAGES],
    ) -> Option<[vk::ShaderModule; NUM_VK_GRAPHICS_STAGES]> {
        let mut modules = [vk::ShaderModule::null(); NUM_VK_GRAPHICS_STAGES];
        for (index, compiled) in compiled_stages.iter().enumerate() {
            let Some(compiled) = compiled else {
                continue;
            };
            modules[index] = self.create_shader_module(&compiled.spirv_words)?;
        }
        Some(modules)
    }

    fn create_pipeline_layout(
        &self,
        compiled_stages: &[Option<CompiledShader>; NUM_VK_GRAPHICS_STAGES],
    ) -> Option<GraphicsDescriptorLayout> {
        let mut bindings = BTreeMap::new();
        let mut binding = 0u32;
        for (stage_index, compiled) in compiled_stages.iter().enumerate() {
            let Some(compiled) = compiled else {
                continue;
            };
            add_shader_descriptor_bindings(
                &mut bindings,
                compiled,
                graphics_stage_flags(stage_index),
                stage_index as u32,
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
        shader_modules: &[vk::ShaderModule; NUM_VK_GRAPHICS_STAGES],
        pipeline_layout: vk::PipelineLayout,
        render_pass: vk::RenderPass,
        pipeline_cache: vk::PipelineCache,
        draw: &DrawCall,
        fixed_state: &FixedPipelineState,
        vs: &CompiledShader,
    ) -> Option<vk::Pipeline> {
        let entry_name = std::ffi::CString::new("main").unwrap();

        let shader_stages = shader_stage_create_infos(shader_modules, &entry_name);

        let (vertex_bindings, vertex_attributes) =
            build_vertex_input_state(draw, vs, self.must_emulate_scaled_formats);
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
        let tessellation = vk::PipelineTessellationStateCreateInfo::builder()
            .patch_control_points(patch_control_points_for_state(fixed_state))
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
            .tessellation_state(&tessellation)
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
                .create_graphics_pipelines(pipeline_cache, &[pipeline_info], None)
        } {
            Ok(pipelines) => Some(pipelines[0]),
            Err((_, e)) => {
                warn!("GraphicsPipelineCache: pipeline creation failed: {:?}", e);
                None
            }
        }
    }

    fn create_graphics_pipeline_from_state(
        &self,
        shader_modules: &[vk::ShaderModule; NUM_VK_GRAPHICS_STAGES],
        pipeline_layout: vk::PipelineLayout,
        render_pass: vk::RenderPass,
        pipeline_cache: vk::PipelineCache,
        fixed_state: &FixedPipelineState,
        vs: &CompiledShader,
    ) -> Option<vk::Pipeline> {
        let entry_name = std::ffi::CString::new("main").unwrap();

        let shader_stages = shader_stage_create_infos(shader_modules, &entry_name);

        let (vertex_bindings, vertex_divisors, vertex_attributes) =
            build_vertex_input_state_from_state(fixed_state, vs, self.must_emulate_scaled_formats);
        let mut vertex_divisor_state = vk::PipelineVertexInputDivisorStateCreateInfoEXT::builder()
            .vertex_binding_divisors(&vertex_divisors);
        let mut vertex_input_builder = vk::PipelineVertexInputStateCreateInfo::builder()
            .vertex_binding_descriptions(&vertex_bindings)
            .vertex_attribute_descriptions(&vertex_attributes);
        if !vertex_divisors.is_empty() {
            vertex_input_builder = vertex_input_builder.push_next(&mut vertex_divisor_state);
        }
        let vertex_input = vertex_input_builder.build();

        let input_assembly_topology =
            input_assembly_topology_for_state(fixed_state, shader_modules);
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
        let tessellation = vk::PipelineTessellationStateCreateInfo::builder()
            .patch_control_points(patch_control_points_for_state(fixed_state))
            .build();

        let num_viewports = self.max_viewports.max(1);
        let viewport_state = vk::PipelineViewportStateCreateInfo::builder()
            .viewport_count(num_viewports)
            .scissor_count(num_viewports)
            .build();

        let rasterization = vk::PipelineRasterizationStateCreateInfo::builder()
            .depth_clamp_enable(!fixed_state.dynamic_state.depth_clamp_disabled())
            .rasterizer_discard_enable(!fixed_state.dynamic_state.rasterize_enable())
            .polygon_mode(maxwell_to_vk::polygon_mode(fixed_state.polygon_mode()))
            .cull_mode(if fixed_state.dynamic_state.cull_enable() {
                map_cull_face(fixed_state.dynamic_state.cull_face())
            } else {
                vk::CullModeFlags::NONE
            })
            .front_face(maxwell_to_vk::front_face(
                fixed_state.dynamic_state.front_face(),
            ))
            .depth_bias_enable(fixed_state.dynamic_state.depth_bias_enable())
            .line_width(1.0)
            .build();

        let multisample = vk::PipelineMultisampleStateCreateInfo::builder()
            .rasterization_samples(
                super::render_pass_cache::RenderPassKey::from_fixed_pipeline_state(fixed_state)
                    .samples,
            )
            .sample_shading_enable(false)
            .alpha_to_coverage_enable(fixed_state.alpha_to_coverage_enabled())
            .alpha_to_one_enable(fixed_state.alpha_to_one_enabled())
            .build();

        let depth_stencil = vk::PipelineDepthStencilStateCreateInfo::builder()
            .depth_test_enable(fixed_state.dynamic_state.depth_test_enable())
            .depth_write_enable(fixed_state.dynamic_state.depth_write_enable())
            .depth_compare_op(if fixed_state.dynamic_state.depth_test_enable() {
                maxwell_to_vk::comparison_op(fixed_state.dynamic_state.depth_test_func())
            } else {
                vk::CompareOp::ALWAYS
            })
            // Upstream gates this on Device::IsDepthBoundsSupported. The
            // active Rust Vulkan owner does not carry that device capability
            // into this reduced constructor yet, so keep the same conservative
            // behavior as the live dynamic-state path.
            .depth_bounds_test_enable(false)
            .stencil_test_enable(fixed_state.dynamic_state.stencil_enable())
            .front(stencil_face_state(
                fixed_state.dynamic_state.front_stencil(),
                fixed_state.dynamic_state.raw2,
            ))
            .back(stencil_face_state(
                fixed_state.dynamic_state.back_stencil(),
                fixed_state.dynamic_state.raw2,
            ))
            .build();

        let num_attachments = fixed_state
            .color_formats
            .iter()
            .rposition(|&format| format != 0)
            .map(|index| index + 1)
            .unwrap_or(1);
        let blend_attachments = (0..num_attachments)
            .map(|index| {
                let attachment = fixed_state.attachments[index];
                let mask = attachment.mask();
                let mut write_mask = vk::ColorComponentFlags::empty();
                if mask[0] {
                    write_mask |= vk::ColorComponentFlags::R;
                }
                if mask[1] {
                    write_mask |= vk::ColorComponentFlags::G;
                }
                if mask[2] {
                    write_mask |= vk::ColorComponentFlags::B;
                }
                if mask[3] {
                    write_mask |= vk::ColorComponentFlags::A;
                }
                vk::PipelineColorBlendAttachmentState::builder()
                    .blend_enable(attachment.is_enabled())
                    .src_color_blend_factor(maxwell_to_vk::blend_factor(
                        attachment.source_rgb_factor(),
                    ))
                    .dst_color_blend_factor(maxwell_to_vk::blend_factor(
                        attachment.dest_rgb_factor(),
                    ))
                    .color_blend_op(maxwell_to_vk::blend_equation(attachment.equation_rgb()))
                    .src_alpha_blend_factor(maxwell_to_vk::blend_factor(
                        attachment.source_alpha_factor(),
                    ))
                    .dst_alpha_blend_factor(maxwell_to_vk::blend_factor(
                        attachment.dest_alpha_factor(),
                    ))
                    .alpha_blend_op(maxwell_to_vk::blend_equation(attachment.equation_alpha()))
                    .color_write_mask(write_mask)
                    .build()
            })
            .collect::<Vec<_>>();

        let color_blend = vk::PipelineColorBlendStateCreateInfo::builder()
            .logic_op_enable(fixed_state.dynamic_state.logic_op_enable())
            .logic_op(vk::LogicOp::from_raw(
                fixed_state.dynamic_state.logic_op() as i32
            ))
            .attachments(&blend_attachments)
            .build();

        let dynamic_states = dynamic_states_for_fixed_state(fixed_state);
        let dynamic_state = vk::PipelineDynamicStateCreateInfo::builder()
            .dynamic_states(&dynamic_states)
            .build();

        let pipeline_info = vk::GraphicsPipelineCreateInfo::builder()
            .stages(&shader_stages)
            .vertex_input_state(&vertex_input)
            .input_assembly_state(&input_assembly)
            .tessellation_state(&tessellation)
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
                .create_graphics_pipelines(pipeline_cache, &[pipeline_info], None)
        } {
            Ok(pipelines) => Some(pipelines[0]),
            Err((_, e)) => {
                warn!(
                    "GraphicsPipelineCache: disk pipeline creation failed: {:?}",
                    e
                );
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

fn graphics_stage_flags(stage_index: usize) -> vk::ShaderStageFlags {
    match stage_index {
        0 => vk::ShaderStageFlags::VERTEX,
        1 => vk::ShaderStageFlags::TESSELLATION_CONTROL,
        2 => vk::ShaderStageFlags::TESSELLATION_EVALUATION,
        3 => vk::ShaderStageFlags::GEOMETRY,
        4 => vk::ShaderStageFlags::FRAGMENT,
        _ => vk::ShaderStageFlags::empty(),
    }
}

fn shader_stage_create_infos(
    shader_modules: &[vk::ShaderModule; NUM_VK_GRAPHICS_STAGES],
    entry_name: &std::ffi::CStr,
) -> Vec<vk::PipelineShaderStageCreateInfo> {
    shader_modules
        .iter()
        .enumerate()
        .filter_map(|(stage_index, &module)| {
            if module == vk::ShaderModule::null() {
                return None;
            }
            Some(
                vk::PipelineShaderStageCreateInfo::builder()
                    .stage(graphics_stage_flags(stage_index))
                    .module(module)
                    .name(entry_name)
                    .build(),
            )
        })
        .collect()
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

/// Port of upstream `GraphicsPipeline::MakePipeline` tessellation/topology
/// compatibility fixup before creating `VkPipelineInputAssemblyStateCreateInfo`.
fn input_assembly_topology_for_state(
    fixed_state: &FixedPipelineState,
    shader_modules: &[vk::ShaderModule; NUM_VK_GRAPHICS_STAGES],
) -> vk::PrimitiveTopology {
    let has_tess_stages = shader_modules[1] != vk::ShaderModule::null()
        || shader_modules[2] != vk::ShaderModule::null();
    let mut topology = maxwell_to_vk::primitive_topology(fixed_state.topology());
    if topology == vk::PrimitiveTopology::PATCH_LIST {
        if !has_tess_stages {
            topology = vk::PrimitiveTopology::POINT_LIST;
        }
    } else if has_tess_stages {
        topology = vk::PrimitiveTopology::PATCH_LIST;
    }
    topology
}

/// Port of upstream `GraphicsPipeline::MakePipeline`
/// `VkPipelineTessellationStateCreateInfo::patchControlPoints`.
fn patch_control_points_for_state(fixed_state: &FixedPipelineState) -> u32 {
    fixed_state.patch_control_points()
}

fn stencil_face_state(
    face: &super::fixed_pipeline_state::StencilFace,
    raw: u32,
) -> vk::StencilOpState {
    vk::StencilOpState {
        fail_op: maxwell_to_vk::stencil_op(face.action_stencil_fail(raw)),
        pass_op: maxwell_to_vk::stencil_op(face.action_depth_pass(raw)),
        depth_fail_op: maxwell_to_vk::stencil_op(face.action_depth_fail(raw)),
        compare_op: maxwell_to_vk::comparison_op(face.test_func(raw)),
        compare_mask: 0,
        write_mask: 0,
        reference: 0,
    }
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

fn make_runtime_info_from_state(
    key: &GraphicsPipelineKey,
    stage: ShaderStage,
    previous_program: Option<&ShaderInfo>,
) -> RuntimeInfo {
    let fixed_state = &key.fixed_state;
    let mut info = RuntimeInfo::default();
    if let Some(previous_program) = previous_program {
        info.previous_stage_stores = previous_program.stores.clone();
        info.previous_stage_legacy_stores_mapping = previous_program.legacy_stores_mapping.clone();
    } else {
        info.previous_stage_stores.mask.fill(u64::MAX);
    }
    match stage {
        ShaderStage::VertexB => {
            let has_geometry = key.unique_hashes.get(4).copied().unwrap_or(0) != 0;
            if !has_geometry {
                if fixed_state.topology() == PrimitiveTopology::Points {
                    info.fixed_state_point_size = Some(f32::from_bits(fixed_state.point_size));
                }
                if fixed_state.xfb_enabled() {
                    fill_transform_feedback_runtime_info(&mut info, fixed_state);
                }
                info.convert_depth_mode = fixed_state.ndc_minus_one_to_one();
            }
            for (index, attrib) in fixed_state.attributes.iter().enumerate() {
                info.generic_input_types[index] = if fixed_state.dynamic_vertex_input() {
                    attribute_type_from_dynamic_state(fixed_state.dynamic_attribute_type(index))
                } else {
                    cast_attribute_type_from_state(*attrib)
                };
            }
        }
        ShaderStage::TessellationEval => {
            info.tess_clockwise = fixed_state.tessellation_clockwise();
            info.tess_primitive = tess_primitive_from_state(fixed_state.tessellation_primitive());
            info.tess_spacing = tess_spacing_from_state(fixed_state.tessellation_spacing());
        }
        ShaderStage::Geometry => {
            if fixed_state.xfb_enabled() {
                fill_transform_feedback_runtime_info(&mut info, fixed_state);
            }
            info.convert_depth_mode = fixed_state.ndc_minus_one_to_one();
        }
        ShaderStage::Fragment => {
            info.alpha_test_func =
                Some(compare_function_from_maxwell(fixed_state.alpha_test_func()));
            info.alpha_test_reference = f32::from_bits(fixed_state.alpha_test_ref);
            for (index, &format) in fixed_state.color_formats.iter().enumerate() {
                if format == 0 {
                    info.frag_color_types[index] = AttributeType::Float;
                    continue;
                }
                let pixel_format = pixel_format_from_render_target_format(format as u32);
                info.frag_color_types[index] = if is_pixel_format_signed_integer(pixel_format) {
                    AttributeType::SignedInt
                } else if is_pixel_format_integer(pixel_format) {
                    AttributeType::UnsignedInt
                } else {
                    AttributeType::Float
                };
            }
        }
        _ => {}
    }
    info.input_topology = input_topology_from_state(fixed_state.topology());
    info.force_early_z = fixed_state.early_z();
    info.y_negate = fixed_state.y_negate();
    info
}

fn fill_transform_feedback_runtime_info(info: &mut RuntimeInfo, fixed_state: &FixedPipelineState) {
    let (varyings, count) =
        crate::transform_feedback::make_transform_feedback_varyings(&fixed_state.xfb_state);
    info.xfb_varyings = varyings
        .iter()
        .map(
            |varying| shader_recompiler::runtime_info::TransformFeedbackVarying {
                buffer: varying.buffer,
                stride: varying.stride,
                offset: varying.offset,
                components: varying.components,
            },
        )
        .collect();
    info.xfb_count = count;
}

fn attribute_type_from_dynamic_state(value: u32) -> AttributeType {
    match value {
        0 => AttributeType::Disabled,
        1 => AttributeType::Float,
        2 => AttributeType::SignedInt,
        3 => AttributeType::UnsignedInt,
        _ => AttributeType::Disabled,
    }
}

fn tess_primitive_from_state(value: u32) -> TessPrimitive {
    match value {
        0 => TessPrimitive::Isolines,
        1 => TessPrimitive::Triangles,
        2 => TessPrimitive::Quads,
        _ => TessPrimitive::Triangles,
    }
}

fn tess_spacing_from_state(value: u32) -> TessSpacing {
    match value {
        0 => TessSpacing::Equal,
        1 => TessSpacing::FractionalOdd,
        2 => TessSpacing::FractionalEven,
        _ => TessSpacing::Equal,
    }
}

fn compare_function_from_maxwell(op: ComparisonOp) -> CompareFunction {
    match op {
        ComparisonOp::Never => CompareFunction::Never,
        ComparisonOp::Less => CompareFunction::Less,
        ComparisonOp::Equal => CompareFunction::Equal,
        ComparisonOp::LessEqual => CompareFunction::LessThanEqual,
        ComparisonOp::Greater => CompareFunction::Greater,
        ComparisonOp::NotEqual => CompareFunction::NotEqual,
        ComparisonOp::GreaterEqual => CompareFunction::GreaterThanEqual,
        ComparisonOp::Always => CompareFunction::Always,
    }
}

fn input_topology_from_state(topology: PrimitiveTopology) -> InputTopology {
    match topology {
        PrimitiveTopology::Points => InputTopology::Points,
        PrimitiveTopology::Lines | PrimitiveTopology::LineLoop | PrimitiveTopology::LineStrip => {
            InputTopology::Lines
        }
        PrimitiveTopology::LinesAdjacency | PrimitiveTopology::LineStripAdjacency => {
            InputTopology::LinesAdjacency
        }
        PrimitiveTopology::TrianglesAdjacency | PrimitiveTopology::TriangleStripAdjacency => {
            InputTopology::TrianglesAdjacency
        }
        PrimitiveTopology::Triangles
        | PrimitiveTopology::TriangleStrip
        | PrimitiveTopology::TriangleFan
        | PrimitiveTopology::Quads
        | PrimitiveTopology::QuadStrip
        | PrimitiveTopology::Polygon
        | PrimitiveTopology::Patches => InputTopology::Triangles,
    }
}

fn two_mut<T>(slice: &mut [T], lhs: usize, rhs: usize) -> Option<(&mut T, &mut T)> {
    if lhs == rhs || lhs >= slice.len() || rhs >= slice.len() {
        return None;
    }
    if lhs < rhs {
        let (left, right) = slice.split_at_mut(rhs);
        Some((&mut left[lhs], &mut right[0]))
    } else {
        let (left, right) = slice.split_at_mut(lhs);
        Some((&mut right[0], &mut left[rhs]))
    }
}

fn graphics_pipeline_key_cache_hash(key: &GraphicsPipelineKey) -> u64 {
    city_hash64(&key.to_cache_bytes())
}

/// Run a shader compilation, converting shader-recompiler panics into an
/// `Err` like upstream's `catch (const Shader::Exception&)` in
/// `vk_pipeline_cache.cpp`.
///
/// Upstream catches per-thread C++ exceptions with no global state. The Rust
/// port surfaces those failures as panics; to suppress the default
/// panic-hook backtrace spam we install — once — a wrapper around the
/// previous hook that stays silent only for threads currently inside this
/// function. An earlier version swapped the process-wide hook under a global
/// mutex held for the entire compilation, which serialized the disk-cache
/// preload to one pipeline at a time (measured on MK8D: 9 VkPipelineBuilder
/// workers ~96% blocked on that mutex, 79s preload).
fn catch_shader_exception<F, T>(f: F) -> std::thread::Result<T>
where
    F: FnOnce() -> T,
{
    SHADER_EXCEPTION_HOOK_INSTALL.call_once(|| {
        let previous = take_hook();
        std::panic::set_hook(Box::new(move |info| {
            if !IN_SHADER_EXCEPTION_SCOPE.with(std::cell::Cell::get) {
                previous(info);
            }
        }));
    });
    IN_SHADER_EXCEPTION_SCOPE.with(|flag| flag.set(true));
    let result = catch_unwind(AssertUnwindSafe(f));
    IN_SHADER_EXCEPTION_SCOPE.with(|flag| flag.set(false));
    result
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

fn cast_attribute_type_from_state(
    attrib: super::fixed_pipeline_state::VertexAttribute,
) -> AttributeType {
    if !attrib.is_enabled() {
        return AttributeType::Disabled;
    }
    match VertexAttribType::from_raw(attrib.attrib_type()) {
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

fn shader_stage_to_graphics_info_index(stage: ShaderStage) -> Option<usize> {
    match stage {
        ShaderStage::VertexA | ShaderStage::VertexB => Some(0),
        ShaderStage::TessellationControl => Some(1),
        ShaderStage::TessellationEval => Some(2),
        ShaderStage::Geometry => Some(3),
        ShaderStage::Fragment => Some(4),
        ShaderStage::Compute => None,
    }
}

fn map_cull_face(face: CullFace) -> vk::CullModeFlags {
    match face {
        CullFace::Front => vk::CullModeFlags::FRONT,
        CullFace::Back => vk::CullModeFlags::BACK,
        CullFace::FrontAndBack => vk::CullModeFlags::FRONT_AND_BACK,
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
    must_emulate_scaled_formats: bool,
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
        let format = maxwell_to_vk::vertex_format(
            must_emulate_scaled_formats,
            attrib.attrib_type,
            attrib.size,
        );
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

fn build_vertex_input_state_from_state(
    fixed_state: &FixedPipelineState,
    vs: &CompiledShader,
    must_emulate_scaled_formats: bool,
) -> (
    Vec<vk::VertexInputBindingDescription>,
    Vec<vk::VertexInputBindingDivisorDescriptionEXT>,
    Vec<vk::VertexInputAttributeDescription>,
) {
    if fixed_state.dynamic_vertex_input() {
        return (Vec::new(), Vec::new(), Vec::new());
    }

    let mut bindings = Vec::with_capacity(fixed_state.vertex_strides.len());
    let mut divisors = Vec::new();
    for (index, &stride) in fixed_state.vertex_strides.iter().enumerate() {
        let divisor = fixed_state.binding_divisors[index];
        let input_rate = if divisor != 0 {
            vk::VertexInputRate::INSTANCE
        } else {
            vk::VertexInputRate::VERTEX
        };
        let binding = index as u32;
        bindings.push(vk::VertexInputBindingDescription {
            binding,
            stride: stride as u32,
            input_rate,
        });
        if divisor != 0 {
            divisors.push(vk::VertexInputBindingDivisorDescriptionEXT { binding, divisor });
        }
    }

    let mut attributes = Vec::new();
    for (location, attrib) in fixed_state.attributes.iter().enumerate() {
        let attrib_type = VertexAttribType::from_raw(attrib.attrib_type());
        let attrib_size = VertexAttribSize::from_raw(attrib.attrib_size());
        if std::env::var_os("RUZU_TRACE_VK_VERTEX_INPUT").is_some()
            && vs.info.loads.generic_any(location)
        {
            log::info!(
                "[VK_VERTEX_INPUT_STATE] candidate location={} enabled={} binding={} offset={} raw_type={} raw_size={} dynamic_vertex_input={}",
                location,
                attrib.is_enabled(),
                attrib.buffer(),
                attrib.offset(),
                attrib.attrib_type(),
                attrib.attrib_size(),
                fixed_state.dynamic_vertex_input()
            );
        }
        if !attrib.is_enabled()
            || attrib_size == VertexAttribSize::Invalid
            || attrib_type == VertexAttribType::Invalid
            || !vs.info.loads.generic_any(location)
        {
            continue;
        }
        let format =
            maxwell_to_vk::vertex_format(must_emulate_scaled_formats, attrib_type, attrib_size);
        if format == vk::Format::UNDEFINED {
            warn!(
                "GraphicsPipelineCache: unsupported disk vertex format location={} type={:?} size={:?}",
                location, attrib_type, attrib_size
            );
            continue;
        }
        if std::env::var_os("RUZU_TRACE_VK_VERTEX_INPUT").is_some() {
            log::info!(
                "[VK_VERTEX_INPUT_STATE] emit location={} binding={} offset={} type={:?} size={:?} format={:?}",
                location,
                attrib.buffer(),
                attrib.offset(),
                attrib_type,
                attrib_size,
                format
            );
        }
        attributes.push(vk::VertexInputAttributeDescription {
            location: location as u32,
            binding: attrib.buffer(),
            format,
            offset: attrib.offset(),
        });
    }

    (bindings, divisors, attributes)
}

fn dynamic_states_for_fixed_state(fixed_state: &FixedPipelineState) -> Vec<vk::DynamicState> {
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
        if fixed_state.dynamic_vertex_input() {
            dynamic_states.push(vk::DynamicState::VERTEX_INPUT_EXT);
        }
        dynamic_states.extend([
            vk::DynamicState::CULL_MODE,
            vk::DynamicState::FRONT_FACE,
            vk::DynamicState::VERTEX_INPUT_BINDING_STRIDE,
            vk::DynamicState::DEPTH_TEST_ENABLE,
            vk::DynamicState::DEPTH_WRITE_ENABLE,
            vk::DynamicState::DEPTH_COMPARE_OP,
            vk::DynamicState::DEPTH_BOUNDS_TEST_ENABLE,
            vk::DynamicState::STENCIL_TEST_ENABLE,
            vk::DynamicState::STENCIL_OP,
        ]);
        if fixed_state.extended_dynamic_state_2() {
            dynamic_states.extend([
                vk::DynamicState::DEPTH_BIAS_ENABLE,
                vk::DynamicState::PRIMITIVE_RESTART_ENABLE,
                vk::DynamicState::RASTERIZER_DISCARD_ENABLE,
            ]);
        }
        if fixed_state.extended_dynamic_state_2_extra() {
            dynamic_states.push(vk::DynamicState::LOGIC_OP_EXT);
        }
        if fixed_state.extended_dynamic_state_3_blend() {
            dynamic_states.extend([
                vk::DynamicState::COLOR_BLEND_ENABLE_EXT,
                vk::DynamicState::COLOR_BLEND_EQUATION_EXT,
                vk::DynamicState::COLOR_WRITE_MASK_EXT,
            ]);
        }
        if fixed_state.extended_dynamic_state_3_enables() {
            dynamic_states.extend([
                vk::DynamicState::DEPTH_CLAMP_ENABLE_EXT,
                vk::DynamicState::LOGIC_OP_ENABLE_EXT,
            ]);
        }
    }
    dynamic_states
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
    use crate::engines::maxwell_3d::PrimitiveTopology;
    use ash::vk::Handle;
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
            pipeline: Arc::new(Mutex::new(vk::Pipeline::null())),
            pipeline_layout: vk::PipelineLayout::null(),
            descriptor_set_layout: vk::DescriptorSetLayout::null(),
            descriptor_bindings: Vec::new(),
            descriptor_bank_info: DescriptorBankInfo::default(),
            stage_infos: Default::default(),
            enabled_uniform_buffer_masks: [0; NUM_STAGES as usize],
            uniform_buffer_sizes: [[0; NUM_GRAPHICS_UNIFORM_BUFFERS as usize]; NUM_STAGES as usize],
            uses_render_area: false,
            uses_rescaling_uniform: false,
            shader_modules: [vk::ShaderModule::null(); NUM_VK_GRAPHICS_STAGES],
            build_condvar: Arc::new(Condvar::new()),
            build_mutex: Arc::new(Mutex::new(())),
            is_built: Arc::new(AtomicBool::new(true)),
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
    fn disk_state_topology_matches_upstream_tessellation_fixup() {
        let mut state = FixedPipelineState::default();
        let mut modules = [vk::ShaderModule::null(); NUM_VK_GRAPHICS_STAGES];

        state.set_topology(PrimitiveTopology::Patches);
        assert_eq!(
            input_assembly_topology_for_state(&state, &modules),
            vk::PrimitiveTopology::POINT_LIST
        );

        modules[1] = vk::ShaderModule::from_raw(1);
        assert_eq!(
            input_assembly_topology_for_state(&state, &modules),
            vk::PrimitiveTopology::PATCH_LIST
        );

        state.set_topology(PrimitiveTopology::Triangles);
        assert_eq!(
            input_assembly_topology_for_state(&state, &modules),
            vk::PrimitiveTopology::PATCH_LIST
        );
    }

    #[test]
    fn disk_state_tessellation_patch_control_points_match_upstream() {
        let mut state = FixedPipelineState::default();

        state.set_patch_control_points_minus_one(0);
        assert_eq!(patch_control_points_for_state(&state), 1);

        state.set_patch_control_points_minus_one(31);
        assert_eq!(patch_control_points_for_state(&state), 32);
    }

    #[test]
    fn disk_state_stencil_face_uses_packed_dynamic_state() {
        let mut dynamic = super::super::fixed_pipeline_state::DynamicState::default();
        dynamic.set_stencil_face(
            0,
            crate::engines::maxwell_3d::StencilOp::Replace,
            crate::engines::maxwell_3d::StencilOp::Incr,
            crate::engines::maxwell_3d::StencilOp::Decr,
            crate::engines::maxwell_3d::ComparisonOp::Greater,
        );
        let face = stencil_face_state(dynamic.front_stencil(), dynamic.raw2);
        assert_eq!(face.fail_op, vk::StencilOp::REPLACE);
        assert_eq!(face.depth_fail_op, vk::StencilOp::INCREMENT_AND_WRAP);
        assert_eq!(face.pass_op, vk::StencilOp::DECREMENT_AND_WRAP);
        assert_eq!(face.compare_op, vk::CompareOp::GREATER);
        assert_eq!(face.compare_mask, 0);
        assert_eq!(face.write_mask, 0);
        assert_eq!(face.reference, 0);
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
            conservative_raster_enable: false,
            engine_state: crate::engines::maxwell_3d::EngineHint::None,
            provoking_vertex_last: false,
            depth_bounds_enable: false,
            mandated_early_z: false,
            alpha_test_enabled: false,
            alpha_test_func: crate::engines::maxwell_3d::ComparisonOp::Always,
            alpha_test_ref: 0.0,
            point_size: 1.0,
            tessellation_primitive: 0,
            tessellation_spacing: 0,
            tessellation_clockwise: false,
            patch_vertices: 1,
            anti_alias_samples_mode: 0,
            anti_alias_alpha_control:
                crate::engines::maxwell_3d::AntiAliasAlphaControlInfo::default(),
            line_anti_alias_enable: false,
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
            transform_feedback_enabled: false,
            transform_feedback_state: Default::default(),
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

    #[test]
    fn disk_runtime_info_uses_fixed_pipeline_key_state_like_upstream() {
        let mut fixed_state = FixedPipelineState::default();
        fixed_state.set_topology(PrimitiveTopology::Points);
        fixed_state.set_ndc_minus_one_to_one(true);
        fixed_state.set_early_z(true);
        fixed_state.set_y_negate(true);
        fixed_state.point_size = 1.5f32.to_bits();
        fixed_state.set_alpha_test_func(crate::engines::maxwell_3d::ComparisonOp::Greater);
        fixed_state.alpha_test_ref = 0.25f32.to_bits();
        fixed_state.set_tessellation_primitive(2);
        fixed_state.set_tessellation_spacing(1);
        fixed_state.set_tessellation_clockwise(true);

        let key = GraphicsPipelineKey {
            unique_hashes: [0, 1, 0, 3, 0, 5],
            fixed_state,
        };

        let vertex = make_runtime_info_from_state(&key, ShaderStage::VertexB, None);
        assert_eq!(vertex.fixed_state_point_size, Some(1.5));
        assert!(vertex.convert_depth_mode);
        assert!(vertex.force_early_z);
        assert!(vertex.y_negate);
        assert_eq!(vertex.input_topology, InputTopology::Points);

        let tess = make_runtime_info_from_state(&key, ShaderStage::TessellationEval, None);
        assert_eq!(tess.tess_primitive, TessPrimitive::Quads);
        assert_eq!(tess.tess_spacing, TessSpacing::FractionalOdd);
        assert!(tess.tess_clockwise);

        let fragment = make_runtime_info_from_state(&key, ShaderStage::Fragment, None);
        assert_eq!(fragment.alpha_test_func, Some(CompareFunction::Greater));
        assert_eq!(fragment.alpha_test_reference, 0.25);
    }

    #[test]
    fn state_vertex_input_keeps_upstream_binding_order_and_divisors() {
        let mut fixed_state = FixedPipelineState::default();
        fixed_state.vertex_strides[0] = 0;
        fixed_state.vertex_strides[1] = 16;
        fixed_state.binding_divisors[0] = 3;
        fixed_state.attributes[0].set_enabled(true);
        fixed_state.attributes[0].set_buffer(1);
        fixed_state.attributes[0].set_offset(4);
        fixed_state.attributes[0].set_type(VertexAttribType::Float as u32);
        fixed_state.attributes[0].set_size(VertexAttribSize::R32G32 as u32);

        let mut info = ShaderInfo::default();
        info.loads.set(32, true);
        let shader = CompiledShader {
            spirv_words: Vec::new(),
            info,
            stage: ShaderStage::VertexB,
        };

        let (bindings, divisors, attributes) =
            build_vertex_input_state_from_state(&fixed_state, &shader, false);

        assert_eq!(bindings.len(), 32);
        assert_eq!(bindings[0].binding, 0);
        assert_eq!(bindings[0].stride, 0);
        assert_eq!(bindings[0].input_rate, vk::VertexInputRate::INSTANCE);
        assert_eq!(bindings[1].binding, 1);
        assert_eq!(bindings[1].stride, 16);
        assert_eq!(bindings[1].input_rate, vk::VertexInputRate::VERTEX);
        assert_eq!(divisors.len(), 1);
        assert_eq!(divisors[0].binding, 0);
        assert_eq!(divisors[0].divisor, 3);
        assert_eq!(attributes.len(), 1);
        assert_eq!(attributes[0].location, 0);
        assert_eq!(attributes[0].binding, 1);
        assert_eq!(attributes[0].offset, 4);
    }

    #[test]
    fn state_dynamic_states_follow_upstream_extension_order() {
        let mut fixed_state = FixedPipelineState::default();
        fixed_state.set_extended_dynamic_state(true);
        fixed_state.set_dynamic_vertex_input(true);
        fixed_state.set_extended_dynamic_state_2(true);
        fixed_state.set_extended_dynamic_state_2_extra(true);
        fixed_state.set_extended_dynamic_state_3_blend(true);
        fixed_state.set_extended_dynamic_state_3_enables(true);

        let states = dynamic_states_for_fixed_state(&fixed_state);
        let vertex_input_pos = states
            .iter()
            .position(|&state| state == vk::DynamicState::VERTEX_INPUT_EXT)
            .unwrap();
        let cull_mode_pos = states
            .iter()
            .position(|&state| state == vk::DynamicState::CULL_MODE)
            .unwrap();
        let stride_pos = states
            .iter()
            .position(|&state| state == vk::DynamicState::VERTEX_INPUT_BINDING_STRIDE)
            .unwrap();

        assert!(vertex_input_pos < cull_mode_pos);
        assert!(cull_mode_pos < stride_pos);
        assert!(states.contains(&vk::DynamicState::LOGIC_OP_EXT));
        assert!(states.contains(&vk::DynamicState::COLOR_BLEND_ENABLE_EXT));
        assert!(states.contains(&vk::DynamicState::DEPTH_CLAMP_ENABLE_EXT));
        assert!(states.contains(&vk::DynamicState::LOGIC_OP_ENABLE_EXT));
    }
}
