// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `vk_pipeline_cache.h` / `vk_pipeline_cache.cpp`.
//!
//! Manages compilation and caching of both graphics and compute pipelines,
//! including disk serialization of the Vulkan pipeline cache.

use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::path::PathBuf;

use ash::vk;
use common::cityhash::city_hash64;

use crate::control::channel_state::ChannelState;
use crate::control::channel_state_cache::{ChannelInfo, ChannelSetupCaches};
use crate::engines::maxwell_3d::DrawCall;
use crate::shader_cache::{GraphicsEnvironments, ShaderCache as SharedShaderCache, NUM_PROGRAMS};
use shader_recompiler::Profile;

use super::compute_pipeline::ComputePipeline;
use super::fixed_pipeline_state::FixedPipelineState;
use super::graphics_pipeline::{GraphicsPipeline, GraphicsPipelineCache, GraphicsPipelineKey};

// ---------------------------------------------------------------------------
// ComputePipelineCacheKey
// ---------------------------------------------------------------------------

/// Port of `ComputePipelineCacheKey`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct ComputePipelineCacheKey {
    pub unique_hash: u64,
    pub shared_memory_size: u32,
    pub workgroup_size: [u32; 3],
}

impl ComputePipelineCacheKey {
    /// Port of `ComputePipelineCacheKey::Hash`.
    ///
    /// Computes the upstream-style CityHash64 over the raw key bytes.
    pub fn hash_value(&self) -> u64 {
        let bytes = unsafe {
            std::slice::from_raw_parts(
                (self as *const Self).cast::<u8>(),
                std::mem::size_of::<Self>(),
            )
        };
        city_hash64(bytes)
    }
}

impl Hash for ComputePipelineCacheKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash_value());
    }
}

// ---------------------------------------------------------------------------
// ShaderPools
// ---------------------------------------------------------------------------

/// Port of `ShaderPools` struct.
///
/// Object pools for IR instructions, blocks, and flow blocks.
pub struct ShaderPools {
    _private: (),
}

impl ShaderPools {
    pub fn new() -> Self {
        ShaderPools { _private: () }
    }

    /// Port of `ShaderPools::ReleaseContents`.
    pub fn release_contents(&mut self) {
        // Upstream releases inst, block, flow_block pools.
        // In Rust, the pools would be cleared/dropped here.
    }
}

// ---------------------------------------------------------------------------
// PipelineCache
// ---------------------------------------------------------------------------

/// Vulkan pipeline cache version for disk serialization.
const CACHE_VERSION: u32 = 11;
const VULKAN_CACHE_MAGIC_NUMBER: [u8; 8] = *b"yuzuvkch";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VulkanPipelineCacheHeaderError {
    TooSmall,
    InvalidMagic,
    VersionMismatch,
}

fn parse_vulkan_pipeline_cache_blob(
    data: &[u8],
    expected_cache_version: u32,
) -> Result<&[u8], VulkanPipelineCacheHeaderError> {
    let header_size = VULKAN_CACHE_MAGIC_NUMBER.len() + std::mem::size_of::<u32>();
    if data.len() < header_size {
        return Err(VulkanPipelineCacheHeaderError::TooSmall);
    }

    let magic_number = &data[..VULKAN_CACHE_MAGIC_NUMBER.len()];
    if magic_number != VULKAN_CACHE_MAGIC_NUMBER {
        return Err(VulkanPipelineCacheHeaderError::InvalidMagic);
    }

    let version_offset = VULKAN_CACHE_MAGIC_NUMBER.len();
    let version = u32::from_le_bytes([
        data[version_offset],
        data[version_offset + 1],
        data[version_offset + 2],
        data[version_offset + 3],
    ]);
    if version != expected_cache_version {
        return Err(VulkanPipelineCacheHeaderError::VersionMismatch);
    }

    Ok(&data[header_size..])
}

fn should_allow_unbuilt_graphics_pipeline(use_asynchronous_shaders: bool, draw: &DrawCall) -> bool {
    if !use_asynchronous_shaders {
        return true;
    }
    if draw.depth_stencil.depth_test_enable || draw.depth_stencil.depth_write_enable {
        return false;
    }
    draw.index_buffer_count <= 6 || draw.vertex_count <= 6
}

fn pipeline_cache_paths(
    shader_cache_dir: &std::path::Path,
    title_id: u64,
) -> Option<(PathBuf, PathBuf)> {
    if title_id == 0 {
        return None;
    }
    let base_dir = shader_cache_dir.join(format!("{:016x}", title_id));
    Some((
        base_dir.join("vulkan.bin"),
        base_dir.join("vulkan_pipelines.bin"),
    ))
}

/// Port of `PipelineCache` class.
///
/// Extends `ShaderCache` to manage Vulkan graphics and compute pipeline
/// objects, with disk serialization support.
pub struct PipelineCache {
    device: ash::Device,
    use_asynchronous_shaders: bool,
    use_vulkan_pipeline_cache: bool,
    channel_caches: ChannelSetupCaches<ChannelInfo>,
    graphics_pipeline_cache: GraphicsPipelineCache,
    graphics_cache: HashMap<GraphicsPipelineKey, GraphicsPipeline>,
    failed_graphics_cache: HashSet<GraphicsPipelineKey>,
    graphics_key: GraphicsPipelineKey,
    current_pipeline: Option<GraphicsPipelineKey>,

    main_pools: ShaderPools,

    pipeline_cache_filename: PathBuf,
    vulkan_pipeline_cache_filename: PathBuf,
    vulkan_pipeline_cache: vk::PipelineCache,

    compute_cache: HashMap<ComputePipelineCacheKey, ComputePipeline>,
}

impl PipelineCache {
    fn create_vulkan_pipeline_cache(&self, initial_data: &[u8]) -> vk::PipelineCache {
        let cache_ci = vk::PipelineCacheCreateInfo::builder()
            .initial_data(initial_data)
            .build();
        unsafe {
            self.device
                .create_pipeline_cache(&cache_ci, None)
                .unwrap_or(vk::PipelineCache::null())
        }
    }

    fn create_empty_vulkan_pipeline_cache(&self) -> vk::PipelineCache {
        self.create_vulkan_pipeline_cache(&[])
    }

    /// Port of `PipelineCache::PipelineCache`.
    pub fn new(
        device: ash::Device,
        use_asynchronous_shaders: bool,
        use_vulkan_pipeline_cache: bool,
        shader_cache: shader_recompiler::PipelineCache,
        profile: Profile,
        extended_dynamic_state_supported: bool,
        extended_dynamic_state2_supported: bool,
        topology_list_primitive_restart_supported: bool,
        patch_list_primitive_restart_supported: bool,
        max_viewports: u32,
    ) -> Self {
        let mut pipeline_cache = PipelineCache {
            device: device.clone(),
            use_asynchronous_shaders,
            use_vulkan_pipeline_cache,
            channel_caches: ChannelSetupCaches::new(),
            graphics_pipeline_cache: GraphicsPipelineCache::new(
                device,
                shader_cache,
                profile,
                extended_dynamic_state_supported,
                extended_dynamic_state2_supported,
                topology_list_primitive_restart_supported,
                patch_list_primitive_restart_supported,
                max_viewports,
            ),
            graphics_cache: HashMap::new(),
            failed_graphics_cache: HashSet::new(),
            graphics_key: GraphicsPipelineKey::default(),
            current_pipeline: None,
            main_pools: ShaderPools::new(),
            pipeline_cache_filename: PathBuf::new(),
            vulkan_pipeline_cache_filename: PathBuf::new(),
            vulkan_pipeline_cache: vk::PipelineCache::null(),
            compute_cache: HashMap::new(),
        };
        pipeline_cache.vulkan_pipeline_cache = if use_vulkan_pipeline_cache {
            pipeline_cache.create_empty_vulkan_pipeline_cache()
        } else {
            vk::PipelineCache::null()
        };
        pipeline_cache
    }

    /// Port of the Vulkan pipeline-cache owner `CreateChannel` edge.
    pub fn create_channel(&mut self, channel: &ChannelState) {
        self.channel_caches.create_channel(channel);
    }

    /// Port of the Vulkan pipeline-cache owner `BindToChannel` edge.
    pub fn bind_to_channel(&mut self, channel_id: i32) {
        self.channel_caches.bind_to_channel(channel_id);
    }

    /// Port of the Vulkan pipeline-cache owner `EraseChannel` edge.
    pub fn erase_channel(&mut self, channel_id: i32) {
        self.channel_caches.erase_channel(channel_id);
    }

    /// Port of `PipelineCache::CurrentGraphicsPipeline`.
    ///
    /// Returns the current graphics pipeline for the bound shader state.
    /// May trigger compilation if the pipeline has not been built yet.
    pub fn current_graphics_pipeline(
        &mut self,
        draw: &DrawCall,
        render_pass: vk::RenderPass,
        read_gpu: &dyn Fn(u64, &mut [u8]),
    ) -> Option<(&GraphicsPipeline, FixedPipelineState)> {
        let (key, fixed_state) = self.graphics_pipeline_cache.make_key(draw, read_gpu)?;
        self.graphics_key = key.clone();

        if let Some(current_key) = self.current_pipeline.clone() {
            let next_key = self
                .graphics_cache
                .get(&current_key)
                .and_then(|pipeline| pipeline.next(&key).cloned());
            if let Some(next_key) = next_key {
                self.current_pipeline = Some(next_key.clone());
                let pipeline = self
                    .graphics_cache
                    .get(&next_key)
                    .expect("graphics cache transition entry vanished before lookup");
                return self
                    .built_pipeline(pipeline, draw)
                    .map(|pipeline| (pipeline, fixed_state));
            }
        }

        self.current_graphics_pipeline_slow_path(draw, render_pass, read_gpu, key, fixed_state)
    }

    /// Shared-cache runtime path matching upstream's pipeline-cache ownership:
    /// shader discovery and unique hashes come from `VideoCommon::ShaderCache`,
    /// while this Vulkan owner builds/caches the VkPipeline.
    pub fn current_graphics_pipeline_with_shared_cache(
        &mut self,
        draw: &DrawCall,
        render_pass: vk::RenderPass,
        shared_cache: &mut SharedShaderCache,
    ) -> Option<(&GraphicsPipeline, FixedPipelineState)> {
        let mut unique_hashes = self.graphics_key.unique_hashes;
        if !shared_cache.refresh_stages(&mut unique_hashes) {
            self.current_pipeline = None;
            return None;
        }
        let (key, fixed_state) = self
            .graphics_pipeline_cache
            .make_key_from_unique_hashes(draw, unique_hashes);
        self.graphics_key = key.clone();

        if let Some(current_key) = self.current_pipeline.clone() {
            let next_key = self
                .graphics_cache
                .get(&current_key)
                .and_then(|pipeline| pipeline.next(&key).cloned());
            if let Some(next_key) = next_key {
                self.current_pipeline = Some(next_key.clone());
                let pipeline = self
                    .graphics_cache
                    .get(&next_key)
                    .expect("graphics cache transition entry vanished before lookup");
                return self
                    .built_pipeline(pipeline, draw)
                    .map(|pipeline| (pipeline, fixed_state));
            }
        }

        self.current_graphics_pipeline_slow_path_with_shared_cache(
            draw,
            render_pass,
            shared_cache,
            key,
            fixed_state,
        )
    }

    /// Port of `PipelineCache::CurrentComputePipeline`.
    ///
    /// Returns the current compute pipeline for the bound compute shader.
    pub fn current_compute_pipeline(&mut self) -> Option<vk::Pipeline> {
        // In the full implementation, this looks up the current compute
        // shader state and returns the cached pipeline.
        None
    }

    /// Port of `PipelineCache::CurrentGraphicsPipelineSlowPath`.
    fn current_graphics_pipeline_slow_path(
        &mut self,
        draw: &DrawCall,
        render_pass: vk::RenderPass,
        read_gpu: &dyn Fn(u64, &mut [u8]),
        key: GraphicsPipelineKey,
        fixed_state: FixedPipelineState,
    ) -> Option<(&GraphicsPipeline, FixedPipelineState)> {
        if self.failed_graphics_cache.contains(&key) {
            return None;
        }

        let is_new = !self.graphics_cache.contains_key(&key);
        if is_new {
            let Some(pipeline) =
                self.create_graphics_pipeline(draw, render_pass, read_gpu, &key, &fixed_state)
            else {
                self.failed_graphics_cache.insert(key);
                return None;
            };
            self.graphics_cache.insert(key.clone(), pipeline);
        }

        if is_new {
            if let Some(current_key) = self.current_pipeline.clone() {
                if current_key != key {
                    if let Some(current_pipeline) = self.graphics_cache.get_mut(&current_key) {
                        current_pipeline.add_transition(key.clone());
                    }
                }
            }
        }

        self.current_pipeline = Some(key.clone());
        let pipeline = self.graphics_cache.get(&key)?;
        self.built_pipeline(pipeline, draw)
            .map(|pipeline| (pipeline, fixed_state))
    }

    fn current_graphics_pipeline_slow_path_with_shared_cache(
        &mut self,
        draw: &DrawCall,
        render_pass: vk::RenderPass,
        shared_cache: &SharedShaderCache,
        key: GraphicsPipelineKey,
        fixed_state: FixedPipelineState,
    ) -> Option<(&GraphicsPipeline, FixedPipelineState)> {
        if self.failed_graphics_cache.contains(&key) {
            return None;
        }

        let is_new = !self.graphics_cache.contains_key(&key);
        if is_new {
            let Some(pipeline) = self.create_graphics_pipeline_with_shared_cache(
                draw,
                render_pass,
                shared_cache,
                &key,
                &fixed_state,
            ) else {
                self.failed_graphics_cache.insert(key);
                return None;
            };
            self.graphics_cache.insert(key.clone(), pipeline);
        }

        if is_new {
            if let Some(current_key) = self.current_pipeline.clone() {
                if current_key != key {
                    if let Some(current_pipeline) = self.graphics_cache.get_mut(&current_key) {
                        current_pipeline.add_transition(key.clone());
                    }
                }
            }
        }

        self.current_pipeline = Some(key.clone());
        let pipeline = self.graphics_cache.get(&key)?;
        self.built_pipeline(pipeline, draw)
            .map(|pipeline| (pipeline, fixed_state))
    }

    /// Port of `PipelineCache::BuiltPipeline`.
    fn built_pipeline<'a>(
        &self,
        pipeline: &'a GraphicsPipeline,
        draw: &DrawCall,
    ) -> Option<&'a GraphicsPipeline> {
        if pipeline.is_built() {
            return Some(pipeline);
        }
        if should_allow_unbuilt_graphics_pipeline(self.use_asynchronous_shaders, draw) {
            return Some(pipeline);
        }
        None
    }

    /// Reduced port of `PipelineCache::CreateGraphicsPipeline`.
    fn create_graphics_pipeline(
        &mut self,
        draw: &DrawCall,
        render_pass: vk::RenderPass,
        read_gpu: &dyn Fn(u64, &mut [u8]),
        key: &GraphicsPipelineKey,
        fixed_state: &FixedPipelineState,
    ) -> Option<GraphicsPipeline> {
        self.main_pools.release_contents();
        self.graphics_pipeline_cache.build_pipeline_keyed(
            draw,
            render_pass,
            read_gpu,
            key,
            fixed_state,
        )
    }

    fn create_graphics_pipeline_with_shared_cache(
        &mut self,
        draw: &DrawCall,
        render_pass: vk::RenderPass,
        shared_cache: &SharedShaderCache,
        key: &GraphicsPipelineKey,
        fixed_state: &FixedPipelineState,
    ) -> Option<GraphicsPipeline> {
        self.main_pools.release_contents();
        let mut environments = GraphicsEnvironments::default();
        shared_cache.get_graphics_environments(&mut environments, &key.unique_hashes);
        self.graphics_pipeline_cache
            .build_pipeline_keyed_from_environments(
                draw,
                render_pass,
                &mut environments,
                key,
                fixed_state,
            )
    }

    /// Port of `PipelineCache::LoadDiskResources`.
    ///
    /// Loads previously compiled pipelines from disk for the given title.
    pub fn load_disk_resources(&mut self, title_id: u64, pipeline_cache_dir: &std::path::Path) {
        let Some((pipeline_cache_filename, vulkan_pipeline_cache_filename)) =
            pipeline_cache_paths(pipeline_cache_dir, title_id)
        else {
            return;
        };

        let base_dir = pipeline_cache_filename
            .parent()
            .expect("pipeline cache path should have a parent directory");
        if let Err(err) = std::fs::create_dir_all(base_dir) {
            log::error!("Failed to create pipeline cache directories: {}", err);
            return;
        }

        self.pipeline_cache_filename = pipeline_cache_filename;
        self.vulkan_pipeline_cache_filename = vulkan_pipeline_cache_filename;

        // Load Vulkan pipeline cache from disk if available
        if self.use_vulkan_pipeline_cache {
            self.vulkan_pipeline_cache =
                self.load_vulkan_pipeline_cache(&self.vulkan_pipeline_cache_filename.clone());
        }
    }

    /// Port of `PipelineCache::SerializeVulkanPipelineCache`.
    ///
    /// Serializes the Vulkan pipeline cache to disk.
    pub fn serialize_vulkan_pipeline_cache(&self, filename: &std::path::Path) {
        let data = unsafe {
            self.device
                .get_pipeline_cache_data(self.vulkan_pipeline_cache)
                .unwrap_or_default()
        };

        let mut output = Vec::with_capacity(VULKAN_CACHE_MAGIC_NUMBER.len() + 4 + data.len());
        output.extend_from_slice(&VULKAN_CACHE_MAGIC_NUMBER);
        output.extend_from_slice(&CACHE_VERSION.to_le_bytes());
        output.extend_from_slice(&data);

        if let Err(e) = std::fs::write(filename, &output) {
            log::error!("Failed to write Vulkan pipeline cache: {}", e);
        }
    }

    /// Port of loading Vulkan pipeline cache from disk.
    fn load_vulkan_pipeline_cache(&self, filename: &std::path::Path) -> vk::PipelineCache {
        let data = match std::fs::read(filename) {
            Ok(data) => data,
            Err(_) => return self.create_empty_vulkan_pipeline_cache(),
        };

        let cache_data = match parse_vulkan_pipeline_cache_blob(&data, CACHE_VERSION) {
            Ok(cache_data) => cache_data,
            Err(VulkanPipelineCacheHeaderError::TooSmall) => {
                let _ = std::fs::remove_file(filename);
                return self.create_empty_vulkan_pipeline_cache();
            }
            Err(VulkanPipelineCacheHeaderError::InvalidMagic) => {
                log::error!("Invalid Vulkan driver pipeline cache file");
                let _ = std::fs::remove_file(filename);
                return self.create_empty_vulkan_pipeline_cache();
            }
            Err(VulkanPipelineCacheHeaderError::VersionMismatch) => {
                log::info!(
                    "Pipeline cache version mismatch (expected {}, got {}), discarding",
                    CACHE_VERSION,
                    u32::from_le_bytes([
                        data[VULKAN_CACHE_MAGIC_NUMBER.len()],
                        data[VULKAN_CACHE_MAGIC_NUMBER.len() + 1],
                        data[VULKAN_CACHE_MAGIC_NUMBER.len() + 2],
                        data[VULKAN_CACHE_MAGIC_NUMBER.len() + 3],
                    ])
                );
                let _ = std::fs::remove_file(filename);
                return self.create_empty_vulkan_pipeline_cache();
            }
        };

        self.create_vulkan_pipeline_cache(cache_data)
    }
}

impl Drop for PipelineCache {
    fn drop(&mut self) {
        // Save the pipeline cache before destroying
        if self.use_vulkan_pipeline_cache && self.vulkan_pipeline_cache != vk::PipelineCache::null()
        {
            let filename = self.vulkan_pipeline_cache_filename.clone();
            if !filename.as_os_str().is_empty() {
                self.serialize_vulkan_pipeline_cache(&filename);
            }
            unsafe {
                self.device
                    .destroy_pipeline_cache(self.vulkan_pipeline_cache, None);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::engines::maxwell_3d::{
        BlendColorInfo, BlendInfo, ColorMaskInfo, ComparisonOp, ConstBufferBinding, CullFace,
        DepthMode, DepthStencilInfo, DrawCall, FrontFace, IndexFormat, PolygonMode,
        PrimitiveTopology, RasterizerInfo, RenderTargetInfo, RtControlInfo, SamplerBinding,
        ScissorInfo, ShaderStageInfo, StencilFaceInfo, ViewportInfo,
    };

    fn make_test_draw_call() -> DrawCall {
        DrawCall {
            topology: PrimitiveTopology::Triangles,
            vertex_first: 0,
            vertex_count: 0,
            indexed: false,
            index_buffer_addr: 0,
            index_buffer_count: 0,
            index_buffer_first: 0,
            index_format: IndexFormat::UnsignedInt,
            vertex_streams: Vec::new(),
            vertex_stream_limits: Default::default(),
            viewports: [ViewportInfo::default(); 16],
            viewport_transforms: Default::default(),
            scissors: [ScissorInfo::default(); 16],
            viewport_scale_offset_enabled: false,
            window_origin_lower_left: false,
            window_origin_flip_y: false,
            surface_clip: Default::default(),
            blend: [BlendInfo::default(); 8],
            blend_color: BlendColorInfo {
                r: 0.0,
                g: 0.0,
                b: 0.0,
                a: 0.0,
            },
            depth_stencil: DepthStencilInfo {
                depth_test_enable: false,
                depth_write_enable: false,
                depth_func: ComparisonOp::Always,
                depth_mode: DepthMode::MinusOneToOne,
                stencil_enable: false,
                stencil_two_side: false,
                front: StencilFaceInfo::default(),
                back: StencilFaceInfo::default(),
            },
            rasterizer: RasterizerInfo {
                cull_enable: false,
                front_face: FrontFace::CCW,
                cull_face: CullFace::Back,
                polygon_mode_front: PolygonMode::Fill,
                polygon_mode_back: PolygonMode::Fill,
                line_width_smooth: 1.0,
                line_width_aliased: 1.0,
                depth_bias: 0.0,
                slope_scale_depth_bias: 0.0,
                depth_bias_clamp: 0.0,
                ..RasterizerInfo::default()
            },
            rasterize_enable: true,
            primitive_restart: Default::default(),
            logic_op: LogicOpInfo::default(),
            depth_clamp_enabled: true,
            program_base_address: 0,
            cb_bindings: [[ConstBufferBinding::default(); 18]; 5],
            vertex_attribs: Vec::new(),
            shader_stages: [ShaderStageInfo::default(); 6],
            color_masks: [ColorMaskInfo::default(); 8],
            rt_control: RtControlInfo::default(),
            tex_header_pool_addr: 0,
            tex_header_pool_limit: 0,
            tex_sampler_pool_addr: 0,
            tex_sampler_pool_limit: 0,
            instance_count: 1,
            base_instance: 0,
            base_vertex: 0,
            inline_index_data: Vec::new(),
            sampler_binding: SamplerBinding::Independently,
            render_targets: [RenderTargetInfo::default(); 8],
        }
    }

    #[test]
    fn parse_vulkan_pipeline_cache_blob_accepts_upstream_header() {
        let mut blob = Vec::new();
        blob.extend_from_slice(&VULKAN_CACHE_MAGIC_NUMBER);
        blob.extend_from_slice(&CACHE_VERSION.to_le_bytes());
        blob.extend_from_slice(&[1, 2, 3, 4]);

        let payload = parse_vulkan_pipeline_cache_blob(&blob, CACHE_VERSION)
            .expect("upstream-shaped header should parse");
        assert_eq!(payload, &[1, 2, 3, 4]);
    }

    #[test]
    fn parse_vulkan_pipeline_cache_blob_rejects_invalid_magic() {
        let mut blob = Vec::new();
        blob.extend_from_slice(b"badmagic");
        blob.extend_from_slice(&CACHE_VERSION.to_le_bytes());

        let result = parse_vulkan_pipeline_cache_blob(&blob, CACHE_VERSION);
        assert_eq!(result, Err(VulkanPipelineCacheHeaderError::InvalidMagic));
    }

    #[test]
    fn parse_vulkan_pipeline_cache_blob_rejects_version_mismatch() {
        let mut blob = Vec::new();
        blob.extend_from_slice(&VULKAN_CACHE_MAGIC_NUMBER);
        blob.extend_from_slice(&(CACHE_VERSION - 1).to_le_bytes());

        let result = parse_vulkan_pipeline_cache_blob(&blob, CACHE_VERSION);
        assert_eq!(result, Err(VulkanPipelineCacheHeaderError::VersionMismatch));
    }

    #[test]
    fn should_allow_unbuilt_graphics_pipeline_blocks_depth_usage() {
        let mut draw = make_test_draw_call();
        draw.depth_stencil.depth_test_enable = true;
        draw.vertex_count = 4;

        assert!(!should_allow_unbuilt_graphics_pipeline(true, &draw));
    }

    #[test]
    fn should_allow_unbuilt_graphics_pipeline_allows_small_draw_without_depth() {
        let mut draw = make_test_draw_call();
        draw.vertex_count = 4;
        draw.index_buffer_count = 4;

        assert!(should_allow_unbuilt_graphics_pipeline(true, &draw));
    }

    #[test]
    fn pipeline_cache_paths_match_upstream_vulkan_names() {
        let root = std::path::Path::new("/tmp/shader");
        let (pipeline, driver) = pipeline_cache_paths(root, 0x0102030405060708).unwrap();
        assert_eq!(pipeline, root.join("0102030405060708").join("vulkan.bin"));
        assert_eq!(
            driver,
            root.join("0102030405060708").join("vulkan_pipelines.bin")
        );
    }

    #[test]
    fn pipeline_cache_paths_skip_zero_title_id() {
        assert!(pipeline_cache_paths(std::path::Path::new("/tmp/shader"), 0).is_none());
    }

    #[test]
    fn compute_pipeline_cache_key_hash_changes_with_shared_memory_size() {
        let key_a = ComputePipelineCacheKey {
            unique_hash: 0x1234,
            shared_memory_size: 0x20,
            workgroup_size: [1, 2, 3],
        };
        let key_b = ComputePipelineCacheKey {
            shared_memory_size: 0x40,
            ..key_a
        };

        assert_ne!(key_a.hash_value(), key_b.hash_value());
    }

    #[test]
    fn compute_pipeline_cache_key_hash_changes_with_workgroup_size() {
        let key_a = ComputePipelineCacheKey {
            unique_hash: 0x1234,
            shared_memory_size: 0x20,
            workgroup_size: [1, 2, 3],
        };
        let key_b = ComputePipelineCacheKey {
            workgroup_size: [1, 2, 4],
            ..key_a
        };

        assert_ne!(key_a.hash_value(), key_b.hash_value());
    }

    #[test]
    fn compute_pipeline_cache_key_layout_matches_upstream_shape() {
        assert_eq!(std::mem::size_of::<ComputePipelineCacheKey>(), 24);
        assert_eq!(std::mem::align_of::<ComputePipelineCacheKey>(), 8);
    }
}
