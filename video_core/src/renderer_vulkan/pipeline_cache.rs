// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `vk_pipeline_cache.h` / `vk_pipeline_cache.cpp`.
//!
//! Manages compilation and caching of both graphics and compute pipelines,
//! including disk serialization of the Vulkan pipeline cache.

use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::path::PathBuf;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};

use ash::vk;
use common::cityhash::city_hash64;
use common::thread_worker::ThreadWorker;

use crate::control::channel_state::ChannelState;
use crate::control::channel_state_cache::{ChannelInfo, ChannelSetupCaches};
use crate::engines::maxwell_3d::DrawCall;
use crate::shader_cache::{GraphicsEnvironments, ShaderCache as SharedShaderCache};
use crate::shader_environment::{
    load_pipelines, serialize_pipeline, ComputeEnvironment, FileEnvironment,
};
use shader_recompiler::backend::bindings::Bindings;
use shader_recompiler::host_translate_info::HostTranslateInfo;
use shader_recompiler::{Profile, RuntimeInfo};

use super::compute_pipeline::ComputePipeline;
use super::fixed_pipeline_state::FixedPipelineState;
use super::graphics_pipeline::{GraphicsPipeline, GraphicsPipelineCache, GraphicsPipelineKey};

/// Diagnostic hit/miss statistics for the graphics pipeline cache
/// (MK8D loading investigation: per-draw shader recompiles during the
/// spinner phase). Prints a `[PIPELINE_STATS]` line every 256 misses.
/// Cheap (GPU-thread only); always on.
pub(crate) mod pipeline_stats {
    use std::sync::atomic::{AtomicU64, Ordering};
    use std::sync::Mutex;

    use super::{FixedPipelineState, GraphicsPipelineKey};

    pub static FAST_HITS: AtomicU64 = AtomicU64::new(0);
    pub static SLOW_HITS: AtomicU64 = AtomicU64::new(0);
    pub static MISSES: AtomicU64 = AtomicU64::new(0);
    static MISS_FIRST: AtomicU64 = AtomicU64::new(0);
    static MISS_HASH_ONLY: AtomicU64 = AtomicU64::new(0);
    static MISS_FIXED_ONLY: AtomicU64 = AtomicU64::new(0);
    static MISS_BOTH: AtomicU64 = AtomicU64::new(0);

    // Per-field change counters for FIXED_ONLY misses (vs the current
    // pipeline's key — i.e. the previous draw).
    static F_RAW1: AtomicU64 = AtomicU64::new(0);
    static F_RAW2: AtomicU64 = AtomicU64::new(0);
    static F_COLOR_FORMATS: AtomicU64 = AtomicU64::new(0);
    static F_ALPHA_POINT: AtomicU64 = AtomicU64::new(0);
    static F_SWIZZLES: AtomicU64 = AtomicU64::new(0);
    static F_ATTR_TYPES: AtomicU64 = AtomicU64::new(0);
    static F_DYNAMIC_STATE: AtomicU64 = AtomicU64::new(0);
    static F_ATTACHMENTS: AtomicU64 = AtomicU64::new(0);
    static F_ATTRIBUTES: AtomicU64 = AtomicU64::new(0);
    static F_DIVISORS_STRIDES: AtomicU64 = AtomicU64::new(0);
    static F_XFB: AtomicU64 = AtomicU64::new(0);

    fn diff_fixed(a: &FixedPipelineState, b: &FixedPipelineState) {
        if a.raw1 != b.raw1 {
            F_RAW1.fetch_add(1, Ordering::Relaxed);
        }
        if a.raw2 != b.raw2 {
            F_RAW2.fetch_add(1, Ordering::Relaxed);
        }
        if a.color_formats != b.color_formats {
            F_COLOR_FORMATS.fetch_add(1, Ordering::Relaxed);
        }
        if a.alpha_test_ref != b.alpha_test_ref || a.point_size != b.point_size {
            F_ALPHA_POINT.fetch_add(1, Ordering::Relaxed);
        }
        if a.viewport_swizzles != b.viewport_swizzles {
            F_SWIZZLES.fetch_add(1, Ordering::Relaxed);
        }
        if a.attribute_types_or_enabled_divisors != b.attribute_types_or_enabled_divisors {
            F_ATTR_TYPES.fetch_add(1, Ordering::Relaxed);
        }
        if a.dynamic_state != b.dynamic_state {
            F_DYNAMIC_STATE.fetch_add(1, Ordering::Relaxed);
        }
        if a.attachments != b.attachments {
            F_ATTACHMENTS.fetch_add(1, Ordering::Relaxed);
        }
        if a.attributes != b.attributes {
            F_ATTRIBUTES.fetch_add(1, Ordering::Relaxed);
        }
        if a.binding_divisors != b.binding_divisors || a.vertex_strides != b.vertex_strides {
            F_DIVISORS_STRIDES.fetch_add(1, Ordering::Relaxed);
        }
        if a.xfb_state != b.xfb_state {
            F_XFB.fetch_add(1, Ordering::Relaxed);
        }
    }

    /// Remember the last missed key so consecutive-miss diffs also work
    /// when the transition cache keeps `current_pipeline` stale.
    static LAST_MISS: Mutex<Option<GraphicsPipelineKey>> = Mutex::new(None);

    /// On a runtime miss, look for cached (typically disk-preloaded) keys
    /// with identical shader hashes and print which FixedPipelineState
    /// fields diverge — pinpoints cross-run key instability. Capped output.
    pub fn log_same_hash_neighbors<'a>(
        key: &GraphicsPipelineKey,
        cached_keys: impl Iterator<Item = &'a GraphicsPipelineKey>,
    ) {
        static PRINTED: AtomicU64 = AtomicU64::new(0);
        if PRINTED.load(Ordering::Relaxed) >= 40 {
            return;
        }
        let mut neighbors = 0u32;
        for cached in cached_keys {
            if cached.unique_hashes != key.unique_hashes {
                continue;
            }
            neighbors += 1;
            if neighbors > 3 {
                continue;
            }
            let a = &key.fixed_state;
            let b = &cached.fixed_state;
            let mut diffs = Vec::new();
            if a.raw1 != b.raw1 {
                diffs.push(format!("raw1 {:08X}!={:08X}", a.raw1, b.raw1));
            }
            if a.raw2 != b.raw2 {
                diffs.push(format!("raw2 {:08X}!={:08X}", a.raw2, b.raw2));
            }
            if a.color_formats != b.color_formats {
                diffs.push(format!(
                    "color_formats {:?}!={:?}",
                    a.color_formats, b.color_formats
                ));
            }
            if a.alpha_test_ref != b.alpha_test_ref {
                diffs.push("alpha_test_ref".into());
            }
            if a.point_size != b.point_size {
                diffs.push("point_size".into());
            }
            if a.viewport_swizzles != b.viewport_swizzles {
                diffs.push("viewport_swizzles".into());
            }
            if a.attribute_types_or_enabled_divisors != b.attribute_types_or_enabled_divisors {
                diffs.push(format!(
                    "attr_types {:016X}!={:016X}",
                    a.attribute_types_or_enabled_divisors, b.attribute_types_or_enabled_divisors
                ));
            }
            if a.dynamic_state != b.dynamic_state {
                diffs.push("dynamic_state".into());
            }
            if a.attachments != b.attachments {
                diffs.push("attachments".into());
            }
            if a.attributes != b.attributes {
                let n = a
                    .attributes
                    .iter()
                    .zip(b.attributes.iter())
                    .filter(|(x, y)| x != y)
                    .count();
                diffs.push(format!("attributes({n} slots)"));
            }
            if a.binding_divisors != b.binding_divisors {
                diffs.push("binding_divisors".into());
            }
            if a.vertex_strides != b.vertex_strides {
                diffs.push("vertex_strides".into());
            }
            if a.xfb_state != b.xfb_state {
                diffs.push("xfb_state".into());
            }
            eprintln!(
                "[PIPELINE_KEY_DIFF] miss vs cached (same hashes {:016X}/{:016X}): {}",
                key.unique_hashes[1],
                key.unique_hashes[5],
                if diffs.is_empty() {
                    "identical?!".to_string()
                } else {
                    diffs.join(", ")
                },
            );
            PRINTED.fetch_add(1, Ordering::Relaxed);
        }
        if neighbors == 0 && PRINTED.fetch_add(1, Ordering::Relaxed) < 40 {
            eprintln!(
                "[PIPELINE_KEY_DIFF] miss with NO cached key sharing hashes {:016X}/{:016X} (genuinely new shaders)",
                key.unique_hashes[1], key.unique_hashes[5],
            );
        }
    }

    pub fn record_fast_hit() {
        FAST_HITS.fetch_add(1, Ordering::Relaxed);
    }

    pub fn record_slow_path(
        is_new: bool,
        key: &GraphicsPipelineKey,
        current: Option<&GraphicsPipelineKey>,
    ) {
        if !is_new {
            SLOW_HITS.fetch_add(1, Ordering::Relaxed);
            return;
        }
        let misses = MISSES.fetch_add(1, Ordering::Relaxed) + 1;

        let mut last_miss = LAST_MISS.lock().unwrap();
        let reference = current.or(last_miss.as_ref());
        match reference {
            None => {
                MISS_FIRST.fetch_add(1, Ordering::Relaxed);
            }
            Some(prev) => {
                let hashes_differ = prev.unique_hashes != key.unique_hashes;
                let fixed_differ = prev.fixed_state != key.fixed_state;
                match (hashes_differ, fixed_differ) {
                    (true, false) => {
                        MISS_HASH_ONLY.fetch_add(1, Ordering::Relaxed);
                    }
                    (false, true) => {
                        MISS_FIXED_ONLY.fetch_add(1, Ordering::Relaxed);
                        diff_fixed(&prev.fixed_state, &key.fixed_state);
                    }
                    _ => {
                        MISS_BOTH.fetch_add(1, Ordering::Relaxed);
                    }
                }
            }
        }
        *last_miss = Some(key.clone());
        drop(last_miss);

        if misses % 256 == 0 {
            eprintln!(
                "[PIPELINE_STATS] fast_hits={} slow_hits={} misses={} (first={} hash_only={} fixed_only={} both={}) \
                 fixed_diffs: raw1={} raw2={} color_fmt={} alpha_point={} swizzle={} attr_types={} dynstate={} \
                 attach={} attribs={} divisors_strides={} xfb={}",
                FAST_HITS.load(Ordering::Relaxed),
                SLOW_HITS.load(Ordering::Relaxed),
                misses,
                MISS_FIRST.load(Ordering::Relaxed),
                MISS_HASH_ONLY.load(Ordering::Relaxed),
                MISS_FIXED_ONLY.load(Ordering::Relaxed),
                MISS_BOTH.load(Ordering::Relaxed),
                F_RAW1.load(Ordering::Relaxed),
                F_RAW2.load(Ordering::Relaxed),
                F_COLOR_FORMATS.load(Ordering::Relaxed),
                F_ALPHA_POINT.load(Ordering::Relaxed),
                F_SWIZZLES.load(Ordering::Relaxed),
                F_ATTR_TYPES.load(Ordering::Relaxed),
                F_DYNAMIC_STATE.load(Ordering::Relaxed),
                F_ATTACHMENTS.load(Ordering::Relaxed),
                F_ATTRIBUTES.load(Ordering::Relaxed),
                F_DIVISORS_STRIDES.load(Ordering::Relaxed),
                F_XFB.load(Ordering::Relaxed),
            );
        }
    }
}
use super::render_pass_cache::RenderPassCache;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CurrentComputePipeline {
    pub pipeline: vk::Pipeline,
    pub requires_descriptor_binding: bool,
}

enum DiskPipelineBuildResult {
    Compute(ComputePipelineCacheKey, ComputePipeline),
    Graphics(GraphicsPipelineKey, GraphicsPipeline),
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

    pub fn read_from_file(file: &mut std::fs::File) -> std::io::Result<Self> {
        use std::io::Read;
        let mut buf8 = [0u8; 8];
        let mut buf4 = [0u8; 4];
        file.read_exact(&mut buf8)?;
        let unique_hash = u64::from_le_bytes(buf8);
        file.read_exact(&mut buf4)?;
        let shared_memory_size = u32::from_le_bytes(buf4);
        let mut workgroup_size = [0u32; 3];
        for value in &mut workgroup_size {
            file.read_exact(&mut buf4)?;
            *value = u32::from_le_bytes(buf4);
        }
        Ok(Self {
            unique_hash,
            shared_memory_size,
            workgroup_size,
        })
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
// Version 12: FixedPipelineState vertex-attribute type/size bits switched
// from Rust enum ordinals to raw Maxwell hardware encodings (matching
// upstream); older caches carry corrupted attribute state.
// Version 13: FixedPipelineState::refresh now leaves fields covered by a
// supported dynamic-state extension at zero (upstream semantics). Older
// caches carry per-draw dynamic state baked into keys — thousands of
// duplicate pipelines per logical key that can never match again.
// Version 14: Maxwell sched-control decoding is anchored at the shader code
// start. Version 13 caches may contain environments captured with the old
// absolute sched grid and therefore rebuild invalid or mismatched pipelines.
// Version 15: FixedPipelineState::refresh preserves color write masks even
// when blending is disabled. Older caches reconstruct pipelines with a zero
// colorWriteMask and can render an entirely black frame.
// Version 16: draw snapshots preserve all 32 vertex binding/attribute slots
// and FixedPipelineState records instance divisors. Version 15 entries can
// contain renumbered sparse attributes and zero divisors.
// Version 17: vertex strides are omitted from fixed pipeline state whenever
// extended dynamic state owns them, matching upstream. Version 16 entries
// can contain per-draw strides and therefore produce duplicate pipelines.
const CACHE_VERSION: u32 = 17;
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
    if draw.zeta.enabled {
        return false;
    }
    draw.index_buffer_count <= 6 || draw.vertex_count <= 6
}

fn graphics_key_dynamic_features_match(
    key: &GraphicsPipelineKey,
    has_extended_dynamic_state: bool,
    has_extended_dynamic_state_2: bool,
    has_extended_dynamic_state_2_extra: bool,
    has_extended_dynamic_state_3_blend: bool,
    has_extended_dynamic_state_3_enables: bool,
    has_dynamic_vertex_input: bool,
) -> bool {
    key.fixed_state.extended_dynamic_state() == has_extended_dynamic_state
        && key.fixed_state.extended_dynamic_state_2() == has_extended_dynamic_state_2
        && key.fixed_state.extended_dynamic_state_2_extra() == has_extended_dynamic_state_2_extra
        && key.fixed_state.extended_dynamic_state_3_blend() == has_extended_dynamic_state_3_blend
        && key.fixed_state.extended_dynamic_state_3_enables()
            == has_extended_dynamic_state_3_enables
        && key.fixed_state.dynamic_vertex_input() == has_dynamic_vertex_input
}

fn graphics_key_supported_for_disk_rebuild(key: &GraphicsPipelineKey) -> Result<(), &'static str> {
    if key.unique_hashes[0] != 0 && key.unique_hashes[1] == 0 {
        return Err("VertexA without VertexB is not supported by the Vulkan shader emitter");
    }
    Ok(())
}

fn graphics_key_cache_hash(key: &GraphicsPipelineKey) -> u64 {
    city_hash64(&key.to_cache_bytes())
}

fn compute_key_to_cache_bytes(key: &ComputePipelineCacheKey) -> Vec<u8> {
    let mut bytes = Vec::with_capacity(std::mem::size_of::<ComputePipelineCacheKey>());
    bytes.extend_from_slice(&key.unique_hash.to_le_bytes());
    bytes.extend_from_slice(&key.shared_memory_size.to_le_bytes());
    for value in key.workgroup_size {
        bytes.extend_from_slice(&value.to_le_bytes());
    }
    bytes
}

fn build_compute_pipeline_from_file_environment(
    device: ash::Device,
    profile: Profile,
    host_info: HostTranslateInfo,
    vulkan_pipeline_cache: vk::PipelineCache,
    key: &ComputePipelineCacheKey,
    env: &mut FileEnvironment,
) -> Option<ComputePipeline> {
    let code = env.cached_instruction_slice().to_vec();
    let base_offset = env.start_address();
    if code.is_empty() {
        return None;
    }
    let mut bindings = Bindings::default();
    let runtime_info = RuntimeInfo::default();
    let compiled = shader_recompiler::compile_shader_from_env_with_bindings_and_host_info(
        &code,
        base_offset,
        env,
        &profile,
        &runtime_info,
        &mut bindings,
        &host_info,
    );
    let create_info = vk::ShaderModuleCreateInfo::builder()
        .code(&compiled.spirv_words)
        .build();
    let spv_module = unsafe { device.create_shader_module(&create_info, None).ok()? };
    ComputePipeline::new(
        device.clone(),
        compiled.info,
        spv_module,
        vulkan_pipeline_cache,
    )
    .or_else(|| {
        log::warn!(
            "Failed to rebuild cached compute pipeline 0x{:016X}",
            key.unique_hash
        );
        None
    })
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

/// Port of upstream `GetTotalPipelineWorkers`.
fn get_total_pipeline_workers() -> usize {
    std::thread::available_parallelism()
        .map(|threads| threads.get())
        .unwrap_or(2)
        .max(2)
        - 1
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
    render_pass_cache: NonNull<RenderPassCache>,
    profile: Profile,
    host_info: HostTranslateInfo,
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
    /// Upstream `Common::ThreadWorker workers`, owned by `PipelineCache`.
    ///
    /// This is the required owner for disk-cache rebuild jobs and async
    /// `GraphicsPipeline` / `ComputePipeline` creation. Current Rust pipeline
    /// constructors are still synchronous, so queuing actual builds is enabled
    /// only after the constructors are split to match upstream.
    workers: ThreadWorker,
    /// Upstream `Common::ThreadWorker serialization_thread`.
    serialization_thread: ThreadWorker,
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
        render_pass_cache: &mut RenderPassCache,
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
        max_vertex_input_bindings: u32,
        vertex_attribute_divisor_supported: bool,
    ) -> Self {
        let mut pipeline_cache = PipelineCache {
            device: device.clone(),
            use_asynchronous_shaders,
            use_vulkan_pipeline_cache,
            channel_caches: ChannelSetupCaches::new(),
            render_pass_cache: NonNull::from(render_pass_cache),
            profile: profile.clone(),
            host_info: HostTranslateInfo::default(),
            graphics_pipeline_cache: GraphicsPipelineCache::new(
                device,
                shader_cache,
                profile,
                extended_dynamic_state_supported,
                extended_dynamic_state2_supported,
                extended_dynamic_state2_extra_supported,
                extended_dynamic_state3_blend_supported,
                extended_dynamic_state3_enables_supported,
                dynamic_vertex_input_supported,
                must_emulate_scaled_formats,
                topology_list_primitive_restart_supported,
                patch_list_primitive_restart_supported,
                max_viewports,
                max_vertex_input_bindings,
                vertex_attribute_divisor_supported,
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
            workers: ThreadWorker::new_stateless(
                get_total_pipeline_workers(),
                "VkPipelineBuilder".to_string(),
            ),
            serialization_thread: ThreadWorker::new_stateless(
                1,
                "VkPipelineSerialization".to_string(),
            ),
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
                pipeline_stats::record_fast_hit();
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

    /// Shared-cache runtime path matching upstream `CurrentComputePipeline`.
    pub fn current_compute_pipeline_with_shared_cache(
        &mut self,
        shared_cache: &mut SharedShaderCache,
    ) -> Option<CurrentComputePipeline> {
        let (shader_hash, shader_size) = {
            let shader = shared_cache.compute_shader()?;
            (shader.unique_hash, shader.size_bytes)
        };
        let kepler_compute = shared_cache.current_kepler_compute()?;
        let qmd = kepler_compute.launch_description();
        let key = ComputePipelineCacheKey {
            unique_hash: shader_hash,
            shared_memory_size: qmd.shared_alloc,
            workgroup_size: [qmd.block_dim_x, qmd.block_dim_y, qmd.block_dim_z],
        };
        if !self.compute_cache.contains_key(&key) {
            let gpu_memory = shared_cache.current_gpu_memory()?;
            let mut env = ComputeEnvironment::from_kepler_compute(kepler_compute, gpu_memory);
            env.generic_environment_mut().set_cached_size(shader_size);
            let pipeline = self.create_compute_pipeline_from_environment(&key, &mut env)?;
            if !self.pipeline_cache_filename.as_os_str().is_empty() {
                let key_bytes = compute_key_to_cache_bytes(&key);
                let filename = self.pipeline_cache_filename.clone();
                let generic_env = env.generic_environment().clone();
                self.serialization_thread.queue_stateless_work(move || {
                    serialize_pipeline(&key_bytes, &[&generic_env], &filename, CACHE_VERSION);
                });
            }
            self.compute_cache.insert(key, pipeline);
        }
        self.compute_cache
            .get(&key)
            .map(|pipeline| CurrentComputePipeline {
                pipeline: pipeline.pipeline(),
                requires_descriptor_binding: pipeline.requires_descriptor_binding(),
            })
    }

    fn create_compute_pipeline_from_environment(
        &mut self,
        key: &ComputePipelineCacheKey,
        env: &mut ComputeEnvironment,
    ) -> Option<ComputePipeline> {
        let code = env
            .generic_environment()
            .cached_instruction_slice()
            .to_vec();
        let base_offset = env.generic_environment().cached_instruction_start();
        if code.is_empty() {
            return None;
        }
        self.create_compute_pipeline_from_code(key, env, &code, base_offset)
    }

    fn create_compute_pipeline_from_code<E>(
        &mut self,
        key: &ComputePipelineCacheKey,
        env: &mut E,
        code: &[u64],
        base_offset: u32,
    ) -> Option<ComputePipeline>
    where
        E: shader_recompiler::environment::Environment,
    {
        let mut bindings = Bindings::default();
        let runtime_info = RuntimeInfo::default();
        let compiled = shader_recompiler::compile_shader_from_env_with_bindings_and_host_info(
            code,
            base_offset,
            env,
            &self.profile,
            &runtime_info,
            &mut bindings,
            &self.host_info,
        );
        let create_info = vk::ShaderModuleCreateInfo::builder()
            .code(&compiled.spirv_words)
            .build();
        let spv_module = unsafe { self.device.create_shader_module(&create_info, None).ok()? };
        ComputePipeline::new_with_worker(
            self.device.clone(),
            compiled.info,
            spv_module,
            self.vulkan_pipeline_cache,
            self.use_asynchronous_shaders.then_some(&self.workers),
        )
        .or_else(|| {
            log::warn!(
                "Failed to rebuild cached compute pipeline 0x{:016X}",
                key.unique_hash
            );
            None
        })
    }

    fn create_compute_pipeline_from_file_environment(
        &mut self,
        key: &ComputePipelineCacheKey,
        env: &mut FileEnvironment,
    ) -> Option<ComputePipeline> {
        let code = env.cached_instruction_slice().to_vec();
        let base_offset = env.start_address();
        if code.is_empty() {
            return None;
        }
        self.create_compute_pipeline_from_code(key, env, &code, base_offset)
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
        pipeline_stats::record_slow_path(is_new, &key, self.current_pipeline.as_ref());
        if is_new {
            pipeline_stats::log_same_hash_neighbors(&key, self.graphics_cache.keys());
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
        let pipeline_cache = self.vulkan_pipeline_cache;
        self.graphics_pipeline_cache.build_pipeline_keyed(
            draw,
            render_pass,
            pipeline_cache,
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
        let pipeline_cache = self.vulkan_pipeline_cache;
        let mut environments = GraphicsEnvironments::default();
        shared_cache.get_graphics_environments(&mut environments, &key.unique_hashes);
        let pipeline = if self.use_asynchronous_shaders {
            self.graphics_pipeline_cache
                .build_pipeline_keyed_from_environments_async(
                    draw,
                    render_pass,
                    pipeline_cache,
                    &mut environments,
                    key,
                    fixed_state,
                    &self.workers,
                )?
        } else {
            self.graphics_pipeline_cache
                .build_pipeline_keyed_from_environments(
                    draw,
                    render_pass,
                    pipeline_cache,
                    &mut environments,
                    key,
                    fixed_state,
                )?
        };
        if !self.pipeline_cache_filename.as_os_str().is_empty() {
            let key_bytes = key.to_cache_bytes();
            let filename = self.pipeline_cache_filename.clone();
            let envs: Vec<_> = environments.span().into_iter().cloned().collect();
            self.serialization_thread.queue_stateless_work(move || {
                let env_refs: Vec<_> = envs.iter().collect();
                serialize_pipeline(&key_bytes, &env_refs, &filename, CACHE_VERSION);
            });
        }
        Some(pipeline)
    }

    fn render_pass_cache(&mut self) -> &mut RenderPassCache {
        unsafe { self.render_pass_cache.as_mut() }
    }

    fn render_pass_for_state(
        &mut self,
        fixed_state: &FixedPipelineState,
    ) -> Option<vk::RenderPass> {
        let key = super::render_pass_cache::RenderPassKey::from_fixed_pipeline_state(fixed_state);
        self.render_pass_cache().get(&key).ok()
    }

    /// Port of `PipelineCache::LoadDiskResources`.
    ///
    /// Loads previously compiled pipelines from disk for the given title.
    pub fn load_disk_resources(&mut self, title_id: u64, pipeline_cache_dir: &std::path::Path) {
        let Some((pipeline_cache_filename, vulkan_pipeline_cache_filename)) =
            pipeline_cache_paths(pipeline_cache_dir, title_id)
        else {
            log::warn!("Skipping Vulkan disk pipeline cache load for title_id=0");
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
        log::info!(
            "Loading Vulkan disk pipeline cache title_id={:016X} file={}",
            title_id,
            self.pipeline_cache_filename.display()
        );

        // Load Vulkan pipeline cache from disk if available
        if self.use_vulkan_pipeline_cache {
            self.vulkan_pipeline_cache =
                self.load_vulkan_pipeline_cache(&self.vulkan_pipeline_cache_filename.clone());
        }

        use std::cell::{Cell, RefCell};

        let total = Cell::new(0usize);
        let mut built = 0usize;
        let skipped = Cell::new(0usize);
        let has_extended_dynamic_state = self
            .graphics_pipeline_cache
            .extended_dynamic_state_supported();
        let has_extended_dynamic_state_2 = self
            .graphics_pipeline_cache
            .extended_dynamic_state2_supported();
        let has_extended_dynamic_state_2_extra = self
            .graphics_pipeline_cache
            .extended_dynamic_state2_extra_supported();
        let has_extended_dynamic_state_3_blend = self
            .graphics_pipeline_cache
            .extended_dynamic_state3_blend_supported();
        let has_extended_dynamic_state_3_enables = self
            .graphics_pipeline_cache
            .extended_dynamic_state3_enables_supported();
        let has_dynamic_vertex_input = self
            .graphics_pipeline_cache
            .dynamic_vertex_input_supported();
        let loaded_compute: RefCell<Vec<(ComputePipelineCacheKey, FileEnvironment)>> =
            RefCell::new(Vec::new());
        let load_compute = |file: &mut std::fs::File, env: FileEnvironment| {
            total.set(total.get() + 1);
            match ComputePipelineCacheKey::read_from_file(file) {
                Ok(key) => {
                    loaded_compute.borrow_mut().push((key, env));
                }
                Err(err) => {
                    skipped.set(skipped.get() + 1);
                    log::warn!("Failed to read cached compute pipeline key: {}", err);
                }
            }
        };
        let loaded_graphics: RefCell<Vec<(GraphicsPipelineKey, Vec<FileEnvironment>)>> =
            RefCell::new(Vec::new());
        let load_graphics = |file: &mut std::fs::File, envs: Vec<FileEnvironment>| {
            total.set(total.get() + 1);
            match GraphicsPipelineKey::read_from_file(file) {
                Ok(key) => {
                    if !graphics_key_dynamic_features_match(
                        &key,
                        has_extended_dynamic_state,
                        has_extended_dynamic_state_2,
                        has_extended_dynamic_state_2_extra,
                        has_extended_dynamic_state_3_blend,
                        has_extended_dynamic_state_3_enables,
                        has_dynamic_vertex_input,
                    ) {
                        skipped.set(skipped.get() + 1);
                        return;
                    }
                    loaded_graphics.borrow_mut().push((key, envs));
                }
                Err(err) => {
                    skipped.set(skipped.get() + 1);
                    log::warn!("Failed to read cached graphics pipeline key: {}", err);
                }
            }
        };
        load_pipelines(
            || false,
            &self.pipeline_cache_filename,
            CACHE_VERSION,
            Box::new(load_compute),
            Box::new(load_graphics),
        );

        let build_results = Arc::new(Mutex::new(Vec::<DiskPipelineBuildResult>::new()));
        let job_skipped = Arc::new(AtomicUsize::new(0));

        let loaded_compute = loaded_compute.into_inner();
        for (key, env) in loaded_compute {
            if self.compute_cache.contains_key(&key) {
                skipped.set(skipped.get() + 1);
                continue;
            }
            let device = self.device.clone();
            let profile = self.profile.clone();
            let host_info = self.host_info.clone();
            let vulkan_pipeline_cache = self.vulkan_pipeline_cache;
            let results = build_results.clone();
            let skipped_jobs = job_skipped.clone();
            self.workers.queue_stateless_work(move || {
                let mut env = env;
                match build_compute_pipeline_from_file_environment(
                    device,
                    profile,
                    host_info,
                    vulkan_pipeline_cache,
                    &key,
                    &mut env,
                ) {
                    Some(pipeline) => results
                        .lock()
                        .unwrap()
                        .push(DiskPipelineBuildResult::Compute(key, pipeline)),
                    None => {
                        skipped_jobs.fetch_add(1, Ordering::Relaxed);
                    }
                }
            });
        }

        let loaded_graphics = loaded_graphics.into_inner();
        for (key, envs) in loaded_graphics {
            if let Err(reason) = graphics_key_supported_for_disk_rebuild(&key) {
                skipped.set(skipped.get() + 1);
                log::debug!(
                    "Skipping cached graphics pipeline 0x{:016X}: {}",
                    graphics_key_cache_hash(&key),
                    reason
                );
                continue;
            }
            if self.graphics_cache.contains_key(&key) {
                skipped.set(skipped.get() + 1);
                continue;
            }
            let Some(render_pass) = self.render_pass_for_state(&key.fixed_state) else {
                skipped.set(skipped.get() + 1);
                log::warn!(
                    "Skipping cached graphics pipeline 0x{:016X}: failed to create render pass",
                    graphics_key_cache_hash(&key)
                );
                continue;
            };
            let mut builder = self.graphics_pipeline_cache.clone_for_disk_worker();
            let vulkan_pipeline_cache = self.vulkan_pipeline_cache;
            let results = build_results.clone();
            let skipped_jobs = job_skipped.clone();
            self.workers.queue_stateless_work(move || {
                let mut envs = envs;
                match builder.build_pipeline_keyed_from_file_environments(
                    render_pass,
                    vulkan_pipeline_cache,
                    &mut envs,
                    &key,
                ) {
                    Some(pipeline) => results
                        .lock()
                        .unwrap()
                        .push(DiskPipelineBuildResult::Graphics(key, pipeline)),
                    None => {
                        skipped_jobs.fetch_add(1, Ordering::Relaxed);
                    }
                }
            });
        }

        self.workers.wait_for_requests();

        let mut skipped_count = skipped.get() + job_skipped.load(Ordering::Relaxed);
        for result in build_results.lock().unwrap().drain(..) {
            match result {
                DiskPipelineBuildResult::Compute(key, pipeline) => {
                    if self.compute_cache.contains_key(&key) {
                        skipped_count += 1;
                    } else {
                        self.compute_cache.insert(key, pipeline);
                        built += 1;
                    }
                }
                DiskPipelineBuildResult::Graphics(key, pipeline) => {
                    if self.graphics_cache.contains_key(&key) {
                        skipped_count += 1;
                    } else {
                        self.graphics_cache.insert(key, pipeline);
                        built += 1;
                    }
                }
            }
        }
        log::info!(
            "Total Pipeline Count: {} (built={}, skipped={})",
            total.get(),
            built,
            skipped_count
        );

        if self.use_vulkan_pipeline_cache {
            self.serialize_vulkan_pipeline_cache(&self.vulkan_pipeline_cache_filename);
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
        // Upstream waits for serialization work through ThreadWorker teardown.
        // Do it explicitly before the final driver-cache serialization so a
        // late `SerializePipeline` job cannot race `vulkan_pipelines.bin`.
        self.serialization_thread.wait_for_requests();

        // Save the pipeline cache before destroying.
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
        AntiAliasAlphaControlInfo, BlendColorInfo, BlendInfo, ColorMaskInfo, ComparisonOp,
        ConstBufferBinding, CullFace, DepthMode, DepthStencilInfo, DrawCall, FrontFace,
        IndexFormat, LogicOpInfo, PolygonMode, PrimitiveTopology, RasterizerInfo, RenderTargetInfo,
        RtControlInfo, SamplerBinding, ScissorInfo, ShaderStageInfo, StencilFaceInfo, ViewportInfo,
        ZetaInfo,
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
            vertex_streams: Default::default(),
            vertex_stream_instances: Default::default(),
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
            conservative_raster_enable: false,
            engine_state: crate::engines::maxwell_3d::EngineHint::None,
            provoking_vertex_last: false,
            depth_bounds_enable: false,
            mandated_early_z: false,
            alpha_test_enabled: false,
            alpha_test_func: ComparisonOp::Always,
            alpha_test_ref: 0.0,
            point_size: 1.0,
            tessellation_primitive: 0,
            tessellation_spacing: 0,
            tessellation_clockwise: false,
            patch_vertices: 1,
            anti_alias_samples_mode: 0,
            anti_alias_alpha_control: AntiAliasAlphaControlInfo::default(),
            line_anti_alias_enable: false,
            program_base_address: 0,
            cb_bindings: [[ConstBufferBinding::default(); 18]; 5],
            vertex_attribs: Default::default(),
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
            zeta: ZetaInfo::default(),
            transform_feedback_enabled: false,
            transform_feedback_state: Default::default(),
            dirty_flags: [false; 256],
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
    fn cache_version_tracks_dynamic_vertex_stride_semantics() {
        assert_eq!(CACHE_VERSION, 17);
    }

    #[test]
    fn should_allow_unbuilt_graphics_pipeline_blocks_depth_usage() {
        let mut draw = make_test_draw_call();
        draw.zeta.enabled = true;
        draw.vertex_count = 4;

        assert!(!should_allow_unbuilt_graphics_pipeline(true, &draw));
    }

    #[test]
    fn should_allow_unbuilt_graphics_pipeline_blocks_bound_zeta_even_without_depth_test() {
        let mut draw = make_test_draw_call();
        draw.zeta.enabled = true;
        draw.depth_stencil.depth_test_enable = false;
        draw.depth_stencil.depth_write_enable = false;
        draw.vertex_count = 4;
        draw.index_buffer_count = 4;

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
    fn graphics_key_dynamic_features_filter_checks_all_upstream_flags() {
        let mut key = GraphicsPipelineKey::default();
        key.fixed_state.set_extended_dynamic_state(true);
        key.fixed_state.set_extended_dynamic_state_2(true);
        key.fixed_state.set_extended_dynamic_state_2_extra(true);
        key.fixed_state.set_extended_dynamic_state_3_blend(true);
        key.fixed_state.set_extended_dynamic_state_3_enables(true);
        key.fixed_state.set_dynamic_vertex_input(true);

        assert!(graphics_key_dynamic_features_match(
            &key, true, true, true, true, true, true,
        ));
        assert!(!graphics_key_dynamic_features_match(
            &key, true, true, false, true, true, true,
        ));
        assert!(!graphics_key_dynamic_features_match(
            &key, true, true, true, false, true, true,
        ));
        assert!(!graphics_key_dynamic_features_match(
            &key, true, true, true, true, false, true,
        ));
        assert!(!graphics_key_dynamic_features_match(
            &key, true, true, true, true, true, false,
        ));
    }

    #[test]
    fn disk_rebuild_accepts_upstream_dual_vertex_key_after_merge_port() {
        let mut key = GraphicsPipelineKey::default();
        key.unique_hashes[0] = 0xA;
        key.unique_hashes[1] = 0xB;

        assert_eq!(graphics_key_supported_for_disk_rebuild(&key), Ok(()));
    }

    #[test]
    fn disk_rebuild_rejects_unmerged_vertex_a_without_vertex_b() {
        let mut key = GraphicsPipelineKey::default();
        key.unique_hashes[0] = 0xA;

        assert_eq!(
            graphics_key_supported_for_disk_rebuild(&key),
            Err("VertexA without VertexB is not supported by the Vulkan shader emitter")
        );
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
    fn total_pipeline_workers_matches_upstream_minimum_policy() {
        let expected = std::thread::available_parallelism()
            .map(|threads| threads.get())
            .unwrap_or(2)
            .max(2)
            - 1;
        assert_eq!(get_total_pipeline_workers(), expected);
        assert!(get_total_pipeline_workers() >= 1);
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
