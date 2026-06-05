// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_shader_cache.h and gl_shader_cache.cpp
//!
//! OpenGL shader cache -- manages compilation and caching of graphics and compute pipelines.

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;

use common::{cityhash::city_hash64, trace};
use shader_recompiler::profile::Profile as ShaderProfile;
use shader_recompiler::runtime_info::{
    CompareFunction, InputTopology, RuntimeInfo, TessPrimitive, TessSpacing,
};
use shader_recompiler::shader_info::Info as ShaderInfo;
use shader_recompiler::{
    compile_dual_vertex_shader_glsl_at_offset_with_bindings,
    compile_shader_glsl_at_offset_with_bindings,
    compile_shader_glsl_at_offset_with_bindings_and_texture_bound,
    compile_shader_glsl_at_offset_with_bindings_and_texture_bound_and_sph, CompiledGlslShader,
    ShaderStage,
};

use crate::shader_cache::{GraphicsEnvironments, ShaderCache as SharedShaderCache};
use crate::shader_environment::{GenericEnvironment, GpuMemoryReader};
use crate::transform_feedback;
use shader_recompiler::program_header::ProgramHeader;

fn comparison_op_to_runtime_func(op: crate::engines::maxwell_3d::ComparisonOp) -> CompareFunction {
    match op {
        crate::engines::maxwell_3d::ComparisonOp::Never => CompareFunction::Never,
        crate::engines::maxwell_3d::ComparisonOp::Less => CompareFunction::Less,
        crate::engines::maxwell_3d::ComparisonOp::Equal => CompareFunction::Equal,
        crate::engines::maxwell_3d::ComparisonOp::LessEqual => CompareFunction::LessThanEqual,
        crate::engines::maxwell_3d::ComparisonOp::Greater => CompareFunction::Greater,
        crate::engines::maxwell_3d::ComparisonOp::NotEqual => CompareFunction::NotEqual,
        crate::engines::maxwell_3d::ComparisonOp::GreaterEqual => CompareFunction::GreaterThanEqual,
        crate::engines::maxwell_3d::ComparisonOp::Always => CompareFunction::Always,
    }
}

fn comparison_func_to_key(func: CompareFunction) -> u32 {
    match func {
        CompareFunction::Never => 0,
        CompareFunction::Less => 1,
        CompareFunction::Equal => 2,
        CompareFunction::LessThanEqual => 3,
        CompareFunction::Greater => 4,
        CompareFunction::NotEqual => 5,
        CompareFunction::GreaterThanEqual => 6,
        CompareFunction::Always => 7,
    }
}

fn comparison_func_from_key(value: u32) -> CompareFunction {
    match value {
        0 => CompareFunction::Never,
        1 => CompareFunction::Less,
        2 => CompareFunction::Equal,
        3 => CompareFunction::LessThanEqual,
        4 => CompareFunction::Greater,
        5 => CompareFunction::NotEqual,
        6 => CompareFunction::GreaterThanEqual,
        _ => CompareFunction::Always,
    }
}

fn parse_hash_env(name: &str) -> Option<u64> {
    std::env::var(name).ok().and_then(|value| {
        let value = value.trim();
        let hex = value
            .strip_prefix("0x")
            .or_else(|| value.strip_prefix("0X"));
        match hex {
            Some(hex) => u64::from_str_radix(hex, 16).ok(),
            None => value.parse::<u64>().ok(),
        }
    })
}

fn patch_fragment_debug_by_source_hash(stage: ShaderStage, source: String) -> String {
    if stage != ShaderStage::Fragment {
        return source;
    }
    let Some(target_hash) = parse_hash_env("RUZU_FRAGMENT_DEBUG_SOURCE_HASH") else {
        return source;
    };
    let source_hash = city_hash64(source.as_bytes());
    if source_hash != target_hash {
        return source;
    }
    let Some(main_index) = source.rfind("void main(){") else {
        log::warn!(
            "[FRAGMENT_DEBUG_HASH] hash=0x{:016X} matched but no main body was found",
            source_hash
        );
        return source;
    };

    let mode =
        std::env::var("RUZU_FRAGMENT_DEBUG_HASH_MODE").unwrap_or_else(|_| "green".to_string());
    let replacement = match mode.as_str() {
        "tex0_center" => {
            "void main(){\nfrag_color0=texture(tex0,vec2(0.5,0.5));\nreturn;\n}\n"
        }
        "tex0_attr1" => "void main(){\nfrag_color0=texture(tex0,in_attr1.xy);\nreturn;\n}\n",
        "uv" => {
            "void main(){\nvec2 ruzu_uv=in_attr1.xy;\nfrag_color0=vec4(fract(ruzu_uv),0.0,1.0);\nreturn;\n}\n"
        }
        "alpha_one" => {
            "void main(){\nvec4 c=texture(tex0,in_attr1.xy);\nfrag_color0=vec4(c.rgb,1.0);\nreturn;\n}\n"
        }
        "red" => "void main(){\nfrag_color0=vec4(1.0,0.0,0.0,1.0);\nreturn;\n}\n",
        _ => "void main(){\nfrag_color0=vec4(0.0,1.0,0.0,1.0);\nreturn;\n}\n",
    };
    log::warn!(
        "[FRAGMENT_DEBUG_HASH] hash=0x{:016X} mode={} patched fragment shader",
        source_hash,
        mode
    );
    format!("{}{}", &source[..main_index], replacement)
}

use super::gl_compute_pipeline::{ComputePipeline, ComputePipelineKey};
use super::gl_device::Device;
use super::gl_graphics_pipeline::{GraphicsPipeline, GraphicsPipelineKey};

/// Cache version for serialized pipeline data.
pub const CACHE_VERSION: u32 = 10;

static SHADER_PIPELINE_LAST_STAGE: AtomicU64 = AtomicU64::new(0);
static GL_PIPELINE_TRACE_OBSERVED: AtomicU64 = AtomicU64::new(0);
static GL_PIPELINE_TRACE_SEQ: AtomicU64 = AtomicU64::new(0);
static SHADER_PIPELINE_STAGE_COUNTS: [AtomicU64; 12] = [
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
    AtomicU64::new(0),
];

fn trace_gl_pipeline(
    stage: u64,
    key: &GraphicsPipelineKey,
    cache_len: usize,
    aux0: u64,
    aux1: u64,
    aux2: u64,
) {
    if !trace::is_enabled(trace::cat::GL_PIPELINE) {
        return;
    }
    if matches!(stage, 1 | 2) && std::env::var_os("RUZU_TRACE_GL_PIPELINE_HITS").is_none() {
        return;
    }
    let observed = GL_PIPELINE_TRACE_OBSERVED.fetch_add(1, Ordering::Relaxed);
    let skip = std::env::var("RUZU_TRACE_GL_PIPELINE_SKIP")
        .ok()
        .and_then(|value| value.parse::<u64>().ok())
        .unwrap_or(0);
    if observed < skip {
        return;
    }
    let seq = GL_PIPELINE_TRACE_SEQ.fetch_add(1, Ordering::Relaxed);
    let _ = trace::emit_raw(
        trace::cat::GL_PIPELINE,
        &[
            stage,
            seq,
            cache_len as u64,
            key.raw as u64,
            key.hash_key(),
            key.unique_hashes[0],
            key.unique_hashes[1],
            key.unique_hashes[2],
            key.unique_hashes[3],
            key.unique_hashes[4],
            key.unique_hashes[5],
            aux0,
            aux1,
            aux2,
        ],
    );
}

/// Port of the OpenGL-specific `Shader::Profile` construction in upstream
/// `gl_shader_cache.cpp`.
fn opengl_shader_profile(device: &Device) -> ShaderProfile {
    ShaderProfile {
        support_int64: device.has_shader_int64(),
        support_vertex_instance_id: true,
        support_vote: true,
        support_viewport_index_layer_non_geometry: device.has_nv_viewport_array2()
            || device.has_vertex_viewport_layer(),
        support_viewport_mask: device.has_nv_viewport_array2(),
        support_typeless_image_loads: device.has_image_load_formatted(),
        support_demote_to_helper_invocation: false,
        support_derivative_control: device.has_derivative_control(),
        support_geometry_shader_passthrough: device.has_geometry_shader_passthrough(),
        support_native_ndc: true,
        support_gl_nv_gpu_shader_5: device.has_nv_gpu_shader5(),
        support_gl_amd_gpu_shader_half_float: device.has_amd_shader_half_float(),
        support_gl_texture_shadow_lod: device.has_texture_shadow_lod(),
        support_gl_warp_intrinsics: false,
        support_gl_variable_aoffi: device.has_variable_aoffi(),
        support_gl_sparse_textures: device.has_sparse_texture2(),
        support_gl_derivative_control: device.has_derivative_control(),
        support_geometry_streams: true,
        warp_size_potentially_larger_than_guest: device
            .is_warp_size_potentially_larger_than_guest(),
        lower_left_origin_mode: true,
        need_declared_frag_colors: true,
        need_fastmath_off: device.needs_fastmath_off(),
        need_gather_subpixel_offset: device.is_amd() || device.is_intel(),
        has_broken_spirv_clamp: true,
        has_broken_unsigned_image_offsets: true,
        has_broken_signed_operations: true,
        has_broken_fp16_float_controls: false,
        has_gl_component_indexing_bug: device.has_component_indexing_bug(),
        has_gl_precise_bug: device.has_precise_bug(),
        has_gl_cbuf_ftou_bug: device.has_cbuf_ftou_bug(),
        has_gl_bool_ref_bug: device.has_bool_ref_bug(),
        ignore_nan_fp_comparisons: true,
        gl_max_compute_smem_size: device.max_compute_shared_memory_size(),
        min_ssbo_alignment: device.shader_storage_buffer_alignment() as u64,
        max_user_clip_distances: 8,
        ..ShaderProfile::default()
    }
}

#[cfg(test)]
fn test_opengl_shader_profile() -> ShaderProfile {
    ShaderProfile {
        support_vertex_instance_id: true,
        support_vote: true,
        support_native_ndc: true,
        support_geometry_streams: true,
        lower_left_origin_mode: true,
        need_declared_frag_colors: true,
        has_broken_spirv_clamp: true,
        has_broken_unsigned_image_offsets: true,
        has_broken_signed_operations: true,
        ignore_nan_fp_comparisons: true,
        max_user_clip_distances: 8,
        ..ShaderProfile::default()
    }
}

fn record_shader_pipeline_stage(stage: usize) {
    if std::env::var_os("RUZU_PROFILE_SHADER_PIPELINE_STALL").is_none() {
        return;
    }
    SHADER_PIPELINE_LAST_STAGE.store(stage as u64, Ordering::Relaxed);
    if let Some(counter) = SHADER_PIPELINE_STAGE_COUNTS.get(stage) {
        counter.fetch_add(1, Ordering::Relaxed);
    }
}

pub fn dump_shader_pipeline_stall_profile() {
    if SHADER_PIPELINE_STAGE_COUNTS[0].load(Ordering::Relaxed) == 0 {
        return;
    }
    const NAMES: [&str; 12] = [
        "current_enter",
        "before_refresh_stages",
        "after_refresh_stages",
        "after_maxwell_lookup",
        "after_key_build",
        "cache_hit",
        "before_slow_path",
        "slow_path_enter",
        "slow_path_cache_hit",
        "before_create_pipeline",
        "after_create_pipeline",
        "slow_path_exit",
    ];
    let last_stage = SHADER_PIPELINE_LAST_STAGE.load(Ordering::Relaxed) as usize;
    let last_stage_name = NAMES.get(last_stage).copied().unwrap_or("unknown");
    eprintln!(
        "[SHADER_PIPELINE_STALL_PROFILE] last_stage={} ({})",
        last_stage, last_stage_name
    );
    for (index, name) in NAMES.iter().enumerate() {
        eprintln!(
            "[SHADER_PIPELINE_STALL_PROFILE]   {:02} {:<24} {}",
            index,
            name,
            SHADER_PIPELINE_STAGE_COUNTS[index].load(Ordering::Relaxed)
        );
    }
}

/// OpenGL shader cache.
///
/// Corresponds to `OpenGL::ShaderCache`.
pub struct ShaderCache {
    /// Whether to use asynchronous shader compilation.
    pub use_asynchronous_shaders: bool,
    /// Whether a strict GL context is required for compilation.
    pub strict_context_required: bool,
    profile: ShaderProfile,

    /// Current graphics pipeline key.
    graphics_key: GraphicsPipelineKey,
    /// Currently bound graphics pipeline (key lookup).
    current_pipeline: Option<GraphicsPipelineKey>,

    /// Cache of compiled graphics pipelines.
    graphics_cache: HashMap<GraphicsPipelineKey, GraphicsPipeline>,
    /// Cache of compiled compute pipelines.
    compute_cache: HashMap<ComputePipelineKey, ComputePipeline>,

    /// Path to the on-disk shader cache file.
    shader_cache_filename: PathBuf,

    /// Optional GPU memory reader used to fault in Maxwell shader bytecode
    /// at the addresses supplied via `set_pending_program_addresses`.
    ///
    /// When unset, `create_graphics_pipeline` produces a placeholder
    /// pipeline with no compiled GLSL (matching the previous behaviour).
    /// When set, the slow path actually invokes
    /// `shader_recompiler::compile_shader_glsl` for each enabled stage and
    /// stages the resulting GLSL strings on the returned `GraphicsPipeline`
    /// so that gap (4) (`glCreateShader` / `glLinkProgram`) can pick them
    /// up without further plumbing changes.
    gpu_memory_reader: Option<GpuMemoryReader>,

    /// Per-stage Maxwell shader program addresses to use as the
    /// `unique_hashes` source for the next graphics pipeline lookup.
    ///
    /// Upstream `ShaderCache::CurrentGraphicsPipelineSlowPath` derives
    /// `unique_hashes` by hashing the actual shader bytecode reachable
    /// from `maxwell3d->regs.program_region.Address() + pipelines[i].offset`
    /// for each of the 6 Maxwell shader stages. Without GPU-memory access
    /// wired up yet, we use the GPU virtual addresses themselves as
    /// stand-in unique identifiers — they are stable per shader for the
    /// lifetime of a draw and are sufficient as cache keys.
    ///
    /// The owning rasterizer (`RasterizerOpenGL::draw`) is responsible for
    /// populating this slot via [`ShaderCache::set_pending_program_addresses`]
    /// before invoking [`ShaderCache::current_graphics_pipeline`].
    /// A zero entry means "stage disabled" and produces a zero hash, matching
    /// upstream's `unique_hashes[i] == 0` semantics.
    pending_program_addresses: [u64; 6],

    /// Owning thread for this cache instance. First call to a mutating method
    /// stores the current thread id; subsequent calls assert it hasn't changed.
    /// Used to verify the "concurrent HashMap access" hypothesis behind the
    /// MK8D ~60s SIGSEGV in `hash_one<GraphicsPipelineKey>` — if multiple
    /// thread ids touch the cache, the plain `HashMap` is unsafe.
    /// 0 = unowned; otherwise hash of `ThreadId`.
    owner_tid_hash: std::sync::atomic::AtomicU64,
}

impl ShaderCache {
    /// Create a new shader cache.
    ///
    /// Corresponds to `ShaderCache::ShaderCache()`.
    pub fn new(device: &Device) -> Self {
        Self::new_with_profile(
            opengl_shader_profile(device),
            device.use_asynchronous_shaders(),
            device.strict_context_required(),
        )
    }

    pub(crate) fn new_with_profile(
        profile: ShaderProfile,
        use_asynchronous_shaders: bool,
        strict_context_required: bool,
    ) -> Self {
        Self {
            use_asynchronous_shaders,
            strict_context_required,
            profile,
            graphics_key: GraphicsPipelineKey::default(),
            current_pipeline: None,
            graphics_cache: HashMap::new(),
            compute_cache: HashMap::new(),
            shader_cache_filename: PathBuf::new(),
            gpu_memory_reader: None,
            pending_program_addresses: [0; 6],
            owner_tid_hash: std::sync::atomic::AtomicU64::new(0),
        }
    }

    #[cfg(test)]
    pub(crate) fn new_for_test() -> Self {
        Self::new_with_profile(test_opengl_shader_profile(), false, false)
    }

    /// Assert that this cache is only ever accessed from a single thread.
    /// If a second thread touches the cache, panic with both thread ids so
    /// we can identify the concurrent caller.
    ///
    /// This is the verification step for the MK8D `hash_one<GraphicsPipelineKey>`
    /// SIGSEGV hypothesis: if multiple threads access `graphics_cache`/`compute_cache`,
    /// the plain `HashMap` corrupts under concurrent insert/get, eventually
    /// jumping into stale code with wrong stack alignment.
    ///
    /// Gated behind `RUZU_ASSERT_SHADER_CACHE_OWNER=1` so it can be enabled
    /// only for diagnostic runs (no overhead in normal use).
    fn assert_single_owner(&self, site: &'static str) {
        if std::env::var_os("RUZU_ASSERT_SHADER_CACHE_OWNER").is_none() {
            return;
        }
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        use std::sync::atomic::Ordering;
        let mut h = DefaultHasher::new();
        std::thread::current().id().hash(&mut h);
        let tid_hash = h.finish().max(1); // 0 = unowned sentinel
                                          // Try to publish ownership on first call (compare_exchange 0 -> tid_hash).
        let prev = self
            .owner_tid_hash
            .compare_exchange(0, tid_hash, Ordering::AcqRel, Ordering::Acquire)
            .unwrap_or_else(|x| x);
        if prev == 0 {
            // We became the owner. First access; nothing to assert.
            eprintln!(
                "[SHADER_CACHE_OWNER] claim site={} thread={:?} tid_hash=0x{:x}",
                site,
                std::thread::current().id(),
                tid_hash
            );
            return;
        }
        if prev != tid_hash {
            panic!(
                "[SHADER_CACHE_OWNER] violation: site={} expected_tid_hash=0x{:x} this_thread={:?} this_tid_hash=0x{:x}",
                site,
                prev,
                std::thread::current().id(),
                tid_hash
            );
        }
    }

    /// Install the GPU memory reader callback used to fault in shader
    /// bytecode during pipeline creation. The reader is forwarded into
    /// every `GenericEnvironment` the cache constructs.
    pub fn set_gpu_memory_reader(&mut self, reader: GpuMemoryReader) {
        self.gpu_memory_reader = Some(reader);
    }

    /// Whether a GPU memory reader is currently installed.
    pub fn has_gpu_memory_reader(&self) -> bool {
        self.gpu_memory_reader.is_some()
    }

    /// Set the per-stage Maxwell shader program addresses that the next
    /// `current_graphics_pipeline` lookup should treat as the
    /// `unique_hashes` array.
    ///
    /// See the field-level docs on `pending_program_addresses` for the
    /// upstream parity rationale and the "stage disabled = 0" convention.
    pub fn set_pending_program_addresses(&mut self, addresses: [u64; 6]) {
        self.pending_program_addresses = addresses;
    }

    /// Read back the pending shader program addresses (test/inspection helper).
    pub fn pending_program_addresses(&self) -> [u64; 6] {
        self.pending_program_addresses
    }

    /// Load disk resources for a given title.
    ///
    /// Port of `ShaderCache::LoadDiskResources()`.
    ///
    /// In the full implementation, this:
    /// 1. Opens the pipeline cache file for the given title ID
    /// 2. Deserializes shader environments and pipeline keys
    /// 3. Recompiles pipelines (potentially asynchronously)
    /// 4. Notifies a progress callback
    pub fn load_disk_resources(&mut self, title_id: u64) {
        let cache_dir = PathBuf::from("shader_cache");
        self.shader_cache_filename = cache_dir.join(format!("{:016x}.bin", title_id));

        if !self.shader_cache_filename.exists() {
            log::info!("No pipeline cache found for title {:016x}", title_id);
            return;
        }
        log::info!(
            "Loading pipeline cache from {:?}",
            self.shader_cache_filename
        );
        // Full implementation requires shader_environment::load_pipelines
    }

    /// Get the current graphics pipeline.
    ///
    /// Port of `ShaderCache::CurrentGraphicsPipeline()`.
    ///
    /// Fast path: if `current_pipeline` is already set and the matching
    /// pipeline lives in the cache, return it (checking the async-build
    /// sync fence if asynchronous compilation is enabled).
    ///
    /// Slow path: delegate to `current_graphics_pipeline_slow_path`, which
    /// builds the key from engine state, looks it up in the cache, and
    /// creates a new pipeline on miss.
    ///
    /// Returns `None` when async compilation is still in flight. Upstream
    /// additionally returns `None` when the game hasn't bound a shader
    /// program yet, but the current Rust port always treats the default
    /// key as valid so the first draw of a fresh boot gets a placeholder
    /// pipeline to exercise the hot path.
    pub fn current_graphics_pipeline(&mut self) -> Option<&mut GraphicsPipeline> {
        self.assert_single_owner("current_graphics_pipeline");
        // Mirror upstream `ShaderCache::CurrentGraphicsPipeline`: rebuild the
        // key from live engine state every call, and only short-circuit when
        // the freshly-built key matches the previously-bound pipeline. This
        // is critical for parity — caching the previous key without
        // rebuilding causes pipeline aliasing the moment the game switches
        // shaders.
        let key = self.build_graphics_key();
        self.graphics_key = key;
        let use_async = self.use_asynchronous_shaders;
        if let Some(prev) = self.current_pipeline {
            if prev == key && self.graphics_cache.contains_key(&key) {
                let pipeline = self.graphics_cache.get_mut(&key).unwrap();
                return Self::built_pipeline(use_async, None, pipeline);
            }
        }
        self.current_graphics_pipeline_slow_path()
    }

    /// Shared-owner runtime path matching upstream `OpenGL::ShaderCache`'s
    /// inherited `VideoCommon::ShaderCache` usage more closely than the local
    /// address-only fallback.
    pub fn current_graphics_pipeline_with_shared_cache(
        &mut self,
        shared_cache: &mut SharedShaderCache,
    ) -> Option<&mut GraphicsPipeline> {
        self.assert_single_owner("current_graphics_pipeline_with_shared_cache");
        record_shader_pipeline_stage(0);
        let trace_pipeline = std::env::var_os("RUZU_TRACE_SHADER_PIPELINE").is_some();
        if trace_pipeline {
            eprintln!("[SHADER_PIPELINE] current_graphics begin");
        }
        record_shader_pipeline_stage(1);
        if !shared_cache.refresh_stages(&mut self.graphics_key.unique_hashes) {
            if trace_pipeline {
                eprintln!("[SHADER_PIPELINE] refresh_stages=false");
            }
            self.current_pipeline = None;
            return None;
        }
        record_shader_pipeline_stage(2);
        if trace_pipeline {
            eprintln!(
                "[SHADER_PIPELINE] refresh_stages=true hashes={:X?}",
                self.graphics_key.unique_hashes
            );
        }

        let maxwell3d = shared_cache.current_maxwell3d()?;
        record_shader_pipeline_stage(3);
        self.graphics_key.raw = 0;
        self.graphics_key.set_early_z(maxwell3d.mandated_early_z());
        self.graphics_key
            .set_gs_input_topology(maxwell3d.draw_manager_topology() as u32);
        self.graphics_key
            .set_tessellation_primitive(maxwell3d.tessellation_domain_type());
        self.graphics_key
            .set_tessellation_spacing(maxwell3d.tessellation_spacing());
        self.graphics_key
            .set_tessellation_clockwise(maxwell3d.tessellation_clockwise());
        self.graphics_key
            .set_xfb_enabled(maxwell3d.transform_feedback_enabled());
        self.graphics_key
            .set_app_stage(maxwell3d.engine_state() as u32);
        let alpha_func = if maxwell3d.alpha_test_enabled() {
            comparison_op_to_runtime_func(maxwell3d.alpha_test_func())
        } else {
            CompareFunction::Always
        };
        self.graphics_key
            .set_alpha_test_func(comparison_func_to_key(alpha_func));
        self.graphics_key.alpha_test_ref = maxwell3d.alpha_test_ref().to_bits();
        if self.graphics_key.xfb_enabled() {
            self.graphics_key.xfb_state = maxwell3d.transform_feedback_state();
        }
        record_shader_pipeline_stage(4);

        let key = self.graphics_key;
        self.current_pipeline = Some(key);
        let maxwell3d = shared_cache.current_maxwell3d();
        trace_gl_pipeline(1, &key, self.graphics_cache.len(), 0, 0, 0);

        if self.graphics_cache.contains_key(&key) {
            if trace_pipeline {
                eprintln!("[SHADER_PIPELINE] graphics_cache hit");
            }
            trace_gl_pipeline(2, &key, self.graphics_cache.len(), 0, 0, 0);
            record_shader_pipeline_stage(5);
            let pipeline = self.graphics_cache.get_mut(&key).unwrap();
            return Self::built_pipeline(self.use_asynchronous_shaders, maxwell3d, pipeline);
        }

        if trace_pipeline {
            eprintln!("[SHADER_PIPELINE] graphics_cache miss -> slow_path");
        }
        trace_gl_pipeline(3, &key, self.graphics_cache.len(), 0, 0, 0);
        record_shader_pipeline_stage(6);
        self.current_graphics_pipeline_slow_path_with_shared_cache(shared_cache)
    }

    /// Get the current compute pipeline.
    ///
    /// Port of `ShaderCache::CurrentComputePipeline()`.
    pub fn current_compute_pipeline(&mut self) -> Option<&mut ComputePipeline> {
        self.assert_single_owner("current_compute_pipeline");
        // In the full implementation:
        // 1. Build compute key from KeplerCompute engine state
        // 2. Look up in compute_cache
        // 3. If not found, create and insert
        None
    }

    /// Slow path for looking up / creating a graphics pipeline.
    ///
    /// Port of `ShaderCache::CurrentGraphicsPipelineSlowPath()`.
    ///
    /// Upstream builds a `GraphicsPipelineKey` from Maxwell3D engine state
    /// (program pointers for each stage, XFB state, tessellation config,
    /// early-Z flag, topology). The current Rust port does not yet thread
    /// Maxwell3D state into the shader cache, so `build_graphics_key` is
    /// a placeholder that returns a deterministic key for the first draw.
    /// That is enough for step 2's goal: exercising the cache machinery
    /// so `current_graphics_pipeline` returns a real `&mut GraphicsPipeline`
    /// instead of `None`.
    fn current_graphics_pipeline_slow_path(&mut self) -> Option<&mut GraphicsPipeline> {
        self.assert_single_owner("current_graphics_pipeline_slow_path");
        let key = self.build_graphics_key();
        self.graphics_key = key;
        self.current_pipeline = Some(key);
        trace_gl_pipeline(1, &key, self.graphics_cache.len(), 1, 0, 0);

        // Cache hit: return the existing pipeline, gated by async-build state.
        if self.graphics_cache.contains_key(&key) {
            trace_gl_pipeline(2, &key, self.graphics_cache.len(), 1, 0, 0);
            let pipeline = self.graphics_cache.get_mut(&key).unwrap();
            return Self::built_pipeline(self.use_asynchronous_shaders, None, pipeline);
        }

        // Cache miss: create a pipeline, insert it, and return a reference
        // to the stored entry.
        trace_gl_pipeline(3, &key, self.graphics_cache.len(), 1, 0, 0);
        let pipeline = self.create_graphics_pipeline()?;
        log::info!(
            "gl_shader_cache: inserted placeholder graphics pipeline (total={})",
            self.graphics_cache.len() + 1
        );
        self.graphics_cache.insert(key, pipeline);
        trace_gl_pipeline(6, &key, self.graphics_cache.len(), 1, 0, 0);
        let inserted = self.graphics_cache.get_mut(&key).expect("just inserted");
        Self::built_pipeline(self.use_asynchronous_shaders, None, inserted)
    }

    fn current_graphics_pipeline_slow_path_with_shared_cache(
        &mut self,
        shared_cache: &mut SharedShaderCache,
    ) -> Option<&mut GraphicsPipeline> {
        self.assert_single_owner("current_graphics_pipeline_slow_path_with_shared_cache");
        record_shader_pipeline_stage(7);
        let trace_pipeline = std::env::var_os("RUZU_TRACE_SHADER_PIPELINE").is_some();
        let key = self.graphics_key;
        self.current_pipeline = Some(key);
        let maxwell3d = shared_cache.current_maxwell3d();
        trace_gl_pipeline(1, &key, self.graphics_cache.len(), 2, 0, 0);

        if self.graphics_cache.contains_key(&key) {
            if trace_pipeline {
                eprintln!("[SHADER_PIPELINE] slow_path cache hit");
            }
            trace_gl_pipeline(2, &key, self.graphics_cache.len(), 2, 0, 0);
            record_shader_pipeline_stage(8);
            let pipeline = self.graphics_cache.get_mut(&key).unwrap();
            return Self::built_pipeline(self.use_asynchronous_shaders, maxwell3d, pipeline);
        }

        if trace_pipeline {
            eprintln!("[SHADER_PIPELINE] create_graphics_pipeline_with_shared_cache begin");
        }
        trace_gl_pipeline(4, &key, self.graphics_cache.len(), 2, 0, 0);
        record_shader_pipeline_stage(9);
        let pipeline = self.create_graphics_pipeline_with_shared_cache(shared_cache)?;
        if trace_pipeline {
            eprintln!("[SHADER_PIPELINE] create_graphics_pipeline_with_shared_cache end");
        }
        trace_gl_pipeline(5, &key, self.graphics_cache.len(), 2, 0, 0);
        record_shader_pipeline_stage(10);
        self.graphics_cache.insert(key, pipeline);
        trace_gl_pipeline(6, &key, self.graphics_cache.len(), 2, 0, 0);
        let inserted = self.graphics_cache.get_mut(&key).expect("just inserted");
        let result = Self::built_pipeline(self.use_asynchronous_shaders, maxwell3d, inserted);
        record_shader_pipeline_stage(11);
        result
    }

    /// Build a `GraphicsPipelineKey` describing the current Maxwell3D state.
    ///
    /// Reduced local fallback path for non-shared callers. The active runtime
    /// path uses `current_graphics_pipeline_with_shared_cache(...)`, which now
    /// mirrors the upstream owner by reading Maxwell state from the shared
    /// `VideoCommon::ShaderCache` channel owner.
    fn build_graphics_key(&self) -> GraphicsPipelineKey {
        GraphicsPipelineKey {
            unique_hashes: self.pending_program_addresses,
            ..GraphicsPipelineKey::default()
        }
    }

    /// Check if a pipeline is built (or if async shaders should return None).
    fn built_pipeline<'a>(
        use_asynchronous_shaders: bool,
        maxwell3d: Option<&crate::engines::maxwell_3d::Maxwell3D>,
        pipeline: &'a mut GraphicsPipeline,
    ) -> Option<&'a mut GraphicsPipeline> {
        if pipeline.is_built() {
            return Some(pipeline);
        }
        if !use_asynchronous_shaders {
            return Some(pipeline);
        }
        let Some(maxwell3d) = maxwell3d else {
            return None;
        };
        if maxwell3d.zeta_enable() {
            return None;
        }
        let draw_state = maxwell3d.draw_manager_state();
        if draw_state.index_buffer.count <= 6 || draw_state.vertex_buffer.count <= 6 {
            return Some(pipeline);
        }
        None
    }

    /// Create a new graphics pipeline from the current engine state.
    ///
    /// Port of upstream `ShaderCache::CreateGraphicsPipeline` (the
    /// no-argument overload at `gl_shader_cache.cpp:429` plus the inner
    /// `CreateGraphicsPipeline(pools, key, envs, ...)` overload at
    /// `gl_shader_cache.cpp:449`).
    ///
    /// Upstream walks `Maxwell::MaxShaderProgram` (6) entries; for each
    /// non-zero `unique_hashes[index]` it builds a `GraphicsEnvironment`
    /// and runs `TranslateProgram` → `EmitGLSL`. The Rust port currently
    /// uses [`GenericEnvironment`] (the upstream-faithful TryFindSize /
    /// SetCachedSize / Analyze base class) directly, since
    /// `GraphicsEnvironment`'s extra Maxwell3D-state ingestion isn't
    /// plumbed yet.
    ///
    /// When no GPU memory reader is installed (the test path) this
    /// function returns the placeholder pipeline so the rest of the
    /// pipeline-cache machinery still exercises end-to-end.
    fn create_graphics_pipeline(&mut self) -> Option<GraphicsPipeline> {
        let mut pipeline = GraphicsPipeline::new(self.graphics_key);

        // Without a reader, return the placeholder unchanged. This is the
        // shape every existing test relies on, and it's also what runs in
        // production until `RasterizerOpenGL` plumbs its `gpu_read`
        // callback into the cache.
        let Some(reader) = self.gpu_memory_reader.as_ref() else {
            return Some(pipeline);
        };

        let uses_vertex_a =
            self.pending_program_addresses[0] != 0 && self.pending_program_addresses[1] != 0;

        // Maxwell3D shader program slot index → recompiler stage and
        // `glsl_sources` slot. Slot 0 is VertexA; upstream merges it into
        // the VertexB program and emits the merged program as GL slot 0.
        const STAGE_LAYOUT: &[(usize, ShaderStage, usize)] = &[
            (1, ShaderStage::VertexB, 0),
            (2, ShaderStage::TessellationControl, 1),
            (3, ShaderStage::TessellationEval, 2),
            (4, ShaderStage::Geometry, 3),
            (5, ShaderStage::Fragment, 4),
        ];
        let mut bindings = shader_recompiler::backend::bindings::Bindings::default();
        let mut infos: [Option<ShaderInfo>; 5] = Default::default();

        if uses_vertex_a {
            let mut va_env = GenericEnvironment::new()
                .with_gpu_read(Arc::clone(reader))
                .with_program(self.pending_program_addresses[0], 0)
                .with_initial_offset(std::mem::size_of::<ProgramHeader>() as u32)
                .with_stage(ShaderStage::VertexA);
            let mut vb_env = GenericEnvironment::new()
                .with_gpu_read(Arc::clone(reader))
                .with_program(self.pending_program_addresses[1], 0)
                .with_initial_offset(std::mem::size_of::<ProgramHeader>() as u32)
                .with_stage(ShaderStage::VertexB);

            if va_env.analyze().is_none() {
                log::warn!("gl_shader_cache: TryFindSize failed for VertexA");
            } else if vb_env.analyze().is_none() {
                log::warn!("gl_shader_cache: TryFindSize failed for VertexB");
            } else {
                let runtime_info =
                    Self::make_runtime_info(&self.graphics_key, ShaderStage::VertexB, None);
                let compiled = compile_dual_vertex_shader_glsl_at_offset_with_bindings(
                    va_env.cached_instruction_slice(),
                    va_env.cached_instruction_start(),
                    vb_env.cached_instruction_slice(),
                    vb_env.cached_instruction_start(),
                    &self.profile,
                    &runtime_info,
                    &mut bindings,
                );
                let mut previous_info = compiled.info.clone();
                infos[0] = Some(previous_info.clone());
                pipeline.glsl_sources[0] = Some(compiled.source);

                for &(slot, stage, gl_slot) in STAGE_LAYOUT {
                    let address = self.pending_program_addresses[slot];
                    if address == 0 || slot == 1 {
                        continue;
                    }

                    let mut env = GenericEnvironment::new()
                        .with_gpu_read(Arc::clone(reader))
                        .with_program(address, 0)
                        .with_initial_offset(std::mem::size_of::<ProgramHeader>() as u32)
                        .with_stage(stage);

                    let Some(_hash) = env.analyze() else {
                        log::warn!(
                            "gl_shader_cache: TryFindSize failed for stage {:?} at 0x{:X}",
                            stage,
                            address
                        );
                        continue;
                    };

                    let runtime_info =
                        Self::make_runtime_info(&self.graphics_key, stage, Some(&previous_info));
                    let texture_bound_buffer = env.texture_bound_buffer();
                    let compiled = self.compile_stage_glsl_at_offset_with_runtime_info(
                        env.cached_instruction_slice(),
                        stage,
                        env.cached_instruction_start(),
                        Some(texture_bound_buffer),
                        Some(env.sph()),
                        &runtime_info,
                        &mut bindings,
                    );
                    log::debug!(
                        "gl_shader_cache: compiled {:?} stage to {} bytes of GLSL",
                        stage,
                        compiled.source.len()
                    );
                    if std::env::var_os("RUZU_DUMP_GLSL").is_some() {
                        use std::sync::atomic::{AtomicUsize, Ordering};
                        static COUNT: AtomicUsize = AtomicUsize::new(0);
                        let idx = COUNT.fetch_add(1, Ordering::Relaxed);
                        let path = format!("/tmp/ruzu_glsl_{:04}_{:?}.glsl", idx, stage);
                        let _ = std::fs::write(&path, &compiled.source);
                        log::warn!(
                            "[GLSL_DUMP] stage={:?} idx={} bytes={} path={}",
                            stage,
                            idx,
                            compiled.source.len(),
                            path
                        );
                    }
                    previous_info = compiled.info.clone();
                    let mut final_source =
                        patch_fragment_debug_by_source_hash(stage, compiled.source);
                    if std::env::var_os("RUZU_FORCE_GREEN_FRAGMENT").is_some()
                        && format!("{:?}", stage) == "Fragment"
                    {
                        if let Some(idx) = final_source.rfind("void main(){") {
                            let head = &final_source[..idx];
                            let new_body =
                                "void main(){\nfrag_color0=vec4(0.0,1.0,0.0,1.0);\nreturn;\n}\n";
                            let new_source = format!("{}{}", head, new_body);
                            log::warn!(
                                "[FORCE_GREEN] patched fragment shader, new size {}",
                                new_source.len()
                            );
                            final_source = new_source;
                        }
                    }
                    if std::env::var_os("RUZU_FORCE_BLACK_FRAGMENT").is_some()
                        && format!("{:?}", stage) == "Fragment"
                    {
                        if let Some(idx) = final_source.rfind("void main(){") {
                            let head = &final_source[..idx];
                            let new_body =
                                "void main(){\nfrag_color0=vec4(0.0,0.0,0.0,1.0);\nreturn;\n}\n";
                            let new_source = format!("{}{}", head, new_body);
                            log::warn!("[FORCE_BLACK] patched FS to opaque black");
                            final_source = new_source;
                        }
                    }
                    if std::env::var_os("RUZU_FORCE_BLUE_FRAGMENT").is_some()
                        && format!("{:?}", stage) == "Fragment"
                    {
                        if let Some(idx) = final_source.rfind("void main(){") {
                            let head = &final_source[..idx];
                            let new_body =
                                "void main(){\nfrag_color0=vec4(0.0,0.0,1.0,1.0);\nreturn;\n}\n";
                            let new_source = format!("{}{}", head, new_body);
                            log::warn!("[FORCE_BLUE] patched FS to opaque blue");
                            final_source = new_source;
                        }
                    }
                    if std::env::var_os("RUZU_PROBE_FRAGCOORD").is_some()
                        && format!("{:?}", stage) == "Fragment"
                    {
                        // Encode gl_FragCoord.w (1=normal,0=bad) as red.
                        // Encode normalized gl_FragCoord.xy as green/blue.
                        if let Some(idx) = final_source.rfind("void main(){") {
                            let head = &final_source[..idx];
                            let new_body = "void main(){\nfloat fcw=gl_FragCoord.w;\nfloat zx=fcw==0.0?1.0:0.0;\nfrag_color0=vec4(zx,clamp(fcw,0.0,1.0),0.0,1.0);\nreturn;\n}\n";
                            let new_source = format!("{}{}", head, new_body);
                            log::warn!("[PROBE_FRAGCOORD] patched FS to dump gl_FragCoord.w");
                            final_source = new_source;
                        }
                    }
                    if std::env::var_os("RUZU_FORCE_FULLSCREEN_VS").is_some()
                        && format!("{:?}", stage).contains("Vertex")
                    {
                        if let Some(idx) = final_source.rfind("void main(){") {
                            let head = &final_source[..idx];
                            let new_body = if std::env::var_os("RUZU_FORCE_FULLSCREEN_VS_READ_ATTR")
                                .is_some()
                            {
                                "void main(){\nvec4 ruzu_dummy=in_attr0;\nfloat ruzu_sink=ruzu_dummy.x*0.0;\nvec2 p=vec2(float((gl_VertexID<<1)&2),float(gl_VertexID&2));\ngl_Position=vec4(p*2.0-1.0+ruzu_sink,0.0,1.0);\nreturn;\n}\n"
                            } else {
                                "void main(){\nvec2 p=vec2(float((gl_VertexID<<1)&2),float(gl_VertexID&2));\ngl_Position=vec4(p*2.0-1.0,0.0,1.0);\nreturn;\n}\n"
                            };
                            let new_source = format!("{}{}", head, new_body);
                            log::warn!("[FORCE_FS_VS] patched vertex shader");
                            final_source = new_source;
                        }
                    }
                    if std::env::var_os("RUZU_FORCE_VERTEX_ZW").is_some()
                        && format!("{:?}", stage).contains("Vertex")
                    {
                        if let Some(idx) = final_source.rfind("return;") {
                            let head = &final_source[..idx];
                            let tail = &final_source[idx..];
                            final_source =
                                format!("{}gl_Position.z=0.0;\ngl_Position.w=1.0;\n{}", head, tail);
                            log::warn!("[FORCE_VERTEX_ZW] patched vertex shader");
                        }
                    }
                    if std::env::var_os("RUZU_FORCE_VERTEX_XY").is_some()
                        && format!("{:?}", stage).contains("Vertex")
                    {
                        if let Some(idx) = final_source.rfind("return;") {
                            let head = &final_source[..idx];
                            let tail = &final_source[idx..];
                            final_source = format!(
                        "{}vec2 ruzu_p=vec2(float((gl_VertexID<<1)&2),float(gl_VertexID&2));\ngl_Position.xy=ruzu_p*2.0-1.0;\n{}",
                        head, tail
                    );
                            log::warn!("[FORCE_VERTEX_XY] patched vertex shader");
                        }
                    }
                    if std::env::var_os("RUZU_FORCE_VERTEX_OUT_ZERO").is_some()
                        && format!("{:?}", stage).contains("Vertex")
                    {
                        if let Some(idx) = final_source.rfind("return;") {
                            let head = &final_source[..idx];
                            let tail = &final_source[idx..];
                            final_source = format!("{}out_attr0=vec4(0.0);\n{}", head, tail);
                            log::warn!("[FORCE_VERTEX_OUT_ZERO] patched vertex shader");
                        }
                    }
                    if std::env::var_os("RUZU_DUMP_GLSL_FINAL").is_some() {
                        use std::sync::atomic::{AtomicUsize, Ordering};
                        static FINAL_COUNT: AtomicUsize = AtomicUsize::new(0);
                        let idx = FINAL_COUNT.fetch_add(1, Ordering::Relaxed);
                        let path = format!("/tmp/ruzu_glsl_final_{:04}_{:?}.glsl", idx, stage);
                        let _ = std::fs::write(&path, &final_source);
                        log::warn!(
                            "[GLSL_FINAL_DUMP] stage={:?} idx={} bytes={} path={}",
                            stage,
                            idx,
                            final_source.len(),
                            path
                        );
                    }
                    pipeline.glsl_sources[gl_slot] = Some(final_source);
                    infos[gl_slot] = Some(previous_info.clone());
                }

                pipeline.apply_shader_infos(&infos);
                return Some(pipeline);
            }
        }

        let mut previous_info: Option<ShaderInfo> = None;
        for &(slot, stage, gl_slot) in STAGE_LAYOUT {
            let address = self.pending_program_addresses[slot];
            if address == 0 {
                continue;
            }
            if uses_vertex_a && slot == 1 {
                continue;
            }

            // Each shader sits in GPU memory at `program_region + offset`,
            // and that combined value is exactly what the rasterizer hands
            // us in `pending_program_addresses`. Treat the whole address
            // as the `program_base + start_address` for the upstream
            // `GenericEnvironment` constructor by passing `start_address = 0`.
            // Both `ShaderStage` (here, from `shader_recompiler`) and
            // `GenericEnvironment::stage` (from `video_core::shader_environment`,
            // which now re-exports `shader_recompiler::stage::Stage`) are
            // the same type, so the assignment is direct.
            let mut env = GenericEnvironment::new()
                .with_gpu_read(Arc::clone(reader))
                .with_program(address, 0)
                .with_initial_offset(std::mem::size_of::<ProgramHeader>() as u32)
                .with_stage(stage);

            // Find the shader's tail with the upstream sentinel scan.
            let Some(_hash) = env.analyze() else {
                log::warn!(
                    "gl_shader_cache: TryFindSize failed for stage {:?} at 0x{:X}",
                    stage,
                    address
                );
                continue;
            };

            // `analyze` populates `env.code` with whole-block reads up to
            // and including the block containing the sentinel; the prefix
            // up to `cached_highest` is the actual shader body the
            // recompiler should consume.
            let runtime_info =
                Self::make_runtime_info(&self.graphics_key, stage, previous_info.as_ref());
            let texture_bound_buffer = env.texture_bound_buffer();
            let compiled = self.compile_stage_glsl_at_offset_with_runtime_info(
                env.cached_instruction_slice(),
                stage,
                env.cached_instruction_start(),
                Some(texture_bound_buffer),
                Some(env.sph()),
                &runtime_info,
                &mut bindings,
            );
            log::debug!(
                "gl_shader_cache: compiled {:?} stage to {} bytes of GLSL",
                stage,
                compiled.source.len()
            );
            if std::env::var_os("RUZU_DUMP_GLSL").is_some() {
                use std::sync::atomic::{AtomicUsize, Ordering};
                static COUNT: AtomicUsize = AtomicUsize::new(0);
                let idx = COUNT.fetch_add(1, Ordering::Relaxed);
                let path = format!("/tmp/ruzu_glsl_{:04}_{:?}.glsl", idx, stage);
                let _ = std::fs::write(&path, &compiled.source);
                log::warn!(
                    "[GLSL_DUMP] stage={:?} idx={} bytes={} path={}",
                    stage,
                    idx,
                    compiled.source.len(),
                    path
                );
            }
            previous_info = Some(compiled.info.clone());
            infos[gl_slot] = previous_info.clone();
            let mut final_source =
                patch_fragment_debug_by_source_hash(ShaderStage::VertexB, compiled.source);
            if std::env::var_os("RUZU_FORCE_GREEN_FRAGMENT").is_some()
                && format!("{:?}", stage) == "Fragment"
            {
                if let Some(idx) = final_source.rfind("void main(){") {
                    let head = &final_source[..idx];
                    let new_body = "void main(){\nfrag_color0=vec4(0.0,1.0,0.0,1.0);\nreturn;\n}\n";
                    let new_source = format!("{}{}", head, new_body);
                    log::warn!(
                        "[FORCE_GREEN] patched fragment shader (path 2), new size {}",
                        new_source.len()
                    );
                    final_source = new_source;
                }
            }
            if std::env::var_os("RUZU_FORCE_BLACK_FRAGMENT").is_some()
                && format!("{:?}", stage) == "Fragment"
            {
                if let Some(idx) = final_source.rfind("void main(){") {
                    let head = &final_source[..idx];
                    let new_body = "void main(){\nfrag_color0=vec4(0.0,0.0,0.0,1.0);\nreturn;\n}\n";
                    let new_source = format!("{}{}", head, new_body);
                    log::warn!("[FORCE_BLACK] patched FS (path 2)");
                    final_source = new_source;
                }
            }
            if std::env::var_os("RUZU_PROBE_FRAGCOORD").is_some()
                && format!("{:?}", stage) == "Fragment"
            {
                if let Some(idx) = final_source.rfind("void main(){") {
                    let head = &final_source[..idx];
                    let new_body = "void main(){\nfloat fcw=gl_FragCoord.w;\nfloat zx=fcw==0.0?1.0:0.0;\nfrag_color0=vec4(zx,clamp(fcw,0.0,1.0),0.0,1.0);\nreturn;\n}\n";
                    let new_source = format!("{}{}", head, new_body);
                    log::warn!("[PROBE_FRAGCOORD] patched FS (path 2)");
                    final_source = new_source;
                }
            }
            if std::env::var_os("RUZU_FORCE_FULLSCREEN_VS").is_some()
                && format!("{:?}", stage).contains("Vertex")
            {
                if let Some(idx) = final_source.rfind("void main(){") {
                    let head = &final_source[..idx];
                    let new_body = if std::env::var_os("RUZU_FORCE_FULLSCREEN_VS_READ_ATTR")
                        .is_some()
                    {
                        "void main(){\nvec4 ruzu_dummy=in_attr0;\nfloat ruzu_sink=ruzu_dummy.x*0.0;\nvec2 p=vec2(float((gl_VertexID<<1)&2),float(gl_VertexID&2));\ngl_Position=vec4(p*2.0-1.0+ruzu_sink,0.0,1.0);\nreturn;\n}\n"
                    } else {
                        "void main(){\nvec2 p=vec2(float((gl_VertexID<<1)&2),float(gl_VertexID&2));\ngl_Position=vec4(p*2.0-1.0,0.0,1.0);\nreturn;\n}\n"
                    };
                    let new_source = format!("{}{}", head, new_body);
                    log::warn!("[FORCE_FS_VS] patched vertex shader (path 2)");
                    final_source = new_source;
                }
            }
            if std::env::var_os("RUZU_FORCE_VERTEX_ZW").is_some()
                && format!("{:?}", stage).contains("Vertex")
            {
                if let Some(idx) = final_source.rfind("return;") {
                    let head = &final_source[..idx];
                    let tail = &final_source[idx..];
                    final_source =
                        format!("{}gl_Position.z=0.0;\ngl_Position.w=1.0;\n{}", head, tail);
                    log::warn!("[FORCE_VERTEX_ZW] patched vertex shader (path 2)");
                }
            }
            if std::env::var_os("RUZU_FORCE_VERTEX_XY").is_some()
                && format!("{:?}", stage).contains("Vertex")
            {
                if let Some(idx) = final_source.rfind("return;") {
                    let head = &final_source[..idx];
                    let tail = &final_source[idx..];
                    final_source = format!(
                        "{}vec2 ruzu_p=vec2(float((gl_VertexID<<1)&2),float(gl_VertexID&2));\ngl_Position.xy=ruzu_p*2.0-1.0;\n{}",
                        head, tail
                    );
                    log::warn!("[FORCE_VERTEX_XY] patched vertex shader (path 2)");
                }
            }
            if std::env::var_os("RUZU_FORCE_VERTEX_OUT_ZERO").is_some()
                && format!("{:?}", stage).contains("Vertex")
            {
                if let Some(idx) = final_source.rfind("return;") {
                    let head = &final_source[..idx];
                    let tail = &final_source[idx..];
                    final_source = format!("{}out_attr0=vec4(0.0);\n{}", head, tail);
                    log::warn!("[FORCE_VERTEX_OUT_ZERO] patched vertex shader (path 2)");
                }
            }
            if std::env::var_os("RUZU_DUMP_GLSL_FINAL").is_some() {
                use std::sync::atomic::{AtomicUsize, Ordering};
                static FINAL_COUNT: AtomicUsize = AtomicUsize::new(0);
                let idx = FINAL_COUNT.fetch_add(1, Ordering::Relaxed);
                let path = format!("/tmp/ruzu_glsl_final_{:04}_{:?}.glsl", idx, stage);
                let _ = std::fs::write(&path, &final_source);
                log::warn!(
                    "[GLSL_FINAL_DUMP] stage={:?} idx={} bytes={} path={}",
                    stage,
                    idx,
                    final_source.len(),
                    path
                );
            }
            pipeline.glsl_sources[gl_slot] = Some(final_source);
        }

        pipeline.apply_shader_infos(&infos);
        Some(pipeline)
    }

    fn create_graphics_pipeline_with_shared_cache(
        &mut self,
        shared_cache: &SharedShaderCache,
    ) -> Option<GraphicsPipeline> {
        let trace_pipeline = std::env::var_os("RUZU_TRACE_SHADER_PIPELINE").is_some();
        if trace_pipeline {
            eprintln!("[SHADER_PIPELINE] get_graphics_environments begin");
        }
        let mut environments = GraphicsEnvironments::default();
        shared_cache.get_graphics_environments(&mut environments, &self.graphics_key.unique_hashes);
        if trace_pipeline {
            eprintln!(
                "[SHADER_PIPELINE] get_graphics_environments end env_ptrs={:?}",
                environments.env_ptrs
            );
        }
        self.create_graphics_pipeline_from_environments(&mut environments)
    }

    fn create_graphics_pipeline_from_environments(
        &mut self,
        environments: &mut GraphicsEnvironments,
    ) -> Option<GraphicsPipeline> {
        let trace_pipeline = std::env::var_os("RUZU_TRACE_SHADER_PIPELINE").is_some();
        if trace_pipeline {
            eprintln!(
                "[SHADER_PIPELINE] create_from_environments begin hashes={:X?}",
                self.graphics_key.unique_hashes
            );
        }
        trace_gl_pipeline(4, &self.graphics_key, self.graphics_cache.len(), 3, 0, 0);
        let mut pipeline = GraphicsPipeline::new(self.graphics_key);
        let uses_vertex_a = self.graphics_key.unique_hashes[0] != 0;
        let uses_vertex_b = self.graphics_key.unique_hashes[1] != 0;

        const STAGE_LAYOUT: &[(usize, ShaderStage, usize)] = &[
            (1, ShaderStage::VertexB, 0),
            (2, ShaderStage::TessellationControl, 1),
            (3, ShaderStage::TessellationEval, 2),
            (4, ShaderStage::Geometry, 3),
            (5, ShaderStage::Fragment, 4),
        ];
        let mut bindings = shader_recompiler::backend::bindings::Bindings::default();
        let mut infos: [Option<ShaderInfo>; 5] = Default::default();

        if uses_vertex_a && uses_vertex_b {
            if trace_pipeline {
                eprintln!("[SHADER_PIPELINE] compile dual vertex begin");
            }
            trace_gl_pipeline(
                7,
                &self.graphics_key,
                self.graphics_cache.len(),
                ShaderStage::VertexA as u64,
                ShaderStage::VertexB as u64,
                0,
            );
            let va_env = environments.envs[0].generic_environment_mut();
            if va_env.cached_code_slice().is_empty() && va_env.analyze().is_none() {
                log::warn!("gl_shader_cache: shared environment analyze failed for VertexA");
                trace_gl_pipeline(
                    10,
                    &self.graphics_key,
                    self.graphics_cache.len(),
                    ShaderStage::VertexA as u64,
                    0,
                    0,
                );
                return Some(pipeline);
            }
            let va_code = va_env.cached_instruction_slice().to_vec();
            let va_start = va_env.cached_instruction_start();

            let vb_env = environments.envs[1].generic_environment_mut();
            if vb_env.cached_code_slice().is_empty() && vb_env.analyze().is_none() {
                log::warn!("gl_shader_cache: shared environment analyze failed for VertexB");
                trace_gl_pipeline(
                    10,
                    &self.graphics_key,
                    self.graphics_cache.len(),
                    ShaderStage::VertexB as u64,
                    1,
                    0,
                );
                return Some(pipeline);
            }
            let runtime_info =
                Self::make_runtime_info(&self.graphics_key, ShaderStage::VertexB, None);
            let compiled = compile_dual_vertex_shader_glsl_at_offset_with_bindings(
                &va_code,
                va_start,
                vb_env.cached_instruction_slice(),
                vb_env.cached_instruction_start(),
                &self.profile,
                &runtime_info,
                &mut bindings,
            );
            if trace_pipeline {
                eprintln!(
                    "[SHADER_PIPELINE] compile dual vertex end source_len={}",
                    compiled.source.len()
                );
            }
            trace_gl_pipeline(
                8,
                &self.graphics_key,
                self.graphics_cache.len(),
                ShaderStage::VertexB as u64,
                compiled.source.len() as u64,
                0,
            );
            let mut previous_info = compiled.info.clone();
            infos[0] = Some(previous_info.clone());
            let mut final_source =
                patch_fragment_debug_by_source_hash(ShaderStage::VertexB, compiled.source);
            if std::env::var_os("RUZU_FORCE_FULLSCREEN_VS").is_some() {
                if let Some(idx) = final_source.rfind("void main(){") {
                    let head = &final_source[..idx];
                    let new_body = if std::env::var_os("RUZU_FORCE_FULLSCREEN_VS_READ_ATTR")
                        .is_some()
                    {
                        "void main(){\nvec4 ruzu_dummy=in_attr0;\nfloat ruzu_sink=ruzu_dummy.x*0.0;\nvec2 p=vec2(float((gl_VertexID<<1)&2),float(gl_VertexID&2));\ngl_Position=vec4(p*2.0-1.0+ruzu_sink,0.0,1.0);\nreturn;\n}\n"
                    } else {
                        "void main(){\nvec2 p=vec2(float((gl_VertexID<<1)&2),float(gl_VertexID&2));\ngl_Position=vec4(p*2.0-1.0,0.0,1.0);\nreturn;\n}\n"
                    };
                    final_source = format!("{}{}", head, new_body);
                    log::warn!("[FORCE_FS_VS] patched dual vertex shader (envs path)");
                }
            }
            if std::env::var_os("RUZU_FORCE_VERTEX_ZW").is_some() {
                if let Some(idx) = final_source.rfind("return;") {
                    let head = &final_source[..idx];
                    let tail = &final_source[idx..];
                    final_source =
                        format!("{}gl_Position.z=0.0;\ngl_Position.w=1.0;\n{}", head, tail);
                    log::warn!("[FORCE_VERTEX_ZW] patched dual vertex shader (envs path)");
                }
            }
            if std::env::var_os("RUZU_FORCE_VERTEX_XY").is_some() {
                if let Some(idx) = final_source.rfind("return;") {
                    let head = &final_source[..idx];
                    let tail = &final_source[idx..];
                    final_source = format!(
                        "{}vec2 ruzu_p=vec2(float((gl_VertexID<<1)&2),float(gl_VertexID&2));\ngl_Position.xy=ruzu_p*2.0-1.0;\n{}",
                        head, tail
                    );
                    log::warn!("[FORCE_VERTEX_XY] patched dual vertex shader (envs path)");
                }
            }
            if std::env::var_os("RUZU_FORCE_VERTEX_OUT_ZERO").is_some() {
                if let Some(idx) = final_source.rfind("return;") {
                    let head = &final_source[..idx];
                    let tail = &final_source[idx..];
                    final_source = format!("{}out_attr0=vec4(0.0);\n{}", head, tail);
                    log::warn!("[FORCE_VERTEX_OUT_ZERO] patched dual vertex shader (envs path)");
                }
            }
            if std::env::var_os("RUZU_DUMP_GLSL_FINAL").is_some() {
                use std::sync::atomic::{AtomicUsize, Ordering};
                static FINAL_COUNT: AtomicUsize = AtomicUsize::new(0);
                let idx = FINAL_COUNT.fetch_add(1, Ordering::Relaxed);
                let path = format!("/tmp/ruzu_glsl_final_{:04}_VertexB.glsl", idx);
                let _ = std::fs::write(&path, &final_source);
                log::warn!(
                    "[GLSL_FINAL_DUMP] stage=VertexB idx={} bytes={} path={}",
                    idx,
                    final_source.len(),
                    path
                );
            }
            pipeline.glsl_sources[0] = Some(final_source);

            for &(slot, _stage, gl_slot) in STAGE_LAYOUT {
                if self.graphics_key.unique_hashes[slot] == 0 || slot == 1 {
                    continue;
                }

                let env = environments.envs[slot].generic_environment_mut();
                let actual_stage = env.shader_stage();
                if trace_pipeline {
                    eprintln!(
                        "[SHADER_PIPELINE] stage {:?} analyze/compile begin",
                        actual_stage
                    );
                }
                trace_gl_pipeline(
                    9,
                    &self.graphics_key,
                    self.graphics_cache.len(),
                    actual_stage as u64,
                    slot as u64,
                    gl_slot as u64,
                );
                if env.cached_code_slice().is_empty() && env.analyze().is_none() {
                    log::warn!(
                        "gl_shader_cache: shared environment analyze failed for stage {:?}",
                        actual_stage
                    );
                    trace_gl_pipeline(
                        10,
                        &self.graphics_key,
                        self.graphics_cache.len(),
                        actual_stage as u64,
                        slot as u64,
                        gl_slot as u64,
                    );
                    continue;
                }

                let runtime_info =
                    Self::make_runtime_info(&self.graphics_key, actual_stage, Some(&previous_info));
                let texture_bound_buffer = env.texture_bound_buffer();
                let compiled = self.compile_stage_glsl_at_offset_with_runtime_info(
                    env.cached_instruction_slice(),
                    actual_stage,
                    env.cached_instruction_start(),
                    Some(texture_bound_buffer),
                    Some(env.sph()),
                    &runtime_info,
                    &mut bindings,
                );
                if trace_pipeline {
                    eprintln!(
                        "[SHADER_PIPELINE] stage {:?} compile end source_len={}",
                        actual_stage,
                        compiled.source.len()
                    );
                }
                trace_gl_pipeline(
                    11,
                    &self.graphics_key,
                    self.graphics_cache.len(),
                    actual_stage as u64,
                    slot as u64,
                    compiled.source.len() as u64,
                );
                previous_info = compiled.info.clone();
                infos[gl_slot] = Some(previous_info.clone());
                let mut final_source =
                    patch_fragment_debug_by_source_hash(actual_stage, compiled.source);
                if std::env::var_os("RUZU_FORCE_GREEN_FRAGMENT").is_some()
                    && format!("{:?}", actual_stage) == "Fragment"
                {
                    if let Some(idx) = final_source.rfind("void main(){") {
                        let head = &final_source[..idx];
                        let new_body =
                            "void main(){\nfrag_color0=vec4(0.0,1.0,0.0,1.0);\nreturn;\n}\n";
                        let new_source = format!("{}{}", head, new_body);
                        log::warn!("[FORCE_GREEN] patched fragment shader (envs path)");
                        final_source = new_source;
                    }
                }
                if std::env::var_os("RUZU_FORCE_BLACK_FRAGMENT").is_some()
                    && format!("{:?}", actual_stage) == "Fragment"
                {
                    if let Some(idx) = final_source.rfind("void main(){") {
                        let head = &final_source[..idx];
                        let new_body =
                            "void main(){\nfrag_color0=vec4(0.0,0.0,0.0,1.0);\nreturn;\n}\n";
                        let new_source = format!("{}{}", head, new_body);
                        log::warn!("[FORCE_BLACK] patched FS (envs path)");
                        final_source = new_source;
                    }
                }
                if std::env::var_os("RUZU_PROBE_FRAGCOORD").is_some()
                    && format!("{:?}", actual_stage) == "Fragment"
                {
                    if let Some(idx) = final_source.rfind("void main(){") {
                        let head = &final_source[..idx];
                        let new_body = "void main(){\nfloat fcw=gl_FragCoord.w;\nfloat zx=fcw==0.0?1.0:0.0;\nfrag_color0=vec4(zx,clamp(fcw,0.0,1.0),0.0,1.0);\nreturn;\n}\n";
                        let new_source = format!("{}{}", head, new_body);
                        log::warn!("[PROBE_FRAGCOORD] patched FS (envs path)");
                        final_source = new_source;
                    }
                }
                if std::env::var_os("RUZU_FORCE_FULLSCREEN_VS").is_some()
                    && format!("{:?}", actual_stage).contains("Vertex")
                {
                    if let Some(idx) = final_source.rfind("void main(){") {
                        let head = &final_source[..idx];
                        let new_body = if std::env::var_os("RUZU_FORCE_FULLSCREEN_VS_READ_ATTR")
                            .is_some()
                        {
                            "void main(){\nvec4 ruzu_dummy=in_attr0;\nfloat ruzu_sink=ruzu_dummy.x*0.0;\nvec2 p=vec2(float((gl_VertexID<<1)&2),float(gl_VertexID&2));\ngl_Position=vec4(p*2.0-1.0+ruzu_sink,0.0,1.0);\nreturn;\n}\n"
                        } else {
                            "void main(){\nvec2 p=vec2(float((gl_VertexID<<1)&2),float(gl_VertexID&2));\ngl_Position=vec4(p*2.0-1.0,0.0,1.0);\nreturn;\n}\n"
                        };
                        let new_source = format!("{}{}", head, new_body);
                        log::warn!("[FORCE_FS_VS] patched vertex shader (envs path)");
                        final_source = new_source;
                    }
                }
                if std::env::var_os("RUZU_FORCE_VERTEX_ZW").is_some()
                    && format!("{:?}", actual_stage).contains("Vertex")
                {
                    if let Some(idx) = final_source.rfind("return;") {
                        let head = &final_source[..idx];
                        let tail = &final_source[idx..];
                        final_source =
                            format!("{}gl_Position.z=0.0;\ngl_Position.w=1.0;\n{}", head, tail);
                        log::warn!("[FORCE_VERTEX_ZW] patched vertex shader (envs path)");
                    }
                }
                if std::env::var_os("RUZU_FORCE_VERTEX_XY").is_some()
                    && format!("{:?}", actual_stage).contains("Vertex")
                {
                    if let Some(idx) = final_source.rfind("return;") {
                        let head = &final_source[..idx];
                        let tail = &final_source[idx..];
                        final_source = format!(
                            "{}vec2 ruzu_p=vec2(float((gl_VertexID<<1)&2),float(gl_VertexID&2));\ngl_Position.xy=ruzu_p*2.0-1.0;\n{}",
                            head, tail
                        );
                        log::warn!("[FORCE_VERTEX_XY] patched vertex shader (envs path)");
                    }
                }
                if std::env::var_os("RUZU_FORCE_VERTEX_OUT_ZERO").is_some()
                    && format!("{:?}", actual_stage).contains("Vertex")
                {
                    if let Some(idx) = final_source.rfind("return;") {
                        let head = &final_source[..idx];
                        let tail = &final_source[idx..];
                        final_source = format!("{}out_attr0=vec4(0.0);\n{}", head, tail);
                        log::warn!("[FORCE_VERTEX_OUT_ZERO] patched vertex shader (envs path)");
                    }
                }
                if std::env::var_os("RUZU_DUMP_GLSL_FINAL").is_some() {
                    use std::sync::atomic::{AtomicUsize, Ordering};
                    static FINAL_COUNT: AtomicUsize = AtomicUsize::new(0);
                    let idx = FINAL_COUNT.fetch_add(1, Ordering::Relaxed);
                    let path = format!("/tmp/ruzu_glsl_final_{:04}_{:?}.glsl", idx, actual_stage);
                    let _ = std::fs::write(&path, &final_source);
                    log::warn!(
                        "[GLSL_FINAL_DUMP] stage={:?} idx={} bytes={} path={}",
                        actual_stage,
                        idx,
                        final_source.len(),
                        path
                    );
                }
                pipeline.glsl_sources[gl_slot] = Some(final_source);
            }

            pipeline.apply_shader_infos(&infos);
            if trace_pipeline {
                eprintln!("[SHADER_PIPELINE] create_from_environments end");
            }
            trace_gl_pipeline(5, &self.graphics_key, self.graphics_cache.len(), 3, 1, 0);
            return Some(pipeline);
        }

        let mut previous_info: Option<ShaderInfo> = None;
        for &(slot, _stage, gl_slot) in STAGE_LAYOUT {
            if self.graphics_key.unique_hashes[slot] == 0 {
                continue;
            }
            if uses_vertex_a && slot == 1 {
                continue;
            }

            let env = environments.envs[slot].generic_environment_mut();
            let actual_stage = env.shader_stage();
            if trace_pipeline {
                eprintln!(
                    "[SHADER_PIPELINE] stage {:?} analyze/compile begin",
                    actual_stage
                );
            }
            trace_gl_pipeline(
                9,
                &self.graphics_key,
                self.graphics_cache.len(),
                actual_stage as u64,
                slot as u64,
                gl_slot as u64,
            );
            if env.cached_code_slice().is_empty() && env.analyze().is_none() {
                log::warn!(
                    "gl_shader_cache: shared environment analyze failed for stage {:?}",
                    actual_stage
                );
                trace_gl_pipeline(
                    10,
                    &self.graphics_key,
                    self.graphics_cache.len(),
                    actual_stage as u64,
                    slot as u64,
                    gl_slot as u64,
                );
                continue;
            }

            let runtime_info =
                Self::make_runtime_info(&self.graphics_key, actual_stage, previous_info.as_ref());
            let texture_bound_buffer = env.texture_bound_buffer();
            let compiled = self.compile_stage_glsl_at_offset_with_runtime_info(
                env.cached_instruction_slice(),
                actual_stage,
                env.cached_instruction_start(),
                Some(texture_bound_buffer),
                Some(env.sph()),
                &runtime_info,
                &mut bindings,
            );
            if trace_pipeline {
                eprintln!(
                    "[SHADER_PIPELINE] stage {:?} compile end source_len={}",
                    actual_stage,
                    compiled.source.len()
                );
            }
            trace_gl_pipeline(
                11,
                &self.graphics_key,
                self.graphics_cache.len(),
                actual_stage as u64,
                slot as u64,
                compiled.source.len() as u64,
            );
            previous_info = Some(compiled.info.clone());
            infos[gl_slot] = previous_info.clone();
            let mut final_source =
                patch_fragment_debug_by_source_hash(actual_stage, compiled.source);
            if std::env::var_os("RUZU_FORCE_GREEN_FRAGMENT").is_some()
                && format!("{:?}", actual_stage) == "Fragment"
            {
                if let Some(idx) = final_source.rfind("void main(){") {
                    let head = &final_source[..idx];
                    let new_body = "void main(){\nfrag_color0=vec4(0.0,1.0,0.0,1.0);\nreturn;\n}\n";
                    let new_source = format!("{}{}", head, new_body);
                    log::warn!("[FORCE_GREEN] patched fragment shader (envs path 2)");
                    final_source = new_source;
                }
            }
            if std::env::var_os("RUZU_FORCE_BLACK_FRAGMENT").is_some()
                && format!("{:?}", actual_stage) == "Fragment"
            {
                if let Some(idx) = final_source.rfind("void main(){") {
                    let head = &final_source[..idx];
                    let new_body = "void main(){\nfrag_color0=vec4(0.0,0.0,0.0,1.0);\nreturn;\n}\n";
                    let new_source = format!("{}{}", head, new_body);
                    log::warn!("[FORCE_BLACK] patched FS (envs path 2)");
                    final_source = new_source;
                }
            }
            if std::env::var_os("RUZU_PROBE_FRAGCOORD").is_some()
                && format!("{:?}", actual_stage) == "Fragment"
            {
                if let Some(idx) = final_source.rfind("void main(){") {
                    let head = &final_source[..idx];
                    let new_body = "void main(){\nfloat fcw=gl_FragCoord.w;\nfloat zx=fcw==0.0?1.0:0.0;\nfrag_color0=vec4(zx,clamp(fcw,0.0,1.0),0.0,1.0);\nreturn;\n}\n";
                    let new_source = format!("{}{}", head, new_body);
                    log::warn!("[PROBE_FRAGCOORD] patched FS (envs path 2)");
                    final_source = new_source;
                }
            }
            if std::env::var_os("RUZU_FORCE_FULLSCREEN_VS").is_some()
                && format!("{:?}", actual_stage).contains("Vertex")
            {
                if let Some(idx) = final_source.rfind("void main(){") {
                    let head = &final_source[..idx];
                    let new_body = if std::env::var_os("RUZU_FORCE_FULLSCREEN_VS_READ_ATTR")
                        .is_some()
                    {
                        "void main(){\nvec4 ruzu_dummy=in_attr0;\nfloat ruzu_sink=ruzu_dummy.x*0.0;\nvec2 p=vec2(float((gl_VertexID<<1)&2),float(gl_VertexID&2));\ngl_Position=vec4(p*2.0-1.0+ruzu_sink,0.0,1.0);\nreturn;\n}\n"
                    } else {
                        "void main(){\nvec2 p=vec2(float((gl_VertexID<<1)&2),float(gl_VertexID&2));\ngl_Position=vec4(p*2.0-1.0,0.0,1.0);\nreturn;\n}\n"
                    };
                    let new_source = format!("{}{}", head, new_body);
                    log::warn!("[FORCE_FS_VS] patched vertex shader (envs path 2)");
                    final_source = new_source;
                }
            }
            if std::env::var_os("RUZU_FORCE_VERTEX_ZW").is_some()
                && format!("{:?}", actual_stage).contains("Vertex")
            {
                if let Some(idx) = final_source.rfind("return;") {
                    let head = &final_source[..idx];
                    let tail = &final_source[idx..];
                    final_source =
                        format!("{}gl_Position.z=0.0;\ngl_Position.w=1.0;\n{}", head, tail);
                    log::warn!("[FORCE_VERTEX_ZW] patched vertex shader (envs path 2)");
                }
            }
            if std::env::var_os("RUZU_FORCE_VERTEX_XY").is_some()
                && format!("{:?}", actual_stage).contains("Vertex")
            {
                if let Some(idx) = final_source.rfind("return;") {
                    let head = &final_source[..idx];
                    let tail = &final_source[idx..];
                    final_source = format!(
                        "{}vec2 ruzu_p=vec2(float((gl_VertexID<<1)&2),float(gl_VertexID&2));\ngl_Position.xy=ruzu_p*2.0-1.0;\n{}",
                        head, tail
                    );
                    log::warn!("[FORCE_VERTEX_XY] patched vertex shader (envs path 2)");
                }
            }
            if std::env::var_os("RUZU_FORCE_VERTEX_OUT_ZERO").is_some()
                && format!("{:?}", actual_stage).contains("Vertex")
            {
                if let Some(idx) = final_source.rfind("return;") {
                    let head = &final_source[..idx];
                    let tail = &final_source[idx..];
                    final_source = format!("{}out_attr0=vec4(0.0);\n{}", head, tail);
                    log::warn!("[FORCE_VERTEX_OUT_ZERO] patched vertex shader (envs path 2)");
                }
            }
            if std::env::var_os("RUZU_DUMP_GLSL_FINAL").is_some() {
                use std::sync::atomic::{AtomicUsize, Ordering};
                static FINAL_COUNT: AtomicUsize = AtomicUsize::new(0);
                let idx = FINAL_COUNT.fetch_add(1, Ordering::Relaxed);
                let path = format!("/tmp/ruzu_glsl_final_{:04}_{:?}.glsl", idx, actual_stage);
                let _ = std::fs::write(&path, &final_source);
                log::warn!(
                    "[GLSL_FINAL_DUMP] stage={:?} idx={} bytes={} path={}",
                    actual_stage,
                    idx,
                    final_source.len(),
                    path
                );
            }
            pipeline.glsl_sources[gl_slot] = Some(final_source);
        }

        pipeline.apply_shader_infos(&infos);
        if trace_pipeline {
            eprintln!("[SHADER_PIPELINE] create_from_environments end");
        }
        trace_gl_pipeline(5, &self.graphics_key, self.graphics_cache.len(), 3, 2, 0);
        Some(pipeline)
    }

    /// Port of upstream `OpenGL::MakeRuntimeInfo(...)` in
    /// `gl_shader_cache.cpp`.
    fn make_runtime_info(
        key: &GraphicsPipelineKey,
        stage: ShaderStage,
        previous_program: Option<&ShaderInfo>,
    ) -> RuntimeInfo {
        let mut info = RuntimeInfo::default();
        if let Some(previous_program) = previous_program {
            info.previous_stage_stores = previous_program.stores.clone();
            info.previous_stage_legacy_stores_mapping =
                previous_program.legacy_stores_mapping.clone();
        } else {
            // Mark all stores as available for vertex shaders.
            info.previous_stage_stores.mask.fill(u64::MAX);
        }

        match stage {
            ShaderStage::VertexB | ShaderStage::Geometry => {
                if key.xfb_enabled() {
                    let (varyings, count) =
                        transform_feedback::make_transform_feedback_varyings(&key.xfb_state);
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
            }
            ShaderStage::TessellationEval => {
                info.tess_clockwise = !key.tessellation_clockwise();
                info.tess_primitive = match key.tessellation_primitive() {
                    0 => TessPrimitive::Isolines,
                    1 => TessPrimitive::Triangles,
                    2 => TessPrimitive::Quads,
                    _ => TessPrimitive::Triangles,
                };
                info.tess_spacing = match key.tessellation_spacing() {
                    0 => TessSpacing::Equal,
                    1 => TessSpacing::FractionalOdd,
                    2 => TessSpacing::FractionalEven,
                    _ => TessSpacing::Equal,
                };
            }
            ShaderStage::Fragment => {
                info.force_early_z = key.early_z();
                info.alpha_test_func = Some(comparison_func_from_key(key.alpha_test_func()));
                info.alpha_test_reference = f32::from_bits(key.alpha_test_ref);
            }
            _ => {}
        }

        info.input_topology = match key.gs_input_topology() {
            0 => InputTopology::Points,
            1 | 2 | 3 => InputTopology::Lines,
            10 | 11 => InputTopology::LinesAdjacency,
            12 | 13 => InputTopology::TrianglesAdjacency,
            _ => InputTopology::Triangles,
        };

        info
    }

    /// Translate a single Maxwell shader stage to GLSL via the recompiler.
    ///
    /// This is the bridge between `gl_shader_cache` and `shader_recompiler`'s
    /// GLSL backend. Once Maxwell3D state is plumbed into `ShaderCache`, the
    /// graphics-pipeline build path will:
    ///   1. Read the shader bytecode for each enabled stage out of GPU
    ///      memory at the addresses Maxwell3D advertises in `regs`.
    ///   2. Call this method per stage to obtain a GLSL source string.
    ///   3. Hand the strings to `glCreateShader` / `glCompileShader` /
    ///      `glLinkProgram` and stash the resulting program handles on
    ///      `GraphicsPipeline`.
    ///
    /// Exposed as a `ShaderCache` method (rather than a free function) so
    /// the eventual real path can pull the active GPU `Profile` and
    /// per-pipeline `RuntimeInfo` from `self`.
    pub fn compile_stage_glsl(&self, code: &[u64], stage: ShaderStage) -> CompiledGlslShader {
        self.compile_stage_glsl_at_offset(code, stage, 0)
    }

    pub fn compile_stage_glsl_at_offset(
        &self,
        code: &[u64],
        stage: ShaderStage,
        base_offset: u32,
    ) -> CompiledGlslShader {
        let runtime_info = Self::make_runtime_info(&self.graphics_key, stage, None);
        let mut bindings = shader_recompiler::backend::bindings::Bindings::default();
        self.compile_stage_glsl_at_offset_with_runtime_info(
            code,
            stage,
            base_offset,
            None,
            None,
            &runtime_info,
            &mut bindings,
        )
    }

    fn compile_stage_glsl_at_offset_with_runtime_info(
        &self,
        code: &[u64],
        stage: ShaderStage,
        base_offset: u32,
        texture_bound_buffer: Option<u32>,
        sph: Option<&ProgramHeader>,
        runtime_info: &RuntimeInfo,
        bindings: &mut shader_recompiler::backend::bindings::Bindings,
    ) -> CompiledGlslShader {
        let compiled = if let Some(texture_bound_buffer) = texture_bound_buffer {
            if let Some(sph) = sph {
                compile_shader_glsl_at_offset_with_bindings_and_texture_bound_and_sph(
                    code,
                    stage,
                    base_offset,
                    texture_bound_buffer,
                    sph,
                    &self.profile,
                    runtime_info,
                    bindings,
                )
            } else {
                compile_shader_glsl_at_offset_with_bindings_and_texture_bound(
                    code,
                    stage,
                    base_offset,
                    texture_bound_buffer,
                    &self.profile,
                    runtime_info,
                    bindings,
                )
            }
        } else {
            compile_shader_glsl_at_offset_with_bindings(
                code,
                stage,
                base_offset,
                &self.profile,
                runtime_info,
                bindings,
            )
        };
        if std::env::var_os("RUZU_DUMP_GLSL").is_some() {
            use std::sync::atomic::{AtomicUsize, Ordering};
            static COUNT: AtomicUsize = AtomicUsize::new(0);
            let idx = COUNT.fetch_add(1, Ordering::Relaxed);
            let path = format!("/tmp/ruzu_glsl_{:04}_{:?}.glsl", idx, stage);
            let _ = std::fs::write(&path, &compiled.source);
            log::warn!(
                "[GLSL_DUMP] stage={:?} idx={} bytes={} path={}",
                stage,
                idx,
                compiled.source.len(),
                path
            );
        }
        compiled
    }

    /// Create a new compute pipeline.
    ///
    /// Port of `ShaderCache::CreateComputePipeline()`.
    fn create_compute_pipeline(&mut self, _key: &ComputePipelineKey) -> Option<ComputePipeline> {
        // In the full implementation:
        // 1. Read compute shader from KeplerCompute engine state
        // 2. Create shader environment
        // 3. Recompile shader
        // 4. Return new ComputePipeline
        None
    }

    /// Returns the number of cached graphics pipelines.
    pub fn graphics_pipeline_count(&self) -> usize {
        self.graphics_cache.len()
    }

    /// Returns the number of cached compute pipelines.
    pub fn compute_pipeline_count(&self) -> usize {
        self.compute_cache.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    use crate::control::channel_state::ChannelState;
    use crate::engines::engine_interface::EngineInterface;
    use crate::engines::maxwell_3d::{EngineHint, Maxwell3D, PrimitiveTopology};
    use crate::memory_manager::MemoryManager;
    use crate::renderer_opengl::gl_graphics_pipeline::NUM_STAGES;
    use parking_lot::Mutex as ParkingLotMutex;

    fn make_cpu_reader(
        cpu_base: u64,
        backing: Arc<Vec<u8>>,
    ) -> Arc<dyn Fn(u64, &mut [u8]) + Send + Sync> {
        Arc::new(move |cpu_addr, dst| {
            if cpu_addr < cpu_base {
                return;
            }
            let offset = (cpu_addr - cpu_base) as usize;
            if offset >= backing.len() {
                return;
            }
            let n = (offset + dst.len()).min(backing.len()) - offset;
            dst[..n].copy_from_slice(&backing[offset..offset + n]);
        })
    }

    fn make_maxwell_for_built_pipeline(
        vertex_count: u32,
        index_count: u32,
        zeta_enable: bool,
    ) -> Maxwell3D {
        let mut maxwell = Maxwell3D::new();
        <Maxwell3D as EngineInterface>::call_method(&mut maxwell, 0x583, vertex_count, true);
        <Maxwell3D as EngineInterface>::call_method(&mut maxwell, 0x5F8, index_count, true);
        <Maxwell3D as EngineInterface>::call_method(&mut maxwell, 0x54E, zeta_enable as u32, true);
        maxwell
    }

    #[test]
    fn shader_cache_creation() {
        let cache = ShaderCache::new_for_test();
        assert_eq!(cache.graphics_pipeline_count(), 0);
        assert_eq!(cache.compute_pipeline_count(), 0);
        assert!(!cache.use_asynchronous_shaders);
    }

    #[test]
    fn cache_version() {
        assert_eq!(CACHE_VERSION, 10);
    }

    #[test]
    fn current_graphics_pipeline_populates_cache_on_first_call() {
        let mut cache = ShaderCache::new_for_test();
        assert_eq!(cache.graphics_pipeline_count(), 0);
        assert!(cache.current_pipeline.is_none());

        // First call: slow path builds a key, creates a placeholder pipeline,
        // inserts it, and returns a live reference.
        {
            let pipeline = cache.current_graphics_pipeline();
            assert!(
                pipeline.is_some(),
                "placeholder pipeline should be returned"
            );
        }

        assert_eq!(cache.graphics_pipeline_count(), 1);
        assert!(cache.current_pipeline.is_some());
    }

    #[test]
    fn current_graphics_pipeline_reuses_cached_entry() {
        let mut cache = ShaderCache::new_for_test();

        // First call creates the entry.
        cache.current_graphics_pipeline();
        assert_eq!(cache.graphics_pipeline_count(), 1);

        // Subsequent calls with the same (default) key hit the fast path and
        // must NOT grow the cache.
        for _ in 0..16 {
            let pipeline = cache.current_graphics_pipeline();
            assert!(pipeline.is_some());
        }
        assert_eq!(cache.graphics_pipeline_count(), 1);
    }

    #[test]
    fn pending_program_addresses_default_to_zero() {
        let cache = ShaderCache::new_for_test();
        assert_eq!(cache.pending_program_addresses(), [0u64; 6]);
    }

    #[test]
    fn build_graphics_key_uses_pending_program_addresses() {
        let mut cache = ShaderCache::new_for_test();
        let addrs = [0x1000, 0x2000, 0x3000, 0x4000, 0x5000, 0x6000];
        cache.set_pending_program_addresses(addrs);

        // Slow path will run on the first call and pick up the new addresses.
        cache
            .current_graphics_pipeline()
            .expect("placeholder pipeline");
        assert_eq!(cache.graphics_pipeline_count(), 1);

        // Both the cached current_pipeline key and the build_graphics_key
        // helper should reflect the addresses.
        let key = cache.build_graphics_key();
        assert_eq!(key.unique_hashes, addrs);
    }

    #[test]
    fn create_graphics_pipeline_with_reader_emits_glsl_for_vertex_stage() {
        // Mock GPU memory: place a SELF_BRANCH_A sentinel at offset 0x80
        // (after a few words of NOPs) at the chosen vertex shader address.
        let vertex_addr: u64 = 0x10_0000_0000;
        const SELF_BRANCH_A: u64 = 0xE2400FFFFF87000F;
        let backing = {
            let mut v = vec![0u8; 0x1000];
            v[0x80..0x88].copy_from_slice(&SELF_BRANCH_A.to_le_bytes());
            Arc::new(v)
        };
        let reader: GpuMemoryReader = Arc::new(move |gpu_addr, dst| {
            if gpu_addr < vertex_addr {
                return;
            }
            let offset = (gpu_addr - vertex_addr) as usize;
            if offset >= backing.len() {
                return;
            }
            let n = (offset + dst.len()).min(backing.len()) - offset;
            dst[..n].copy_from_slice(&backing[offset..offset + n]);
        });

        let mut cache = ShaderCache::new_for_test();
        cache.set_gpu_memory_reader(reader);

        // Slot 1 = VertexB; the rest are disabled.
        cache.set_pending_program_addresses([0, vertex_addr, 0, 0, 0, 0]);

        let pipeline = cache
            .current_graphics_pipeline()
            .expect("pipeline must be created");

        // glsl_sources[0] is the vertex slot.
        let vertex_glsl = pipeline.glsl_sources[0]
            .as_ref()
            .expect("vertex GLSL must be populated");
        assert!(
            !vertex_glsl.is_empty(),
            "compile_shader_glsl produced empty source"
        );
        // Other stages remain `None`.
        for slot in 1..NUM_STAGES {
            assert!(
                pipeline.glsl_sources[slot].is_none(),
                "stage slot {} should be None",
                slot
            );
        }
    }

    #[test]
    fn distinct_program_addresses_create_distinct_cache_entries() {
        let mut cache = ShaderCache::new_for_test();

        cache.set_pending_program_addresses([0x1000, 0, 0, 0, 0, 0]);
        cache.current_graphics_pipeline().expect("first pipeline");
        assert_eq!(cache.graphics_pipeline_count(), 1);

        // Same addresses again — must hit the cache, not grow it.
        cache.set_pending_program_addresses([0x1000, 0, 0, 0, 0, 0]);
        cache
            .current_graphics_pipeline()
            .expect("first pipeline (cached)");
        assert_eq!(cache.graphics_pipeline_count(), 1);

        // Different addresses — must create a second cache entry.
        cache.set_pending_program_addresses([0x2000, 0, 0, 0, 0, 0]);
        cache.current_graphics_pipeline().expect("second pipeline");
        assert_eq!(cache.graphics_pipeline_count(), 2);
    }

    #[test]
    fn create_graphics_pipeline_from_shared_environments_emits_glsl_for_vertex_stage() {
        let vertex_addr: u64 = 0x20_0000_0000;
        const SELF_BRANCH_A: u64 = 0xE2400FFFFF87000F;
        let backing = {
            let mut v = vec![0u8; 0x1000];
            v[0x80..0x88].copy_from_slice(&SELF_BRANCH_A.to_le_bytes());
            Arc::new(v)
        };
        let reader: GpuMemoryReader = Arc::new(move |gpu_addr, dst| {
            if gpu_addr < vertex_addr {
                return;
            }
            let offset = (gpu_addr - vertex_addr) as usize;
            if offset >= backing.len() {
                return;
            }
            let n = (offset + dst.len()).min(backing.len()) - offset;
            dst[..n].copy_from_slice(&backing[offset..offset + n]);
        });

        let mut cache = ShaderCache::new_for_test();
        cache.graphics_key.unique_hashes[1] = 0x1234;

        let mut environments = GraphicsEnvironments::default();
        *environments.envs[1].generic_environment_mut() = GenericEnvironment::new()
            .with_gpu_read(reader)
            .with_program(vertex_addr, 0)
            .with_stage(ShaderStage::VertexB);

        let pipeline = cache
            .create_graphics_pipeline_from_environments(&mut environments)
            .expect("pipeline must be created");
        let vertex_glsl = pipeline.glsl_sources[0]
            .as_ref()
            .expect("vertex GLSL must be populated");
        assert!(!vertex_glsl.is_empty());
    }

    #[test]
    fn shared_cache_path_populates_live_graphics_key_fields_from_maxwell() {
        let gpu_base = 0x1_0000_0000;
        let cpu_base = 0x4000;
        let mut backing = vec![0u8; 0x2000];
        backing[0x180..0x188].copy_from_slice(&0xE2400FFFFF87000Fu64.to_le_bytes());
        let backing = Arc::new(backing);

        let memory_manager = Arc::new(ParkingLotMutex::new(MemoryManager::new(0)));
        memory_manager
            .lock()
            .map(gpu_base, cpu_base, 0x2000, 0, false);

        let mut maxwell = Maxwell3D::new();
        maxwell.set_memory_manager(Arc::clone(&memory_manager));
        maxwell.set_guest_memory_reader(make_cpu_reader(cpu_base, Arc::clone(&backing)));
        <Maxwell3D as EngineInterface>::call_method(&mut maxwell, 0x582, 1, true);
        <Maxwell3D as EngineInterface>::call_method(&mut maxwell, 0x583, 0, true);
        <Maxwell3D as EngineInterface>::call_method(&mut maxwell, 0x810, 1 | (1 << 4), true);
        <Maxwell3D as EngineInterface>::call_method(&mut maxwell, 0x811, 0x100, true);
        <Maxwell3D as EngineInterface>::call_method(&mut maxwell, 0x84, 1, true);
        <Maxwell3D as EngineInterface>::call_method(
            &mut maxwell,
            0xC8,
            0x2 | (1 << 4) | (2 << 8),
            true,
        );
        <Maxwell3D as EngineInterface>::call_method(&mut maxwell, 0x1C0, 3, true);
        <Maxwell3D as EngineInterface>::call_method(&mut maxwell, 0x1C1, 5, true);
        <Maxwell3D as EngineInterface>::call_method(&mut maxwell, 0x1C2, 0x20, true);
        <Maxwell3D as EngineInterface>::call_method(&mut maxwell, 0x1D1, 1, true);
        <Maxwell3D as EngineInterface>::call_method(&mut maxwell, 0xA00, 0x0403_0201, true);
        <Maxwell3D as EngineInterface>::call_method(
            &mut maxwell,
            0x586,
            PrimitiveTopology::TriangleStrip as u32,
            true,
        );
        <Maxwell3D as EngineInterface>::call_method(&mut maxwell, 0x652, 1, true);
        <Maxwell3D as EngineInterface>::call_method(&mut maxwell, 0x65C, 2, true);
        maxwell.set_engine_state(EngineHint::OnHleMacro);

        let mut channel = ChannelState::new(7);
        channel.program_id = 0x1234;
        channel.memory_manager = Some(Arc::clone(&memory_manager));
        channel.maxwell_3d = Some(Box::new(maxwell));
        channel.kepler_compute = Some(Box::default());

        let mut shared_cache = SharedShaderCache::default();
        shared_cache.create_channel(&channel);
        shared_cache.bind_to_channel(7);

        let mut cache = ShaderCache::new_for_test();
        let pipeline = cache
            .current_graphics_pipeline_with_shared_cache(&mut shared_cache)
            .expect("shared path should build a pipeline");

        assert_eq!(
            pipeline.key.unique_hashes[1],
            shared_cache.shader_info_slots()[1]
                .map(|ptr| unsafe { &*ptr }.unique_hash)
                .unwrap()
        );
        assert!(pipeline.key.early_z());
        assert!(pipeline.key.xfb_enabled());
        assert_eq!(
            (pipeline.key.raw >> 2) & 0xF,
            PrimitiveTopology::Lines as u32
        );
        assert_eq!((pipeline.key.raw >> 6) & 0x3, 2);
        assert_eq!((pipeline.key.raw >> 8) & 0x3, 1);
        assert_eq!((pipeline.key.raw >> 10) & 0x1, 1);
        assert_eq!(
            (pipeline.key.raw >> 11) & 0x7,
            EngineHint::OnHleMacro as u32
        );
        assert_eq!(pipeline.key.xfb_state.layouts[0].stream, 3);
        assert_eq!(pipeline.key.xfb_state.layouts[0].varying_count, 5);
        assert_eq!(pipeline.key.xfb_state.layouts[0].stride, 0x20);
        assert_eq!(pipeline.key.xfb_state.varyings[0][0].raw(), 0x0403_0201);
    }

    #[test]
    fn built_pipeline_async_shared_path_blocks_when_depth_is_enabled() {
        let maxwell = make_maxwell_for_built_pipeline(64, 64, true);
        let mut pipeline = GraphicsPipeline::new(GraphicsPipelineKey::default());
        pipeline.set_built_for_test(false);
        assert!(ShaderCache::built_pipeline(true, Some(&maxwell), &mut pipeline).is_none());
    }

    #[test]
    fn built_pipeline_async_shared_path_allows_small_draws() {
        let maxwell = make_maxwell_for_built_pipeline(4, 64, false);
        let mut pipeline = GraphicsPipeline::new(GraphicsPipelineKey::default());
        pipeline.set_built_for_test(false);
        assert!(ShaderCache::built_pipeline(true, Some(&maxwell), &mut pipeline).is_some());
    }

    #[test]
    fn compile_stage_glsl_emits_non_empty_source_for_empty_code() {
        let cache = ShaderCache::new_for_test();
        let compiled = cache.compile_stage_glsl(&[], ShaderStage::VertexB);
        assert!(
            !compiled.source.is_empty(),
            "GLSL bridge should produce non-empty source even for empty bytecode"
        );
        assert_eq!(compiled.stage, ShaderStage::VertexB);
    }

    #[test]
    fn opengl_fragment_profile_declares_all_frag_colors() {
        let cache = ShaderCache::new_for_test();
        let compiled = cache.compile_stage_glsl(&[], ShaderStage::Fragment);

        for index in 0..8 {
            assert!(
                compiled.source.contains(&format!(
                    "layout(location={})out vec4 frag_color{};",
                    index, index
                )),
                "OpenGL profile must declare frag_color{} even when the shader does not write it",
                index
            );
        }
    }

    #[test]
    fn shader_cache_uses_stored_opengl_profile() {
        let mut profile = test_opengl_shader_profile();
        profile.need_declared_frag_colors = false;
        let cache = ShaderCache::new_with_profile(profile, false, false);
        let compiled = cache.compile_stage_glsl(&[], ShaderStage::Fragment);

        assert!(
            !compiled.source.contains("frag_color7"),
            "ShaderCache must pass its stored profile to GLSL compilation"
        );
    }

    #[test]
    fn current_graphics_pipeline_placeholder_is_built() {
        let mut cache = ShaderCache::new_for_test();
        let pipeline = cache
            .current_graphics_pipeline()
            .expect("placeholder pipeline");
        // Default-constructed `GraphicsPipeline` reports built=true so the
        // synchronous-compilation path in `RasterizerOpenGL::draw` can
        // proceed without blocking on an async build fence.
        assert!(pipeline.is_built());
    }
}
