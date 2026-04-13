// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_shader_cache.h and gl_shader_cache.cpp
//!
//! OpenGL shader cache -- manages compilation and caching of graphics and compute pipelines.

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use shader_recompiler::profile::Profile as ShaderProfile;
use shader_recompiler::runtime_info::RuntimeInfo;
use shader_recompiler::{compile_shader_glsl, CompiledGlslShader, ShaderStage};

use crate::shader_environment::{GenericEnvironment, GpuMemoryReader};

use super::gl_compute_pipeline::{ComputePipeline, ComputePipelineKey};
use super::gl_graphics_pipeline::{GraphicsPipeline, GraphicsPipelineKey, NUM_STAGES};

/// Cache version for serialized pipeline data.
pub const CACHE_VERSION: u32 = 10;

/// OpenGL shader cache.
///
/// Corresponds to `OpenGL::ShaderCache`.
pub struct ShaderCache {
    /// Whether to use asynchronous shader compilation.
    pub use_asynchronous_shaders: bool,
    /// Whether a strict GL context is required for compilation.
    pub strict_context_required: bool,

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
}

impl ShaderCache {
    /// Create a new shader cache.
    ///
    /// Corresponds to `ShaderCache::ShaderCache()`.
    pub fn new() -> Self {
        Self {
            use_asynchronous_shaders: false,
            strict_context_required: false,
            graphics_key: GraphicsPipelineKey::default(),
            current_pipeline: None,
            graphics_cache: HashMap::new(),
            compute_cache: HashMap::new(),
            shader_cache_filename: PathBuf::new(),
            gpu_memory_reader: None,
            pending_program_addresses: [0; 6],
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
                if use_async && !pipeline.is_built() {
                    return None;
                }
                return Some(pipeline);
            }
        }
        self.current_graphics_pipeline_slow_path()
    }

    /// Get the current compute pipeline.
    ///
    /// Port of `ShaderCache::CurrentComputePipeline()`.
    pub fn current_compute_pipeline(&mut self) -> Option<&mut ComputePipeline> {
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
        let key = self.build_graphics_key();
        self.graphics_key = key;
        self.current_pipeline = Some(key);

        // Cache hit: return the existing pipeline, gated by async-build state.
        if self.graphics_cache.contains_key(&key) {
            let pipeline = self.graphics_cache.get_mut(&key).unwrap();
            if self.use_asynchronous_shaders && !pipeline.is_built() {
                return None;
            }
            return Some(pipeline);
        }

        // Cache miss: create a pipeline, insert it, and return a reference
        // to the stored entry.
        let pipeline = self.create_graphics_pipeline()?;
        log::info!(
            "gl_shader_cache: inserted placeholder graphics pipeline (total={})",
            self.graphics_cache.len() + 1
        );
        self.graphics_cache.insert(key, pipeline);
        let inserted = self.graphics_cache.get_mut(&key).expect("just inserted");
        if self.use_asynchronous_shaders && !inserted.is_built() {
            return None;
        }
        Some(inserted)
    }

    /// Build a `GraphicsPipelineKey` describing the current Maxwell3D state.
    ///
    /// Upstream reads `Tegra::Engines::Maxwell3D::regs` and hashes the active
    /// shader program bytecode, tessellation state, transform feedback
    /// state, and polygon-mode flags. Without GPU-memory access yet, we
    /// populate `unique_hashes` with the per-stage shader program *addresses*
    /// supplied via `set_pending_program_addresses`. Two pipelines whose
    /// shaders live at the same six GPU addresses will collapse onto the
    /// same cache entry — which is the correct behaviour for as long as
    /// the addresses are stable per-shader.
    ///
    /// Tessellation, XFB, polygon-mode, and topology contributions to the
    /// key are still left as defaults; they become real once the rest of
    /// Maxwell3D state is plumbed through.
    fn build_graphics_key(&self) -> GraphicsPipelineKey {
        GraphicsPipelineKey {
            unique_hashes: self.pending_program_addresses,
            ..GraphicsPipelineKey::default()
        }
    }

    /// Check if a pipeline is built (or if async shaders should return None).
    fn built_pipeline<'a>(
        &self,
        pipeline: &'a mut GraphicsPipeline,
    ) -> Option<&'a mut GraphicsPipeline> {
        if self.use_asynchronous_shaders && !pipeline.is_built() {
            return None;
        }
        Some(pipeline)
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

        // Maxwell3D shader program slot index → recompiler stage and
        // `glsl_sources` slot. Slot 0 is VertexA, which upstream merges
        // into the VertexB program; we skip it here and rely on slot 1
        // (VertexB) for the vertex shader.
        const STAGE_LAYOUT: &[(usize, ShaderStage, usize)] = &[
            (1, ShaderStage::VertexB, 0),
            (2, ShaderStage::TessellationControl, 1),
            (3, ShaderStage::TessellationEval, 2),
            (4, ShaderStage::Geometry, 3),
            (5, ShaderStage::Fragment, 4),
        ];

        for &(slot, stage, gl_slot) in STAGE_LAYOUT {
            let address = self.pending_program_addresses[slot];
            if address == 0 {
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
                .with_program(address, 0);
            env.stage = stage;

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
            let words = (env.cached_highest as usize) / std::mem::size_of::<u64>();
            let code_slice = &env.code[..words.min(env.code.len())];
            let compiled = self.compile_stage_glsl(code_slice, stage);
            log::debug!(
                "gl_shader_cache: compiled {:?} stage to {} bytes of GLSL",
                stage,
                compiled.source.len()
            );
            pipeline.glsl_sources[gl_slot] = Some(compiled.source);
        }

        Some(pipeline)
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
        let profile = ShaderProfile::default();
        let runtime_info = RuntimeInfo::default();
        compile_shader_glsl(code, stage, &profile, &runtime_info)
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

    #[test]
    fn shader_cache_creation() {
        let cache = ShaderCache::new();
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
        let mut cache = ShaderCache::new();
        assert_eq!(cache.graphics_pipeline_count(), 0);
        assert!(cache.current_pipeline.is_none());

        // First call: slow path builds a key, creates a placeholder pipeline,
        // inserts it, and returns a live reference.
        {
            let pipeline = cache.current_graphics_pipeline();
            assert!(pipeline.is_some(), "placeholder pipeline should be returned");
        }

        assert_eq!(cache.graphics_pipeline_count(), 1);
        assert!(cache.current_pipeline.is_some());
    }

    #[test]
    fn current_graphics_pipeline_reuses_cached_entry() {
        let mut cache = ShaderCache::new();

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
        let cache = ShaderCache::new();
        assert_eq!(cache.pending_program_addresses(), [0u64; 6]);
    }

    #[test]
    fn build_graphics_key_uses_pending_program_addresses() {
        let mut cache = ShaderCache::new();
        let addrs = [0x1000, 0x2000, 0x3000, 0x4000, 0x5000, 0x6000];
        cache.set_pending_program_addresses(addrs);

        // Slow path will run on the first call and pick up the new addresses.
        cache.current_graphics_pipeline().expect("placeholder pipeline");
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

        let mut cache = ShaderCache::new();
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
        let mut cache = ShaderCache::new();

        cache.set_pending_program_addresses([0x1000, 0, 0, 0, 0, 0]);
        cache.current_graphics_pipeline().expect("first pipeline");
        assert_eq!(cache.graphics_pipeline_count(), 1);

        // Same addresses again — must hit the cache, not grow it.
        cache.set_pending_program_addresses([0x1000, 0, 0, 0, 0, 0]);
        cache.current_graphics_pipeline().expect("first pipeline (cached)");
        assert_eq!(cache.graphics_pipeline_count(), 1);

        // Different addresses — must create a second cache entry.
        cache.set_pending_program_addresses([0x2000, 0, 0, 0, 0, 0]);
        cache.current_graphics_pipeline().expect("second pipeline");
        assert_eq!(cache.graphics_pipeline_count(), 2);
    }

    #[test]
    fn compile_stage_glsl_emits_non_empty_source_for_empty_code() {
        let cache = ShaderCache::new();
        let compiled = cache.compile_stage_glsl(&[], ShaderStage::VertexB);
        assert!(
            !compiled.source.is_empty(),
            "GLSL bridge should produce non-empty source even for empty bytecode"
        );
        assert_eq!(compiled.stage, ShaderStage::VertexB);
    }

    #[test]
    fn current_graphics_pipeline_placeholder_is_built() {
        let mut cache = ShaderCache::new();
        let pipeline = cache
            .current_graphics_pipeline()
            .expect("placeholder pipeline");
        // Default-constructed `GraphicsPipeline` reports built=true so the
        // synchronous-compilation path in `RasterizerOpenGL::draw` can
        // proceed without blocking on an async build fence.
        assert!(pipeline.is_built());
    }
}
