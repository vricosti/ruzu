// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_shader_cache.h and gl_shader_cache.cpp
//!
//! OpenGL shader cache -- manages compilation and caching of graphics and compute pipelines.

use std::collections::HashMap;
use std::path::PathBuf;

use super::gl_compute_pipeline::{ComputePipeline, ComputePipelineKey};
use super::gl_graphics_pipeline::{GraphicsPipeline, GraphicsPipelineKey};

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
        }
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
    /// Returns None if no pipeline is currently bound or if async compilation
    /// hasn't completed yet.
    pub fn current_graphics_pipeline(&mut self) -> Option<&mut GraphicsPipeline> {
        let use_async = self.use_asynchronous_shaders;
        if let Some(key) = self.current_pipeline {
            if self.graphics_cache.contains_key(&key) {
                let pipeline = self.graphics_cache.get_mut(&key).unwrap();
                if use_async && !pipeline.is_built() {
                    return None;
                }
                return Some(pipeline);
            }
        }
        // Slow path: look up or create the pipeline
        None
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
    fn current_graphics_pipeline_slow_path(&mut self) -> Option<&mut GraphicsPipeline> {
        // In the full implementation:
        // 1. Build graphics key from Maxwell3D engine state
        // 2. Look up in graphics_cache
        // 3. If not found, create via create_graphics_pipeline()
        // 4. Update current_pipeline
        None
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
    /// Port of `ShaderCache::CreateGraphicsPipeline()`.
    fn create_graphics_pipeline(&mut self) -> Option<GraphicsPipeline> {
        // In the full implementation:
        // 1. Read shader programs from Maxwell3D engine state
        // 2. Create shader environments
        // 3. Recompile shaders from SPIR-V or GLSL
        // 4. Link program pipeline
        // 5. Return new GraphicsPipeline
        None
    }

    /// Create a new compute pipeline.
    ///
    /// Port of `ShaderCache::CreateComputePipeline()`.
    fn create_compute_pipeline(
        &mut self,
        _key: &ComputePipelineKey,
    ) -> Option<ComputePipeline> {
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
}
