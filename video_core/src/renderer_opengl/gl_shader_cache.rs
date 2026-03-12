// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_shader_cache.h and gl_shader_cache.cpp
//!
//! OpenGL shader cache — manages compilation and caching of graphics and compute pipelines.

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
    /// Currently bound graphics pipeline (index into graphics_cache).
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
        // TODO: Initialize profile, host_info from device capabilities
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
    /// Corresponds to `ShaderCache::LoadDiskResources()`.
    pub fn load_disk_resources(&mut self, _title_id: u64) {
        todo!("ShaderCache::LoadDiskResources")
    }

    /// Get the current graphics pipeline.
    ///
    /// Corresponds to `ShaderCache::CurrentGraphicsPipeline()`.
    pub fn current_graphics_pipeline(&mut self) -> Option<&mut GraphicsPipeline> {
        todo!("ShaderCache::CurrentGraphicsPipeline")
    }

    /// Get the current compute pipeline.
    ///
    /// Corresponds to `ShaderCache::CurrentComputePipeline()`.
    pub fn current_compute_pipeline(&mut self) -> Option<&mut ComputePipeline> {
        todo!("ShaderCache::CurrentComputePipeline")
    }

    /// Slow path for looking up / creating a graphics pipeline.
    fn current_graphics_pipeline_slow_path(&mut self) -> Option<&mut GraphicsPipeline> {
        todo!("ShaderCache::CurrentGraphicsPipelineSlowPath")
    }

    /// Check if a pipeline is built (or if async shaders should return None).
    fn built_pipeline<'a>(
        &self,
        _pipeline: &'a mut GraphicsPipeline,
    ) -> Option<&'a mut GraphicsPipeline> {
        todo!("ShaderCache::BuiltPipeline")
    }

    /// Create a new graphics pipeline from the current engine state.
    fn create_graphics_pipeline(&mut self) -> Option<GraphicsPipeline> {
        todo!("ShaderCache::CreateGraphicsPipeline")
    }

    /// Create a new compute pipeline.
    fn create_compute_pipeline(
        &mut self,
        _key: &ComputePipelineKey,
    ) -> Option<ComputePipeline> {
        todo!("ShaderCache::CreateComputePipeline")
    }
}
