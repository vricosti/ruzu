// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `vk_pipeline_cache.h` / `vk_pipeline_cache.cpp`.
//!
//! Manages compilation and caching of both graphics and compute pipelines,
//! including disk serialization of the Vulkan pipeline cache.

use std::collections::HashMap;
use std::path::PathBuf;

use ash::vk;

// ---------------------------------------------------------------------------
// ComputePipelineCacheKey
// ---------------------------------------------------------------------------

/// Port of `ComputePipelineCacheKey`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(C)]
pub struct ComputePipelineCacheKey {
    pub unique_hash: u64,
    pub shared_memory_size: u32,
    pub workgroup_size: [u32; 3],
}

impl ComputePipelineCacheKey {
    /// Port of `ComputePipelineCacheKey::Hash`.
    pub fn hash_value(&self) -> usize {
        todo!("ComputePipelineCacheKey::hash_value")
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
        // Upstream releases inst, block, flow_block pools
    }
}

// ---------------------------------------------------------------------------
// PipelineCache
// ---------------------------------------------------------------------------

/// Port of `PipelineCache` class.
///
/// Extends `ShaderCache` to manage Vulkan graphics and compute pipeline
/// objects, with disk serialization support.
pub struct PipelineCache {
    _use_asynchronous_shaders: bool,
    _use_vulkan_pipeline_cache: bool,

    _main_pools: ShaderPools,

    _pipeline_cache_filename: PathBuf,
    _vulkan_pipeline_cache_filename: PathBuf,
    _vulkan_pipeline_cache: vk::PipelineCache,
}

impl PipelineCache {
    /// Port of `PipelineCache::PipelineCache`.
    pub fn new() -> Self {
        todo!("PipelineCache::new")
    }

    /// Port of `PipelineCache::CurrentGraphicsPipeline`.
    pub fn current_graphics_pipeline(&mut self) {
        todo!("PipelineCache::current_graphics_pipeline")
    }

    /// Port of `PipelineCache::CurrentComputePipeline`.
    pub fn current_compute_pipeline(&mut self) {
        todo!("PipelineCache::current_compute_pipeline")
    }

    /// Port of `PipelineCache::LoadDiskResources`.
    pub fn load_disk_resources(&mut self, _title_id: u64) {
        todo!("PipelineCache::load_disk_resources")
    }

    // --- Private ---

    fn current_graphics_pipeline_slow_path(&mut self) { todo!() }
    fn built_pipeline(&self) { todo!() }
    fn create_graphics_pipeline(&mut self) { todo!() }
    fn create_compute_pipeline(&mut self) { todo!() }

    fn serialize_vulkan_pipeline_cache(&self, _filename: &std::path::Path, _cache_version: u32) {
        todo!()
    }

    fn load_vulkan_pipeline_cache(
        &self,
        _filename: &std::path::Path,
        _expected_cache_version: u32,
    ) -> vk::PipelineCache {
        todo!()
    }
}
