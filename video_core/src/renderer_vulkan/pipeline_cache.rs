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
    ///
    /// Computes a hash from the key fields for use in the pipeline cache
    /// lookup table. Uses a simple FNV-like combination.
    pub fn hash_value(&self) -> u64 {
        let mut h = self.unique_hash;
        h ^= self.shared_memory_size as u64;
        h = h.wrapping_mul(0x100000001b3);
        for &ws in &self.workgroup_size {
            h ^= ws as u64;
            h = h.wrapping_mul(0x100000001b3);
        }
        h
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
const CACHE_VERSION: u32 = 10;

/// Port of `PipelineCache` class.
///
/// Extends `ShaderCache` to manage Vulkan graphics and compute pipeline
/// objects, with disk serialization support.
pub struct PipelineCache {
    device: ash::Device,
    use_asynchronous_shaders: bool,
    use_vulkan_pipeline_cache: bool,

    main_pools: ShaderPools,

    pipeline_cache_filename: PathBuf,
    vulkan_pipeline_cache_filename: PathBuf,
    vulkan_pipeline_cache: vk::PipelineCache,

    compute_cache: HashMap<u64, vk::Pipeline>,
}

impl PipelineCache {
    /// Port of `PipelineCache::PipelineCache`.
    pub fn new(
        device: ash::Device,
        use_asynchronous_shaders: bool,
        use_vulkan_pipeline_cache: bool,
    ) -> Self {
        let vulkan_pipeline_cache = if use_vulkan_pipeline_cache {
            let cache_ci = vk::PipelineCacheCreateInfo::builder().build();
            unsafe {
                device
                    .create_pipeline_cache(&cache_ci, None)
                    .unwrap_or(vk::PipelineCache::null())
            }
        } else {
            vk::PipelineCache::null()
        };

        PipelineCache {
            device,
            use_asynchronous_shaders,
            use_vulkan_pipeline_cache,
            main_pools: ShaderPools::new(),
            pipeline_cache_filename: PathBuf::new(),
            vulkan_pipeline_cache_filename: PathBuf::new(),
            vulkan_pipeline_cache,
            compute_cache: HashMap::new(),
        }
    }

    /// Port of `PipelineCache::CurrentGraphicsPipeline`.
    ///
    /// Returns the current graphics pipeline for the bound shader state.
    /// May trigger compilation if the pipeline has not been built yet.
    pub fn current_graphics_pipeline(&mut self) -> Option<vk::Pipeline> {
        // In the full implementation, this looks up the current shader
        // state, finds or creates the corresponding graphics pipeline,
        // and returns it. This requires the full GPU state integration.
        None
    }

    /// Port of `PipelineCache::CurrentComputePipeline`.
    ///
    /// Returns the current compute pipeline for the bound compute shader.
    pub fn current_compute_pipeline(&mut self) -> Option<vk::Pipeline> {
        // In the full implementation, this looks up the current compute
        // shader state and returns the cached pipeline.
        None
    }

    /// Port of `PipelineCache::LoadDiskResources`.
    ///
    /// Loads previously compiled pipelines from disk for the given title.
    pub fn load_disk_resources(&mut self, title_id: u64, pipeline_cache_dir: &std::path::Path) {
        self.pipeline_cache_filename = pipeline_cache_dir.join(format!("{:016x}.bin", title_id));
        self.vulkan_pipeline_cache_filename =
            pipeline_cache_dir.join(format!("{:016x}_vk.bin", title_id));

        // Load Vulkan pipeline cache from disk if available
        if self.use_vulkan_pipeline_cache {
            if let Some(cache) =
                self.load_vulkan_pipeline_cache(&self.vulkan_pipeline_cache_filename.clone())
            {
                self.vulkan_pipeline_cache = cache;
            }
        }
    }

    /// Port of `PipelineCache::SerializeVulkanPipelineCache`.
    ///
    /// Serializes the Vulkan pipeline cache to disk.
    pub fn serialize_vulkan_pipeline_cache(&self, filename: &std::path::Path) {
        if self.vulkan_pipeline_cache == vk::PipelineCache::null() {
            return;
        }

        let data = unsafe {
            self.device
                .get_pipeline_cache_data(self.vulkan_pipeline_cache)
                .unwrap_or_default()
        };

        if data.is_empty() {
            return;
        }

        // Write version header + cache data
        let mut output = Vec::with_capacity(4 + data.len());
        output.extend_from_slice(&CACHE_VERSION.to_le_bytes());
        output.extend_from_slice(&data);

        if let Err(e) = std::fs::write(filename, &output) {
            log::error!("Failed to write Vulkan pipeline cache: {}", e);
        }
    }

    /// Port of loading Vulkan pipeline cache from disk.
    fn load_vulkan_pipeline_cache(&self, filename: &std::path::Path) -> Option<vk::PipelineCache> {
        let data = std::fs::read(filename).ok()?;
        if data.len() < 4 {
            return None;
        }

        let version = u32::from_le_bytes([data[0], data[1], data[2], data[3]]);
        if version != CACHE_VERSION {
            log::info!(
                "Pipeline cache version mismatch (expected {}, got {}), discarding",
                CACHE_VERSION,
                version
            );
            return None;
        }

        let cache_data = &data[4..];
        let cache_ci = vk::PipelineCacheCreateInfo::builder()
            .initial_data(cache_data)
            .build();

        unsafe {
            self.device
                .create_pipeline_cache(&cache_ci, None)
                .ok()
        }
    }
}

impl Drop for PipelineCache {
    fn drop(&mut self) {
        // Save the pipeline cache before destroying
        if self.use_vulkan_pipeline_cache
            && self.vulkan_pipeline_cache != vk::PipelineCache::null()
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
