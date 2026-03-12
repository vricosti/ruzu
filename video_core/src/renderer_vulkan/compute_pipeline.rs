// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `vk_compute_pipeline.h` / `vk_compute_pipeline.cpp`.
//!
//! Manages compilation and configuration of a single Vulkan compute pipeline.

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Condvar, Mutex};

use ash::vk;

// ---------------------------------------------------------------------------
// ComputePipeline
// ---------------------------------------------------------------------------

/// Port of `ComputePipeline` class.
///
/// Wraps a single Vulkan compute pipeline, handling asynchronous building,
/// descriptor set allocation, and per-dispatch configuration.
pub struct ComputePipeline {
    /// SPV shader module.
    _spv_module: vk::ShaderModule,

    /// Descriptor set layout for this pipeline.
    _descriptor_set_layout: vk::DescriptorSetLayout,

    /// Pipeline layout.
    _pipeline_layout: vk::PipelineLayout,

    /// Descriptor update template.
    _descriptor_update_template: vk::DescriptorUpdateTemplate,

    /// The compiled compute pipeline handle.
    _pipeline: vk::Pipeline,

    /// Uniform buffer sizes per binding.
    _uniform_buffer_sizes: Vec<u32>,

    /// Synchronization for async build.
    build_condvar: Condvar,
    build_mutex: Mutex<()>,
    is_built: AtomicBool,
}

impl ComputePipeline {
    /// Port of `ComputePipeline::ComputePipeline`.
    pub fn new() -> Self {
        todo!("ComputePipeline::new")
    }

    /// Port of `ComputePipeline::Configure`.
    ///
    /// Binds descriptor sets and dispatches the compute shader.
    pub fn configure(&self) {
        todo!("ComputePipeline::configure")
    }

    /// Returns whether the pipeline has finished building.
    pub fn is_built(&self) -> bool {
        self.is_built.load(Ordering::Relaxed)
    }
}
