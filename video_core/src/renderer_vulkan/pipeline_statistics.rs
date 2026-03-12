// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `pipeline_statistics.h` / `pipeline_statistics.cpp`.
//!
//! Collects and reports Vulkan pipeline compilation statistics
//! (code size, register counts, etc.) when supported by the device.

use std::sync::Mutex;

use ash::vk;

// ---------------------------------------------------------------------------
// Stats
// ---------------------------------------------------------------------------

/// Port of `PipelineStatistics::Stats` inner struct.
#[derive(Debug, Clone, Copy, Default)]
struct Stats {
    code_size: u64,
    register_count: u64,
    sgpr_count: u64,
    vgpr_count: u64,
    branches_count: u64,
    basic_block_count: u64,
}

// ---------------------------------------------------------------------------
// PipelineStatistics
// ---------------------------------------------------------------------------

/// Port of `PipelineStatistics` class.
///
/// Collects per-pipeline statistics via `VK_KHR_pipeline_executable_properties`
/// and provides an aggregate report.
pub struct PipelineStatistics {
    collected_stats: Mutex<Vec<Stats>>,
}

impl PipelineStatistics {
    /// Port of `PipelineStatistics::PipelineStatistics`.
    pub fn new() -> Self {
        PipelineStatistics {
            collected_stats: Mutex::new(Vec::new()),
        }
    }

    /// Port of `PipelineStatistics::Collect`.
    pub fn collect(&self, _pipeline: vk::Pipeline) {
        todo!("PipelineStatistics::collect")
    }

    /// Port of `PipelineStatistics::Report`.
    pub fn report(&self) {
        todo!("PipelineStatistics::report")
    }
}
