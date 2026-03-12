// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `vk_query_cache.h` / `vk_query_cache.cpp`.
//!
//! Vulkan-specific query cache runtime that handles GPU query synchronization,
//! conditional rendering, and streamer interfaces.

use ash::vk;

// ---------------------------------------------------------------------------
// QueryCacheRuntime
// ---------------------------------------------------------------------------

/// Port of `QueryCacheRuntime` class.
///
/// Manages host conditional rendering, query barriers, value synchronization,
/// and streamer interfaces for all query types.
pub struct QueryCacheRuntime {
    _private: (), // Upstream uses a pimpl (QueryCacheRuntimeImpl)
}

impl QueryCacheRuntime {
    /// Port of `QueryCacheRuntime::QueryCacheRuntime`.
    pub fn new() -> Self {
        todo!("QueryCacheRuntime::new")
    }

    /// Port of `QueryCacheRuntime::Barriers`.
    pub fn barriers(&mut self, _is_prebarrier: bool) {
        todo!("QueryCacheRuntime::barriers")
    }

    /// Port of `QueryCacheRuntime::EndHostConditionalRendering`.
    pub fn end_host_conditional_rendering(&mut self) {
        todo!("QueryCacheRuntime::end_host_conditional_rendering")
    }

    /// Port of `QueryCacheRuntime::PauseHostConditionalRendering`.
    pub fn pause_host_conditional_rendering(&mut self) {
        todo!("QueryCacheRuntime::pause_host_conditional_rendering")
    }

    /// Port of `QueryCacheRuntime::ResumeHostConditionalRendering`.
    pub fn resume_host_conditional_rendering(&mut self) {
        todo!("QueryCacheRuntime::resume_host_conditional_rendering")
    }

    /// Port of `QueryCacheRuntime::HostConditionalRenderingCompareValue`.
    pub fn host_conditional_rendering_compare_value(&mut self, _qc_dirty: bool) -> bool {
        todo!("QueryCacheRuntime::host_conditional_rendering_compare_value")
    }

    /// Port of `QueryCacheRuntime::HostConditionalRenderingCompareValues`.
    pub fn host_conditional_rendering_compare_values(
        &mut self,
        _qc_dirty: bool,
        _equal_check: bool,
    ) -> bool {
        todo!("QueryCacheRuntime::host_conditional_rendering_compare_values")
    }

    /// Port of `QueryCacheRuntime::GetStreamerInterface`.
    pub fn get_streamer_interface(&self, _query_type: u32) {
        todo!("QueryCacheRuntime::get_streamer_interface")
    }

    /// Port of `QueryCacheRuntime::Bind3DEngine`.
    pub fn bind_3d_engine(&mut self) {
        todo!("QueryCacheRuntime::bind_3d_engine")
    }
}

// ---------------------------------------------------------------------------
// QueryCache type alias
// ---------------------------------------------------------------------------

// In upstream: `using QueryCache = VideoCommon::QueryCacheBase<QueryCacheParams>;`
// The generic QueryCacheBase is not yet ported; this serves as a placeholder.

/// Placeholder for the Vulkan query cache instantiation.
/// Port of `QueryCache` type alias.
pub struct QueryCache {
    pub runtime: QueryCacheRuntime,
}
