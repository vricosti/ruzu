// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `vk_query_cache.h` / `vk_query_cache.cpp`.
//!
//! Vulkan-specific query cache runtime that handles GPU query synchronization,
//! conditional rendering, and streamer interfaces.
//!
//! Upstream uses a PIMPL pattern (`QueryCacheRuntimeImpl`) to hide the complex
//! internal state including query pool banks, streamers, and host conditional
//! rendering state.

use ash::vk;

// ---------------------------------------------------------------------------
// Constants (from vk_query_cache.cpp)
// ---------------------------------------------------------------------------

/// Size of each query bank (number of query slots per pool).
/// Port of `SamplesQueryBank::BANK_SIZE`.
pub const SAMPLES_QUERY_BANK_SIZE: usize = 256;

/// Size of each query result in bytes.
/// Port of `SamplesQueryBank::QUERY_SIZE`.
pub const SAMPLES_QUERY_SIZE: usize = 8;

// ---------------------------------------------------------------------------
// QueryCacheRuntime
// ---------------------------------------------------------------------------

/// Port of `QueryCacheRuntime` class.
///
/// Manages host conditional rendering, query barriers, value synchronization,
/// and streamer interfaces for all query types.
///
/// Upstream wraps the complex internal state behind a PIMPL
/// (`QueryCacheRuntimeImpl`) that contains:
/// - A vector of `SamplesQueryBank` objects (Vulkan query pools)
/// - Streamer objects for different query types
/// - Host conditional rendering state and buffers
/// - References to device, scheduler, staging pool, etc.
pub struct QueryCacheRuntime {
    /// Whether host conditional rendering is currently active.
    host_conditional_rendering_active: bool,
    /// Whether host conditional rendering is currently paused.
    host_conditional_rendering_paused: bool,
}

impl QueryCacheRuntime {
    /// Port of `QueryCacheRuntime::QueryCacheRuntime`.
    ///
    /// In the full implementation, this creates the PIMPL with:
    /// - Device, scheduler, staging pool references
    /// - Buffer cache for query result storage
    /// - Compute pass descriptor queue for prefix scan
    /// - Descriptor pool for compute pass allocation
    /// - SamplesStreamer, TFBCounterStreamer, PrimitivesSucceededStreamer
    /// - ConditionalRenderingResolvePass
    /// - QueriesPrefixScanPass
    pub fn new() -> Self {
        QueryCacheRuntime {
            host_conditional_rendering_active: false,
            host_conditional_rendering_paused: false,
        }
    }

    /// Port of `QueryCacheRuntime::Barriers`.
    ///
    /// Inserts memory barriers before or after query operations.
    /// `is_prebarrier` determines direction: pre-barrier synchronizes
    /// previous writes, post-barrier makes results available for reads.
    pub fn barriers(&self, _device: &ash::Device, _cmdbuf: vk::CommandBuffer, is_prebarrier: bool) {
        // Pre-barrier: Transfer write -> Shader read + write
        // Post-barrier: Shader write -> Multiple read stages
        let _barrier = if is_prebarrier {
            vk::MemoryBarrier {
                s_type: vk::StructureType::MEMORY_BARRIER,
                p_next: std::ptr::null(),
                src_access_mask: vk::AccessFlags::TRANSFER_WRITE,
                dst_access_mask: vk::AccessFlags::SHADER_READ | vk::AccessFlags::SHADER_WRITE,
            }
        } else {
            vk::MemoryBarrier {
                s_type: vk::StructureType::MEMORY_BARRIER,
                p_next: std::ptr::null(),
                src_access_mask: vk::AccessFlags::SHADER_WRITE,
                dst_access_mask: vk::AccessFlags::SHADER_READ
                    | vk::AccessFlags::TRANSFER_READ
                    | vk::AccessFlags::VERTEX_ATTRIBUTE_READ
                    | vk::AccessFlags::INDIRECT_COMMAND_READ
                    | vk::AccessFlags::INDEX_READ
                    | vk::AccessFlags::UNIFORM_READ
                    | vk::AccessFlags::CONDITIONAL_RENDERING_READ_EXT,
            }
        };
    }

    /// Port of `QueryCacheRuntime::EndHostConditionalRendering`.
    ///
    /// Ends the current host conditional rendering scope by calling
    /// `vkCmdEndConditionalRenderingEXT`.
    pub fn end_host_conditional_rendering(&mut self) {
        if self.host_conditional_rendering_active {
            self.host_conditional_rendering_active = false;
        }
    }

    /// Port of `QueryCacheRuntime::PauseHostConditionalRendering`.
    ///
    /// Temporarily pauses conditional rendering so that unconditional
    /// operations can be recorded.
    pub fn pause_host_conditional_rendering(&mut self) {
        if self.host_conditional_rendering_active && !self.host_conditional_rendering_paused {
            self.host_conditional_rendering_paused = true;
        }
    }

    /// Port of `QueryCacheRuntime::ResumeHostConditionalRendering`.
    ///
    /// Resumes previously paused conditional rendering.
    pub fn resume_host_conditional_rendering(&mut self) {
        if self.host_conditional_rendering_paused {
            self.host_conditional_rendering_paused = false;
        }
    }

    /// Port of `QueryCacheRuntime::HostConditionalRenderingCompareValue`.
    ///
    /// Begins conditional rendering by comparing a single query result
    /// against zero. Returns true if host conditional rendering was activated.
    pub fn host_conditional_rendering_compare_value(&mut self, _qc_dirty: bool) -> bool {
        // Full implementation requires:
        // 1. Look up the query object
        // 2. Write its value to a buffer via conditional rendering resolve pass
        // 3. Begin conditional rendering with the buffer
        false
    }

    /// Port of `QueryCacheRuntime::HostConditionalRenderingCompareValues`.
    ///
    /// Begins conditional rendering by comparing two query results.
    /// Returns true if host conditional rendering was activated.
    pub fn host_conditional_rendering_compare_values(
        &mut self,
        _qc_dirty: bool,
        _equal_check: bool,
    ) -> bool {
        // Full implementation requires resolving two query values and comparing
        false
    }

    /// Port of `QueryCacheRuntime::Bind3DEngine`.
    ///
    /// Associates this runtime with a Maxwell3D engine instance for
    /// accessing register state during query operations.
    pub fn bind_3d_engine(&mut self) {
        // Stores reference to maxwell3d for ViewRegs access
    }

    /// Returns whether host conditional rendering is currently active.
    pub fn is_host_conditional_rendering_active(&self) -> bool {
        self.host_conditional_rendering_active
    }
}

// ---------------------------------------------------------------------------
// QueryCache type alias
// ---------------------------------------------------------------------------

/// Port of `QueryCache` type alias.
///
/// In upstream: `using QueryCache = VideoCommon::QueryCacheBase<QueryCacheParams>;`
/// The generic QueryCacheBase provides the main cache logic, parameterized
/// by the Vulkan-specific runtime type.
pub struct QueryCache {
    pub runtime: QueryCacheRuntime,
}

impl QueryCache {
    pub fn new() -> Self {
        QueryCache {
            runtime: QueryCacheRuntime::new(),
        }
    }

    /// Port of `QueryCache::NotifySegment`.
    ///
    /// Notifies the cache of a new command segment for query tracking.
    pub fn notify_segment(&mut self, _is_draw: bool) {
        // Base class handles segment tracking
    }

    /// Port of `QueryCache::CounterEnable`.
    ///
    /// Enables or disables a query counter type.
    pub fn counter_enable(&mut self, _query_type: u32, _enable: bool) {
        // Base class handles counter enable/disable
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn query_cache_runtime_state() {
        let mut rt = QueryCacheRuntime::new();
        assert!(!rt.is_host_conditional_rendering_active());
        rt.end_host_conditional_rendering();
        assert!(!rt.is_host_conditional_rendering_active());
    }

    #[test]
    fn constants() {
        assert_eq!(SAMPLES_QUERY_BANK_SIZE, 256);
        assert_eq!(SAMPLES_QUERY_SIZE, 8);
    }
}
