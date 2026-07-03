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

use std::sync::Arc;

use ash::vk;

use crate::control::channel_state::ChannelState;
use crate::control::channel_state_cache::ChannelCacheAccessor;
use crate::query_cache::query_cache::QueryCacheRuntimeHandle;
use crate::query_cache::query_cache_base::{LookupData, QueryCacheBase};
use crate::query_cache::types::QueryPropertiesFlags;

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
    /// Whether a 3D engine has been bound through the query-cache owner path.
    ///
    /// Upstream binds a live `Maxwell3D*` here. The current Rust owner graph
    /// does not yet carry that engine reference, but the lifecycle edge still
    /// belongs to this runtime owner.
    bound_3d_engine: bool,
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
            bound_3d_engine: false,
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
        self.bound_3d_engine = true;
    }

    /// Returns whether host conditional rendering is currently active.
    pub fn is_host_conditional_rendering_active(&self) -> bool {
        self.host_conditional_rendering_active
    }

    pub fn is_3d_engine_bound(&self) -> bool {
        self.bound_3d_engine
    }
}

impl QueryCacheRuntimeHandle for QueryCacheRuntime {
    fn bind_3d_engine(&mut self) {
        QueryCacheRuntime::bind_3d_engine(self);
    }

    fn end_host_conditional_rendering(&mut self) {
        QueryCacheRuntime::end_host_conditional_rendering(self);
    }

    fn pause_host_conditional_rendering(&mut self) {
        QueryCacheRuntime::pause_host_conditional_rendering(self);
    }

    fn resume_host_conditional_rendering(&mut self) {
        QueryCacheRuntime::resume_host_conditional_rendering(self);
    }

    fn host_conditional_rendering_compare_value(
        &mut self,
        _object_1: LookupData,
        qc_dirty: bool,
    ) -> bool {
        QueryCacheRuntime::host_conditional_rendering_compare_value(self, qc_dirty)
    }

    fn host_conditional_rendering_compare_values(
        &mut self,
        _object_1: LookupData,
        _object_2: LookupData,
        qc_dirty: bool,
        equal_check: bool,
    ) -> bool {
        QueryCacheRuntime::host_conditional_rendering_compare_values(self, qc_dirty, equal_check)
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
    pub base: QueryCacheBase,
    pub runtime: QueryCacheRuntime,
    /// Channel-bound GPU device memory manager. Used to translate the
    /// query's GPU virtual address to the underlying CPU/guest address
    /// when writing the query result back. Mirrors the wiring in
    /// `gl_query_cache::QueryCache`.
    channel_memory_manager: Option<Arc<parking_lot::Mutex<crate::memory_manager::MemoryManager>>>,
    /// Source of the GPU tick counter for queries with timestamps.
    /// Mirrors `gl_query_cache::QueryCache::gpu_ticks_getter`.
    gpu_ticks_getter: Option<Arc<dyn Fn() -> u64 + Send + Sync>>,
}

impl QueryCache {
    pub fn new() -> Self {
        QueryCache {
            base: QueryCacheBase::new(),
            runtime: QueryCacheRuntime::new(),
            channel_memory_manager: None,
            gpu_ticks_getter: None,
        }
    }

    pub fn create_channel(&mut self, channel: &ChannelState) {
        self.base.create_channel(channel);
    }

    pub fn bind_to_channel(&mut self, channel_id: i32) {
        self.base.bind_to_channel(channel_id);
        self.runtime.bind_3d_engine();
        self.channel_memory_manager = self
            .base
            .channel_caches
            .current_channel_state()
            .and_then(ChannelCacheAccessor::gpu_memory_arc);
    }

    pub fn erase_channel(&mut self, channel_id: i32) {
        self.base.erase_channel(channel_id);
        self.channel_memory_manager = None;
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

    /// Wire the GPU tick getter used for timestamped queries.
    pub fn set_gpu_ticks_getter(&mut self, getter: Arc<dyn Fn() -> u64 + Send + Sync>) {
        self.gpu_ticks_getter = Some(getter);
    }

    pub fn reset_counter(&mut self, _query_type: u32) {}
    pub fn invalidate_region(&mut self, _addr: u64, _size: usize) {}
    pub fn flush_region(&mut self, _addr: u64, _size: usize) {}
    pub fn notify_wfi(&mut self) {
        self.base.notify_wfi();
    }
    pub fn commit_async_flushes(&mut self) {}
    pub fn has_uncommitted_flushes(&self) -> bool {
        false
    }
    pub fn should_wait_async_flushes(&self) -> bool {
        false
    }
    pub fn pop_async_flushes(&mut self) {}

    /// Port of upstream `RasterizerVulkan::Query` →
    /// `QueryCacheBase<Vulkan>::CounterReport`. Captures a write-back
    /// closure and enqueues it via the rasterizer-provided `signal_fence`
    /// (for fence queries) or `sync_operation` (for synchronous queries).
    /// The closure runs on the GPU fence release thread once the host GPU
    /// finishes the corresponding work, and writes the query result to
    /// the guest memory address that the game polls.
    ///
    /// Mirrors `gl_query_cache::QueryCache::query`.
    pub fn query(
        &mut self,
        gpu_addr: u64,
        flags: QueryPropertiesFlags,
        payload: u32,
        signal_fence: impl FnOnce(Box<dyn FnOnce() + Send>),
        sync_operation: impl FnOnce(Box<dyn FnOnce() + Send>),
    ) {
        let Some(memory_manager) = self.channel_memory_manager.as_ref().cloned() else {
            return;
        };
        let has_timeout = flags.contains(QueryPropertiesFlags::HAS_TIMEOUT);
        let is_fence = flags.contains(QueryPropertiesFlags::IS_A_FENCE);
        let gpu_ticks_getter = self.gpu_ticks_getter.as_ref().cloned();
        let operation = Box::new(move || {
            let mm = memory_manager.lock();
            if has_timeout {
                let gpu_ticks = gpu_ticks_getter
                    .as_ref()
                    .map(|getter| getter())
                    .unwrap_or(0);
                mm.write_block_unsafe(gpu_addr + 8, &gpu_ticks.to_le_bytes());
                mm.write_block_unsafe(gpu_addr, &(payload as u64).to_le_bytes());
            } else {
                mm.write_block_unsafe(gpu_addr, &payload.to_le_bytes());
            }
        });
        if is_fence {
            signal_fence(operation);
        } else {
            sync_operation(operation);
        }
    }

    #[cfg(test)]
    pub fn bound_memory_manager_for_test(
        &self,
    ) -> Option<&Arc<parking_lot::Mutex<crate::memory_manager::MemoryManager>>> {
        self.channel_memory_manager.as_ref()
    }

    #[cfg(test)]
    pub fn has_bound_memory_manager_for_test(&self) -> bool {
        self.channel_memory_manager.is_some()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
    use crate::memory_manager::MemoryManager;
    use crate::query_cache::query_cache::QueryCacheRuntimeHandle;
    use crate::query_cache::query_cache_base::LookupData;
    use parking_lot::Mutex as ParkingMutex;
    use std::sync::Arc;

    fn make_query_memory_manager(
        gpu_addr: u64,
        d_addr: u64,
        size: usize,
    ) -> (Arc<ParkingMutex<MemoryManager>>, Vec<u8>) {
        let device_memory = Arc::new(MaxwellDeviceMemoryManager::default());
        let mut backing = vec![0u8; size];
        device_memory.smmu_set_physical_base_for_test(backing.as_ptr() as usize);
        device_memory.smmu_map_with_cpu_backing(
            d_addr,
            backing.as_mut_ptr(),
            0x4000_0000,
            size,
            5,
            true,
        );

        let mut mm = MemoryManager::new_with_geometry_and_device_memory(
            0,
            Arc::clone(&device_memory),
            40,
            1u64 << 34,
            16,
            12,
        );
        mm.map(gpu_addr, d_addr, size as u64, 0, false);
        (Arc::new(ParkingMutex::new(mm)), backing)
    }

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

    #[test]
    fn runtime_trait_bridge_routes_conditional_rendering_hooks() {
        let mut rt = QueryCacheRuntime::new();
        let handle: &mut dyn QueryCacheRuntimeHandle = &mut rt;

        assert!(!handle.host_conditional_rendering_compare_value(
            LookupData {
                address: 0x1000,
                found_query: None,
            },
            false,
        ));
        assert!(!handle.host_conditional_rendering_compare_values(
            LookupData {
                address: 0x1000,
                found_query: None,
            },
            LookupData {
                address: 0x2000,
                found_query: None,
            },
            false,
            true,
        ));
        handle.end_host_conditional_rendering();
        assert!(!rt.is_host_conditional_rendering_active());
    }

    #[test]
    fn bind_to_channel_wires_memory_manager_from_channel_cache_owner() {
        let mut cache = QueryCache::new();
        let mm = Arc::new(ParkingMutex::new(MemoryManager::new(33)));
        let mut channel = ChannelState::new(8);
        channel.program_id = 0x3344;
        channel.memory_manager = Some(Arc::clone(&mm));

        cache.create_channel(&channel);
        cache.bind_to_channel(channel.bind_id);

        let bound = cache
            .bound_memory_manager_for_test()
            .expect("bound channel memory manager");
        assert!(Arc::ptr_eq(bound, &mm));
        assert_eq!(cache.base.channel_caches.program_id, 0x3344);
        assert!(cache.runtime.is_3d_engine_bound());

        cache.erase_channel(channel.bind_id);
        assert!(!cache.has_bound_memory_manager_for_test());
    }

    #[test]
    fn query_sync_operation_writes_payload_and_timestamp_through_bound_memory_manager() {
        let mut cache = QueryCache::new();
        let (mm, backing) = make_query_memory_manager(0x5038_50000, 0x5510_6000, 0x1000);

        let mut channel = ChannelState::new(9);
        channel.memory_manager = Some(Arc::clone(&mm));
        cache.create_channel(&channel);
        cache.bind_to_channel(channel.bind_id);
        cache.set_gpu_ticks_getter(Arc::new(|| 0x1122_3344_5566_7788));

        cache.query(
            0x5038_50000,
            QueryPropertiesFlags::HAS_TIMEOUT,
            0xAABB_CCDD,
            |_func| panic!("fence path should not be used"),
            |func| func(),
        );

        assert_eq!(&backing[0..8], &(0xAABB_CCDDu64).to_le_bytes());
        assert_eq!(&backing[8..16], &0x1122_3344_5566_7788u64.to_le_bytes());
    }

    #[test]
    fn query_fence_operation_defers_writeback_to_signal_fence_callback() {
        let mut cache = QueryCache::new();
        let (mm, backing) = make_query_memory_manager(0x5038_50000, 0x5510_6000, 0x1000);

        let mut channel = ChannelState::new(10);
        channel.memory_manager = Some(Arc::clone(&mm));
        cache.create_channel(&channel);
        cache.bind_to_channel(channel.bind_id);

        let deferred = Arc::new(ParkingMutex::new(None::<Box<dyn FnOnce() + Send>>));
        let deferred_clone = Arc::clone(&deferred);

        cache.query(
            0x5038_50000,
            QueryPropertiesFlags::IS_A_FENCE,
            0x1234_5678,
            move |func| {
                *deferred_clone.lock() = Some(func);
            },
            |_func| panic!("sync path should not be used"),
        );

        assert_eq!(&backing[0..4], &[0; 4]);

        let func = deferred.lock().take().expect("fence callback queued");
        func();

        assert_eq!(&backing[0..4], &0x1234_5678u32.to_le_bytes());
    }
}
