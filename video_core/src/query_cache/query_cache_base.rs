// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/query_cache/query_cache_base.h
//!
//! Defines `QueryCacheBase`, the core query-cache manager that tracks cached
//! queries by page address, handles invalidation, flushing, and region-dirty
//! checks.  In C++ this is a CRTP template class; in Rust it is a concrete
//! struct parameterized by a `QueryCacheTraits` trait.

use std::collections::HashMap;
use std::sync::Mutex;

use super::query_base::{QueryBase, VAddr};
use super::query_stream::StreamerOps;
use super::types::{QueryPropertiesFlags, QueryType};

// Re-export for convenience — upstream uses `Core::DEVICE_PAGEBITS`.
// The constant lives in `core::device_memory_manager` in the Rust port.
// We duplicate the value here to avoid a crate dependency cycle.
const DEVICE_PAGEBITS: u64 = 12;
const DEVICE_PAGEMASK: u64 = (1 << DEVICE_PAGEBITS) - 1;

/// GPU virtual address type alias.
pub type GPUVAddr = u64;

/// Lookup result used by conditional rendering acceleration.
///
/// Maps to C++ `LookupData`.
#[derive(Debug)]
pub struct LookupData {
    pub address: VAddr,
    pub found_query: Option<*const QueryBase>,
}

/// Packed location identifying a query within a specific streamer.
///
/// Maps to C++ `QueryCacheBase::QueryLocation` union.
///
/// Layout (matching C++ `BitField`):
///   - bits \[0..27): `query_id`
///   - bits \[27..32): `stream_id`
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct QueryLocation {
    pub raw: u32,
}

impl QueryLocation {
    /// Number of bits used for `query_id`.
    const QUERY_ID_BITS: u32 = 27;
    const QUERY_ID_MASK: u32 = (1 << Self::QUERY_ID_BITS) - 1;

    /// Create a new `QueryLocation` from stream and query IDs.
    pub fn new(stream_id: u32, query_id: u32) -> Self {
        debug_assert!(stream_id < 32, "stream_id must fit in 5 bits");
        debug_assert!(
            query_id <= Self::QUERY_ID_MASK,
            "query_id must fit in 27 bits"
        );
        Self {
            raw: (stream_id << Self::QUERY_ID_BITS) | (query_id & Self::QUERY_ID_MASK),
        }
    }

    /// Extract the stream ID (bits 27..32).
    pub fn stream_id(&self) -> usize {
        (self.raw >> Self::QUERY_ID_BITS) as usize
    }

    /// Extract the query ID (bits 0..27).
    pub fn query_id(&self) -> usize {
        (self.raw & Self::QUERY_ID_MASK) as usize
    }

    /// Unpack into `(stream_id, query_id)`.
    ///
    /// Maps to C++ `QueryLocation::unpack`.
    pub fn unpack(&self) -> (usize, usize) {
        (self.stream_id(), self.query_id())
    }
}

/// Trait that must be implemented by the runtime-specific query cache.
///
/// Maps to the `Traits` template parameter of C++ `QueryCacheBase<Traits>`.
pub trait QueryCacheTraits {
    /// The runtime type (e.g., Vulkan or OpenGL query cache runtime).
    type Runtime;

    /// Get a streamer interface by query type.
    fn get_streamer(runtime: &Self::Runtime, query_type: QueryType) -> Option<&dyn StreamerOps>;
}

/// Page-indexed cache of query locations.
///
/// Outer key: page number (`address >> DEVICE_PAGEBITS`).
/// Inner key: page offset (`address & DEVICE_PAGEMASK`), as `u32`.
/// Value: the `QueryLocation` that wrote to that address.
pub type ContentCache = HashMap<u64, HashMap<u32, QueryLocation>>;

/// Core query cache state shared across all backend implementations.
///
/// Maps to C++ `QueryCacheBase<Traits>` (non-template-dependent state).
pub struct QueryCacheBase {
    /// Page-indexed cache of active query locations.
    pub cached_queries: ContentCache,
    /// Mutex protecting `cached_queries`.
    pub cache_mutex: Mutex<()>,
}

impl QueryCacheBase {
    /// Create a new empty query cache.
    pub fn new() -> Self {
        Self {
            cached_queries: HashMap::new(),
            cache_mutex: Mutex::new(()),
        }
    }

    /// Invalidate all cached queries overlapping `[addr, addr+size)`.
    ///
    /// Maps to C++ `QueryCacheBase::InvalidateRegion`.
    pub fn invalidate_region(&mut self, _addr: VAddr, _size: usize) {
        todo!("QueryCacheBase::invalidate_region")
    }

    /// Flush all cached queries overlapping `[addr, addr+size)`.
    ///
    /// Maps to C++ `QueryCacheBase::FlushRegion`.
    pub fn flush_region(&mut self, _addr: VAddr, _size: usize) {
        todo!("QueryCacheBase::flush_region")
    }

    /// Build a bitmask from a slice of query types.
    ///
    /// Maps to C++ `QueryCacheBase::BuildMask`.
    pub fn build_mask(types: &[QueryType]) -> u64 {
        let mut mask: u64 = 0;
        for &query_type in types {
            mask |= 1u64 << (query_type as u64);
        }
        mask
    }

    /// Return true when a CPU region has been modified from the GPU.
    ///
    /// Maps to C++ `QueryCacheBase::IsRegionGpuModified`.
    pub fn is_region_gpu_modified(&self, _addr: VAddr, _size: usize) -> bool {
        todo!("QueryCacheBase::is_region_gpu_modified")
    }

    /// Enable or disable a counter.
    ///
    /// Maps to C++ `QueryCacheBase::CounterEnable`.
    pub fn counter_enable(&mut self, _counter_type: QueryType, _is_enabled: bool) {
        todo!("QueryCacheBase::counter_enable")
    }

    /// Reset a counter.
    ///
    /// Maps to C++ `QueryCacheBase::CounterReset`.
    pub fn counter_reset(&mut self, _counter_type: QueryType) {
        todo!("QueryCacheBase::counter_reset")
    }

    /// Close a counter.
    ///
    /// Maps to C++ `QueryCacheBase::CounterClose`.
    pub fn counter_close(&mut self, _counter_type: QueryType) {
        todo!("QueryCacheBase::counter_close")
    }

    /// Report a counter value to the given GPU virtual address.
    ///
    /// Maps to C++ `QueryCacheBase::CounterReport`.
    pub fn counter_report(
        &mut self,
        _addr: GPUVAddr,
        _counter_type: QueryType,
        _flags: QueryPropertiesFlags,
        _payload: u32,
        _subreport: u32,
    ) {
        todo!("QueryCacheBase::counter_report")
    }

    /// Notify that a Wait-For-Idle has been issued.
    ///
    /// Maps to C++ `QueryCacheBase::NotifyWFI`.
    pub fn notify_wfi(&mut self) {
        todo!("QueryCacheBase::notify_wfi")
    }

    /// Attempt to use host-side conditional rendering.
    ///
    /// Maps to C++ `QueryCacheBase::AccelerateHostConditionalRendering`.
    pub fn accelerate_host_conditional_rendering(&mut self) -> bool {
        todo!("QueryCacheBase::accelerate_host_conditional_rendering")
    }

    /// Commit pending async flushes.
    ///
    /// Maps to C++ `QueryCacheBase::CommitAsyncFlushes`.
    pub fn commit_async_flushes(&mut self) {
        todo!("QueryCacheBase::commit_async_flushes")
    }

    /// Check if there are uncommitted flushes.
    ///
    /// Maps to C++ `QueryCacheBase::HasUncommittedFlushes`.
    pub fn has_uncommitted_flushes(&self) -> bool {
        todo!("QueryCacheBase::has_uncommitted_flushes")
    }

    /// Check if we should wait for async flushes.
    ///
    /// Maps to C++ `QueryCacheBase::ShouldWaitAsyncFlushes`.
    pub fn should_wait_async_flushes(&self) -> bool {
        todo!("QueryCacheBase::should_wait_async_flushes")
    }

    /// Pop completed async flushes.
    ///
    /// Maps to C++ `QueryCacheBase::PopAsyncFlushes`.
    pub fn pop_async_flushes(&mut self) {
        todo!("QueryCacheBase::pop_async_flushes")
    }

    /// Notify a GPU segment transition (pause/resume).
    ///
    /// Maps to C++ `QueryCacheBase::NotifySegment`.
    pub fn notify_segment(&mut self, _resume: bool) {
        todo!("QueryCacheBase::notify_segment")
    }

    /// Iterate over cached queries in the given address range.
    ///
    /// If `remove_from_cache` is true, matching entries are removed after iteration.
    ///
    /// Maps to C++ `QueryCacheBase::IterateCache`.
    pub fn iterate_cache<F>(
        &mut self,
        addr: VAddr,
        size: usize,
        remove_from_cache: bool,
        mut func: F,
    ) where
        F: FnMut(QueryLocation) -> bool,
    {
        let addr_begin = addr;
        let addr_end = addr_begin + size as u64;
        let page_end = addr_end >> DEVICE_PAGEBITS;

        let _lock = self.cache_mutex.lock().unwrap();
        let mut page = addr_begin >> DEVICE_PAGEBITS;
        while page <= page_end {
            let page_start = page << DEVICE_PAGEBITS;

            let in_range = |query_offset: u32| -> bool {
                let cache_begin = page_start + query_offset as u64;
                let cache_end = cache_begin + std::mem::size_of::<u32>() as u64;
                cache_begin < addr_end && addr_begin < cache_end
            };

            if let Some(contents) = self.cached_queries.get_mut(&page) {
                let mut early_exit = false;
                for (&offset, &location) in contents.iter() {
                    if !in_range(offset) {
                        continue;
                    }
                    if func(location) {
                        early_exit = true;
                        break;
                    }
                }

                if remove_from_cache {
                    contents.retain(|&offset, _| !in_range(offset));
                }

                if early_exit {
                    return;
                }
            }

            page += 1;
        }
    }

    /// Invalidate a single query by location.
    ///
    /// Maps to C++ `QueryCacheBase::InvalidateQuery`.
    pub fn invalidate_query_at(_location: QueryLocation) {
        todo!("QueryCacheBase::invalidate_query_at")
    }

    /// Check if a query is dirty (host-managed but not guest-synced).
    ///
    /// Maps to C++ `QueryCacheBase::IsQueryDirty`.
    pub fn is_query_dirty(_location: QueryLocation) -> bool {
        todo!("QueryCacheBase::is_query_dirty")
    }

    /// Semi-flush a dirty query (write final value to guest memory if available).
    ///
    /// Maps to C++ `QueryCacheBase::SemiFlushQueryDirty`.
    pub fn semi_flush_query_dirty(_location: QueryLocation) -> bool {
        todo!("QueryCacheBase::semi_flush_query_dirty")
    }

    /// Request a guest-host synchronization.
    ///
    /// Maps to C++ `QueryCacheBase::RequestGuestHostSync`.
    pub fn request_guest_host_sync(&self) {
        todo!("QueryCacheBase::request_guest_host_sync")
    }

    /// Unregister queries that have been processed.
    ///
    /// Maps to C++ `QueryCacheBase::UnregisterPending`.
    pub fn unregister_pending(&mut self) {
        todo!("QueryCacheBase::unregister_pending")
    }
}

impl Default for QueryCacheBase {
    fn default() -> Self {
        Self::new()
    }
}
