// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/query_cache.h
//!
//! Top-level query cache interface for GPU queries (samples passed, primitives
//! generated, transform feedback primitives written).

use std::collections::HashMap;
use std::sync::Mutex;

/// Query types supported by the GPU.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(usize)]
pub enum QueryType {
    SamplesPassed = 0,
    PrimitivesGenerated = 1,
    TfbPrimitivesWritten = 2,
    Count = 3,
}

/// Number of query types.
pub const NUM_QUERY_TYPES: usize = QueryType::Count as usize;

/// Slot ID for async jobs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct AsyncJobId(pub u32);

/// Null async job ID.
pub const NULL_ASYNC_JOB_ID: AsyncJobId = AsyncJobId(0);

/// Base class for host GPU counters.
///
/// In the full port, this is a trait with `blocking_query`, `end_query`, and
/// dependency chain management.
pub trait HostCounterBase {
    /// Returns the current value of the query.
    fn query(&mut self, r#async: bool) -> u64;

    /// Returns true when flushing this query will potentially wait.
    fn wait_pending(&self) -> bool;

    /// Returns the nesting depth.
    fn depth(&self) -> u64;
}

/// Base class for cached queries.
///
/// In the full port, this manages the guest memory address, host counter binding,
/// and flushing.
pub struct CachedQueryBase {
    pub cpu_addr: u64,
    pub host_ptr: *mut u8,
    pub timestamp: Option<u64>,
    pub assigned_async_job: AsyncJobId,
}

/// Counter stream that manages enable/disable/reset of a specific query type.
///
/// In the full port, this is parameterized over QueryCache and HostCounter types.
pub struct CounterStreamBase {
    pub query_type: QueryType,
    enabled: bool,
}

impl CounterStreamBase {
    pub fn new(query_type: QueryType) -> Self {
        Self {
            query_type,
            enabled: false,
        }
    }

    /// Returns true when the counter stream is enabled.
    pub fn is_enabled(&self) -> bool {
        self.enabled
    }

    /// Enables the stream.
    pub fn enable(&mut self) {
        self.enabled = true;
    }

    /// Disables the stream.
    pub fn disable(&mut self) {
        self.enabled = false;
    }

    /// Resets the stream to zero without disabling.
    pub fn reset(&mut self) {
        // NOTE: Full implementation ends the current HostCounter query and starts a new one
        // at value 0. This requires HostCounter integration (GPU query objects).
        // Stubbed: no-op until host counter integration is complete.
        log::warn!(
            "CounterStreamBase::reset: host counter not integrated, reset ignored for {:?}",
            self.query_type
        );
    }
}

/// Query cache legacy interface.
///
/// This manages GPU query objects, their caching, and async flush operations.
/// The full implementation requires RasterizerInterface, DeviceMemoryManager,
/// and concrete HostCounter/CachedQuery types.
pub struct QueryCacheLegacy {
    pub mutex: Mutex<()>,
    streams: [CounterStreamBase; NUM_QUERY_TYPES],
    // cached_queries: HashMap<u64, Vec<CachedQuery>>,
    // uncommitted_flushes: Option<Vec<AsyncJobId>>,
    // committed_flushes: VecDeque<Option<Vec<AsyncJobId>>>,
}

impl QueryCacheLegacy {
    pub fn new() -> Self {
        Self {
            mutex: Mutex::new(()),
            streams: [
                CounterStreamBase::new(QueryType::SamplesPassed),
                CounterStreamBase::new(QueryType::PrimitivesGenerated),
                CounterStreamBase::new(QueryType::TfbPrimitivesWritten),
            ],
        }
    }

    /// Invalidate a memory region.
    pub fn invalidate_region(&self, _addr: u64, _size: usize) {
        // NOTE: Full implementation walks the cached_queries map and removes
        // entries that overlap [addr, addr+size). Requires CachedQuery integration.
        // Stubbed until query cache implementation is complete.
    }

    /// Flush a memory region.
    pub fn flush_region(&self, _addr: u64, _size: usize) {
        // NOTE: Full implementation flushes any cached queries overlapping
        // [addr, addr+size) back to guest memory. Requires CachedQuery integration.
        // Stubbed until query cache implementation is complete.
    }

    /// Enable all counter streams.
    pub fn enable_counters(&mut self) {
        for stream in &mut self.streams {
            stream.enable();
        }
    }

    /// Reset a counter type.
    pub fn reset_counter(&mut self, query_type: QueryType) {
        self.streams[query_type as usize].reset();
    }

    /// Disable all active streams.
    pub fn disable_streams(&mut self) {
        for stream in &mut self.streams {
            stream.disable();
        }
    }

    /// Returns the stream for the given query type.
    pub fn stream(&self, query_type: QueryType) -> &CounterStreamBase {
        &self.streams[query_type as usize]
    }

    pub fn commit_async_flushes(&self) {
        // NOTE: Full implementation moves uncommitted_flushes into committed_flushes queue.
        // Stubbed until async flush queue is integrated.
    }

    pub fn has_uncommitted_flushes(&self) -> bool {
        false
    }

    pub fn should_wait_async_flushes(&self) -> bool {
        false
    }

    pub fn pop_async_flushes(&self) {
        // NOTE: Full implementation pops the front of committed_flushes and flushes
        // any associated CachedQuery objects. Stubbed until async flush queue is integrated.
    }
}

impl Default for QueryCacheLegacy {
    fn default() -> Self {
        Self::new()
    }
}
