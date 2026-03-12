// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/query_cache/query_cache.h
//!
//! Contains the full template implementation of the query cache: `GuestStreamer`,
//! `StubStreamer`, `SyncValuesStruct`, and the `QueryCacheBaseImpl` inner type
//! that holds runtime state, streamer references, and pending flush tracking.

use std::collections::VecDeque;
use std::sync::Mutex;

use super::query_base::{GuestQuery, QueryBase, QueryFlagBits, VAddr};
use super::query_cache_base::{QueryCacheBase, QueryLocation};
use super::query_stream::{SimpleStreamer, StreamerOps, StreamerState};
use super::types::{QueryType, MAX_QUERY_TYPES};

/// Sync payload written back to guest memory.
///
/// Maps to C++ `SyncValuesStruct`.
#[derive(Debug, Clone)]
pub struct SyncValuesStruct {
    pub address: VAddr,
    pub value: u64,
    pub size: u64,
}

impl SyncValuesStruct {
    /// Whether this struct type generates a base buffer (used by runtime sync).
    pub const GENERATES_BASE_BUFFER: bool = true;
}

/// A streamer that writes guest-provided values back to guest memory.
///
/// Maps to C++ `GuestStreamer<Traits>`.
pub struct GuestStreamer {
    /// The underlying simple streamer for slot management.
    pub simple: SimpleStreamer<GuestQuery>,
    /// Queue of query IDs pending synchronization.
    pending_sync: VecDeque<usize>,
}

impl GuestStreamer {
    /// Create a new guest streamer with the given ID.
    pub fn new(id: usize) -> Self {
        Self {
            simple: SimpleStreamer::new(id),
            pending_sync: VecDeque::new(),
        }
    }
}

impl StreamerOps for GuestStreamer {
    fn get_query(&self, id: usize) -> Option<&QueryBase> {
        self.simple.get_query(id).map(|gq| &gq.base)
    }

    fn get_query_mut(&mut self, id: usize) -> Option<&mut QueryBase> {
        self.simple.get_query_mut(id).map(|gq| &mut gq.base)
    }

    fn write_counter(
        &mut self,
        address: VAddr,
        has_timestamp: bool,
        value: u32,
        _subreport: Option<u32>,
    ) -> usize {
        let query = GuestQuery::new(has_timestamp, address, value as u64);
        let new_id = self.simple.build_query(query);
        self.pending_sync.push_back(new_id);
        new_id
    }

    fn has_pending_sync(&self) -> bool {
        !self.pending_sync.is_empty()
    }

    fn sync_writes(&mut self) {
        if self.pending_sync.is_empty() {
            return;
        }
        let mut sync_values: Vec<SyncValuesStruct> = Vec::with_capacity(self.pending_sync.len());
        for &pending_id in &self.pending_sync {
            if let Some(query) = self.simple.slot_queries.get_mut(pending_id) {
                if query.base.flags.intersects(QueryFlagBits::IS_REWRITTEN)
                    || query.base.flags.intersects(QueryFlagBits::IS_INVALIDATED)
                {
                    continue;
                }
                query.base.flags |= QueryFlagBits::IS_HOST_SYNCED;
                let size = if query.base.flags.intersects(QueryFlagBits::HAS_TIMESTAMP) {
                    8u64
                } else {
                    4u64
                };
                sync_values.push(SyncValuesStruct {
                    address: query.base.guest_address,
                    value: query.base.value,
                    size,
                });
            }
        }
        self.pending_sync.clear();
        if !sync_values.is_empty() {
            // TODO: call runtime.sync_values(sync_values) when runtime is wired up
            let _ = sync_values;
        }
    }

    fn free(&mut self, query_id: usize) {
        self.simple.free(query_id);
    }
}

/// A streamer that replaces the actual counter value with a fixed stub value.
///
/// Maps to C++ `StubStreamer<Traits>`.
pub struct StubStreamer {
    /// The underlying guest streamer.
    pub guest: GuestStreamer,
    /// The fixed value to use instead of the real counter.
    stub_value: u32,
}

impl StubStreamer {
    /// Create a new stub streamer with the given ID and stub value.
    pub fn new(id: usize, stub_value: u32) -> Self {
        Self {
            guest: GuestStreamer::new(id),
            stub_value,
        }
    }
}

impl StreamerOps for StubStreamer {
    fn get_query(&self, id: usize) -> Option<&QueryBase> {
        self.guest.get_query(id)
    }

    fn get_query_mut(&mut self, id: usize) -> Option<&mut QueryBase> {
        self.guest.get_query_mut(id)
    }

    fn write_counter(
        &mut self,
        address: VAddr,
        has_timestamp: bool,
        _value: u32,
        subreport: Option<u32>,
    ) -> usize {
        self.guest
            .write_counter(address, has_timestamp, self.stub_value, subreport)
    }

    fn has_pending_sync(&self) -> bool {
        self.guest.has_pending_sync()
    }

    fn sync_writes(&mut self) {
        self.guest.sync_writes();
    }

    fn free(&mut self, query_id: usize) {
        self.guest.free(query_id);
    }
}

/// Internal implementation state for `QueryCacheBase`, holding runtime references,
/// streamer pointers, and pending flush queues.
///
/// Maps to C++ `QueryCacheBase<Traits>::QueryCacheBaseImpl`.
pub struct QueryCacheBaseImpl {
    /// Bitmask of active streamers.
    pub streamer_mask: u64,
    /// Mutex guarding flush operations.
    pub flush_guard: Mutex<()>,
    /// Queue of pending flush bitmasks.
    pub flushes_pending: VecDeque<u64>,
    /// Query locations pending unregistration.
    pub pending_unregister: Vec<QueryLocation>,
}

impl QueryCacheBaseImpl {
    /// Create a new impl. The caller is responsible for populating streamers.
    pub fn new() -> Self {
        Self {
            streamer_mask: 0,
            flush_guard: Mutex::new(()),
            flushes_pending: VecDeque::new(),
            pending_unregister: Vec::new(),
        }
    }

    /// Iterate over streamers identified by `mask`, calling `func` for each.
    ///
    /// Maps to C++ `QueryCacheBaseImpl::ForEachStreamerIn`.
    pub fn for_each_streamer_in<F>(&self, mut mask: u64, mut func: F)
    where
        F: FnMut(usize) -> bool,
    {
        while mask != 0 {
            let position = mask.trailing_zeros() as usize;
            mask &= !(1u64 << position);
            if func(position) {
                return;
            }
        }
    }

    /// Iterate over all active streamers.
    ///
    /// Maps to C++ `QueryCacheBaseImpl::ForEachStreamer`.
    pub fn for_each_streamer<F>(&self, func: F)
    where
        F: FnMut(usize) -> bool,
    {
        self.for_each_streamer_in(self.streamer_mask, func);
    }
}

impl Default for QueryCacheBaseImpl {
    fn default() -> Self {
        Self::new()
    }
}
