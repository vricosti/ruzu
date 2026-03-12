// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/query_cache/query_stream.h
//!
//! Defines `StreamerInterface` (the base trait/struct for all query streamers)
//! and `SimpleStreamer<Q>` (a generic streamer backed by a slot-based query pool).

use std::collections::VecDeque;
use std::sync::Mutex;

use super::query_base::{QueryBase, VAddr};

/// Base interface for query streamers.
///
/// Maps to C++ `StreamerInterface`. In the C++ code this is an abstract base class
/// with virtual methods. In Rust we use a trait for the pure-virtual parts and a
/// concrete struct for the shared state.
pub trait StreamerOps {
    /// Retrieve a query by ID. Returns `None` if the ID is invalid.
    fn get_query(&self, id: usize) -> Option<&QueryBase>;

    /// Retrieve a mutable query by ID.
    fn get_query_mut(&mut self, id: usize) -> Option<&mut QueryBase>;

    /// Start counting for this streamer.
    fn start_counter(&mut self) {
        // Default: do nothing
    }

    /// Pause counting.
    fn pause_counter(&mut self) {
        // Default: do nothing
    }

    /// Reset the counter.
    fn reset_counter(&mut self) {
        // Default: do nothing
    }

    /// Close the counter.
    fn close_counter(&mut self) {
        // Default: do nothing
    }

    /// Returns true if there are pending sync operations.
    fn has_pending_sync(&self) -> bool {
        false
    }

    /// Pre-sync writes.
    fn presync_writes(&mut self) {
        // Default: do nothing
    }

    /// Sync writes to the host.
    fn sync_writes(&mut self) {
        // Default: do nothing
    }

    /// Write a counter value at the given address.
    fn write_counter(
        &mut self,
        address: VAddr,
        has_timestamp: bool,
        value: u32,
        subreport: Option<u32>,
    ) -> usize;

    /// Returns true if there are unsynced queries.
    fn has_unsynced_queries(&self) -> bool {
        false
    }

    /// Push unsynced queries for flushing.
    fn push_unsynced_queries(&mut self) {
        // Default: do nothing
    }

    /// Pop unsynced queries after flushing.
    fn pop_unsynced_queries(&mut self) {
        // Default: do nothing
    }

    /// Free a query by ID.
    fn free(&mut self, query_id: usize);
}

/// Shared state for all streamer types, corresponding to the non-virtual
/// members of C++ `StreamerInterface`.
#[derive(Debug)]
pub struct StreamerState {
    /// The streamer's unique ID.
    pub id: usize,
    /// Bitmask of streamers this one depends on.
    pub dependence_mask: u64,
    /// Bitmask of streamers that depend on this one.
    pub dependent_mask: u64,
    /// Amendment value applied to query results.
    pub amend_value: u64,
    /// Accumulated value for this streamer.
    pub accumulation_value: u64,
}

impl StreamerState {
    /// Create a new streamer state with the given ID.
    pub fn new(id: usize) -> Self {
        Self {
            id,
            dependence_mask: 0,
            dependent_mask: 0,
            amend_value: 0,
            accumulation_value: 0,
        }
    }

    /// Get this streamer's ID.
    pub fn get_id(&self) -> usize {
        self.id
    }

    /// Get the dependence mask.
    pub fn get_dependence_mask(&self) -> u64 {
        self.dependence_mask
    }

    /// Get the dependent mask.
    ///
    /// NOTE: upstream C++ `GetDependentMask()` returns `dependence_mask` (likely a bug),
    /// preserved here for parity.
    pub fn get_dependent_mask(&self) -> u64 {
        self.dependence_mask
    }

    /// Get the amend value.
    pub fn get_amend_value(&self) -> u64 {
        self.amend_value
    }

    /// Set the accumulation value.
    pub fn set_accumulation_value(&mut self, new_value: u64) {
        self.accumulation_value = new_value;
    }

    /// Register a dependency on another streamer.
    ///
    /// Maps to C++ `StreamerInterface::MakeDependent`.
    pub fn make_dependent(&mut self, other_id: usize, other_dependent_mask: &mut u64) {
        self.dependence_mask |= 1u64 << other_id;
        *other_dependent_mask |= 1u64 << self.id;
    }
}

/// A simple slot-based streamer that stores queries in a `VecDeque` and
/// recycles freed slot indices.
///
/// Maps to C++ `SimpleStreamer<QueryType>`. The type parameter `Q` must
/// be constructible and convertible to/from `QueryBase`.
pub struct SimpleStreamer<Q> {
    /// Shared streamer state.
    pub state: StreamerState,
    /// Guard for thread-safe slot allocation/release.
    guard: Mutex<()>,
    /// Query storage indexed by slot ID.
    pub slot_queries: VecDeque<Q>,
    /// Recycled slot indices available for reuse.
    old_queries: VecDeque<usize>,
}

impl<Q> SimpleStreamer<Q> {
    /// Create a new simple streamer with the given ID.
    pub fn new(id: usize) -> Self {
        Self {
            state: StreamerState::new(id),
            guard: Mutex::new(()),
            slot_queries: VecDeque::new(),
            old_queries: VecDeque::new(),
        }
    }

    /// Allocate a new query slot, recycling old slots when available.
    ///
    /// Maps to C++ `SimpleStreamer::BuildQuery`.
    pub fn build_query(&mut self, query: Q) -> usize {
        let _lock = self.guard.lock().unwrap();
        if let Some(reuse_id) = self.old_queries.pop_front() {
            self.slot_queries[reuse_id] = query;
            reuse_id
        } else {
            let new_id = self.slot_queries.len();
            self.slot_queries.push_back(query);
            new_id
        }
    }

    /// Release a query slot for recycling.
    ///
    /// Maps to C++ `SimpleStreamer::ReleaseQuery`.
    pub fn release_query(&mut self, query_id: usize) {
        assert!(
            query_id < self.slot_queries.len(),
            "SimpleStreamer::release_query: query_id {} out of bounds (len {})",
            query_id,
            self.slot_queries.len()
        );
        self.old_queries.push_back(query_id);
    }

    /// Free a query (thread-safe wrapper around `release_query`).
    ///
    /// Maps to C++ `SimpleStreamer::Free`.
    pub fn free(&mut self, query_id: usize) {
        // Note: upstream locks guard here, but &mut self already guarantees
        // exclusive access in Rust.
        self.release_query(query_id);
    }

    /// Get a reference to a query by slot ID.
    pub fn get_query(&self, query_id: usize) -> Option<&Q> {
        self.slot_queries.get(query_id)
    }

    /// Get a mutable reference to a query by slot ID.
    pub fn get_query_mut(&mut self, query_id: usize) -> Option<&mut Q> {
        self.slot_queries.get_mut(query_id)
    }
}
