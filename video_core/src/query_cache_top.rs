// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/query_cache.h
//!
//! Shared legacy query-cache owners used by backend query cache implementations.

use std::collections::{HashMap, VecDeque};
use std::sync::{Arc, Mutex};

use crate::control::channel_state::ChannelState;
use crate::control::channel_state_cache::{ChannelInfo, ChannelSetupCaches};

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

/// Async flush bookkeeping.
#[derive(Debug, Clone, Default)]
pub struct AsyncJob {
    pub collected: bool,
    pub value: u64,
    pub query_location: u64,
    pub timestamp: Option<u64>,
}

/// Shared operations required by `CounterStreamBase`, `HostCounterBase`, and
/// `CachedQueryBase` to manipulate backend-specific host counter handles.
pub trait CounterHandle: Clone {
    fn query(&self, r#async: bool) -> u64;
    fn wait_pending(&self) -> bool;
    fn depth(&self) -> u64;
    fn end_query(&self, any_command_queued: bool);
}

/// Shared dependency/result state for backend host counters.
pub struct HostCounterBase<H: CounterHandle> {
    pub dependency: Option<H>,
    pub result: Option<u64>,
    pub depth: u64,
    pub base_result: u64,
}

impl<H: CounterHandle> HostCounterBase<H> {
    pub fn new(mut dependency: Option<H>) -> Self {
        let mut depth = dependency.as_ref().map(|dep| dep.depth() + 1).unwrap_or(0);
        let mut base_result = 0;
        if depth > 96 {
            depth = 0;
            if let Some(dep) = dependency.take() {
                base_result = dep.query(false);
            }
        }
        Self {
            dependency,
            result: None,
            depth,
            base_result,
        }
    }

    pub fn query<F>(&mut self, r#async: bool, blocking_query: F) -> u64
    where
        F: FnOnce(bool) -> u64,
    {
        if let Some(result) = self.result {
            return result;
        }
        let mut value = blocking_query(r#async) + self.base_result;
        if let Some(dep) = self.dependency.take() {
            value += dep.query(r#async);
        }
        self.result = Some(value);
        value
    }

    pub fn wait_pending(&self) -> bool {
        self.result.is_some()
    }

    pub fn depth(&self) -> u64 {
        self.depth
    }
}

/// Shared guest-mapped query state.
pub struct CachedQueryBase<H: CounterHandle> {
    pub cpu_addr: u64,
    pub gpu_addr: u64,
    pub counter: Option<H>,
    pub timestamp: Option<u64>,
    pub assigned_async_job: AsyncJobId,
}

impl<H: CounterHandle> CachedQueryBase<H> {
    pub fn new(cpu_addr: u64, gpu_addr: u64) -> Self {
        Self {
            cpu_addr,
            gpu_addr,
            counter: None,
            timestamp: None,
            assigned_async_job: NULL_ASYNC_JOB_ID,
        }
    }

    pub fn bind_counter<F>(
        &mut self,
        counter: Option<H>,
        timestamp: Option<u64>,
        flush_existing: F,
    ) -> Option<(AsyncJobId, u64)>
    where
        F: FnOnce() -> u64,
    {
        let result = if self.counter.is_some() {
            let async_job_id = self.assigned_async_job;
            Some((async_job_id, flush_existing()))
        } else {
            None
        };
        self.counter = counter;
        self.timestamp = timestamp;
        result
    }

    pub fn size_in_bytes(&self) -> u64 {
        if self.timestamp.is_some() {
            16
        } else {
            8
        }
    }
}

/// Shared per-query-type stream state.
pub struct CounterStreamBase<H: CounterHandle> {
    pub query_type: QueryType,
    pub current: Option<H>,
    pub last: Option<H>,
}

impl<H: CounterHandle> CounterStreamBase<H> {
    pub fn new(query_type: QueryType) -> Self {
        Self {
            query_type,
            current: None,
            last: None,
        }
    }

    pub fn reset_with(&mut self, any_command_queued: bool, new_current: Option<H>) {
        if let Some(current) = self.current.take() {
            current.end_query(any_command_queued);
        }
        self.current = new_current;
        self.last = None;
    }

    pub fn current_with(&mut self, any_command_queued: bool, new_current: H) -> Option<H> {
        let current = self.current.take()?;
        current.end_query(any_command_queued);
        self.last = Some(current);
        self.current = Some(new_current);
        self.last.clone()
    }

    pub fn is_enabled(&self) -> bool {
        self.current.is_some()
    }

    pub fn enable_with(&mut self, new_current: H) {
        if self.current.is_none() {
            self.current = Some(new_current);
        }
    }

    pub fn disable(&mut self, any_command_queued: bool) {
        if let Some(current) = self.current.as_ref() {
            current.end_query(any_command_queued);
        }
        self.last = self.current.take();
    }
}

/// Shared `QueryCacheLegacy<QueryCache, CachedQuery, CounterStream, HostCounter>`
/// state owned by backend query caches.
pub struct QueryCacheLegacy<Q, H: CounterHandle> {
    pub mutex: Mutex<()>,
    pub channel_caches: ChannelSetupCaches<ChannelInfo>,
    pub cached_queries: HashMap<u64, Vec<Q>>,
    pub streams: [CounterStreamBase<H>; NUM_QUERY_TYPES],
    pub slot_async_jobs: Arc<parking_lot::Mutex<HashMap<AsyncJobId, AsyncJob>>>,
    pub uncommitted_flushes: Option<Vec<AsyncJobId>>,
    pub committed_flushes: VecDeque<Option<Vec<AsyncJobId>>>,
    next_async_job_id: u32,
}

impl<Q, H: CounterHandle> QueryCacheLegacy<Q, H> {
    pub fn new() -> Self {
        Self {
            mutex: Mutex::new(()),
            channel_caches: ChannelSetupCaches::new(),
            cached_queries: HashMap::new(),
            streams: [
                CounterStreamBase::new(QueryType::SamplesPassed),
                CounterStreamBase::new(QueryType::PrimitivesGenerated),
                CounterStreamBase::new(QueryType::TfbPrimitivesWritten),
            ],
            slot_async_jobs: Arc::new(parking_lot::Mutex::new(HashMap::new())),
            uncommitted_flushes: None,
            committed_flushes: VecDeque::new(),
            next_async_job_id: 1,
        }
    }

    pub fn stream(&self, query_type: QueryType) -> &CounterStreamBase<H> {
        &self.streams[query_type as usize]
    }

    pub fn stream_mut(&mut self, query_type: QueryType) -> &mut CounterStreamBase<H> {
        &mut self.streams[query_type as usize]
    }

    pub fn alloc_async_job_id(&mut self) -> AsyncJobId {
        let id = AsyncJobId(self.next_async_job_id);
        self.next_async_job_id = self.next_async_job_id.wrapping_add(1).max(1);
        id
    }

    pub fn create_channel(&mut self, channel: &ChannelState) {
        self.channel_caches.create_channel(channel);
    }

    pub fn bind_to_channel(&mut self, id: i32) {
        self.channel_caches.bind_to_channel(id);
    }

    pub fn erase_channel(&mut self, id: i32) {
        self.channel_caches.erase_channel(id);
    }

    pub fn commit_async_flushes(&mut self) {
        self.committed_flushes
            .push_back(self.uncommitted_flushes.take());
    }

    pub fn has_uncommitted_flushes(&self) -> bool {
        self.uncommitted_flushes.is_some()
    }

    pub fn should_wait_async_flushes(&self) -> bool {
        matches!(self.committed_flushes.front(), Some(Some(_)))
    }
}

impl<Q, H: CounterHandle> Default for QueryCacheLegacy<Q, H> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Clone, Debug)]
    struct TestCounter {
        ended: std::sync::Arc<std::sync::Mutex<Vec<bool>>>,
        queried: std::sync::Arc<std::sync::Mutex<Vec<bool>>>,
        value: u64,
        depth: u64,
    }

    impl CounterHandle for TestCounter {
        fn query(&self, r#async: bool) -> u64 {
            self.queried.lock().unwrap().push(r#async);
            self.value
        }

        fn wait_pending(&self) -> bool {
            true
        }

        fn depth(&self) -> u64 {
            self.depth
        }

        fn end_query(&self, any_command_queued: bool) {
            self.ended.lock().unwrap().push(any_command_queued);
        }
    }

    #[test]
    fn counter_stream_current_ends_and_rotates() {
        let ended = std::sync::Arc::new(std::sync::Mutex::new(Vec::new()));
        let queried = std::sync::Arc::new(std::sync::Mutex::new(Vec::new()));
        let current = TestCounter {
            ended: ended.clone(),
            queried: queried.clone(),
            value: 1,
            depth: 0,
        };
        let next = TestCounter {
            ended,
            queried,
            value: 2,
            depth: 1,
        };
        let mut stream = CounterStreamBase::new(QueryType::SamplesPassed);
        stream.current = Some(current.clone());
        let previous = stream
            .current_with(true, next.clone())
            .expect("previous query");
        assert_eq!(previous.value, 1);
        assert_eq!(stream.last.as_ref().map(|counter| counter.value), Some(1));
        assert_eq!(
            stream.current.as_ref().map(|counter| counter.value),
            Some(2)
        );
        assert_eq!(*current.ended.lock().unwrap(), vec![true]);
    }

    #[test]
    fn host_counter_base_collapses_deep_dependencies() {
        let ended = std::sync::Arc::new(std::sync::Mutex::new(Vec::new()));
        let queried = std::sync::Arc::new(std::sync::Mutex::new(Vec::new()));
        let dependency = TestCounter {
            ended,
            queried: queried.clone(),
            value: 9,
            depth: 96,
        };
        let mut base = HostCounterBase::new(Some(dependency));
        let value = base.query(false, |_| 3);
        assert_eq!(value, 12);
        assert_eq!(*queried.lock().unwrap(), vec![false]);
        assert_eq!(base.depth(), 0);
    }
}
