// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_query_cache.h and gl_query_cache.cpp
//!
//! OpenGL query cache — manages GPU occlusion/primitives queries.

use std::sync::Arc;

use parking_lot::Mutex;

use crate::control::channel_state::ChannelState;
use crate::control::channel_state_cache::ChannelCacheAccessor;
use crate::query_cache_top::{
    AsyncJob, AsyncJobId, CachedQueryBase, CounterHandle, HostCounterBase, QueryCacheLegacy,
    QueryType, NUM_QUERY_TYPES,
};

/// Map a query type to the corresponding GL target.
///
/// Corresponds to the anonymous `GetTarget()` in gl_query_cache.cpp.
pub fn get_target(query_type: QueryType) -> u32 {
    match query_type {
        QueryType::SamplesPassed => gl::SAMPLES_PASSED,
        QueryType::PrimitivesGenerated => gl::PRIMITIVES_GENERATED,
        QueryType::TfbPrimitivesWritten => gl::TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN,
        QueryType::Count => unreachable!("QueryType::Count is not a concrete GL query target"),
    }
}

/// OpenGL query cache.
///
/// Corresponds to `OpenGL::QueryCache`.
pub struct QueryCache {
    /// Per-type pool of reusable query objects.
    query_pools: [Vec<u32>; NUM_QUERY_TYPES],
    channel_memory_manager: Option<Arc<parking_lot::Mutex<crate::memory_manager::MemoryManager>>>,
    gpu_ticks_getter: Option<Arc<dyn Fn() -> u64 + Send + Sync>>,
    legacy: QueryCacheLegacy<CachedQuery, HostCounterHandle>,
    commands_queued: bool,
}

impl QueryCache {
    /// Create a new query cache.
    ///
    /// Corresponds to `QueryCache::QueryCache()`.
    pub fn new() -> Self {
        let mut cache = Self {
            query_pools: [Vec::new(), Vec::new(), Vec::new()],
            channel_memory_manager: None,
            gpu_ticks_getter: None,
            legacy: QueryCacheLegacy::new(),
            commands_queued: false,
        };
        cache.enable_stream(QueryType::SamplesPassed);
        cache.enable_stream(QueryType::PrimitivesGenerated);
        cache.enable_stream(QueryType::TfbPrimitivesWritten);
        cache
    }

    /// Allocate a query object for the given type.
    ///
    /// Corresponds to `QueryCache::AllocateQuery()`.
    pub fn allocate_query(&mut self, query_type: QueryType) -> u32 {
        let pool = &mut self.query_pools[query_type as usize];
        if let Some(query) = pool.pop() {
            return query;
        }
        let mut query: u32 = 0;
        unsafe {
            gl::GenQueries(1, &mut query);
        }
        query
    }

    /// Return a query object to the pool.
    ///
    /// Corresponds to `QueryCache::Reserve()`.
    pub fn reserve(&mut self, query_type: QueryType, query: u32) {
        self.query_pools[query_type as usize].push(query);
    }

    pub fn set_gpu_ticks_getter(&mut self, getter: Arc<dyn Fn() -> u64 + Send + Sync>) {
        self.gpu_ticks_getter = Some(getter);
    }

    /// Port of `QueryCacheLegacy::CreateChannel`.
    pub fn create_channel(&mut self, channel: &ChannelState) {
        self.legacy.create_channel(channel);
    }

    /// Port of `QueryCacheLegacy::BindToChannel`.
    pub fn bind_to_channel(&mut self, channel_id: i32) {
        self.legacy.bind_to_channel(channel_id);
        self.channel_memory_manager = self
            .legacy
            .channel_caches
            .current_channel_state()
            .and_then(ChannelCacheAccessor::gpu_memory_arc);
    }

    /// Port of `QueryCacheLegacy::EraseChannel`.
    pub fn erase_channel(&mut self, channel_id: i32) {
        self.legacy.erase_channel(channel_id);
        self.channel_memory_manager = None;
    }

    pub fn reset_counter(&mut self, query_type: u32) {
        let query_type = match query_type {
            0 => QueryType::SamplesPassed,
            1 => QueryType::PrimitivesGenerated,
            2 => QueryType::TfbPrimitivesWritten,
            _ => return,
        };
        self.reset_stream(query_type);
    }

    pub fn set_commands_queued(&mut self, commands_queued: bool) {
        self.commands_queued = commands_queued;
    }

    pub fn invalidate_region(&mut self, addr: u64, size: usize) {
        self.flush_and_remove_region(addr, size, false);
    }

    pub fn flush_region(&mut self, addr: u64, size: usize) {
        self.flush_and_remove_region(addr, size, false);
    }

    pub fn commit_async_flushes(&mut self) {
        self.legacy.commit_async_flushes();
    }

    pub fn has_uncommitted_flushes(&self) -> bool {
        self.legacy.has_uncommitted_flushes()
    }

    pub fn should_wait_async_flushes(&self) -> bool {
        self.legacy.should_wait_async_flushes()
    }

    pub fn pop_async_flushes(&mut self) {
        let Some(flush_list) = self.legacy.committed_flushes.pop_front() else {
            return;
        };
        let Some(flush_list) = flush_list else {
            return;
        };
        for async_job_id in flush_list {
            let should_collect = {
                let async_jobs = self.legacy.slot_async_jobs.lock();
                async_jobs
                    .get(&async_job_id)
                    .map(|async_job| !async_job.collected)
                    .unwrap_or(false)
            };
            if should_collect {
                let query_location = self
                    .legacy
                    .slot_async_jobs
                    .lock()
                    .get(&async_job_id)
                    .map(|async_job| async_job.query_location)
                    .unwrap_or(0);
                self.flush_and_remove_region(query_location, 2, true);
            }
        }
    }

    pub fn query(
        &mut self,
        gpu_addr: u64,
        query_type: QueryType,
        timestamp: Option<u64>,
        sync_operation: impl FnOnce(Box<dyn FnOnce() + Send>),
        invalidate_query_cache_writeback: impl FnOnce(u64, u64) + Send + 'static,
    ) {
        let Some(memory_manager) = self.channel_memory_manager.as_ref().cloned() else {
            if std::env::var_os("RUZU_TRACE_GL_QUERY").is_some() {
                log::info!(
                    "GLQueryCache::query drop gpu=0x{:X} type={:?} timestamp={:?}",
                    gpu_addr,
                    query_type,
                    timestamp
                );
            }
            return;
        };
        if std::env::var_os("RUZU_TRACE_GL_QUERY").is_some() {
            log::info!(
                "GLQueryCache::query gpu=0x{:X} type={:?} timestamp={:?}",
                gpu_addr,
                query_type,
                timestamp
            );
        }
        let cpu_addr = {
            let mm = memory_manager.lock();
            mm.gpu_to_cpu_address(gpu_addr)
        };
        let Some(cpu_addr) = cpu_addr else {
            return;
        };

        let page = cpu_addr >> 12;
        let idx = if let Some(pos) = self.legacy.cached_queries.get(&page).and_then(|queries| {
            queries
                .iter()
                .position(|query| query.cpu_addr() == cpu_addr)
        }) {
            pos
        } else {
            self.legacy
                .cached_queries
                .entry(page)
                .or_default()
                .push(CachedQuery::new(query_type, cpu_addr, gpu_addr));
            self.legacy.cached_queries[&page].len() - 1
        };

        let previous = self.slice_current_stream(query_type);
        let mut query = self
            .legacy
            .cached_queries
            .get_mut(&page)
            .expect("cached query page must exist")
            .remove(idx);

        if let Some((async_job_id, value)) = query.bind_counter(previous, timestamp, self) {
            if let Some(async_job) = self.legacy.slot_async_jobs.lock().get_mut(&async_job_id) {
                async_job.collected = true;
                async_job.value = value;
            }
        }

        let new_async_job_id = self.legacy.alloc_async_job_id();
        self.legacy.slot_async_jobs.lock().insert(
            new_async_job_id,
            AsyncJob {
                collected: false,
                value: 0,
                query_location: cpu_addr,
                timestamp,
            },
        );
        query.base.assigned_async_job = new_async_job_id;
        self.legacy
            .cached_queries
            .get_mut(&page)
            .expect("cached query page must exist")
            .insert(idx, query);
        self.legacy
            .uncommitted_flushes
            .get_or_insert_with(Vec::new)
            .push(new_async_job_id);

        let async_jobs = Arc::clone(&self.legacy.slot_async_jobs);
        let operation = Box::new(move || {
            let value = async_jobs
                .lock()
                .get(&new_async_job_id)
                .map(|job| job.value)
                .unwrap_or(0);
            let mm = memory_manager.lock();
            if let Some(timestamp) = timestamp {
                mm.write_block_unsafe_owned(gpu_addr + 8, &timestamp.to_le_bytes());
                mm.write_block_unsafe_owned(gpu_addr, &value.to_le_bytes());
                invalidate_query_cache_writeback(gpu_addr, 16);
            } else {
                mm.write_block_unsafe_owned(gpu_addr, &(value as u32).to_le_bytes());
                invalidate_query_cache_writeback(gpu_addr, 4);
            }
            async_jobs.lock().remove(&new_async_job_id);
        });
        sync_operation(operation);
    }

    fn counter(
        &mut self,
        dependency: Option<HostCounterHandle>,
        query_type: QueryType,
    ) -> HostCounterHandle {
        Arc::new(Mutex::new(HostCounter::new(self, dependency, query_type)))
    }

    fn enable_stream(&mut self, query_type: QueryType) {
        let dependency = self.legacy.stream(query_type).last.clone();
        if self.legacy.stream(query_type).current.is_none() {
            let new_counter = self.counter(dependency, query_type);
            self.legacy.stream_mut(query_type).enable_with(new_counter);
        }
    }

    fn reset_stream(&mut self, query_type: QueryType) {
        let new_current = if self.legacy.stream(query_type).current.is_some() {
            Some(self.counter(None, query_type))
        } else {
            None
        };
        self.legacy
            .stream_mut(query_type)
            .reset_with(self.commands_queued, new_current);
    }

    fn disable_stream(&mut self, query_type: QueryType) {
        self.legacy
            .stream_mut(query_type)
            .disable(self.commands_queued);
    }

    fn slice_current_stream(&mut self, query_type: QueryType) -> Option<HostCounterHandle> {
        let dependency = self.legacy.stream(query_type).current.clone();
        let new_counter = self.counter(dependency, query_type);
        self.legacy
            .stream_mut(query_type)
            .current_with(self.commands_queued, new_counter)
    }

    fn flush_and_remove_region(&mut self, addr: u64, size: usize, r#async: bool) {
        let addr_begin = addr;
        let addr_end = addr_begin + size as u64;
        let page_begin = addr_begin >> 12;
        let page_end = addr_end >> 12;
        for page in page_begin..=page_end {
            let Some(mut page_vec) = self.legacy.cached_queries.remove(&page) else {
                continue;
            };
            let mut kept = Vec::with_capacity(page_vec.len());
            for mut query in page_vec.drain(..) {
                let cache_begin = query.cpu_addr();
                let cache_end = cache_begin + query.size_in_bytes();
                if cache_begin < addr_end && addr_begin < cache_end {
                    let async_job_id = query.base.assigned_async_job;
                    let value = query.flush(self, r#async);
                    if async_job_id != AsyncJobId(0) {
                        if let Some(async_job) =
                            self.legacy.slot_async_jobs.lock().get_mut(&async_job_id)
                        {
                            async_job.collected = true;
                            async_job.value = value;
                        }
                        query.base.assigned_async_job = AsyncJobId(0);
                    }
                } else {
                    kept.push(query);
                }
            }
            if !kept.is_empty() {
                self.legacy.cached_queries.insert(page, kept);
            }
        }
    }
}

#[cfg(test)]
impl QueryCache {
    fn new_for_test() -> Self {
        Self {
            query_pools: [Vec::new(), Vec::new(), Vec::new()],
            channel_memory_manager: None,
            gpu_ticks_getter: None,
            legacy: QueryCacheLegacy::new(),
            commands_queued: false,
        }
    }
}

type HostCounterHandle = Arc<Mutex<HostCounter>>;

impl CounterHandle for HostCounterHandle {
    fn query(&self, r#async: bool) -> u64 {
        self.lock().query(r#async)
    }

    fn wait_pending(&self) -> bool {
        self.lock().wait_pending()
    }

    fn depth(&self) -> u64 {
        self.lock().depth()
    }

    fn end_query(&self, any_command_queued: bool) {
        self.lock().end_query(any_command_queued);
    }
}

/// A host-side query counter.
///
/// Corresponds to `OpenGL::HostCounter`.
pub struct HostCounter {
    pub query_type: QueryType,
    pub query: u32,
    base: HostCounterBase<HostCounterHandle>,
}

impl HostCounter {
    /// Create and begin a new host counter.
    pub fn new(
        cache: &mut QueryCache,
        dependency: Option<HostCounterHandle>,
        query_type: QueryType,
    ) -> Self {
        let query = cache.allocate_query(query_type);
        unsafe {
            gl::BeginQuery(get_target(query_type), query);
        }
        Self {
            query_type,
            query,
            base: HostCounterBase::new(dependency),
        }
    }

    /// End the query.
    pub fn end_query(&self, any_command_queued: bool) {
        if !any_command_queued {
            unsafe {
                gl::Flush();
            }
        }
        unsafe {
            gl::EndQuery(get_target(self.query_type));
        }
    }

    /// Block and read the query result.
    pub fn blocking_query(&self, _async: bool) -> u64 {
        let mut value: i64 = 0;
        unsafe {
            gl::GetQueryObjecti64v(self.query, gl::QUERY_RESULT, &mut value);
        }
        value as u64
    }

    pub fn query(&mut self, r#async: bool) -> u64 {
        let query = self.query;
        self.base.query(r#async, move |_async_query| {
            let mut value: i64 = 0;
            unsafe {
                gl::GetQueryObjecti64v(query, gl::QUERY_RESULT, &mut value);
            }
            value as u64
        })
    }

    pub fn wait_pending(&self) -> bool {
        self.base.wait_pending()
    }

    pub fn depth(&self) -> u64 {
        self.base.depth()
    }
}

/// A cached query mapped to guest memory.
///
/// Corresponds to `OpenGL::CachedQuery`.
pub struct CachedQuery {
    pub query_type: QueryType,
    pub base: CachedQueryBase<HostCounterHandle>,
}

impl CachedQuery {
    /// Create a new cached query.
    pub fn new(query_type: QueryType, cpu_addr: u64, gpu_addr: u64) -> Self {
        Self {
            query_type,
            base: CachedQueryBase::new(cpu_addr, gpu_addr),
        }
    }

    pub fn cpu_addr(&self) -> u64 {
        self.base.cpu_addr
    }

    pub fn bind_counter(
        &mut self,
        counter: Option<HostCounterHandle>,
        timestamp: Option<u64>,
        cache: &mut QueryCache,
    ) -> Option<(AsyncJobId, u64)> {
        let result = if self.base.counter.is_some() {
            let async_job_id = self.base.assigned_async_job;
            let value = self.flush(cache, false);
            Some((async_job_id, value))
        } else {
            None
        };
        self.base.counter = counter;
        self.base.timestamp = timestamp;
        result
    }

    pub fn size_in_bytes(&self) -> u64 {
        self.base.size_in_bytes()
    }

    pub fn flush(&mut self, cache: &mut QueryCache, r#async: bool) -> u64 {
        let slice_counter = self
            .base
            .counter
            .as_ref()
            .map(|counter| counter.wait_pending())
            .unwrap_or(false)
            && cache.legacy.stream(self.query_type).is_enabled();
        if slice_counter {
            cache.disable_stream(self.query_type);
        }
        let value = self
            .base
            .counter
            .as_ref()
            .map(|counter| counter.query(r#async))
            .unwrap_or(0);
        if slice_counter {
            cache.enable_stream(self.query_type);
        }
        if !r#async {
            if let Some(memory_manager) = cache.channel_memory_manager.as_ref() {
                let mm = memory_manager.lock();
                mm.write_block_unsafe_owned(self.base.gpu_addr, &value.to_le_bytes());
                if let Some(timestamp) = self.base.timestamp {
                    mm.write_block_unsafe_owned(self.base.gpu_addr + 8, &timestamp.to_le_bytes());
                }
            }
        }
        value
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::control::channel_state::ChannelState;
    use crate::memory_manager::MemoryManager;
    use parking_lot::Mutex as ParkingMutex;
    use std::sync::Arc;

    #[test]
    fn bind_to_channel_wires_channel_memory_manager_through_legacy_owner() {
        let mut cache = QueryCache::new_for_test();
        let mm = Arc::new(ParkingMutex::new(MemoryManager::new(17)));
        let mut channel = ChannelState::new(5);
        channel.program_id = 0x1234;
        channel.memory_manager = Some(Arc::clone(&mm));

        cache.create_channel(&channel);
        cache.bind_to_channel(channel.bind_id);

        let bound = cache
            .channel_memory_manager
            .as_ref()
            .expect("channel memory manager should be bound");
        assert!(Arc::ptr_eq(bound, &mm));
        assert_eq!(cache.legacy.channel_caches.program_id, 0x1234);

        cache.erase_channel(channel.bind_id);
        assert!(cache.channel_memory_manager.is_none());
    }

    #[test]
    fn async_flush_queue_tracks_pending_and_collects_query_values() {
        let mut cache = QueryCache::new_for_test();
        let cpu_addr = 0x5510_6000u64;
        let page = cpu_addr >> 12;
        let async_job_id = AsyncJobId(7);

        cache.legacy.cached_queries.insert(
            page,
            vec![CachedQuery {
                query_type: QueryType::SamplesPassed,
                base: CachedQueryBase {
                    cpu_addr,
                    gpu_addr: 0x5038_50000,
                    counter: None,
                    timestamp: Some(0x1234),
                    assigned_async_job: async_job_id,
                },
            }],
        );
        cache.legacy.slot_async_jobs.lock().insert(
            async_job_id,
            AsyncJob {
                collected: false,
                value: 99,
                query_location: cpu_addr,
                timestamp: Some(0x1234),
            },
        );
        cache.legacy.uncommitted_flushes = Some(vec![async_job_id]);

        assert!(cache.has_uncommitted_flushes());
        cache.commit_async_flushes();
        assert!(!cache.has_uncommitted_flushes());
        assert!(cache.should_wait_async_flushes());

        cache.pop_async_flushes();

        assert!(!cache.should_wait_async_flushes());
        assert!(cache
            .legacy
            .cached_queries
            .get(&page)
            .map(|queries| queries.is_empty())
            .unwrap_or(true));
        let async_jobs = cache.legacy.slot_async_jobs.lock();
        let job = async_jobs
            .get(&async_job_id)
            .expect("async job retained until writeback");
        assert!(job.collected);
        assert_eq!(job.value, 0);
    }

    #[test]
    fn sync_flush_writes_immediate_guest_value_and_timestamp() {
        let mut cache = QueryCache::new_for_test();
        let writes = Arc::new(Mutex::new(Vec::<(u64, Vec<u8>)>::new()));
        let writes_clone = Arc::clone(&writes);
        let mut mm = crate::memory_manager::MemoryManager::new(0);
        mm.map(0x5038_50000, 0x5510_6000, 0x1000, 0, false);
        mm.set_guest_memory_writer(Arc::new(move |addr, data| {
            writes_clone.lock().push((addr, data.to_vec()));
        }));
        cache.channel_memory_manager = Some(Arc::new(parking_lot::Mutex::new(mm)));

        let mut query = CachedQuery {
            query_type: QueryType::SamplesPassed,
            base: CachedQueryBase {
                cpu_addr: 0x5510_6000,
                gpu_addr: 0x5038_50000,
                counter: None,
                timestamp: Some(0x1122_3344_5566_7788),
                assigned_async_job: AsyncJobId(0),
            },
        };

        let value = query.flush(&mut cache, false);
        assert_eq!(value, 0);

        let writes = writes.lock();
        assert_eq!(writes.len(), 2);
        assert_eq!(writes[0].0, 0x5510_6000);
        assert_eq!(writes[0].1, 0u64.to_le_bytes());
        assert_eq!(writes[1].0, 0x5510_6008);
        assert_eq!(writes[1].1, 0x1122_3344_5566_7788u64.to_le_bytes());
    }

    #[test]
    fn flush_region_only_touches_overlapping_pages() {
        let mut cache = QueryCache::new_for_test();
        let hit_page = 0x5510_6000u64 >> 12;
        let cold_page = 0x6610_6000u64 >> 12;
        cache.legacy.cached_queries.insert(
            hit_page,
            vec![CachedQuery::new(
                QueryType::SamplesPassed,
                0x5510_6000,
                0x5038_50000,
            )],
        );
        cache.legacy.cached_queries.insert(
            cold_page,
            vec![CachedQuery::new(
                QueryType::SamplesPassed,
                0x6610_6000,
                0x6038_50000,
            )],
        );

        cache.flush_region(0x5510_6000, 8);

        assert!(!cache.legacy.cached_queries.contains_key(&hit_page));
        assert!(cache.legacy.cached_queries.contains_key(&cold_page));
    }

    #[test]
    fn commit_async_flushes_preserves_empty_batches_as_non_waiting() {
        let mut cache = QueryCache::new_for_test();
        cache.commit_async_flushes();
        assert!(!cache.should_wait_async_flushes());
        cache.pop_async_flushes();
        assert!(!cache.should_wait_async_flushes());
    }
}
