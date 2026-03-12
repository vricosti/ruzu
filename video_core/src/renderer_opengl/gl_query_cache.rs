// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_query_cache.h and gl_query_cache.cpp
//!
//! OpenGL query cache — manages GPU occlusion/primitives queries.

/// Number of query types.
pub const NUM_QUERY_TYPES: usize = 3;

/// Query type enumeration matching upstream `VideoCore::QueryType`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u32)]
pub enum QueryType {
    SamplesPassed = 0,
    PrimitivesGenerated = 1,
    TfbPrimitivesWritten = 2,
}

/// Map a query type to the corresponding GL target.
///
/// Corresponds to the anonymous `GetTarget()` in gl_query_cache.cpp.
pub fn get_target(query_type: QueryType) -> u32 {
    match query_type {
        QueryType::SamplesPassed => gl::SAMPLES_PASSED,
        QueryType::PrimitivesGenerated => gl::PRIMITIVES_GENERATED,
        QueryType::TfbPrimitivesWritten => gl::TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN,
    }
}

/// OpenGL query cache.
///
/// Corresponds to `OpenGL::QueryCache`.
pub struct QueryCache {
    /// Per-type pool of reusable query objects.
    query_pools: [Vec<u32>; NUM_QUERY_TYPES],
}

impl QueryCache {
    /// Create a new query cache.
    ///
    /// Corresponds to `QueryCache::QueryCache()`.
    pub fn new() -> Self {
        Self {
            query_pools: [Vec::new(), Vec::new(), Vec::new()],
        }
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
}

/// A host-side query counter.
///
/// Corresponds to `OpenGL::HostCounter`.
pub struct HostCounter {
    pub query_type: QueryType,
    pub query: u32,
}

impl HostCounter {
    /// Create and begin a new host counter.
    pub fn new(cache: &mut QueryCache, query_type: QueryType) -> Self {
        let query = cache.allocate_query(query_type);
        unsafe {
            gl::BeginQuery(get_target(query_type), query);
        }
        Self { query_type, query }
    }

    /// End the query.
    pub fn end_query(&self) {
        unsafe {
            gl::EndQuery(get_target(self.query_type));
        }
    }

    /// Block and read the query result.
    pub fn blocking_query(&self) -> u64 {
        let mut value: i64 = 0;
        unsafe {
            gl::GetQueryObjecti64v(self.query, gl::QUERY_RESULT, &mut value);
        }
        value as u64
    }
}

/// A cached query mapped to guest memory.
///
/// Corresponds to `OpenGL::CachedQuery`.
pub struct CachedQuery {
    pub query_type: QueryType,
    pub cpu_addr: u64,
}

impl CachedQuery {
    /// Create a new cached query.
    pub fn new(query_type: QueryType, cpu_addr: u64) -> Self {
        Self {
            query_type,
            cpu_addr,
        }
    }

    /// Flush the cached query result.
    ///
    /// Port of `CachedQuery::Flush()`.
    ///
    /// In the full implementation, this would write the accumulated query
    /// result back to guest memory at `cpu_addr`. For now, returns 0 since
    /// we don't have memory manager integration.
    pub fn flush(&mut self) -> u64 {
        // In the full implementation:
        // 1. Accumulate results from all host counters
        // 2. Write the result to guest memory at self.cpu_addr
        log::trace!("CachedQuery::flush at {:016x}", self.cpu_addr);
        0
    }
}
