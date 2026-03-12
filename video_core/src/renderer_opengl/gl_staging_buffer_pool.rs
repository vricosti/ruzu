// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_staging_buffer_pool.h and
//! gl_staging_buffer_pool.cpp
//!
//! OpenGL staging buffer pool — manages persistent mapped buffers for CPU-GPU transfers.

/// Stream buffer size (64 MiB).
pub const STREAM_BUFFER_SIZE: usize = 64 * 1024 * 1024;

/// Number of sync regions in the stream buffer.
pub const NUM_SYNCS: usize = 16;

/// Size of each sync region.
pub const REGION_SIZE: usize = STREAM_BUFFER_SIZE / NUM_SYNCS;

/// Maximum alignment for stream buffer requests.
pub const MAX_ALIGNMENT: usize = 256;

/// A mapped region from a staging buffer.
///
/// Corresponds to `OpenGL::StagingBufferMap`.
pub struct StagingBufferMap {
    /// Pointer to the mapped memory.
    pub mapped_ptr: *mut u8,
    /// Size of the mapped region.
    pub mapped_size: usize,
    /// Offset within the staging buffer.
    pub offset: usize,
    /// GL buffer handle.
    pub buffer: u32,
    /// Index in the alloc array (for freeing deferred buffers).
    pub index: usize,
    /// Whether this map has an associated sync object.
    pub has_sync: bool,
}

/// A single staging buffer allocation.
struct StagingBufferAlloc {
    sync: gl::types::GLsync,
    buffer: u32,
    map: *mut u8,
    size: usize,
    sync_index: usize,
    deferred: bool,
}

/// A collection of staging buffers for a given access pattern.
///
/// Corresponds to `OpenGL::StagingBuffers`.
pub struct StagingBuffers {
    allocs: Vec<StagingBufferAlloc>,
    storage_flags: u32,
    map_flags: u32,
    current_sync_index: usize,
}

impl StagingBuffers {
    /// Create a new staging buffer collection.
    pub fn new(storage_flags: u32, map_flags: u32) -> Self {
        Self {
            allocs: Vec::new(),
            storage_flags,
            map_flags,
            current_sync_index: 0,
        }
    }

    /// Request a mapped staging buffer of the given size.
    pub fn request_map(
        &mut self,
        _requested_size: usize,
        _insert_fence: bool,
        _deferred: bool,
    ) -> StagingBufferMap {
        todo!("StagingBuffers::RequestMap")
    }

    /// Free a deferred staging buffer.
    pub fn free_deferred_staging_buffer(&mut self, index: usize) {
        assert!(self.allocs[index].deferred);
        self.allocs[index].deferred = false;
    }

    /// Request or allocate a buffer of the given size.
    fn request_buffer(&mut self, _requested_size: usize) -> usize {
        todo!("StagingBuffers::RequestBuffer")
    }

    /// Find an existing free buffer that fits the requested size.
    fn find_buffer(&self, _requested_size: usize) -> Option<usize> {
        todo!("StagingBuffers::FindBuffer")
    }
}

/// A persistent mapped stream buffer for uniform data.
///
/// Corresponds to `OpenGL::StreamBuffer`.
pub struct StreamBuffer {
    iterator: usize,
    used_iterator: usize,
    free_iterator: usize,
    mapped_pointer: *mut u8,
    buffer: u32,
    fences: [gl::types::GLsync; NUM_SYNCS],
}

impl StreamBuffer {
    /// Create a new stream buffer.
    pub fn new() -> Self {
        // TODO: Create persistent mapped buffer with GL_MAP_WRITE_BIT |
        //       GL_MAP_PERSISTENT_BIT | GL_MAP_COHERENT_BIT
        Self {
            iterator: 0,
            used_iterator: 0,
            free_iterator: 0,
            mapped_pointer: std::ptr::null_mut(),
            buffer: 0,
            fences: [std::ptr::null(); NUM_SYNCS],
        }
    }

    /// Request a region of the stream buffer.
    ///
    /// Returns (mapped slice pointer, offset).
    pub fn request(&mut self, _size: usize) -> (*mut u8, usize) {
        todo!("StreamBuffer::Request")
    }

    /// Get the GL buffer handle.
    pub fn handle(&self) -> u32 {
        self.buffer
    }

    fn region(offset: usize) -> usize {
        offset / REGION_SIZE
    }
}

/// Top-level staging buffer pool.
///
/// Corresponds to `OpenGL::StagingBufferPool`.
pub struct StagingBufferPool {
    upload_buffers: StagingBuffers,
    download_buffers: StagingBuffers,
}

impl StagingBufferPool {
    /// Create a new staging buffer pool.
    pub fn new() -> Self {
        Self {
            upload_buffers: StagingBuffers::new(
                gl::MAP_WRITE_BIT,
                gl::MAP_WRITE_BIT | gl::MAP_FLUSH_EXPLICIT_BIT,
            ),
            download_buffers: StagingBuffers::new(
                gl::MAP_READ_BIT | gl::CLIENT_STORAGE_BIT,
                gl::MAP_READ_BIT,
            ),
        }
    }

    /// Request an upload staging buffer.
    pub fn request_upload_buffer(&mut self, size: usize) -> StagingBufferMap {
        self.upload_buffers.request_map(size, true, false)
    }

    /// Request a download staging buffer.
    pub fn request_download_buffer(
        &mut self,
        size: usize,
        deferred: bool,
    ) -> StagingBufferMap {
        self.download_buffers.request_map(size, false, deferred)
    }

    /// Free a deferred staging buffer.
    pub fn free_deferred_staging_buffer(&mut self, buffer: &StagingBufferMap) {
        self.download_buffers
            .free_deferred_staging_buffer(buffer.index);
    }
}
