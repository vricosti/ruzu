// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_staging_buffer_pool.h and
//! gl_staging_buffer_pool.cpp
//!
//! OpenGL staging buffer pool -- manages persistent mapped buffers for CPU-GPU transfers.

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
    sync_index: usize,
    buffer: u32,
    map: *mut u8,
    size: usize,
    deferred: bool,
}

/// Round up to the next power of two.
fn next_pow2(v: usize) -> usize {
    if v == 0 {
        return 1;
    }
    let mut n = v - 1;
    n |= n >> 1;
    n |= n >> 2;
    n |= n >> 4;
    n |= n >> 8;
    n |= n >> 16;
    n |= n >> 32;
    n + 1
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
    ///
    /// Port of `StagingBuffers::RequestMap`.
    pub fn request_map(
        &mut self,
        requested_size: usize,
        insert_fence: bool,
        deferred: bool,
    ) -> StagingBufferMap {
        let index = self.request_buffer(requested_size);
        let alloc = &mut self.allocs[index];

        if insert_fence {
            self.current_sync_index += 1;
            alloc.sync_index = self.current_sync_index;
        } else {
            alloc.sync_index = 0;
        }
        alloc.deferred = deferred;

        StagingBufferMap {
            mapped_ptr: alloc.map,
            mapped_size: requested_size,
            offset: 0,
            buffer: alloc.buffer,
            index,
            has_sync: insert_fence,
        }
    }

    /// Free a deferred staging buffer.
    ///
    /// Port of `StagingBuffers::FreeDeferredStagingBuffer`.
    pub fn free_deferred_staging_buffer(&mut self, index: usize) {
        assert!(self.allocs[index].deferred);
        self.allocs[index].deferred = false;
    }

    /// Request or allocate a buffer of the given size.
    ///
    /// Port of `StagingBuffers::RequestBuffer`.
    fn request_buffer(&mut self, requested_size: usize) -> usize {
        if let Some(index) = self.find_buffer(requested_size) {
            return index;
        }

        let next_pow2_size = next_pow2(requested_size);
        let mut buffer: u32 = 0;
        let map: *mut u8;
        let persistent_flags = self.storage_flags | gl::MAP_PERSISTENT_BIT;
        let persistent_map_flags = self.map_flags | gl::MAP_PERSISTENT_BIT;

        unsafe {
            gl::CreateBuffers(1, &mut buffer);
            gl::NamedBufferStorage(
                buffer,
                next_pow2_size as isize,
                std::ptr::null(),
                persistent_flags,
            );
            map = gl::MapNamedBufferRange(
                buffer,
                0,
                next_pow2_size as isize,
                persistent_map_flags,
            ) as *mut u8;
        }

        self.allocs.push(StagingBufferAlloc {
            sync: std::ptr::null(),
            sync_index: 0,
            buffer,
            map,
            size: next_pow2_size,
            deferred: false,
        });
        self.allocs.len() - 1
    }

    /// Find an existing free buffer that fits the requested size.
    ///
    /// Port of `StagingBuffers::FindBuffer`.
    fn find_buffer(&mut self, requested_size: usize) -> Option<usize> {
        let mut known_unsignaled_index = self.current_sync_index + 1;
        let mut smallest_buffer = usize::MAX;
        let mut found: Option<usize> = None;

        for index in 0..self.allocs.len() {
            let buffer_size = self.allocs[index].size;
            if buffer_size < requested_size || buffer_size >= smallest_buffer {
                continue;
            }
            if self.allocs[index].deferred {
                continue;
            }
            let sync = self.allocs[index].sync;
            if !sync.is_null() {
                let sync_index = self.allocs[index].sync_index;
                if sync_index >= known_unsignaled_index {
                    continue;
                }
                let status = unsafe {
                    gl::ClientWaitSync(sync, gl::SYNC_FLUSH_COMMANDS_BIT, 0)
                };
                if status == gl::TIMEOUT_EXPIRED || status == gl::WAIT_FAILED {
                    known_unsignaled_index = known_unsignaled_index.min(sync_index);
                    continue;
                }
                // Signaled - release the sync
                unsafe {
                    gl::DeleteSync(sync);
                }
                self.allocs[index].sync = std::ptr::null();
            }
            smallest_buffer = buffer_size;
            found = Some(index);
        }
        found
    }
}

impl Drop for StagingBuffers {
    fn drop(&mut self) {
        for alloc in &self.allocs {
            unsafe {
                if alloc.buffer != 0 {
                    gl::DeleteBuffers(1, &alloc.buffer);
                }
                if !alloc.sync.is_null() {
                    gl::DeleteSync(alloc.sync);
                }
            }
        }
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

// SAFETY: The GL handles and mapped pointers are only accessed on the GL thread.
unsafe impl Send for StreamBuffer {}
unsafe impl Sync for StreamBuffer {}

impl StreamBuffer {
    /// Create a new stream buffer.
    ///
    /// Port of `StreamBuffer::StreamBuffer`.
    pub fn new() -> Self {
        let mut buffer: u32 = 0;
        let mapped_pointer: *mut u8;
        let flags = gl::MAP_WRITE_BIT | gl::MAP_PERSISTENT_BIT | gl::MAP_COHERENT_BIT;

        unsafe {
            gl::CreateBuffers(1, &mut buffer);
            gl::NamedBufferStorage(
                buffer,
                STREAM_BUFFER_SIZE as isize,
                std::ptr::null(),
                flags,
            );
            mapped_pointer = gl::MapNamedBufferRange(
                buffer,
                0,
                STREAM_BUFFER_SIZE as isize,
                flags,
            ) as *mut u8;
        }

        let mut fences = [std::ptr::null(); NUM_SYNCS];
        for fence in &mut fences {
            unsafe {
                *fence = gl::FenceSync(gl::SYNC_GPU_COMMANDS_COMPLETE, 0);
            }
        }

        Self {
            iterator: 0,
            used_iterator: 0,
            free_iterator: 0,
            mapped_pointer,
            buffer,
            fences,
        }
    }

    /// Request a region of the stream buffer.
    ///
    /// Returns (mapped slice pointer, offset).
    ///
    /// Port of `StreamBuffer::Request`.
    pub fn request(&mut self, size: usize) -> (*mut u8, usize) {
        assert!(size < REGION_SIZE);

        // Create fences for used regions
        let region_start = Self::region(self.used_iterator);
        let region_end = Self::region(self.iterator);
        for region in region_start..region_end {
            unsafe {
                if !self.fences[region].is_null() {
                    gl::DeleteSync(self.fences[region]);
                }
                self.fences[region] = gl::FenceSync(gl::SYNC_GPU_COMMANDS_COMPLETE, 0);
            }
        }
        self.used_iterator = self.iterator;

        // Wait for regions we're about to overwrite
        let wait_start = Self::region(self.free_iterator) + 1;
        let wait_end = (Self::region(self.iterator + size) + 1).min(NUM_SYNCS);
        for region in wait_start..wait_end {
            unsafe {
                if !self.fences[region].is_null() {
                    gl::ClientWaitSync(self.fences[region], 0, u64::MAX);
                    gl::DeleteSync(self.fences[region]);
                    self.fences[region] = std::ptr::null();
                }
            }
        }
        if self.iterator + size >= self.free_iterator {
            self.free_iterator = self.iterator + size;
        }

        // Wrap around if needed
        if self.iterator + size > STREAM_BUFFER_SIZE {
            for region in Self::region(self.used_iterator)..NUM_SYNCS {
                unsafe {
                    if !self.fences[region].is_null() {
                        gl::DeleteSync(self.fences[region]);
                    }
                    self.fences[region] = gl::FenceSync(gl::SYNC_GPU_COMMANDS_COMPLETE, 0);
                }
            }
            self.used_iterator = 0;
            self.iterator = 0;
            self.free_iterator = size;

            for region in 0..=Self::region(size) {
                unsafe {
                    if !self.fences[region].is_null() {
                        gl::ClientWaitSync(self.fences[region], 0, u64::MAX);
                        gl::DeleteSync(self.fences[region]);
                        self.fences[region] = std::ptr::null();
                    }
                }
            }
        }

        let offset = self.iterator;
        // Align up to MAX_ALIGNMENT
        self.iterator = (self.iterator + size + MAX_ALIGNMENT - 1) & !(MAX_ALIGNMENT - 1);

        let ptr = unsafe { self.mapped_pointer.add(offset) };
        (ptr, offset)
    }

    /// Get the GL buffer handle.
    pub fn handle(&self) -> u32 {
        self.buffer
    }

    fn region(offset: usize) -> usize {
        offset / REGION_SIZE
    }
}

impl Drop for StreamBuffer {
    fn drop(&mut self) {
        unsafe {
            for fence in &self.fences {
                if !fence.is_null() {
                    gl::DeleteSync(*fence);
                }
            }
            if self.buffer != 0 {
                gl::DeleteBuffers(1, &self.buffer);
            }
        }
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
    ///
    /// Port of `StagingBufferPool::RequestUploadBuffer`.
    pub fn request_upload_buffer(&mut self, size: usize) -> StagingBufferMap {
        self.upload_buffers.request_map(size, true, false)
    }

    /// Request a download staging buffer.
    ///
    /// Port of `StagingBufferPool::RequestDownloadBuffer`.
    pub fn request_download_buffer(
        &mut self,
        size: usize,
        deferred: bool,
    ) -> StagingBufferMap {
        self.download_buffers.request_map(size, false, deferred)
    }

    /// Free a deferred staging buffer.
    ///
    /// Port of `StagingBufferPool::FreeDeferredStagingBuffer`.
    pub fn free_deferred_staging_buffer(&mut self, buffer: &StagingBufferMap) {
        self.download_buffers
            .free_deferred_staging_buffer(buffer.index);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn constants() {
        assert_eq!(STREAM_BUFFER_SIZE, 64 * 1024 * 1024);
        assert_eq!(NUM_SYNCS, 16);
        assert_eq!(REGION_SIZE, STREAM_BUFFER_SIZE / NUM_SYNCS);
        assert_eq!(MAX_ALIGNMENT, 256);
    }

    #[test]
    fn stream_buffer_region() {
        assert_eq!(StreamBuffer::region(0), 0);
        assert_eq!(StreamBuffer::region(REGION_SIZE - 1), 0);
        assert_eq!(StreamBuffer::region(REGION_SIZE), 1);
        assert_eq!(StreamBuffer::region(STREAM_BUFFER_SIZE - 1), NUM_SYNCS - 1);
    }

    #[test]
    fn next_pow2_works() {
        assert_eq!(next_pow2(0), 1);
        assert_eq!(next_pow2(1), 1);
        assert_eq!(next_pow2(2), 2);
        assert_eq!(next_pow2(3), 4);
        assert_eq!(next_pow2(5), 8);
        assert_eq!(next_pow2(256), 256);
        assert_eq!(next_pow2(257), 512);
    }
}
