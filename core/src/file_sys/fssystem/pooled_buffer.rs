// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fssystem/fssystem_pooled_buffer.h / .cpp

pub const BUFFER_POOL_ALIGNMENT: usize = 4 * 1024;
pub const BUFFER_POOL_WORK_SIZE: usize = 320;

/// A pooled buffer for temporary storage allocations.
/// Corresponds to upstream `PooledBuffer`.
pub struct PooledBuffer {
    buffer: Option<Vec<u8>>,
    size: usize,
}

impl PooledBuffer {
    pub fn new() -> Self {
        Self {
            buffer: None,
            size: 0,
        }
    }

    pub fn with_size(ideal_size: usize, required_size: usize) -> Self {
        let mut pb = Self::new();
        pb.allocate(ideal_size, required_size);
        pb
    }

    pub fn allocate(&mut self, ideal_size: usize, _required_size: usize) {
        self.allocate_core(ideal_size, _required_size, false);
    }

    pub fn allocate_particularly_large(&mut self, ideal_size: usize, required_size: usize) {
        self.allocate_core(ideal_size, required_size, true);
    }

    pub fn shrink(&mut self, ideal_size: usize) {
        if ideal_size == 0 {
            self.buffer = None;
            self.size = 0;
        } else if self.size > ideal_size {
            self.size = ideal_size;
            if let Some(ref mut buf) = self.buffer {
                buf.truncate(ideal_size);
            }
        }
    }

    pub fn deallocate(&mut self) {
        self.shrink(0);
    }

    pub fn get_buffer(&self) -> &[u8] {
        self.buffer.as_deref().expect("buffer not allocated")
    }

    pub fn get_buffer_mut(&mut self) -> &mut [u8] {
        self.buffer.as_deref_mut().expect("buffer not allocated")
    }

    pub fn get_size(&self) -> usize {
        self.size
    }

    pub fn get_allocatable_size_max() -> usize {
        Self::get_allocatable_size_max_core(false)
    }

    pub fn get_allocatable_particularly_large_size_max() -> usize {
        Self::get_allocatable_size_max_core(true)
    }

    fn get_allocatable_size_max_core(_large: bool) -> usize {
        // Simplified: return a large allocation limit
        64 * 1024 * 1024
    }

    fn allocate_core(&mut self, ideal_size: usize, required_size: usize, _large: bool) {
        let alloc_size = std::cmp::max(ideal_size, required_size);
        if alloc_size > 0 {
            self.buffer = Some(vec![0u8; alloc_size]);
            self.size = alloc_size;
        }
    }
}

impl Default for PooledBuffer {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for PooledBuffer {
    fn drop(&mut self) {
        self.deallocate();
    }
}
