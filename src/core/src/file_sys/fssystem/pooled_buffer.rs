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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_constants() {
        assert_eq!(BUFFER_POOL_ALIGNMENT, 4096);
        assert_eq!(BUFFER_POOL_WORK_SIZE, 320);
    }

    #[test]
    fn test_new() {
        let pb = PooledBuffer::new();
        assert_eq!(pb.get_size(), 0);
    }

    #[test]
    fn test_with_size() {
        let pb = PooledBuffer::with_size(1024, 512);
        assert_eq!(pb.get_size(), 1024);
        assert_eq!(pb.get_buffer().len(), 1024);
    }

    #[test]
    fn test_allocate() {
        let mut pb = PooledBuffer::new();
        pb.allocate(256, 128);
        assert_eq!(pb.get_size(), 256);
    }

    #[test]
    fn test_allocate_particularly_large() {
        let mut pb = PooledBuffer::new();
        pb.allocate_particularly_large(4096, 1024);
        assert_eq!(pb.get_size(), 4096);
    }

    #[test]
    fn test_shrink() {
        let mut pb = PooledBuffer::with_size(1024, 512);
        pb.shrink(256);
        assert_eq!(pb.get_size(), 256);
    }

    #[test]
    fn test_shrink_no_effect_if_smaller() {
        let mut pb = PooledBuffer::with_size(256, 128);
        pb.shrink(512);
        // Shrink should not grow, so size should stay at 256.
        assert_eq!(pb.get_size(), 256);
    }

    #[test]
    fn test_deallocate() {
        let mut pb = PooledBuffer::with_size(1024, 512);
        pb.deallocate();
        assert_eq!(pb.get_size(), 0);
    }

    #[test]
    fn test_get_buffer_mut() {
        let mut pb = PooledBuffer::with_size(16, 16);
        let buf = pb.get_buffer_mut();
        buf[0] = 0xAA;
        buf[1] = 0xBB;
        assert_eq!(pb.get_buffer()[0], 0xAA);
        assert_eq!(pb.get_buffer()[1], 0xBB);
    }

    #[test]
    fn test_get_allocatable_size_max() {
        let max = PooledBuffer::get_allocatable_size_max();
        assert!(max > 0);
    }

    #[test]
    fn test_get_allocatable_particularly_large_size_max() {
        let max = PooledBuffer::get_allocatable_particularly_large_size_max();
        assert!(max > 0);
    }

    #[test]
    fn test_default() {
        let pb = PooledBuffer::default();
        assert_eq!(pb.get_size(), 0);
    }
}
