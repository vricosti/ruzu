//! Port of zuyu/src/common/scratch_buffer.h
//! Status: COMPLET
//! Derniere synchro: 2026-03-05

use std::ops::{Deref, DerefMut, Index, IndexMut};

/// A buffer that grows but never shrinks, used as temporary working space.
///
/// Mirrors the C++ `Common::ScratchBuffer<T>`. The buffer only reallocates
/// when the requested size exceeds the current capacity. Data is *not*
/// zero-initialized on growth (matching the C++ `make_unique_for_overwrite`
/// semantics), but in Rust we must initialize, so we use `Default`.
pub struct ScratchBuffer<T: Default + Clone> {
    last_requested_size: usize,
    buffer: Vec<T>,
}

impl<T: Default + Clone> Default for ScratchBuffer<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Default + Clone> ScratchBuffer<T> {
    /// Create an empty scratch buffer.
    pub fn new() -> Self {
        Self {
            last_requested_size: 0,
            buffer: Vec::new(),
        }
    }

    /// Create a scratch buffer with the given initial capacity.
    pub fn with_capacity(initial_capacity: usize) -> Self {
        let mut buffer = Vec::with_capacity(initial_capacity);
        buffer.resize(initial_capacity, T::default());
        Self {
            last_requested_size: initial_capacity,
            buffer,
        }
    }

    /// Grow the buffer if `size` exceeds current capacity.
    /// Previously held data remains intact.
    pub fn resize(&mut self, size: usize) {
        if size > self.buffer.len() {
            self.buffer.resize(size, T::default());
        }
        self.last_requested_size = size;
    }

    /// Grow the buffer if `size` exceeds current capacity.
    /// Previously held data may be destroyed on reallocation.
    pub fn resize_destructive(&mut self, size: usize) {
        if size > self.buffer.len() {
            self.buffer = vec![T::default(); size];
        }
        self.last_requested_size = size;
    }

    /// Returns a raw pointer to the underlying data.
    pub fn data(&self) -> &[T] {
        &self.buffer[..self.last_requested_size]
    }

    /// Returns a mutable raw pointer to the underlying data.
    pub fn data_mut(&mut self) -> &mut [T] {
        &mut self.buffer[..self.last_requested_size]
    }

    /// Returns the last requested size.
    pub fn size(&self) -> usize {
        self.last_requested_size
    }

    /// Returns the allocated capacity.
    pub fn capacity(&self) -> usize {
        self.buffer.len()
    }

    /// Swap contents with another scratch buffer.
    pub fn swap(&mut self, other: &mut ScratchBuffer<T>) {
        std::mem::swap(&mut self.last_requested_size, &mut other.last_requested_size);
        std::mem::swap(&mut self.buffer, &mut other.buffer);
    }

    /// Returns an iterator over the active region.
    pub fn iter(&self) -> std::slice::Iter<'_, T> {
        self.data().iter()
    }

    /// Returns a mutable iterator over the active region.
    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, T> {
        let size = self.last_requested_size;
        self.buffer[..size].iter_mut()
    }
}

impl<T: Default + Clone> Deref for ScratchBuffer<T> {
    type Target = [T];

    fn deref(&self) -> &[T] {
        &self.buffer[..self.last_requested_size]
    }
}

impl<T: Default + Clone> DerefMut for ScratchBuffer<T> {
    fn deref_mut(&mut self) -> &mut [T] {
        let size = self.last_requested_size;
        &mut self.buffer[..size]
    }
}

impl<T: Default + Clone> Index<usize> for ScratchBuffer<T> {
    type Output = T;

    fn index(&self, i: usize) -> &T {
        &self.buffer[i]
    }
}

impl<T: Default + Clone> IndexMut<usize> for ScratchBuffer<T> {
    fn index_mut(&mut self, i: usize) -> &mut T {
        &mut self.buffer[i]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_resize_grows() {
        let mut buf = ScratchBuffer::<u8>::new();
        buf.resize(10);
        assert_eq!(buf.size(), 10);
        assert!(buf.capacity() >= 10);

        buf.resize(5);
        assert_eq!(buf.size(), 5);
        // Capacity should not shrink
        assert!(buf.capacity() >= 10);
    }

    #[test]
    fn test_resize_destructive() {
        let mut buf = ScratchBuffer::<u32>::with_capacity(4);
        buf[0] = 42;
        buf.resize_destructive(100);
        assert_eq!(buf.size(), 100);
        assert!(buf.capacity() >= 100);
    }

    #[test]
    fn test_data_slice() {
        let mut buf = ScratchBuffer::<i32>::with_capacity(10);
        buf.resize(3);
        buf[0] = 1;
        buf[1] = 2;
        buf[2] = 3;
        assert_eq!(buf.data(), &[1, 2, 3]);
    }
}
