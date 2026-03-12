// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `shader_recompiler/object_pool.h`
//!
//! Arena-style object pool for allocating shader IR objects with batch
//! deallocation. Objects are allocated in chunks and freed all at once
//! via `release_contents()`.

use std::cell::UnsafeCell;

/// Arena-style object pool that allocates objects in chunks.
///
/// All allocated objects are dropped together when `release_contents()` is
/// called or when the pool itself is dropped. This matches the upstream
/// `ObjectPool<T>` which uses chunk-based allocation with batch release.
pub struct ObjectPool<T> {
    chunks: Vec<Vec<UnsafeCell<Option<T>>>>,
    chunk_size: usize,
    current_chunk: usize,
    current_index: usize,
}

impl<T> ObjectPool<T> {
    /// Create a new object pool with the given chunk size.
    pub fn new(chunk_size: usize) -> Self {
        let chunk_size = if chunk_size == 0 { 8192 } else { chunk_size };
        let mut pool = Self {
            chunks: Vec::new(),
            chunk_size,
            current_chunk: 0,
            current_index: 0,
        };
        pool.add_chunk(chunk_size);
        pool
    }

    /// Allocate a new object in the pool, returning a reference to it.
    ///
    /// # Safety
    /// The returned reference is valid until `release_contents()` is called
    /// or the pool is dropped.
    pub fn create(&mut self, value: T) -> &mut T {
        if self.current_index >= self.chunks[self.current_chunk].len() {
            self.add_chunk(self.chunk_size);
            self.current_chunk = self.chunks.len() - 1;
            self.current_index = 0;
        }
        let cell = &self.chunks[self.current_chunk][self.current_index];
        // SAFETY: We have exclusive access via &mut self and the cell is unoccupied
        unsafe {
            let ptr = cell.get();
            *ptr = Some(value);
            self.current_index += 1;
            (*ptr).as_mut().unwrap()
        }
    }

    /// Release all objects in the pool, resetting it for reuse.
    pub fn release_contents(&mut self) {
        // Drop all objects
        for chunk in &mut self.chunks {
            for cell in chunk.iter() {
                // SAFETY: We have exclusive access via &mut self
                unsafe {
                    *cell.get() = None;
                }
            }
        }
        // Consolidate into a single chunk if needed
        if self.chunks.len() > 1 {
            let total = self.chunks[0].len() + self.chunk_size * (self.chunks.len() - 1);
            self.chunks.clear();
            self.add_chunk(total);
        }
        self.current_chunk = 0;
        self.current_index = 0;
    }

    fn add_chunk(&mut self, size: usize) {
        let mut chunk = Vec::with_capacity(size);
        for _ in 0..size {
            chunk.push(UnsafeCell::new(None));
        }
        self.chunks.push(chunk);
    }
}

impl<T> Default for ObjectPool<T> {
    fn default() -> Self {
        Self::new(8192)
    }
}

impl<T> Drop for ObjectPool<T> {
    fn drop(&mut self) {
        // All Option<T> values are dropped when the Vecs are dropped,
        // which handles calling T's destructor.
    }
}
