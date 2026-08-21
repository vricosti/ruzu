//! Port of zuyu/src/common/virtual_buffer.h and zuyu/src/common/virtual_buffer.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-05

use std::ptr;

/// Allocate virtual memory pages using mmap (Linux) or VirtualAlloc (Windows).
/// Returns a null pointer on failure.
fn allocate_memory_pages(size: usize) -> *mut u8 {
    if size == 0 {
        return ptr::null_mut();
    }

    #[cfg(unix)]
    unsafe {
        let base = libc::mmap(
            ptr::null_mut(),
            size,
            libc::PROT_READ | libc::PROT_WRITE,
            libc::MAP_ANON | libc::MAP_PRIVATE,
            -1,
            0,
        );
        if base == libc::MAP_FAILED {
            ptr::null_mut()
        } else {
            base as *mut u8
        }
    }

    #[cfg(not(unix))]
    {
        // Fallback: use standard allocation
        let layout = std::alloc::Layout::from_size_align(size, 4096).unwrap();
        unsafe {
            let ptr = std::alloc::alloc_zeroed(layout);
            ptr
        }
    }
}

/// Free virtual memory pages.
fn free_memory_pages(base: *mut u8, size: usize) {
    if base.is_null() || size == 0 {
        return;
    }

    #[cfg(unix)]
    unsafe {
        let ret = libc::munmap(base as *mut libc::c_void, size);
        assert!(ret == 0, "munmap failed");
    }

    #[cfg(not(unix))]
    unsafe {
        let layout = std::alloc::Layout::from_size_align(size, 4096).unwrap();
        std::alloc::dealloc(base, layout);
    }
}

/// A buffer backed by virtual memory that is committed on demand.
/// This is the Rust equivalent of Common::VirtualBuffer<T> in C++.
///
/// The buffer allocates virtual address space using mmap and the OS
/// commits physical pages on first access (demand paging).
pub struct VirtualBuffer<T> {
    base_ptr: *mut T,
    alloc_size: usize, // in bytes
}

impl<T> VirtualBuffer<T> {
    /// Create a new empty VirtualBuffer.
    pub fn new() -> Self {
        Self {
            base_ptr: ptr::null_mut(),
            alloc_size: 0,
        }
    }

    /// Create a new VirtualBuffer with `count` elements.
    pub fn with_count(count: usize) -> Self {
        let alloc_size = count * std::mem::size_of::<T>();
        let base_ptr = allocate_memory_pages(alloc_size) as *mut T;
        assert!(
            !base_ptr.is_null() || alloc_size == 0,
            "Failed to allocate virtual memory"
        );
        Self {
            base_ptr,
            alloc_size,
        }
    }

    /// Resize the buffer to hold `count` elements.
    /// Previous contents are lost.
    pub fn resize(&mut self, count: usize) {
        let new_size = count * std::mem::size_of::<T>();
        if new_size == self.alloc_size {
            return;
        }

        free_memory_pages(self.base_ptr as *mut u8, self.alloc_size);

        self.alloc_size = new_size;
        self.base_ptr = allocate_memory_pages(new_size) as *mut T;
        assert!(
            !self.base_ptr.is_null() || new_size == 0,
            "Failed to allocate virtual memory"
        );
    }

    /// Get the number of elements.
    pub fn size(&self) -> usize {
        if std::mem::size_of::<T>() == 0 {
            0
        } else {
            self.alloc_size / std::mem::size_of::<T>()
        }
    }

    /// Get a raw pointer to the underlying data.
    pub fn data(&self) -> *const T {
        self.base_ptr
    }

    /// Get a mutable raw pointer to the underlying data.
    pub fn data_mut(&mut self) -> *mut T {
        self.base_ptr
    }

    /// Get a reference to the element at `index`.
    ///
    /// # Safety
    /// The caller must ensure `index` is within bounds.
    pub unsafe fn get_unchecked(&self, index: usize) -> &T {
        &*self.base_ptr.add(index)
    }

    /// Get a mutable reference to the element at `index`.
    ///
    /// # Safety
    /// The caller must ensure `index` is within bounds.
    pub unsafe fn get_unchecked_mut(&mut self, index: usize) -> &mut T {
        &mut *self.base_ptr.add(index)
    }
}

impl<T> std::ops::Index<usize> for VirtualBuffer<T> {
    type Output = T;

    fn index(&self, index: usize) -> &T {
        assert!(index < self.size(), "VirtualBuffer index out of bounds");
        unsafe { &*self.base_ptr.add(index) }
    }
}

impl<T> std::ops::IndexMut<usize> for VirtualBuffer<T> {
    fn index_mut(&mut self, index: usize) -> &mut T {
        assert!(index < self.size(), "VirtualBuffer index out of bounds");
        unsafe { &mut *self.base_ptr.add(index) }
    }
}

impl<T> Default for VirtualBuffer<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Drop for VirtualBuffer<T> {
    fn drop(&mut self) {
        free_memory_pages(self.base_ptr as *mut u8, self.alloc_size);
        self.base_ptr = ptr::null_mut();
        self.alloc_size = 0;
    }
}

// VirtualBuffer manages raw memory; the pointer is not shared.
unsafe impl<T: Send> Send for VirtualBuffer<T> {}
unsafe impl<T: Sync> Sync for VirtualBuffer<T> {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_and_index() {
        let mut buf = VirtualBuffer::<u64>::with_count(1024);
        buf[0] = 42;
        buf[1023] = 99;
        assert_eq!(buf[0], 42);
        assert_eq!(buf[1023], 99);
    }

    #[test]
    fn test_resize() {
        let mut buf = VirtualBuffer::<u32>::with_count(100);
        assert_eq!(buf.size(), 100);
        buf.resize(200);
        assert_eq!(buf.size(), 200);
        buf[199] = 7;
        assert_eq!(buf[199], 7);
    }

    #[test]
    fn test_empty() {
        let buf = VirtualBuffer::<u64>::new();
        assert_eq!(buf.size(), 0);
        assert!(buf.data().is_null());
    }
}
