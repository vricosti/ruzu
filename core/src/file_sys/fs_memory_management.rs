// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Ported from: core/file_sys/fs_memory_management.h
//
// In C++, these functions manage aligned heap allocation for filesystem buffers.
// In Rust, we use Vec<u8> with proper alignment via Layout, providing safe
// equivalents that maintain the same alignment guarantees.

use std::alloc::{self, Layout};
use std::ptr;

/// Required alignment for filesystem allocations (matches C++ `alignof(u64)`).
pub const REQUIRED_ALIGNMENT: usize = std::mem::align_of::<u64>();

/// Allocate a buffer of the given size with `REQUIRED_ALIGNMENT`.
/// Returns a raw pointer, or null on failure.
///
/// # Safety
/// The returned pointer must be freed with `deallocate` using the same size.
pub unsafe fn allocate_unsafe(size: usize) -> *mut u8 {
    if size == 0 {
        return ptr::null_mut();
    }
    let layout = match Layout::from_size_align(size, REQUIRED_ALIGNMENT) {
        Ok(l) => l,
        Err(_) => return ptr::null_mut(),
    };
    let ptr = alloc::alloc(layout);
    if ptr.is_null() {
        return ptr::null_mut();
    }
    debug_assert!(
        (ptr as usize) % REQUIRED_ALIGNMENT == 0,
        "Allocated pointer is not properly aligned"
    );
    ptr
}

/// Deallocate a previously allocated buffer.
///
/// # Safety
/// `ptr` must have been returned by `allocate_unsafe` with the same `size`.
pub unsafe fn deallocate_unsafe(ptr: *mut u8, size: usize) {
    if size == 0 {
        return;
    }
    let layout = Layout::from_size_align(size, REQUIRED_ALIGNMENT)
        .expect("Invalid layout for deallocation");
    alloc::dealloc(ptr, layout);
}

/// Allocate a buffer of the given size with `REQUIRED_ALIGNMENT`.
/// Returns a raw pointer, or null on failure.
///
/// # Safety
/// The returned pointer must be freed with `deallocate` using the same size.
pub unsafe fn allocate(size: usize) -> *mut u8 {
    allocate_unsafe(size)
}

/// Deallocate a previously allocated buffer, if the pointer is non-null.
///
/// # Safety
/// `ptr` must have been returned by `allocate` with the same `size`, or be null.
pub unsafe fn deallocate(ptr: *mut u8, size: usize) {
    if !ptr.is_null() {
        deallocate_unsafe(ptr, size);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_allocate_deallocate() {
        unsafe {
            let ptr = allocate(64);
            assert!(!ptr.is_null());
            assert_eq!((ptr as usize) % REQUIRED_ALIGNMENT, 0);
            deallocate(ptr, 64);
        }
    }

    #[test]
    fn test_deallocate_null() {
        unsafe {
            // Should not panic
            deallocate(ptr::null_mut(), 64);
        }
    }

    #[test]
    fn test_allocate_zero() {
        unsafe {
            let ptr = allocate(0);
            assert!(ptr.is_null());
        }
    }
}
