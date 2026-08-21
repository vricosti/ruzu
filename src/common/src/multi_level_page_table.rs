//! Port of zuyu/src/common/multi_level_page_table.h, multi_level_page_table.inc,
//! and multi_level_page_table.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-05

use std::ptr;

/// Multi-level page table (two-level lookup).
/// The first level is a vector of pointers to second-level chunks.
/// The entire table is backed by a single mmap allocation for efficiency.
pub struct MultiLevelPageTable<T: Copy + Default> {
    #[allow(dead_code)]
    address_space_bits: usize,
    #[allow(dead_code)]
    first_level_bits: usize,
    #[allow(dead_code)]
    page_bits: usize,
    first_level_shift: usize,
    first_level_chunk_size: usize,
    alloc_size: usize,
    first_level_map: Vec<*mut libc::c_void>,
    base_ptr: *mut T,
}

impl<T: Copy + Default> MultiLevelPageTable<T> {
    /// Create an empty (uninitialized) multi-level page table.
    pub fn new() -> Self {
        Self {
            address_space_bits: 0,
            first_level_bits: 0,
            page_bits: 0,
            first_level_shift: 0,
            first_level_chunk_size: 0,
            alloc_size: 0,
            first_level_map: Vec::new(),
            base_ptr: ptr::null_mut(),
        }
    }

    /// Create a new multi-level page table.
    ///
    /// # Arguments
    /// * `address_space_bits` - Total address space bits
    /// * `first_level_bits` - Number of bits for the first level index
    /// * `page_bits` - Number of bits for the page offset
    pub fn with_params(
        address_space_bits: usize,
        first_level_bits: usize,
        page_bits: usize,
    ) -> Self {
        if page_bits == 0 {
            return Self {
                address_space_bits,
                first_level_bits,
                page_bits,
                first_level_shift: 0,
                first_level_chunk_size: 0,
                alloc_size: 0,
                first_level_map: Vec::new(),
                base_ptr: ptr::null_mut(),
            };
        }

        let first_level_shift = address_space_bits - first_level_bits;
        let first_level_chunk_size =
            (1usize << (first_level_shift - page_bits)) * std::mem::size_of::<T>();
        let alloc_size = (1usize << (address_space_bits - page_bits)) * std::mem::size_of::<T>();
        let first_level_size = 1usize << first_level_bits;
        let first_level_map = vec![ptr::null_mut(); first_level_size];

        #[cfg(unix)]
        let base_ptr = unsafe {
            let base = libc::mmap(
                ptr::null_mut(),
                alloc_size,
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
                -1,
                0,
            );
            if base == libc::MAP_FAILED {
                panic!(
                    "MultiLevelPageTable: mmap failed to allocate {} bytes",
                    alloc_size
                );
            }
            base as *mut T
        };

        #[cfg(not(unix))]
        let base_ptr = {
            let layout =
                std::alloc::Layout::from_size_align(alloc_size, 4096).expect("Invalid layout");
            unsafe {
                let ptr = std::alloc::alloc_zeroed(layout);
                assert!(!ptr.is_null(), "Failed to allocate memory");
                ptr as *mut T
            }
        };

        Self {
            address_space_bits,
            first_level_bits,
            page_bits,
            first_level_shift,
            first_level_chunk_size,
            alloc_size,
            first_level_map,
            base_ptr,
        }
    }

    /// Reserve a range of the address space by allocating the corresponding first-level entries.
    pub fn reserve_range(&mut self, start: u64, size: usize) {
        let new_start = (start >> self.first_level_shift) as usize;
        let new_end = ((start + size as u64) >> self.first_level_shift) as usize;
        for i in new_start..=new_end {
            if self.first_level_map[i].is_null() {
                self.allocate_level(i as u64);
            }
        }
    }

    /// Get a reference to the element at the given index.
    pub fn get(&self, index: usize) -> &T {
        assert!(!self.base_ptr.is_null());
        unsafe { &*self.base_ptr.add(index) }
    }

    /// Get a mutable reference to the element at the given index.
    pub fn get_mut(&mut self, index: usize) -> &mut T {
        assert!(!self.base_ptr.is_null());
        unsafe { &mut *self.base_ptr.add(index) }
    }

    /// Get a raw pointer to the underlying data.
    pub fn data(&self) -> *const T {
        self.base_ptr
    }

    /// Get a mutable raw pointer to the underlying data.
    pub fn data_mut(&mut self) -> *mut T {
        self.base_ptr
    }

    fn allocate_level(&mut self, level: u64) {
        let offset = level as usize * self.first_level_chunk_size;
        let ptr = unsafe { (self.base_ptr as *mut u8).add(offset) as *mut libc::c_void };

        #[cfg(unix)]
        let base = unsafe {
            let base = libc::mmap(
                ptr,
                self.first_level_chunk_size,
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_ANONYMOUS | libc::MAP_PRIVATE,
                -1,
                0,
            );
            if base == libc::MAP_FAILED {
                panic!(
                    "MultiLevelPageTable: mmap failed to allocate level {}",
                    level
                );
            }
            base
        };

        #[cfg(not(unix))]
        let base = ptr; // Already allocated in the fallback path

        self.first_level_map[level as usize] = base;
    }
}

impl<T: Copy + Default> std::ops::Index<usize> for MultiLevelPageTable<T> {
    type Output = T;

    fn index(&self, index: usize) -> &T {
        self.get(index)
    }
}

impl<T: Copy + Default> std::ops::IndexMut<usize> for MultiLevelPageTable<T> {
    fn index_mut(&mut self, index: usize) -> &mut T {
        self.get_mut(index)
    }
}

impl<T: Copy + Default> Default for MultiLevelPageTable<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Copy + Default> Drop for MultiLevelPageTable<T> {
    fn drop(&mut self) {
        if self.base_ptr.is_null() {
            return;
        }
        #[cfg(unix)]
        unsafe {
            let ret = libc::munmap(self.base_ptr as *mut libc::c_void, self.alloc_size);
            assert!(ret == 0, "munmap failed");
        }
        #[cfg(not(unix))]
        unsafe {
            if self.alloc_size > 0 {
                let layout = std::alloc::Layout::from_size_align(self.alloc_size, 4096).unwrap();
                std::alloc::dealloc(self.base_ptr as *mut u8, layout);
            }
        }
        self.base_ptr = ptr::null_mut();
    }
}

// Safety: The buffer is managed by this struct and access is controlled.
unsafe impl<T: Copy + Default + Send> Send for MultiLevelPageTable<T> {}
unsafe impl<T: Copy + Default + Sync> Sync for MultiLevelPageTable<T> {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_and_access() {
        let mut table = MultiLevelPageTable::<u64>::with_params(36, 10, 12);
        table.reserve_range(0, 0x1000);
        table[0] = 42;
        assert_eq!(table[0], 42);
    }

    #[test]
    fn test_u32_table() {
        let mut table = MultiLevelPageTable::<u32>::with_params(32, 10, 12);
        table.reserve_range(0, 0x1000);
        table[0] = 123;
        assert_eq!(table[0], 123);
    }
}
