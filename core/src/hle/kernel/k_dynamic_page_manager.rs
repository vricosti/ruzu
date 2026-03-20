//! Port of zuyu/src/core/hle/kernel/k_dynamic_page_manager.h
//! Status: Stubbed
//! Derniere synchro: 2026-03-11

use std::sync::Mutex;

use super::k_memory_block::PAGE_SIZE;
use super::k_page_bitmap::KPageBitmap;

/// Port of KDynamicPageManager::PageBuffer.
#[repr(C)]
pub struct PageBuffer {
    m_buffer: [u8; PAGE_SIZE],
}

const _: () = assert!(std::mem::size_of::<PageBuffer>() == PAGE_SIZE);

/// Port of Kernel::KDynamicPageManager.
///
/// Manages a region of virtual memory for dynamic slab allocations.
/// Stubbed: uses a Vec<u8> for backing memory instead of actual kernel VA mapping.
pub struct KDynamicPageManager {
    lock: Mutex<()>,
    page_bitmap: KPageBitmap,
    used: usize,
    peak: usize,
    count: usize,
    address: u64,
    aligned_address: u64,
    size: usize,
    backing_memory: Vec<u8>,
}

impl KDynamicPageManager {
    pub fn new() -> Self {
        Self {
            lock: Mutex::new(()),
            page_bitmap: KPageBitmap::new(),
            used: 0,
            peak: 0,
            count: 0,
            address: 0,
            aligned_address: 0,
            size: 0,
            backing_memory: Vec::new(),
        }
    }

    pub fn get_address(&self) -> u64 {
        self.address
    }
    pub fn get_size(&self) -> usize {
        self.size
    }
    pub fn get_used(&self) -> usize {
        self.used
    }
    pub fn get_peak(&self) -> usize {
        self.peak
    }
    pub fn get_count(&self) -> usize {
        self.count
    }

    /// Stubbed initialize.
    pub fn initialize(&mut self, memory: u64, size: usize, align: usize) -> Result<(), ()> {
        if size == 0 {
            return Err(());
        }
        self.backing_memory.resize(size, 0);
        self.address = memory;
        self.aligned_address = common::alignment::align_down(memory, align as u64);

        let managed_size = (self.address + size as u64 - self.aligned_address) as usize;
        let overhead_size = common::alignment::align_up(
            KPageBitmap::calculate_management_overhead_size(managed_size / PAGE_SIZE) as u64,
            PAGE_SIZE as u64,
        ) as usize;
        if overhead_size >= size {
            return Err(());
        }

        self.size = common::alignment::align_down((size - overhead_size) as u64, PAGE_SIZE as u64) as usize;
        self.count = self.size / PAGE_SIZE;

        let allocatable_size =
            (self.address + size as u64 - overhead_size as u64 - self.aligned_address) as usize;
        self.page_bitmap.initialize(allocatable_size / PAGE_SIZE);

        // Free pages to bitmap.
        for i in 0..self.count {
            self.page_bitmap.set_bit(
                ((self.address + (i * PAGE_SIZE) as u64 - self.aligned_address)
                    / PAGE_SIZE as u64) as usize,
            );
        }

        Ok(())
    }

    /// Get a mutable pointer into backing memory at the given virtual address.
    /// Port of upstream `GetPointer<T>`.
    pub fn get_pointer_mut(&mut self, addr: u64) -> &mut [u8] {
        let offset = (addr - self.address) as usize;
        &mut self.backing_memory[offset..]
    }

    /// Get a read-only pointer into backing memory at the given virtual address.
    pub fn get_pointer(&self, addr: u64) -> &[u8] {
        let offset = (addr - self.address) as usize;
        &self.backing_memory[offset..]
    }

    /// Allocate a single page.
    /// Port of upstream `KDynamicPageManager::Allocate()`.
    /// Returns the offset (as a virtual address) of the allocated page, or None.
    pub fn allocate(&mut self) -> Option<u64> {
        let _lock = self.lock.lock().unwrap();

        // Find a free block.
        let soffset = self.page_bitmap.find_free_block(true);
        if soffset < 0 {
            return None;
        }
        let offset = soffset as usize;

        // Update tracking.
        self.page_bitmap.clear_bit(offset);
        self.used += 1;
        if self.used > self.peak {
            self.peak = self.used;
        }

        // Return the address of the allocated page.
        Some(self.aligned_address + (offset * PAGE_SIZE) as u64)
    }

    /// Allocate a contiguous range of pages.
    /// Port of upstream `KDynamicPageManager::Allocate(size_t count)`.
    pub fn allocate_count(&mut self, count: usize) -> Option<u64> {
        let _lock = self.lock.lock().unwrap();

        let soffset = self.page_bitmap.find_free_range(count);
        if soffset < 0 {
            return None;
        }
        let offset = soffset as usize;

        self.page_bitmap.clear_range(offset, count);
        self.used += count;
        if self.used > self.peak {
            self.peak = self.used;
        }

        Some(self.aligned_address + (offset * PAGE_SIZE) as u64)
    }

    /// Free a previously allocated page.
    /// Port of upstream `KDynamicPageManager::Free`.
    pub fn free(&mut self, page_addr: u64) {
        // Zero the page.
        let page_offset = (page_addr - self.address) as usize;
        if page_offset + PAGE_SIZE <= self.backing_memory.len() {
            self.backing_memory[page_offset..page_offset + PAGE_SIZE].fill(0);
        }

        let _lock = self.lock.lock().unwrap();

        // Set the bit for the free page.
        let offset = ((page_addr - self.aligned_address) / PAGE_SIZE as u64) as usize;
        self.page_bitmap.set_bit(offset);

        self.used -= 1;
    }
}

impl Default for KDynamicPageManager {
    fn default() -> Self {
        Self::new()
    }
}
