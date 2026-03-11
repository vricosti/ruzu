//! Port of zuyu/src/core/hle/kernel/k_dynamic_page_manager.h
//! Status: Stubbed
//! Derniere synchro: 2026-03-11

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
}

impl Default for KDynamicPageManager {
    fn default() -> Self {
        Self::new()
    }
}
