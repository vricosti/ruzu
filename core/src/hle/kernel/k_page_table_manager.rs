//! Port of zuyu/src/core/hle/kernel/k_page_table_manager.h
//! Status: Stubbed
//! Derniere synchro: 2026-03-11

use super::k_page_table_slab_heap::{KPageTableSlabHeap, RefCount};

/// Port of Kernel::KPageTableManager.
///
/// Stubbed: depends on KDynamicResourceManager and KDynamicPageManager.
pub struct KPageTableManager {
    used: usize,
    peak: usize,
    count: usize,
}

impl KPageTableManager {
    pub const PAGE_TABLE_SIZE: usize = KPageTableSlabHeap::PAGE_TABLE_SIZE;

    pub fn new() -> Self {
        Self {
            used: 0,
            peak: 0,
            count: 0,
        }
    }

    pub fn get_size(&self) -> usize {
        0 // Stubbed
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
}

impl Default for KPageTableManager {
    fn default() -> Self {
        Self::new()
    }
}
