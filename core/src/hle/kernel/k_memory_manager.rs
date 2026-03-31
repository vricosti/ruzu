//! Port of zuyu/src/core/hle/kernel/k_memory_manager.h and k_memory_manager.cpp
//! Status: Ported — Impl managers with page heap allocation and ref counting.
//! Derniere synchro: 2026-03-19
//!
//! The kernel physical memory manager. Manages page heaps for each memory pool.
//!
//! Upstream owns an array of `Impl` managers (one per physical memory region),
//! linked into per-pool chains. Each Impl wraps a KPageHeap for block allocation
//! and maintains per-page reference counts.

use super::k_memory_block::PAGE_SIZE;
use super::k_page_heap::KPageHeap;
use std::sync::Mutex;

// ---------------------------------------------------------------------------
// Pool
// ---------------------------------------------------------------------------

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Pool {
    Application = 0,
    Applet = 1,
    System = 2,
    SystemNonSecure = 3,
    Count = 4,
}

impl Pool {
    pub const SHIFT: u32 = 4;
    pub const MASK: u32 = 0xF << Self::SHIFT;

    /// Alias: Secure == System (upstream `Pool::Secure = Pool::System`).
    pub const SECURE: Pool = Pool::System;
}

// ---------------------------------------------------------------------------
// Direction
// ---------------------------------------------------------------------------

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Direction {
    FromFront = 0,
    FromBack = 1,
}

impl Direction {
    pub const SHIFT: u32 = 0;
    pub const MASK: u32 = 0xF << Self::SHIFT;
}

// ---------------------------------------------------------------------------
// Impl — per-region page heap manager
// ---------------------------------------------------------------------------

/// Port of Kernel::KMemoryManager::Impl.
///
/// Each Impl manages a contiguous physical memory region with a KPageHeap
/// for block allocation and per-page reference counts.
struct Impl {
    m_heap: KPageHeap,
    m_page_reference_counts: Vec<u16>,
    m_pool: Pool,
}

impl Impl {
    fn new() -> Self {
        Self {
            m_heap: KPageHeap::new(),
            m_page_reference_counts: Vec::new(),
            m_pool: Pool::Application,
        }
    }

    /// Initialize the manager for a physical memory region.
    ///
    /// Upstream `Impl::Initialize` takes management region pointers for metadata
    /// storage. We simplify by heap-allocating the ref count Vec and letting
    /// KPageHeap manage its own bitmap storage.
    fn initialize(&mut self, address: u64, size: usize, pool: Pool) {
        self.m_pool = pool;

        // Initialize the page heap for this region.
        self.m_heap.initialize(address, size);

        // Allocate reference counts (one u16 per page).
        let num_pages = size / PAGE_SIZE;
        self.m_page_reference_counts = vec![0u16; num_pages];

        // Free the entire region to the heap so it's available for allocation.
        self.m_heap.free(address, num_pages);

        log::debug!(
            "KMemoryManager::Impl::initialize: pool={:?}, addr={:#x}, size={:#x} ({} pages)",
            pool,
            address,
            size,
            num_pages
        );
    }

    fn get_pool(&self) -> Pool {
        self.m_pool
    }

    fn get_size(&self) -> usize {
        self.m_heap.get_size()
    }

    fn get_end_address(&self) -> u64 {
        self.m_heap.get_end_address()
    }

    fn get_page_offset(&self, address: u64) -> usize {
        self.m_heap.get_page_offset(address)
    }

    fn get_page_offset_to_end(&self, address: u64) -> usize {
        self.m_heap.get_page_offset_to_end(address)
    }

    /// Allocate aligned pages from the heap.
    /// Upstream: `Impl::AllocateAligned`.
    fn allocate_aligned(&mut self, heap_index: i32, num_pages: usize, align_pages: usize) -> u64 {
        self.m_heap
            .allocate_aligned(heap_index, num_pages, align_pages)
    }

    /// Free pages back to the heap.
    fn free(&mut self, addr: u64, num_pages: usize) {
        self.m_heap.free(addr, num_pages);
    }

    /// Set reference counts to 1 for first open.
    /// Upstream: `Impl::OpenFirst`.
    fn open_first(&mut self, address: u64, num_pages: usize) {
        let mut index = self.get_page_offset(address);
        let end = index + num_pages;
        while index < end {
            self.m_page_reference_counts[index] += 1;
            debug_assert_eq!(
                self.m_page_reference_counts[index], 1,
                "OpenFirst: ref count must be 1 at index {}",
                index
            );
            index += 1;
        }
    }

    /// Increment reference counts (must already be >= 1).
    /// Upstream: `Impl::Open`.
    fn open(&mut self, address: u64, num_pages: usize) {
        let mut index = self.get_page_offset(address);
        let end = index + num_pages;
        while index < end {
            self.m_page_reference_counts[index] += 1;
            debug_assert!(
                self.m_page_reference_counts[index] > 1,
                "Open: ref count must be > 1 at index {}",
                index
            );
            index += 1;
        }
    }

    /// Decrement reference counts; free pages when count reaches 0.
    /// Upstream: `Impl::Close`.
    fn close(&mut self, address: u64, num_pages: usize) {
        let mut index = self.get_page_offset(address);
        let end = index + num_pages;

        let mut free_start = 0usize;
        let mut free_count = 0usize;
        while index < end {
            debug_assert!(
                self.m_page_reference_counts[index] > 0,
                "Close: ref count must be > 0 at index {}",
                index
            );
            self.m_page_reference_counts[index] -= 1;
            let ref_count = self.m_page_reference_counts[index];

            if ref_count == 0 {
                if free_count > 0 {
                    free_count += 1;
                } else {
                    free_start = index;
                    free_count = 1;
                }
            } else {
                if free_count > 0 {
                    self.free(
                        self.m_heap.get_address() + (free_start as u64) * PAGE_SIZE as u64,
                        free_count,
                    );
                    free_count = 0;
                }
            }

            index += 1;
        }

        if free_count > 0 {
            self.free(
                self.m_heap.get_address() + (free_start as u64) * PAGE_SIZE as u64,
                free_count,
            );
        }
    }

    /// Check if this manager contains the given address.
    fn contains(&self, address: u64) -> bool {
        address >= self.m_heap.get_address() && address < self.get_end_address()
    }
}

// ---------------------------------------------------------------------------
// KMemoryManager
// ---------------------------------------------------------------------------

pub const MAX_MANAGER_COUNT: usize = 10;
const POOL_COUNT: usize = Pool::Count as usize;

/// Port of Kernel::KMemoryManager.
///
/// Upstream holds an array of `Impl` page-heap managers linked into per-pool
/// chains. We use a Vec of Impl managers with per-pool head indices.
pub struct KMemoryManager {
    m_managers: Vec<Impl>,
    m_num_managers: usize,
    /// Head index into m_managers for each pool.
    m_pool_managers_head: [Option<usize>; POOL_COUNT],
    /// Total size of each pool (set during kernel init).
    m_pool_sizes: [usize; POOL_COUNT],
    /// Per-pool optimized process tracking.
    m_optimized_process_ids: [u64; POOL_COUNT],
    m_has_optimized_process: [bool; POOL_COUNT],
    /// Per-pool lock. Upstream: `PoolArray<KLightLock>`.
    m_pool_locks: [Mutex<()>; POOL_COUNT],
}

impl KMemoryManager {
    pub fn new() -> Self {
        Self {
            m_managers: Vec::new(),
            m_num_managers: 0,
            m_pool_managers_head: [None; POOL_COUNT],
            m_pool_sizes: [0; POOL_COUNT],
            m_optimized_process_ids: [0; POOL_COUNT],
            m_has_optimized_process: [false; POOL_COUNT],
            m_pool_locks: [
                Mutex::new(()),
                Mutex::new(()),
                Mutex::new(()),
                Mutex::new(()),
            ],
        }
    }

    /// Initialize a pool with a physical memory region.
    ///
    /// Simplified version of upstream's `Initialize()` which traverses the
    /// memory layout tree. Here the caller provides the region directly.
    /// This creates one Impl manager for the pool and makes it available
    /// for allocation.
    pub fn initialize_pool(&mut self, pool: Pool, phys_start: u64, size: usize) {
        assert!(
            self.m_num_managers < MAX_MANAGER_COUNT,
            "Too many memory managers"
        );

        let mut manager = Impl::new();
        manager.initialize(phys_start, size, pool);
        self.m_managers.push(manager);

        let manager_index = self.m_num_managers;
        self.m_num_managers += 1;

        // Set as head for this pool.
        let pool_index = pool as usize;
        self.m_pool_managers_head[pool_index] = Some(manager_index);
        self.m_pool_sizes[pool_index] = size;

        log::info!(
            "KMemoryManager: initialized pool {:?} at {:#x}..{:#x} ({:#x} bytes)",
            pool,
            phys_start,
            phys_start + size as u64,
            size
        );
    }

    /// Set the total size for a pool.
    /// Called during kernel initialization to configure pool sizes.
    pub fn set_pool_size(&mut self, pool: Pool, size: usize) {
        self.m_pool_sizes[pool as usize] = size;
    }

    // --- Allocation ---

    /// Allocate contiguous physical pages and open the first reference.
    ///
    /// Upstream: `KPhysicalAddress AllocateAndOpenContinuous(size_t num_pages,
    ///     size_t align_pages, u32 option)`.
    pub fn allocate_and_open_continuous(
        &mut self,
        num_pages: usize,
        align_pages: usize,
        option: u32,
    ) -> u64 {
        if num_pages == 0 {
            return 0;
        }

        let (pool, _dir) = Self::decode_option(option);
        let pool_index = pool as usize;
        let _lk = self.m_pool_locks[pool_index].lock().unwrap();

        let heap_index = KPageHeap::get_aligned_block_index(num_pages, align_pages);

        // Find a manager for this pool and try to allocate.
        let manager_idx = self.m_pool_managers_head[pool_index];
        let Some(idx) = manager_idx else {
            log::error!("AllocateAndOpenContinuous: no manager for pool {:?}", pool);
            return 0;
        };

        let manager = &mut self.m_managers[idx];
        let allocated_block = manager.allocate_aligned(heap_index, num_pages, align_pages);

        if allocated_block == 0 {
            log::error!(
                "AllocateAndOpenContinuous: allocation failed for {} pages in pool {:?}",
                num_pages,
                pool
            );
            return 0;
        }

        // Open first reference.
        manager.open_first(allocated_block, num_pages);

        log::debug!(
            "AllocateAndOpenContinuous: allocated {} pages at {:#x} from pool {:?}",
            num_pages,
            allocated_block,
            pool
        );

        allocated_block
    }

    /// Open (increment) reference counts for pages at the given address.
    /// Upstream: `void Open(KPhysicalAddress, size_t)`.
    pub fn open(&mut self, address: u64, num_pages: usize) {
        let manager = self.get_manager_mut(address);
        manager.open(address, num_pages);
    }

    /// Close (decrement) reference counts; free pages when count reaches 0.
    /// Upstream: `void Close(KPhysicalAddress, size_t)`.
    pub fn close(&mut self, address: u64, num_pages: usize) {
        let manager = self.get_manager_mut(address);
        manager.close(address, num_pages);
    }

    // --- Query ---

    /// Upstream: `size_t GetSize(Pool pool)`.
    pub fn get_size(&self, pool: Pool) -> usize {
        self.m_pool_sizes[pool as usize]
    }

    /// Upstream: `size_t GetSize()`.
    pub fn get_total_size(&self) -> usize {
        self.m_pool_sizes.iter().sum()
    }

    // --- Optimized memory ---

    /// Upstream: `Result InitializeOptimizedMemory(u64 process_id, Pool pool)`.
    pub fn initialize_optimized_memory(&self, process_id: u64, pool: Pool) -> u32 {
        let pool_index = pool as usize;
        let _lk = self.m_pool_locks[pool_index].lock().unwrap();
        log::trace!(
            "InitializeOptimizedMemory: process_id={:#x}, pool={:?}",
            process_id,
            pool
        );
        0 // RESULT_SUCCESS
    }

    /// Upstream: `void FinalizeOptimizedMemory(u64 process_id, Pool pool)`.
    pub fn finalize_optimized_memory(&self, process_id: u64, pool: Pool) {
        let pool_index = pool as usize;
        let _lk = self.m_pool_locks[pool_index].lock().unwrap();
        log::trace!(
            "FinalizeOptimizedMemory: process_id={:#x}, pool={:?}",
            process_id,
            pool
        );
    }

    // --- Static helpers ---

    pub fn encode_option(pool: Pool, dir: Direction) -> u32 {
        ((pool as u32) << Pool::SHIFT) | ((dir as u32) << Direction::SHIFT)
    }

    pub fn get_pool(option: u32) -> Pool {
        let raw = (option & Pool::MASK) >> Pool::SHIFT;
        match raw {
            0 => Pool::Application,
            1 => Pool::Applet,
            2 => Pool::System,
            3 => Pool::SystemNonSecure,
            _ => panic!("Invalid pool value: {}", raw),
        }
    }

    pub fn get_direction(option: u32) -> Direction {
        let raw = (option & Direction::MASK) >> Direction::SHIFT;
        match raw {
            0 => Direction::FromFront,
            1 => Direction::FromBack,
            _ => panic!("Invalid direction value: {}", raw),
        }
    }

    pub fn decode_option(option: u32) -> (Pool, Direction) {
        (Self::get_pool(option), Self::get_direction(option))
    }

    pub fn calculate_management_overhead_size(region_size: usize) -> usize {
        let ref_count_size = (region_size / PAGE_SIZE) * std::mem::size_of::<u16>();
        let optimize_map_size =
            (common::alignment::align_up((region_size / PAGE_SIZE) as u64, 64) as usize / 64)
                * std::mem::size_of::<u64>();
        let manager_meta_size = common::alignment::align_up(
            (optimize_map_size + ref_count_size) as u64,
            PAGE_SIZE as u64,
        ) as usize;
        let page_heap_size = KPageHeap::calculate_management_overhead_size(region_size);
        manager_meta_size + page_heap_size
    }

    // --- Private helpers ---

    fn get_manager_mut(&mut self, address: u64) -> &mut Impl {
        for manager in &mut self.m_managers {
            if manager.contains(address) {
                return manager;
            }
        }
        panic!("KMemoryManager: no manager for address {:#x}", address);
    }
}

impl Default for KMemoryManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn allocate_and_open_continuous_returns_address_in_pool() {
        let mut mgr = KMemoryManager::new();
        // Initialize a small pool: 16 pages at physical address 0x1_0000_0000.
        let pool_base = 0x1_0000_0000u64;
        let pool_size = 16 * PAGE_SIZE;
        mgr.initialize_pool(Pool::SECURE, pool_base, pool_size);

        let option = KMemoryManager::encode_option(Pool::SECURE, Direction::FromBack);
        let addr = mgr.allocate_and_open_continuous(1, 1, option);
        assert_ne!(addr, 0);
        assert!(addr >= pool_base);
        assert!(addr < pool_base + pool_size as u64);
    }

    #[test]
    fn open_and_close_ref_counting() {
        let mut mgr = KMemoryManager::new();
        let pool_base = 0x1_0000_0000u64;
        let pool_size = 16 * PAGE_SIZE;
        mgr.initialize_pool(Pool::SECURE, pool_base, pool_size);

        let option = KMemoryManager::encode_option(Pool::SECURE, Direction::FromBack);

        // Allocate 1 page (OpenFirst sets refcount to 1).
        let addr = mgr.allocate_and_open_continuous(1, 1, option);
        assert_ne!(addr, 0);

        // Open again (refcount becomes 2).
        mgr.open(addr, 1);

        // First close (refcount becomes 1, page NOT freed).
        mgr.close(addr, 1);

        // Second close (refcount becomes 0, page freed).
        mgr.close(addr, 1);
    }
}
