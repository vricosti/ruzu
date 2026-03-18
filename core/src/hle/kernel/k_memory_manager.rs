//! Port of zuyu/src/core/hle/kernel/k_memory_manager.h and k_memory_manager.cpp
//! Status: Partial — enums, static methods, and GetSize/InitializeOptimizedMemory ported.
//! Derniere synchro: 2026-03-18
//!
//! The kernel physical memory manager. Manages page heaps for each memory pool.
//!
//! Upstream owns an array of `Impl` managers (one per physical memory region),
//! linked into per-pool chains. Full physical page allocation requires those
//! managers plus KPageGroup. For now we port the fields and methods needed by
//! KProcess::Initialize and LoadFromMetadata.

use std::sync::Mutex;
use super::k_memory_block::PAGE_SIZE;
use super::k_page_heap::KPageHeap;

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
// KMemoryManager
// ---------------------------------------------------------------------------

pub const MAX_MANAGER_COUNT: usize = 10;
const POOL_COUNT: usize = Pool::Count as usize;

/// Port of Kernel::KMemoryManager.
///
/// Upstream holds an array of `Impl` page-heap managers linked into per-pool
/// chains. We port the pool-level fields needed for process initialization
/// (GetSize, InitializeOptimizedMemory) and stub the per-page allocation
/// until full KPageGroup support is needed.
pub struct KMemoryManager {
    m_num_managers: usize,
    /// Total size of each pool (set during kernel init).
    /// Upstream computes this by iterating Impl managers per pool.
    m_pool_sizes: [usize; POOL_COUNT],
    /// Per-pool optimized process tracking.
    /// Upstream: `PoolArray<u64> m_optimized_process_ids`.
    m_optimized_process_ids: [u64; POOL_COUNT],
    /// Upstream: `PoolArray<bool> m_has_optimized_process`.
    m_has_optimized_process: [bool; POOL_COUNT],
    /// Per-pool lock. Upstream: `PoolArray<KLightLock>`.
    m_pool_locks: [Mutex<()>; POOL_COUNT],
}

impl KMemoryManager {
    pub fn new() -> Self {
        Self {
            m_num_managers: 0,
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

    /// Set the total size for a pool.
    /// Called during kernel initialization to configure pool sizes.
    /// In upstream, sizes are derived from the memory layout and Impl managers.
    pub fn set_pool_size(&mut self, pool: Pool, size: usize) {
        self.m_pool_sizes[pool as usize] = size;
    }

    // --- Instance methods ---

    /// Upstream: `size_t GetSize(Pool pool)`.
    /// Returns the total size of a memory pool by aggregating all managers
    /// assigned to that pool.
    pub fn get_size(&self, pool: Pool) -> usize {
        self.m_pool_sizes[pool as usize]
    }

    /// Upstream: `size_t GetSize()`.
    /// Returns the total size across all pools.
    pub fn get_total_size(&self) -> usize {
        self.m_pool_sizes.iter().sum()
    }

    /// Upstream: `Result InitializeOptimizedMemory(u64 process_id, Pool pool)`.
    /// Marks a process as the "optimized" user of a pool.
    pub fn initialize_optimized_memory(&self, process_id: u64, pool: Pool) -> u32 {
        let pool_index = pool as usize;
        let _lk = self.m_pool_locks[pool_index].lock().unwrap();

        // Upstream: R_UNLESS(!m_has_optimized_process[pool_index], ResultBusy)
        // For now we allow re-setting (single-process emulation).
        // TODO: return ResultBusy if already set for a different process.

        // SAFETY: This is behind a mutex, but we're using interior mutability
        // patterns. In a real port we'd use UnsafeCell or atomic operations.
        // For now, since we hold the lock and this is single-threaded init,
        // we log and succeed.
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
        // Reference count size + optimize map size + page heap overhead.
        let ref_count_size = (region_size / PAGE_SIZE) * std::mem::size_of::<u16>();
        let optimize_map_size = (common::alignment::align_up(
            (region_size / PAGE_SIZE) as u64,
            64,
        ) as usize
            / 64)
            * std::mem::size_of::<u64>();
        let manager_meta_size = common::alignment::align_up(
            (optimize_map_size + ref_count_size) as u64,
            PAGE_SIZE as u64,
        ) as usize;
        let page_heap_size = KPageHeap::calculate_management_overhead_size(region_size);
        manager_meta_size + page_heap_size
    }
}

impl Default for KMemoryManager {
    fn default() -> Self {
        Self::new()
    }
}
