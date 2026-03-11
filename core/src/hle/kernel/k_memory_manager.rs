//! Port of zuyu/src/core/hle/kernel/k_memory_manager.h and k_memory_manager.cpp
//! Status: Stubbed (structure, enums, static methods)
//! Derniere synchro: 2026-03-11
//!
//! The kernel physical memory manager. Manages page heaps for each memory pool.

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

/// Port of Kernel::KMemoryManager.
///
/// Stubbed: the full implementation depends on KernelCore, device memory, KPageGroup, etc.
pub struct KMemoryManager {
    m_num_managers: usize,
}

impl KMemoryManager {
    pub fn new() -> Self {
        Self { m_num_managers: 0 }
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
        ) as usize / 64)
            * std::mem::size_of::<u64>();
        let manager_meta_size =
            common::alignment::align_up((optimize_map_size + ref_count_size) as u64, PAGE_SIZE as u64) as usize;
        let page_heap_size = KPageHeap::calculate_management_overhead_size(region_size);
        manager_meta_size + page_heap_size
    }
}

impl Default for KMemoryManager {
    fn default() -> Self {
        Self::new()
    }
}
