//! Port of zuyu/src/core/hle/kernel/board/nintendo/nx/k_memory_layout.h/.cpp
//! Status: COMPLET (stub — full implementation requires KMemoryLayout, KMemoryManager)
//! Derniere synchro: 2026-03-11
//!
//! Board-level memory layout constants and initialization for the
//! Nintendo NX (Switch). The .cpp sets up device physical memory regions,
//! DRAM regions, and pool partition regions.

/// Main physical memory address (DRAM base).
/// Maps to upstream `MainMemoryAddress` in the Kernel namespace.
pub const MAIN_MEMORY_ADDRESS: u64 = 0x8000_0000;

/// DRAM physical base address used for memory layout initialization.
/// Maps to upstream `DramPhysicalAddress`.
pub const DRAM_PHYSICAL_ADDRESS: u64 = 0x8000_0000;

/// Size of the early-reserved DRAM region.
/// Maps to upstream `ReservedEarlyDramSize`.
pub const RESERVED_EARLY_DRAM_SIZE: usize = 0x6_0000; // 384 KiB

/// Carveout alignment for pool partitions.
const CARVEOUT_ALIGNMENT: usize = 0x2_0000; // 128 KiB

/// Maximum carveout size.
const CARVEOUT_SIZE_MAX: usize = 512 * 1024 * 1024 - CARVEOUT_ALIGNMENT;

/// Set up device physical memory regions (MMIO, interrupt controllers, etc.).
///
/// Upstream inserts entries into the KMemoryLayout physical memory region tree.
/// This is a stub; the full implementation requires KMemoryLayout.
pub fn setup_device_physical_memory_regions() {
    // TODO: Insert PMC, memory controller, GIC, IRAM, etc. regions
    // into the KMemoryLayout physical memory region tree.
}

/// Set up DRAM physical memory regions.
///
/// Upstream inserts the main DRAM region and early-reserved region.
pub fn setup_dram_physical_memory_regions() {
    // TODO: Insert DRAM, DramReservedEarly, and optional KTrace regions.
}

/// Set up pool partition memory regions (application, applet, system, etc.).
///
/// This is the most complex initialization function, splitting DRAM into
/// application pool, applet pool, unsafe system pool, pool management,
/// and secure system pool regions.
pub fn setup_pool_partition_memory_regions() {
    // TODO: Full implementation requires KMemoryLayout, KSystemControl::Init,
    //       and KMemoryManager::CalculateManagementOverheadSize.
}
