//! Port of zuyu/src/core/hle/kernel/k_memory_layout.h and k_memory_layout.cpp
//! Status: EN COURS
//! Derniere synchro: 2026-03-05
//!
//! Memory layout configuration constants for the kernel.
//! This module provides the constants and types that describe the
//! physical and virtual memory layout of the emulated Switch hardware.

/// Page size (4 KiB), same as kernel memory_types.h.
pub const PAGE_BITS: usize = 12;
pub const PAGE_SIZE: usize = 1 << PAGE_BITS;

/// L1 block size (1 GiB).
pub const L1_BLOCK_SIZE: u64 = 1024 * 1024 * 1024;
/// L2 block size (2 MiB).
pub const L2_BLOCK_SIZE: u64 = 2 * 1024 * 1024;

/// Main memory size (4 GiB).
pub const MAIN_MEMORY_SIZE: u64 = 4 * 1024 * 1024 * 1024;
/// Maximum main memory size (8 GiB).
pub const MAIN_MEMORY_SIZE_MAX: u64 = 8 * 1024 * 1024 * 1024;

/// Reserved early DRAM (384 KiB).
pub const RESERVED_EARLY_DRAM_SIZE: u64 = 384 * 1024;
/// DRAM physical base address.
pub const DRAM_PHYSICAL_ADDRESS: u64 = 0x8000_0000;

/// Kernel ASLR alignment (2 MiB).
pub const KERNEL_ASLR_ALIGNMENT: u64 = 2 * 1024 * 1024;

/// Kernel virtual address space width (39 bits = 512 GiB).
pub const KERNEL_VIRTUAL_ADDRESS_SPACE_WIDTH: u64 = 1u64 << 39;
/// Kernel physical address space width (48 bits).
pub const KERNEL_PHYSICAL_ADDRESS_SPACE_WIDTH: u64 = 1u64 << 48;

/// Kernel virtual address space base.
///
/// The kernel lives in the upper part of the 64-bit address space.
/// Base = 0 - (1 << 39) = 0xFFFF_FF80_0000_0000 on a real Switch.
/// For HLE purposes we track these as metadata only.
pub const KERNEL_VIRTUAL_ADDRESS_SPACE_BASE: u64 =
    0u64.wrapping_sub(KERNEL_VIRTUAL_ADDRESS_SPACE_WIDTH);
pub const KERNEL_VIRTUAL_ADDRESS_SPACE_END: u64 = KERNEL_VIRTUAL_ADDRESS_SPACE_BASE
    .wrapping_add(KERNEL_VIRTUAL_ADDRESS_SPACE_WIDTH - KERNEL_ASLR_ALIGNMENT);
pub const KERNEL_VIRTUAL_ADDRESS_SPACE_SIZE: u64 =
    KERNEL_VIRTUAL_ADDRESS_SPACE_END.wrapping_sub(KERNEL_VIRTUAL_ADDRESS_SPACE_BASE);

/// Kernel virtual address code base and size.
pub const KERNEL_VIRTUAL_ADDRESS_CODE_BASE: u64 = KERNEL_VIRTUAL_ADDRESS_SPACE_BASE;
pub const KERNEL_VIRTUAL_ADDRESS_CODE_SIZE: u64 = 392 * 1024; // 392 KiB

/// Kernel physical address space.
pub const KERNEL_PHYSICAL_ADDRESS_SPACE_BASE: u64 = 0;
pub const KERNEL_PHYSICAL_ADDRESS_SPACE_END: u64 = KERNEL_PHYSICAL_ADDRESS_SPACE_WIDTH;
pub const KERNEL_PHYSICAL_ADDRESS_SPACE_SIZE: u64 =
    KERNEL_PHYSICAL_ADDRESS_SPACE_END - KERNEL_PHYSICAL_ADDRESS_SPACE_BASE;
pub const KERNEL_PHYSICAL_ADDRESS_CODE_BASE: u64 = DRAM_PHYSICAL_ADDRESS + RESERVED_EARLY_DRAM_SIZE;

/// Kernel slab heap sizes.
pub const KERNEL_SLAB_HEAP_DATA_SIZE: u64 = 5 * 1024 * 1024; // 5 MiB
pub const KERNEL_SLAB_HEAP_GAPS_SIZE_MAX: u64 = 2 * 1024 * 1024 - 64 * 1024;
pub const KERNEL_SLAB_HEAP_SIZE: u64 = KERNEL_SLAB_HEAP_DATA_SIZE + KERNEL_SLAB_HEAP_GAPS_SIZE_MAX;

/// Check if an address is in the kernel virtual address space.
pub fn is_kernel_address(address: u64) -> bool {
    address >= KERNEL_VIRTUAL_ADDRESS_SPACE_BASE && address < KERNEL_VIRTUAL_ADDRESS_SPACE_END
}

/// Compute the maximum page table overhead for a given memory size.
pub fn get_maximum_overhead_size(size: u64) -> u64 {
    let l1_entries = (size + L1_BLOCK_SIZE - 1) / L1_BLOCK_SIZE;
    let l2_entries = (size + L2_BLOCK_SIZE - 1) / L2_BLOCK_SIZE;
    (l1_entries + l2_entries) * PAGE_SIZE as u64
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_page_size() {
        assert_eq!(PAGE_SIZE, 4096);
        assert_eq!(PAGE_BITS, 12);
    }

    #[test]
    fn test_kernel_address_space_constants() {
        // The kernel address space size should be close to 512 GiB minus alignment.
        assert_eq!(KERNEL_VIRTUAL_ADDRESS_SPACE_WIDTH, 1u64 << 39);
        assert_eq!(KERNEL_ASLR_ALIGNMENT, 2 * 1024 * 1024);
    }

    #[test]
    fn test_dram_address() {
        assert_eq!(DRAM_PHYSICAL_ADDRESS, 0x8000_0000);
        assert_eq!(KERNEL_PHYSICAL_ADDRESS_CODE_BASE, 0x8000_0000 + 384 * 1024);
    }
}
