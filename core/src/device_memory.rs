//! Port of zuyu/src/core/device_memory.h and zuyu/src/core/device_memory.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-05
//!
//! Device memory allocation. Allocates a large block of host memory for the
//! emulated Switch DRAM, using common::host_memory::HostMemory.

use common::host_memory::HostMemory;

/// DRAM memory map constants, matching C++ Core::DramMemoryMap.
pub mod dram_memory_map {
    pub const BASE: u64 = 0x8000_0000;
    pub const KERNEL_RESERVE_BASE: u64 = BASE + 0x60000;
    pub const SLAB_HEAP_BASE: u64 = KERNEL_RESERVE_BASE + 0x85000;
}

/// Default intended memory size (4 GB DRAM).
/// In the C++ code this comes from KSystemControl::Init::GetIntendedMemorySize().
/// We hardcode the default value here; the kernel module can override if needed.
const DEFAULT_INTENDED_MEMORY_SIZE: usize = 4 * 1024 * 1024 * 1024; // 4 GiB

/// Virtual reserve size for the host memory mapping.
const VIRTUAL_RESERVE_SIZE: usize = 1 << 39; // 512 GiB

/// Represents the Switch's device DRAM, backed by a host memory allocation.
pub struct DeviceMemory {
    pub buffer: HostMemory,
}

impl DeviceMemory {
    /// Creates a new DeviceMemory with the default intended memory size.
    pub fn new() -> Self {
        Self::with_size(DEFAULT_INTENDED_MEMORY_SIZE)
    }

    /// Creates a new DeviceMemory with a specific backing size.
    pub fn with_size(backing_size: usize) -> Self {
        let buffer = HostMemory::new(backing_size, VIRTUAL_RESERVE_SIZE);
        Self { buffer }
    }

    /// Gets the physical address for a pointer within the device memory.
    /// The physical address is offset by DramMemoryMap::Base.
    ///
    /// # Safety
    /// The pointer must be within the backing memory range.
    pub unsafe fn get_physical_addr(&self, ptr: *const u8) -> u64 {
        let offset = ptr as usize - self.buffer.backing_base_pointer() as usize;
        offset as u64 + dram_memory_map::BASE
    }

    /// Gets the raw physical address (without DramMemoryMap::Base offset).
    ///
    /// # Safety
    /// The pointer must be within the backing memory range.
    pub unsafe fn get_raw_physical_addr(&self, ptr: *const u8) -> u64 {
        (ptr as usize - self.buffer.backing_base_pointer() as usize) as u64
    }

    /// Gets a mutable pointer to a physical address within device memory.
    ///
    /// # Safety
    /// The address must be a valid physical address within the DRAM range.
    pub unsafe fn get_pointer(&self, addr: u64) -> *mut u8 {
        self.buffer
            .backing_base_pointer()
            .add((addr - dram_memory_map::BASE) as usize)
    }

    /// Gets a const pointer to a physical address within device memory.
    ///
    /// # Safety
    /// The address must be a valid physical address within the DRAM range.
    pub unsafe fn get_pointer_const(&self, addr: u64) -> *const u8 {
        self.buffer
            .backing_base_pointer()
            .add((addr - dram_memory_map::BASE) as usize)
            as *const u8
    }

    /// Gets a mutable pointer from a raw physical address (no base offset).
    ///
    /// # Safety
    /// The raw address must be within the backing memory range.
    pub unsafe fn get_pointer_from_raw(&self, addr: u64) -> *mut u8 {
        self.buffer.backing_base_pointer().add(addr as usize)
    }

    /// Gets a const pointer from a raw physical address (no base offset).
    ///
    /// # Safety
    /// The raw address must be within the backing memory range.
    pub unsafe fn get_pointer_from_raw_const(&self, addr: u64) -> *const u8 {
        self.buffer.backing_base_pointer().add(addr as usize) as *const u8
    }
}

impl Default for DeviceMemory {
    fn default() -> Self {
        Self::new()
    }
}
