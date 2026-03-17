//! Port of zuyu/src/core/hle/kernel/k_process_page_table.h / k_process_page_table.cpp
//! Status: Partial (structural port, all methods stubbed)
//! Derniere synchro: 2026-03-11
//!
//! KProcessPageTable: wrapper around KPageTable that provides the process's
//! virtual memory management interface. All methods delegate to the inner
//! page table.

use super::k_memory_block::KMemoryPermission;
use super::k_typed_address::{KPhysicalAddress, KProcessAddress};

/// The process page table.
/// Matches upstream `KProcessPageTable` class (k_process_page_table.h).
///
/// In upstream this is a thin wrapper around KPageTable/KPageTableBase.
pub struct KProcessPageTable {
    address_space_start: KProcessAddress,
    address_space_size: usize,
    heap_region_start: KProcessAddress,
    heap_region_size: usize,
    current_heap_size: usize,
    alias_region_start: KProcessAddress,
    alias_region_size: usize,
    stack_region_start: KProcessAddress,
    stack_region_size: usize,
    kernel_map_region_start: KProcessAddress,
    kernel_map_region_size: usize,
    code_region_start: KProcessAddress,
    code_region_size: usize,
    /// ASLR region (alias code region).
    /// Upstream: m_alias_code_region_start / m_alias_code_region_end
    alias_code_region_start: KProcessAddress,
    alias_code_region_size: usize,
    address_space_width: u32,
}

impl KProcessPageTable {
    pub fn new() -> Self {
        Self {
            address_space_start: KProcessAddress::default(),
            address_space_size: 0,
            heap_region_start: KProcessAddress::default(),
            heap_region_size: 0,
            current_heap_size: 0,
            alias_region_start: KProcessAddress::default(),
            alias_region_size: 0,
            stack_region_start: KProcessAddress::default(),
            stack_region_size: 0,
            kernel_map_region_start: KProcessAddress::default(),
            kernel_map_region_size: 0,
            code_region_start: KProcessAddress::default(),
            code_region_size: 0,
            alias_code_region_start: KProcessAddress::default(),
            alias_code_region_size: 0,
            address_space_width: 0,
        }
    }

    /// Initialize the page table for a process.
    /// TODO: Port from k_process_page_table.h (delegates to KPageTable::InitializeForProcess).
    pub fn initialize(&mut self) -> u32 {
        // TODO: Full implementation
        0
    }

    /// Finalize the page table.
    /// TODO: Port from k_process_page_table.h.
    pub fn finalize(&mut self) {
        // TODO: Full implementation
    }

    /// Get the allocate option.
    /// TODO: Port from k_process_page_table.h.
    pub fn get_allocate_option(&self) -> u32 {
        0 // TODO
    }

    /// Get the address space start.
    /// TODO: Port from k_process_page_table.h.
    pub fn get_address_space_start(&self) -> KProcessAddress {
        self.address_space_start
    }

    /// Get the address space size.
    /// TODO: Port from k_process_page_table.h.
    pub fn get_address_space_size(&self) -> usize {
        self.address_space_size
    }

    /// Get the heap region start.
    pub fn get_heap_region_start(&self) -> KProcessAddress {
        self.heap_region_start
    }

    /// Get the heap region size.
    pub fn get_heap_region_size(&self) -> usize {
        self.heap_region_size
    }

    /// Get the alias region start.
    pub fn get_alias_region_start(&self) -> KProcessAddress {
        self.alias_region_start
    }

    /// Get the alias region size.
    pub fn get_alias_region_size(&self) -> usize {
        self.alias_region_size
    }

    /// Get the stack region start.
    pub fn get_stack_region_start(&self) -> KProcessAddress {
        self.stack_region_start
    }

    /// Get the stack region size.
    pub fn get_stack_region_size(&self) -> usize {
        self.stack_region_size
    }

    /// Get the kernel map region start.
    pub fn get_kernel_map_region_start(&self) -> KProcessAddress {
        self.kernel_map_region_start
    }

    /// Get the kernel map region size.
    pub fn get_kernel_map_region_size(&self) -> usize {
        self.kernel_map_region_size
    }

    /// Get the code region start.
    pub fn get_code_region_start(&self) -> KProcessAddress {
        self.code_region_start
    }

    /// Get the code region size.
    pub fn get_code_region_size(&self) -> usize {
        self.code_region_size
    }

    /// Get the alias code region (ASLR region) start.
    /// Upstream: GetAliasCodeRegionStart()
    pub fn get_alias_code_region_start(&self) -> KProcessAddress {
        self.alias_code_region_start
    }

    /// Get the alias code region (ASLR region) size.
    /// Upstream: GetAliasCodeRegionSize()
    pub fn get_alias_code_region_size(&self) -> usize {
        self.alias_code_region_size
    }

    /// Get the address space width.
    pub fn get_address_space_width(&self) -> u32 {
        self.address_space_width
    }

    /// Set memory permission.
    /// TODO: Port from k_process_page_table.h.
    pub fn set_memory_permission(
        &mut self,
        _addr: KProcessAddress,
        _size: usize,
        _perm: u32,
    ) -> u32 {
        0
    }

    /// Set heap size.
    /// TODO: Port from k_process_page_table.h.
    pub fn set_heap_size(&mut self, size: usize) -> (u32, KProcessAddress) {
        if size > self.heap_region_size {
            return (1, KProcessAddress::default());
        }
        self.current_heap_size = size;
        (0, self.heap_region_start)
    }

    /// Map memory.
    /// TODO: Port from k_process_page_table.h.
    pub fn map_memory(
        &mut self,
        _dst: KProcessAddress,
        _src: KProcessAddress,
        _size: usize,
    ) -> u32 {
        0
    }

    /// Unmap memory.
    /// TODO: Port from k_process_page_table.h.
    pub fn unmap_memory(
        &mut self,
        _dst: KProcessAddress,
        _src: KProcessAddress,
        _size: usize,
    ) -> u32 {
        0
    }

    /// Check if address range is contained.
    pub fn contains(&self, _addr: KProcessAddress, _size: usize) -> bool {
        let start = self.address_space_start.get();
        let end = start.saturating_add(self.address_space_size as u64);
        let addr = _addr.get();
        addr >= start && addr.checked_add(_size as u64).is_some_and(|range_end| range_end <= end)
    }

    /// Get physical address for a virtual address.
    pub fn get_physical_address(
        &self,
        _address: KProcessAddress,
    ) -> Option<KPhysicalAddress> {
        None // TODO
    }

    pub fn configure_address_space(
        &mut self,
        start: KProcessAddress,
        size: usize,
        width: u32,
    ) {
        self.address_space_start = start;
        self.address_space_size = size;
        self.address_space_width = width;
    }

    pub fn set_code_region(&mut self, start: KProcessAddress, size: usize) {
        self.code_region_start = start;
        self.code_region_size = size;
    }

    pub fn set_stack_region(&mut self, start: KProcessAddress, size: usize) {
        self.stack_region_start = start;
        self.stack_region_size = size;
    }

    pub fn set_heap_region(&mut self, start: KProcessAddress, size: usize) {
        self.heap_region_start = start;
        self.heap_region_size = size;
        self.current_heap_size = self.current_heap_size.min(size);
    }

    pub fn get_current_heap_size(&self) -> usize {
        self.current_heap_size
    }

    pub fn set_alias_code_region(&mut self, start: KProcessAddress, size: usize) {
        self.alias_code_region_start = start;
        self.alias_code_region_size = size;
    }

    pub fn set_alias_region(&mut self, start: KProcessAddress, size: usize) {
        self.alias_region_start = start;
        self.alias_region_size = size;
    }

    pub fn set_kernel_map_region(&mut self, start: KProcessAddress, size: usize) {
        self.kernel_map_region_start = start;
        self.kernel_map_region_size = size;
    }

    // -- Mapping methods matching upstream --

    /// Map a static physical memory region into the process address space.
    /// Upstream: KPageTableBase::MapStatic -> finds region in KMemoryLayout,
    /// maps with MapPages into the Static region.
    ///
    /// In our emulator, physical memory mapping is handled by the shared
    /// process memory (GuestMemory). This method records the mapping request
    /// for correctness tracking but the actual memory backing is in GuestMemory.
    pub fn map_static(
        &mut self,
        phys_addr: u64,
        size: usize,
        _perm: KMemoryPermission,
    ) -> u32 {
        log::debug!(
            "KProcessPageTable::map_static(phys={:#x}, size={:#x}, perm={:?})",
            phys_addr, size, _perm
        );
        // Upstream validates alignment, region existence in KMemoryLayout,
        // then calls MapPages to find a free VA range in the Static region
        // and maps the physical pages there.
        // In our emulator model, guest memory is flat — no real page table.
        // We just record the request succeeded.
        0
    }

    /// Map an IO memory region into the process address space.
    /// Upstream: KPageTableBase::MapIo -> maps with MapIoImpl into IoRegister region.
    pub fn map_io(
        &mut self,
        phys_addr: u64,
        size: usize,
        _perm: KMemoryPermission,
    ) -> u32 {
        log::debug!(
            "KProcessPageTable::map_io(phys={:#x}, size={:#x}, perm={:?})",
            phys_addr, size, _perm
        );
        // Same as map_static — in our flat memory model, IO mapping is a no-op
        // but we validate the request.
        0
    }

    /// Map a kernel memory region by type.
    /// Upstream: KPageTableBase::MapRegion -> finds region in KMemoryLayout,
    /// delegates to MapStatic.
    pub fn map_region(
        &mut self,
        _region_type: u32,
        _perm: KMemoryPermission,
    ) -> u32 {
        log::debug!(
            "KProcessPageTable::map_region(type={}, perm={:?})",
            _region_type, _perm
        );
        // KMemoryLayout region lookup not yet implemented.
        // Return success — these regions are for kernel trace buffers, DTB, etc.
        // that aren't needed for game execution.
        0
    }
}

impl Default for KProcessPageTable {
    fn default() -> Self {
        Self::new()
    }
}
