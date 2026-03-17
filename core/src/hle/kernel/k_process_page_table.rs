//! Port of zuyu/src/core/hle/kernel/k_process_page_table.h
//! Status: EN COURS
//! Derniere synchro: 2026-03-17
//!
//! KProcessPageTable: thin wrapper around KPageTableBase matching upstream.
//! All methods delegate to the inner KPageTableBase.

use super::k_memory_block::{KMemoryInfo, KMemoryPermission, PAGE_SIZE};
use super::k_page_table_base::KPageTableBase;
use super::k_typed_address::{KPhysicalAddress, KProcessAddress};

/// The process page table.
/// Matches upstream `KProcessPageTable` (k_process_page_table.h).
/// Thin wrapper around KPageTableBase.
pub struct KProcessPageTable {
    base: KPageTableBase,
}

impl KProcessPageTable {
    pub fn new() -> Self {
        Self {
            base: KPageTableBase::new(),
        }
    }

    /// Initialize for a user process.
    /// Matches upstream delegation to m_page_table.InitializeForProcess.
    pub fn initialize_for_process(
        &mut self,
        as_flags: u32,
        enable_aslr: bool,
        enable_das_merge: bool,
        from_back: bool,
        pool: u32,
        code_address: usize,
        code_size: usize,
        aslr_space_start: usize,
    ) -> u32 {
        self.base.initialize_for_process(
            as_flags,
            enable_aslr,
            enable_das_merge,
            from_back,
            pool,
            code_address,
            code_size,
            aslr_space_start,
        )
    }

    pub fn finalize(&mut self) {
        // TODO: delegate to base.finalize()
    }

    // -- Region getters delegating to base --

    pub fn get_address_space_start(&self) -> KProcessAddress {
        KProcessAddress::new(self.base.get_address_space_start() as u64)
    }

    pub fn get_address_space_size(&self) -> usize {
        self.base.get_address_space_size()
    }

    pub fn get_heap_region_start(&self) -> KProcessAddress {
        KProcessAddress::new(self.base.get_heap_region_start() as u64)
    }

    pub fn get_heap_region_size(&self) -> usize {
        self.base.get_heap_region_size()
    }

    pub fn get_alias_region_start(&self) -> KProcessAddress {
        KProcessAddress::new(self.base.get_alias_region_start() as u64)
    }

    pub fn get_alias_region_size(&self) -> usize {
        self.base.get_alias_region_size()
    }

    pub fn get_stack_region_start(&self) -> KProcessAddress {
        KProcessAddress::new(self.base.get_stack_region_start() as u64)
    }

    pub fn get_stack_region_size(&self) -> usize {
        self.base.get_stack_region_size()
    }

    pub fn get_kernel_map_region_start(&self) -> KProcessAddress {
        KProcessAddress::new(self.base.get_kernel_map_region_start() as u64)
    }

    pub fn get_kernel_map_region_size(&self) -> usize {
        self.base.get_kernel_map_region_size()
    }

    pub fn get_code_region_start(&self) -> KProcessAddress {
        KProcessAddress::new(self.base.get_code_region_start() as u64)
    }

    pub fn get_code_region_size(&self) -> usize {
        self.base.get_code_region_size()
    }

    pub fn get_alias_code_region_start(&self) -> KProcessAddress {
        KProcessAddress::new(self.base.get_alias_code_region_start() as u64)
    }

    pub fn get_alias_code_region_size(&self) -> usize {
        self.base.get_alias_code_region_size()
    }

    pub fn get_address_space_width(&self) -> u32 {
        self.base.get_address_space_width()
    }

    pub fn get_allocate_option(&self) -> u32 {
        self.base.get_allocate_option()
    }

    pub fn get_current_heap_size(&self) -> usize {
        self.base.get_heap_region_size() // TODO: track current heap vs region
    }

    // -- Mapping operations delegating to base --

    pub fn set_heap_size(&mut self, size: usize) -> (u32, KProcessAddress) {
        let (result, addr) = self.base.set_heap_size(size);
        (result, KProcessAddress::new(addr as u64))
    }

    pub fn set_max_heap_size(&mut self, size: usize) -> u32 {
        self.base.set_max_heap_size(size)
    }

    pub fn set_memory_permission(
        &mut self,
        addr: KProcessAddress,
        size: usize,
        perm: u32,
    ) -> u32 {
        self.base.set_memory_permission(
            addr.get() as usize,
            size,
            KMemoryPermission::from_bits_truncate(perm as u8),
        )
    }

    pub fn set_memory_attribute(
        &mut self,
        addr: KProcessAddress,
        size: usize,
        mask: u32,
        attr: u32,
    ) -> u32 {
        self.base.set_memory_attribute(addr.get() as usize, size, mask, attr)
    }

    pub fn map_memory(&mut self, dst: KProcessAddress, src: KProcessAddress, size: usize) -> u32 {
        self.base.map_memory(dst.get() as usize, src.get() as usize, size)
    }

    pub fn unmap_memory(&mut self, dst: KProcessAddress, src: KProcessAddress, size: usize) -> u32 {
        self.base.unmap_memory(dst.get() as usize, src.get() as usize, size)
    }

    pub fn map_physical_memory(&mut self, addr: KProcessAddress, size: usize) -> u32 {
        self.base.map_physical_memory(addr.get() as usize, size)
    }

    pub fn unmap_physical_memory(&mut self, addr: KProcessAddress, size: usize) -> u32 {
        self.base.unmap_physical_memory(addr.get() as usize, size)
    }

    pub fn map_static(&mut self, phys_addr: u64, size: usize, perm: KMemoryPermission) -> u32 {
        self.base.map_static(phys_addr, size, perm)
    }

    pub fn map_io(&mut self, phys_addr: u64, size: usize, perm: KMemoryPermission) -> u32 {
        self.base.map_io(phys_addr, size, perm)
    }

    pub fn map_region(&mut self, region_type: u32, perm: KMemoryPermission) -> u32 {
        self.base.map_region(region_type, perm)
    }

    pub fn set_process_memory_permission(
        &mut self,
        addr: KProcessAddress,
        size: usize,
        perm: KMemoryPermission,
    ) -> u32 {
        self.base.set_process_memory_permission(addr.get() as usize, size, perm)
    }

    // -- Query --

    pub fn query_info(&self, addr: usize) -> Option<KMemoryInfo> {
        self.base.query_info(addr)
    }

    pub fn contains(&self, addr: KProcessAddress, size: usize) -> bool {
        self.base.contains_range(addr.get() as usize, size)
    }

    pub fn get_physical_address(&self, _address: KProcessAddress) -> Option<KPhysicalAddress> {
        None // TODO
    }

    // -- Page mapping --

    pub fn map_pages_find_free(
        &mut self,
        num_pages: usize,
        alignment: usize,
        phys_addr: u64,
        is_pa_valid: bool,
        region_start: KProcessAddress,
        region_num_pages: usize,
        state: super::k_memory_block::KMemoryState,
        perm: KMemoryPermission,
    ) -> (u32, KProcessAddress) {
        let (result, addr) = self.base.map_pages_find_free(
            num_pages, alignment, phys_addr, is_pa_valid,
            region_start.get() as usize, region_num_pages, state, perm,
        );
        (result, KProcessAddress::new(addr as u64))
    }

    pub fn map_pages_at_address(
        &mut self,
        addr: KProcessAddress,
        num_pages: usize,
        state: super::k_memory_block::KMemoryState,
        perm: KMemoryPermission,
    ) -> u32 {
        self.base.map_pages_at_address(addr.get() as usize, num_pages, state, perm)
    }

    pub fn unmap_pages(
        &mut self,
        addr: KProcessAddress,
        num_pages: usize,
        state: super::k_memory_block::KMemoryState,
    ) -> u32 {
        self.base.unmap_pages(addr.get() as usize, num_pages, state)
    }

    pub fn lock_for_transfer_memory(
        &mut self,
        addr: KProcessAddress,
        size: usize,
        perm: KMemoryPermission,
    ) -> u32 {
        self.base.lock_for_transfer_memory(addr.get() as usize, size, perm)
    }

    pub fn unlock_for_transfer_memory(
        &mut self,
        addr: KProcessAddress,
        size: usize,
    ) -> u32 {
        self.base.unlock_for_transfer_memory(addr.get() as usize, size)
    }

    // -- IPC memory locking --

    pub fn lock_for_ipc_user_buffer(
        &mut self,
        out_paddr: &mut u64,
        addr: KProcessAddress,
        size: usize,
    ) -> u32 {
        self.base.lock_for_ipc_user_buffer(out_paddr, addr.get() as usize, size)
    }

    pub fn unlock_for_ipc_user_buffer(&mut self, addr: KProcessAddress, size: usize) -> u32 {
        self.base.unlock_for_ipc_user_buffer(addr.get() as usize, size)
    }

    // -- Direct base access --

    pub fn get_base(&self) -> &KPageTableBase {
        &self.base
    }

    pub fn get_base_mut(&mut self) -> &mut KPageTableBase {
        &mut self.base
    }

    // -- Compatibility setters (used during process load before InitializeForProcess) --
    // These directly modify the base fields. Will be removed once
    // KProcess::load_from_metadata calls initialize_for_process instead.

    pub fn configure_address_space(
        &mut self,
        start: KProcessAddress,
        size: usize,
        width: u32,
    ) {
        let base = self.get_base_mut();
        base.m_address_space_start = start.get() as usize;
        base.m_address_space_end = start.get() as usize + size;
        base.m_address_space_width = width;
        // Initialize the block manager for this address space.
        let _ = base.m_memory_block_manager.initialize(
            base.m_address_space_start,
            base.m_address_space_end,
        );
    }

    pub fn set_code_region(&mut self, start: KProcessAddress, size: usize) {
        let base = self.get_base_mut();
        base.m_code_region_start = start.get() as usize;
        base.m_code_region_end = start.get() as usize + size;
        // For 32-bit, alias_code_region covers the full address space.
        if base.m_address_space_width <= 32 {
            base.m_alias_code_region_start = base.m_code_region_start;
            base.m_alias_code_region_end = base.m_address_space_end;
            base.m_stack_region_start = base.m_code_region_start;
            base.m_stack_region_end = base.m_code_region_end;
            base.m_kernel_map_region_start = base.m_code_region_start;
            base.m_kernel_map_region_end = base.m_code_region_end;
        }
    }

    pub fn set_stack_region(&mut self, start: KProcessAddress, size: usize) {
        // Direct set for legacy compatibility.
        let base = self.get_base_mut();
        base.m_stack_region_start = start.get() as usize;
        base.m_stack_region_end = start.get() as usize + size;
    }

    pub fn set_heap_region(&mut self, start: KProcessAddress, size: usize) {
        let base = self.get_base_mut();
        base.m_heap_region_start = start.get() as usize;
        base.m_heap_region_end = start.get() as usize + size;
        base.m_max_heap_size = size;
        if base.m_current_heap_end < base.m_heap_region_start {
            base.m_current_heap_end = base.m_heap_region_start;
        }
        if base.m_current_heap_end > base.m_heap_region_end {
            base.m_current_heap_end = base.m_heap_region_end;
        }
    }
}

impl Default for KProcessPageTable {
    fn default() -> Self {
        Self::new()
    }
}
