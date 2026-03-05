//! Port of zuyu/src/core/hle/kernel/k_page_table_base.h and k_page_table_base.cpp
//! Status: EN COURS
//! Derniere synchro: 2026-03-05
//!
//! Kernel page table that manages process virtual memory mappings,
//! permissions, and memory states. This is the core of the HLE kernel's
//! memory management, implementing operations like MapCodeMemory, SetHeapSize,
//! MapSharedMemory, QueryMemory, etc.
//!
//! In the C++ code, this is split between KPageTableBase (the logic) and
//! KProcessPageTable (a thin wrapper). We combine them here since the
//! Rust HLE emulator doesn't need the full complexity of the kernel's
//! page table manager hierarchy.

use std::collections::BTreeMap;

use crate::address_space_info::{self, AddressSpaceType};
use crate::memory_block::{
    KMemoryAttribute, KMemoryBlock, KMemoryInfo, KMemoryPermission, KMemoryState,
    PAGE_SIZE, PAGE_SIZE_U64,
};

/// Errors from kernel page table operations.
#[derive(Debug, thiserror::Error)]
pub enum PageTableError {
    #[error("invalid address: 0x{0:016X}")]
    InvalidAddress(u64),
    #[error("invalid size: 0x{0:X}")]
    InvalidSize(u64),
    #[error("out of memory")]
    OutOfMemory,
    #[error("invalid state at 0x{addr:016X}: expected {expected:?}, got {actual:?}")]
    InvalidState {
        addr: u64,
        expected: KMemoryState,
        actual: KMemoryState,
    },
    #[error("invalid memory state combination")]
    InvalidCombination,
    #[error("region already mapped at 0x{0:016X}")]
    AlreadyMapped(u64),
    #[error("region not mapped at 0x{0:016X}")]
    NotMapped(u64),
    #[error("insufficient address space")]
    InsufficientAddressSpace,
}

pub type PageTableResult<T> = Result<T, PageTableError>;

/// Memory fill values used when allocating certain region types.
#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum MemoryFillValue {
    Zero = 0,
    Stack = b'X',
    Ipc = b'Y',
    Heap = b'Z',
}

/// Kernel page table: manages the virtual memory layout for a process.
///
/// Uses a sorted list of memory blocks (BTreeMap keyed by address) to track
/// the state of every byte in the process address space. This matches the
/// kernel's KMemoryBlockManager approach.
pub struct KPageTable {
    /// Address space configuration
    address_space_start: u64,
    address_space_end: u64,
    address_space_width: usize,

    /// Region bases and sizes (filled in during initialization)
    code_region_start: u64,
    code_region_end: u64,
    alias_region_start: u64,
    alias_region_end: u64,
    heap_region_start: u64,
    heap_region_end: u64,
    stack_region_start: u64,
    stack_region_end: u64,
    kernel_map_region_start: u64,
    kernel_map_region_end: u64,

    /// Current heap end (grows with SetHeapSize)
    current_heap_end: u64,

    /// Maximum heap size allowed.
    max_heap_size: u64,

    /// Memory block list: maps base_address -> KMemoryBlock.
    /// The entire address space is covered; unmapped regions have state FREE.
    blocks: BTreeMap<u64, KMemoryBlock>,

    /// Whether the page table has been initialized.
    is_initialized: bool,

    /// Is this a kernel or user address space.
    is_kernel: bool,

    /// Allocate option (memory pool).
    allocate_option: u32,
}

impl KPageTable {
    /// Create a new uninitialized page table.
    pub fn new() -> Self {
        Self {
            address_space_start: 0,
            address_space_end: 0,
            address_space_width: 0,
            code_region_start: 0,
            code_region_end: 0,
            alias_region_start: 0,
            alias_region_end: 0,
            heap_region_start: 0,
            heap_region_end: 0,
            stack_region_start: 0,
            stack_region_end: 0,
            kernel_map_region_start: 0,
            kernel_map_region_end: 0,
            current_heap_end: 0,
            max_heap_size: 0,
            blocks: BTreeMap::new(),
            is_initialized: false,
            is_kernel: false,
            allocate_option: 0,
        }
    }

    /// Initialize the page table for a user process.
    ///
    /// Sets up the address space regions according to the given bit width
    /// (32, 36, or 39) and code region parameters.
    ///
    /// This corresponds to KPageTableBase::InitializeForProcess.
    pub fn initialize_for_process(
        &mut self,
        as_type: AddressSpaceWidth,
        enable_aslr: bool,
        enable_das_merge: bool,
        from_back: bool,
        code_addr: u64,
        code_size: u64,
        aslr_space_start: u64,
    ) -> PageTableResult<()> {
        let width = match as_type {
            AddressSpaceWidth::Bit32 => 32usize,
            AddressSpaceWidth::Bit36 => 36,
            AddressSpaceWidth::Bit32NoMap => 32,
            AddressSpaceWidth::Bit39 => 39,
        };

        self.address_space_width = width;

        match width {
            32 => {
                let map_small_start = address_space_info::get_address_space_start(32, AddressSpaceType::MapSmall);
                let map_small_size = address_space_info::get_address_space_size(32, AddressSpaceType::MapSmall);
                let map_large_start = address_space_info::get_address_space_start(32, AddressSpaceType::MapLarge);
                let map_large_size = address_space_info::get_address_space_size(32, AddressSpaceType::MapLarge);
                let alias_size = address_space_info::get_address_space_size(32, AddressSpaceType::Alias);
                let heap_size = address_space_info::get_address_space_size(32, AddressSpaceType::Heap);

                self.address_space_start = 0;
                self.address_space_end = 1u64 << 32;

                self.code_region_start = map_small_start;
                self.code_region_end = map_small_start + map_small_size;
                self.alias_region_start = map_large_start;
                self.alias_region_end = map_large_start + alias_size;
                self.heap_region_start = map_large_start;
                self.heap_region_end = map_large_start + heap_size;
                self.stack_region_start = map_large_start;
                self.stack_region_end = map_large_start + map_large_size;
                self.kernel_map_region_start = map_large_start;
                self.kernel_map_region_end = map_large_start + map_large_size;
            }
            36 => {
                let map_small_start = address_space_info::get_address_space_start(36, AddressSpaceType::MapSmall);
                let map_small_size = address_space_info::get_address_space_size(36, AddressSpaceType::MapSmall);
                let map_large_start = address_space_info::get_address_space_start(36, AddressSpaceType::MapLarge);
                let map_large_size = address_space_info::get_address_space_size(36, AddressSpaceType::MapLarge);
                let alias_size = address_space_info::get_address_space_size(36, AddressSpaceType::Alias);
                let heap_size = address_space_info::get_address_space_size(36, AddressSpaceType::Heap);

                self.address_space_start = 0;
                self.address_space_end = 1u64 << 36;

                self.code_region_start = map_small_start;
                self.code_region_end = map_small_start + map_small_size;
                self.alias_region_start = map_large_start;
                self.alias_region_end = map_large_start + alias_size;
                self.heap_region_start = map_large_start;
                self.heap_region_end = map_large_start + heap_size;
                self.stack_region_start = map_large_start;
                self.stack_region_end = map_large_start + map_large_size;
                self.kernel_map_region_start = map_large_start;
                self.kernel_map_region_end = map_large_start + map_large_size;
            }
            39 => {
                let map39_start = address_space_info::get_address_space_start(39, AddressSpaceType::Map39Bit);
                let map39_size = address_space_info::get_address_space_size(39, AddressSpaceType::Map39Bit);
                let small_size = address_space_info::get_address_space_size(39, AddressSpaceType::MapSmall);
                let heap_size = address_space_info::get_address_space_size(39, AddressSpaceType::Heap);
                let alias_size = address_space_info::get_address_space_size(39, AddressSpaceType::Alias);
                let stack_size = address_space_info::get_address_space_size(39, AddressSpaceType::Stack);

                self.address_space_start = 0;
                self.address_space_end = 1u64 << 39;

                // For 39-bit, we use the ASLR region as the code region base.
                // The code region base is typically aslr_space_start + code_addr
                // For simplicity in HLE, we just use the map39 start.
                self.code_region_start = map39_start;
                self.code_region_end = map39_start + small_size;

                // Place alias, heap, stack, kernel map after the code region
                let alias_start = self.code_region_end;
                self.alias_region_start = alias_start;
                self.alias_region_end = alias_start + alias_size;

                let heap_start = self.alias_region_end;
                self.heap_region_start = heap_start;
                self.heap_region_end = heap_start + heap_size;

                let stack_start = self.heap_region_end;
                self.stack_region_start = stack_start;
                self.stack_region_end = stack_start + stack_size;

                let kmap_start = self.stack_region_end;
                self.kernel_map_region_start = kmap_start;
                self.kernel_map_region_end = kmap_start + small_size;
            }
            _ => {
                return Err(PageTableError::InvalidAddress(width as u64));
            }
        }

        self.current_heap_end = self.heap_region_start;
        self.max_heap_size = self.heap_region_end - self.heap_region_start;

        // Initialize the block list with a single FREE block covering the whole address space.
        self.blocks.clear();
        self.blocks.insert(
            self.address_space_start,
            KMemoryBlock::new(
                self.address_space_start,
                ((self.address_space_end - self.address_space_start) / PAGE_SIZE_U64) as usize,
                KMemoryState::FREE,
                KMemoryPermission::NONE,
                KMemoryAttribute::NONE,
            ),
        );

        self.is_initialized = true;
        self.is_kernel = false;

        log::debug!(
            "KPageTable initialized: width={}, AS=[{:#X}..{:#X}), code=[{:#X}..{:#X}), heap=[{:#X}..{:#X}), stack=[{:#X}..{:#X})",
            width,
            self.address_space_start, self.address_space_end,
            self.code_region_start, self.code_region_end,
            self.heap_region_start, self.heap_region_end,
            self.stack_region_start, self.stack_region_end,
        );

        Ok(())
    }

    // -- Region accessors --

    pub fn address_space_start(&self) -> u64 { self.address_space_start }
    pub fn address_space_end(&self) -> u64 { self.address_space_end }
    pub fn address_space_width(&self) -> usize { self.address_space_width }

    pub fn code_region_start(&self) -> u64 { self.code_region_start }
    pub fn code_region_end(&self) -> u64 { self.code_region_end }

    pub fn alias_region_start(&self) -> u64 { self.alias_region_start }
    pub fn alias_region_end(&self) -> u64 { self.alias_region_end }

    pub fn heap_region_start(&self) -> u64 { self.heap_region_start }
    pub fn heap_region_end(&self) -> u64 { self.heap_region_end }
    pub fn current_heap_end(&self) -> u64 { self.current_heap_end }

    pub fn stack_region_start(&self) -> u64 { self.stack_region_start }
    pub fn stack_region_end(&self) -> u64 { self.stack_region_end }

    pub fn kernel_map_region_start(&self) -> u64 { self.kernel_map_region_start }
    pub fn kernel_map_region_end(&self) -> u64 { self.kernel_map_region_end }

    pub fn is_initialized(&self) -> bool { self.is_initialized }

    pub fn allocate_option(&self) -> u32 { self.allocate_option }
    pub fn set_allocate_option(&mut self, opt: u32) { self.allocate_option = opt; }

    // -- Memory block operations --

    /// Query memory info for a given address (SVC QueryMemory).
    ///
    /// Returns information about the memory block containing the given address,
    /// or the first block after it if the address is in a gap.
    pub fn query_memory(&self, addr: u64) -> KMemoryInfo {
        if let Some((&base, block)) = self.blocks.range(..=addr).next_back() {
            if addr < block.end_address() {
                return block.get_memory_info();
            }
        }

        // Address is in a free region or beyond all blocks.
        // Find the next block after this address.
        if let Some((&next_base, _)) = self.blocks.range((addr + 1)..).next() {
            let gap_start = self.find_gap_start(addr);
            return KMemoryInfo {
                address: gap_start,
                size: next_base - gap_start,
                state: KMemoryState::FREE,
                permission: KMemoryPermission::NONE,
                attribute: KMemoryAttribute::NONE,
                ipc_lock_count: 0,
                device_use_count: 0,
            };
        }

        // Beyond all blocks: return info to end of address space.
        let gap_start = self.find_gap_start(addr);
        KMemoryInfo {
            address: gap_start,
            size: self.address_space_end.saturating_sub(gap_start),
            state: KMemoryState::FREE,
            permission: KMemoryPermission::NONE,
            attribute: KMemoryAttribute::NONE,
            ipc_lock_count: 0,
            device_use_count: 0,
        }
    }

    /// Map code memory at the given address.
    ///
    /// This is called when loading a process's code segments.
    /// Corresponds to KPageTableBase::MapCodeMemory.
    pub fn map_code_memory(
        &mut self,
        dst_addr: u64,
        src_addr: u64,
        size: u64,
    ) -> PageTableResult<()> {
        self.validate_range(dst_addr, size)?;
        self.set_memory_state(
            dst_addr,
            size,
            KMemoryState::CODE,
            KMemoryPermission::USER_READ_EXECUTE,
            KMemoryAttribute::NONE,
        )
    }

    /// Set process memory permission (SVC SetProcessMemoryPermission).
    ///
    /// Changes the permission of a region that is in the Code or CodeData state.
    pub fn set_process_memory_permission(
        &mut self,
        addr: u64,
        size: u64,
        perm: KMemoryPermission,
    ) -> PageTableResult<()> {
        self.validate_range(addr, size)?;

        // Find the block and verify state.
        let block = self.find_block(addr)
            .ok_or(PageTableError::NotMapped(addr))?;

        let state = block.state;
        if state != KMemoryState::CODE && state != KMemoryState::CODE_DATA
            && state != KMemoryState::ALIAS_CODE && state != KMemoryState::ALIAS_CODE_DATA
        {
            return Err(PageTableError::InvalidState {
                addr,
                expected: KMemoryState::CODE,
                actual: state,
            });
        }

        // Determine the new state based on permission.
        let new_state = if perm.contains(KMemoryPermission::USER_EXECUTE) {
            if state == KMemoryState::ALIAS_CODE_DATA { KMemoryState::ALIAS_CODE } else { KMemoryState::CODE }
        } else {
            if state == KMemoryState::ALIAS_CODE { KMemoryState::ALIAS_CODE_DATA } else { KMemoryState::CODE_DATA }
        };

        self.set_memory_state(addr, size, new_state, perm, KMemoryAttribute::NONE)
    }

    /// Set the heap size (SVC SetHeapSize).
    ///
    /// Grows or shrinks the heap. Returns the heap base address.
    pub fn set_heap_size(&mut self, size: u64) -> PageTableResult<u64> {
        let new_end = self.heap_region_start + size;

        if size > self.max_heap_size {
            return Err(PageTableError::OutOfMemory);
        }

        if new_end > self.current_heap_end {
            // Grow: map new pages as Normal/ReadWrite.
            let grow_start = self.current_heap_end;
            let grow_size = new_end - grow_start;
            self.set_memory_state(
                grow_start,
                grow_size,
                KMemoryState::NORMAL,
                KMemoryPermission::USER_READ_WRITE,
                KMemoryAttribute::NONE,
            )?;
        } else if new_end < self.current_heap_end {
            // Shrink: unmap pages back to FREE.
            let shrink_start = new_end;
            let shrink_size = self.current_heap_end - new_end;
            self.set_memory_state(
                shrink_start,
                shrink_size,
                KMemoryState::FREE,
                KMemoryPermission::NONE,
                KMemoryAttribute::NONE,
            )?;
        }

        self.current_heap_end = new_end;
        Ok(self.heap_region_start)
    }

    /// Map a stack region.
    pub fn map_stack(
        &mut self,
        addr: u64,
        size: u64,
    ) -> PageTableResult<()> {
        self.validate_range(addr, size)?;
        self.set_memory_state(
            addr,
            size,
            KMemoryState::STACK,
            KMemoryPermission::USER_READ_WRITE,
            KMemoryAttribute::NONE,
        )
    }

    /// Map a thread local storage page.
    pub fn map_thread_local(
        &mut self,
        addr: u64,
        size: u64,
    ) -> PageTableResult<()> {
        self.validate_range(addr, size)?;
        self.set_memory_state(
            addr,
            size,
            KMemoryState::THREAD_LOCAL,
            KMemoryPermission::USER_READ_WRITE,
            KMemoryAttribute::NONE,
        )
    }

    /// Map shared memory.
    pub fn map_shared_memory(
        &mut self,
        addr: u64,
        size: u64,
        perm: KMemoryPermission,
    ) -> PageTableResult<()> {
        self.validate_range(addr, size)?;
        self.set_memory_state(
            addr,
            size,
            KMemoryState::SHARED,
            perm,
            KMemoryAttribute::NONE,
        )
    }

    /// Unmap shared memory.
    pub fn unmap_shared_memory(
        &mut self,
        addr: u64,
        size: u64,
    ) -> PageTableResult<()> {
        self.validate_range(addr, size)?;
        self.set_memory_state(
            addr,
            size,
            KMemoryState::FREE,
            KMemoryPermission::NONE,
            KMemoryAttribute::NONE,
        )
    }

    /// Map transfer memory.
    pub fn map_transfer_memory(
        &mut self,
        addr: u64,
        size: u64,
        perm: KMemoryPermission,
    ) -> PageTableResult<()> {
        self.validate_range(addr, size)?;
        self.set_memory_state(
            addr,
            size,
            KMemoryState::TRANSFERRED,
            perm,
            KMemoryAttribute::NONE,
        )
    }

    /// Find a free region of the given size within [start, end).
    ///
    /// Used for ASLR and dynamic allocation. Returns the base address
    /// of a free region, or an error if none is found.
    pub fn find_free_area(
        &self,
        region_start: u64,
        region_end: u64,
        size: u64,
        alignment: u64,
    ) -> PageTableResult<u64> {
        let mut addr = align_up(region_start, alignment);

        for (&block_addr, block) in self.blocks.range(region_start..) {
            if block_addr >= region_end {
                break;
            }

            if block.state == KMemoryState::FREE {
                let free_start = block.address.max(addr);
                let aligned_start = align_up(free_start, alignment);
                let free_end = block.end_address().min(region_end);

                if aligned_start + size <= free_end {
                    return Ok(aligned_start);
                }
            }

            addr = block.end_address();
        }

        Err(PageTableError::InsufficientAddressSpace)
    }

    // -- Internal helpers --

    fn validate_range(&self, addr: u64, size: u64) -> PageTableResult<()> {
        if size == 0 {
            return Err(PageTableError::InvalidSize(0));
        }
        if addr % PAGE_SIZE_U64 != 0 {
            return Err(PageTableError::InvalidAddress(addr));
        }
        if size % PAGE_SIZE_U64 != 0 {
            return Err(PageTableError::InvalidSize(size));
        }
        let end = addr.checked_add(size)
            .ok_or(PageTableError::InvalidAddress(addr))?;
        if end > self.address_space_end {
            return Err(PageTableError::InvalidAddress(addr));
        }
        Ok(())
    }

    /// Set the memory state for a region, splitting/merging blocks as needed.
    ///
    /// This is the core operation: it updates the block list so that
    /// [addr, addr+size) has the specified state, permission, and attribute.
    fn set_memory_state(
        &mut self,
        addr: u64,
        size: u64,
        new_state: KMemoryState,
        new_perm: KMemoryPermission,
        new_attr: KMemoryAttribute,
    ) -> PageTableResult<()> {
        let end = addr + size;

        // Collect all blocks that overlap [addr, end) and remove them.
        let mut affected: Vec<KMemoryBlock> = Vec::new();
        let mut to_remove: Vec<u64> = Vec::new();

        for (&block_addr, block) in self.blocks.range(..end) {
            if block.end_address() > addr {
                affected.push(block.clone());
                to_remove.push(block_addr);
            }
        }

        for key in &to_remove {
            self.blocks.remove(key);
        }

        // If no blocks were found, the region is implicitly free.
        if affected.is_empty() {
            self.blocks.insert(addr, KMemoryBlock::new(
                addr,
                (size / PAGE_SIZE_U64) as usize,
                new_state,
                new_perm,
                new_attr,
            ));
            return Ok(());
        }

        // Re-insert any portion of the first block that falls before addr.
        let first = &affected[0];
        if first.address < addr {
            let pre_pages = ((addr - first.address) / PAGE_SIZE_U64) as usize;
            self.blocks.insert(first.address, KMemoryBlock::new(
                first.address,
                pre_pages,
                first.state,
                first.permission,
                first.attribute,
            ));
        }

        // Insert the new block for [addr, end).
        self.blocks.insert(addr, KMemoryBlock::new(
            addr,
            (size / PAGE_SIZE_U64) as usize,
            new_state,
            new_perm,
            new_attr,
        ));

        // Re-insert any portion of the last block that falls after end.
        let last = &affected[affected.len() - 1];
        if last.end_address() > end {
            let post_pages = ((last.end_address() - end) / PAGE_SIZE_U64) as usize;
            self.blocks.insert(end, KMemoryBlock::new(
                end,
                post_pages,
                last.state,
                last.permission,
                last.attribute,
            ));
        }

        Ok(())
    }

    /// Find the block containing the given address.
    fn find_block(&self, addr: u64) -> Option<&KMemoryBlock> {
        if let Some((&_base, block)) = self.blocks.range(..=addr).next_back() {
            if addr < block.end_address() {
                return Some(block);
            }
        }
        None
    }

    /// Find the start of the gap (free region) containing addr.
    fn find_gap_start(&self, addr: u64) -> u64 {
        if let Some((&base, block)) = self.blocks.range(..=addr).next_back() {
            if addr < block.end_address() {
                // addr is inside a block, not in a gap
                return block.end_address();
            }
            return block.end_address();
        }
        self.address_space_start
    }
}

impl Default for KPageTable {
    fn default() -> Self {
        Self::new()
    }
}

/// Address space width (from CreateProcessFlag).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum AddressSpaceWidth {
    /// 32-bit address space
    Bit32 = 0,
    /// 36-bit address space
    Bit36 = 1,
    /// 32-bit address space without map region
    Bit32NoMap = 2,
    /// 39-bit address space
    Bit39 = 3,
}

impl AddressSpaceWidth {
    pub fn from_flags(flags: u32) -> Self {
        match flags & 0x7 {
            0 => Self::Bit32,
            1 => Self::Bit36,
            2 => Self::Bit32NoMap,
            3 => Self::Bit39,
            _ => Self::Bit39, // Default to 39-bit
        }
    }
}

/// Align up to the given alignment.
fn align_up(value: u64, alignment: u64) -> u64 {
    (value + alignment - 1) & !(alignment - 1)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_pt_39() -> KPageTable {
        let mut pt = KPageTable::new();
        pt.initialize_for_process(
            AddressSpaceWidth::Bit39,
            false,
            false,
            false,
            0x8000000,
            0x1000,
            0x8000000,
        ).unwrap();
        pt
    }

    #[test]
    fn test_initialize_39bit() {
        let pt = make_pt_39();
        assert!(pt.is_initialized());
        assert_eq!(pt.address_space_width(), 39);
        assert_eq!(pt.address_space_end(), 1u64 << 39);
        assert!(pt.heap_region_start() > 0);
        assert!(pt.stack_region_start() > 0);
    }

    #[test]
    fn test_query_memory_free() {
        let pt = make_pt_39();
        let info = pt.query_memory(0x1_0000_0000);
        assert_eq!(info.state, KMemoryState::FREE);
    }

    #[test]
    fn test_map_code_memory() {
        let mut pt = make_pt_39();
        let addr = pt.code_region_start();
        pt.map_code_memory(addr, addr, PAGE_SIZE_U64).unwrap();

        let info = pt.query_memory(addr);
        assert_eq!(info.state, KMemoryState::CODE);
        assert!(info.permission.contains(KMemoryPermission::USER_READ));
        assert!(info.permission.contains(KMemoryPermission::USER_EXECUTE));
    }

    #[test]
    fn test_set_heap_size() {
        let mut pt = make_pt_39();
        let heap_base = pt.set_heap_size(4 * PAGE_SIZE_U64).unwrap();
        assert_eq!(heap_base, pt.heap_region_start());

        let info = pt.query_memory(heap_base);
        assert_eq!(info.state, KMemoryState::NORMAL);
        assert_eq!(info.size, 4 * PAGE_SIZE_U64);

        // Shrink
        pt.set_heap_size(2 * PAGE_SIZE_U64).unwrap();
        let info = pt.query_memory(heap_base);
        assert_eq!(info.size, 2 * PAGE_SIZE_U64);

        // The freed area should be FREE again.
        let info2 = pt.query_memory(heap_base + 2 * PAGE_SIZE_U64);
        assert_eq!(info2.state, KMemoryState::FREE);
    }

    #[test]
    fn test_map_stack() {
        let mut pt = make_pt_39();
        let stack_addr = pt.stack_region_start();
        pt.map_stack(stack_addr, 4 * PAGE_SIZE_U64).unwrap();

        let info = pt.query_memory(stack_addr);
        assert_eq!(info.state, KMemoryState::STACK);
    }

    #[test]
    fn test_find_free_area() {
        let mut pt = make_pt_39();
        // Map something small at the start of heap
        let heap_start = pt.heap_region_start();
        pt.set_heap_size(PAGE_SIZE_U64).unwrap();

        // Find free area after the heap allocation
        let result = pt.find_free_area(
            heap_start,
            pt.heap_region_end(),
            PAGE_SIZE_U64,
            PAGE_SIZE_U64,
        );
        assert!(result.is_ok());
        let free_addr = result.unwrap();
        assert!(free_addr >= heap_start + PAGE_SIZE_U64);
    }

    #[test]
    fn test_set_process_memory_permission() {
        let mut pt = make_pt_39();
        let addr = pt.code_region_start();

        // Map as CODE first
        pt.map_code_memory(addr, addr, PAGE_SIZE_U64).unwrap();

        // Change to CodeData (read-write, no execute)
        pt.set_process_memory_permission(
            addr,
            PAGE_SIZE_U64,
            KMemoryPermission::USER_READ_WRITE,
        ).unwrap();

        let info = pt.query_memory(addr);
        assert_eq!(info.state, KMemoryState::CODE_DATA);
    }

    #[test]
    fn test_block_splitting() {
        let mut pt = make_pt_39();
        let base = pt.code_region_start();

        // Map a large region
        pt.set_memory_state(
            base,
            4 * PAGE_SIZE_U64,
            KMemoryState::CODE,
            KMemoryPermission::USER_READ_EXECUTE,
            KMemoryAttribute::NONE,
        ).unwrap();

        // Map a smaller region in the middle with different state
        pt.set_memory_state(
            base + PAGE_SIZE_U64,
            PAGE_SIZE_U64,
            KMemoryState::CODE_DATA,
            KMemoryPermission::USER_READ_WRITE,
            KMemoryAttribute::NONE,
        ).unwrap();

        // First page should still be CODE
        let info0 = pt.query_memory(base);
        assert_eq!(info0.state, KMemoryState::CODE);
        assert_eq!(info0.size, PAGE_SIZE_U64);

        // Second page should be CODE_DATA
        let info1 = pt.query_memory(base + PAGE_SIZE_U64);
        assert_eq!(info1.state, KMemoryState::CODE_DATA);
        assert_eq!(info1.size, PAGE_SIZE_U64);

        // Pages 3-4 should still be CODE
        let info2 = pt.query_memory(base + 2 * PAGE_SIZE_U64);
        assert_eq!(info2.state, KMemoryState::CODE);
        assert_eq!(info2.size, 2 * PAGE_SIZE_U64);
    }
}
