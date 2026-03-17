//! Port of zuyu/src/core/hle/kernel/k_page_table_base.h
//! Status: Stubbed (structure and enums only — this is the large VM management file)
//! Derniere synchro: 2026-03-11
//!
//! The base class for virtual memory page tables.
//! Contains all address space regions, memory block manager, and virtual memory operations.
//! Most methods are stubbed as todo!() since they depend on many other kernel subsystems.

use bitflags::bitflags;

use std::sync::{Arc, Mutex};

use super::k_address_space_info::{AddressSpaceInfoType, KAddressSpaceInfo};
use super::k_memory_block::*;
use super::k_memory_block_manager::KMemoryBlockManager;
use super::k_memory_layout::KERNEL_ASLR_ALIGNMENT;
use super::k_memory_manager;
use crate::hle::kernel::svc::svc_results;
use crate::hle::result::ResultCode;
use crate::memory::memory::Memory;

// ---------------------------------------------------------------------------
// DisableMergeAttribute
// ---------------------------------------------------------------------------

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct DisableMergeAttribute: u8 {
        const NONE                       = 0;
        const DISABLE_HEAD               = 1 << 0;
        const DISABLE_HEAD_AND_BODY      = 1 << 1;
        const ENABLE_HEAD_AND_BODY       = 1 << 2;
        const DISABLE_TAIL               = 1 << 3;
        const ENABLE_TAIL                = 1 << 4;
        const ENABLE_AND_MERGE_HEAD_BODY_TAIL = 1 << 5;

        const ENABLE_HEAD_BODY_TAIL  = Self::ENABLE_HEAD_AND_BODY.bits() | Self::ENABLE_TAIL.bits();
        const DISABLE_HEAD_BODY_TAIL = Self::DISABLE_HEAD_AND_BODY.bits() | Self::DISABLE_TAIL.bits();
    }
}

// ---------------------------------------------------------------------------
// KPageProperties
// ---------------------------------------------------------------------------

/// Port of Kernel::KPageProperties.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct KPageProperties {
    pub perm: KMemoryPermission,
    pub io: bool,
    pub uncached: bool,
    pub disable_merge_attributes: DisableMergeAttribute,
}

const _: () = assert!(std::mem::size_of::<KPageProperties>() == std::mem::size_of::<u32>());

// ---------------------------------------------------------------------------
// MemoryFillValue
// ---------------------------------------------------------------------------

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemoryFillValue {
    Zero = 0,
    Stack = b'X',
    Ipc = b'Y',
    Heap = b'Z',
}

// ---------------------------------------------------------------------------
// OperationType
// ---------------------------------------------------------------------------

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperationType {
    Map = 0,
    MapGroup = 1,
    MapFirstGroup = 2,
    Unmap = 3,
    ChangePermissions = 4,
    ChangePermissionsAndRefresh = 5,
    ChangePermissionsAndRefreshAndFlush = 6,
    Separate = 7,
    MapFirstGroupPhysical = 65000,
    UnmapPhysical = 65001,
}

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

pub const PAGE_BITS: usize = 12;
pub const MAX_PHYSICAL_MAP_ALIGNMENT: usize = 1 << 30; // 1 GiB
pub const REGION_ALIGNMENT: usize = 2 << 20; // 2 MiB
const _: () = assert!(REGION_ALIGNMENT == KERNEL_ASLR_ALIGNMENT);

// ---------------------------------------------------------------------------
// KPageTableBase
// ---------------------------------------------------------------------------

/// Port of Kernel::KPageTableBase.
///
/// Most methods are stubbed. This struct preserves all upstream fields.
pub struct KPageTableBase {
    /// The page table implementation used by dynarmic.
    /// Upstream: `std::unique_ptr<Common::PageTable> m_impl`
    pub(crate) m_impl: Option<Box<common::page_table::PageTable>>,
    /// Reference to the Memory bridge (MapMemoryRegion/UnmapRegion/ProtectRegion).
    /// Upstream: `Core::Memory::Memory* m_memory`
    pub(crate) m_memory: Option<Arc<Mutex<Memory>>>,
    pub(crate) m_address_space_start: usize,
    pub(crate) m_address_space_end: usize,
    pub(crate) m_heap_region_start: usize,
    pub(crate) m_heap_region_end: usize,
    pub(crate) m_current_heap_end: usize,
    pub(crate) m_alias_region_start: usize,
    pub(crate) m_alias_region_end: usize,
    pub(crate) m_stack_region_start: usize,
    pub(crate) m_stack_region_end: usize,
    pub(crate) m_kernel_map_region_start: usize,
    pub(crate) m_kernel_map_region_end: usize,
    pub(crate) m_alias_code_region_start: usize,
    pub(crate) m_alias_code_region_end: usize,
    pub(crate) m_code_region_start: usize,
    pub(crate) m_code_region_end: usize,
    pub(crate) m_max_heap_size: usize,
    pub(crate) m_mapped_physical_memory_size: usize,
    pub(crate) m_mapped_unsafe_physical_memory: usize,
    pub(crate) m_mapped_insecure_memory: usize,
    pub(crate) m_mapped_ipc_server_memory: usize,
    pub(crate) m_memory_block_manager: KMemoryBlockManager,
    pub(crate) m_allocate_option: u32,
    pub(crate) m_address_space_width: u32,
    pub(crate) m_is_kernel: bool,
    pub(crate) m_enable_aslr: bool,
    pub(crate) m_enable_device_address_space_merge: bool,
    pub(crate) m_heap_fill_value: MemoryFillValue,
    pub(crate) m_ipc_fill_value: MemoryFillValue,
    pub(crate) m_stack_fill_value: MemoryFillValue,
}

impl KPageTableBase {
    pub fn new() -> Self {
        Self {
            m_impl: None,
            m_memory: None,
            m_address_space_start: 0,
            m_address_space_end: 0,
            m_heap_region_start: 0,
            m_heap_region_end: 0,
            m_current_heap_end: 0,
            m_alias_region_start: 0,
            m_alias_region_end: 0,
            m_stack_region_start: 0,
            m_stack_region_end: 0,
            m_kernel_map_region_start: 0,
            m_kernel_map_region_end: 0,
            m_alias_code_region_start: 0,
            m_alias_code_region_end: 0,
            m_code_region_start: 0,
            m_code_region_end: 0,
            m_max_heap_size: 0,
            m_mapped_physical_memory_size: 0,
            m_mapped_unsafe_physical_memory: 0,
            m_mapped_insecure_memory: 0,
            m_mapped_ipc_server_memory: 0,
            m_memory_block_manager: KMemoryBlockManager::new(),
            m_allocate_option: 0,
            m_address_space_width: 0,
            m_is_kernel: false,
            m_enable_aslr: false,
            m_enable_device_address_space_merge: false,
            m_heap_fill_value: MemoryFillValue::Zero,
            m_ipc_fill_value: MemoryFillValue::Zero,
            m_stack_fill_value: MemoryFillValue::Zero,
        }
    }

    // --- Accessors matching upstream ---

    pub fn is_kernel(&self) -> bool {
        self.m_is_kernel
    }
    pub fn is_aslr_enabled(&self) -> bool {
        self.m_enable_aslr
    }

    pub fn contains(&self, addr: usize) -> bool {
        self.m_address_space_start <= addr && addr <= self.m_address_space_end.wrapping_sub(1)
    }

    pub fn contains_range(&self, addr: usize, size: usize) -> bool {
        self.m_address_space_start <= addr
            && addr < addr.wrapping_add(size)
            && addr.wrapping_add(size).wrapping_sub(1) <= self.m_address_space_end.wrapping_sub(1)
    }

    pub fn get_address_space_start(&self) -> usize {
        self.m_address_space_start
    }
    pub fn get_address_space_size(&self) -> usize {
        self.m_address_space_end - self.m_address_space_start
    }
    pub fn get_heap_region_start(&self) -> usize {
        self.m_heap_region_start
    }
    pub fn get_heap_region_size(&self) -> usize {
        self.m_heap_region_end - self.m_heap_region_start
    }
    pub fn get_alias_region_start(&self) -> usize {
        self.m_alias_region_start
    }
    pub fn get_alias_region_size(&self) -> usize {
        self.m_alias_region_end - self.m_alias_region_start
    }
    pub fn get_stack_region_start(&self) -> usize {
        self.m_stack_region_start
    }
    pub fn get_stack_region_size(&self) -> usize {
        self.m_stack_region_end - self.m_stack_region_start
    }
    pub fn get_kernel_map_region_start(&self) -> usize {
        self.m_kernel_map_region_start
    }
    pub fn get_kernel_map_region_size(&self) -> usize {
        self.m_kernel_map_region_end - self.m_kernel_map_region_start
    }
    pub fn get_code_region_start(&self) -> usize {
        self.m_code_region_start
    }
    pub fn get_code_region_size(&self) -> usize {
        self.m_code_region_end - self.m_code_region_start
    }
    pub fn get_alias_code_region_start(&self) -> usize {
        self.m_alias_code_region_start
    }
    pub fn get_alias_code_region_size(&self) -> usize {
        self.m_alias_code_region_end - self.m_alias_code_region_start
    }

    pub fn get_allocate_option(&self) -> u32 {
        self.m_allocate_option
    }
    pub fn get_address_space_width(&self) -> u32 {
        self.m_address_space_width
    }

    pub fn get_num_guard_pages(&self) -> usize {
        if self.m_is_kernel { 1 } else { 4 }
    }

    /// Port of GetAddressSpaceWidth(Svc::CreateProcessFlag).
    pub fn get_address_space_width_from_flags(flags: u32) -> usize {
        // Mask bits [0:3] for address space type.
        match flags & 0xF {
            // AddressSpace64Bit
            1 => 39,
            // AddressSpace64BitDeprecated
            2 => 36,
            // AddressSpace32Bit, AddressSpace32BitWithoutAlias
            0 | 3 => 32,
            _ => panic!("Invalid address space flag"),
        }
    }

    pub fn get_memory_block_manager(&self) -> &KMemoryBlockManager {
        &self.m_memory_block_manager
    }

    pub fn get_memory_block_manager_mut(&mut self) -> &mut KMemoryBlockManager {
        &mut self.m_memory_block_manager
    }

    // -- Region resolution matching upstream GetRegionAddress/GetRegionSize --

    /// Get the start address for a memory region by state.
    /// Matches upstream `KPageTableBase::GetRegionAddress(Svc::MemoryState)`.
    pub fn get_region_address(&self, state: KMemoryState) -> usize {
        let svc_state = state.bits() & KMemoryState::MASK.bits();
        match svc_state {
            s if s == KMemoryState::FREE.bits()
                || s == KMemoryState::KERNEL.bits()
                || s == KMemoryState::NORMAL.bits()
                || s == KMemoryState::CODE_DATA.bits()
                || s == KMemoryState::SHARED.bits()
                || s == KMemoryState::ALIAS_CODE.bits()
                || s == KMemoryState::ALIAS_CODE_DATA.bits()
                || s == KMemoryState::TRANSFERRED.bits()
                || s == KMemoryState::SHARED_TRANSFERRED.bits()
                || s == KMemoryState::SHARED_CODE.bits()
                || s == KMemoryState::GENERATED_CODE.bits()
                || s == KMemoryState::CODE_OUT.bits()
                || s == KMemoryState::COVERAGE.bits()
                || s == KMemoryState::INSECURE.bits() =>
            {
                self.m_alias_code_region_start
            }
            s if s == KMemoryState::CODE.bits() => self.m_code_region_start,
            s if s == KMemoryState::STACK.bits() => self.m_stack_region_start,
            s if s == KMemoryState::IO_MEMORY.bits()
                || s == KMemoryState::STATIC.bits()
                || s == KMemoryState::THREAD_LOCAL.bits() =>
            {
                self.m_kernel_map_region_start
            }
            s if s == KMemoryState::IO_REGISTER.bits()
                =>
            {
                self.m_alias_region_start
            }
            _ => {
                log::error!("GetRegionAddress: unknown state {:#x}", svc_state);
                self.m_address_space_start
            }
        }
    }

    /// Get the size of a memory region by state.
    /// Matches upstream `KPageTableBase::GetRegionSize(Svc::MemoryState)`.
    pub fn get_region_size(&self, state: KMemoryState) -> usize {
        let start = self.get_region_address(state);
        let svc_state = state.bits() & KMemoryState::MASK.bits();
        let end = match svc_state {
            s if s == KMemoryState::FREE.bits()
                || s == KMemoryState::NORMAL.bits()
                || s == KMemoryState::CODE_DATA.bits()
                || s == KMemoryState::SHARED.bits()
                || s == KMemoryState::ALIAS_CODE.bits()
                || s == KMemoryState::ALIAS_CODE_DATA.bits()
                || s == KMemoryState::TRANSFERRED.bits()
                || s == KMemoryState::SHARED_TRANSFERRED.bits()
                || s == KMemoryState::SHARED_CODE.bits()
                || s == KMemoryState::GENERATED_CODE.bits()
                || s == KMemoryState::CODE_OUT.bits()
                || s == KMemoryState::COVERAGE.bits()
                || s == KMemoryState::INSECURE.bits()
                || s == KMemoryState::KERNEL.bits() =>
            {
                self.m_alias_code_region_end
            }
            s if s == KMemoryState::CODE.bits() => self.m_code_region_end,
            s if s == KMemoryState::STACK.bits() => self.m_stack_region_end,
            s if s == KMemoryState::IO_MEMORY.bits()
                || s == KMemoryState::STATIC.bits()
                || s == KMemoryState::THREAD_LOCAL.bits() =>
            {
                self.m_kernel_map_region_end
            }
            s if s == KMemoryState::IO_REGISTER.bits()
                =>
            {
                self.m_alias_region_end
            }
            _ => self.m_address_space_end,
        };
        end - start
    }

    /// Check if an address range can be contained within a region for a given state.
    pub fn can_contain(&self, addr: usize, size: usize, state: KMemoryState) -> bool {
        let region_start = self.get_region_address(state);
        let region_size = self.get_region_size(state);
        let region_end = region_start + region_size;
        region_start <= addr && addr + size <= region_end
    }

    pub fn is_in_alias_region(&self, addr: usize, size: usize) -> bool {
        self.m_alias_region_start <= addr && addr + size <= self.m_alias_region_end
    }

    pub fn is_in_heap_region(&self, addr: usize, size: usize) -> bool {
        self.m_heap_region_start <= addr && addr + size <= self.m_heap_region_end
    }

    // -- CheckMemoryState --

    /// Default ignore attribute for CheckMemoryState.
    /// Matches upstream `DefaultMemoryIgnoreAttr`.
    pub const DEFAULT_MEMORY_IGNORE_ATTR: KMemoryAttribute =
        KMemoryAttribute::from_bits_truncate(
            KMemoryAttribute::IPC_LOCKED.bits() | KMemoryAttribute::DEVICE_SHARED.bits(),
        );

    /// Check that a single block's state/perm/attr match expectations.
    /// Matches upstream `CheckMemoryState(const KMemoryInfo&, ...)`.
    pub fn check_memory_state_info(
        info: &KMemoryInfo,
        state_mask: KMemoryState,
        state: KMemoryState,
        perm_mask: KMemoryPermission,
        perm: KMemoryPermission,
        attr_mask: KMemoryAttribute,
        attr: KMemoryAttribute,
    ) -> u32 {
        if (info.m_state & state_mask) != state {
            return svc_results::RESULT_INVALID_CURRENT_MEMORY.get_inner_value();
        }
        if (info.m_permission & perm_mask) != perm {
            return svc_results::RESULT_INVALID_CURRENT_MEMORY.get_inner_value();
        }
        if (info.m_attribute & attr_mask) != attr {
            return svc_results::RESULT_INVALID_CURRENT_MEMORY.get_inner_value();
        }
        0
    }

    /// Check memory state over a contiguous range (all blocks must match independently).
    /// Matches upstream `CheckMemoryStateContiguous`.
    pub fn check_memory_state_contiguous(
        &self,
        addr: usize,
        size: usize,
        state_mask: KMemoryState,
        state: KMemoryState,
        perm_mask: KMemoryPermission,
        perm: KMemoryPermission,
        attr_mask: KMemoryAttribute,
        attr: KMemoryAttribute,
    ) -> (u32, usize) {
        let last_addr = addr + size - 1;
        let mut blocks_needed: usize = 0;

        let mut iter = self.m_memory_block_manager.find_iterator(addr);
        let Some(first_block) = iter.next() else {
            return (svc_results::RESULT_INVALID_CURRENT_MEMORY.get_inner_value(), 0);
        };
        let mut info = first_block.get_memory_info();

        // If start address isn't aligned to block start, need a split.
        if (addr & !(PAGE_SIZE - 1)) != info.get_address() {
            blocks_needed += 1;
        }

        loop {
            let result = Self::check_memory_state_info(
                &info, state_mask, state, perm_mask, perm, attr_mask, attr,
            );
            if result != 0 {
                return (result, 0);
            }

            if last_addr <= info.get_last_address() {
                break;
            }

            let Some(next_block) = iter.next() else {
                return (svc_results::RESULT_INVALID_CURRENT_MEMORY.get_inner_value(), 0);
            };
            info = next_block.get_memory_info();
        }

        // If end address isn't aligned to block end, need a split.
        if ((addr + size + PAGE_SIZE - 1) & !(PAGE_SIZE - 1)) != info.get_end_address() {
            blocks_needed += 1;
        }

        (0, blocks_needed)
    }

    /// Check memory state over a range (all blocks must have the SAME state/perm/attr).
    /// Matches upstream `CheckMemoryState(out_state, out_perm, out_attr, ..., iterator, last_addr, ...)`.
    pub fn check_memory_state_range(
        &self,
        addr: usize,
        size: usize,
        state_mask: KMemoryState,
        state: KMemoryState,
        perm_mask: KMemoryPermission,
        perm: KMemoryPermission,
        attr_mask: KMemoryAttribute,
        attr: KMemoryAttribute,
        ignore_attr: KMemoryAttribute,
    ) -> (u32, Option<KMemoryState>, Option<KMemoryPermission>, Option<KMemoryAttribute>, usize) {
        let last_addr = addr + size - 1;
        let mut blocks_needed: usize = 0;

        let mut iter = self.m_memory_block_manager.find_iterator(addr);
        let Some(first_block) = iter.next() else {
            return (svc_results::RESULT_INVALID_CURRENT_MEMORY.get_inner_value(), None, None, None, 0);
        };

        // If start address isn't aligned to block start, need a split.
        if (addr & !(PAGE_SIZE - 1)) != first_block.get_address() {
            blocks_needed += 1;
        }

        let mut info = first_block.get_memory_info();
        let first_state = info.m_state;
        let first_perm = info.m_permission;
        let first_attr = info.m_attribute;

        loop {
            // All blocks must have the same state/perm/attr (ignoring ignore_attr).
            if info.m_state != first_state {
                return (svc_results::RESULT_INVALID_CURRENT_MEMORY.get_inner_value(), None, None, None, 0);
            }
            if info.m_permission != first_perm {
                return (svc_results::RESULT_INVALID_CURRENT_MEMORY.get_inner_value(), None, None, None, 0);
            }
            if (info.m_attribute | ignore_attr) != (first_attr | ignore_attr) {
                return (svc_results::RESULT_INVALID_CURRENT_MEMORY.get_inner_value(), None, None, None, 0);
            }

            // Check against the provided masks.
            let result = Self::check_memory_state_info(
                &info, state_mask, state, perm_mask, perm, attr_mask, attr,
            );
            if result != 0 {
                return (result, None, None, None, 0);
            }

            if last_addr <= info.get_last_address() {
                break;
            }

            let Some(next_block) = iter.next() else {
                return (svc_results::RESULT_INVALID_CURRENT_MEMORY.get_inner_value(), None, None, None, 0);
            };
            info = next_block.get_memory_info();
        }

        // If end address isn't aligned to block end, need a split.
        if ((last_addr & !(PAGE_SIZE - 1)) + PAGE_SIZE) != info.get_end_address() {
            blocks_needed += 1;
        }

        let out_attr = first_attr & !ignore_attr;
        (0, Some(first_state), Some(first_perm), Some(out_attr), blocks_needed)
    }

    /// Convenience: check memory state, discarding output.
    /// Matches upstream `CheckMemoryState(addr, size, ...)`.
    pub fn check_memory_state(
        &self,
        addr: usize,
        size: usize,
        state_mask: KMemoryState,
        state: KMemoryState,
        perm_mask: KMemoryPermission,
        perm: KMemoryPermission,
        attr_mask: KMemoryAttribute,
        attr: KMemoryAttribute,
    ) -> u32 {
        let (result, _, _, _, _) = self.check_memory_state_range(
            addr, size, state_mask, state, perm_mask, perm, attr_mask, attr,
            Self::DEFAULT_MEMORY_IGNORE_ATTR,
        );
        result
    }

    // -- InitializeForProcess --

    /// Initialize the page table for a user process.
    /// Matches upstream `KPageTableBase::InitializeForProcess`.
    ///
    /// Computes all memory region layouts based on address space width,
    /// code address/size, and ASLR randomization.
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
        let as_width = Self::get_address_space_width_from_flags(as_flags) as u32;
        let start: usize = 0;
        let end: usize = 1usize << as_width;

        // Validate the region.
        debug_assert!(start <= code_address);
        debug_assert!(code_address < code_address + code_size);
        debug_assert!(code_address + code_size - 1 <= end - 1);

        // Helpers for address space info lookups.
        let get_space_start = |info_type: AddressSpaceInfoType| -> usize {
            KAddressSpaceInfo::get_address_space_start(as_width as usize, info_type) as usize
        };
        let get_space_size = |info_type: AddressSpaceInfoType| -> usize {
            KAddressSpaceInfo::get_address_space_size(as_width as usize, info_type) as usize
        };

        self.m_address_space_width = as_width;
        let mut alias_region_size = get_space_size(AddressSpaceInfoType::Alias);
        let mut heap_region_size = get_space_size(AddressSpaceInfoType::Heap);

        // Adjust for 32-bit without alias.
        let as_type = as_flags & 0xF;
        if as_type == 3 {
            // AddressSpace32BitWithoutAlias
            heap_region_size += alias_region_size;
            alias_region_size = 0;
        }

        // Set code regions and determine remaining sizes.
        let process_code_start: usize;
        let process_code_end: usize;
        let mut stack_region_size: usize;
        let mut kernel_map_region_size: usize;

        if as_width == 39 {
            alias_region_size = get_space_size(AddressSpaceInfoType::Alias);
            heap_region_size = get_space_size(AddressSpaceInfoType::Heap);
            stack_region_size = get_space_size(AddressSpaceInfoType::Stack);
            kernel_map_region_size = get_space_size(AddressSpaceInfoType::MapSmall);
            self.m_code_region_start = self.m_address_space_start + aslr_space_start
                + get_space_start(AddressSpaceInfoType::Map39Bit);
            self.m_code_region_end =
                self.m_code_region_start + get_space_size(AddressSpaceInfoType::Map39Bit);
            self.m_alias_code_region_start = self.m_code_region_start;
            self.m_alias_code_region_end = self.m_code_region_end;
            process_code_start = code_address & !(REGION_ALIGNMENT - 1);
            process_code_end = (code_address + code_size + REGION_ALIGNMENT - 1) & !(REGION_ALIGNMENT - 1);
        } else {
            stack_region_size = 0;
            kernel_map_region_size = 0;
            self.m_code_region_start = get_space_start(AddressSpaceInfoType::MapSmall);
            self.m_code_region_end =
                self.m_code_region_start + get_space_size(AddressSpaceInfoType::MapSmall);
            self.m_stack_region_start = self.m_code_region_start;
            self.m_alias_code_region_start = self.m_code_region_start;
            self.m_alias_code_region_end = get_space_start(AddressSpaceInfoType::MapLarge)
                + get_space_size(AddressSpaceInfoType::MapLarge);
            self.m_stack_region_end = self.m_code_region_end;
            self.m_kernel_map_region_start = self.m_code_region_start;
            self.m_kernel_map_region_end = self.m_code_region_end;
            process_code_start = self.m_code_region_start;
            process_code_end = self.m_code_region_end;
        }

        // Set other basic fields.
        self.m_enable_aslr = enable_aslr;
        self.m_enable_device_address_space_merge = enable_das_merge;
        self.m_address_space_start = start;
        self.m_address_space_end = end;
        self.m_is_kernel = false;

        // Determine the region for placing alias/heap/stack/kmap.
        let alloc_start: usize;
        let alloc_size: usize;
        if (process_code_start - self.m_code_region_start) >= (end - process_code_end) {
            alloc_start = self.m_code_region_start;
            alloc_size = process_code_start - self.m_code_region_start;
        } else {
            alloc_start = process_code_end;
            alloc_size = end - process_code_end;
        }
        let needed_size = alias_region_size + heap_region_size + stack_region_size + kernel_map_region_size;
        if alloc_size < needed_size {
            return svc_results::RESULT_OUT_OF_MEMORY.get_inner_value();
        }
        let remaining_size = alloc_size - needed_size;

        // Determine random placements for each region.
        let (alias_rnd, heap_rnd, stack_rnd, kmap_rnd) = if enable_aslr {
            let max_units = if remaining_size / REGION_ALIGNMENT > 0 {
                remaining_size / REGION_ALIGNMENT
            } else {
                0
            };
            let gen = |max: usize| -> usize {
                if max == 0 { return 0; }
                use std::collections::hash_map::DefaultHasher;
                use std::hash::{Hash, Hasher};
                let mut h = DefaultHasher::new();
                std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap_or_default()
                    .as_nanos()
                    .hash(&mut h);
                (h.finish() as usize % (max + 1)) * REGION_ALIGNMENT
            };
            (gen(max_units), gen(max_units), gen(max_units), gen(max_units))
        } else {
            (0, 0, 0, 0)
        };

        // Setup alias and heap regions.
        self.m_alias_region_start = alloc_start + alias_rnd;
        self.m_alias_region_end = self.m_alias_region_start + alias_region_size;
        self.m_heap_region_start = alloc_start + heap_rnd;
        self.m_heap_region_end = self.m_heap_region_start + heap_region_size;

        if alias_rnd <= heap_rnd {
            self.m_heap_region_start += alias_region_size;
            self.m_heap_region_end += alias_region_size;
        } else {
            self.m_alias_region_start += heap_region_size;
            self.m_alias_region_end += heap_region_size;
        }

        // Setup stack region.
        if stack_region_size > 0 {
            self.m_stack_region_start = alloc_start + stack_rnd;
            self.m_stack_region_end = self.m_stack_region_start + stack_region_size;

            if alias_rnd < stack_rnd {
                self.m_stack_region_start += alias_region_size;
                self.m_stack_region_end += alias_region_size;
            } else {
                self.m_alias_region_start += stack_region_size;
                self.m_alias_region_end += stack_region_size;
            }

            if heap_rnd < stack_rnd {
                self.m_stack_region_start += heap_region_size;
                self.m_stack_region_end += heap_region_size;
            } else {
                self.m_heap_region_start += stack_region_size;
                self.m_heap_region_end += stack_region_size;
            }
        }

        // Setup kernel map region.
        if kernel_map_region_size > 0 {
            self.m_kernel_map_region_start = alloc_start + kmap_rnd;
            self.m_kernel_map_region_end = self.m_kernel_map_region_start + kernel_map_region_size;

            if alias_rnd < kmap_rnd {
                self.m_kernel_map_region_start += alias_region_size;
                self.m_kernel_map_region_end += alias_region_size;
            } else {
                self.m_alias_region_start += kernel_map_region_size;
                self.m_alias_region_end += kernel_map_region_size;
            }

            if heap_rnd < kmap_rnd {
                self.m_kernel_map_region_start += heap_region_size;
                self.m_kernel_map_region_end += heap_region_size;
            } else {
                self.m_heap_region_start += kernel_map_region_size;
                self.m_heap_region_end += kernel_map_region_size;
            }

            if stack_region_size > 0 {
                if stack_rnd < kmap_rnd {
                    self.m_kernel_map_region_start += stack_region_size;
                    self.m_kernel_map_region_end += stack_region_size;
                } else {
                    self.m_stack_region_start += kernel_map_region_size;
                    self.m_stack_region_end += kernel_map_region_size;
                }
            }
        }

        // Set heap and fill members.
        self.m_current_heap_end = self.m_heap_region_start;
        self.m_max_heap_size = 0;
        self.m_mapped_physical_memory_size = 0;
        self.m_mapped_unsafe_physical_memory = 0;
        self.m_mapped_insecure_memory = 0;
        self.m_mapped_ipc_server_memory = 0;

        self.m_heap_fill_value = MemoryFillValue::Zero;
        self.m_ipc_fill_value = MemoryFillValue::Zero;
        self.m_stack_fill_value = MemoryFillValue::Zero;

        // Set allocation option.
        let direction = if from_back {
            k_memory_manager::Direction::FromBack
        } else {
            k_memory_manager::Direction::FromFront
        };
        self.m_allocate_option =
            ((pool & k_memory_manager::Pool::MASK) << k_memory_manager::Pool::SHIFT)
                | ((direction as u32 & k_memory_manager::Direction::MASK)
                    << k_memory_manager::Direction::SHIFT);

        // Validate regions inside address space.
        debug_assert!(self.m_alias_region_start >= self.m_address_space_start);
        debug_assert!(self.m_alias_region_end <= self.m_address_space_end);
        debug_assert!(self.m_heap_region_start >= self.m_address_space_start);
        debug_assert!(self.m_heap_region_end <= self.m_address_space_end);
        debug_assert!(self.m_stack_region_start >= self.m_address_space_start);
        debug_assert!(self.m_stack_region_end <= self.m_address_space_end);
        debug_assert!(self.m_kernel_map_region_start >= self.m_address_space_start);
        debug_assert!(self.m_kernel_map_region_end <= self.m_address_space_end);

        // Initialize the memory block manager.
        if self
            .m_memory_block_manager
            .initialize(self.m_address_space_start, self.m_address_space_end)
            .is_err()
        {
            return svc_results::RESULT_OUT_OF_MEMORY.get_inner_value();
        }

        log::debug!(
            "KPageTableBase::InitializeForProcess: width={}, code=[{:#x}..{:#x}], \
             alias_code=[{:#x}..{:#x}], heap=[{:#x}..{:#x}], alias=[{:#x}..{:#x}], \
             stack=[{:#x}..{:#x}], kmap=[{:#x}..{:#x}]",
            as_width, self.m_code_region_start, self.m_code_region_end,
            self.m_alias_code_region_start, self.m_alias_code_region_end,
            self.m_heap_region_start, self.m_heap_region_end,
            self.m_alias_region_start, self.m_alias_region_end,
            self.m_stack_region_start, self.m_stack_region_end,
            self.m_kernel_map_region_start, self.m_kernel_map_region_end,
        );

        0 // success
    }

    // -- Memory / PageTable wiring --

    /// Set the Memory bridge and initialize the page table implementation.
    /// Must be called after InitializeForProcess.
    pub fn set_memory(&mut self, memory: Arc<Mutex<Memory>>) {
        self.m_memory = Some(memory);
    }

    /// Initialize the Common::PageTable impl.
    /// Matches upstream: `m_impl = make_unique<PageTable>(); m_impl->Resize(width, PageBits)`
    pub fn initialize_impl(&mut self) {
        let mut pt = Box::new(common::page_table::PageTable::new());
        pt.resize(self.m_address_space_width as usize, PAGE_BITS);
        self.m_impl = Some(pt);
    }

    /// Get the inner page table (for dynarmic).
    pub fn get_impl(&self) -> Option<&common::page_table::PageTable> {
        self.m_impl.as_deref()
    }

    pub fn get_impl_mut(&mut self) -> Option<&mut common::page_table::PageTable> {
        self.m_impl.as_deref_mut()
    }

    // -- Operate --

    /// Convert KMemoryPermission to Common::MemoryPermission.
    /// Matches upstream `ConvertToMemoryPermission`.
    fn convert_to_memory_permission(perm: KMemoryPermission) -> crate::memory::memory::MemoryPermission {
        use crate::memory::memory::MemoryPermission;
        let mut perms = MemoryPermission::empty();
        if perm.contains(KMemoryPermission::USER_READ) {
            perms |= MemoryPermission::READ;
        }
        if perm.contains(KMemoryPermission::USER_WRITE) {
            perms |= MemoryPermission::WRITE;
        }
        perms
    }

    /// Core page table operation — maps, unmaps, or changes permissions.
    /// Matches upstream `KPageTableBase::Operate(PageLinkedList*, virt_addr, num_pages,
    /// phys_addr, is_pa_valid, properties, operation, reuse_ll)`.
    ///
    /// In the emulator, we don't use PageLinkedList (no guest page table entries).
    /// The operation modifies Core::Memory::Memory which updates the dynarmic PageTable.
    pub fn operate(
        &mut self,
        virt_addr: usize,
        num_pages: usize,
        phys_addr: u64,
        _is_pa_valid: bool,
        properties: KPageProperties,
        operation: OperationType,
    ) -> u32 {
        debug_assert!(num_pages > 0);
        debug_assert!(virt_addr % PAGE_SIZE == 0);

        let size = num_pages * PAGE_SIZE;

        match operation {
            OperationType::Unmap | OperationType::UnmapPhysical => {
                let separate_heap = matches!(operation, OperationType::UnmapPhysical);

                if let (Some(memory), Some(impl_pt)) = (&self.m_memory, &mut self.m_impl) {
                    memory.lock().unwrap().unmap_region(
                        impl_pt,
                        virt_addr as u64,
                        size as u64,
                        separate_heap,
                    );
                }

                0 // success
            }
            OperationType::Map => {
                debug_assert!(virt_addr != 0);

                if let (Some(memory), Some(impl_pt)) = (&self.m_memory, &mut self.m_impl) {
                    memory.lock().unwrap().map_memory_region(
                        impl_pt,
                        virt_addr as u64,
                        size as u64,
                        phys_addr,
                        Self::convert_to_memory_permission(properties.perm),
                        false,
                    );
                }

                // TODO: if phys_addr is heap, call MemoryManager.Open(phys_addr, num_pages)

                0
            }
            OperationType::Separate => {
                // No-op in emulator.
                0
            }
            OperationType::ChangePermissions
            | OperationType::ChangePermissionsAndRefresh
            | OperationType::ChangePermissionsAndRefreshAndFlush => {
                if let (Some(memory), Some(impl_pt)) = (&self.m_memory, &mut self.m_impl) {
                    memory.lock().unwrap().protect_region(
                        impl_pt,
                        virt_addr as u64,
                        size as u64,
                        Self::convert_to_memory_permission(properties.perm),
                    );
                }

                0
            }
            _ => {
                log::error!("KPageTableBase::Operate: unhandled operation {:?}", operation);
                svc_results::RESULT_INVALID_STATE.get_inner_value()
            }
        }
    }

    // -- FindFreeArea --

    /// Find a free area in the given region.
    /// Matches upstream `KPageTableBase::FindFreeArea`.
    ///
    /// If ASLR is enabled, tries up to 8 random placements first,
    /// then falls back to a random-offset linear scan, then a plain linear scan.
    pub fn find_free_area(
        &self,
        region_start: usize,
        region_num_pages: usize,
        num_pages: usize,
        alignment: usize,
        offset: usize,
        guard_pages: usize,
    ) -> usize {
        let mut address: usize = 0;

        if num_pages <= region_num_pages {
            if self.m_enable_aslr {
                // Try to directly find a free area up to 8 times.
                let max_offset_pages = region_num_pages
                    .saturating_sub(num_pages)
                    .saturating_sub(guard_pages);

                for _ in 0..8 {
                    if max_offset_pages == 0 || alignment == 0 {
                        break;
                    }
                    let random_offset = {
                        use std::collections::hash_map::DefaultHasher;
                        use std::hash::{Hash, Hasher};
                        let mut h = DefaultHasher::new();
                        std::time::SystemTime::now()
                            .duration_since(std::time::UNIX_EPOCH)
                            .unwrap_or_default()
                            .as_nanos()
                            .hash(&mut h);
                        (h.finish() as usize % ((max_offset_pages * PAGE_SIZE / alignment) + 1))
                            * alignment
                    };
                    let candidate =
                        ((region_start + random_offset) & !(alignment - 1)) + offset;

                    if let Some(info) = self.m_memory_block_manager.query_info(candidate) {
                        if info.m_state != KMemoryState::FREE {
                            continue;
                        }
                        if region_start > candidate {
                            continue;
                        }
                        if info.get_address() + guard_pages * PAGE_SIZE > candidate {
                            continue;
                        }
                        if candidate + (num_pages + guard_pages) * PAGE_SIZE - 1
                            > info.get_last_address()
                        {
                            continue;
                        }
                        if candidate + (num_pages + guard_pages) * PAGE_SIZE - 1
                            > region_start + region_num_pages * PAGE_SIZE - 1
                        {
                            continue;
                        }
                        address = candidate;
                        break;
                    }
                }

                // Fall back to finding free area with random offset.
                if address == 0 {
                    let offset_pages = {
                        use std::collections::hash_map::DefaultHasher;
                        use std::hash::{Hash, Hasher};
                        let mut h = DefaultHasher::new();
                        std::time::SystemTime::now()
                            .duration_since(std::time::UNIX_EPOCH)
                            .unwrap_or_default()
                            .as_nanos()
                            .hash(&mut h);
                        h.finish() as usize % (max_offset_pages + 1)
                    };
                    address = self.m_memory_block_manager.find_free_area(
                        region_start + offset_pages * PAGE_SIZE,
                        region_num_pages.saturating_sub(offset_pages),
                        num_pages,
                        alignment,
                        offset,
                        guard_pages,
                    ).unwrap_or(0);
                }
            }
            // Find the first free area (no ASLR or fallback).
            if address == 0 {
                address = self.m_memory_block_manager.find_free_area(
                    region_start,
                    region_num_pages,
                    num_pages,
                    alignment,
                    offset,
                    guard_pages,
                ).unwrap_or(0);
            }
        }

        address
    }

    // -- SetMaxHeapSize / SetHeapSize --

    /// Matches upstream `KPageTableBase::SetMaxHeapSize`.
    pub fn set_max_heap_size(&mut self, size: usize) -> u32 {
        self.m_max_heap_size = size;
        0
    }

    /// Matches upstream `KPageTableBase::SetHeapSize`.
    /// Grows or shrinks the heap region.
    pub fn set_heap_size(&mut self, size: usize) -> (u32, usize) {
        // Validate size.
        if size > self.get_heap_region_size() {
            return (svc_results::RESULT_OUT_OF_MEMORY.get_inner_value(), 0);
        }
        if size > self.m_max_heap_size {
            return (svc_results::RESULT_OUT_OF_MEMORY.get_inner_value(), 0);
        }

        let target_end = self.m_heap_region_start + size;

        if target_end > self.m_current_heap_end {
            // Growing heap — upstream allocates physical pages and maps them.
            // For now, just advance the pointer. The memory backing is in
            // the shared GuestMemory which already covers the full address space.
            // TODO: call m_memory->MapMemoryRegion when Memory is ported.
            self.m_current_heap_end = target_end;
        } else if target_end < self.m_current_heap_end {
            // Shrinking heap — upstream unmaps and frees pages.
            // TODO: call m_memory->UnmapRegion when Memory is ported.
            self.m_current_heap_end = target_end;
        }

        // Update memory block manager.
        if size > 0 {
            self.m_memory_block_manager.update(
                self.m_heap_region_start,
                size / PAGE_SIZE,
                KMemoryState::NORMAL,
                KMemoryPermission::USER_READ_WRITE,
                KMemoryAttribute::NONE,
                KMemoryBlockDisableMergeAttribute::NONE,
                KMemoryBlockDisableMergeAttribute::NONE,
            );
        }

        (0, self.m_heap_region_start)
    }

    // -- QueryInfo --

    /// Query memory info at an address.
    /// Matches upstream `KPageTableBase::QueryInfoImpl`.
    pub fn query_info(&self, addr: usize) -> Option<KMemoryInfo> {
        self.m_memory_block_manager.query_info(addr)
    }

    // -- SetMemoryPermission --

    /// Set memory permission for a range.
    /// Matches upstream `KPageTableBase::SetMemoryPermission`.
    pub fn set_memory_permission(
        &mut self,
        addr: usize,
        size: usize,
        perm: KMemoryPermission,
    ) -> u32 {
        let num_pages = size / PAGE_SIZE;
        // Validate the range is within address space.
        if !self.contains_range(addr, size) {
            return svc_results::RESULT_INVALID_CURRENT_MEMORY.get_inner_value();
        }

        // Check current memory state — verify the region can be reprotected.
        // Upstream: state_mask = FlagCanReprotect, state = FlagCanReprotect
        let (result, out_state, _out_perm, _out_attr, _) = self.check_memory_state_range(
            addr, size,
            KMemoryState::FLAG_CAN_REPROTECT, KMemoryState::FLAG_CAN_REPROTECT,
            KMemoryPermission::NONE, KMemoryPermission::NONE,
            KMemoryAttribute::from_bits_truncate(0xFF), KMemoryAttribute::NONE,
            Self::DEFAULT_MEMORY_IGNORE_ATTR,
        );
        if result != 0 {
            return result;
        }

        let state = out_state.unwrap_or(KMemoryState::NORMAL);

        // TODO: call Operate to change protection in Core::Memory::Memory
        // Update block manager.
        self.m_memory_block_manager.update(
            addr,
            num_pages,
            state,
            perm,
            KMemoryAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NONE,
        );
        0
    }

    // -- SetMemoryAttribute --

    /// Set memory attribute for a range.
    /// Matches upstream `KPageTableBase::SetMemoryAttribute`.
    pub fn set_memory_attribute(
        &mut self,
        _addr: usize,
        _size: usize,
        _mask: u32,
        _attr: u32,
    ) -> u32 {
        // TODO: full implementation with CheckMemoryState + Operate
        0
    }

    // -- MapMemory / UnmapMemory --

    pub fn map_memory(&mut self, _dst: usize, _src: usize, _size: usize) -> u32 {
        // TODO: full implementation (stack mirror mapping)
        0
    }

    pub fn unmap_memory(&mut self, _dst: usize, _src: usize, _size: usize) -> u32 {
        // TODO: full implementation
        0
    }

    // -- MapStatic / MapIo / MapRegion (for capabilities) --

    pub fn map_static(&mut self, phys_addr: u64, size: usize, _perm: KMemoryPermission) -> u32 {
        log::debug!("KPageTableBase::map_static(phys={:#x}, size={:#x})", phys_addr, size);
        // TODO: full MapStatic with FindFreeArea + Operate + block manager update
        0
    }

    pub fn map_io(&mut self, phys_addr: u64, size: usize, _perm: KMemoryPermission) -> u32 {
        log::debug!("KPageTableBase::map_io(phys={:#x}, size={:#x})", phys_addr, size);
        // TODO: full MapIo implementation
        0
    }

    pub fn map_region(&mut self, _region_type: u32, _perm: KMemoryPermission) -> u32 {
        log::debug!("KPageTableBase::map_region(type={})", _region_type);
        // TODO: full MapRegion (lookup region in KMemoryLayout, delegate to MapStatic)
        0
    }

    // -- SetProcessMemoryPermission (for NSO loading) --

    /// Set memory permission for process code/data sections.
    /// Matches upstream `KPageTableBase::SetProcessMemoryPermission`.
    pub fn set_process_memory_permission(
        &mut self,
        addr: usize,
        size: usize,
        perm: KMemoryPermission,
    ) -> u32 {
        let num_pages = size / PAGE_SIZE;
        if !self.contains_range(addr, size) {
            return svc_results::RESULT_INVALID_CURRENT_MEMORY.get_inner_value();
        }

        // Upstream: determines new state based on current state + permission.
        // Code + Write -> CodeData, Code + Execute -> Code, etc.
        let new_state = if perm.contains(KMemoryPermission::USER_EXECUTE) {
            KMemoryState::CODE
        } else {
            KMemoryState::CODE_DATA
        };

        // TODO: full CheckMemoryState, Operate (change protection), MakePageGroup
        self.m_memory_block_manager.update(
            addr,
            num_pages,
            new_state,
            perm,
            KMemoryAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NONE,
        );
        0
    }

    // -- MapPhysicalMemory / UnmapPhysicalMemory --

    pub fn map_physical_memory(&mut self, _addr: usize, _size: usize) -> u32 {
        // TODO: full implementation
        0
    }

    pub fn unmap_physical_memory(&mut self, _addr: usize, _size: usize) -> u32 {
        // TODO: full implementation
        0
    }
}

impl Default for KPageTableBase {
    fn default() -> Self {
        Self::new()
    }
}
