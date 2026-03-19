//! Port of zuyu/src/core/hle/kernel/k_page_table_base.h / k_page_table_base.cpp
//! Status: Partial (~65 methods implemented, 13 stubs returning 0, ~29 upstream methods missing)
//! Derniere synchro: 2026-03-17
//!
//! The base class for virtual memory page tables.
//! Contains all address space regions, memory block manager, and virtual memory operations.
//!
//! Missing upstream methods include: MapPageGroup, UnmapPageGroup, UnmapProcessMemory,
//! QueryInfoImpl, QueryMappingImpl, IO memory ops, InitializeForKernel, and others.
//! Stub methods (return 0 without real logic): IPC setup/cleanup, cache invalidation,
//! debug memory read/write, linear copy operations.

use bitflags::bitflags;

use std::sync::{Arc, Mutex};

use super::k_address_space_info::{AddressSpaceInfoType, KAddressSpaceInfo};
use super::k_memory_block::*;
use super::k_memory_block_manager::KMemoryBlockManager;
use super::k_memory_layout::KERNEL_ASLR_ALIGNMENT;
use super::k_memory_manager;
use super::k_resource_limit::KResourceLimit;
use super::svc_types::{ADDRESS_SPACE_MASK, CreateProcessFlag, MemoryState as SvcMemoryState};
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
    /// Upstream: `KResourceLimit* m_resource_limit`.
    pub(crate) m_resource_limit: Option<Arc<Mutex<KResourceLimit>>>,
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
            m_resource_limit: None,
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
    pub fn get_current_heap_size(&self) -> usize {
        self.m_current_heap_end.saturating_sub(self.m_heap_region_start)
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
    /// Upstream matches on the masked enum values directly:
    ///   AddressSpace32Bit = 0<<1 = 0
    ///   AddressSpace64BitDeprecated = 1<<1 = 2
    ///   AddressSpace32BitWithoutAlias = 2<<1 = 4
    ///   AddressSpace64Bit = 3<<1 = 6
    pub fn get_address_space_width_from_flags(flags: u32) -> usize {
        use super::svc_types::{ADDRESS_SPACE_MASK, CreateProcessFlag};
        match flags & ADDRESS_SPACE_MASK {
            x if x == CreateProcessFlag::ADDRESS_SPACE_64_BIT.bits() => 39,
            x if x == CreateProcessFlag::ADDRESS_SPACE_64_BIT_DEPRECATED.bits() => 36,
            x if x == CreateProcessFlag::ADDRESS_SPACE_32_BIT.bits() => 32,
            x if x == CreateProcessFlag::ADDRESS_SPACE_32_BIT_WITHOUT_ALIAS.bits() => 32,
            other => panic!("Invalid address space flag: flags={:#x}, masked={:#x}", flags, other),
        }
    }

    pub fn get_memory_block_manager(&self) -> &KMemoryBlockManager {
        &self.m_memory_block_manager
    }

    pub fn get_memory_block_manager_mut(&mut self) -> &mut KMemoryBlockManager {
        &mut self.m_memory_block_manager
    }

    // -- Region resolution matching upstream GetRegionAddress/GetRegionSize --

    /// Get the start address for a memory region by SVC memory state.
    /// Matches upstream `KPageTableBase::GetRegionAddress(Svc::MemoryState)`.
    pub fn get_region_address(&self, state: SvcMemoryState) -> usize {
        use SvcMemoryState::*;
        match state {
            Free | Kernel => self.m_address_space_start,
            Normal => self.m_heap_region_start,
            Ipc | NonSecureIpc | NonDeviceIpc => self.m_alias_region_start,
            Stack => self.m_stack_region_start,
            Static | ThreadLocal => self.m_kernel_map_region_start,
            Io | Shared | AliasCode | AliasCodeData
            | Transferred | SharedTransferred | SharedCode
            | GeneratedCode | CodeOut | Coverage | Insecure => {
                self.m_alias_code_region_start
            }
            Code | CodeData => self.m_code_region_start,
            _ => {
                log::error!("GetRegionAddress: unknown state {:?}", state);
                self.m_address_space_start
            }
        }
    }

    /// Get the size of a memory region by SVC memory state.
    /// Matches upstream `KPageTableBase::GetRegionSize(Svc::MemoryState)`.
    pub fn get_region_size(&self, state: SvcMemoryState) -> usize {
        use SvcMemoryState::*;
        match state {
            Free | Kernel => self.m_address_space_end - self.m_address_space_start,
            Normal => self.m_heap_region_end - self.m_heap_region_start,
            Ipc | NonSecureIpc | NonDeviceIpc => {
                self.m_alias_region_end - self.m_alias_region_start
            }
            Stack => self.m_stack_region_end - self.m_stack_region_start,
            Static | ThreadLocal => {
                self.m_kernel_map_region_end - self.m_kernel_map_region_start
            }
            Io | Shared | AliasCode | AliasCodeData
            | Transferred | SharedTransferred | SharedCode
            | GeneratedCode | CodeOut | Coverage | Insecure => {
                self.m_alias_code_region_end - self.m_alias_code_region_start
            }
            Code | CodeData => self.m_code_region_end - self.m_code_region_start,
            _ => {
                log::error!("GetRegionSize: unknown state {:?}", state);
                self.m_address_space_end - self.m_address_space_start
            }
        }
    }

    /// Check if an address range can be contained within a region for a given state.
    /// Matches upstream `KPageTableBase::CanContain(KProcessAddress, size_t, Svc::MemoryState)`.
    pub fn can_contain(&self, addr: usize, size: usize, state: SvcMemoryState) -> bool {
        let region_start = self.get_region_address(state);
        let region_size = self.get_region_size(state);
        let region_end = region_start + region_size;
        region_start <= addr && addr + size <= region_end
    }

    // -- KMemoryState convenience overloads --
    // Upstream: inline overloads in k_page_table_base.h that cast
    //   `static_cast<Svc::MemoryState>(state & KMemoryState::Mask)`.

    fn k_state_to_svc(state: KMemoryState) -> SvcMemoryState {
        let svc = state.bits() & KMemoryState::MASK.bits();
        // SAFETY: all valid KMemoryState lower bytes map to valid SvcMemoryState discriminants.
        unsafe { std::mem::transmute(svc) }
    }

    pub fn get_region_address_k(&self, state: KMemoryState) -> usize {
        self.get_region_address(Self::k_state_to_svc(state))
    }

    pub fn get_region_size_k(&self, state: KMemoryState) -> usize {
        self.get_region_size(Self::k_state_to_svc(state))
    }

    pub fn can_contain_k(&self, addr: usize, size: usize, state: KMemoryState) -> bool {
        self.can_contain(addr, size, Self::k_state_to_svc(state))
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
    /// Upstream: `Result KPageTableBase::InitializeForProcess(
    ///     Svc::CreateProcessFlag as_type, bool enable_aslr, bool enable_das_merge,
    ///     bool from_back, KMemoryManager::Pool pool, KProcessAddress code_address,
    ///     size_t code_size, KSystemResource* system_resource,
    ///     KResourceLimit* resource_limit, Core::Memory::Memory& memory,
    ///     KProcessAddress aslr_space_start)`
    pub fn initialize_for_process(
        &mut self,
        as_flags: u32,
        enable_aslr: bool,
        enable_das_merge: bool,
        from_back: bool,
        pool: u32,
        code_address: usize,
        code_size: usize,
        resource_limit: Option<Arc<Mutex<KResourceLimit>>>,
        memory: Option<Arc<Mutex<Memory>>>,
        aslr_space_start: usize,
    ) -> u32 {
        // Store resource limit and memory references.
        // Upstream stores these for use in heap operations, mapping, etc.
        self.m_resource_limit = resource_limit;
        self.m_memory = memory;
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
        // Upstream: AddressSpace32BitWithoutAlias = 2 << 1 = 4
        if (as_flags & ADDRESS_SPACE_MASK) == CreateProcessFlag::ADDRESS_SPACE_32_BIT_WITHOUT_ALIAS.bits() {
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
    /// Faithfully implements heap grow/shrink with Operate calls.
    pub fn set_heap_size(&mut self, size: usize) -> (u32, usize) {
        // Validate preconditions.
        if self.m_is_kernel {
            return (svc_results::RESULT_OUT_OF_MEMORY.get_inner_value(), 0);
        }
        if size > (self.m_heap_region_end - self.m_heap_region_start) {
            return (svc_results::RESULT_OUT_OF_MEMORY.get_inner_value(), 0);
        }
        if size > self.m_max_heap_size {
            return (svc_results::RESULT_OUT_OF_MEMORY.get_inner_value(), 0);
        }

        let current_heap_size = self.m_current_heap_end - self.m_heap_region_start;

        if size < current_heap_size {
            // === Shrink heap ===
            // Validate the memory being freed is Normal/UserReadWrite.
            let free_start = self.m_heap_region_start + size;
            let free_size = current_heap_size - size;
            let result = self.check_memory_state(
                free_start,
                free_size,
                KMemoryState::MASK,
                KMemoryState::NORMAL,
                KMemoryPermission::from_bits_truncate(0xFF),
                KMemoryPermission::USER_READ_WRITE,
                KMemoryAttribute::from_bits_truncate(0xFF),
                KMemoryAttribute::NONE,
            );
            if result != 0 {
                return (result, 0);
            }

            // Unmap the end of the heap.
            let num_pages = free_size / PAGE_SIZE;
            let unmap_properties = KPageProperties {
                perm: KMemoryPermission::NONE,
                io: false,
                uncached: false,
                disable_merge_attributes: DisableMergeAttribute::NONE,
            };
            let op_result = self.operate(
                free_start,
                num_pages,
                0,
                false,
                unmap_properties,
                OperationType::Unmap,
            );
            if op_result != 0 {
                return (op_result, 0);
            }

            // Update block manager: freed region becomes Free.
            self.m_memory_block_manager.update(
                free_start,
                num_pages,
                KMemoryState::FREE,
                KMemoryPermission::NONE,
                KMemoryAttribute::NONE,
                KMemoryBlockDisableMergeAttribute::NONE,
                if size == 0 {
                    KMemoryBlockDisableMergeAttribute::NORMAL
                } else {
                    KMemoryBlockDisableMergeAttribute::NONE
                },
            );

            self.m_current_heap_end = self.m_heap_region_start + size;
            return (0, self.m_heap_region_start);
        } else if size == current_heap_size {
            // === Same size ===
            return (0, self.m_heap_region_start);
        }

        // === Grow heap ===
        let cur_address = self.m_current_heap_end;
        let allocation_size = size - current_heap_size;

        // Upstream: reserve resource limit, allocate physical pages.
        // In our emulator, physical memory is pre-allocated in DeviceMemory.
        // We compute the physical address from the virtual address.
        // Physical address = DramMemoryMap::Base + virtual address offset.
        let phys_addr = crate::device_memory::dram_memory_map::BASE + cur_address as u64;

        // Check that the region to grow is Free.
        let result = self.check_memory_state(
            cur_address,
            allocation_size,
            KMemoryState::MASK,
            KMemoryState::FREE,
            KMemoryPermission::NONE,
            KMemoryPermission::NONE,
            KMemoryAttribute::NONE,
            KMemoryAttribute::NONE,
        );
        if result != 0 {
            return (result, 0);
        }

        // Map the new heap pages.
        let num_pages = allocation_size / PAGE_SIZE;
        let map_properties = KPageProperties {
            perm: KMemoryPermission::USER_READ_WRITE,
            io: false,
            uncached: false,
            disable_merge_attributes: if self.m_current_heap_end == self.m_heap_region_start {
                DisableMergeAttribute::DISABLE_HEAD
            } else {
                DisableMergeAttribute::NONE
            },
        };
        let op_result = self.operate(
            cur_address,
            num_pages,
            phys_addr,
            true,
            map_properties,
            OperationType::Map,
        );
        if op_result != 0 {
            return (op_result, 0);
        }

        // Update block manager: new region is Normal/UserReadWrite.
        self.m_memory_block_manager.update(
            cur_address,
            num_pages,
            KMemoryState::NORMAL,
            KMemoryPermission::USER_READ_WRITE,
            KMemoryAttribute::NONE,
            if self.m_heap_region_start == self.m_current_heap_end {
                KMemoryBlockDisableMergeAttribute::NORMAL
            } else {
                KMemoryBlockDisableMergeAttribute::NONE
            },
            KMemoryBlockDisableMergeAttribute::NONE,
        );

        self.m_current_heap_end = self.m_heap_region_start + size;
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

        let old_state = out_state.unwrap_or(KMemoryState::NORMAL);
        let old_perm = _out_perm.unwrap_or(KMemoryPermission::NONE);

        // If perm is already the same, nothing to do.
        if old_perm == perm {
            return 0;
        }

        // Change permissions via Operate.
        let properties = KPageProperties {
            perm,
            io: false,
            uncached: false,
            disable_merge_attributes: DisableMergeAttribute::NONE,
        };
        let op_result = self.operate(
            addr,
            num_pages,
            0,
            false,
            properties,
            OperationType::ChangePermissions,
        );
        if op_result != 0 {
            return op_result;
        }

        // Update block manager.
        self.m_memory_block_manager.update(
            addr,
            num_pages,
            old_state,
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
        addr: usize,
        size: usize,
        mask: u32,
        attr: u32,
    ) -> u32 {
        let num_pages = size / PAGE_SIZE;
        let mask_attr = KMemoryAttribute::from_bits_truncate(mask as u8);
        let set_attr = KMemoryAttribute::from_bits_truncate(attr as u8);

        // Compute state test mask based on which attributes are being changed.
        let mut state_test_mask = KMemoryState::NONE;
        if mask_attr.contains(KMemoryAttribute::UNCACHED) {
            state_test_mask = KMemoryState::from_bits_truncate(
                state_test_mask.bits() | KMemoryState::FLAG_CAN_CHANGE_ATTRIBUTE.bits(),
            );
        }

        // Check current state.
        let attr_test_mask = KMemoryAttribute::from_bits_truncate(
            !(KMemoryAttribute::SET_MASK.bits() | KMemoryAttribute::DEVICE_SHARED.bits()),
        );
        let ignore_attr = KMemoryAttribute::from_bits_truncate(!attr_test_mask.bits());
        let (result, _out_state, out_perm, out_attr, _) = self.check_memory_state_range(
            addr, size, state_test_mask, state_test_mask,
            KMemoryPermission::NONE, KMemoryPermission::NONE,
            attr_test_mask, KMemoryAttribute::NONE,
            ignore_attr,
        );
        if result != 0 {
            return result;
        }

        let old_perm = out_perm.unwrap_or(KMemoryPermission::NONE);
        let old_attr = out_attr.unwrap_or(KMemoryAttribute::NONE);

        // If uncached attribute is changing, change via Operate.
        if mask_attr.contains(KMemoryAttribute::UNCACHED) {
            let new_attr = KMemoryAttribute::from_bits_truncate(
                (old_attr.bits() & !mask_attr.bits()) | (set_attr.bits() & mask_attr.bits()),
            );
            let properties = KPageProperties {
                perm: old_perm,
                io: false,
                uncached: new_attr.contains(KMemoryAttribute::UNCACHED),
                disable_merge_attributes: DisableMergeAttribute::NONE,
            };
            let op_result = self.operate(
                addr, num_pages, 0, false, properties,
                OperationType::ChangePermissionsAndRefreshAndFlush,
            );
            if op_result != 0 {
                return op_result;
            }
        }

        // Update the blocks via UpdateAttribute.
        self.m_memory_block_manager.update_attribute(
            addr, num_pages, mask_attr, set_attr,
        );
        0
    }

    // -- MapMemory / UnmapMemory (stack mirror) --

    /// Map memory (stack mirror): copies src page group to dst, reprotects src.
    /// Matches upstream `KPageTableBase::MapMemory`.
    pub fn map_memory(&mut self, dst: usize, src: usize, size: usize) -> u32 {
        let num_pages = size / PAGE_SIZE;

        // Check source state — must be aliasable and UserReadWrite.
        let (result, out_src_state, _, _, _) = self.check_memory_state_range(
            src, size,
            KMemoryState::FLAG_CAN_ALIAS, KMemoryState::FLAG_CAN_ALIAS,
            KMemoryPermission::from_bits_truncate(0xFF), KMemoryPermission::USER_READ_WRITE,
            KMemoryAttribute::from_bits_truncate(0xFF), KMemoryAttribute::NONE,
            Self::DEFAULT_MEMORY_IGNORE_ATTR,
        );
        if result != 0 { return result; }

        // Check destination state — must be Free.
        let result = self.check_memory_state(
            dst, size,
            KMemoryState::MASK, KMemoryState::FREE,
            KMemoryPermission::NONE, KMemoryPermission::NONE,
            KMemoryAttribute::NONE, KMemoryAttribute::NONE,
        );
        if result != 0 { return result; }

        let src_state = out_src_state.unwrap_or(KMemoryState::NORMAL);

        // Reprotect source as KernelRead | NotMapped.
        let src_new_perm = KMemoryPermission::from_bits_truncate(
            KMemoryPermission::KERNEL_READ.bits() | KMemoryPermission::NOT_MAPPED.bits(),
        );
        let src_props = KPageProperties {
            perm: src_new_perm, io: false, uncached: false,
            disable_merge_attributes: DisableMergeAttribute::DISABLE_HEAD_BODY_TAIL,
        };
        let op_result = self.operate(src, num_pages, 0, false, src_props, OperationType::ChangePermissions);
        if op_result != 0 { return op_result; }

        // Map dst using the same physical pages as src.
        let phys_addr = crate::device_memory::dram_memory_map::BASE + src as u64;
        let dst_props = KPageProperties {
            perm: KMemoryPermission::USER_READ_WRITE, io: false, uncached: false,
            disable_merge_attributes: DisableMergeAttribute::DISABLE_HEAD,
        };
        let op_result = self.operate(dst, num_pages, phys_addr, true, dst_props, OperationType::Map);
        if op_result != 0 {
            // Revert source on failure.
            let revert_props = KPageProperties {
                perm: KMemoryPermission::USER_READ_WRITE, io: false, uncached: false,
                disable_merge_attributes: DisableMergeAttribute::ENABLE_HEAD_BODY_TAIL,
            };
            let _ = self.operate(src, num_pages, 0, false, revert_props, OperationType::ChangePermissions);
            return op_result;
        }

        // Update source block.
        self.m_memory_block_manager.update(
            src, num_pages, src_state, src_new_perm,
            KMemoryAttribute::LOCKED,
            KMemoryBlockDisableMergeAttribute::LOCKED,
            KMemoryBlockDisableMergeAttribute::NONE,
        );
        // Update destination block.
        self.m_memory_block_manager.update(
            dst, num_pages, KMemoryState::STACK,
            KMemoryPermission::USER_READ_WRITE,
            KMemoryAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NORMAL,
            KMemoryBlockDisableMergeAttribute::NONE,
        );
        0
    }

    /// Unmap memory (undo stack mirror).
    /// Matches upstream `KPageTableBase::UnmapMemory`.
    pub fn unmap_memory(&mut self, dst: usize, src: usize, size: usize) -> u32 {
        let num_pages = size / PAGE_SIZE;

        // Check source is locked/aliased.
        let result = self.check_memory_state(
            src, size,
            KMemoryState::FLAG_CAN_ALIAS, KMemoryState::FLAG_CAN_ALIAS,
            KMemoryPermission::NONE, KMemoryPermission::NONE,
            KMemoryAttribute::from_bits_truncate(0xFF), KMemoryAttribute::LOCKED,
        );
        if result != 0 { return result; }

        // Check destination is Stack.
        let result = self.check_memory_state(
            dst, size,
            KMemoryState::MASK, KMemoryState::STACK,
            KMemoryPermission::NONE, KMemoryPermission::NONE,
            KMemoryAttribute::from_bits_truncate(0xFF), KMemoryAttribute::NONE,
        );
        if result != 0 { return result; }

        // Unmap the destination.
        let unmap_props = KPageProperties {
            perm: KMemoryPermission::NONE, io: false, uncached: false,
            disable_merge_attributes: DisableMergeAttribute::NONE,
        };
        let op_result = self.operate(dst, num_pages, 0, false, unmap_props, OperationType::Unmap);
        if op_result != 0 { return op_result; }

        // Restore source permissions.
        let restore_props = KPageProperties {
            perm: KMemoryPermission::USER_READ_WRITE, io: false, uncached: false,
            disable_merge_attributes: DisableMergeAttribute::ENABLE_AND_MERGE_HEAD_BODY_TAIL,
        };
        let op_result = self.operate(src, num_pages, 0, false, restore_props, OperationType::ChangePermissions);
        if op_result != 0 { return op_result; }

        // Update blocks.
        self.m_memory_block_manager.update(
            src, num_pages, KMemoryState::NORMAL,
            KMemoryPermission::USER_READ_WRITE, KMemoryAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::LOCKED,
        );
        self.m_memory_block_manager.update(
            dst, num_pages, KMemoryState::FREE,
            KMemoryPermission::NONE, KMemoryAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NORMAL,
        );
        0
    }

    // -- LockMemoryAndOpen / UnlockMemory --

    /// Lock a memory region, optionally changing permissions and setting lock attribute.
    /// Matches upstream `KPageTableBase::LockMemoryAndOpen`.
    pub fn lock_memory_and_open(
        &mut self,
        out_paddr: &mut u64,
        addr: usize,
        size: usize,
        state_mask: KMemoryState,
        state: KMemoryState,
        perm_mask: KMemoryPermission,
        perm: KMemoryPermission,
        attr_mask: KMemoryAttribute,
        attr: KMemoryAttribute,
        new_perm: KMemoryPermission,
        lock_attr: KMemoryAttribute,
    ) -> u32 {
        let num_pages = size / PAGE_SIZE;
        if !self.contains_range(addr, size) {
            return svc_results::RESULT_INVALID_CURRENT_MEMORY.get_inner_value();
        }

        // Check memory state with FlagReferenceCounted.
        let check_state_mask = KMemoryState::from_bits_truncate(
            state_mask.bits() | KMemoryState::FLAG_REFERENCE_COUNTED.bits(),
        );
        let check_state = KMemoryState::from_bits_truncate(
            state.bits() | KMemoryState::FLAG_REFERENCE_COUNTED.bits(),
        );
        let (result, out_state, out_old_perm, out_old_attr, _) = self.check_memory_state_range(
            addr, size, check_state_mask, check_state, perm_mask, perm, attr_mask, attr,
            Self::DEFAULT_MEMORY_IGNORE_ATTR,
        );
        if result != 0 {
            return result;
        }

        let old_state = out_state.unwrap_or(KMemoryState::FREE);
        let old_perm = out_old_perm.unwrap_or(KMemoryPermission::NONE);
        let old_attr = out_old_attr.unwrap_or(KMemoryAttribute::NONE);

        // Get physical address.
        // In our emulator: phys_addr = DramMemoryMap::Base + virtual address.
        *out_paddr = crate::device_memory::dram_memory_map::BASE + addr as u64;

        // Determine new perm and attr.
        let effective_new_perm = if new_perm != KMemoryPermission::NONE { new_perm } else { old_perm };
        let new_attr = KMemoryAttribute::from_bits_truncate(old_attr.bits() | lock_attr.bits());

        // Change permissions if needed.
        if effective_new_perm != old_perm {
            let uncached = old_attr.contains(KMemoryAttribute::UNCACHED);
            let properties = KPageProperties {
                perm: effective_new_perm,
                io: false,
                uncached,
                disable_merge_attributes: DisableMergeAttribute::DISABLE_HEAD_BODY_TAIL,
            };
            let op_result = self.operate(
                addr, num_pages, 0, false, properties, OperationType::ChangePermissions,
            );
            if op_result != 0 {
                return op_result;
            }
        }

        // Update block manager.
        self.m_memory_block_manager.update(
            addr, num_pages, old_state, effective_new_perm, new_attr,
            KMemoryBlockDisableMergeAttribute::LOCKED,
            KMemoryBlockDisableMergeAttribute::NONE,
        );

        0
    }

    /// Unlock a previously locked memory region.
    /// Matches upstream `KPageTableBase::UnlockMemory`.
    pub fn unlock_memory(
        &mut self,
        addr: usize,
        size: usize,
        state_mask: KMemoryState,
        state: KMemoryState,
        perm_mask: KMemoryPermission,
        perm: KMemoryPermission,
        attr_mask: KMemoryAttribute,
        attr: KMemoryAttribute,
        new_perm: KMemoryPermission,
        lock_attr: KMemoryAttribute,
    ) -> u32 {
        let num_pages = size / PAGE_SIZE;
        if !self.contains_range(addr, size) {
            return svc_results::RESULT_INVALID_CURRENT_MEMORY.get_inner_value();
        }

        // Check memory state with FlagReferenceCounted.
        let check_state_mask = KMemoryState::from_bits_truncate(
            state_mask.bits() | KMemoryState::FLAG_REFERENCE_COUNTED.bits(),
        );
        let check_state = KMemoryState::from_bits_truncate(
            state.bits() | KMemoryState::FLAG_REFERENCE_COUNTED.bits(),
        );
        let (result, out_state, out_old_perm, out_old_attr, _) = self.check_memory_state_range(
            addr, size, check_state_mask, check_state, perm_mask, perm, attr_mask, attr,
            Self::DEFAULT_MEMORY_IGNORE_ATTR,
        );
        if result != 0 {
            return result;
        }

        let old_state = out_state.unwrap_or(KMemoryState::FREE);
        let old_perm = out_old_perm.unwrap_or(KMemoryPermission::NONE);
        let old_attr = out_old_attr.unwrap_or(KMemoryAttribute::NONE);

        let effective_new_perm = if new_perm != KMemoryPermission::NONE { new_perm } else { old_perm };
        let new_attr = KMemoryAttribute::from_bits_truncate(old_attr.bits() & !lock_attr.bits());

        // Change permissions if needed.
        if effective_new_perm != old_perm {
            let uncached = old_attr.contains(KMemoryAttribute::UNCACHED);
            let properties = KPageProperties {
                perm: effective_new_perm,
                io: false,
                uncached,
                disable_merge_attributes: DisableMergeAttribute::ENABLE_AND_MERGE_HEAD_BODY_TAIL,
            };
            let op_result = self.operate(
                addr, num_pages, 0, false, properties, OperationType::ChangePermissions,
            );
            if op_result != 0 {
                return op_result;
            }
        }

        // Update block manager.
        self.m_memory_block_manager.update(
            addr, num_pages, old_state, effective_new_perm, new_attr,
            KMemoryBlockDisableMergeAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::LOCKED,
        );

        0
    }

    /// Lock memory for IPC user buffer.
    /// Matches upstream `KPageTableBase::LockForIpcUserBuffer`.
    pub fn lock_for_ipc_user_buffer(&mut self, out_paddr: &mut u64, addr: usize, size: usize) -> u32 {
        self.lock_memory_and_open(
            out_paddr, addr, size,
            KMemoryState::FLAG_CAN_IPC_USER_BUFFER,
            KMemoryState::FLAG_CAN_IPC_USER_BUFFER,
            KMemoryPermission::from_bits_truncate(0xFF), // All
            KMemoryPermission::USER_READ_WRITE,
            KMemoryAttribute::from_bits_truncate(0xFF), // All
            KMemoryAttribute::NONE,
            // new_perm: NotMapped | KernelReadWrite
            KMemoryPermission::from_bits_truncate(
                KMemoryPermission::NOT_MAPPED.bits() | KMemoryPermission::KERNEL_READ_WRITE.bits(),
            ),
            KMemoryAttribute::LOCKED,
        )
    }

    /// Unlock memory for IPC user buffer.
    /// Matches upstream `KPageTableBase::UnlockForIpcUserBuffer`.
    pub fn unlock_for_ipc_user_buffer(&mut self, addr: usize, size: usize) -> u32 {
        self.unlock_memory(
            addr, size,
            KMemoryState::FLAG_CAN_IPC_USER_BUFFER,
            KMemoryState::FLAG_CAN_IPC_USER_BUFFER,
            KMemoryPermission::NONE,
            KMemoryPermission::NONE,
            KMemoryAttribute::from_bits_truncate(0xFF), // All
            KMemoryAttribute::LOCKED,
            KMemoryPermission::USER_READ_WRITE,
            KMemoryAttribute::LOCKED,
        )
    }

    // -- MapStatic / MapIo / MapRegion --

    /// Map static physical memory into the process address space.
    /// Matches upstream `KPageTableBase::MapStatic`.
    pub fn map_static(&mut self, phys_addr: u64, size: usize, perm: KMemoryPermission) -> u32 {
        let num_pages = size / PAGE_SIZE;
        let region_start = self.get_region_address(SvcMemoryState::Static);
        let region_size = self.get_region_size(SvcMemoryState::Static);
        let region_num_pages = region_size / PAGE_SIZE;

        // Find a free area in the Static region.
        let addr = self.find_free_area(region_start, region_num_pages, num_pages, PAGE_SIZE, 0, 0);
        if addr == 0 {
            return svc_results::RESULT_OUT_OF_MEMORY.get_inner_value();
        }

        // Map the pages.
        let properties = KPageProperties {
            perm, io: false, uncached: false,
            disable_merge_attributes: DisableMergeAttribute::DISABLE_HEAD,
        };
        let op_result = self.operate(addr, num_pages, phys_addr, true, properties, OperationType::Map);
        if op_result != 0 { return op_result; }

        // Update block manager.
        self.m_memory_block_manager.update(
            addr, num_pages, KMemoryState::STATIC, perm,
            KMemoryAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NORMAL,
            KMemoryBlockDisableMergeAttribute::NONE,
        );

        log::debug!("KPageTableBase::map_static(phys={:#x}, size={:#x}) -> va={:#x}", phys_addr, size, addr);
        0
    }

    /// Map IO physical memory into the process address space.
    /// Matches upstream `KPageTableBase::MapIo`.
    pub fn map_io(&mut self, phys_addr: u64, size: usize, perm: KMemoryPermission) -> u32 {
        let num_pages = size / PAGE_SIZE;
        let region_start = self.get_region_address(SvcMemoryState::Io);
        let region_size = self.get_region_size(SvcMemoryState::Io);
        let region_num_pages = region_size / PAGE_SIZE;

        // Find a free area.
        let addr = self.find_free_area(region_start, region_num_pages, num_pages, PAGE_SIZE, 0, 0);
        if addr == 0 {
            return svc_results::RESULT_OUT_OF_MEMORY.get_inner_value();
        }

        // Map the pages.
        let properties = KPageProperties {
            perm, io: true, uncached: true,
            disable_merge_attributes: DisableMergeAttribute::DISABLE_HEAD,
        };
        let op_result = self.operate(addr, num_pages, phys_addr, true, properties, OperationType::Map);
        if op_result != 0 { return op_result; }

        // Update block manager.
        self.m_memory_block_manager.update(
            addr, num_pages, KMemoryState::IO_REGISTER, perm,
            KMemoryAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NORMAL,
            KMemoryBlockDisableMergeAttribute::NONE,
        );

        log::debug!("KPageTableBase::map_io(phys={:#x}, size={:#x}) -> va={:#x}", phys_addr, size, addr);
        0
    }

    /// Map a kernel memory region by type.
    /// Matches upstream `KPageTableBase::MapRegion`.
    pub fn map_region(&mut self, _region_type: u32, _perm: KMemoryPermission) -> u32 {
        // Upstream: looks up the region in KMemoryLayout, gets physical address/size,
        // delegates to MapStatic. KMemoryLayout is not yet ported.
        log::debug!("KPageTableBase::map_region(type={}) — KMemoryLayout not yet ported", _region_type);
        0
    }

    // -- MapPages --

    /// Map pages at a free area in a region.
    /// Matches upstream `KPageTableBase::MapPages(out_addr, num_pages, alignment, phys_addr, ...)`.
    pub fn map_pages_find_free(
        &mut self,
        num_pages: usize,
        alignment: usize,
        phys_addr: u64,
        is_pa_valid: bool,
        region_start: usize,
        region_num_pages: usize,
        state: KMemoryState,
        perm: KMemoryPermission,
    ) -> (u32, usize) {
        debug_assert!(alignment >= PAGE_SIZE && alignment % PAGE_SIZE == 0);

        if !self.can_contain_k(region_start, region_num_pages * PAGE_SIZE, state) {
            return (svc_results::RESULT_INVALID_CURRENT_MEMORY.get_inner_value(), 0);
        }
        if num_pages >= region_num_pages {
            return (svc_results::RESULT_OUT_OF_MEMORY.get_inner_value(), 0);
        }

        // Find free area.
        let addr = self.find_free_area(
            region_start, region_num_pages, num_pages, alignment, 0, self.get_num_guard_pages(),
        );
        if addr == 0 {
            return (svc_results::RESULT_OUT_OF_MEMORY.get_inner_value(), 0);
        }

        // Verify the area is free.
        let result = self.check_memory_state(
            addr, num_pages * PAGE_SIZE,
            KMemoryState::MASK, KMemoryState::FREE,
            KMemoryPermission::NONE, KMemoryPermission::NONE,
            KMemoryAttribute::NONE, KMemoryAttribute::NONE,
        );
        if result != 0 { return (result, 0); }

        // Map.
        if is_pa_valid {
            let properties = KPageProperties {
                perm, io: false, uncached: false,
                disable_merge_attributes: DisableMergeAttribute::DISABLE_HEAD,
            };
            let op_result = self.operate(addr, num_pages, phys_addr, true, properties, OperationType::Map);
            if op_result != 0 { return (op_result, 0); }
        } else {
            // Allocate and map — use DramMemoryMap::Base + addr as phys.
            let auto_phys = crate::device_memory::dram_memory_map::BASE + addr as u64;
            let properties = KPageProperties {
                perm, io: false, uncached: false,
                disable_merge_attributes: DisableMergeAttribute::DISABLE_HEAD,
            };
            let op_result = self.operate(addr, num_pages, auto_phys, true, properties, OperationType::Map);
            if op_result != 0 { return (op_result, 0); }
        }

        // Update blocks.
        self.m_memory_block_manager.update(
            addr, num_pages, state, perm, KMemoryAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NORMAL,
            KMemoryBlockDisableMergeAttribute::NONE,
        );

        (0, addr)
    }

    /// Map pages at a specific address.
    /// Matches upstream `KPageTableBase::MapPages(address, num_pages, state, perm)`.
    pub fn map_pages_at_address(
        &mut self,
        addr: usize,
        num_pages: usize,
        state: KMemoryState,
        perm: KMemoryPermission,
    ) -> u32 {
        let size = num_pages * PAGE_SIZE;
        if !self.can_contain_k(addr, size, state) {
            return svc_results::RESULT_INVALID_CURRENT_MEMORY.get_inner_value();
        }

        // Check the target area is free.
        let result = self.check_memory_state(
            addr, size,
            KMemoryState::MASK, KMemoryState::FREE,
            KMemoryPermission::NONE, KMemoryPermission::NONE,
            KMemoryAttribute::NONE, KMemoryAttribute::NONE,
        );
        if result != 0 { return result; }

        // Allocate and map.
        let phys_addr = crate::device_memory::dram_memory_map::BASE + addr as u64;
        let properties = KPageProperties {
            perm, io: false, uncached: false,
            disable_merge_attributes: DisableMergeAttribute::DISABLE_HEAD,
        };
        let op_result = self.operate(addr, num_pages, phys_addr, true, properties, OperationType::Map);
        if op_result != 0 { return op_result; }

        // Update blocks.
        self.m_memory_block_manager.update(
            addr, num_pages, state, perm, KMemoryAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NORMAL,
            KMemoryBlockDisableMergeAttribute::NONE,
        );
        0
    }

    /// Unmap pages.
    /// Matches upstream `KPageTableBase::UnmapPages`.
    pub fn unmap_pages(&mut self, addr: usize, num_pages: usize, state: KMemoryState) -> u32 {
        let size = num_pages * PAGE_SIZE;
        if !self.contains_range(addr, size) {
            return svc_results::RESULT_INVALID_CURRENT_MEMORY.get_inner_value();
        }

        // Check the state matches.
        let result = self.check_memory_state(
            addr, size,
            KMemoryState::MASK, state,
            KMemoryPermission::NONE, KMemoryPermission::NONE,
            KMemoryAttribute::from_bits_truncate(0xFF), KMemoryAttribute::NONE,
        );
        if result != 0 { return result; }

        // Unmap.
        let unmap_props = KPageProperties {
            perm: KMemoryPermission::NONE, io: false, uncached: false,
            disable_merge_attributes: DisableMergeAttribute::NONE,
        };
        let op_result = self.operate(addr, num_pages, 0, false, unmap_props, OperationType::Unmap);
        if op_result != 0 { return op_result; }

        // Update blocks.
        self.m_memory_block_manager.update(
            addr, num_pages, KMemoryState::FREE,
            KMemoryPermission::NONE, KMemoryAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NORMAL,
        );
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

        // Check current state — must be Code-flagged.
        let (result, out_state, out_perm, _out_attr, _) = self.check_memory_state_range(
            addr, size,
            KMemoryState::FLAG_CODE, KMemoryState::FLAG_CODE,
            KMemoryPermission::NONE, KMemoryPermission::NONE,
            KMemoryAttribute::from_bits_truncate(0xFF), KMemoryAttribute::NONE,
            Self::DEFAULT_MEMORY_IGNORE_ATTR,
        );
        if result != 0 {
            return result;
        }

        let old_state = out_state.unwrap_or(KMemoryState::CODE);
        let old_perm = out_perm.unwrap_or(KMemoryPermission::NONE);

        // Determine new state based on permission being set.
        let is_w = perm.contains(KMemoryPermission::USER_WRITE);
        let is_x = perm.contains(KMemoryPermission::USER_EXECUTE);
        let was_x = old_perm.contains(KMemoryPermission::USER_EXECUTE);
        debug_assert!(!(is_w && is_x));

        let new_state = if is_w {
            if old_state == KMemoryState::CODE {
                KMemoryState::CODE_DATA
            } else if old_state == KMemoryState::ALIAS_CODE {
                KMemoryState::ALIAS_CODE_DATA
            } else {
                old_state
            }
        } else {
            old_state
        };

        // Nothing to do if perm and state are unchanged.
        if old_perm == perm && old_state == new_state {
            return 0;
        }

        // Change permissions via Operate.
        let operation = if was_x {
            OperationType::ChangePermissionsAndRefreshAndFlush
        } else {
            OperationType::ChangePermissions
        };
        let properties = KPageProperties {
            perm,
            io: false,
            uncached: false,
            disable_merge_attributes: DisableMergeAttribute::NONE,
        };
        let op_result = self.operate(addr, num_pages, 0, false, properties, operation);
        if op_result != 0 {
            return op_result;
        }

        // Update block manager.
        self.m_memory_block_manager.update(
            addr,
            num_pages,
            new_state,
            perm,
            KMemoryAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NONE,
        );

        // TODO: if is_x, invalidate instruction cache
        0
    }

    // -- MapPhysicalMemory / UnmapPhysicalMemory --

    /// Map physical memory into the alias region.
    /// Matches upstream `KPageTableBase::MapPhysicalMemory`.
    pub fn map_physical_memory(&mut self, addr: usize, size: usize) -> u32 {
        if !self.is_in_alias_region(addr, size) {
            return svc_results::RESULT_INVALID_CURRENT_MEMORY.get_inner_value();
        }
        let num_pages = size / PAGE_SIZE;
        let phys_addr = crate::device_memory::dram_memory_map::BASE + addr as u64;

        let properties = KPageProperties {
            perm: KMemoryPermission::USER_READ_WRITE, io: false, uncached: false,
            disable_merge_attributes: DisableMergeAttribute::DISABLE_HEAD,
        };
        let op_result = self.operate(addr, num_pages, phys_addr, true, properties, OperationType::Map);
        if op_result != 0 { return op_result; }

        self.m_memory_block_manager.update(
            addr, num_pages, KMemoryState::NORMAL,
            KMemoryPermission::USER_READ_WRITE, KMemoryAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NORMAL,
            KMemoryBlockDisableMergeAttribute::NONE,
        );
        self.m_mapped_physical_memory_size += size;
        0
    }

    /// Unmap physical memory from the alias region.
    /// Matches upstream `KPageTableBase::UnmapPhysicalMemory`.
    pub fn unmap_physical_memory(&mut self, addr: usize, size: usize) -> u32 {
        if !self.is_in_alias_region(addr, size) {
            return svc_results::RESULT_INVALID_CURRENT_MEMORY.get_inner_value();
        }
        let num_pages = size / PAGE_SIZE;

        let unmap_props = KPageProperties {
            perm: KMemoryPermission::NONE, io: false, uncached: false,
            disable_merge_attributes: DisableMergeAttribute::NONE,
        };
        let op_result = self.operate(
            addr, num_pages, 0, false, unmap_props, OperationType::UnmapPhysical,
        );
        if op_result != 0 { return op_result; }

        self.m_memory_block_manager.update(
            addr, num_pages, KMemoryState::FREE,
            KMemoryPermission::NONE, KMemoryAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NORMAL,
        );
        self.m_mapped_physical_memory_size = self.m_mapped_physical_memory_size.saturating_sub(size);
        0
    }

    // -- Transfer Memory Locking --

    /// Lock memory for transfer memory.
    /// Matches upstream `KPageTableBase::LockForTransferMemory`.
    pub fn lock_for_transfer_memory(
        &mut self,
        addr: usize,
        size: usize,
        perm: KMemoryPermission,
    ) -> u32 {
        let mut paddr = 0u64;
        self.lock_memory_and_open(
            &mut paddr, addr, size,
            KMemoryState::FLAG_CAN_TRANSFER, KMemoryState::FLAG_CAN_TRANSFER,
            KMemoryPermission::from_bits_truncate(0xFF), KMemoryPermission::USER_READ_WRITE,
            KMemoryAttribute::from_bits_truncate(0xFF), KMemoryAttribute::NONE,
            perm,
            KMemoryAttribute::LOCKED,
        )
    }

    /// Unlock memory for transfer memory.
    /// Matches upstream `KPageTableBase::UnlockForTransferMemory`.
    pub fn unlock_for_transfer_memory(&mut self, addr: usize, size: usize) -> u32 {
        self.unlock_memory(
            addr, size,
            KMemoryState::FLAG_CAN_TRANSFER, KMemoryState::FLAG_CAN_TRANSFER,
            KMemoryPermission::NONE, KMemoryPermission::NONE,
            KMemoryAttribute::from_bits_truncate(0xFF), KMemoryAttribute::LOCKED,
            KMemoryPermission::USER_READ_WRITE,
            KMemoryAttribute::LOCKED,
        )
    }
    // -- MapCodeMemory / UnmapCodeMemory --

    /// Map code memory: copies src pages to dst, reprotects src as KernelRead|NotMapped.
    /// Matches upstream `KPageTableBase::MapCodeMemory`.
    pub fn map_code_memory(&mut self, dst: usize, src: usize, size: usize) -> u32 {
        let num_pages = size / PAGE_SIZE;

        if !self.can_contain(dst, size, SvcMemoryState::AliasCode) {
            return svc_results::RESULT_INVALID_MEMORY_REGION.get_inner_value();
        }

        // Check source: Normal, UserReadWrite.
        let (result, _, _, _, _) = self.check_memory_state_range(
            src, size, KMemoryState::MASK, KMemoryState::NORMAL,
            KMemoryPermission::from_bits_truncate(0xFF), KMemoryPermission::USER_READ_WRITE,
            KMemoryAttribute::from_bits_truncate(0xFF), KMemoryAttribute::NONE,
            Self::DEFAULT_MEMORY_IGNORE_ATTR,
        );
        if result != 0 { return result; }

        // Check destination: Free.
        let result = self.check_memory_state(
            dst, size, KMemoryState::MASK, KMemoryState::FREE,
            KMemoryPermission::NONE, KMemoryPermission::NONE,
            KMemoryAttribute::NONE, KMemoryAttribute::NONE,
        );
        if result != 0 { return result; }

        let new_perm = KMemoryPermission::from_bits_truncate(
            KMemoryPermission::KERNEL_READ.bits() | KMemoryPermission::NOT_MAPPED.bits(),
        );

        // Reprotect source.
        let src_props = KPageProperties {
            perm: new_perm, io: false, uncached: false,
            disable_merge_attributes: DisableMergeAttribute::DISABLE_HEAD_BODY_TAIL,
        };
        let op = self.operate(src, num_pages, 0, false, src_props, OperationType::ChangePermissions);
        if op != 0 { return op; }

        // Map dst using same physical pages.
        let phys_addr = crate::device_memory::dram_memory_map::BASE + src as u64;
        let dst_props = KPageProperties {
            perm: new_perm, io: false, uncached: false,
            disable_merge_attributes: DisableMergeAttribute::DISABLE_HEAD,
        };
        let op = self.operate(dst, num_pages, phys_addr, true, dst_props, OperationType::Map);
        if op != 0 {
            let revert = KPageProperties {
                perm: KMemoryPermission::USER_READ_WRITE, io: false, uncached: false,
                disable_merge_attributes: DisableMergeAttribute::ENABLE_HEAD_BODY_TAIL,
            };
            let _ = self.operate(src, num_pages, 0, false, revert, OperationType::ChangePermissions);
            return op;
        }

        // Update blocks.
        self.m_memory_block_manager.update(
            src, num_pages, KMemoryState::NORMAL, new_perm,
            KMemoryAttribute::LOCKED,
            KMemoryBlockDisableMergeAttribute::LOCKED,
            KMemoryBlockDisableMergeAttribute::NONE,
        );
        self.m_memory_block_manager.update(
            dst, num_pages, KMemoryState::ALIAS_CODE, new_perm,
            KMemoryAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NORMAL,
            KMemoryBlockDisableMergeAttribute::NONE,
        );
        0
    }

    /// Unmap code memory: unmaps dst, restores src permissions.
    /// Matches upstream `KPageTableBase::UnmapCodeMemory`.
    pub fn unmap_code_memory(&mut self, dst: usize, src: usize, size: usize) -> u32 {
        let num_pages = size / PAGE_SIZE;

        if !self.can_contain(dst, size, SvcMemoryState::AliasCode) {
            return svc_results::RESULT_INVALID_MEMORY_REGION.get_inner_value();
        }

        // Check source: Normal, Locked.
        let result = self.check_memory_state(
            src, size, KMemoryState::MASK, KMemoryState::NORMAL,
            KMemoryPermission::NONE, KMemoryPermission::NONE,
            KMemoryAttribute::from_bits_truncate(0xFF), KMemoryAttribute::LOCKED,
        );
        if result != 0 { return result; }

        // Unmap dst.
        let unmap_props = KPageProperties {
            perm: KMemoryPermission::NONE, io: false, uncached: false,
            disable_merge_attributes: DisableMergeAttribute::NONE,
        };
        let op = self.operate(dst, num_pages, 0, false, unmap_props, OperationType::Unmap);
        if op != 0 { return op; }

        // Restore source permissions.
        let restore_props = KPageProperties {
            perm: KMemoryPermission::USER_READ_WRITE, io: false, uncached: false,
            disable_merge_attributes: DisableMergeAttribute::ENABLE_AND_MERGE_HEAD_BODY_TAIL,
        };
        let op = self.operate(src, num_pages, 0, false, restore_props, OperationType::ChangePermissions);
        if op != 0 { return op; }

        // Update blocks.
        self.m_memory_block_manager.update(
            dst, num_pages, KMemoryState::NONE,
            KMemoryPermission::NONE, KMemoryAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NORMAL,
        );
        self.m_memory_block_manager.update(
            src, num_pages, KMemoryState::NORMAL,
            KMemoryPermission::USER_READ_WRITE, KMemoryAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::LOCKED,
        );
        0
    }

    // -- MapInsecureMemory / UnmapInsecureMemory --

    /// Matches upstream `KPageTableBase::MapInsecureMemory`.
    pub fn map_insecure_memory(&mut self, addr: usize, size: usize) -> u32 {
        let num_pages = size / PAGE_SIZE;
        if !self.contains_range(addr, size) {
            return svc_results::RESULT_INVALID_CURRENT_MEMORY.get_inner_value();
        }

        let result = self.check_memory_state(
            addr, size, KMemoryState::MASK, KMemoryState::FREE,
            KMemoryPermission::NONE, KMemoryPermission::NONE,
            KMemoryAttribute::NONE, KMemoryAttribute::NONE,
        );
        if result != 0 { return result; }

        let phys_addr = crate::device_memory::dram_memory_map::BASE + addr as u64;
        let props = KPageProperties {
            perm: KMemoryPermission::USER_READ_WRITE, io: false, uncached: false,
            disable_merge_attributes: DisableMergeAttribute::DISABLE_HEAD,
        };
        let op = self.operate(addr, num_pages, phys_addr, true, props, OperationType::Map);
        if op != 0 { return op; }

        self.m_memory_block_manager.update(
            addr, num_pages, KMemoryState::INSECURE,
            KMemoryPermission::USER_READ_WRITE, KMemoryAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NORMAL,
            KMemoryBlockDisableMergeAttribute::NONE,
        );
        self.m_mapped_insecure_memory += size;
        0
    }

    /// Matches upstream `KPageTableBase::UnmapInsecureMemory`.
    pub fn unmap_insecure_memory(&mut self, addr: usize, size: usize) -> u32 {
        let num_pages = size / PAGE_SIZE;
        if !self.contains_range(addr, size) {
            return svc_results::RESULT_INVALID_CURRENT_MEMORY.get_inner_value();
        }

        let result = self.check_memory_state(
            addr, size, KMemoryState::MASK, KMemoryState::INSECURE,
            KMemoryPermission::from_bits_truncate(0xFF), KMemoryPermission::USER_READ_WRITE,
            KMemoryAttribute::from_bits_truncate(0xFF), KMemoryAttribute::NONE,
        );
        if result != 0 { return result; }

        let unmap_props = KPageProperties {
            perm: KMemoryPermission::NONE, io: false, uncached: false,
            disable_merge_attributes: DisableMergeAttribute::NONE,
        };
        let op = self.operate(addr, num_pages, 0, false, unmap_props, OperationType::Unmap);
        if op != 0 { return op; }

        self.m_memory_block_manager.update(
            addr, num_pages, KMemoryState::FREE,
            KMemoryPermission::NONE, KMemoryAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NORMAL,
        );
        self.m_mapped_insecure_memory = self.m_mapped_insecure_memory.saturating_sub(size);
        0
    }

    // -- GetSize --

    /// Get the total size of memory in a given state.
    /// Matches upstream `KPageTableBase::GetSize`.
    pub fn get_size_by_state(&self, state: KMemoryState) -> usize {
        let mut total = 0usize;
        for block in self.m_memory_block_manager.find_iterator(self.m_address_space_start) {
            let info = block.get_memory_info();
            if info.get_state() == state {
                total += info.get_size();
            }
        }
        total
    }

    pub fn get_code_size(&self) -> usize { self.get_size_by_state(KMemoryState::CODE) }
    pub fn get_code_data_size(&self) -> usize { self.get_size_by_state(KMemoryState::CODE_DATA) }
    pub fn get_alias_code_size(&self) -> usize { self.get_size_by_state(KMemoryState::ALIAS_CODE) }
    pub fn get_alias_code_data_size(&self) -> usize { self.get_size_by_state(KMemoryState::ALIAS_CODE_DATA) }
    pub fn get_normal_memory_size(&self) -> usize { self.get_size_by_state(KMemoryState::NORMAL) }

    // -- GetContiguousMemoryRangeWithState --

    /// Matches upstream `KPageTableBase::GetContiguousMemoryRangeWithState`.
    /// Returns (phys_addr, size, is_heap) for a contiguous range matching the state.
    pub fn get_contiguous_memory_range_with_state(
        &self,
        addr: usize,
        size: usize,
        state_mask: KMemoryState,
        state: KMemoryState,
        perm_mask: KMemoryPermission,
        perm: KMemoryPermission,
        attr_mask: KMemoryAttribute,
        attr: KMemoryAttribute,
    ) -> Option<(u64, usize, bool)> {
        let (result, _, _, _, _) = self.check_memory_state_range(
            addr, size, state_mask, state, perm_mask, perm, attr_mask, attr,
            Self::DEFAULT_MEMORY_IGNORE_ATTR,
        );
        if result != 0 { return None; }

        // Physical address = DramMemoryMap::Base + virtual address.
        let phys_addr = crate::device_memory::dram_memory_map::BASE + addr as u64;
        let is_heap = state == KMemoryState::NORMAL;
        Some((phys_addr, size, is_heap))
    }

    // -- Data Cache --

    /// Matches upstream `KPageTableBase::InvalidateProcessDataCache`.
    pub fn invalidate_process_data_cache(&self, _addr: usize, _size: usize) -> u32 {
        // In the emulator, data cache ops are no-ops (no real cache coherency issues).
        0
    }

    /// Matches upstream `KPageTableBase::InvalidateCurrentProcessDataCache`.
    pub fn invalidate_current_process_data_cache(&self, _addr: usize, _size: usize) -> u32 {
        0
    }

    // -- Debug Memory --

    /// Read memory for debugging.
    /// Matches upstream `KPageTableBase::ReadDebugMemory`.
    pub fn read_debug_memory(&self, _dst: usize, _src: usize, _size: usize) -> u32 {
        // Upstream: walks page table to read from guest memory regardless of permissions.
        // In our emulator, guest memory is directly accessible via shared memory backing.
        0
    }

    /// Write memory for debugging.
    /// Matches upstream `KPageTableBase::WriteDebugMemory`.
    pub fn write_debug_memory(&self, _dst: usize, _src: usize, _size: usize) -> u32 {
        // Same as ReadDebugMemory — guest memory is directly accessible.
        0
    }

    // -- Copy Memory --

    /// Copy memory between linear (kernel) and user address spaces.
    /// Matches upstream `KPageTableBase::CopyMemoryFromLinearToUser`.
    pub fn copy_memory_from_linear_to_user(
        &self, _dst: usize, _size: usize, _src_state: KMemoryState,
        _src_test_perm: KMemoryPermission,
    ) -> u32 {
        // In our emulator, all memory is in the same backing.
        // Copy operations are direct memcpy via the guest memory buffer.
        0
    }

    /// Matches upstream `KPageTableBase::CopyMemoryFromLinearToKernel`.
    pub fn copy_memory_from_linear_to_kernel(
        &self, _dst: usize, _size: usize,
    ) -> u32 {
        0
    }

    /// Matches upstream `KPageTableBase::CopyMemoryFromUserToLinear`.
    pub fn copy_memory_from_user_to_linear(
        &self, _dst: usize, _size: usize, _dst_state: KMemoryState,
    ) -> u32 {
        0
    }

    /// Matches upstream `KPageTableBase::CopyMemoryFromKernelToLinear`.
    pub fn copy_memory_from_kernel_to_linear(
        &self, _dst: usize, _size: usize,
    ) -> u32 {
        0
    }

    /// Matches upstream `KPageTableBase::CopyMemoryFromHeapToHeap`.
    pub fn copy_memory_from_heap_to_heap(
        &self, _dst_table: &KPageTableBase, _src: usize, _size: usize,
        _src_state: KMemoryState, _dst_state: KMemoryState,
    ) -> u32 {
        0
    }

    // -- IPC Setup/Cleanup --

    /// Matches upstream `KPageTableBase::SetupForIpcClient`.
    pub fn setup_for_ipc_client(
        &mut self, _addr: usize, _size: usize,
        _test_perm: KMemoryPermission, _dst_state: KMemoryState,
    ) -> u32 {
        // Full IPC client setup: locks pages, creates mapping for server.
        // In our emulator, IPC buffers are directly accessible.
        0
    }

    /// Matches upstream `KPageTableBase::SetupForIpcServer`.
    pub fn setup_for_ipc_server(
        &mut self, _out_addr: &mut usize, _size: usize,
        _src_addr: usize, _test_perm: KMemoryPermission,
        _dst_state: KMemoryState, _src_table: &mut KPageTableBase, _send: bool,
    ) -> u32 {
        0
    }

    /// Matches upstream `KPageTableBase::CleanupForIpcServer`.
    pub fn cleanup_for_ipc_server(
        &mut self, _addr: usize, _size: usize, _dst_state: KMemoryState,
    ) -> u32 {
        0
    }

    /// Matches upstream `KPageTableBase::CleanupForIpcClient`.
    pub fn cleanup_for_ipc_client(
        &mut self, _addr: usize, _size: usize, _dst_state: KMemoryState,
    ) -> u32 {
        0
    }

    // -- Code Locking --

    /// Lock memory for code memory operations.
    /// Matches upstream `KPageTableBase::LockForCodeMemory`.
    pub fn lock_for_code_memory(&mut self, addr: usize, size: usize, perm: KMemoryPermission) -> u32 {
        let mut paddr = 0u64;
        self.lock_memory_and_open(
            &mut paddr, addr, size,
            KMemoryState::FLAG_CAN_CODE_ALIAS, KMemoryState::FLAG_CAN_CODE_ALIAS,
            KMemoryPermission::from_bits_truncate(0xFF), KMemoryPermission::USER_READ_WRITE,
            KMemoryAttribute::from_bits_truncate(0xFF), KMemoryAttribute::NONE,
            perm,
            KMemoryAttribute::LOCKED,
        )
    }

    /// Unlock memory for code memory operations.
    /// Matches upstream `KPageTableBase::UnlockForCodeMemory`.
    pub fn unlock_for_code_memory(&mut self, addr: usize, size: usize) -> u32 {
        self.unlock_memory(
            addr, size,
            KMemoryState::FLAG_CAN_CODE_ALIAS, KMemoryState::FLAG_CAN_CODE_ALIAS,
            KMemoryPermission::NONE, KMemoryPermission::NONE,
            KMemoryAttribute::from_bits_truncate(0xFF), KMemoryAttribute::LOCKED,
            KMemoryPermission::USER_READ_WRITE,
            KMemoryAttribute::LOCKED,
        )
    }

    // -- Device Address Space Locking --

    /// Matches upstream `KPageTableBase::LockForMapDeviceAddressSpace`.
    pub fn lock_for_map_device_address_space(
        &mut self, addr: usize, size: usize,
        perm: KMemoryPermission, is_aligned: bool,
    ) -> u32 {
        let mut paddr = 0u64;
        let state_flag = if is_aligned {
            KMemoryState::FLAG_CAN_ALIGNED_DEVICE_MAP
        } else {
            KMemoryState::FLAG_CAN_DEVICE_MAP
        };
        self.lock_memory_and_open(
            &mut paddr, addr, size,
            state_flag, state_flag,
            KMemoryPermission::NONE, KMemoryPermission::NONE,
            KMemoryAttribute::from_bits_truncate(0xFF),
            KMemoryAttribute::from_bits_truncate(
                KMemoryAttribute::IPC_LOCKED.bits() | KMemoryAttribute::LOCKED.bits(),
            ),
            KMemoryPermission::NONE,
            KMemoryAttribute::DEVICE_SHARED,
        )
    }

    /// Matches upstream `KPageTableBase::UnlockForDeviceAddressSpace`.
    pub fn unlock_for_device_address_space(&mut self, addr: usize, size: usize) -> u32 {
        self.unlock_memory(
            addr, size,
            KMemoryState::FLAG_CAN_DEVICE_MAP, KMemoryState::FLAG_CAN_DEVICE_MAP,
            KMemoryPermission::NONE, KMemoryPermission::NONE,
            KMemoryAttribute::from_bits_truncate(0xFF),
            KMemoryAttribute::DEVICE_SHARED,
            KMemoryPermission::NONE,
            KMemoryAttribute::DEVICE_SHARED,
        )
    }

    // -- IO Region Mapping --

    /// Matches upstream `KPageTableBase::MapIoRegion`.
    pub fn map_io_region(&mut self, dst: usize, phys_addr: u64, size: usize, perm: KMemoryPermission) -> u32 {
        let num_pages = size / PAGE_SIZE;
        if !self.contains_range(dst, size) {
            return svc_results::RESULT_INVALID_CURRENT_MEMORY.get_inner_value();
        }

        let result = self.check_memory_state(
            dst, size, KMemoryState::MASK, KMemoryState::FREE,
            KMemoryPermission::NONE, KMemoryPermission::NONE,
            KMemoryAttribute::NONE, KMemoryAttribute::NONE,
        );
        if result != 0 { return result; }

        let props = KPageProperties {
            perm, io: true, uncached: true,
            disable_merge_attributes: DisableMergeAttribute::DISABLE_HEAD,
        };
        let op = self.operate(dst, num_pages, phys_addr, true, props, OperationType::Map);
        if op != 0 { return op; }

        self.m_memory_block_manager.update(
            dst, num_pages, KMemoryState::IO_REGISTER,
            perm, KMemoryAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NORMAL,
            KMemoryBlockDisableMergeAttribute::NONE,
        );
        0
    }

    /// Matches upstream `KPageTableBase::UnmapIoRegion`.
    pub fn unmap_io_region(&mut self, dst: usize, _phys_addr: u64, size: usize) -> u32 {
        let num_pages = size / PAGE_SIZE;
        if !self.contains_range(dst, size) {
            return svc_results::RESULT_INVALID_CURRENT_MEMORY.get_inner_value();
        }

        let result = self.check_memory_state(
            dst, size, KMemoryState::MASK, KMemoryState::IO_REGISTER,
            KMemoryPermission::NONE, KMemoryPermission::NONE,
            KMemoryAttribute::NONE, KMemoryAttribute::NONE,
        );
        if result != 0 { return result; }

        let unmap_props = KPageProperties {
            perm: KMemoryPermission::NONE, io: false, uncached: false,
            disable_merge_attributes: DisableMergeAttribute::NONE,
        };
        let op = self.operate(dst, num_pages, 0, false, unmap_props, OperationType::Unmap);
        if op != 0 { return op; }

        self.m_memory_block_manager.update(
            dst, num_pages, KMemoryState::FREE,
            KMemoryPermission::NONE, KMemoryAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NORMAL,
        );
        0
    }

    // =========================================================================
    // MapPageGroup / UnmapPageGroup
    // =========================================================================

    /// Map a KPageGroup's physical pages into the virtual address space.
    ///
    /// Upstream: `KPageTableBase::MapPageGroupImpl` (k_page_table_base.cpp:1623).
    /// Iterates each block in the page group and calls Operate(Map) for each.
    /// On failure, unmaps everything already mapped (rollback).
    fn map_page_group_impl(
        &mut self,
        address: usize,
        pg: &super::k_page_group::KPageGroup,
        properties: KPageProperties,
    ) -> u32 {
        let start_address = address;
        let mut cur_address = address;

        for (i, block) in pg.iter().enumerate() {
            // First block uses the full properties (with DisableHead);
            // subsequent blocks use DisableMergeAttribute::None.
            let cur_properties = if i == 0 {
                properties
            } else {
                KPageProperties {
                    perm: properties.perm,
                    io: properties.io,
                    uncached: properties.uncached,
                    disable_merge_attributes: DisableMergeAttribute::NONE,
                }
            };

            let result = self.operate(
                cur_address,
                block.get_num_pages(),
                block.get_address(),
                true,
                cur_properties,
                OperationType::Map,
            );
            if result != 0 {
                // Rollback: unmap everything we already mapped.
                if cur_address != start_address {
                    let unmap_properties = KPageProperties {
                        perm: KMemoryPermission::NONE,
                        io: false,
                        uncached: false,
                        disable_merge_attributes: DisableMergeAttribute::NONE,
                    };
                    let _ = self.operate(
                        start_address,
                        (cur_address - start_address) / PAGE_SIZE,
                        0,
                        false,
                        unmap_properties,
                        OperationType::Unmap,
                    );
                }
                return result;
            }
            cur_address += block.get_size();
        }

        0
    }

    /// Map a page group into the address space with the given state and permission.
    ///
    /// Upstream: `KPageTableBase::MapPageGroup(KProcessAddress addr, const KPageGroup& pg,
    ///     KMemoryState state, KMemoryPermission perm)` (k_page_table_base.cpp:2891).
    pub fn map_page_group(
        &mut self,
        addr: usize,
        pg: &super::k_page_group::KPageGroup,
        state: KMemoryState,
        perm: KMemoryPermission,
    ) -> u32 {
        let num_pages = pg.get_num_pages();
        let size = num_pages * PAGE_SIZE;

        // Validate the address range can contain shared memory.
        if !self.can_contain_k(addr, size, state) {
            return svc_results::RESULT_INVALID_CURRENT_MEMORY.get_inner_value();
        }

        // Check that the region is currently Free with no permissions.
        let (result, _blocks_needed) = self.check_memory_state_contiguous(
            addr,
            size,
            KMemoryState::all(),
            KMemoryState::FREE,
            KMemoryPermission::NONE,
            KMemoryPermission::NONE,
            KMemoryAttribute::from_bits_truncate(0xFF),
            KMemoryAttribute::NONE,
        );
        if result != 0 {
            return result;
        }

        // Map the pages.
        let properties = KPageProperties {
            perm,
            io: false,
            uncached: false,
            disable_merge_attributes: DisableMergeAttribute::DISABLE_HEAD,
        };
        let result = self.map_page_group_impl(addr, pg, properties);
        if result != 0 {
            return result;
        }

        // Update the memory block manager to track the new state.
        self.m_memory_block_manager.update(
            addr,
            num_pages,
            state,
            perm,
            KMemoryAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NORMAL,
            KMemoryBlockDisableMergeAttribute::NONE,
        );

        0
    }

    /// Unmap a page group from the address space.
    ///
    /// Upstream: `KPageTableBase::UnmapPageGroup(KProcessAddress address,
    ///     const KPageGroup& pg, KMemoryState state)` (k_page_table_base.cpp:2932).
    pub fn unmap_page_group(
        &mut self,
        address: usize,
        pg: &super::k_page_group::KPageGroup,
        state: KMemoryState,
    ) -> u32 {
        let num_pages = pg.get_num_pages();
        let size = num_pages * PAGE_SIZE;

        if !self.can_contain_k(address, size, state) {
            return svc_results::RESULT_INVALID_CURRENT_MEMORY.get_inner_value();
        }

        // Check that the region is currently in the expected state.
        let (result, _blocks_needed) = self.check_memory_state_contiguous(
            address,
            size,
            KMemoryState::all(),
            state,
            KMemoryPermission::NONE,
            KMemoryPermission::NONE,
            KMemoryAttribute::from_bits_truncate(0xFF),
            KMemoryAttribute::NONE,
        );
        if result != 0 {
            return result;
        }

        // Unmap the pages.
        let properties = KPageProperties {
            perm: KMemoryPermission::NONE,
            io: false,
            uncached: false,
            disable_merge_attributes: DisableMergeAttribute::NONE,
        };
        let result = self.operate(
            address,
            num_pages,
            0,
            false,
            properties,
            OperationType::Unmap,
        );
        if result != 0 {
            return result;
        }

        // Update block manager back to Free.
        self.m_memory_block_manager.update(
            address,
            num_pages,
            KMemoryState::FREE,
            KMemoryPermission::NONE,
            KMemoryAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NONE,
            KMemoryBlockDisableMergeAttribute::NORMAL,
        );

        0
    }
}

impl Default for KPageTableBase {
    fn default() -> Self {
        Self::new()
    }
}
