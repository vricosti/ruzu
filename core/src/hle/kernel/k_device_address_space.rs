//! Port of zuyu/src/core/hle/kernel/k_device_address_space.h/.cpp
//! Status: COMPLET (stub — runtime dependencies not yet available)
//! Derniere synchro: 2026-03-11
//!
//! KDeviceAddressSpace: kernel object for managing device address space
//! mappings. Full implementation requires KDevicePageTable which is not
//! yet ported.

use crate::hle::kernel::k_memory_block::{convert_to_k_memory_permission, SvcMemoryPermission};
use crate::hle::kernel::k_process_page_table::KProcessPageTable;
use crate::hle::kernel::svc::svc_results::{
    RESULT_INVALID_COMBINATION, RESULT_INVALID_CURRENT_MEMORY, RESULT_INVALID_ENUM_VALUE,
};
use crate::hle::kernel::svc::svc_types::{
    DeviceName, MapDeviceAddressSpaceOption, MemoryPermission,
};
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// KDeviceAddressSpace: manages a device's virtual address space for
/// DMA mappings. Upstream inherits from
/// KAutoObjectWithSlabHeapAndContainer<KDeviceAddressSpace, KAutoObjectWithList>.
#[derive(Clone)]
pub struct KDeviceAddressSpace {
    // m_lock: KLightLock,
    // m_table: KDevicePageTable,
    m_space_address: u64,
    m_space_size: u64,
    m_is_initialized: bool,
}

impl KDeviceAddressSpace {
    pub fn new() -> Self {
        Self {
            m_space_address: 0,
            m_space_size: 0,
            m_is_initialized: false,
        }
    }

    /// Static initialization (forwards to KDevicePageTable::Initialize).
    pub fn initialize_static() {
        // KDevicePageTable::Initialize() — not yet ported.
    }

    /// Initialize with the given address range.
    pub fn initialize(&mut self, address: u64, size: u64) -> ResultCode {
        self.m_space_address = address;
        self.m_space_size = size;
        self.m_is_initialized = true;
        ResultCode::new(0)
    }

    pub fn finalize(&mut self) {
        // m_table.Finalize() — not yet ported.
    }

    pub fn is_initialized(&self) -> bool {
        self.m_is_initialized
    }

    pub fn post_destroy(_arg: usize) {}

    pub fn attach(&self, _device_name: DeviceName) -> ResultCode {
        // Upstream: m_table.Attach(device_name, m_space_address, m_space_size)
        // NOTE: Also commented out in upstream C++ — returns R_SUCCEED() without doing work.
        ResultCode::new(0)
    }

    pub fn detach(&self, _device_name: DeviceName) -> ResultCode {
        // Upstream: m_table.Detach(device_name)
        // NOTE: Also commented out in upstream C++ — returns R_SUCCEED() without doing work.
        ResultCode::new(0)
    }

    pub fn map_by_force(
        &self,
        page_table: &mut KProcessPageTable,
        process_address: u64,
        size: usize,
        device_address: u64,
        option: u32,
    ) -> ResultCode {
        self.map(
            page_table,
            process_address,
            size,
            device_address,
            option,
            false,
        )
    }

    pub fn map_aligned(
        &self,
        page_table: &mut KProcessPageTable,
        process_address: u64,
        size: usize,
        device_address: u64,
        option: u32,
    ) -> ResultCode {
        self.map(
            page_table,
            process_address,
            size,
            device_address,
            option,
            true,
        )
    }

    /// Unmap a device address range.
    /// Port of upstream `KDeviceAddressSpace::Unmap`.
    /// Upstream validates address range, locks pages, then calls m_table.Unmap
    /// (currently commented out in upstream too).
    pub fn unmap(
        &self,
        page_table: &mut KProcessPageTable,
        process_address: u64,
        size: usize,
        device_address: u64,
    ) -> ResultCode {
        // Validate address range.
        if !(self.m_space_address <= device_address
            && device_address + size as u64 - 1 <= self.m_space_address + self.m_space_size - 1)
        {
            return RESULT_INVALID_CURRENT_MEMORY;
        }

        let lock_result = page_table
            .get_base_mut()
            .lock_for_unmap_device_address_space(process_address as usize, size, true);
        if lock_result != RESULT_SUCCESS.get_inner_value() {
            return ResultCode::new(lock_result);
        }

        let unlock_result = page_table
            .get_base_mut()
            .unlock_for_device_address_space(process_address as usize, size);
        debug_assert_eq!(unlock_result, RESULT_SUCCESS.get_inner_value());
        if unlock_result != RESULT_SUCCESS.get_inner_value() {
            return ResultCode::new(unlock_result);
        }

        RESULT_SUCCESS
    }

    /// Map a device address range.
    /// Port of upstream `KDeviceAddressSpace::Map`.
    /// Upstream validates address range, decodes options, locks pages, then calls
    /// m_table.Map (currently commented out in upstream too).
    fn map(
        &self,
        page_table: &mut KProcessPageTable,
        process_address: u64,
        size: usize,
        device_address: u64,
        option: u32,
        is_aligned: bool,
    ) -> ResultCode {
        // Validate address range.
        if !(self.m_space_address <= device_address
            && device_address + size as u64 - 1 <= self.m_space_address + self.m_space_size - 1)
        {
            return RESULT_INVALID_CURRENT_MEMORY;
        }

        let option_pack = MapDeviceAddressSpaceOption { raw: option };
        let device_perm = option_pack.permission();
        let flags = option_pack.flags();
        let reserved = option_pack.reserved();

        if flags != 0 {
            return RESULT_INVALID_ENUM_VALUE;
        }
        if reserved != 0 {
            return RESULT_INVALID_ENUM_VALUE;
        }

        let (lock_result, is_io) = page_table.get_base_mut().lock_for_map_device_address_space(
            process_address as usize,
            size,
            to_kernel_device_memory_permission(device_perm),
            is_aligned,
            true,
        );
        if lock_result != RESULT_SUCCESS.get_inner_value() {
            return ResultCode::new(lock_result);
        }
        if is_io && (flags & 1) != 0 {
            let unlock_result = page_table
                .get_base_mut()
                .unlock_for_device_address_space(process_address as usize, size);
            debug_assert_eq!(unlock_result, RESULT_SUCCESS.get_inner_value());
            return RESULT_INVALID_COMBINATION;
        }

        RESULT_SUCCESS
    }
}

fn to_kernel_device_memory_permission(
    perm: MemoryPermission,
) -> crate::hle::kernel::k_memory_block::KMemoryPermission {
    let bits = match perm {
        MemoryPermission::None => 0,
        MemoryPermission::Read => SvcMemoryPermission::READ.bits(),
        MemoryPermission::Write => SvcMemoryPermission::WRITE.bits(),
        MemoryPermission::ReadWrite => {
            SvcMemoryPermission::READ.bits() | SvcMemoryPermission::WRITE.bits()
        }
        MemoryPermission::Execute => SvcMemoryPermission::EXECUTE.bits(),
        MemoryPermission::ReadExecute => {
            SvcMemoryPermission::READ.bits() | SvcMemoryPermission::EXECUTE.bits()
        }
        MemoryPermission::DontCare => 0,
    };
    convert_to_k_memory_permission(SvcMemoryPermission::from_bits_truncate(bits))
}

impl Default for KDeviceAddressSpace {
    fn default() -> Self {
        Self::new()
    }
}
