//! Port of zuyu/src/core/hle/kernel/k_device_address_space.h/.cpp
//! Status: COMPLET (stub — runtime dependencies not yet available)
//! Derniere synchro: 2026-03-11
//!
//! KDeviceAddressSpace: kernel object for managing device address space
//! mappings. Full implementation requires KDevicePageTable which is not
//! yet ported.

use crate::hle::result::ResultCode;

/// KDeviceAddressSpace: manages a device's virtual address space for
/// DMA mappings. Upstream inherits from
/// KAutoObjectWithSlabHeapAndContainer<KDeviceAddressSpace, KAutoObjectWithList>.
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

    pub fn attach(&self, _device_name: u32) -> ResultCode {
        // Upstream: m_table.Attach(device_name, m_space_address, m_space_size)
        // NOTE: Also commented out in upstream C++ — returns R_SUCCEED() without doing work.
        ResultCode::new(0)
    }

    pub fn detach(&self, _device_name: u32) -> ResultCode {
        // Upstream: m_table.Detach(device_name)
        // NOTE: Also commented out in upstream C++ — returns R_SUCCEED() without doing work.
        ResultCode::new(0)
    }

    pub fn map_by_force(
        &self,
        _process_address: u64,
        _size: usize,
        _device_address: u64,
        _option: u32,
    ) -> ResultCode {
        self.map(_process_address, _size, _device_address, _option, false)
    }

    pub fn map_aligned(
        &self,
        _process_address: u64,
        _size: usize,
        _device_address: u64,
        _option: u32,
    ) -> ResultCode {
        self.map(_process_address, _size, _device_address, _option, true)
    }

    /// Unmap a device address range.
    /// Port of upstream `KDeviceAddressSpace::Unmap`.
    /// Upstream validates address range, locks pages, then calls m_table.Unmap
    /// (currently commented out in upstream too).
    pub fn unmap(&self, _process_address: u64, size: usize, device_address: u64) -> ResultCode {
        // Validate address range.
        if !(self.m_space_address <= device_address
            && device_address + size as u64 - 1 <= self.m_space_address + self.m_space_size - 1)
        {
            return crate::hle::kernel::svc::svc_results::RESULT_INVALID_CURRENT_MEMORY;
        }
        // Upstream: m_table.Unmap — also commented out in upstream C++.
        ResultCode::new(0)
    }

    /// Map a device address range.
    /// Port of upstream `KDeviceAddressSpace::Map`.
    /// Upstream validates address range, decodes options, locks pages, then calls
    /// m_table.Map (currently commented out in upstream too).
    fn map(
        &self,
        _process_address: u64,
        size: usize,
        device_address: u64,
        _option: u32,
        _is_aligned: bool,
    ) -> ResultCode {
        // Validate address range.
        if !(self.m_space_address <= device_address
            && device_address + size as u64 - 1 <= self.m_space_address + self.m_space_size - 1)
        {
            return crate::hle::kernel::svc::svc_results::RESULT_INVALID_CURRENT_MEMORY;
        }
        // Upstream: m_table.Map — also commented out in upstream C++.
        ResultCode::new(0)
    }
}

impl Default for KDeviceAddressSpace {
    fn default() -> Self {
        Self::new()
    }
}
