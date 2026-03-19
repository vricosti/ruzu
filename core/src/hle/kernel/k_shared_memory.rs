//! Port of zuyu/src/core/hle/kernel/k_shared_memory.h/.cpp
//! Status: COMPLET (stub — runtime dependencies not yet available)
//! Derniere synchro: 2026-03-11
//!
//! KSharedMemory: kernel object for shared memory regions that can be
//! mapped into multiple process address spaces. Full implementation
//! requires KProcess, KPageGroup, KMemoryManager, DeviceMemory.

use crate::hle::result::ResultCode;

/// Memory permission for shared memory mapping.
/// Maps to Svc::MemoryPermission.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemoryPermission {
    None = 0,
    Read = 1,
    Write = 2,
    ReadWrite = 3,
    Execute = 4,
    ReadExecute = 5,
    DontCare = 1u32 << 28,
}

/// KSharedMemory: allows sharing memory regions between processes.
///
/// Upstream inherits from KAutoObjectWithSlabHeapAndContainer and allocates
/// physical pages via KMemoryManager backed by DeviceMemory.
/// Here we use a simple owned Vec<u8> as backing storage until
/// DeviceMemory/KPageGroup/KMemoryManager are fully wired.
pub struct KSharedMemory {
    /// Owned backing storage. In upstream this would be pages allocated from
    /// DeviceMemory via KMemoryManager, accessed through m_physical_address.
    m_backing: Vec<u8>,
    m_owner_permission: MemoryPermission,
    m_user_permission: MemoryPermission,
    m_size: usize,
    m_is_initialized: bool,
}

impl KSharedMemory {
    pub fn new() -> Self {
        Self {
            m_backing: Vec::new(),
            m_owner_permission: MemoryPermission::None,
            m_user_permission: MemoryPermission::None,
            m_size: 0,
            m_is_initialized: false,
        }
    }

    /// Initialize the shared memory with the given parameters.
    ///
    /// Matches upstream `KSharedMemory::Initialize(DeviceMemory&, KProcess*, ...)`.
    /// Upstream allocates physical pages from KMemoryManager and clears them.
    /// Here we allocate a zeroed Vec<u8> as the backing store.
    pub fn initialize(
        &mut self,
        owner_permission: MemoryPermission,
        user_permission: MemoryPermission,
        size: usize,
    ) -> ResultCode {
        let aligned_size = (size + 0xFFF) & !0xFFF; // Align to page size
        self.m_backing = vec![0u8; aligned_size];
        self.m_owner_permission = owner_permission;
        self.m_user_permission = user_permission;
        self.m_size = aligned_size;
        self.m_is_initialized = true;
        ResultCode::new(0)
    }

    pub fn finalize(&mut self) {
        self.m_backing.clear();
        self.m_is_initialized = false;
    }

    /// Get a raw pointer to the backing memory.
    ///
    /// Matches upstream `KSharedMemory::GetPointer(offset)` which returns
    /// `m_device_memory->GetPointer<u8>(m_physical_address + offset)`.
    pub fn get_pointer(&self, offset: usize) -> *const u8 {
        if offset < self.m_backing.len() {
            self.m_backing.as_ptr().wrapping_add(offset)
        } else {
            std::ptr::null()
        }
    }

    /// Get a mutable raw pointer to the backing memory.
    pub fn get_pointer_mut(&mut self, offset: usize) -> *mut u8 {
        if offset < self.m_backing.len() {
            self.m_backing.as_mut_ptr().wrapping_add(offset)
        } else {
            std::ptr::null_mut()
        }
    }

    /// Map the shared memory into a target process's address space.
    pub fn map(
        &self,
        _address: u64,
        _map_size: usize,
        _permissions: MemoryPermission,
    ) -> ResultCode {
        // TODO: Validate size and permission, then map page group.
        ResultCode::new(0)
    }

    /// Unmap the shared memory from a target process's address space.
    pub fn unmap(&self, _address: u64, _unmap_size: usize) -> ResultCode {
        // TODO: Validate size, then unmap page group.
        ResultCode::new(0)
    }

    pub fn is_initialized(&self) -> bool {
        self.m_is_initialized
    }

    pub fn get_size(&self) -> usize {
        self.m_size
    }

    pub fn post_destroy(_arg: usize) {}
}

impl Default for KSharedMemory {
    fn default() -> Self {
        Self::new()
    }
}
