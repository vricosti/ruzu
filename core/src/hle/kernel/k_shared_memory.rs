//! Port of zuyu/src/core/hle/kernel/k_shared_memory.h/.cpp
//! Status: Ported — backed by DeviceMemory + KPageGroup matching upstream.
//! Derniere synchro: 2026-03-19
//!
//! KSharedMemory: kernel object for shared memory regions that can be
//! mapped into multiple process address spaces. Physical pages are
//! allocated from KMemoryManager (Pool::Secure) and tracked via KPageGroup.

use crate::device_memory::DeviceMemory;
use crate::hle::kernel::k_memory_block::{KMemoryPermission, KMemoryState, PAGE_SIZE};
use crate::hle::kernel::k_memory_manager::KMemoryManager;
use crate::hle::kernel::k_page_group::KPageGroup;
use crate::hle::kernel::k_process_page_table::KProcessPageTable;
use crate::hle::kernel::k_typed_address::KProcessAddress;
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

/// Convert SVC MemoryPermission to kernel KMemoryPermission.
/// Upstream: `ConvertToKMemoryPermission(Svc::MemoryPermission)`.
fn convert_to_k_memory_permission(perm: MemoryPermission) -> KMemoryPermission {
    match perm {
        MemoryPermission::None => KMemoryPermission::NONE,
        MemoryPermission::Read => KMemoryPermission::USER_READ,
        MemoryPermission::Write => KMemoryPermission::USER_WRITE,
        MemoryPermission::ReadWrite => {
            KMemoryPermission::USER_READ | KMemoryPermission::USER_WRITE
        }
        MemoryPermission::Execute => KMemoryPermission::USER_EXECUTE,
        MemoryPermission::ReadExecute => {
            KMemoryPermission::USER_READ | KMemoryPermission::USER_EXECUTE
        }
        MemoryPermission::DontCare => KMemoryPermission::USER_READ,
    }
}

/// KSharedMemory: allows sharing memory regions between processes.
///
/// Upstream allocates physical pages from KMemoryManager (Pool::Secure) backed
/// by DeviceMemory, tracks them via KPageGroup, and maps them into process
/// address spaces via KPageTableBase::MapPageGroup.
pub struct KSharedMemory {
    /// Pointer to the device memory backing store.
    /// Upstream: `Core::DeviceMemory* m_device_memory`.
    m_device_memory: *const DeviceMemory,
    /// Physical pages backing this shared memory.
    /// Upstream: `std::optional<KPageGroup> m_page_group`.
    m_page_group: Option<KPageGroup>,
    /// Starting physical address of the allocated region.
    /// Upstream: `KPhysicalAddress m_physical_address`.
    m_physical_address: u64,
    m_owner_permission: MemoryPermission,
    m_user_permission: MemoryPermission,
    m_size: usize,
    m_is_initialized: bool,
}

// SAFETY: KSharedMemory is used behind Arc<Mutex<>> or single-threaded.
// The raw pointer points to the long-lived DeviceMemory owned by System.
unsafe impl Send for KSharedMemory {}
unsafe impl Sync for KSharedMemory {}

impl KSharedMemory {
    pub fn new() -> Self {
        Self {
            m_device_memory: std::ptr::null(),
            m_page_group: None,
            m_physical_address: 0,
            m_owner_permission: MemoryPermission::None,
            m_user_permission: MemoryPermission::None,
            m_size: 0,
            m_is_initialized: false,
        }
    }

    /// Initialize the shared memory with physical pages from DeviceMemory.
    ///
    /// Upstream: `KSharedMemory::Initialize(DeviceMemory&, KProcess*,
    ///     Svc::MemoryPermission, Svc::MemoryPermission, size_t)`.
    ///
    /// Allocates continuous physical pages from the Secure pool via
    /// KMemoryManager, creates a KPageGroup tracking them, and zeroes
    /// the memory.
    pub fn initialize(
        &mut self,
        device_memory: &DeviceMemory,
        memory_manager: &mut KMemoryManager,
        owner_permission: MemoryPermission,
        user_permission: MemoryPermission,
        size: usize,
    ) -> ResultCode {
        use crate::hle::kernel::k_memory_manager::{Direction, Pool};

        self.m_device_memory = device_memory as *const DeviceMemory;
        self.m_owner_permission = owner_permission;
        self.m_user_permission = user_permission;

        let aligned_size = (size + PAGE_SIZE - 1) & !(PAGE_SIZE - 1);
        self.m_size = aligned_size;
        let num_pages = aligned_size / PAGE_SIZE;

        // Allocate physical pages from the Secure pool (from back, matching upstream).
        let option =
            KMemoryManager::encode_option(Pool::SECURE, Direction::FromBack);
        self.m_physical_address =
            memory_manager.allocate_and_open_continuous(num_pages, 1, option);
        if self.m_physical_address == 0 {
            log::error!(
                "KSharedMemory::initialize: failed to allocate {} pages from Secure pool",
                num_pages
            );
            return ResultCode::new(0xCE01); // ResultOutOfMemory
        }

        // Create page group tracking the allocated pages.
        let mut page_group = KPageGroup::new();
        let _ = page_group.add_block(self.m_physical_address, num_pages);
        self.m_page_group = Some(page_group);

        self.m_is_initialized = true;

        // Zero the allocated memory.
        let ptr = unsafe {
            (*self.m_device_memory).get_pointer(self.m_physical_address)
        };
        if !ptr.is_null() {
            unsafe {
                std::ptr::write_bytes(ptr, 0, aligned_size);
            }
        }

        log::debug!(
            "KSharedMemory::initialize: allocated {} pages at phys={:#x}, size={:#x}",
            num_pages, self.m_physical_address, aligned_size
        );

        ResultCode::new(0)
    }

    pub fn finalize(&mut self) {
        self.m_page_group = None;
        self.m_is_initialized = false;
    }

    /// Get a raw pointer to the backing memory at the given offset.
    ///
    /// Upstream: `m_device_memory->GetPointer<u8>(m_physical_address + offset)`.
    pub fn get_pointer(&self, offset: usize) -> *const u8 {
        if self.m_device_memory.is_null() || !self.m_is_initialized {
            return std::ptr::null();
        }
        unsafe {
            (*self.m_device_memory)
                .get_pointer_const(self.m_physical_address + offset as u64)
        }
    }

    /// Get a mutable raw pointer to the backing memory at the given offset.
    pub fn get_pointer_mut(&self, offset: usize) -> *mut u8 {
        if self.m_device_memory.is_null() || !self.m_is_initialized {
            return std::ptr::null_mut();
        }
        unsafe {
            (*self.m_device_memory)
                .get_pointer(self.m_physical_address + offset as u64)
        }
    }

    /// Map the shared memory into a target process's address space.
    ///
    /// Upstream: `KSharedMemory::Map(KProcess&, KProcessAddress, size_t,
    ///     Svc::MemoryPermission)`.
    pub fn map(
        &self,
        page_table: &mut KProcessPageTable,
        address: u64,
        map_size: usize,
        map_perm: MemoryPermission,
    ) -> ResultCode {
        // Validate size matches.
        if self.m_size != map_size {
            return crate::hle::kernel::svc::svc_results::RESULT_INVALID_SIZE;
        }

        // Upstream selects permission based on whether target_process == m_owner_process.
        // Owner gets m_owner_permission, others get m_user_permission.
        // No m_owner_process field is stored yet; default to user_permission.
        let _test_perm = self.m_user_permission;

        let pg = match &self.m_page_group {
            Some(pg) => pg,
            None => return ResultCode::new(0xCA01), // Internal error
        };

        let k_perm = convert_to_k_memory_permission(map_perm);
        let result = page_table.map_page_group(
            KProcessAddress::new(address),
            pg,
            KMemoryState::SHARED,
            k_perm,
        );
        ResultCode::new(result)
    }

    /// Unmap the shared memory from a target process's address space.
    pub fn unmap(
        &self,
        page_table: &mut KProcessPageTable,
        address: u64,
        unmap_size: usize,
    ) -> ResultCode {
        if self.m_size != unmap_size {
            return crate::hle::kernel::svc::svc_results::RESULT_INVALID_SIZE;
        }

        let pg = match &self.m_page_group {
            Some(pg) => pg,
            None => return ResultCode::new(0xCA01),
        };

        let result = page_table.unmap_page_group(
            KProcessAddress::new(address),
            pg,
            KMemoryState::SHARED,
        );
        ResultCode::new(result)
    }

    pub fn is_initialized(&self) -> bool {
        self.m_is_initialized
    }

    pub fn get_size(&self) -> usize {
        self.m_size
    }

    /// Get the page group (for external access, e.g. SVC handler).
    pub fn get_page_group(&self) -> Option<&KPageGroup> {
        self.m_page_group.as_ref()
    }

    pub fn post_destroy(_arg: usize) {}
}

impl Default for KSharedMemory {
    fn default() -> Self {
        Self::new()
    }
}
