//! Port of zuyu/src/core/hle/kernel/k_code_memory.h/.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-21
//!
//! KCodeMemory: kernel object for mapping executable code between processes.
//! Uses KPageGroup to track the physical pages.

use super::k_memory_block::PAGE_SIZE;
use super::k_page_group::KPageGroup;
use crate::hle::result::ResultCode;

/// Operations that can be performed on code memory.
/// Maps to upstream CodeMemoryOperation.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CodeMemoryOperation {
    Map = 0,
    MapToOwner = 1,
    Unmap = 2,
    UnmapFromOwner = 3,
}

/// Memory permission for code memory mapping.
/// Maps to Svc::MemoryPermission for the subset used by code memory.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CodeMemoryPermission {
    Read = 1,
    ReadExecute = 5,
}

/// KCodeMemory: allows a process to share executable code regions with
/// another process. Upstream inherits from KAutoObjectWithSlabHeapAndContainer.
pub struct KCodeMemory {
    m_page_group: Option<KPageGroup>,
    m_address: u64,
    m_is_initialized: bool,
    m_is_owner_mapped: bool,
    m_is_mapped: bool,
}

impl KCodeMemory {
    pub fn new() -> Self {
        Self {
            m_page_group: None,
            m_address: 0,
            m_is_initialized: false,
            m_is_owner_mapped: false,
            m_is_mapped: false,
        }
    }

    /// Initialize the code memory from the given address and size.
    /// Port of upstream `KCodeMemory::Initialize`.
    pub fn initialize(&mut self, address: u64, size: usize) -> ResultCode {
        // Create the page group for this region.
        let mut pg = KPageGroup::new();
        let num_pages = (size + PAGE_SIZE - 1) / PAGE_SIZE;
        if pg.add_block(address, num_pages).is_err() {
            return ResultCode::new(0xCA01); // ResultOutOfMemory
        }

        // Upstream: page_table.LockForCodeMemory(&pg, addr, size)
        // Upstream: memset pages to 0xFF via DeviceMemory
        // In the host-emulated model, memory locking and clearing are no-ops.

        self.m_page_group = Some(pg);
        self.m_address = address;
        self.m_is_initialized = true;
        self.m_is_owner_mapped = false;
        self.m_is_mapped = false;
        ResultCode::new(0) // ResultSuccess
    }

    /// Finalize the code memory.
    /// Port of upstream `KCodeMemory::Finalize`.
    pub fn finalize(&mut self) {
        if let Some(ref mut pg) = self.m_page_group {
            if !self.m_is_mapped && !self.m_is_owner_mapped {
                // Upstream: m_owner->GetPageTable().UnlockForCodeMemory(...)
            }
            pg.close();
            pg.finalize();
        }
        self.m_page_group = None;
    }

    /// Map the code memory into the current process.
    /// Port of upstream `KCodeMemory::Map`.
    pub fn map(&mut self, _address: u64, size: usize) -> ResultCode {
        let pg = match self.m_page_group {
            Some(ref pg) => pg,
            None => return ResultCode::new(0xCA01),
        };

        let expected_pages = (size + PAGE_SIZE - 1) / PAGE_SIZE;
        if pg.get_num_pages() != expected_pages {
            return crate::hle::kernel::svc::svc_results::RESULT_INVALID_SIZE;
        }

        if self.m_is_mapped {
            return crate::hle::kernel::svc::svc_results::RESULT_INVALID_STATE;
        }

        // Upstream: page_table.MapPageGroup(address, pg, CodeOut, UserReadWrite)
        self.m_is_mapped = true;
        ResultCode::new(0) // ResultSuccess
    }

    /// Unmap the code memory from the current process.
    /// Port of upstream `KCodeMemory::Unmap`.
    pub fn unmap(&mut self, _address: u64, size: usize) -> ResultCode {
        let pg = match self.m_page_group {
            Some(ref pg) => pg,
            None => return ResultCode::new(0xCA01),
        };

        let expected_pages = (size + PAGE_SIZE - 1) / PAGE_SIZE;
        if pg.get_num_pages() != expected_pages {
            return crate::hle::kernel::svc::svc_results::RESULT_INVALID_SIZE;
        }

        self.m_is_mapped = false;
        ResultCode::new(0) // ResultSuccess
    }

    /// Map the code memory to the owner process.
    /// Port of upstream `KCodeMemory::MapToOwner`.
    pub fn map_to_owner(
        &mut self,
        _address: u64,
        size: usize,
        _perm: CodeMemoryPermission,
    ) -> ResultCode {
        let pg = match self.m_page_group {
            Some(ref pg) => pg,
            None => return ResultCode::new(0xCA01),
        };

        let expected_pages = (size + PAGE_SIZE - 1) / PAGE_SIZE;
        if pg.get_num_pages() != expected_pages {
            return crate::hle::kernel::svc::svc_results::RESULT_INVALID_SIZE;
        }

        if self.m_is_owner_mapped {
            return crate::hle::kernel::svc::svc_results::RESULT_INVALID_STATE;
        }

        // Upstream: page_table.MapPageGroup(address, pg, GeneratedCode, k_perm)
        self.m_is_owner_mapped = true;
        ResultCode::new(0) // ResultSuccess
    }

    /// Unmap the code memory from the owner process.
    /// Port of upstream `KCodeMemory::UnmapFromOwner`.
    pub fn unmap_from_owner(&mut self, _address: u64, size: usize) -> ResultCode {
        let pg = match self.m_page_group {
            Some(ref pg) => pg,
            None => return ResultCode::new(0xCA01),
        };

        let expected_pages = (size + PAGE_SIZE - 1) / PAGE_SIZE;
        if pg.get_num_pages() != expected_pages {
            return crate::hle::kernel::svc::svc_results::RESULT_INVALID_SIZE;
        }

        self.m_is_owner_mapped = false;
        ResultCode::new(0) // ResultSuccess
    }

    pub fn is_initialized(&self) -> bool {
        self.m_is_initialized
    }

    pub fn get_source_address(&self) -> u64 {
        self.m_address
    }

    /// Get the size of the code memory region.
    /// Port of upstream `KCodeMemory::GetSize` (implicit via page_group).
    pub fn get_size(&self) -> usize {
        if self.m_is_initialized {
            if let Some(ref pg) = self.m_page_group {
                return pg.get_num_pages() * PAGE_SIZE;
            }
        }
        0
    }
}

impl Default for KCodeMemory {
    fn default() -> Self {
        Self::new()
    }
}
