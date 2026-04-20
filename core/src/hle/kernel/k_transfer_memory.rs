//! Port of zuyu/src/core/hle/kernel/k_transfer_memory.h/.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-21
//!
//! KTransferMemory: kernel object for transferring memory ownership between
//! processes. Uses KPageGroup to track the physical pages.

use super::k_memory_block::PAGE_SIZE;
use super::k_page_group::KPageGroup;
use super::k_process::KProcess;
use super::k_process_page_table::KProcessPageTable;
use super::k_resource_limit::LimitableResource;
use super::k_shared_memory::MemoryPermission;
use super::k_typed_address::KProcessAddress;
use crate::hle::result::ResultCode;
use std::sync::{Arc, Mutex, Weak};
use super::k_process::ProcessLock;

/// KTransferMemory: allows transferring a memory region from one process
/// to another. Upstream inherits from KAutoObjectWithSlabHeapAndContainer.
pub struct KTransferMemory {
    m_page_group: Option<KPageGroup>,
    m_owner: Option<Weak<ProcessLock>>,
    m_address: u64,
    m_owner_perm: MemoryPermission,
    m_is_initialized: bool,
    m_is_mapped: bool,
}

impl KTransferMemory {
    pub fn new() -> Self {
        Self {
            m_page_group: None,
            m_owner: None,
            m_address: 0,
            m_owner_perm: MemoryPermission::None,
            m_is_initialized: false,
            m_is_mapped: false,
        }
    }

    /// Initialize the transfer memory from the given address range.
    /// Port of upstream `KTransferMemory::Initialize`.
    pub fn initialize(
        &mut self,
        owner: &Arc<ProcessLock>,
        address: u64,
        size: usize,
        owner_perm: MemoryPermission,
    ) -> ResultCode {
        // Create the page group for this region.
        let mut pg = KPageGroup::new();
        let num_pages = (size + PAGE_SIZE - 1) / PAGE_SIZE;
        if pg.add_block(address, num_pages).is_err() {
            return ResultCode::new(0xCA01); // ResultOutOfMemory
        }

        // Upstream: page_table.LockForTransferMemory(&pg, addr, size, perm)
        // In the host-emulated model, memory locking is a no-op.

        self.m_page_group = Some(pg);
        self.m_owner = Some(Arc::downgrade(owner));
        self.m_address = address;
        self.m_owner_perm = owner_perm;
        self.m_is_initialized = true;
        self.m_is_mapped = false;
        ResultCode::new(0) // ResultSuccess
    }

    /// Finalize and release the transfer memory.
    /// Port of upstream `KTransferMemory::Finalize`.
    pub fn finalize(&mut self, owner_page_table: &mut KProcessPageTable) {
        if let Some(ref mut pg) = self.m_page_group {
            if !self.m_is_mapped {
                let size = pg.get_num_pages() * PAGE_SIZE;
                if size > 0 {
                    let result = owner_page_table
                        .unlock_for_transfer_memory(KProcessAddress::new(self.m_address), size);
                    debug_assert_eq!(
                        result, 0,
                        "KTransferMemory::finalize unlock_for_transfer_memory failed: 0x{result:X}"
                    );
                }
            }
            pg.close();
            pg.finalize();
        }
        self.m_page_group = None;
    }

    pub fn is_initialized(&self) -> bool {
        self.m_is_initialized
    }

    pub fn get_owner(&self) -> Option<Arc<ProcessLock>> {
        self.m_owner.as_ref().and_then(Weak::upgrade)
    }

    pub fn get_source_address(&self) -> u64 {
        self.m_address
    }

    /// Get the size of the transfer memory's source region.
    /// Always reports the page-group's size, regardless of map state.
    pub fn get_source_size(&self) -> usize {
        if let Some(ref pg) = self.m_page_group {
            return pg.get_num_pages() * PAGE_SIZE;
        }
        0
    }

    /// Whether the transfer memory has been mapped into a target process.
    pub fn is_mapped(&self) -> bool {
        self.m_is_mapped
    }

    /// Get the size of the transfer memory region.
    /// Port of upstream `KTransferMemory::GetSize`.
    pub fn get_size(&self) -> usize {
        if self.m_is_initialized {
            if let Some(ref pg) = self.m_page_group {
                return pg.get_num_pages() * PAGE_SIZE;
            }
        }
        0
    }

    /// Map the transfer memory into the current process.
    /// Port of upstream `KTransferMemory::Map`.
    pub fn map(&mut self, _address: u64, size: usize, map_perm: MemoryPermission) -> ResultCode {
        let pg = match self.m_page_group {
            Some(ref pg) => pg,
            None => return ResultCode::new(0xCA01),
        };

        // Validate the size.
        let expected_pages = (size + PAGE_SIZE - 1) / PAGE_SIZE;
        if pg.get_num_pages() != expected_pages {
            return crate::hle::kernel::svc::svc_results::RESULT_INVALID_SIZE;
        }

        // Validate the permission.
        if self.m_owner_perm != map_perm {
            return crate::hle::kernel::svc::svc_results::RESULT_INVALID_STATE;
        }

        // Ensure we're not already mapped.
        if self.m_is_mapped {
            return crate::hle::kernel::svc::svc_results::RESULT_INVALID_STATE;
        }

        // Upstream: page_table.MapPageGroup(address, pg, state, perm)
        // In the host-emulated model, the memory is already accessible.

        self.m_is_mapped = true;
        ResultCode::new(0) // ResultSuccess
    }

    /// Unmap the transfer memory from the current process.
    /// Port of upstream `KTransferMemory::Unmap`.
    pub fn unmap(&mut self, _address: u64, size: usize) -> ResultCode {
        let pg = match self.m_page_group {
            Some(ref pg) => pg,
            None => return ResultCode::new(0xCA01),
        };

        // Validate the size.
        let expected_pages = (size + PAGE_SIZE - 1) / PAGE_SIZE;
        if pg.get_num_pages() != expected_pages {
            return crate::hle::kernel::svc::svc_results::RESULT_INVALID_SIZE;
        }

        // Upstream: page_table.UnmapPageGroup(address, pg, state)

        assert!(self.m_is_mapped);
        self.m_is_mapped = false;
        ResultCode::new(0) // ResultSuccess
    }

    /// Post-destroy: release resource limit and owner reference bookkeeping.
    /// Port of upstream `KTransferMemory::PostDestroy`.
    pub fn post_destroy(&self, owner: &mut KProcess) {
        if let Some(ref rl) = owner.resource_limit {
            rl.lock()
                .unwrap()
                .release(LimitableResource::TransferMemoryCountMax, 1);
        }
    }
}

impl Default for KTransferMemory {
    fn default() -> Self {
        Self::new()
    }
}
