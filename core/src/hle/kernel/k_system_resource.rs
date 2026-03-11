//! Port of zuyu/src/core/hle/kernel/k_system_resource.h and k_system_resource.cpp
//! Status: Stubbed
//! Derniere synchro: 2026-03-11
//!
//! KSystemResource is a base class for kernel objects that own memory management
//! infrastructure (slab managers, page table managers). KSecureSystemResource
//! is a concrete implementation that allocates secure memory.

use super::k_dynamic_page_manager::KDynamicPageManager;
use super::k_dynamic_resource_manager::{KBlockInfoManager, KMemoryBlockSlabManager};
use super::k_memory_block::PAGE_SIZE;
use super::k_memory_manager;
use super::k_page_table_manager::KPageTableManager;
use super::k_page_table_slab_heap::KPageTableSlabHeap;

/// Port of Kernel::KSystemResource.
///
/// Base class that holds references to memory block slab manager, block info manager,
/// and page table manager. Upstream inherits from KAutoObject; here we keep just the
/// resource-management fields.
pub struct KSystemResource {
    memory_block_slab_manager: Option<Box<KMemoryBlockSlabManager>>,
    block_info_manager: Option<Box<KBlockInfoManager>>,
    page_table_manager: Option<Box<KPageTableManager>>,
    is_secure_resource: bool,
}

impl KSystemResource {
    pub fn new() -> Self {
        Self {
            memory_block_slab_manager: None,
            block_info_manager: None,
            page_table_manager: None,
            is_secure_resource: false,
        }
    }

    pub fn set_secure_resource(&mut self) {
        self.is_secure_resource = true;
    }

    pub fn is_secure_resource(&self) -> bool {
        self.is_secure_resource
    }

    pub fn set_managers(
        &mut self,
        mb: Box<KMemoryBlockSlabManager>,
        bi: Box<KBlockInfoManager>,
        pt: Box<KPageTableManager>,
    ) {
        assert!(self.memory_block_slab_manager.is_none());
        assert!(self.block_info_manager.is_none());
        assert!(self.page_table_manager.is_none());

        self.memory_block_slab_manager = Some(mb);
        self.block_info_manager = Some(bi);
        self.page_table_manager = Some(pt);
    }

    pub fn get_memory_block_slab_manager(&self) -> &KMemoryBlockSlabManager {
        self.memory_block_slab_manager.as_ref().unwrap()
    }

    pub fn get_block_info_manager(&self) -> &KBlockInfoManager {
        self.block_info_manager.as_ref().unwrap()
    }

    pub fn get_page_table_manager(&self) -> &KPageTableManager {
        self.page_table_manager.as_ref().unwrap()
    }

    pub fn get_memory_block_slab_manager_mut(&mut self) -> &mut KMemoryBlockSlabManager {
        self.memory_block_slab_manager.as_mut().unwrap()
    }

    pub fn get_block_info_manager_mut(&mut self) -> &mut KBlockInfoManager {
        self.block_info_manager.as_mut().unwrap()
    }

    pub fn get_page_table_manager_mut(&mut self) -> &mut KPageTableManager {
        self.page_table_manager.as_mut().unwrap()
    }
}

impl Default for KSystemResource {
    fn default() -> Self {
        Self::new()
    }
}

/// Port of Kernel::KSecureSystemResource.
///
/// Stubbed: Initialize/Finalize depend on KSystemControl, KScopedResourceReservation,
/// KPageTable::GetHeapPhysicalAddress, and DeviceMemory — all unported.
pub struct KSecureSystemResource {
    base: KSystemResource,
    is_initialized: bool,
    resource_pool: k_memory_manager::Pool,
    dynamic_page_manager: KDynamicPageManager,
    memory_block_slab_manager: KMemoryBlockSlabManager,
    block_info_manager: KBlockInfoManager,
    page_table_manager: KPageTableManager,
    page_table_heap: KPageTableSlabHeap,
    resource_address: u64,
    resource_size: usize,
}

impl KSecureSystemResource {
    pub fn new() -> Self {
        let mut base = KSystemResource::new();
        base.set_secure_resource();

        Self {
            base,
            is_initialized: false,
            resource_pool: k_memory_manager::Pool::Application,
            dynamic_page_manager: KDynamicPageManager::new(),
            memory_block_slab_manager: KMemoryBlockSlabManager::new(),
            block_info_manager: KBlockInfoManager::new(),
            page_table_manager: KPageTableManager::new(),
            page_table_heap: KPageTableSlabHeap::new(),
            resource_address: 0,
            resource_size: 0,
        }
    }

    pub fn is_initialized(&self) -> bool {
        self.is_initialized
    }

    pub fn get_size(&self) -> usize {
        self.resource_size
    }

    pub fn get_used_size(&self) -> usize {
        self.dynamic_page_manager.get_used() * PAGE_SIZE
    }

    pub fn get_dynamic_page_manager(&self) -> &KDynamicPageManager {
        &self.dynamic_page_manager
    }

    /// Stubbed: depends on KSystemControl, KScopedResourceReservation, etc.
    pub fn initialize(
        &mut self,
        _size: usize,
        _pool: k_memory_manager::Pool,
    ) -> Result<(), ()> {
        // TODO: implement when KSystemControl and related infrastructure is ported.
        Err(())
    }

    /// Stubbed: depends on KSystemControl.
    pub fn finalize(&mut self) {
        assert_eq!(self.memory_block_slab_manager.get_used(), 0);
        assert_eq!(self.block_info_manager.get_used(), 0);
        assert_eq!(self.page_table_manager.get_used(), 0);
        // TODO: free secure memory, release resource reservation.
    }

    pub fn calculate_required_secure_memory_size(
        _size: usize,
        _pool: k_memory_manager::Pool,
    ) -> usize {
        // Stubbed: depends on KSystemControl::CalculateRequiredSecureMemorySize.
        0
    }

    pub fn base(&self) -> &KSystemResource {
        &self.base
    }

    pub fn base_mut(&mut self) -> &mut KSystemResource {
        &mut self.base
    }
}

impl Default for KSecureSystemResource {
    fn default() -> Self {
        Self::new()
    }
}
