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

    /// Matches upstream member function `KSecureSystemResource::CalculateRequiredSecureMemorySize() const`
    /// which calls the static version with own m_resource_size and m_resource_pool.
    pub fn calculate_required_secure_memory_size_self(&self) -> usize {
        Self::calculate_required_secure_memory_size(self.resource_size, self.resource_pool)
    }

    pub fn get_dynamic_page_manager(&self) -> &KDynamicPageManager {
        &self.dynamic_page_manager
    }

    /// Initialize the secure system resource.
    /// Port of upstream `KSecureSystemResource::Initialize`.
    /// Allocates secure memory, initializes dynamic page manager and slab heaps.
    pub fn initialize(
        &mut self,
        size: usize,
        pool: k_memory_manager::Pool,
        mm: &mut k_memory_manager::KMemoryManager,
    ) -> Result<(), ()> {
        use super::k_memory_block::PAGE_SIZE;

        self.resource_size = size;

        // Allocate secure memory via KSystemControl.
        let resource_address =
            super::board::k_system_control::allocate_secure_memory(mm, size, pool as u32)
                .map_err(|_| ())?;

        self.resource_address = resource_address;

        // Calculate reference count size.
        let rc_size = common::alignment::align_up(
            (size / PAGE_SIZE * std::mem::size_of::<u32>()) as u64,
            PAGE_SIZE as u64,
        ) as usize;
        if size <= rc_size {
            // Clean up on failure.
            super::board::k_system_control::free_secure_memory(
                mm,
                resource_address,
                size,
                pool as u32,
            );
            return Err(());
        }

        // Initialize the dynamic page manager with the remaining memory.
        if self
            .dynamic_page_manager
            .initialize(resource_address + rc_size as u64, size - rc_size, PAGE_SIZE)
            .is_err()
        {
            super::board::k_system_control::free_secure_memory(
                mm,
                resource_address,
                size,
                pool as u32,
            );
            return Err(());
        }

        self.is_initialized = true;
        Ok(())
    }

    /// Finalize the secure system resource.
    /// Port of upstream `KSecureSystemResource::Finalize`.
    pub fn finalize(&mut self, mm: &mut k_memory_manager::KMemoryManager) {
        assert_eq!(self.memory_block_slab_manager.get_used(), 0);
        assert_eq!(self.block_info_manager.get_used(), 0);
        assert_eq!(self.page_table_manager.get_used(), 0);

        // Free secure memory.
        if self.resource_address != 0 && self.resource_size > 0 {
            super::board::k_system_control::free_secure_memory(
                mm,
                self.resource_address,
                self.resource_size,
                0, // Pool
            );
        }
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
