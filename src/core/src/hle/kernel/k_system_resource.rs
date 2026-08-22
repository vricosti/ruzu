//! Port of zuyu/src/core/hle/kernel/k_system_resource.h and k_system_resource.cpp
//! Status: Partial
//! Derniere synchro: 2026-08-04
//!
//! KSystemResource is a base class for kernel objects that own memory management
//! infrastructure (slab managers, page table managers). KSecureSystemResource
//! is a concrete implementation that allocates secure memory.

use super::k_dynamic_page_manager::KDynamicPageManager;
use super::k_dynamic_resource_manager::{
    KBlockInfoManager, KBlockInfoSlabHeap, KMemoryBlockSlabHeap, KMemoryBlockSlabManager,
};
use super::k_dynamic_slab_heap::KDynamicSlabHeap;
use super::k_memory_block::PAGE_SIZE;
use super::k_memory_manager;
use super::k_page_table_manager::KPageTableManager;
use super::k_page_table_slab_heap::{KPageTableSlabHeap, RefCount};
use super::k_resource_limit::{KResourceLimit, LimitableResource};
use super::k_scoped_resource_reservation::KScopedResourceReservation;
use crate::hle::result::ResultCode;
use std::sync::{Arc, Mutex, MutexGuard};

/// Port of Kernel::KSystemResource.
///
/// Base class that holds references to memory block slab manager, block info manager,
/// and page table manager. Upstream inherits from KAutoObject; here we keep just the
/// resource-management fields.
pub struct KSystemResource {
    memory_block_slab_manager: Option<Arc<KMemoryBlockSlabManager>>,
    block_info_manager: Option<Arc<KBlockInfoManager>>,
    page_table_manager: Option<Arc<KPageTableManager>>,
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
        mb: Arc<KMemoryBlockSlabManager>,
        bi: Arc<KBlockInfoManager>,
        pt: Arc<KPageTableManager>,
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

    pub fn memory_block_slab_manager_arc(&self) -> Arc<KMemoryBlockSlabManager> {
        Arc::clone(self.memory_block_slab_manager.as_ref().unwrap())
    }

    pub fn block_info_manager_arc(&self) -> Arc<KBlockInfoManager> {
        Arc::clone(self.block_info_manager.as_ref().unwrap())
    }

    pub fn page_table_manager_arc(&self) -> Arc<KPageTableManager> {
        Arc::clone(self.page_table_manager.as_ref().unwrap())
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
    dynamic_page_manager: Arc<Mutex<KDynamicPageManager>>,
    memory_block_slab_manager: Option<Arc<KMemoryBlockSlabManager>>,
    block_info_manager: Option<Arc<KBlockInfoManager>>,
    page_table_manager: Option<Arc<KPageTableManager>>,
    memory_block_heap: Option<Arc<KMemoryBlockSlabHeap>>,
    block_info_heap: Option<Arc<KBlockInfoSlabHeap>>,
    page_table_heap: Option<Arc<KPageTableSlabHeap>>,
    resource_limit: Option<Arc<KResourceLimit>>,
    resource_address: u64,
    resource_size: usize,
}

impl KSecureSystemResource {
    pub fn new() -> Self {
        let mut base = KSystemResource::new();
        base.set_secure_resource();

        let dynamic_page_manager = Arc::new(Mutex::new(KDynamicPageManager::new()));

        Self {
            base,
            is_initialized: false,
            resource_pool: k_memory_manager::Pool::Application,
            dynamic_page_manager,
            memory_block_slab_manager: None,
            block_info_manager: None,
            page_table_manager: None,
            memory_block_heap: None,
            block_info_heap: None,
            page_table_heap: None,
            resource_limit: None,
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
        self.dynamic_page_manager.lock().unwrap().get_used() * PAGE_SIZE
    }

    /// Matches upstream member function `KSecureSystemResource::CalculateRequiredSecureMemorySize() const`
    /// which calls the static version with own m_resource_size and m_resource_pool.
    pub fn calculate_required_secure_memory_size_self(&self) -> usize {
        Self::calculate_required_secure_memory_size(self.resource_size, self.resource_pool)
    }

    pub fn get_dynamic_page_manager(&self) -> MutexGuard<'_, KDynamicPageManager> {
        self.dynamic_page_manager.lock().unwrap()
    }

    /// Initialize the secure system resource.
    /// Port of upstream `KSecureSystemResource::Initialize`.
    /// Allocates secure memory, initializes dynamic page manager and slab heaps.
    pub fn initialize(
        &mut self,
        size: usize,
        resource_limit: Option<Arc<KResourceLimit>>,
        pool: k_memory_manager::Pool,
        mm: &mut k_memory_manager::KMemoryManager,
    ) -> Result<(), ResultCode> {
        use super::k_memory_block::PAGE_SIZE;

        // Set members (k_system_resource.cpp:12-14).
        self.resource_limit = resource_limit.clone();
        self.resource_size = size;
        self.resource_pool = pool;

        // Reserve the physical memory consumed by the secure resource before
        // allocating it (k_system_resource.cpp:16-21).
        let secure_size = self.calculate_required_secure_memory_size_self();
        let mut memory_reservation = KScopedResourceReservation::new(
            resource_limit,
            LimitableResource::PhysicalMemoryMax,
            secure_size as i64,
        );
        if !memory_reservation.succeeded() {
            return Err(super::svc::svc_results::RESULT_LIMIT_REACHED);
        }

        // Allocate secure memory via KSystemControl.
        let resource_address =
            super::board::k_system_control::allocate_secure_memory(mm, size, pool as u32)
                .map_err(|result| result)?;

        self.resource_address = resource_address;

        // Calculate reference count size.
        let rc_size = common::alignment::align_up(
            (size / PAGE_SIZE * std::mem::size_of::<RefCount>()) as u64,
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
            return Err(super::svc::svc_results::RESULT_OUT_OF_MEMORY);
        }

        // Initialize the dynamic page manager with the remaining memory.
        if self
            .dynamic_page_manager
            .lock()
            .unwrap()
            .initialize(resource_address + rc_size as u64, size - rc_size, PAGE_SIZE)
            .is_err()
        {
            super::board::k_system_control::free_secure_memory(
                mm,
                resource_address,
                size,
                pool as u32,
            );
            return Err(super::svc::svc_results::RESULT_OUT_OF_MEMORY);
        }

        let memory_block_heap = Arc::new(KDynamicSlabHeap::new(false));
        let block_info_heap = Arc::new(KDynamicSlabHeap::new(false));
        let page_table_heap = Arc::new(KPageTableSlabHeap::new());
        memory_block_heap.initialize_with_pages(Arc::clone(&self.dynamic_page_manager), 0);
        block_info_heap.initialize_with_pages(Arc::clone(&self.dynamic_page_manager), 0);
        let memory_block_slab_manager = Arc::new(KMemoryBlockSlabManager::new_with_resources(
            Some(Arc::clone(&self.dynamic_page_manager)),
            Arc::clone(&memory_block_heap),
        ));
        let block_info_manager = Arc::new(KBlockInfoManager::new_with_resources(
            Some(Arc::clone(&self.dynamic_page_manager)),
            Arc::clone(&block_info_heap),
        ));
        page_table_heap.initialize(Arc::clone(&self.dynamic_page_manager), 0);
        let page_table_manager = Arc::new(KPageTableManager::new(Arc::clone(&page_table_heap)));
        self.base.set_managers(
            Arc::clone(&memory_block_slab_manager),
            Arc::clone(&block_info_manager),
            Arc::clone(&page_table_manager),
        );
        self.memory_block_slab_manager = Some(memory_block_slab_manager);
        self.block_info_manager = Some(block_info_manager);
        self.page_table_manager = Some(page_table_manager);
        self.memory_block_heap = Some(memory_block_heap);
        self.block_info_heap = Some(block_info_heap);
        self.page_table_heap = Some(page_table_heap);

        memory_reservation.commit();
        self.is_initialized = true;
        Ok(())
    }

    /// Finalize the secure system resource.
    /// Port of upstream `KSecureSystemResource::Finalize`.
    pub fn finalize(&mut self, mm: &mut k_memory_manager::KMemoryManager) {
        assert_eq!(
            self.memory_block_slab_manager.as_ref().unwrap().get_used(),
            0
        );
        assert_eq!(self.block_info_manager.as_ref().unwrap().get_used(), 0);
        assert_eq!(self.page_table_manager.as_ref().unwrap().get_used(), 0);
        debug_assert_eq!(self.memory_block_heap.as_ref().unwrap().get_used(), 0);
        debug_assert_eq!(self.block_info_heap.as_ref().unwrap().get_used(), 0);
        debug_assert_eq!(self.page_table_heap.as_ref().unwrap().get_used(), 0);

        // Free secure memory.
        if self.resource_address != 0 && self.resource_size > 0 {
            super::board::k_system_control::free_secure_memory(
                mm,
                self.resource_address,
                self.resource_size,
                self.resource_pool as u32,
            );
        }

        if let Some(resource_limit) = self.resource_limit.take() {
            resource_limit.release(
                LimitableResource::PhysicalMemoryMax,
                self.calculate_required_secure_memory_size_self() as i64,
            );
        }
        self.is_initialized = false;
    }

    pub fn calculate_required_secure_memory_size(
        size: usize,
        pool: k_memory_manager::Pool,
    ) -> usize {
        super::board::k_system_control::calculate_required_secure_memory_size(size, pool as u32)
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn secure_resource_publishes_managers_backed_by_its_dynamic_pool() {
        let mut memory_manager = k_memory_manager::KMemoryManager::new();
        memory_manager.initialize_pool(
            k_memory_manager::Pool::Application,
            0x1_0000_0000,
            2 * 1024 * 1024,
        );
        let mut resource = KSecureSystemResource::new();
        resource
            .initialize(
                256 * 1024,
                None,
                k_memory_manager::Pool::Application,
                &mut memory_manager,
            )
            .unwrap();

        assert!(resource.is_initialized());
        assert!(Arc::ptr_eq(
            &resource.base().memory_block_slab_manager_arc(),
            resource.memory_block_slab_manager.as_ref().unwrap(),
        ));
        assert!(Arc::ptr_eq(
            &resource.base().block_info_manager_arc(),
            resource.block_info_manager.as_ref().unwrap(),
        ));
        assert!(Arc::ptr_eq(
            &resource.base().page_table_manager_arc(),
            resource.page_table_manager.as_ref().unwrap(),
        ));

        let memory_block = resource
            .base()
            .get_memory_block_slab_manager()
            .allocate()
            .unwrap();
        let block_info = resource.base().get_block_info_manager().allocate().unwrap();
        let page_table = resource.base().get_page_table_manager().allocate().unwrap();
        assert_eq!(resource.get_used_size(), 3 * PAGE_SIZE);

        resource
            .base()
            .get_memory_block_slab_manager()
            .free(memory_block);
        resource.base().get_block_info_manager().free(block_info);
        resource.base().get_page_table_manager().free(page_table);
        resource.finalize(&mut memory_manager);
        assert!(!resource.is_initialized());
    }

    #[test]
    fn secure_memory_size_uses_edens_applet_pool_id() {
        assert_eq!(
            KSecureSystemResource::calculate_required_secure_memory_size(
                0x20_000,
                k_memory_manager::Pool::Applet,
            ),
            0,
        );
        assert_eq!(
            KSecureSystemResource::calculate_required_secure_memory_size(
                0x20_000,
                k_memory_manager::Pool::System,
            ),
            0x20_000,
        );
    }
}
