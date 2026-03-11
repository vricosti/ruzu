//! Port of zuyu/src/core/hle/kernel/init/init_slab_setup.h/.cpp
//! Status: COMPLET (stub — runtime dependencies not yet available)
//! Derniere synchro: 2026-03-11
//!
//! Slab heap initialization: resource counts, size calculation, and
//! slab heap setup. Full implementation requires KMemoryLayout, KMemoryManager,
//! and all slab-allocated kernel object types.

use crate::hardware_properties;

/// Default slab resource counts, matching upstream constexpr values.
const SLAB_COUNT_K_PROCESS: usize = 80;
const SLAB_COUNT_K_THREAD: usize = 800;
const SLAB_COUNT_K_EVENT: usize = 900;
const SLAB_COUNT_K_INTERRUPT_EVENT: usize = 100;
const SLAB_COUNT_K_PORT: usize = 384;
const SLAB_COUNT_K_SHARED_MEMORY: usize = 80;
const SLAB_COUNT_K_TRANSFER_MEMORY: usize = 200;
const SLAB_COUNT_K_CODE_MEMORY: usize = 10;
const SLAB_COUNT_K_DEVICE_ADDRESS_SPACE: usize = 300;
const SLAB_COUNT_K_SESSION: usize = 1133;
const SLAB_COUNT_K_LIGHT_SESSION: usize = 100;
const SLAB_COUNT_K_OBJECT_NAME: usize = 7;
const SLAB_COUNT_K_RESOURCE_LIMIT: usize = 5;
const SLAB_COUNT_K_DEBUG: usize = hardware_properties::NUM_CPU_CORES as usize;
const SLAB_COUNT_K_IO_POOL: usize = 1;
const SLAB_COUNT_K_IO_REGION: usize = 6;
const SLAB_COUNT_K_SESSION_REQUEST_MAPPINGS: usize = 40;

/// Extra thread count when resource limit is increased.
const SLAB_COUNT_EXTRA_K_THREAD: usize = (1024 + 256 + 256) - SLAB_COUNT_K_THREAD;

/// Slab resource counts for all kernel object types.
#[derive(Debug, Clone)]
pub struct KSlabResourceCounts {
    pub num_k_process: usize,
    pub num_k_thread: usize,
    pub num_k_event: usize,
    pub num_k_interrupt_event: usize,
    pub num_k_port: usize,
    pub num_k_shared_memory: usize,
    pub num_k_transfer_memory: usize,
    pub num_k_code_memory: usize,
    pub num_k_device_address_space: usize,
    pub num_k_session: usize,
    pub num_k_light_session: usize,
    pub num_k_object_name: usize,
    pub num_k_resource_limit: usize,
    pub num_k_debug: usize,
    pub num_k_io_pool: usize,
    pub num_k_io_region: usize,
    pub num_k_session_request_mappings: usize,
}

impl KSlabResourceCounts {
    /// Create default slab resource counts matching upstream.
    pub fn create_default() -> Self {
        Self {
            num_k_process: SLAB_COUNT_K_PROCESS,
            num_k_thread: SLAB_COUNT_K_THREAD,
            num_k_event: SLAB_COUNT_K_EVENT,
            num_k_interrupt_event: SLAB_COUNT_K_INTERRUPT_EVENT,
            num_k_port: SLAB_COUNT_K_PORT,
            num_k_shared_memory: SLAB_COUNT_K_SHARED_MEMORY,
            num_k_transfer_memory: SLAB_COUNT_K_TRANSFER_MEMORY,
            num_k_code_memory: SLAB_COUNT_K_CODE_MEMORY,
            num_k_device_address_space: SLAB_COUNT_K_DEVICE_ADDRESS_SPACE,
            num_k_session: SLAB_COUNT_K_SESSION,
            num_k_light_session: SLAB_COUNT_K_LIGHT_SESSION,
            num_k_object_name: SLAB_COUNT_K_OBJECT_NAME,
            num_k_resource_limit: SLAB_COUNT_K_RESOURCE_LIMIT,
            num_k_debug: SLAB_COUNT_K_DEBUG,
            num_k_io_pool: SLAB_COUNT_K_IO_POOL,
            num_k_io_region: SLAB_COUNT_K_IO_REGION,
            num_k_session_request_mappings: SLAB_COUNT_K_SESSION_REQUEST_MAPPINGS,
        }
    }

    /// Get the extra thread count for resource limit increase.
    pub fn extra_k_thread_count() -> usize {
        SLAB_COUNT_EXTRA_K_THREAD
    }
}

/// Initialize slab resource counts for the kernel.
/// If the thread resource limit should be increased, adds extra threads.
pub fn initialize_slab_resource_counts(counts: &mut KSlabResourceCounts) {
    *counts = KSlabResourceCounts::create_default();
    if crate::hle::kernel::board::k_system_control::init::should_increase_thread_resource_limit() {
        counts.num_k_thread += SLAB_COUNT_EXTRA_K_THREAD;
    }
}

/// Calculate the total slab heap size for all kernel object types.
///
/// Full implementation requires sizeof() for each kernel object type.
/// This is a stub that returns a placeholder value.
pub fn calculate_total_slab_heap_size(_counts: &KSlabResourceCounts) -> usize {
    // TODO: Sum up aligned sizes for each slab type, plus gap size.
    0
}

/// Initialize slab heaps from the slab memory region.
///
/// Full implementation requires KMemoryLayout, KSystemControl, and all
/// slab-allocated kernel object types.
pub fn initialize_slab_heaps() {
    // TODO: Implement once all kernel object types and KMemoryLayout are available.
}
