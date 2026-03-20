//! Port of zuyu/src/core/hle/kernel/board/nintendo/nx/k_system_control.h/.cpp
//! Status: COMPLET (stub for settings-dependent paths)
//! Derniere synchro: 2026-03-11
//!
//! KSystemControl: board-level system control for the Nintendo NX.
//! Provides initialization parameters (memory sizes, pool sizes),
//! random number generation, and secure memory management.

use super::secure_monitor::{MemoryArrangement, MemorySize};
use crate::hle::kernel::k_trace::K_TRACE_BUFFER_SIZE;
use crate::hle::result::ResultCode;

/// Secure applet memory size (4 MiB).
pub const SECURE_APPLET_MEMORY_SIZE: usize = 4 * 1024 * 1024;

// Internal constants matching upstream impl namespace.
const REQUIRED_NON_SECURE_SYSTEM_MEMORY_SIZE_VI: usize = 0x2280 * 4 * 1024;
const REQUIRED_NON_SECURE_SYSTEM_MEMORY_SIZE_VI_FATAL: usize = 0x200 * 4 * 1024;
const REQUIRED_NON_SECURE_SYSTEM_MEMORY_SIZE_NVSERVICES: usize = 0x704 * 4 * 1024;
const REQUIRED_NON_SECURE_SYSTEM_MEMORY_SIZE_MISC: usize = 0x80 * 4 * 1024;

const REQUIRED_NON_SECURE_SYSTEM_MEMORY_SIZE: usize = REQUIRED_NON_SECURE_SYSTEM_MEMORY_SIZE_VI
    + REQUIRED_NON_SECURE_SYSTEM_MEMORY_SIZE_NVSERVICES
    + REQUIRED_NON_SECURE_SYSTEM_MEMORY_SIZE_MISC;

const REQUIRED_NON_SECURE_SYSTEM_MEMORY_SIZE_WITH_FATAL: usize =
    REQUIRED_NON_SECURE_SYSTEM_MEMORY_SIZE + REQUIRED_NON_SECURE_SYSTEM_MEMORY_SIZE_VI_FATAL;

const SECURE_ALIGNMENT: usize = 128 * 1024;

const KIB: usize = 1024;
const MIB: usize = 1024 * KIB;
const GIB: usize = 1024 * MIB;

/// Get the memory size enum for the current configuration.
/// Reads from Settings::values.memory_layout_mode.
fn get_memory_size_for_init() -> MemorySize {
    use common::settings::MemoryLayout;
    match *common::settings::values().memory_layout_mode.get_value() {
        MemoryLayout::Memory4Gb => MemorySize::MemorySize4GB,
        MemoryLayout::Memory6Gb => MemorySize::MemorySize6GB,
        MemoryLayout::Memory8Gb => MemorySize::MemorySize8GB,
    }
}

/// Get the memory arrangement for the current configuration.
fn get_memory_arrange_for_init() -> MemoryArrangement {
    match get_memory_size_for_init() {
        MemorySize::MemorySize4GB => MemoryArrangement::MemoryArrangement4GB,
        MemorySize::MemorySize6GB => MemoryArrangement::MemoryArrangement6GB,
        MemorySize::MemorySize8GB => MemoryArrangement::MemoryArrangement8GB,
    }
}

/// Initialization-time system control functions.
pub mod init {
    use super::*;

    /// Get the real (physical) memory size.
    pub fn get_real_memory_size() -> usize {
        get_intended_memory_size()
    }

    /// Get the intended (configured) memory size.
    pub fn get_intended_memory_size() -> usize {
        match get_memory_size_for_init() {
            MemorySize::MemorySize4GB => 4 * GIB,
            MemorySize::MemorySize6GB => 6 * GIB,
            MemorySize::MemorySize8GB => 8 * GIB,
        }
    }

    /// Compute the kernel physical base address given the DRAM base.
    pub fn get_kernel_physical_base_address(base_address: u64) -> u64 {
        let real_dram_size = get_real_memory_size() as u64;
        let intended_dram_size = get_intended_memory_size() as u64;
        if intended_dram_size * 2 < real_dram_size {
            base_address
        } else {
            base_address + ((real_dram_size - intended_dram_size) / 2)
        }
    }

    /// Whether the thread resource limit should be increased.
    pub fn should_increase_thread_resource_limit() -> bool {
        true
    }

    /// Get the application pool size.
    pub fn get_application_pool_size() -> usize {
        match get_memory_arrange_for_init() {
            MemoryArrangement::MemoryArrangement4GB => 3285 * MIB,
            MemoryArrangement::MemoryArrangement4GBForAppletDev => 2048 * MIB,
            MemoryArrangement::MemoryArrangement4GBForSystemDev => 3285 * MIB,
            MemoryArrangement::MemoryArrangement6GB => 4916 * MIB,
            MemoryArrangement::MemoryArrangement6GBForAppletDev => 3285 * MIB,
            MemoryArrangement::MemoryArrangement8GB => 6547 * MIB,
        }
    }

    /// Get the applet pool size.
    pub fn get_applet_pool_size() -> usize {
        let base_pool_size = match get_memory_arrange_for_init() {
            MemoryArrangement::MemoryArrangement4GB => 507 * MIB,
            MemoryArrangement::MemoryArrangement4GBForAppletDev => 1554 * MIB,
            MemoryArrangement::MemoryArrangement4GBForSystemDev => 448 * MIB,
            MemoryArrangement::MemoryArrangement6GB => 562 * MIB,
            MemoryArrangement::MemoryArrangement6GBForAppletDev => 2193 * MIB,
            MemoryArrangement::MemoryArrangement8GB => 562 * MIB,
        };

        let extra_system_memory_for_atmosphere: usize = 33 * MIB;
        base_pool_size - extra_system_memory_for_atmosphere - K_TRACE_BUFFER_SIZE
    }

    /// Get the minimum non-secure system pool size.
    pub fn get_minimum_non_secure_system_pool_size() -> usize {
        REQUIRED_NON_SECURE_SYSTEM_MEMORY_SIZE_WITH_FATAL
    }
}

/// Generate a random u64.
pub fn generate_random_u64() -> u64 {
    use std::collections::hash_map::RandomState;
    use std::hash::{BuildHasher, Hasher};
    let s = RandomState::new();
    let mut hasher = s.build_hasher();
    hasher.write_u64(0);
    hasher.finish()
}

/// Generate a random u64 in the inclusive range [min, max].
pub fn generate_random_range(min: u64, max: u64) -> u64 {
    if max == u64::MAX && min == u64::MIN {
        return generate_random_u64();
    }

    let range_size = (max + 1) - min;
    let effective_max = (u64::MAX / range_size) * range_size;
    loop {
        let rnd = generate_random_u64();
        if rnd < effective_max {
            return min + (rnd % range_size);
        }
    }
}

/// Calculate the required secure memory size.
pub fn calculate_required_secure_memory_size(size: usize, _pool: u32) -> usize {
    // If pool == Applet, return 0; otherwise return size.
    // Pool::Applet is pool index 2 in upstream.
    if _pool == 2 {
        0
    } else {
        size
    }
}

/// Allocate secure memory.
pub fn allocate_secure_memory(_size: usize, _pool: u32) -> ResultCode {
    // TODO: Implement once KMemoryManager is available.
    ResultCode::new(0)
}

/// Free secure memory.
pub fn free_secure_memory(_address: u64, _size: usize, _pool: u32) {
    // TODO: Implement once KMemoryManager is available.
}

/// Get the insecure memory pool identifier.
pub fn get_insecure_memory_pool() -> u32 {
    // KMemoryManager::Pool::SystemNonSecure
    3
}
