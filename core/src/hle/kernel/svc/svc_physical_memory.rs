//! Port of zuyu/src/core/hle/kernel/svc/svc_physical_memory.cpp
//! Status: Ported
//! Derniere synchro: 2026-03-20
//!
//! SVC handlers for physical memory operations (SetHeapSize, MapPhysicalMemory, etc.).

use crate::core::System;
use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::kernel::svc_common::HEAP_SIZE_ALIGNMENT;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

fn is_4kb_aligned(val: u64) -> bool {
    val % 4096 == 0
}

/// Set the process heap to a given size. Can both extend and shrink the heap.
pub fn set_heap_size(out_address: &mut u64, size: u64) -> ResultCode {
    log::trace!("svc::SetHeapSize called, heap_size=0x{:X}", size);

    let validation = validate_heap_size(size);
    if validation != RESULT_SUCCESS {
        return validation;
    }

    // Upstream note: This standalone function lacks system access.
    // Use set_heap_size_current_process instead.
    *out_address = 0;
    RESULT_NOT_IMPLEMENTED
}

fn validate_heap_size(size: u64) -> ResultCode {
    // Validate size.
    if size % (HEAP_SIZE_ALIGNMENT as u64) != 0 {
        return RESULT_INVALID_SIZE;
    }
    if size >= MAIN_MEMORY_SIZE_MAX {
        return RESULT_INVALID_SIZE;
    }
    RESULT_SUCCESS
}

/// Set the process heap to a given size using the current process page table.
///
/// Matches upstream ownership: current process -> page table -> SetHeapSize.
pub fn set_heap_size_current_process(
    system: &System,
    out_address: &mut u64,
    size: u64,
) -> ResultCode {
    let validation = validate_heap_size(size);
    if validation != RESULT_SUCCESS {
        *out_address = 0;
        return validation;
    }

    let (result, address) = system
        .current_process_arc()
        .lock()
        .unwrap()
        .set_heap_size(size as usize);
    *out_address = address.get();
    ResultCode::new(result)
}

/// Maps memory at a desired address.
pub fn map_physical_memory(system: &System, addr: u64, size: u64) -> ResultCode {
    log::debug!(
        "svc::MapPhysicalMemory called, addr=0x{:016X}, size=0x{:X}",
        addr,
        size
    );

    if !is_4kb_aligned(addr) {
        log::error!("Address is not aligned to 4KB, 0x{:016X}", addr);
        return RESULT_INVALID_ADDRESS;
    }
    if !is_4kb_aligned(size) {
        log::error!("Size is not aligned to 4KB, 0x{:X}", size);
        return RESULT_INVALID_SIZE;
    }
    if size == 0 {
        log::error!("Size is zero");
        return RESULT_INVALID_SIZE;
    }
    if addr >= addr.wrapping_add(size) {
        log::error!("Size causes 64-bit overflow of address");
        return RESULT_INVALID_MEMORY_REGION;
    }

    let mut process = system.current_process_arc().lock().unwrap();

    // Upstream: R_UNLESS(current_process->GetTotalSystemResourceSize() == 0, ResultInvalidState)
    // Note: upstream checks this is non-zero, which means if it IS zero, we fail.
    // For now, skip this check since system_resource_size is not tracked yet on KProcess fields.
    // Upstream note: Add total_system_resource_size field to KProcess and check here.

    let addr_kpa = crate::hle::kernel::k_typed_address::KProcessAddress::new(addr);
    if !process.page_table.contains(addr_kpa, size as usize) {
        log::error!(
            "Address is not within the address space, addr=0x{:016X}, size=0x{:016X}",
            addr,
            size
        );
        return RESULT_INVALID_MEMORY_REGION;
    }

    // The base page table's map_physical_memory checks is_in_alias_region internally.
    let result = process
        .page_table
        .map_physical_memory(addr_kpa, size as usize);
    ResultCode::new(result)
}

/// Unmaps memory previously mapped via MapPhysicalMemory.
pub fn unmap_physical_memory(system: &System, addr: u64, size: u64) -> ResultCode {
    log::debug!(
        "svc::UnmapPhysicalMemory called, addr=0x{:016X}, size=0x{:X}",
        addr,
        size
    );

    if !is_4kb_aligned(addr) {
        log::error!("Address is not aligned to 4KB, 0x{:016X}", addr);
        return RESULT_INVALID_ADDRESS;
    }
    if !is_4kb_aligned(size) {
        log::error!("Size is not aligned to 4KB, 0x{:X}", size);
        return RESULT_INVALID_SIZE;
    }
    if size == 0 {
        log::error!("Size is zero");
        return RESULT_INVALID_SIZE;
    }
    if addr >= addr.wrapping_add(size) {
        log::error!("Size causes 64-bit overflow of address");
        return RESULT_INVALID_MEMORY_REGION;
    }

    let mut process = system.current_process_arc().lock().unwrap();

    // Upstream note: Check GetTotalSystemResourceSize() == 0 => ResultInvalidState

    let addr_kpa = crate::hle::kernel::k_typed_address::KProcessAddress::new(addr);
    if !process.page_table.contains(addr_kpa, size as usize) {
        log::error!(
            "Address is not within the address space, addr=0x{:016X}, size=0x{:016X}",
            addr,
            size
        );
        return RESULT_INVALID_MEMORY_REGION;
    }

    // The base page table's unmap_physical_memory checks is_in_alias_region internally.
    let result = process
        .page_table
        .unmap_physical_memory(addr_kpa, size as usize);
    ResultCode::new(result)
}

/// Maps physical memory unsafely. (Upstream: UNIMPLEMENTED)
pub fn map_physical_memory_unsafe(_address: u64, _size: u64) -> ResultCode {
    log::warn!("svc::MapPhysicalMemoryUnsafe: Upstream UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}

/// Unmaps physical memory unsafely. (Upstream: UNIMPLEMENTED)
pub fn unmap_physical_memory_unsafe(_address: u64, _size: u64) -> ResultCode {
    log::warn!("svc::UnmapPhysicalMemoryUnsafe: Upstream UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}

/// Sets the unsafe memory limit. (Upstream: UNIMPLEMENTED)
pub fn set_unsafe_limit(_limit: u64) -> ResultCode {
    log::warn!("svc::SetUnsafeLimit: Upstream UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}
