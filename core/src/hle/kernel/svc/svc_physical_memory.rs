//! Port of zuyu/src/core/hle/kernel/svc/svc_physical_memory.cpp
//! Status: COMPLET (stubs for kernel calls)
//! Derniere synchro: 2026-03-11
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

    // TODO: GetCurrentProcess(kernel).GetPageTable().SetHeapSize(&address, size)
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

    let (result, address) = system.current_process_arc().lock().unwrap().set_heap_size(size as usize);
    *out_address = address.get();
    ResultCode::new(result)
}

/// Maps memory at a desired address.
pub fn map_physical_memory(addr: u64, size: u64) -> ResultCode {
    log::debug!("svc::MapPhysicalMemory called, addr=0x{:016X}, size=0x{:X}", addr, size);

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

    // TODO: Check GetTotalSystemResourceSize() == 0 => ResultInvalidState
    // TODO: page_table.Contains(addr, size) and IsInAliasRegion checks
    // TODO: page_table.MapPhysicalMemory(addr, size)
    log::warn!("svc::MapPhysicalMemory: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Unmaps memory previously mapped via MapPhysicalMemory.
pub fn unmap_physical_memory(addr: u64, size: u64) -> ResultCode {
    log::debug!("svc::UnmapPhysicalMemory called, addr=0x{:016X}, size=0x{:X}", addr, size);

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

    // TODO: Check GetTotalSystemResourceSize() == 0 => ResultInvalidState
    // TODO: page_table.Contains and IsInAliasRegion checks
    // TODO: page_table.UnmapPhysicalMemory(addr, size)
    log::warn!("svc::UnmapPhysicalMemory: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Maps physical memory unsafely. (Unimplemented upstream.)
pub fn map_physical_memory_unsafe(_address: u64, _size: u64) -> ResultCode {
    log::warn!("svc::MapPhysicalMemoryUnsafe: UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}

/// Unmaps physical memory unsafely. (Unimplemented upstream.)
pub fn unmap_physical_memory_unsafe(_address: u64, _size: u64) -> ResultCode {
    log::warn!("svc::UnmapPhysicalMemoryUnsafe: UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}

/// Sets the unsafe memory limit. (Unimplemented upstream.)
pub fn set_unsafe_limit(_limit: u64) -> ResultCode {
    log::warn!("svc::SetUnsafeLimit: UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}
