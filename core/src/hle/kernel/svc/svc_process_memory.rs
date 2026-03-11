//! Port of zuyu/src/core/hle/kernel/svc/svc_process_memory.cpp
//! Status: COMPLET (stubs for kernel calls)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for process memory operations.

use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::kernel::svc_common::Handle;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

fn is_valid_address_range(address: u64, size: u64) -> bool {
    address.checked_add(size).map_or(false, |end| end > address)
}

fn is_valid_process_memory_permission(perm: MemoryPermission) -> bool {
    matches!(
        perm,
        MemoryPermission::None
            | MemoryPermission::Read
            | MemoryPermission::ReadWrite
            | MemoryPermission::ReadExecute
    )
}

fn is_4kb_aligned(val: u64) -> bool {
    val % 4096 == 0
}

/// Sets process memory permission.
pub fn set_process_memory_permission(
    process_handle: Handle,
    address: u64,
    size: u64,
    perm: MemoryPermission,
) -> ResultCode {
    log::trace!(
        "svc::SetProcessMemoryPermission called, handle=0x{:X}, addr=0x{:X}, size=0x{:X}, perm={:?}",
        process_handle, address, size, perm
    );

    if address % PAGE_SIZE != 0 {
        return RESULT_INVALID_ADDRESS;
    }
    if size % PAGE_SIZE != 0 {
        return RESULT_INVALID_SIZE;
    }
    if size == 0 {
        return RESULT_INVALID_SIZE;
    }
    if address >= address.wrapping_add(size) {
        return RESULT_INVALID_CURRENT_MEMORY;
    }
    if !is_valid_process_memory_permission(perm) {
        return RESULT_INVALID_NEW_MEMORY_PERMISSION;
    }

    // TODO: Get process from handle, validate range, set permission
    log::warn!("svc::SetProcessMemoryPermission: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Maps process memory from src_process to dst (current) process.
pub fn map_process_memory(
    dst_address: u64,
    process_handle: Handle,
    src_address: u64,
    size: u64,
) -> ResultCode {
    log::trace!(
        "svc::MapProcessMemory called, dst=0x{:X}, handle=0x{:X}, src=0x{:X}, size=0x{:X}",
        dst_address, process_handle, src_address, size
    );

    if dst_address % PAGE_SIZE != 0 {
        return RESULT_INVALID_ADDRESS;
    }
    if src_address % PAGE_SIZE != 0 {
        return RESULT_INVALID_ADDRESS;
    }
    if size % PAGE_SIZE != 0 {
        return RESULT_INVALID_SIZE;
    }
    if size == 0 {
        return RESULT_INVALID_SIZE;
    }
    if dst_address >= dst_address.wrapping_add(size) {
        return RESULT_INVALID_CURRENT_MEMORY;
    }
    if src_address >= src_address.wrapping_add(size) {
        return RESULT_INVALID_CURRENT_MEMORY;
    }

    // TODO: Get processes, validate ranges, create page group, map
    log::warn!("svc::MapProcessMemory: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Unmaps process memory.
pub fn unmap_process_memory(
    dst_address: u64,
    process_handle: Handle,
    src_address: u64,
    size: u64,
) -> ResultCode {
    log::trace!(
        "svc::UnmapProcessMemory called, dst=0x{:X}, handle=0x{:X}, src=0x{:X}, size=0x{:X}",
        dst_address, process_handle, src_address, size
    );

    if dst_address % PAGE_SIZE != 0 {
        return RESULT_INVALID_ADDRESS;
    }
    if src_address % PAGE_SIZE != 0 {
        return RESULT_INVALID_ADDRESS;
    }
    if size % PAGE_SIZE != 0 {
        return RESULT_INVALID_SIZE;
    }
    if size == 0 {
        return RESULT_INVALID_SIZE;
    }
    if dst_address >= dst_address.wrapping_add(size) {
        return RESULT_INVALID_CURRENT_MEMORY;
    }
    if src_address >= src_address.wrapping_add(size) {
        return RESULT_INVALID_CURRENT_MEMORY;
    }

    // TODO: Get processes, validate ranges, unmap
    log::warn!("svc::UnmapProcessMemory: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Maps process code memory.
pub fn map_process_code_memory(
    process_handle: Handle,
    dst_address: u64,
    src_address: u64,
    size: u64,
) -> ResultCode {
    log::debug!(
        "svc::MapProcessCodeMemory called. handle=0x{:08X}, dst=0x{:016X}, src=0x{:016X}, size=0x{:016X}",
        process_handle, dst_address, src_address, size
    );

    if !is_4kb_aligned(src_address) {
        log::error!("src_address is not page-aligned (0x{:016X})", src_address);
        return RESULT_INVALID_ADDRESS;
    }
    if !is_4kb_aligned(dst_address) {
        log::error!("dst_address is not page-aligned (0x{:016X})", dst_address);
        return RESULT_INVALID_ADDRESS;
    }
    if size == 0 || !is_4kb_aligned(size) {
        log::error!("Size is zero or not page-aligned (0x{:016X})", size);
        return RESULT_INVALID_SIZE;
    }
    if !is_valid_address_range(dst_address, size) {
        log::error!("Destination address range overflows (0x{:016X}, 0x{:016X})", dst_address, size);
        return RESULT_INVALID_CURRENT_MEMORY;
    }
    if !is_valid_address_range(src_address, size) {
        log::error!("Source address range overflows (0x{:016X}, 0x{:016X})", src_address, size);
        return RESULT_INVALID_CURRENT_MEMORY;
    }

    // TODO: Get process from handle, verify ranges, map code memory
    log::warn!("svc::MapProcessCodeMemory: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Unmaps process code memory.
pub fn unmap_process_code_memory(
    process_handle: Handle,
    dst_address: u64,
    src_address: u64,
    size: u64,
) -> ResultCode {
    log::debug!(
        "svc::UnmapProcessCodeMemory called. handle=0x{:08X}, dst=0x{:016X}, src=0x{:016X}, size=0x{:016X}",
        process_handle, dst_address, src_address, size
    );

    if !is_4kb_aligned(dst_address) {
        log::error!("dst_address is not page-aligned (0x{:016X})", dst_address);
        return RESULT_INVALID_ADDRESS;
    }
    if !is_4kb_aligned(src_address) {
        log::error!("src_address is not page-aligned (0x{:016X})", src_address);
        return RESULT_INVALID_ADDRESS;
    }
    if size == 0 || !is_4kb_aligned(size) {
        log::error!("Size is zero or not page-aligned (0x{:016X})", size);
        return RESULT_INVALID_SIZE;
    }
    if !is_valid_address_range(dst_address, size) {
        log::error!("Destination address range overflows (0x{:016X}, 0x{:016X})", dst_address, size);
        return RESULT_INVALID_CURRENT_MEMORY;
    }
    if !is_valid_address_range(src_address, size) {
        log::error!("Source address range overflows (0x{:016X}, 0x{:016X})", src_address, size);
        return RESULT_INVALID_CURRENT_MEMORY;
    }

    // TODO: Get process from handle, verify ranges, unmap code memory
    log::warn!("svc::UnmapProcessCodeMemory: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}
