//! Port of zuyu/src/core/hle/kernel/svc/svc_shared_memory.cpp
//! Status: COMPLET (stubs for kernel calls)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for shared memory operations.

use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::kernel::svc_common::Handle;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

fn is_valid_shared_memory_permission(perm: MemoryPermission) -> bool {
    matches!(perm, MemoryPermission::Read | MemoryPermission::ReadWrite)
}

/// Maps shared memory into the current process.
pub fn map_shared_memory(
    shmem_handle: Handle,
    address: u64,
    size: u64,
    map_perm: MemoryPermission,
) -> ResultCode {
    log::trace!(
        "svc::MapSharedMemory called, handle=0x{:X}, addr=0x{:X}, size=0x{:X}, perm={:?}",
        shmem_handle, address, size, map_perm
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
    if !is_valid_shared_memory_permission(map_perm) {
        return RESULT_INVALID_NEW_MEMORY_PERMISSION;
    }

    // TODO: Get shared memory from handle.
    // TODO: Verify mapping is in range (CanContain).
    // TODO: process.AddSharedMemory, shmem->Map
    log::warn!("svc::MapSharedMemory: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Unmaps shared memory from the current process.
pub fn unmap_shared_memory(shmem_handle: Handle, address: u64, size: u64) -> ResultCode {
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

    // TODO: Get shared memory from handle.
    // TODO: Verify mapping is in range.
    // TODO: shmem->Unmap, process.RemoveSharedMemory
    log::warn!("svc::UnmapSharedMemory: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Creates shared memory. (Unimplemented upstream.)
pub fn create_shared_memory(
    _out_handle: &mut Handle,
    _size: u64,
    _owner_perm: MemoryPermission,
    _remote_perm: MemoryPermission,
) -> ResultCode {
    log::warn!("svc::CreateSharedMemory: UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}
