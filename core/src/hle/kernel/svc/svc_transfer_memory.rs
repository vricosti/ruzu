//! Port of zuyu/src/core/hle/kernel/svc/svc_transfer_memory.cpp
//! Status: COMPLET (stubs for kernel calls)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for transfer memory operations.

use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::kernel::svc_common::Handle;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

fn is_valid_transfer_memory_permission(perm: MemoryPermission) -> bool {
    matches!(
        perm,
        MemoryPermission::None | MemoryPermission::Read | MemoryPermission::ReadWrite
    )
}

/// Creates a TransferMemory object.
pub fn create_transfer_memory(
    out: &mut Handle,
    address: u64,
    size: u64,
    map_perm: MemoryPermission,
) -> ResultCode {
    // Validate the size.
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

    // Validate the permissions.
    if !is_valid_transfer_memory_permission(map_perm) {
        return RESULT_INVALID_NEW_MEMORY_PERMISSION;
    }

    // TODO: Reserve transfer memory from resource limit.
    // TODO: KTransferMemory::Create, verify region in range, Initialize, Commit, Register.
    // TODO: Add to handle table.
    *out = 0;
    log::warn!("svc::CreateTransferMemory: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Maps a transfer memory object.
pub fn map_transfer_memory(
    trmem_handle: Handle,
    address: u64,
    size: u64,
    map_perm: MemoryPermission,
) -> ResultCode {
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
    if !is_valid_transfer_memory_permission(map_perm) {
        return RESULT_INVALID_STATE;
    }

    // TODO: Get transfer memory from handle.
    // TODO: Verify mapping is in range (CanContain for KMemoryState::Transferred).
    // TODO: trmem->Map(address, size, map_perm)
    log::warn!("svc::MapTransferMemory: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Unmaps a transfer memory object.
pub fn unmap_transfer_memory(trmem_handle: Handle, address: u64, size: u64) -> ResultCode {
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

    // TODO: Get transfer memory from handle.
    // TODO: Verify mapping is in range.
    // TODO: trmem->Unmap(address, size)
    log::warn!("svc::UnmapTransferMemory: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}
