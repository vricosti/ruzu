//! Port of zuyu/src/core/hle/kernel/svc/svc_shared_memory.cpp
//! Status: Ported — MapSharedMemory wired to real kernel objects.
//! Derniere synchro: 2026-03-19
//!
//! SVC handlers for shared memory operations.

use crate::core::System;
use crate::hle::kernel::k_memory_block::KMemoryState;
use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::kernel::svc_common::Handle;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

fn is_valid_shared_memory_permission(perm: MemoryPermission) -> bool {
    matches!(perm, MemoryPermission::Read | MemoryPermission::ReadWrite)
}

/// Convert SVC MemoryPermission to KSharedMemory MemoryPermission.
fn to_shmem_perm(
    perm: MemoryPermission,
) -> crate::hle::kernel::k_shared_memory::MemoryPermission {
    match perm {
        MemoryPermission::Read => crate::hle::kernel::k_shared_memory::MemoryPermission::Read,
        MemoryPermission::ReadWrite => {
            crate::hle::kernel::k_shared_memory::MemoryPermission::ReadWrite
        }
        _ => crate::hle::kernel::k_shared_memory::MemoryPermission::None,
    }
}

/// Maps shared memory into the current process.
///
/// Upstream: `svc_shared_memory.cpp: MapSharedMemory(Core::System&, Handle, u64, u64, MemoryPermission)`.
pub fn map_shared_memory(
    system: &System,
    shmem_handle: Handle,
    address: u64,
    size: u64,
    map_perm: MemoryPermission,
) -> ResultCode {
    log::info!(
        "svc::MapSharedMemory called, handle=0x{:X}, addr=0x{:X}, size=0x{:X}, perm={:?}",
        shmem_handle, address, size, map_perm
    );

    // Validate input.
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

    // Get the KSharedMemory from the handle table.
    let mut process = system.current_process_arc().lock().unwrap();

    let object_id = match process.handle_table.get_object(shmem_handle) {
        Some(id) => id,
        None => {
            log::error!(
                "svc::MapSharedMemory: handle {:#x} not in handle table",
                shmem_handle
            );
            return RESULT_INVALID_HANDLE;
        }
    };

    let shmem = match process.get_shared_memory_by_object_id(object_id) {
        Some(s) => s,
        None => {
            log::error!(
                "svc::MapSharedMemory: object_id {} is not a KSharedMemory",
                object_id
            );
            return RESULT_INVALID_HANDLE;
        }
    };

    // Validate that the address can contain shared memory.
    if !process
        .page_table
        .can_contain(
            crate::hle::kernel::k_typed_address::KProcessAddress::new(address),
            size as usize,
            KMemoryState::SHARED,
        )
    {
        log::error!(
            "svc::MapSharedMemory: address {:#x} size {:#x} cannot contain Shared",
            address, size
        );
        return RESULT_INVALID_CURRENT_MEMORY;
    }

    // Map the shared memory into the process page table.
    let result = shmem.map(
        &mut process.page_table,
        address,
        size as usize,
        to_shmem_perm(map_perm),
    );

    if result.is_success() {
        log::info!(
            "svc::MapSharedMemory: mapped handle={:#x} at {:#x} size={:#x} perm={:?}",
            shmem_handle, address, size, map_perm
        );
    } else {
        log::error!(
            "svc::MapSharedMemory: map failed, result={:?}",
            result
        );
    }

    result
}

/// Unmaps shared memory from the current process.
pub fn unmap_shared_memory(
    system: &System,
    shmem_handle: Handle,
    address: u64,
    size: u64,
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

    let mut process = system.current_process_arc().lock().unwrap();

    let object_id = match process.handle_table.get_object(shmem_handle) {
        Some(id) => id,
        None => return RESULT_INVALID_HANDLE,
    };

    let shmem = match process.get_shared_memory_by_object_id(object_id) {
        Some(s) => s,
        None => return RESULT_INVALID_HANDLE,
    };

    shmem.unmap(&mut process.page_table, address, size as usize)
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
