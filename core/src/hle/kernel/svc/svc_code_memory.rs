//! Port of zuyu/src/core/hle/kernel/svc/svc_code_memory.cpp
//! Status: COMPLET (stubs for kernel calls)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for code memory operations.

use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::kernel::svc_common::Handle;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

fn is_valid_map_code_memory_permission(perm: MemoryPermission) -> bool {
    matches!(perm, MemoryPermission::ReadWrite)
}

fn is_valid_map_to_owner_code_memory_permission(perm: MemoryPermission) -> bool {
    matches!(perm, MemoryPermission::Read | MemoryPermission::ReadExecute)
}

fn is_valid_unmap_code_memory_permission(perm: MemoryPermission) -> bool {
    matches!(perm, MemoryPermission::None)
}

fn is_valid_unmap_from_owner_code_memory_permission(perm: MemoryPermission) -> bool {
    matches!(perm, MemoryPermission::None)
}

/// Creates a code memory object.
pub fn create_code_memory(out: &mut Handle, address: u64, size: u64) -> ResultCode {
    log::trace!(
        "svc::CreateCodeMemory called, address=0x{:X}, size=0x{:X}",
        address, size
    );

    // Validate address / size.
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

    // TODO: KCodeMemory::Create, Initialize, Register, add to handle table
    log::warn!("svc::CreateCodeMemory: kernel object access not yet implemented");
    *out = 0;
    RESULT_NOT_IMPLEMENTED
}

/// Controls a code memory object (map, unmap, map to owner, unmap from owner).
pub fn control_code_memory(
    code_memory_handle: Handle,
    operation: CodeMemoryOperation,
    address: u64,
    size: u64,
    perm: MemoryPermission,
) -> ResultCode {
    log::trace!(
        "svc::ControlCodeMemory called, handle=0x{:X}, op={:?}, addr=0x{:X}, size=0x{:X}, perm={:?}",
        code_memory_handle, operation, address, size, perm
    );

    // Validate the address / size.
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

    // TODO: Get the code memory from its handle.
    // TODO: Perform the operation based on CodeMemoryOperation variant.
    match operation {
        CodeMemoryOperation::Map => {
            if !is_valid_map_code_memory_permission(perm) {
                return RESULT_INVALID_NEW_MEMORY_PERMISSION;
            }
            // TODO: code_mem->Map(address, size)
        }
        CodeMemoryOperation::Unmap => {
            if !is_valid_unmap_code_memory_permission(perm) {
                return RESULT_INVALID_NEW_MEMORY_PERMISSION;
            }
            // TODO: code_mem->Unmap(address, size)
        }
        CodeMemoryOperation::MapToOwner => {
            if !is_valid_map_to_owner_code_memory_permission(perm) {
                return RESULT_INVALID_NEW_MEMORY_PERMISSION;
            }
            // TODO: code_mem->MapToOwner(address, size, perm)
        }
        CodeMemoryOperation::UnmapFromOwner => {
            if !is_valid_unmap_from_owner_code_memory_permission(perm) {
                return RESULT_INVALID_NEW_MEMORY_PERMISSION;
            }
            // TODO: code_mem->UnmapFromOwner(address, size)
        }
    }

    log::warn!("svc::ControlCodeMemory: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}
