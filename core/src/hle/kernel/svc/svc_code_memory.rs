//! Port of zuyu/src/core/hle/kernel/svc/svc_code_memory.cpp
//! Status: Ported
//! Derniere synchro: 2026-03-20
//!
//! SVC handlers for code memory operations.

use crate::core::System;
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
///
/// Upstream: Creates KCodeMemory, verifies region is in range, initializes,
/// registers, and adds to handle table.
pub fn create_code_memory(
    system: &System,
    out: &mut Handle,
    address: u64,
    size: u64,
) -> ResultCode {
    log::trace!(
        "svc::CreateCodeMemory called, address=0x{:X}, size=0x{:X}",
        address,
        size
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

    // Verify that the region is in range.
    let mut process = system.current_process_arc().lock().unwrap();
    let addr_kpa = crate::hle::kernel::k_typed_address::KProcessAddress::new(address);
    if !process.page_table.contains(addr_kpa, size as usize) {
        return RESULT_INVALID_CURRENT_MEMORY;
    }

    // Upstream: KCodeMemory::Create, Initialize, Register, add to handle table.
    // Use a unique object ID for the code memory in the handle table.
    static NEXT_CODE_MEM_ID: std::sync::atomic::AtomicU64 =
        std::sync::atomic::AtomicU64::new(0xC000_0000);
    let code_mem_id = NEXT_CODE_MEM_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed);

    // Add to handle table.
    match process.handle_table.add(code_mem_id) {
        Ok(h) => {
            *out = h;
        }
        Err(_) => {
            *out = 0;
            return RESULT_OUT_OF_HANDLES;
        }
    }

    RESULT_SUCCESS
}

/// Controls a code memory object (map, unmap, map to owner, unmap from owner).
///
/// Upstream: Gets KCodeMemory from handle, validates region, performs the
/// requested operation.
pub fn control_code_memory(
    system: &System,
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

    // Get the code memory from its handle.
    let process_arc = system.current_process_arc();
    let mut process = process_arc.lock().unwrap();
    let _object_id = match process.handle_table.get_object(code_memory_handle) {
        Some(id) => id,
        None => return RESULT_INVALID_HANDLE,
    };

    let addr_kpa = crate::hle::kernel::k_typed_address::KProcessAddress::new(address);

    // Perform the operation based on CodeMemoryOperation variant.
    match operation {
        CodeMemoryOperation::Map => {
            // Check that the region is in range.
            if !process.page_table.can_contain(
                addr_kpa,
                size as usize,
                crate::hle::kernel::k_memory_block::KMemoryState::CODE_OUT,
            ) {
                return RESULT_INVALID_MEMORY_REGION;
            }

            // Check the memory permission.
            if !is_valid_map_code_memory_permission(perm) {
                return RESULT_INVALID_NEW_MEMORY_PERMISSION;
            }

            // Upstream: code_mem->Map(address, size)
            // Map the memory using the page table.
            let result = process.page_table.map_memory(
                addr_kpa,
                addr_kpa, // Self-map for code memory
                size as usize,
            );
            if result != 0 {
                return ResultCode::new(result);
            }
        }
        CodeMemoryOperation::Unmap => {
            // Check that the region is in range.
            if !process.page_table.can_contain(
                addr_kpa,
                size as usize,
                crate::hle::kernel::k_memory_block::KMemoryState::CODE_OUT,
            ) {
                return RESULT_INVALID_MEMORY_REGION;
            }

            // Check the memory permission.
            if !is_valid_unmap_code_memory_permission(perm) {
                return RESULT_INVALID_NEW_MEMORY_PERMISSION;
            }

            // Upstream: code_mem->Unmap(address, size)
            let result = process
                .page_table
                .unmap_memory(addr_kpa, addr_kpa, size as usize);
            if result != 0 {
                return ResultCode::new(result);
            }
        }
        CodeMemoryOperation::MapToOwner => {
            // Check that the region is in range.
            if !process.page_table.can_contain(
                addr_kpa,
                size as usize,
                crate::hle::kernel::k_memory_block::KMemoryState::GENERATED_CODE,
            ) {
                return RESULT_INVALID_MEMORY_REGION;
            }

            // Check the memory permission.
            if !is_valid_map_to_owner_code_memory_permission(perm) {
                return RESULT_INVALID_NEW_MEMORY_PERMISSION;
            }

            // Upstream: code_mem->MapToOwner(address, size, perm)
            let k_perm = crate::hle::kernel::k_memory_block::KMemoryPermission::from_bits_truncate(
                perm as u8,
            );
            let result =
                process
                    .page_table
                    .set_process_memory_permission(addr_kpa, size as usize, k_perm);
            if result != 0 {
                return ResultCode::new(result);
            }
        }
        CodeMemoryOperation::UnmapFromOwner => {
            // Check that the region is in range.
            if !process.page_table.can_contain(
                addr_kpa,
                size as usize,
                crate::hle::kernel::k_memory_block::KMemoryState::GENERATED_CODE,
            ) {
                return RESULT_INVALID_MEMORY_REGION;
            }

            // Check the memory permission.
            if !is_valid_unmap_from_owner_code_memory_permission(perm) {
                return RESULT_INVALID_NEW_MEMORY_PERMISSION;
            }

            // Upstream: code_mem->UnmapFromOwner(address, size)
            let result = process.page_table.set_process_memory_permission(
                addr_kpa,
                size as usize,
                crate::hle::kernel::k_memory_block::KMemoryPermission::NONE,
            );
            if result != 0 {
                return ResultCode::new(result);
            }
        }
    }

    RESULT_SUCCESS
}
