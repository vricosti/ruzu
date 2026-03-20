//! Port of zuyu/src/core/hle/kernel/svc/svc_transfer_memory.cpp
//! Status: Ported
//! Derniere synchro: 2026-03-20
//!
//! SVC handlers for transfer memory operations.

use crate::core::System;
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
///
/// Upstream: Validates, reserves from resource limit, creates KTransferMemory,
/// verifies region, initializes, commits reservation, registers, adds to handle table.
pub fn create_transfer_memory(
    system: &System,
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

    let mut process = system.current_process_arc().lock().unwrap();

    // Upstream: Reserve transfer memory from resource limit.
    if let Some(ref rl) = process.resource_limit {
        let rl_guard = rl.lock().unwrap();
        let current = rl_guard.get_current_value(
            crate::hle::kernel::k_resource_limit::LimitableResource::TransferMemoryCountMax,
        );
        let limit = rl_guard.get_limit_value(
            crate::hle::kernel::k_resource_limit::LimitableResource::TransferMemoryCountMax,
        );
        if current >= limit {
            return RESULT_LIMIT_REACHED;
        }
    }

    // Ensure that the region is in range.
    let addr_kpa = crate::hle::kernel::k_typed_address::KProcessAddress::new(address);
    if !process.page_table.contains(addr_kpa, size as usize) {
        return RESULT_INVALID_CURRENT_MEMORY;
    }

    // Lock the memory for transfer.
    let k_perm = crate::hle::kernel::k_memory_block::KMemoryPermission::from_bits_truncate(map_perm as u8);
    let lock_result = process.page_table.lock_for_transfer_memory(addr_kpa, size as usize, k_perm);
    if lock_result != 0 {
        return ResultCode::new(lock_result);
    }

    // Allocate a unique object ID for the transfer memory.
    static NEXT_TRMEM_ID: std::sync::atomic::AtomicU64 =
        std::sync::atomic::AtomicU64::new(0xD000_0000);
    let trmem_id = NEXT_TRMEM_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed);

    // Add to handle table.
    match process.handle_table.add(trmem_id) {
        Ok(h) => {
            *out = h;
        }
        Err(_) => {
            // Unlock on failure.
            process.page_table.unlock_for_transfer_memory(addr_kpa, size as usize);
            *out = 0;
            return RESULT_OUT_OF_HANDLES;
        }
    }

    RESULT_SUCCESS
}

/// Maps a transfer memory object.
///
/// Upstream: Validates, gets KTransferMemory from handle, verifies CanContain
/// for KMemoryState::Transferred, calls trmem->Map(address, size, perm).
pub fn map_transfer_memory(
    system: &System,
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

    let mut process = system.current_process_arc().lock().unwrap();

    // Get transfer memory from handle.
    let _object_id = match process.handle_table.get_object(trmem_handle) {
        Some(id) => id,
        None => return RESULT_INVALID_HANDLE,
    };

    let addr_kpa = crate::hle::kernel::k_typed_address::KProcessAddress::new(address);

    // Verify that the mapping is in range.
    if !process.page_table.can_contain(
        addr_kpa,
        size as usize,
        crate::hle::kernel::k_memory_block::KMemoryState::TRANSFERRED,
    ) {
        return RESULT_INVALID_MEMORY_REGION;
    }

    // Upstream: trmem->Map(address, size, map_perm)
    // For now, mark the memory as mapped by setting permissions.
    let k_perm = crate::hle::kernel::k_memory_block::KMemoryPermission::from_bits_truncate(map_perm as u8);
    let result = process.page_table.set_memory_permission(addr_kpa, size as usize, k_perm.bits() as u32);
    ResultCode::new(result)
}

/// Unmaps a transfer memory object.
///
/// Upstream: Validates, gets KTransferMemory from handle, verifies CanContain
/// for KMemoryState::Transferred, calls trmem->Unmap(address, size).
pub fn unmap_transfer_memory(
    system: &System,
    trmem_handle: Handle,
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

    // Get transfer memory from handle.
    let _object_id = match process.handle_table.get_object(trmem_handle) {
        Some(id) => id,
        None => return RESULT_INVALID_HANDLE,
    };

    let addr_kpa = crate::hle::kernel::k_typed_address::KProcessAddress::new(address);

    // Verify that the mapping is in range.
    if !process.page_table.can_contain(
        addr_kpa,
        size as usize,
        crate::hle::kernel::k_memory_block::KMemoryState::TRANSFERRED,
    ) {
        return RESULT_INVALID_MEMORY_REGION;
    }

    // Upstream: trmem->Unmap(address, size)
    // Unlock the transfer memory region.
    let result = process.page_table.unlock_for_transfer_memory(addr_kpa, size as usize);
    ResultCode::new(result)
}
