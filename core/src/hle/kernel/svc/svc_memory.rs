//! Port of zuyu/src/core/hle/kernel/svc/svc_memory.cpp
//! Status: Ported
//! Derniere synchro: 2026-03-20
//!
//! SVC handlers for memory operations (SetMemoryPermission, SetMemoryAttribute,
//! MapMemory, UnmapMemory).

use crate::core::System;
use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

fn is_valid_set_memory_permission(perm: MemoryPermission) -> bool {
    matches!(
        perm,
        MemoryPermission::None | MemoryPermission::Read | MemoryPermission::ReadWrite
    )
}

/// Checks if address + size > address (i.e., no overflow and size > 0).
fn is_valid_address_range(address: u64, size: u64) -> bool {
    address.checked_add(size).map_or(false, |end| end > address)
}

fn is_4kb_aligned(val: u64) -> bool {
    val % 4096 == 0
}

/// Helper function that performs the common sanity checks for svcMapMemory
/// and svcUnmapMemory.
fn map_unmap_memory_sanity_checks(
    system: &System,
    dst_addr: u64,
    src_addr: u64,
    size: u64,
) -> ResultCode {
    if !is_4kb_aligned(dst_addr) {
        log::error!(
            "Destination address is not aligned to 4KB, 0x{:016X}",
            dst_addr
        );
        return RESULT_INVALID_ADDRESS;
    }
    if !is_4kb_aligned(src_addr) {
        log::error!("Source address is not aligned to 4KB, 0x{:016X}", src_addr);
        return RESULT_INVALID_SIZE;
    }
    if size == 0 {
        log::error!("Size is 0");
        return RESULT_INVALID_SIZE;
    }
    if !is_4kb_aligned(size) {
        log::error!("Size is not aligned to 4KB, 0x{:016X}", size);
        return RESULT_INVALID_SIZE;
    }
    if !is_valid_address_range(dst_addr, size) {
        log::error!(
            "Destination is not a valid address range, addr=0x{:016X}, size=0x{:016X}",
            dst_addr,
            size
        );
        return RESULT_INVALID_CURRENT_MEMORY;
    }
    if !is_valid_address_range(src_addr, size) {
        log::error!(
            "Source is not a valid address range, addr=0x{:016X}, size=0x{:016X}",
            src_addr,
            size
        );
        return RESULT_INVALID_CURRENT_MEMORY;
    }

    // Check that the source range is within the current process address space.
    let process = system.current_process_arc().lock().unwrap();
    let src_addr_kpa = crate::hle::kernel::k_typed_address::KProcessAddress::new(src_addr);
    if !process.page_table.contains(src_addr_kpa, size as usize) {
        log::error!(
            "Source is not within the address space, addr=0x{:016X}, size=0x{:016X}",
            src_addr,
            size
        );
        return RESULT_INVALID_CURRENT_MEMORY;
    }

    RESULT_SUCCESS
}

/// Sets memory permissions.
pub fn set_memory_permission(
    system: &System,
    address: u64,
    size: u64,
    perm: MemoryPermission,
) -> ResultCode {
    log::debug!(
        "svc::SetMemoryPermission called, address=0x{:016X}, size=0x{:X}, perm={:?}",
        address,
        size,
        perm
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
    if !is_valid_set_memory_permission(perm) {
        return RESULT_INVALID_NEW_MEMORY_PERMISSION;
    }

    // Validate that the region is in range for the current process.
    let mut process = system.current_process_arc().lock().unwrap();
    let addr_kpa = crate::hle::kernel::k_typed_address::KProcessAddress::new(address);
    if !process.page_table.contains(addr_kpa, size as usize) {
        return RESULT_INVALID_CURRENT_MEMORY;
    }

    // Set the memory permission.
    let result = process
        .page_table
        .set_memory_permission(addr_kpa, size as usize, perm as u32);
    ResultCode::new(result)
}

/// Sets memory attributes (uncached, permission-locked).
pub fn set_memory_attribute(
    system: &System,
    address: u64,
    size: u64,
    mask: u32,
    attr: u32,
) -> ResultCode {
    log::debug!(
        "svc::SetMemoryAttribute called, address=0x{:016X}, size=0x{:X}, mask=0x{:08X}, attr=0x{:08X}",
        address, size, mask, attr
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

    // Validate the attribute and mask.
    let supported_mask =
        (MemoryAttribute::Uncached as u32) | (MemoryAttribute::PermissionLocked as u32);
    if (mask | attr) != mask {
        return RESULT_INVALID_COMBINATION;
    }
    if (mask | attr | supported_mask) != supported_mask {
        return RESULT_INVALID_COMBINATION;
    }

    // Check that permission locked is either being set or not masked.
    let perm_locked = MemoryAttribute::PermissionLocked as u32;
    if (mask & perm_locked) != (attr & perm_locked) {
        return RESULT_INVALID_COMBINATION;
    }

    // Validate that the region is in range for the current process.
    let mut process = system.current_process_arc().lock().unwrap();
    let addr_kpa = crate::hle::kernel::k_typed_address::KProcessAddress::new(address);
    if !process.page_table.contains(addr_kpa, size as usize) {
        return RESULT_INVALID_CURRENT_MEMORY;
    }

    // Set the memory attribute.
    let result = process
        .page_table
        .set_memory_attribute(addr_kpa, size as usize, mask, attr);
    ResultCode::new(result)
}

/// Maps a memory range into a different range.
pub fn map_memory(system: &System, dst_addr: u64, src_addr: u64, size: u64) -> ResultCode {
    log::info!(
        "svc::MapMemory called, dst_addr=0x{:X}, src_addr=0x{:X}, size=0x{:X}",
        dst_addr,
        src_addr,
        size
    );

    let result = map_unmap_memory_sanity_checks(system, dst_addr, src_addr, size);
    if result != RESULT_SUCCESS {
        return result;
    }

    let mut process = system.current_process_arc().lock().unwrap();
    let dst_kpa = crate::hle::kernel::k_typed_address::KProcessAddress::new(dst_addr);
    let src_kpa = crate::hle::kernel::k_typed_address::KProcessAddress::new(src_addr);
    let r = process.page_table.map_memory(dst_kpa, src_kpa, size as usize);
    ResultCode::new(r)
}

/// Unmaps a region that was previously mapped with svcMapMemory.
pub fn unmap_memory(system: &System, dst_addr: u64, src_addr: u64, size: u64) -> ResultCode {
    log::trace!(
        "svc::UnmapMemory called, dst_addr=0x{:X}, src_addr=0x{:X}, size=0x{:X}",
        dst_addr,
        src_addr,
        size
    );

    let result = map_unmap_memory_sanity_checks(system, dst_addr, src_addr, size);
    if result != RESULT_SUCCESS {
        return result;
    }

    let mut process = system.current_process_arc().lock().unwrap();
    let dst_kpa = crate::hle::kernel::k_typed_address::KProcessAddress::new(dst_addr);
    let src_kpa = crate::hle::kernel::k_typed_address::KProcessAddress::new(src_addr);
    let r = process
        .page_table
        .unmap_memory(dst_kpa, src_kpa, size as usize);
    ResultCode::new(r)
}
