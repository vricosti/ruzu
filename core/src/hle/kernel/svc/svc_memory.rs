//! Port of zuyu/src/core/hle/kernel/svc/svc_memory.cpp
//! Status: COMPLET (stubs for kernel calls)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for memory operations (SetMemoryPermission, SetMemoryAttribute,
//! MapMemory, UnmapMemory).

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
    dst_addr: u64,
    src_addr: u64,
    size: u64,
) -> ResultCode {
    if !is_4kb_aligned(dst_addr) {
        log::error!("Destination address is not aligned to 4KB, 0x{:016X}", dst_addr);
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
        log::error!("Destination is not a valid address range, addr=0x{:016X}, size=0x{:016X}", dst_addr, size);
        return RESULT_INVALID_CURRENT_MEMORY;
    }
    if !is_valid_address_range(src_addr, size) {
        log::error!("Source is not a valid address range, addr=0x{:016X}, size=0x{:016X}", src_addr, size);
        return RESULT_INVALID_CURRENT_MEMORY;
    }

    // TODO: R_UNLESS(manager.Contains(src_addr, size), ResultInvalidCurrentMemory)
    RESULT_SUCCESS
}

/// Sets memory permissions.
pub fn set_memory_permission(address: u64, size: u64, perm: MemoryPermission) -> ResultCode {
    log::debug!(
        "svc::SetMemoryPermission called, address=0x{:016X}, size=0x{:X}, perm={:?}",
        address, size, perm
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

    // TODO: page_table.Contains(address, size) check
    // TODO: page_table.SetMemoryPermission(address, size, perm)
    log::warn!("svc::SetMemoryPermission: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Sets memory attributes (uncached, permission-locked).
pub fn set_memory_attribute(address: u64, size: u64, mask: u32, attr: u32) -> ResultCode {
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
    let supported_mask = (MemoryAttribute::Uncached as u32) | (MemoryAttribute::PermissionLocked as u32);
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

    // TODO: page_table.Contains(address, size) check
    // TODO: page_table.SetMemoryAttribute(address, size, mask, attr)
    log::warn!("svc::SetMemoryAttribute: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Maps a memory range into a different range.
pub fn map_memory(dst_addr: u64, src_addr: u64, size: u64) -> ResultCode {
    log::trace!(
        "svc::MapMemory called, dst_addr=0x{:X}, src_addr=0x{:X}, size=0x{:X}",
        dst_addr, src_addr, size
    );

    let result = map_unmap_memory_sanity_checks(dst_addr, src_addr, size);
    if result != RESULT_SUCCESS {
        return result;
    }

    // TODO: page_table.MapMemory(dst_addr, src_addr, size)
    log::warn!("svc::MapMemory: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Unmaps a region that was previously mapped with svcMapMemory.
pub fn unmap_memory(dst_addr: u64, src_addr: u64, size: u64) -> ResultCode {
    log::trace!(
        "svc::UnmapMemory called, dst_addr=0x{:X}, src_addr=0x{:X}, size=0x{:X}",
        dst_addr, src_addr, size
    );

    let result = map_unmap_memory_sanity_checks(dst_addr, src_addr, size);
    if result != RESULT_SUCCESS {
        return result;
    }

    // TODO: page_table.UnmapMemory(dst_addr, src_addr, size)
    log::warn!("svc::UnmapMemory: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}
