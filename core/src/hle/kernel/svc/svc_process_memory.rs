//! Port of zuyu/src/core/hle/kernel/svc/svc_process_memory.cpp
//! Status: Ported
//! Derniere synchro: 2026-03-20
//!
//! SVC handlers for process memory operations.

use crate::core::System;
use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::kernel::svc_common::{Handle, PseudoHandle};
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
    system: &System,
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

    // Get the process from its handle.
    // For pseudo-handle CurrentProcess or self-handle, use current process.
    let process_arc = resolve_process_handle(system, process_handle);
    let process_arc = match process_arc {
        Some(p) => p,
        None => return RESULT_INVALID_HANDLE,
    };

    let mut process = process_arc.lock().unwrap();
    let addr_kpa = crate::hle::kernel::k_typed_address::KProcessAddress::new(address);

    // Validate that the address is in range.
    if !process.page_table.contains(addr_kpa, size as usize) {
        return RESULT_INVALID_CURRENT_MEMORY;
    }

    // Set the memory permission.
    let k_perm = crate::hle::kernel::k_memory_block::KMemoryPermission::from_bits_truncate(perm as u8);
    let result = process.page_table.set_process_memory_permission(addr_kpa, size as usize, k_perm);
    ResultCode::new(result)
}

/// Maps process memory from src_process to dst (current) process.
pub fn map_process_memory(
    system: &System,
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

    // Upstream: Get src process from handle, get dst and src page tables,
    // validate ranges, create page group, map.
    // In single-process mode, src and dst are the same process.
    let process_arc = resolve_process_handle(system, process_handle);
    let process_arc = match process_arc {
        Some(p) => p,
        None => return RESULT_INVALID_HANDLE,
    };

    let mut process = process_arc.lock().unwrap();
    let src_kpa = crate::hle::kernel::k_typed_address::KProcessAddress::new(src_address);
    let dst_kpa = crate::hle::kernel::k_typed_address::KProcessAddress::new(dst_address);

    // Validate that the source is in range.
    if !process.page_table.contains(src_kpa, size as usize) {
        return RESULT_INVALID_CURRENT_MEMORY;
    }

    // Upstream: dst_pt.CanContain(dst_address, size, KMemoryState::SharedCode)
    if !process.page_table.can_contain(
        dst_kpa,
        size as usize,
        crate::hle::kernel::k_memory_block::KMemoryState::SHARED_CODE,
    ) {
        return RESULT_INVALID_MEMORY_REGION;
    }

    // Upstream: Create page group from src, map to dst.
    // For now, do a direct memory copy mapping.
    let result = process.page_table.map_memory(dst_kpa, src_kpa, size as usize);
    ResultCode::new(result)
}

/// Unmaps process memory.
pub fn unmap_process_memory(
    system: &System,
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

    let process_arc = resolve_process_handle(system, process_handle);
    let process_arc = match process_arc {
        Some(p) => p,
        None => return RESULT_INVALID_HANDLE,
    };

    let mut process = process_arc.lock().unwrap();
    let src_kpa = crate::hle::kernel::k_typed_address::KProcessAddress::new(src_address);
    let dst_kpa = crate::hle::kernel::k_typed_address::KProcessAddress::new(dst_address);

    // Validate ranges.
    if !process.page_table.contains(src_kpa, size as usize) {
        return RESULT_INVALID_CURRENT_MEMORY;
    }
    if !process.page_table.can_contain(
        dst_kpa,
        size as usize,
        crate::hle::kernel::k_memory_block::KMemoryState::SHARED_CODE,
    ) {
        return RESULT_INVALID_MEMORY_REGION;
    }

    let result = process.page_table.unmap_memory(dst_kpa, src_kpa, size as usize);
    ResultCode::new(result)
}

/// Maps process code memory.
pub fn map_process_code_memory(
    system: &System,
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

    let process_arc = resolve_process_handle(system, process_handle);
    let process_arc = match process_arc {
        Some(p) => p,
        None => {
            log::error!("Invalid process handle specified (handle=0x{:08X})", process_handle);
            return RESULT_INVALID_HANDLE;
        }
    };

    let mut process = process_arc.lock().unwrap();
    let src_kpa = crate::hle::kernel::k_typed_address::KProcessAddress::new(src_address);

    if !process.page_table.contains(src_kpa, size as usize) {
        log::error!(
            "Source address range is not within the address space (0x{:016X}, 0x{:016X})",
            src_address, size
        );
        return RESULT_INVALID_CURRENT_MEMORY;
    }

    // Upstream: page_table.MapCodeMemory(dst_address, src_address, size)
    // Needs: KPageTableBase::map_code_memory
    let dst_kpa = crate::hle::kernel::k_typed_address::KProcessAddress::new(dst_address);
    let result = process.page_table.map_memory(dst_kpa, src_kpa, size as usize);
    ResultCode::new(result)
}

/// Unmaps process code memory.
pub fn unmap_process_code_memory(
    system: &System,
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

    let process_arc = resolve_process_handle(system, process_handle);
    let process_arc = match process_arc {
        Some(p) => p,
        None => {
            log::error!("Invalid process handle specified (handle=0x{:08X})", process_handle);
            return RESULT_INVALID_HANDLE;
        }
    };

    let mut process = process_arc.lock().unwrap();
    let src_kpa = crate::hle::kernel::k_typed_address::KProcessAddress::new(src_address);

    if !process.page_table.contains(src_kpa, size as usize) {
        log::error!(
            "Source address range is not within the address space (0x{:016X}, 0x{:016X})",
            src_address, size
        );
        return RESULT_INVALID_CURRENT_MEMORY;
    }

    // Upstream: page_table.UnmapCodeMemory(dst_address, src_address, size)
    let dst_kpa = crate::hle::kernel::k_typed_address::KProcessAddress::new(dst_address);
    let result = process.page_table.unmap_memory(dst_kpa, src_kpa, size as usize);
    ResultCode::new(result)
}

/// Helper: resolve a process handle to the process Arc.
/// For pseudo-handle CurrentProcess or handle 0, returns the current process.
/// For other handles, looks up in the handle table (in single-process mode,
/// all valid handles map to the current process).
fn resolve_process_handle(
    system: &System,
    handle: Handle,
) -> Option<std::sync::Arc<std::sync::Mutex<crate::hle::kernel::k_process::KProcess>>> {
    if handle == PseudoHandle::CurrentProcess as Handle || handle == 0 {
        return Some(system.current_process_arc().clone());
    }

    let current = system.current_process_arc();
    let guard = current.lock().unwrap();
    match guard.handle_table.get_object(handle) {
        Some(_) => {
            drop(guard);
            Some(system.current_process_arc().clone())
        }
        None => None,
    }
}
