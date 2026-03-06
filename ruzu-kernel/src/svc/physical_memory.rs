// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use common::{error, is_page_aligned, ResultCode, VAddr};
use log::debug;

use crate::kernel::KernelCore;
use crate::memory_manager::MemoryPermission;

/// SVC 0x2C: MapPhysicalMemory
///
/// Maps physical memory at the given address with READ_WRITE permissions.
/// Used by games for GPU buffer allocations and large data regions.
pub fn svc_map_physical_memory(kernel: &mut KernelCore, addr: VAddr, size: u64) -> ResultCode {
    debug!("MapPhysicalMemory: addr=0x{:X}, size=0x{:X}", addr, size);

    if !is_page_aligned(addr) || !is_page_aligned(size) || size == 0 {
        return error::INVALID_SIZE;
    }

    let process = match kernel.process_mut() {
        Some(p) => p,
        None => return error::INVALID_STATE,
    };

    // Check if the region is already mapped (partially or fully).
    // If so, just return success — some games call this redundantly.
    let mem_info = process.memory.query_memory(addr);
    if mem_info.state != crate::memory_manager::MemoryState::Unmapped
        && mem_info.base_addr <= addr
        && mem_info.base_addr + mem_info.size >= addr + size
    {
        debug!("MapPhysicalMemory: region already mapped, returning success");
        return ResultCode::SUCCESS;
    }

    match process.memory.map(
        addr,
        size,
        MemoryPermission::READ_WRITE,
        crate::memory_manager::MemoryState::Normal,
    ) {
        Ok(_) => ResultCode::SUCCESS,
        Err(e) => {
            debug!("MapPhysicalMemory failed: {:?}", e);
            error::OUT_OF_MEMORY
        }
    }
}

/// SVC 0x2D: UnmapPhysicalMemory
///
/// Unmaps physical memory at the given address.
pub fn svc_unmap_physical_memory(kernel: &mut KernelCore, addr: VAddr, size: u64) -> ResultCode {
    debug!("UnmapPhysicalMemory: addr=0x{:X}, size=0x{:X}", addr, size);

    if !is_page_aligned(addr) || !is_page_aligned(size) || size == 0 {
        return error::INVALID_SIZE;
    }

    let process = match kernel.process_mut() {
        Some(p) => p,
        None => return error::INVALID_STATE,
    };

    let _ = process.memory.unmap(addr, size);
    ResultCode::SUCCESS
}
