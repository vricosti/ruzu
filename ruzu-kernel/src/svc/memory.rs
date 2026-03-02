// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use log::debug;
use ruzu_common::error;
use ruzu_common::{ResultCode, VAddr, is_page_aligned};

use crate::kernel::KernelCore;
use crate::memory_manager::MemoryPermission;

/// SVC 0x01: SetHeapSize
/// X1 = size
/// Returns: X0 = result, X1 = heap address
pub fn svc_set_heap_size(kernel: &mut KernelCore, size: u64) -> Result<VAddr, ResultCode> {
    debug!("SetHeapSize: size=0x{:X}", size);

    if !is_page_aligned(size) {
        return Err(error::INVALID_SIZE);
    }

    let process = kernel.process_mut().ok_or(error::INVALID_STATE)?;
    process
        .set_heap_size(size)
        .map_err(|_| error::OUT_OF_MEMORY)
}

/// SVC 0x02: SetMemoryPermission
/// X0 = addr, X1 = size, X2 = permission
/// Returns: X0 = result
pub fn svc_set_memory_permission(
    kernel: &mut KernelCore,
    addr: VAddr,
    size: u64,
    perm: u32,
) -> ResultCode {
    debug!(
        "SetMemoryPermission: addr=0x{:X}, size=0x{:X}, perm={}",
        addr, size, perm
    );

    if !is_page_aligned(addr) || !is_page_aligned(size) || size == 0 {
        return error::INVALID_SIZE;
    }

    let process = match kernel.process_mut() {
        Some(p) => p,
        None => return error::INVALID_STATE,
    };

    let permission = MemoryPermission::from_bits_truncate(perm);
    match process.memory.set_permissions(addr, size, permission) {
        Ok(_) => ResultCode::SUCCESS,
        Err(_) => {
            // Region may not exist as an exact match — just succeed for robustness.
            debug!("SetMemoryPermission: region not found, returning success anyway");
            ResultCode::SUCCESS
        }
    }
}

/// SVC 0x03: SetMemoryAttribute
/// X0 = addr, X1 = size, X2 = mask, X3 = attribute
/// Returns: X0 = result
///
/// Stub — accepts and returns success. Full implementation would set
/// uncached/permission attributes on the memory region.
pub fn svc_set_memory_attribute(
    _kernel: &mut KernelCore,
    addr: VAddr,
    size: u64,
    _mask: u32,
    _attr: u32,
) -> ResultCode {
    debug!("SetMemoryAttribute: addr=0x{:X}, size=0x{:X}", addr, size);
    ResultCode::SUCCESS
}

/// SVC 0x04: MapMemory
/// X0 = dst_addr, X1 = src_addr, X2 = size
/// Creates a mirror mapping: maps dst as Stack with RW, copies data from src.
/// Used by games to set up thread stacks.
/// Returns: X0 = result
pub fn svc_map_memory(
    kernel: &mut KernelCore,
    dst: VAddr,
    src: VAddr,
    size: u64,
) -> ResultCode {
    debug!(
        "MapMemory: dst=0x{:X}, src=0x{:X}, size=0x{:X}",
        dst, src, size
    );

    if !is_page_aligned(dst) || !is_page_aligned(src) || !is_page_aligned(size) || size == 0 {
        return error::INVALID_SIZE;
    }

    let process = match kernel.process_mut() {
        Some(p) => p,
        None => return error::INVALID_STATE,
    };

    // Map destination as Stack with RW.
    if process
        .memory
        .map(
            dst,
            size,
            MemoryPermission::READ_WRITE,
            crate::memory_manager::MemoryState::Stack,
        )
        .is_err()
    {
        return error::INVALID_MEMORY_STATE;
    }

    // Copy data from src to dst.
    let data = match process.memory.read_bytes(src, size as usize) {
        Ok(d) => d,
        Err(_) => {
            // Source might not be fully readable — just leave dst zeroed.
            return ResultCode::SUCCESS;
        }
    };
    let _ = process.memory.write_bytes(dst, &data);

    ResultCode::SUCCESS
}

/// SVC 0x05: UnmapMemory
/// X0 = dst_addr, X1 = src_addr, X2 = size
/// Copies data back from dst to src, then unmaps dst.
/// Returns: X0 = result
pub fn svc_unmap_memory(
    kernel: &mut KernelCore,
    dst: VAddr,
    src: VAddr,
    size: u64,
) -> ResultCode {
    debug!(
        "UnmapMemory: dst=0x{:X}, src=0x{:X}, size=0x{:X}",
        dst, src, size
    );

    if !is_page_aligned(dst) || !is_page_aligned(src) || !is_page_aligned(size) || size == 0 {
        return error::INVALID_SIZE;
    }

    let process = match kernel.process_mut() {
        Some(p) => p,
        None => return error::INVALID_STATE,
    };

    // Copy data back from dst to src.
    if let Ok(data) = process.memory.read_bytes(dst, size as usize) {
        let _ = process.memory.write_bytes(src, &data);
    }

    // Unmap dst.
    let _ = process.memory.unmap(dst, size);

    ResultCode::SUCCESS
}
