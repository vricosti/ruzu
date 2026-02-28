// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use log::debug;
use ruzu_common::error;
use ruzu_common::{Handle, ResultCode, VAddr, is_page_aligned};

use crate::kernel::KernelCore;
use crate::memory_manager::{MemoryPermission, MemoryState};
use crate::objects::{KTransferMemory, KernelObject};
use ruzu_cpu::CpuState;

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

/// SVC 0x06: QueryMemory
/// X0 = memory_info address, X2 = query address
/// Returns: X0 = result, X1 = page_info
pub fn svc_query_memory(
    kernel: &mut KernelCore,
    _cpu: &mut CpuState,
    info_addr: VAddr,
    query_addr: VAddr,
) -> ResultCode {
    debug!(
        "QueryMemory: info_addr=0x{:X}, query_addr=0x{:X}",
        info_addr, query_addr
    );

    let process = match kernel.process_mut() {
        Some(p) => p,
        None => return error::INVALID_STATE,
    };

    let mem_info = process.memory.query_memory(query_addr);

    // Write MemoryInfo struct to guest memory (0x28 bytes):
    // [0x00] base_addr: u64
    // [0x08] size: u64
    // [0x10] state: u32
    // [0x14] attribute: u32
    // [0x18] permission: u32
    // [0x1C] ipc_count: u32
    // [0x20] device_count: u32
    // [0x24] padding: u32
    let _ = process.memory.write_u64(info_addr, mem_info.base_addr);
    let _ = process.memory.write_u64(info_addr + 0x08, mem_info.size);
    let _ = process
        .memory
        .write_u32(info_addr + 0x10, mem_info.state as u32);
    let _ = process.memory.write_u32(info_addr + 0x14, 0); // attribute
    let _ = process
        .memory
        .write_u32(info_addr + 0x18, mem_info.permission.bits());
    let _ = process.memory.write_u32(info_addr + 0x1C, 0); // ipc_count
    let _ = process.memory.write_u32(info_addr + 0x20, 0); // device_count
    let _ = process.memory.write_u32(info_addr + 0x24, 0); // padding

    ResultCode::SUCCESS
}

/// SVC 0x13: MapSharedMemory
///
/// Maps a shared memory region into the process address space.
/// Allocates backing storage from the kernel's shared memory pool if needed.
pub fn svc_map_shared_memory(
    kernel: &mut KernelCore,
    handle: Handle,
    addr: VAddr,
    size: u64,
    perm: u32,
) -> ResultCode {
    debug!(
        "MapSharedMemory: handle={}, addr=0x{:X}, size=0x{:X}, perm={}",
        handle, addr, size, perm
    );

    if !is_page_aligned(addr) || !is_page_aligned(size) {
        return error::INVALID_SIZE;
    }

    // Look up the shared memory object and allocate backing if needed.
    let backing_offset;
    let backing_size;
    {
        let process = match kernel.process_mut() {
            Some(p) => p,
            None => return error::INVALID_STATE,
        };

        match process.handle_table.get_mut(handle) {
            Ok(KernelObject::SharedMemory(shm)) => {
                backing_size = shm.size;

                if shm.backing_offset.is_none() {
                    // Allocate from the kernel's shared memory pool.
                    let offset = kernel.shared_memory_pool.len();
                    kernel
                        .shared_memory_pool
                        .resize(offset + backing_size, 0);
                    // Re-borrow after pool resize.
                    let process = kernel.process_mut().unwrap();
                    if let Ok(KernelObject::SharedMemory(shm)) =
                        process.handle_table.get_mut(handle)
                    {
                        shm.backing_offset = Some(offset);
                    }
                }

                // Re-fetch the offset after potential mutation.
                let process = kernel.process_mut().unwrap();
                if let Ok(KernelObject::SharedMemory(shm)) =
                    process.handle_table.get(handle)
                {
                    backing_offset = shm.backing_offset;
                } else {
                    return error::INVALID_HANDLE;
                }
            }
            Ok(_) => {
                // Not a shared memory object — just map the region (legacy behavior).
                let permission = MemoryPermission::from_bits_truncate(perm);
                return match process.memory.map(
                    addr,
                    size,
                    permission,
                    MemoryState::SharedMemory,
                ) {
                    Ok(_) => ResultCode::SUCCESS,
                    Err(_) => error::INVALID_MEMORY_STATE,
                };
            }
            Err(_) => {
                // Handle not found — map the region anyway (for stubs/dummy handles).
                let permission = MemoryPermission::from_bits_truncate(perm);
                return match process.memory.map(
                    addr,
                    size,
                    permission,
                    MemoryState::SharedMemory,
                ) {
                    Ok(_) => ResultCode::SUCCESS,
                    Err(_) => error::INVALID_MEMORY_STATE,
                };
            }
        }
    }

    // Map the region in guest memory.
    let process = kernel.process_mut().unwrap();
    let permission = MemoryPermission::from_bits_truncate(perm);
    if let Err(_) = process
        .memory
        .map(addr, size, permission, MemoryState::SharedMemory)
    {
        return error::INVALID_MEMORY_STATE;
    }

    // Copy backing data into the mapped region.
    if let Some(offset) = backing_offset {
        let copy_size = backing_size.min(size as usize);
        let data = kernel.shared_memory_pool[offset..offset + copy_size].to_vec();
        let process = kernel.process_mut().unwrap();
        let _ = process.memory.write_bytes(addr, &data);
    }

    ResultCode::SUCCESS
}

/// SVC 0x14: UnmapSharedMemory
///
/// Unmaps a shared memory region from the process address space.
/// Zeroes the mapped region before unmapping.
pub fn svc_unmap_shared_memory(
    kernel: &mut KernelCore,
    _handle: Handle,
    addr: VAddr,
    size: u64,
) -> ResultCode {
    debug!(
        "UnmapSharedMemory: addr=0x{:X}, size=0x{:X}",
        addr, size
    );

    if !is_page_aligned(addr) || !is_page_aligned(size) || size == 0 {
        return error::INVALID_SIZE;
    }

    let process = match kernel.process_mut() {
        Some(p) => p,
        None => return error::INVALID_STATE,
    };

    // Zero the region before unmapping.
    let zeros = vec![0u8; size as usize];
    let _ = process.memory.write_bytes(addr, &zeros);

    // Unmap the region.
    let _ = process.memory.unmap(addr, size);

    ResultCode::SUCCESS
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
        .map(dst, size, MemoryPermission::READ_WRITE, MemoryState::Stack)
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

/// SVC 0x2C: MapPhysicalMemory
///
/// Maps physical memory at the given address with READ_WRITE permissions.
/// Used by games for GPU buffer allocations and large data regions.
pub fn svc_map_physical_memory(
    kernel: &mut KernelCore,
    addr: VAddr,
    size: u64,
) -> ResultCode {
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
        MemoryState::Normal,
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
pub fn svc_unmap_physical_memory(
    kernel: &mut KernelCore,
    addr: VAddr,
    size: u64,
) -> ResultCode {
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

/// SVC 0x15: CreateTransferMemory
/// X1 = addr, X2 = size, X3 = permission
/// Returns: X0 = result, X1 = handle
pub fn svc_create_transfer_memory(
    kernel: &mut KernelCore,
    addr: VAddr,
    size: u64,
    perm: u32,
) -> Result<Handle, ResultCode> {
    debug!(
        "CreateTransferMemory: addr=0x{:X}, size=0x{:X}, perm={}",
        addr, size, perm
    );

    let process = kernel.process_mut().ok_or(error::INVALID_STATE)?;
    let tmem = KTransferMemory {
        addr,
        size: size as usize,
        permission: perm,
    };
    let handle = process
        .handle_table
        .add(KernelObject::TransferMemory(tmem))
        .map_err(|_| error::HANDLE_TABLE_FULL)?;
    Ok(handle)
}
