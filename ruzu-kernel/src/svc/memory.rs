// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use log::debug;
use ruzu_common::error;
use ruzu_common::{Handle, ResultCode, VAddr, is_page_aligned};

use crate::kernel::KernelCore;
use crate::memory_manager::{MemoryPermission, MemoryState};
use crate::objects::KernelObject;
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
