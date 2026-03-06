// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use log::debug;
use common::{error, Handle, ResultCode, VAddr, is_page_aligned};

use crate::kernel::KernelCore;
use crate::memory_manager::{MemoryPermission, MemoryState};
use crate::objects::KernelObject;

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
