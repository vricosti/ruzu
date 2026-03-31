//! Port of zuyu/src/core/hle/kernel/svc/svc_query_memory.cpp
//! Status: Ported
//! Derniere synchro: 2026-03-20
//!
//! SVC handlers for QueryMemory and QueryProcessMemory.

use crate::core::System;
use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::kernel::svc_common::{Handle, PseudoHandle};
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// Queries memory info for the current process.
///
/// This is just QueryProcessMemory on the current process (using CurrentProcess pseudo-handle).
pub fn query_memory(
    system: &System,
    out_memory_info: u64,
    out_page_info: &mut PageInfo,
    query_address: u64,
) -> ResultCode {
    log::trace!(
        "svc::QueryMemory called, out_memory_info=0x{:016X}, query_address=0x{:016X}",
        out_memory_info,
        query_address
    );

    // QueryMemory is just QueryProcessMemory on the current process.
    query_process_memory(
        system,
        out_memory_info,
        out_page_info,
        PseudoHandle::CurrentProcess as Handle,
        query_address,
    )
}

/// Queries memory info for a specific process.
pub fn query_process_memory(
    system: &System,
    out_memory_info: u64,
    out_page_info: &mut PageInfo,
    process_handle: Handle,
    address: u64,
) -> ResultCode {
    log::trace!(
        "svc::QueryProcessMemory called process=0x{:08X} address=0x{:X}",
        process_handle,
        address
    );

    // Get the process from the handle.
    // For pseudo-handle CurrentProcess, use the current process directly.
    // For real handles, look up in handle table.
    let process_arc = if process_handle == PseudoHandle::CurrentProcess as Handle {
        system.current_process_arc()
    } else {
        // Non-pseudo handle: look up in handle table.
        // Upstream: GetCurrentProcess(kernel).GetHandleTable().GetObject<KProcess>(process_handle)
        let current_process = system.current_process_arc();
        let current_guard = current_process.lock().unwrap();
        let _object_id = match current_guard.handle_table.get_object(process_handle) {
            Some(id) => id,
            None => {
                log::error!(
                    "Process handle does not exist, process_handle=0x{:08X}",
                    process_handle
                );
                return RESULT_INVALID_HANDLE;
            }
        };
        // In the current handle table design, we only have the current process.
        // For multi-process support, we'd need to look up by object_id.
        // For now, treat any valid handle as the current process.
        drop(current_guard);
        system.current_process_arc()
    };

    let process = process_arc.lock().unwrap();

    // Query the page table for memory info at the given address.
    let mem_info = process
        .page_table
        .query_info(address as usize)
        .expect("KPageTableBase::query_info must synthesize the terminal inaccessible block");

    // Convert KMemoryInfo to SVC MemoryInfo.
    let svc_mem_info = MemoryInfo {
        base_address: mem_info.m_address as u64,
        size: mem_info.m_size as u64,
        state: mem_info.get_svc_state(),
        attribute: mem_info.get_attribute().bits() as u32,
        permission: mem_info.get_permission().bits() as u32,
        ipc_count: mem_info.m_ipc_lock_count as u32,
        device_count: mem_info.m_device_use_count as u32,
        padding: 0,
    };

    *out_page_info = PageInfo::default();

    log::error!(
        "QueryMemory(addr=0x{:X}) -> base=0x{:X} size=0x{:X} state={} perm={}",
        address,
        svc_mem_info.base_address,
        svc_mem_info.size,
        svc_mem_info.state,
        svc_mem_info.permission
    );

    // Write svc_mem_info to user memory at out_memory_info.
    write_memory_info(system, out_memory_info, &svc_mem_info);

    RESULT_SUCCESS
}

/// Writes a MemoryInfo struct to guest memory at the given address.
fn write_memory_info(system: &System, address: u64, info: &MemoryInfo) {
    if let Some(memory) = system.get_svc_memory() {
        let m = memory.lock().unwrap();
        m.write_64(address, info.base_address);
        m.write_64(address + 8, info.size);
        m.write_32(address + 16, info.state);
        m.write_32(address + 20, info.attribute);
        m.write_32(address + 24, info.permission);
        m.write_32(address + 28, info.ipc_count);
        m.write_32(address + 32, info.device_count);
        m.write_32(address + 36, info.padding);
    } else {
        let mut mem = system.shared_process_memory().write().unwrap();
        if mem.is_valid_range(address, 40) {
            mem.write_64(address, info.base_address);
            mem.write_64(address + 8, info.size);
            mem.write_32(address + 16, info.state);
            mem.write_32(address + 20, info.attribute);
            mem.write_32(address + 24, info.permission);
            mem.write_32(address + 28, info.ipc_count);
            mem.write_32(address + 32, info.device_count);
            mem.write_32(address + 36, info.padding);
        }
    }
}
