//! Port of zuyu/src/core/hle/kernel/svc/svc_shared_memory.cpp
//! Status: Ported — MapSharedMemory wired to real kernel objects.
//! Derniere synchro: 2026-03-19
//!
//! SVC handlers for shared memory operations.

use crate::core::System;
use crate::hle::kernel::k_memory_block::KMemoryState;
use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::kernel::svc_common::Handle;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

fn is_valid_shared_memory_permission(perm: MemoryPermission) -> bool {
    matches!(perm, MemoryPermission::Read | MemoryPermission::ReadWrite)
}

/// Convert SVC MemoryPermission to KSharedMemory MemoryPermission.
fn to_shmem_perm(perm: MemoryPermission) -> crate::hle::kernel::k_shared_memory::MemoryPermission {
    match perm {
        MemoryPermission::Read => crate::hle::kernel::k_shared_memory::MemoryPermission::Read,
        MemoryPermission::ReadWrite => {
            crate::hle::kernel::k_shared_memory::MemoryPermission::ReadWrite
        }
        _ => crate::hle::kernel::k_shared_memory::MemoryPermission::None,
    }
}

fn invalid_shared_memory_region_result() -> ResultCode {
    RESULT_INVALID_MEMORY_REGION
}

/// Maps shared memory into the current process.
///
/// Upstream: `svc_shared_memory.cpp: MapSharedMemory(Core::System&, Handle, u64, u64, MemoryPermission)`.
pub fn map_shared_memory(
    system: &System,
    shmem_handle: Handle,
    address: u64,
    size: u64,
    map_perm: MemoryPermission,
) -> ResultCode {
    log::debug!(
        "svc::MapSharedMemory called, handle=0x{:X}, addr=0x{:X}, size=0x{:X}, perm={:?}",
        shmem_handle,
        address,
        size,
        map_perm
    );

    // Validate input.
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
    if !is_valid_shared_memory_permission(map_perm) {
        return RESULT_INVALID_NEW_MEMORY_PERMISSION;
    }

    // Get the KSharedMemory from the handle table.
    let mut process = system.current_process_arc().lock().unwrap();

    let object_id = match process.handle_table.get_object(shmem_handle) {
        Some(id) => id,
        None => {
            log::error!(
                "svc::MapSharedMemory: handle {:#x} not in handle table",
                shmem_handle
            );
            if let Some(current_thread) = system.current_thread() {
                let thread = current_thread.lock().unwrap();
                log::error!(
                    "svc::MapSharedMemory invalid handle context: tid={} pc=0x{:08X} lr=0x{:08X} sp=0x{:08X}",
                    thread.get_thread_id(),
                    thread.thread_context.pc,
                    thread.thread_context.lr,
                    thread.thread_context.sp
                );
                if let Some(parent) = thread.parent.as_ref().and_then(|w| w.upgrade()) {
                    let process = parent.lock().unwrap();
                    let arm_ctx = crate::arm::arm_interface::ThreadContext {
                        r: thread.thread_context.r,
                        fp: thread.thread_context.fp,
                        lr: thread.thread_context.lr,
                        sp: thread.thread_context.sp,
                        pc: thread.thread_context.pc,
                        pstate: thread.thread_context.pstate,
                        padding: thread.thread_context.padding,
                        v: thread.thread_context.v,
                        fpcr: thread.thread_context.fpcr,
                        fpsr: thread.thread_context.fpsr,
                        tpidr: thread.thread_context.tpidr,
                    };
                    for entry in crate::arm::debug::get_backtrace_from_context(&process, &arm_ctx)
                        .iter()
                        .take(12)
                    {
                        log::error!(
                            "svc::MapSharedMemory backtrace: module={} addr=0x{:X} original=0x{:X} offset=0x{:X} symbol={}",
                            entry.module,
                            entry.address,
                            entry.original_address,
                            entry.offset,
                            entry.name
                        );
                    }
                }
            }
            return RESULT_INVALID_HANDLE;
        }
    };

    let shmem = match process.get_shared_memory_by_object_id(object_id) {
        Some(s) => s,
        None => {
            log::error!(
                "svc::MapSharedMemory: object_id {} is not a KSharedMemory",
                object_id
            );
            return RESULT_INVALID_HANDLE;
        }
    };

    // Validate that the address can contain shared memory.
    let can_contain = process.page_table.can_contain(
        crate::hle::kernel::k_typed_address::KProcessAddress::new(address),
        size as usize,
        KMemoryState::SHARED,
    );
    if !can_contain {
        log::debug!(
            "svc::MapSharedMemory: address {:#x} size {:#x} cannot contain Shared",
            address,
            size
        );
        // Dump the process page-table region layout so we can diagnose
        // which region the request collides with vs upstream.
        let pt = &process.page_table;
        // Convert ProcessAddress accessors to raw u64 via .into()
        let heap_s: u64 = pt.get_heap_region_start().into();
        let heap_e: u64 = heap_s + pt.get_heap_region_size() as u64;
        let alias_s: u64 = pt.get_alias_region_start().into();
        let alias_e: u64 = alias_s + pt.get_alias_region_size() as u64;
        let alias_code_s: u64 = pt.get_alias_code_region_start().into();
        let alias_code_e: u64 = alias_code_s + pt.get_alias_code_region_size() as u64;
        let code_s: u64 = pt.get_code_region_start().into();
        let code_e: u64 = code_s + pt.get_code_region_size() as u64;
        let stack_s: u64 = pt.get_stack_region_start().into();
        let stack_e: u64 = stack_s + pt.get_stack_region_size() as u64;
        let kmap_s: u64 = pt.get_kernel_map_region_start().into();
        let kmap_e: u64 = kmap_s + pt.get_kernel_map_region_size() as u64;
        let addr_s: u64 = pt.get_address_space_start().into();
        let addr_e: u64 = addr_s + pt.get_address_space_size() as u64;
        log::debug!(
            "  region: alias_code=[{:#x}..{:#x}] code=[{:#x}..{:#x}] heap=[{:#x}..{:#x}] alias=[{:#x}..{:#x}] stack=[{:#x}..{:#x}] kmap=[{:#x}..{:#x}] addr_space=[{:#x}..{:#x}]",
            alias_code_s, alias_code_e,
            code_s, code_e,
            heap_s, heap_e,
            alias_s, alias_e,
            stack_s, stack_e,
            kmap_s, kmap_e,
            addr_s, addr_e,
        );
        let end = address.saturating_add(size);
        let last = end.saturating_sub(1);
        let is_in_region =
            alias_code_s <= address && address < end && last <= alias_code_e.saturating_sub(1);
        let is_in_heap = !(end <= heap_s || heap_e <= address || heap_s == heap_e);
        let is_in_alias = !(end <= alias_s || alias_e <= address || alias_s == alias_e);
        log::debug!(
            "  check: is_in_region={} is_in_heap={} is_in_alias={} (need true && false && false)",
            is_in_region,
            is_in_heap,
            is_in_alias
        );
        return invalid_shared_memory_region_result();
    }

    let result = process.add_shared_memory(shmem.clone(), address, size as usize);
    if !result.is_success() {
        return result;
    }

    // Map the shared memory into the process page table.
    let result = shmem.map(
        &mut process.page_table,
        address,
        size as usize,
        to_shmem_perm(map_perm),
    );

    if result.is_success() {
        log::debug!(
            "svc::MapSharedMemory: mapped handle={:#x} at {:#x} size={:#x} perm={:?}",
            shmem_handle,
            address,
            size,
            map_perm
        );
        if std::env::var_os("RUZU_TRACE_FONT_SHMEM_BYTES").is_some() && size == 0x1100000 {
            for offset in [0x0_u64, 0x8, 0x11d10c] {
                let bytes = process.read_memory_vec(address + offset, 32);
                let mut hex = String::with_capacity(bytes.len() * 2);
                for byte in bytes {
                    use std::fmt::Write as _;
                    let _ = write!(hex, "{byte:02x}");
                }
                eprintln!(
                    "[FONT_SHMEM_BYTES] base=0x{address:X} offset=0x{offset:X} addr=0x{:X} bytes={hex}",
                    address + offset
                );
            }
        }
    } else {
        process.remove_shared_memory(&shmem, address, size as usize);
        log::error!("svc::MapSharedMemory: map failed, result={:?}", result);
    }

    result
}

/// Unmaps shared memory from the current process.
pub fn unmap_shared_memory(
    system: &System,
    shmem_handle: Handle,
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

    let object_id = match process.handle_table.get_object(shmem_handle) {
        Some(id) => id,
        None => return RESULT_INVALID_HANDLE,
    };

    let shmem = match process.get_shared_memory_by_object_id(object_id) {
        Some(s) => s,
        None => return RESULT_INVALID_HANDLE,
    };

    if !process.page_table.can_contain(
        crate::hle::kernel::k_typed_address::KProcessAddress::new(address),
        size as usize,
        KMemoryState::SHARED,
    ) {
        return invalid_shared_memory_region_result();
    }

    let result = shmem.unmap(&mut process.page_table, address, size as usize);
    if result.is_success() {
        process.remove_shared_memory(&shmem, address, size as usize);
    }
    result
}

/// Creates shared memory. (Unimplemented upstream.)
pub fn create_shared_memory(
    _out_handle: &mut Handle,
    _size: u64,
    _owner_perm: MemoryPermission,
    _remote_perm: MemoryPermission,
) -> ResultCode {
    log::warn!("svc::CreateSharedMemory: UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn failed_shared_memory_region_uses_upstream_result() {
        assert_eq!(
            invalid_shared_memory_region_result(),
            RESULT_INVALID_MEMORY_REGION
        );
        assert_ne!(
            invalid_shared_memory_region_result(),
            RESULT_INVALID_CURRENT_MEMORY
        );
    }

    #[test]
    fn map_shared_memory_overflow_uses_invalid_current_memory() {
        let system = System::new();
        assert_eq!(
            map_shared_memory(
                &system,
                0,
                u64::MAX & !(PAGE_SIZE - 1),
                PAGE_SIZE,
                MemoryPermission::Read,
            ),
            RESULT_INVALID_CURRENT_MEMORY
        );
    }
}
