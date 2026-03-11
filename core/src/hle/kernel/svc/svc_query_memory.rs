//! Port of zuyu/src/core/hle/kernel/svc/svc_query_memory.cpp
//! Status: COMPLET (stubs for kernel calls)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for QueryMemory and QueryProcessMemory.

use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::kernel::svc_common::Handle;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// Queries memory info for the current process.
///
/// This is just QueryProcessMemory on the current process.
pub fn query_memory(
    out_memory_info: u64,
    out_page_info: &mut PageInfo,
    query_address: u64,
) -> ResultCode {
    log::trace!(
        "svc::QueryMemory called, out_memory_info=0x{:016X}, query_address=0x{:016X}",
        out_memory_info, query_address
    );

    // TODO: QueryProcessMemory(system, out_memory_info, out_page_info, CurrentProcess, query_address)
    log::warn!("svc::QueryMemory: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Queries memory info for a specific process.
pub fn query_process_memory(
    out_memory_info: u64,
    out_page_info: &mut PageInfo,
    process_handle: Handle,
    address: u64,
) -> ResultCode {
    log::trace!(
        "svc::QueryProcessMemory called process=0x{:08X} address=0x{:X}",
        process_handle, address
    );

    // TODO: Get process from handle.
    // TODO: process->GetPageTable().QueryInfo(&mem_info, out_page_info, address)
    // TODO: Write svc_mem_info to user memory at out_memory_info
    log::warn!("svc::QueryProcessMemory: kernel object access not yet implemented");
    *out_page_info = PageInfo::default();
    RESULT_NOT_IMPLEMENTED
}
