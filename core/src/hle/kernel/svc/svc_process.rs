//! Port of zuyu/src/core/hle/kernel/svc/svc_process.cpp
//! Status: COMPLET (stubs for kernel calls)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for process operations (ExitProcess, GetProcessId, GetProcessList, etc.).

use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::kernel::svc_common::Handle;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// Exits the current process.
pub fn exit_process() {
    // TODO: Get current process, verify state == Running, call system.Exit()
    log::info!("svc::ExitProcess called");
    log::warn!("svc::ExitProcess: kernel object access not yet implemented");
}

/// Gets the ID of the specified process or a specified thread's owning process.
pub fn get_process_id(out_process_id: &mut u64, handle: Handle) -> ResultCode {
    log::debug!("svc::GetProcessId called handle=0x{:08X}", handle);

    // TODO: Get object from handle table.
    // If KProcess, use directly. If KThread, get owner process.
    // Set *out_process_id = process->GetId()
    log::warn!("svc::GetProcessId: kernel object access not yet implemented");
    *out_process_id = 0;
    RESULT_NOT_IMPLEMENTED
}

/// Gets the list of process IDs.
pub fn get_process_list(
    out_num_processes: &mut i32,
    _out_process_ids: u64,
    out_process_ids_size: i32,
) -> ResultCode {
    log::debug!(
        "svc::GetProcessList called. out_process_ids_size={}",
        out_process_ids_size
    );

    // If the supplied size is negative or greater than INT32_MAX / sizeof(u64), bail.
    if (out_process_ids_size as u32 & 0xF000_0000) != 0 {
        log::error!(
            "Supplied size outside [0, 0x0FFFFFFF] range. out_process_ids_size={}",
            out_process_ids_size
        );
        return RESULT_OUT_OF_RANGE;
    }

    // TODO: Get process list from kernel, copy to user memory.
    *out_num_processes = 0;
    log::warn!("svc::GetProcessList: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Gets information about a process.
pub fn get_process_info(
    out: &mut i64,
    process_handle: Handle,
    info_type: ProcessInfoType,
) -> ResultCode {
    log::debug!(
        "svc::GetProcessInfo called, handle=0x{:08X}, type={:?}",
        process_handle, info_type
    );

    // TODO: Get process from handle.
    if !matches!(info_type, ProcessInfoType::ProcessState) {
        log::error!("Expected info_type to be ProcessState but got {:?}", info_type);
        return RESULT_INVALID_ENUM_VALUE;
    }

    // TODO: *out = process->GetState() as i64
    *out = 0;
    log::warn!("svc::GetProcessInfo: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Creates a process. (Unimplemented upstream.)
pub fn create_process(
    _out_handle: &mut Handle,
    _parameters: u64,
    _caps: u64,
    _num_caps: i32,
) -> ResultCode {
    log::warn!("svc::CreateProcess: UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}

/// Starts a process. (Unimplemented upstream.)
pub fn start_process(
    _process_handle: Handle,
    _priority: i32,
    _core_id: i32,
    _main_thread_stack_size: u64,
) -> ResultCode {
    log::warn!("svc::StartProcess: UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}

/// Terminates a process. (Unimplemented upstream.)
pub fn terminate_process(_process_handle: Handle) -> ResultCode {
    log::warn!("svc::TerminateProcess: UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}
