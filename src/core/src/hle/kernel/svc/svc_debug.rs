//! Port of zuyu/src/core/hle/kernel/svc/svc_debug.cpp
//! Status: COMPLET (all stubs matching upstream UNIMPLEMENTED)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for debug operations.

use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::kernel::svc_common::Handle;
use crate::hle::result::ResultCode;

pub fn debug_active_process(_out_handle: &mut Handle, _process_id: u64) -> ResultCode {
    log::warn!("svc::DebugActiveProcess: UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}

pub fn break_debug_process(_debug_handle: Handle) -> ResultCode {
    log::warn!("svc::BreakDebugProcess: UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}

pub fn terminate_debug_process(_debug_handle: Handle) -> ResultCode {
    log::warn!("svc::TerminateDebugProcess: UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}

pub fn get_debug_event(_out_info: u64, _debug_handle: Handle) -> ResultCode {
    log::warn!("svc::GetDebugEvent: UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}

pub fn continue_debug_event(
    _debug_handle: Handle,
    _flags: u32,
    _user_thread_ids: u64,
    _num_thread_ids: i32,
) -> ResultCode {
    log::warn!("svc::ContinueDebugEvent: UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}

pub fn get_debug_thread_context(
    _out_context: u64,
    _debug_handle: Handle,
    _thread_id: u64,
    _context_flags: u32,
) -> ResultCode {
    log::warn!("svc::GetDebugThreadContext: UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}

pub fn set_debug_thread_context(
    _debug_handle: Handle,
    _thread_id: u64,
    _user_context: u64,
    _context_flags: u32,
) -> ResultCode {
    log::warn!("svc::SetDebugThreadContext: UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}

pub fn query_debug_process_memory(
    _out_memory_info: u64,
    _out_page_info: &mut PageInfo,
    _process_handle: Handle,
    _address: u64,
) -> ResultCode {
    log::warn!("svc::QueryDebugProcessMemory: UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}

pub fn read_debug_process_memory(
    _buffer: u64,
    _debug_handle: Handle,
    _address: u64,
    _size: u64,
) -> ResultCode {
    log::warn!("svc::ReadDebugProcessMemory: UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}

pub fn write_debug_process_memory(
    _debug_handle: Handle,
    _buffer: u64,
    _address: u64,
    _size: u64,
) -> ResultCode {
    log::warn!("svc::WriteDebugProcessMemory: UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}

pub fn set_hardware_break_point(
    _name: HardwareBreakPointRegisterName,
    _flags: u64,
    _value: u64,
) -> ResultCode {
    log::warn!("svc::SetHardwareBreakPoint: UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}

pub fn get_debug_thread_param(
    _out_64: &mut u64,
    _out_32: &mut u32,
    _debug_handle: Handle,
    _thread_id: u64,
    _param: DebugThreadParam,
) -> ResultCode {
    log::warn!("svc::GetDebugThreadParam: UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}
