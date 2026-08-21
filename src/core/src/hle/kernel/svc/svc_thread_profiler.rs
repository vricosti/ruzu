//! Port of zuyu/src/core/hle/kernel/svc/svc_thread_profiler.cpp
//! Status: COMPLET (stubs matching upstream UNIMPLEMENTED)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for thread profiler operations.

use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::kernel::svc_common::Handle;
use crate::hle::result::ResultCode;

/// Gets debug future thread info. (Unimplemented upstream.)
pub fn get_debug_future_thread_info(
    _out_context: &mut LastThreadContext64,
    _out_thread_id: &mut u64,
    _debug_handle: Handle,
    _ns: i64,
) -> ResultCode {
    log::warn!("svc::GetDebugFutureThreadInfo: UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}

/// Gets the last thread info. (Unimplemented upstream.)
pub fn get_last_thread_info(
    _out_context: &mut LastThreadContext64,
    _out_tls_address: &mut u64,
    _out_flags: &mut u32,
) -> ResultCode {
    log::warn!("svc::GetLastThreadInfo: UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}
