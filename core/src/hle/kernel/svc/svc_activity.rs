//! Port of zuyu/src/core/hle/kernel/svc/svc_activity.cpp
//! Status: COMPLET (stubs for kernel calls)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for thread and process activity.

use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::kernel::svc_common::Handle;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// Sets the thread activity.
///
/// Validates the activity, gets the thread from its handle,
/// checks that it belongs to the current process and is not the current thread,
/// then sets the activity.
pub fn set_thread_activity(thread_handle: Handle, thread_activity: ThreadActivity) -> ResultCode {
    log::debug!(
        "svc::SetThreadActivity called, handle=0x{:08X}, activity={:?}",
        thread_handle,
        thread_activity
    );

    // Validate the activity.
    let is_valid = matches!(
        thread_activity,
        ThreadActivity::Runnable | ThreadActivity::Paused
    );
    if !is_valid {
        return RESULT_INVALID_ENUM_VALUE;
    }

    // TODO: Get the thread from its handle via GetCurrentProcess().GetHandleTable().GetObject<KThread>()
    // TODO: Check that the activity is being set on a non-current thread for the current process.
    // TODO: thread->SetActivity(thread_activity)

    log::warn!("svc::SetThreadActivity: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Sets the process activity. (Unimplemented upstream.)
pub fn set_process_activity(
    _process_handle: Handle,
    _process_activity: ProcessActivity,
) -> ResultCode {
    log::warn!("svc::SetProcessActivity: UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}
