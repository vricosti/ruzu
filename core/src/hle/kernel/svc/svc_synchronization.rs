//! Port of zuyu/src/core/hle/kernel/svc/svc_synchronization.cpp
//! Status: COMPLET (stubs for kernel calls)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for synchronization operations (CloseHandle, ResetSignal,
//! WaitSynchronization, CancelSynchronization, SynchronizePreemptionState).

use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc_common::{Handle, ARGUMENT_HANDLE_COUNT_MAX};
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// Closes a handle.
pub fn close_handle(handle: Handle) -> ResultCode {
    log::trace!("svc::CloseHandle closing handle 0x{:08X}", handle);

    // TODO: GetCurrentProcess(kernel).GetHandleTable().Remove(handle)
    log::warn!("svc::CloseHandle: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Clears the signaled state of an event or process.
pub fn reset_signal(handle: Handle) -> ResultCode {
    log::debug!("svc::ResetSignal called handle 0x{:08X}", handle);

    // TODO: Try to reset as readable event, then as process.
    // If neither found, return ResultInvalidHandle.
    log::warn!("svc::ResetSignal: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Wait for the given handles to synchronize, timeout after the specified nanoseconds.
pub fn wait_synchronization(
    out_index: &mut i32,
    user_handles: u64,
    num_handles: i32,
    timeout_ns: i64,
) -> ResultCode {
    log::trace!(
        "svc::WaitSynchronization called user_handles=0x{:x}, num_handles={}, timeout_ns={}",
        user_handles, num_handles, timeout_ns
    );

    // Ensure number of handles is valid.
    if !(0..=ARGUMENT_HANDLE_COUNT_MAX).contains(&num_handles) {
        return RESULT_OUT_OF_RANGE;
    }

    // TODO: Copy user handles from memory.
    // TODO: Convert handles to synchronization objects.
    // TODO: Convert timeout to ticks.
    // TODO: KSynchronizationObject::Wait(...)

    *out_index = -1;
    log::warn!("svc::WaitSynchronization: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Resumes a thread waiting on WaitSynchronization.
pub fn cancel_synchronization(handle: Handle) -> ResultCode {
    log::trace!("svc::CancelSynchronization called handle=0x{:X}", handle);

    // TODO: Get thread from handle, call thread->WaitCancel()
    log::warn!("svc::CancelSynchronization: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Synchronizes preemption state.
pub fn synchronize_preemption_state() {
    // TODO: Lock scheduler, check if current thread is pinned, clear interrupt flag, unpin.
    log::warn!("svc::SynchronizePreemptionState: kernel object access not yet implemented");
}
