//! Port of zuyu/src/core/hle/kernel/svc/svc_event.cpp
//! Status: COMPLET (stubs for kernel calls)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for event operations (SignalEvent, ClearEvent, CreateEvent).

use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc_common::Handle;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// Signals an event.
pub fn signal_event(event_handle: Handle) -> ResultCode {
    log::debug!("svc::SignalEvent called, event_handle=0x{:08X}", event_handle);

    // TODO: Get the event from handle table, call event->Signal()
    log::warn!("svc::SignalEvent: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Clears an event. Tries writable event first, then readable event.
pub fn clear_event(event_handle: Handle) -> ResultCode {
    log::trace!("svc::ClearEvent called, event_handle=0x{:08X}", event_handle);

    // TODO: Try to clear writable event, then readable event.
    // If neither found, return ResultInvalidHandle.
    log::warn!("svc::ClearEvent: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Creates an event, returning write and read handles.
pub fn create_event(out_write: &mut Handle, out_read: &mut Handle) -> ResultCode {
    log::debug!("svc::CreateEvent called");

    // TODO: Reserve event from process resource limit.
    // TODO: KEvent::Create, Initialize, Register.
    // TODO: Add event and readable event to handle table.
    log::warn!("svc::CreateEvent: kernel object access not yet implemented");
    *out_write = 0;
    *out_read = 0;
    RESULT_NOT_IMPLEMENTED
}
