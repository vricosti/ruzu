//! Port of zuyu/src/core/hle/kernel/svc/svc_interrupt_event.cpp
//! Status: COMPLET (stubs matching upstream UNIMPLEMENTED)
//! Derniere synchro: 2026-03-11
//!
//! SVC handler for creating interrupt events.

use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::kernel::svc_common::Handle;
use crate::hle::result::ResultCode;

/// Creates an interrupt event. (Unimplemented upstream.)
pub fn create_interrupt_event(
    _out: &mut Handle,
    _interrupt_id: i32,
    _interrupt_type: InterruptType,
) -> ResultCode {
    log::warn!("svc::CreateInterruptEvent: UNIMPLEMENTED");
    RESULT_NOT_IMPLEMENTED
}
