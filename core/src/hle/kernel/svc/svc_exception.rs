//! Port of zuyu/src/core/hle/kernel/svc/svc_exception.cpp
//! Status: COMPLET (stubs for kernel calls)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for Break and ReturnFromException.

use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::result::ResultCode;

/// Break program execution.
///
/// Upstream logs the break reason, handles debug buffers, saves a break report,
/// and optionally notifies the debugger.
pub fn break_execution(reason: u32, info1: u64, info2: u64) {
    let break_reason_raw = reason & !0x80000000u32;
    let notification_only = (reason & 0x80000000) != 0;

    // Match upstream break reason handling.
    match break_reason_raw {
        0 => {
            // BreakReason::Panic
            log::error!(
                "Userspace PANIC! info1=0x{:016X}, info2=0x{:016X}",
                info1, info2
            );
        }
        1 => {
            // BreakReason::Assert
            log::error!(
                "Userspace Assertion failed! info1=0x{:016X}, info2=0x{:016X}",
                info1, info2
            );
        }
        2 => {
            // BreakReason::User
            log::warn!(
                "Userspace Break! 0x{:016X} with size 0x{:016X}",
                info1, info2
            );
        }
        3 => {
            // BreakReason::PreLoadDll
            log::info!(
                "Userspace Attempting to load an NRO at 0x{:016X} with size 0x{:016X}",
                info1, info2
            );
        }
        4 => {
            // BreakReason::PostLoadDll
            log::info!(
                "Userspace Loaded an NRO at 0x{:016X} with size 0x{:016X}",
                info1, info2
            );
        }
        5 => {
            // BreakReason::PreUnloadDll
            log::info!(
                "Userspace Attempting to unload an NRO at 0x{:016X} with size 0x{:016X}",
                info1, info2
            );
        }
        6 => {
            // BreakReason::PostUnloadDll
            log::info!(
                "Userspace Unloaded an NRO at 0x{:016X} with size 0x{:016X}",
                info1, info2
            );
        }
        7 => {
            // BreakReason::CppException
            log::error!("Signalling debugger. Uncaught C++ exception encountered.");
        }
        _ => {
            log::warn!(
                "Signalling debugger, Unknown break reason 0x{:X}, info1=0x{:016X}, info2=0x{:016X}",
                reason, info1, info2
            );
        }
    }

    // TODO: system.GetReporter().SaveSvcBreakReport(...)
    // TODO: If !notification_only, log backtrace, handle debugger
    if !notification_only {
        log::error!(
            "Emulated program broke execution! reason=0x{:08X}, info1=0x{:016X}, info2=0x{:016X}",
            reason, info1, info2
        );
    }

    // TODO: Debugger notification
}

/// Return from exception. (Unimplemented upstream.)
pub fn return_from_exception(_result: ResultCode) {
    log::warn!("svc::ReturnFromException: UNIMPLEMENTED");
}
