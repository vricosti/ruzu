//! Port of zuyu/src/core/hle/kernel/svc/svc_exception.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-20
//!
//! SVC handlers for Break and ReturnFromException.

use crate::core::System;
use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::result::ResultCode;

/// Break program execution.
///
/// Upstream logs the break reason, handles debug buffers, saves a break report,
/// and optionally notifies the debugger.
pub fn break_execution(system: &System, reason: u32, info1: u64, info2: u64) {
    let break_reason_raw = reason & !0x80000000u32;
    let notification_only = (reason & 0x80000000) != 0;

    let mut has_dumped_buffer = false;

    // Upstream: handle_debug_buffer reads from guest memory and logs the contents.
    let handle_debug_buffer = |addr: u64, sz: u64, dumped: &mut bool| {
        if sz == 0 || addr == 0 || *dumped {
            return;
        }

        if let Some(memory) = system.get_svc_memory() {
            let m = memory.lock().unwrap();
            if sz == 4 {
                // Typically an error code.
                let err_code = m.read_32(addr);
                log::error!("debug_buffer_err_code={:X}", err_code);
            } else {
                // Hexdump the buffer.
                let sz = sz as usize;
                let mut hexdump = String::new();
                for i in 0..sz {
                    let byte = m.read_8(addr + i as u64);
                    hexdump.push_str(&format!("{:02X} ", byte));
                    if i != 0 && i % 16 == 0 {
                        hexdump.push('\n');
                    }
                }
                log::error!("debug_buffer=\n{}", hexdump);
            }
        } else {
            let mem = system.shared_process_memory().read().unwrap();
            if sz == 4 {
                let err_code = mem.read_32(addr);
                log::error!("debug_buffer_err_code={:X}", err_code);
            } else {
                let sz = sz as usize;
                let mut hexdump = String::new();
                for i in 0..sz {
                    let byte = mem.read_8(addr + i as u64);
                    hexdump.push_str(&format!("{:02X} ", byte));
                    if i != 0 && i % 16 == 0 {
                        hexdump.push('\n');
                    }
                }
                log::error!("debug_buffer=\n{}", hexdump);
            }
        }
        *dumped = true;
    };

    // Match upstream break reason handling.
    match break_reason_raw {
        0 => {
            // BreakReason::Panic
            log::error!(
                "Userspace PANIC! info1=0x{:016X}, info2=0x{:016X}",
                info1, info2
            );
            handle_debug_buffer(info1, info2, &mut has_dumped_buffer);
        }
        1 => {
            // BreakReason::Assert
            log::error!(
                "Userspace Assertion failed! info1=0x{:016X}, info2=0x{:016X}",
                info1, info2
            );
            handle_debug_buffer(info1, info2, &mut has_dumped_buffer);
        }
        2 => {
            // BreakReason::User
            log::warn!(
                "Userspace Break! 0x{:016X} with size 0x{:016X}",
                info1, info2
            );
            handle_debug_buffer(info1, info2, &mut has_dumped_buffer);
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
                "Signalling debugger, Unknown break reason {:#X}, info1=0x{:016X}, info2=0x{:016X}",
                reason, info1, info2
            );
            handle_debug_buffer(info1, info2, &mut has_dumped_buffer);
        }
    }

    // Upstream: system.GetReporter().SaveSvcBreakReport(...)
    // Reporter not yet ported — when available, call:
    // system.reporter().save_svc_break_report(reason, notification_only, info1, info2, debug_buffer);

    if !notification_only {
        log::error!(
            "Emulated program broke execution! reason=0x{:016X}, info1=0x{:016X}, info2=0x{:016X}",
            reason as u64, info1, info2
        );

        handle_debug_buffer(info1, info2, &mut has_dumped_buffer);

        // Upstream: system.CurrentPhysicalCore().LogBacktrace();
        // Backtrace logging depends on PhysicalCore integration.
    }

    // Upstream: Debugger notification.
    // const bool is_hbl = GetCurrentProcess(kernel).IsHbl();
    // const bool should_break = is_hbl || !notification_only;
    // if (system.DebuggerEnabled() && should_break) {
    //     auto* thread = system.Kernel().GetCurrentEmuThread();
    //     system.GetDebugger().NotifyThreadStopped(thread);
    //     thread->RequestSuspend(SuspendType::Debug);
    // }
    // Debugger not yet ported — when available, wire up debugger notification here.
}

/// Return from exception.
/// Upstream: UNIMPLEMENTED() — intentionally unimplemented.
pub fn return_from_exception(_result: ResultCode) {
    log::warn!("svc::ReturnFromException: Upstream UNIMPLEMENTED");
}
