//! Port of zuyu/src/core/hle/kernel/svc/svc_lock.cpp
//! Status: COMPLET (stubs for kernel calls)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for mutex arbitration (ArbitrateLock, ArbitrateUnlock).

use crate::core::System;
use crate::hle::kernel::k_condition_variable::KConditionVariable;
use crate::hle::kernel::k_memory_layout::is_kernel_address;
use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc_common::Handle;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

fn should_trace_sync_debug() -> bool {
    std::env::var_os("RUZU_TRACE_SYNC").is_some()
}

fn should_trace_sync_backtrace_once(tid: u64) -> bool {
    static DID_TRACE_TID73_LOCK: std::sync::atomic::AtomicBool =
        std::sync::atomic::AtomicBool::new(false);
    static DID_TRACE_TID73_UNLOCK: std::sync::atomic::AtomicBool =
        std::sync::atomic::AtomicBool::new(false);
    match tid {
        t if t == 73 => {
            !DID_TRACE_TID73_LOCK.swap(true, std::sync::atomic::Ordering::Relaxed)
                || !DID_TRACE_TID73_UNLOCK.swap(true, std::sync::atomic::Ordering::Relaxed)
        }
        _ => false,
    }
}

fn log_sync_context(system: &System, label: &str) {
    let Some(current_thread_id) = system.current_thread_id() else {
        return;
    };
    let Some(current_thread) = system.current_thread() else {
        return;
    };
    let core_index = current_thread.lock().unwrap().get_current_core().max(0) as usize;
    let process = system.current_process_arc().lock().unwrap();
    let Some(cpu) = process.get_arm_interface(core_index) else {
        return;
    };
    let mut ctx = crate::arm::arm_interface::ThreadContext::default();
    cpu.get_context(&mut ctx);
    log::info!(
        "svc::{} ctx tid={} pc=0x{:08X} lr=0x{:08X} sp=0x{:08X}",
        label,
        current_thread_id,
        ctx.pc,
        ctx.lr,
        ctx.sp
    );
    if should_trace_sync_backtrace_once(current_thread_id) {
        let bt = crate::arm::debug::get_backtrace_from_context(&process, &ctx);
        for (index, entry) in bt.iter().take(12).enumerate() {
            log::info!(
                "svc::{} bt[{}]: tid={} module={} addr=0x{:X} orig=0x{:X} off=0x{:X} symbol={}",
                label,
                index,
                current_thread_id,
                entry.module,
                entry.address,
                entry.original_address,
                entry.offset,
                entry.name,
            );
        }
    }
}

/// Attempts to lock a mutex.
pub fn arbitrate_lock(
    system: &System,
    thread_handle: Handle,
    address: u64,
    tag: u32,
) -> ResultCode {
    log::trace!(
        "svc::ArbitrateLock called thread_handle=0x{:08X}, address=0x{:X}, tag=0x{:08X}",
        thread_handle,
        address,
        tag
    );
    if should_trace_sync_debug() {
        log::info!(
            "svc::ArbitrateLock tid={:?} thread_handle=0x{:08X} address=0x{:X} tag=0x{:08X}",
            system.current_thread_id(),
            thread_handle,
            address,
            tag
        );
        log_sync_context(system, "ArbitrateLock");
    }

    // Validate the input address.
    if is_kernel_address(address as usize) {
        return RESULT_INVALID_CURRENT_MEMORY;
    }
    if address % 4 != 0 {
        return RESULT_INVALID_ADDRESS;
    }

    let Some(current_thread) = system.current_thread() else {
        return RESULT_INVALID_HANDLE;
    };

    let result = KConditionVariable::wait_for_address(
        system.current_process_arc(),
        &current_thread,
        thread_handle,
        address,
        tag,
    );

    log::trace!(
        "svc::ArbitrateLock return thread_handle=0x{:08X}, address=0x{:X}, tag=0x{:08X}, result={:#x}",
        thread_handle,
        address,
        tag,
        result.get_inner_value()
    );

    result
}

/// Unlocks a mutex.
pub fn arbitrate_unlock(system: &System, address: u64) -> ResultCode {
    log::trace!("svc::ArbitrateUnlock called address=0x{:X}", address);
    if should_trace_sync_debug() {
        log::info!(
            "svc::ArbitrateUnlock tid={:?} address=0x{:X}",
            system.current_thread_id(),
            address
        );
        log_sync_context(system, "ArbitrateUnlock");
    }

    // Validate the input address.
    if is_kernel_address(address as usize) {
        return RESULT_INVALID_CURRENT_MEMORY;
    }
    if address % 4 != 0 {
        return RESULT_INVALID_ADDRESS;
    }

    let Some(current_thread) = system.current_thread() else {
        return RESULT_INVALID_HANDLE;
    };

    let result = KConditionVariable::signal_to_address(
        system.current_process_arc(),
        &current_thread,
        address,
    );

    log::trace!(
        "svc::ArbitrateUnlock return address=0x{:X}, result={:#x}",
        address,
        result.get_inner_value()
    );

    result
}
