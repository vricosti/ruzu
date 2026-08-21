//! Port of zuyu/src/core/hle/kernel/svc/svc_condition_variable.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-20
//!
//! SVC handlers for condition variable operations.

use crate::core::System;
use crate::hle::kernel::k_memory_layout::is_kernel_address;
use crate::hle::kernel::svc::svc_results::*;
use crate::hle::result::ResultCode;

fn trace_lock_addr_target() -> Option<u64> {
    let raw = std::env::var("RUZU_TRACE_LOCK_ADDR").ok()?;
    let trimmed = raw.trim();
    let trimmed = trimmed
        .strip_prefix("0x")
        .or_else(|| trimmed.strip_prefix("0X"))
        .unwrap_or(trimmed);
    u64::from_str_radix(trimmed, 16).ok()
}

fn should_trace_wait_process_wide_key(address: u64, cv_key: u64) -> bool {
    common::trace::is_enabled(common::trace::cat::LOCK_PI)
        && trace_lock_addr_target().map_or(true, |target| target == address || target == cv_key)
}

fn trace_wait_process_wide_key(
    stage: u64,
    tid: u64,
    core: u64,
    address: u64,
    cv_key: u64,
    tag: u32,
    timeout_ns: i64,
    result: ResultCode,
) {
    common::trace::emit_raw(
        common::trace::cat::LOCK_PI,
        &[
            stage,
            tid,
            address,
            0,
            tag as u64,
            0,
            core,
            cv_key,
            0,
            timeout_ns as u64,
            result.get_inner_value() as u64,
            0,
            0,
            0,
        ],
    );
}

fn trace_signal_process_wide_key(stage: u64, tid: u64, core: u64, cv_key: u64, count: i32) {
    common::trace::emit_raw(
        common::trace::cat::LOCK_PI,
        &[
            stage,
            tid,
            cv_key,
            count as u32 as u64,
            core,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
        ],
    );
}

/// Wait process wide key atomic.
pub fn wait_process_wide_key_atomic(
    system: &System,
    address: u64,
    cv_key: u64,
    tag: u32,
    timeout_ns: i64,
) -> ResultCode {
    log::trace!(
        "svc::WaitProcessWideKeyAtomic called address=0x{:X}, cv_key=0x{:X}, tag=0x{:08X}, timeout_ns={}",
        address, cv_key, tag, timeout_ns
    );
    let entry_tid = system.current_thread_id().unwrap_or(0);
    let entry_core = system
        .kernel()
        .map(|kernel| kernel.current_physical_core_index() as u64)
        .unwrap_or(0);
    log::trace!(
        "WaitProcessWideKeyAtomic tid={} core={} address=0x{:X} cv_key=0x{:X} tag=0x{:08X} timeout_ns={}",
        entry_tid,
        entry_core,
        address,
        cv_key,
        tag,
        timeout_ns
    );
    let trace_wait = should_trace_wait_process_wide_key(address, cv_key);
    if trace_wait {
        trace_wait_process_wide_key(
            20,
            entry_tid,
            entry_core,
            address,
            cv_key,
            tag,
            timeout_ns,
            crate::hle::result::RESULT_SUCCESS,
        );
    }

    // Validate input.
    if is_kernel_address(address as usize) {
        if trace_wait {
            trace_wait_process_wide_key(
                21,
                entry_tid,
                entry_core,
                address,
                cv_key,
                tag,
                timeout_ns,
                RESULT_INVALID_CURRENT_MEMORY,
            );
        }
        return RESULT_INVALID_CURRENT_MEMORY;
    }
    if address % 4 != 0 {
        if trace_wait {
            trace_wait_process_wide_key(
                21,
                entry_tid,
                entry_core,
                address,
                cv_key,
                tag,
                timeout_ns,
                RESULT_INVALID_ADDRESS,
            );
        }
        return RESULT_INVALID_ADDRESS;
    }

    // Convert timeout from nanoseconds to ticks.
    // Upstream: kernel.HardwareTimer().GetTick() + offset_tick + 2
    let timeout: i64 = if timeout_ns > 0 {
        let offset_tick = timeout_ns;
        if offset_tick > 0 {
            let hardware_tick = system
                .kernel()
                .and_then(|_| crate::hle::kernel::kernel::get_current_hardware_tick())
                .unwrap_or(0);
            let t = hardware_tick + offset_tick + 2;
            if t <= 0 {
                i64::MAX
            } else {
                t
            }
        } else {
            i64::MAX
        }
    } else {
        timeout_ns
    };

    let Some(current_thread) = system.current_thread() else {
        if trace_wait {
            trace_wait_process_wide_key(
                21,
                entry_tid,
                entry_core,
                address,
                cv_key,
                tag,
                timeout_ns,
                RESULT_INVALID_HANDLE,
            );
        }
        return RESULT_INVALID_HANDLE;
    };

    // Upstream: Common::AlignDown(cv_key, sizeof(u32)) — aligns down to 4-byte boundary.
    let aligned_cv_key = cv_key & !3u64;
    let result = crate::hle::kernel::k_process::KProcess::wait_condition_variable(
        &system.current_process_arc(),
        &current_thread,
        address,
        aligned_cv_key,
        tag,
        timeout,
    );

    log::trace!(
        "svc::WaitProcessWideKeyAtomic return address=0x{:X}, cv_key=0x{:X}, result={:#x}",
        address,
        aligned_cv_key,
        result
    );
    if trace_wait {
        trace_wait_process_wide_key(
            21,
            entry_tid,
            entry_core,
            address,
            aligned_cv_key,
            tag,
            timeout_ns,
            ResultCode::new(result),
        );
    }

    ResultCode::new(result)
}

/// Signal process wide key.
pub fn signal_process_wide_key(system: &System, cv_key: u64, count: i32) {
    log::trace!(
        "svc::SignalProcessWideKey called, cv_key=0x{:X}, count=0x{:08X}",
        cv_key,
        count
    );
    let entry_tid = system.current_thread_id().unwrap_or(0);
    let entry_core = system
        .kernel()
        .map(|kernel| kernel.current_physical_core_index() as u64)
        .unwrap_or(0);
    log::trace!(
        "SignalProcessWideKey tid={} core={} cv_key=0x{:X} count={}",
        entry_tid,
        entry_core,
        cv_key,
        count
    );

    // Upstream: Common::AlignDown(cv_key, sizeof(u32))
    let aligned_cv_key = cv_key & !3u64;
    let trace_signal = should_trace_wait_process_wide_key(aligned_cv_key, aligned_cv_key);
    if trace_signal {
        trace_signal_process_wide_key(24, entry_tid, entry_core, aligned_cv_key, count);
    }
    system
        .current_process_arc()
        .lock()
        .unwrap()
        .signal_condition_variable(aligned_cv_key, count);
    if trace_signal {
        trace_signal_process_wide_key(25, entry_tid, entry_core, aligned_cv_key, count);
    }
    log::trace!(
        "svc::SignalProcessWideKey return cv_key=0x{:X}, count={}",
        aligned_cv_key,
        count
    );
}
