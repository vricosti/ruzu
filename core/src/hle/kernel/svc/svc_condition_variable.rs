//! Port of zuyu/src/core/hle/kernel/svc/svc_condition_variable.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-20
//!
//! SVC handlers for condition variable operations.

use crate::core::System;
use crate::hle::kernel::k_memory_layout::is_kernel_address;
use crate::hle::kernel::svc::svc_results::*;
use crate::hle::result::ResultCode;

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

    // Validate input.
    if is_kernel_address(address as usize) {
        return RESULT_INVALID_CURRENT_MEMORY;
    }
    if address % 4 != 0 {
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

    ResultCode::new(result)
}

/// Signal process wide key.
pub fn signal_process_wide_key(system: &System, cv_key: u64, count: i32) {
    log::trace!(
        "svc::SignalProcessWideKey called, cv_key=0x{:X}, count=0x{:08X}",
        cv_key,
        count
    );

    // Upstream: Common::AlignDown(cv_key, sizeof(u32))
    let aligned_cv_key = cv_key & !3u64;
    if let Some(current_thread) = system.current_thread() {
        let scheduler_lock_ptr = current_thread.lock().unwrap().scheduler_lock_ptr;
        if scheduler_lock_ptr != 0 {
            let scheduler_lock = unsafe {
                &*(scheduler_lock_ptr
                    as *const crate::hle::kernel::k_scheduler_lock::KAbstractSchedulerLock)
            };
            let _scheduler_guard =
                crate::hle::kernel::k_scheduler_lock::KScopedSchedulerLock::new(scheduler_lock);
            system
                .current_process_arc()
                .lock()
                .unwrap()
                .signal_condition_variable(aligned_cv_key, count);
        } else {
            system
                .current_process_arc()
                .lock()
                .unwrap()
                .signal_condition_variable(aligned_cv_key, count);
        }
    } else {
        system
            .current_process_arc()
            .lock()
            .unwrap()
            .signal_condition_variable(aligned_cv_key, count);
    }
    log::trace!(
        "svc::SignalProcessWideKey return cv_key=0x{:X}, count={}",
        aligned_cv_key,
        count
    );
}
