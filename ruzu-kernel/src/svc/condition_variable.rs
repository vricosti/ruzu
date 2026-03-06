// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use log::debug;
use common::{error, ResultCode, VAddr};

use crate::kernel::KernelCore;
use crate::thread::{ThreadState, WaitReason};
use ruzu_cpu::CpuState;

/// SVC 0x1C: WaitProcessWideKeyAtomic (condition variable wait)
pub fn svc_wait_process_wide_key_atomic(
    kernel: &mut KernelCore,
    cpu: &mut CpuState,
    mutex_addr: VAddr,
    condvar_addr: VAddr,
    _tag: u32,
    timeout_ns: i64,
) {
    debug!(
        "WaitProcessWideKeyAtomic: mutex=0x{:X}, condvar=0x{:X}, timeout={}",
        mutex_addr, condvar_addr, timeout_ns
    );

    let thread_idx = match kernel.current_thread_idx {
        Some(idx) => idx,
        None => {
            cpu.x[0] = error::INVALID_STATE.raw() as u64;
            return;
        }
    };

    // Step 1: Unlock the mutex (same logic as ArbitrateUnlock).
    super::lock::svc_arbitrate_unlock(kernel, cpu, mutex_addr);

    // Step 2: Block the calling thread on the condvar.
    let tick = kernel.tick_counter;
    let process = kernel.process_mut().unwrap();
    process.threads[thread_idx].begin_wait(
        WaitReason::CondVar {
            condvar_addr,
            mutex_addr,
        },
        tick,
    );
    // Result will be set when woken by SignalProcessWideKey.
}

/// SVC 0x1D: SignalProcessWideKey (condition variable signal)
pub fn svc_signal_process_wide_key(
    kernel: &mut KernelCore,
    cpu: &mut CpuState,
    condvar_addr: VAddr,
    count: i32,
) {
    debug!(
        "SignalProcessWideKey: condvar=0x{:X}, count={}",
        condvar_addr, count
    );

    let process = match kernel.process_mut() {
        Some(p) => p,
        None => {
            cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
            return;
        }
    };

    // Collect indices of threads waiting on this condvar, sorted by priority.
    let mut waiters: Vec<usize> = process
        .threads
        .iter()
        .enumerate()
        .filter(|(_, t)| {
            t.state == ThreadState::Waiting
                && matches!(t.wait_reason, WaitReason::CondVar { condvar_addr: addr, .. } if addr == condvar_addr)
        })
        .map(|(i, _)| i)
        .collect();

    waiters.sort_by_key(|&i| process.threads[i].priority);

    let wake_count = if count < 0 {
        waiters.len()
    } else {
        (count as usize).min(waiters.len())
    };

    for &idx in waiters.iter().take(wake_count) {
        // wake() sets state to Runnable and writes x[0] = SUCCESS.
        process.threads[idx].wake(ResultCode::SUCCESS.raw(), -1);
    }

    cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
}
