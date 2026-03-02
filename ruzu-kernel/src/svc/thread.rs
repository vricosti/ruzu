// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use log::debug;
use ruzu_common::error;
use ruzu_common::{Handle, ResultCode, VAddr};

use crate::kernel::KernelCore;
use crate::thread::{ThreadState, WaitReason, THREAD_PRIORITY_LOWEST};
use ruzu_cpu::CpuState;

/// SVC 0x08: CreateThread
pub fn svc_create_thread(
    kernel: &mut KernelCore,
    entry: VAddr,
    arg: u64,
    stack_top: VAddr,
    priority: u32,
    core_id: i32,
) -> Result<Handle, ResultCode> {
    debug!(
        "CreateThread: entry=0x{:X}, arg=0x{:X}, stack=0x{:X}, pri={}, core={}",
        entry, arg, stack_top, priority, core_id
    );

    if priority > THREAD_PRIORITY_LOWEST {
        return Err(error::INVALID_PRIORITY);
    }

    let process = kernel.process_mut().ok_or(error::INVALID_STATE)?;
    process
        .create_thread(entry, arg, stack_top, priority, core_id)
        .map_err(|_| error::OUT_OF_RESOURCE)
}

/// SVC 0x09: StartThread
pub fn svc_start_thread(kernel: &mut KernelCore, handle: Handle) -> ResultCode {
    debug!("StartThread: handle={}", handle);

    let process = match kernel.process_mut() {
        Some(p) => p,
        None => return error::INVALID_STATE,
    };

    if let Some(thread) = process.thread_by_handle_mut(handle) {
        thread.start();
        ResultCode::SUCCESS
    } else {
        error::INVALID_HANDLE
    }
}

/// SVC 0x0A: ExitThread
pub fn svc_exit_thread(kernel: &mut KernelCore) {
    debug!("ExitThread");

    if let Some(process) = kernel.process_mut() {
        if let Some(thread) = process.current_thread_mut() {
            thread.state = ThreadState::Terminated;
        }
    }
}

/// SVC 0x0B: SleepThread
pub fn svc_sleep_thread(kernel: &mut KernelCore, cpu: &mut CpuState, ns: i64) {
    debug!("SleepThread: ns={}", ns);

    let thread_idx = match kernel.current_thread_idx {
        Some(idx) => idx,
        None => {
            // No thread context — fallback to success.
            cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
            return;
        }
    };

    if ns == 0 || ns == -2 {
        // Yield: set thread to Runnable and let scheduler pick another.
        // The thread stays Runnable so it can be re-scheduled immediately.
        cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
    } else if ns > 0 {
        // Timed sleep: convert ns to ticks and block.
        let current_tick = kernel.tick_counter;
        let timeout_ticks = (ns as u128 * 19_200_000 / 1_000_000_000) as u64;
        let wake_tick = current_tick + timeout_ticks;

        let process = kernel.process_mut().unwrap();
        process.threads[thread_idx].begin_wait(WaitReason::Sleep { wake_tick }, current_tick);
        // Result will be set when woken by timeout check.
    } else {
        // ns == -1: sleep until explicitly woken (rare).
        cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
    }
}

/// SVC 0x0C: GetThreadPriority
pub fn svc_get_thread_priority(kernel: &KernelCore, handle: Handle) -> Result<u32, ResultCode> {
    let process = kernel.process().ok_or(error::INVALID_STATE)?;
    let thread = process
        .thread_by_handle(handle)
        .ok_or(error::INVALID_HANDLE)?;
    Ok(thread.priority)
}

/// SVC 0x0D: SetThreadPriority
pub fn svc_set_thread_priority(
    kernel: &mut KernelCore,
    handle: Handle,
    priority: u32,
) -> ResultCode {
    debug!(
        "SetThreadPriority: handle={}, priority={}",
        handle, priority
    );

    if priority > THREAD_PRIORITY_LOWEST {
        return error::INVALID_PRIORITY;
    }

    let process = match kernel.process_mut() {
        Some(p) => p,
        None => return error::INVALID_STATE,
    };

    if let Some(thread) = process.thread_by_handle_mut(handle) {
        thread.priority = priority;
        ResultCode::SUCCESS
    } else {
        error::INVALID_HANDLE
    }
}

/// SVC 0x0E: SetThreadCoreMask
///
/// Accept and ignore in single-core emulation mode.
pub fn svc_set_thread_core_mask(
    _handle: Handle,
    _ideal_core: u32,
    _affinity_mask: u64,
) -> ResultCode {
    debug!("SetThreadCoreMask: accept and ignore (single-core emulation)");
    ResultCode::SUCCESS
}

/// SVC 0x0F: GetThreadCoreMask
///
/// Returns ideal_core=0 and affinity_mask=0xF (4 cores).
pub fn svc_get_thread_core_mask(_handle: Handle) -> Result<(u32, u64), ResultCode> {
    debug!("GetThreadCoreMask: returning ideal_core=0, mask=0xF");
    Ok((0, 0xF))
}

/// SVC 0x25: GetThreadId
pub fn svc_get_thread_id(kernel: &KernelCore) -> Result<u64, ResultCode> {
    let process = kernel.process().ok_or(error::INVALID_STATE)?;
    let thread = process.current_thread().ok_or(error::INVALID_STATE)?;
    Ok(thread.thread_id)
}
