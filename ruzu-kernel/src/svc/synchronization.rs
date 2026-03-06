// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use log::debug;
use common::{error, Handle, ResultCode, VAddr};

use crate::kernel::KernelCore;
use crate::objects::KernelObject;
use crate::thread::{ThreadState, WaitReason};
use ruzu_cpu::CpuState;

/// SVC 0x16: CloseHandle
pub fn svc_close_handle(kernel: &mut KernelCore, handle: Handle) -> ResultCode {
    debug!("CloseHandle: handle={}", handle);

    let process = match kernel.process_mut() {
        Some(p) => p,
        None => return error::INVALID_STATE,
    };

    match process.handle_table.close(handle) {
        Ok(_) => ResultCode::SUCCESS,
        Err(rc) => rc,
    }
}

/// SVC 0x18: WaitSynchronization
pub fn svc_wait_synchronization(
    kernel: &mut KernelCore,
    cpu: &mut CpuState,
    handles_ptr: VAddr,
    handles_count: usize,
    timeout_ns: i64,
) {
    debug!(
        "WaitSynchronization: ptr=0x{:X}, count={}, timeout={}",
        handles_ptr, handles_count, timeout_ns
    );

    if handles_count == 0 || handles_count > 64 {
        cpu.x[0] = error::OUT_OF_RANGE.raw() as u64;
        return;
    }

    let thread_idx = match kernel.current_thread_idx {
        Some(idx) => idx,
        None => {
            cpu.x[0] = error::INVALID_STATE.raw() as u64;
            return;
        }
    };

    let process = match kernel.process_mut() {
        Some(p) => p,
        None => {
            cpu.x[0] = error::INVALID_STATE.raw() as u64;
            return;
        }
    };

    // Read handle array from guest memory.
    let mut handles = Vec::with_capacity(handles_count);
    for i in 0..handles_count {
        let addr = handles_ptr + (i as u64) * 4;
        match process.memory.read_u32(addr) {
            Ok(h) => handles.push(h),
            Err(_) => {
                cpu.x[0] = error::INVALID_ADDRESS.raw() as u64;
                return;
            }
        }
    }

    // Check if any handle is already signaled.
    for (idx, &handle) in handles.iter().enumerate() {
        let is_signaled = match process.handle_table.get(handle) {
            Ok(KernelObject::Event(ev)) => ev.signaled,
            _ => false,
        };

        if is_signaled {
            // Auto-clear the event.
            if let Ok(KernelObject::Event(ev)) = process.handle_table.get_mut(handle) {
                ev.signaled = false;
            }
            cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
            cpu.x[1] = idx as u64;
            debug!(
                "WaitSynchronization: handle {} already signaled (idx={})",
                handle, idx
            );
            return;
        }
    }

    // None signaled.
    if timeout_ns == 0 {
        cpu.x[0] = error::TIMEOUT.raw() as u64;
        cpu.x[1] = u64::MAX; // invalid index
        return;
    }

    // Check cancel_pending flag before blocking.
    if process.threads[thread_idx].cancel_pending {
        process.threads[thread_idx].cancel_pending = false;
        cpu.x[0] = error::CANCELLED.raw() as u64;
        cpu.x[1] = u64::MAX;
        return;
    }

    // Block the calling thread.
    let tick = kernel.tick_counter;
    let process = kernel.process_mut().unwrap();
    process.threads[thread_idx].begin_wait(
        WaitReason::Synchronization {
            handles,
            timeout_ns,
        },
        tick,
    );
    // Don't set cpu.x[0] yet — will be set when woken by SignalEvent or timeout.
    debug!(
        "WaitSynchronization: thread {} blocked (timeout_ns={})",
        thread_idx, timeout_ns
    );
}

/// SVC 0x19: CancelSynchronization
pub fn svc_cancel_synchronization(
    kernel: &mut KernelCore,
    cpu: &mut CpuState,
    thread_handle: Handle,
) {
    debug!("CancelSynchronization: thread_handle={}", thread_handle);

    let process = match kernel.process_mut() {
        Some(p) => p,
        None => {
            cpu.x[0] = error::INVALID_STATE.raw() as u64;
            return;
        }
    };

    // Find the target thread by handle.
    let target_idx = process
        .threads
        .iter()
        .position(|t| t.handle == thread_handle);

    match target_idx {
        Some(idx) => {
            if process.threads[idx].state == ThreadState::Waiting {
                // Thread is waiting — wake it with CANCELLED.
                process.threads[idx].wake(error::CANCELLED.raw(), -1);
            } else {
                // Thread is not waiting — set cancel_pending flag.
                process.threads[idx].cancel_pending = true;
            }
            cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
        }
        None => {
            cpu.x[0] = error::INVALID_HANDLE.raw() as u64;
        }
    }
}
