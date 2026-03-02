// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use log::debug;
use ruzu_common::{error, Handle, ResultCode};

use crate::kernel::KernelCore;
use crate::objects::{KEvent, KernelObject};
use crate::thread::{ThreadState, WaitReason};
use ruzu_cpu::CpuState;

/// SVC 0x45: CreateEvent
pub fn svc_create_event(kernel: &mut KernelCore, cpu: &mut CpuState) {
    debug!("CreateEvent");

    let process = match kernel.process_mut() {
        Some(p) => p,
        None => {
            cpu.x[0] = error::INVALID_STATE.raw() as u64;
            return;
        }
    };

    // Create the event and add writable + readable handles.
    let event = KEvent::new();
    let writable_handle = match process.handle_table.add(KernelObject::Event(event.clone())) {
        Ok(h) => h,
        Err(rc) => {
            cpu.x[0] = rc.raw() as u64;
            return;
        }
    };

    let mut readable_event = KEvent::new();
    readable_event.writable_handle = Some(writable_handle);
    let readable_handle = match process
        .handle_table
        .add(KernelObject::Event(readable_event))
    {
        Ok(h) => h,
        Err(rc) => {
            let _ = process.handle_table.close(writable_handle);
            cpu.x[0] = rc.raw() as u64;
            return;
        }
    };

    // Update writable event with cross-reference.
    if let Ok(KernelObject::Event(ev)) = process.handle_table.get_mut(writable_handle) {
        ev.readable_handle = Some(readable_handle);
        ev.writable_handle = Some(writable_handle);
    }

    cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
    cpu.x[1] = writable_handle as u64;
    cpu.x[2] = readable_handle as u64;
    debug!(
        "CreateEvent: writable={}, readable={}",
        writable_handle, readable_handle
    );
}

/// SVC 0x11: SignalEvent
pub fn svc_signal_event(kernel: &mut KernelCore, cpu: &mut CpuState, handle: Handle) {
    debug!("SignalEvent: handle={}", handle);

    let process = match kernel.process_mut() {
        Some(p) => p,
        None => {
            cpu.x[0] = error::INVALID_STATE.raw() as u64;
            return;
        }
    };

    // Look up the event and get its readable handle, then signal it.
    let readable_handle = match process.handle_table.get_mut(handle) {
        Ok(KernelObject::Event(ev)) => {
            ev.signaled = true;
            ev.readable_handle
        }
        _ => {
            cpu.x[0] = error::INVALID_HANDLE.raw() as u64;
            return;
        }
    };

    // Also mark the readable event as signaled.
    if let Some(rh) = readable_handle {
        if rh != handle {
            if let Ok(KernelObject::Event(rev)) = process.handle_table.get_mut(rh) {
                rev.signaled = true;
            }
        }
    }

    // Wake any threads waiting on the readable handle (or writable handle).
    let handles_to_check: Vec<Handle> = [Some(handle), readable_handle]
        .iter()
        .filter_map(|h| *h)
        .collect();

    for thread in process.threads.iter_mut() {
        if thread.state != ThreadState::Waiting {
            continue;
        }
        if let WaitReason::Synchronization { ref handles, .. } = thread.wait_reason {
            for (idx, h) in handles.iter().enumerate() {
                if handles_to_check.contains(h) {
                    // wake() sets state to Runnable and writes x[0]/x[1].
                    thread.wake(ResultCode::SUCCESS.raw(), idx as i32);
                    break;
                }
            }
        }
    }

    cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
}

/// SVC 0x12: ClearEvent / SVC 0x17: ResetSignal
pub fn svc_clear_event(kernel: &mut KernelCore, cpu: &mut CpuState, handle: Handle) {
    debug!("ClearEvent/ResetSignal: handle={}", handle);

    let process = match kernel.process_mut() {
        Some(p) => p,
        None => {
            cpu.x[0] = error::INVALID_STATE.raw() as u64;
            return;
        }
    };

    match process.handle_table.get_mut(handle) {
        Ok(KernelObject::Event(ev)) => {
            ev.signaled = false;
            cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
        }
        _ => {
            cpu.x[0] = error::INVALID_HANDLE.raw() as u64;
        }
    }
}
