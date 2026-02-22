// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use log::debug;
use ruzu_common::error;
use ruzu_common::{Handle, ResultCode, VAddr};

use crate::kernel::KernelCore;
use crate::thread::{ThreadState, THREAD_PRIORITY_LOWEST};

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

// SleepThread (0x0B) is now implemented in svc.rs with thread wait state support.

/// SVC 0x0C: GetThreadPriority
pub fn svc_get_thread_priority(kernel: &KernelCore, handle: Handle) -> Result<u32, ResultCode> {
    let process = kernel.process().ok_or(error::INVALID_STATE)?;
    let thread = process.thread_by_handle(handle).ok_or(error::INVALID_HANDLE)?;
    Ok(thread.priority)
}

/// SVC 0x0D: SetThreadPriority
pub fn svc_set_thread_priority(
    kernel: &mut KernelCore,
    handle: Handle,
    priority: u32,
) -> ResultCode {
    debug!("SetThreadPriority: handle={}, priority={}", handle, priority);

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
