// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use common::{error, Handle, ResultCode};
use log::debug;

use crate::kernel::KernelCore;
use crate::thread::{ThreadState, WaitReason};

/// SVC 0x32: SetThreadActivity
pub fn svc_set_thread_activity(
    kernel: &mut KernelCore,
    thread_handle: Handle,
    activity: u32,
) -> ResultCode {
    debug!(
        "SetThreadActivity: thread_handle={}, activity={}",
        thread_handle, activity
    );

    let tick = kernel.tick_counter;
    let process = match kernel.process_mut() {
        Some(p) => p,
        None => return error::INVALID_STATE,
    };

    let target_idx = process
        .threads
        .iter()
        .position(|t| t.handle == thread_handle);

    match target_idx {
        Some(idx) => {
            match activity {
                1 => {
                    // Suspend
                    if process.threads[idx].state == ThreadState::Waiting
                        && matches!(process.threads[idx].wait_reason, WaitReason::Suspended)
                    {
                        // Already suspended.
                        return ResultCode::SUCCESS;
                    }
                    process.threads[idx].begin_wait(WaitReason::Suspended, tick);
                }
                0 => {
                    // Resume
                    if process.threads[idx].state == ThreadState::Waiting
                        && matches!(process.threads[idx].wait_reason, WaitReason::Suspended)
                    {
                        process.threads[idx].wake(ResultCode::SUCCESS.raw(), -1);
                    }
                }
                _ => return error::INVALID_ENUM_VALUE,
            }
            ResultCode::SUCCESS
        }
        None => error::INVALID_HANDLE,
    }
}
