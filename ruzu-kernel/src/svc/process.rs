// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use log::{debug, info};
use common::{error, ResultCode};

use crate::kernel::KernelCore;

/// SVC 0x07: ExitProcess
pub fn svc_exit_process(kernel: &mut KernelCore) {
    info!("ExitProcess called");
    if let Some(process) = kernel.process_mut() {
        process.is_running = false;
    }
    kernel.stop();
}

/// SVC 0x24: GetProcessId
pub fn svc_get_process_id(kernel: &KernelCore) -> Result<u64, ResultCode> {
    let process = kernel.process().ok_or(error::INVALID_STATE)?;
    Ok(process.pid)
}

/// SVC 0x23: GetProcessInfo
///
/// Returns process information based on info_type.
/// info_type 0 = ProcessState (0=created, 1=attached, 2=running, 3=crashed, 4=terminated).
pub fn svc_get_process_info(info_type: u32) -> u64 {
    match info_type {
        // ProcessState: 0=created, 1=attached, 2=running, 3=crashed, 4=terminated
        0 => 2, // Running
        // All other types: return 0 (sufficient to unblock init checks).
        _ => {
            debug!("GetProcessInfo: unhandled info_type={}", info_type);
            0
        }
    }
}
