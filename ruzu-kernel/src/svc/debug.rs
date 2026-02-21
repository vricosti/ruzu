// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use log::{info, warn};
use ruzu_common::error;
use ruzu_common::{ResultCode, VAddr};

use crate::kernel::KernelCore;

/// SVC 0x27: OutputDebugString
/// X0 = pointer to string, X1 = length
pub fn svc_output_debug_string(kernel: &mut KernelCore, addr: VAddr, len: usize) -> ResultCode {
    let process = match kernel.process() {
        Some(p) => p,
        None => return error::INVALID_STATE,
    };

    // Read string from guest memory
    match process.memory.read_bytes(addr, len) {
        Ok(bytes) => {
            let msg = String::from_utf8_lossy(&bytes);
            info!("[Guest Debug] {}", msg.trim_end());
            ResultCode::SUCCESS
        }
        Err(_) => error::INVALID_ADDRESS,
    }
}

/// SVC 0x26: Break
pub fn svc_break(kernel: &mut KernelCore, reason: u64, info1: u64, info2: u64) {
    warn!(
        "svcBreak: reason=0x{:X}, info1=0x{:X}, info2=0x{:X}",
        reason, info1, info2
    );

    // Reason 0 = panic, reason 1 = assert, reason 2 = user
    match reason & 0xFF {
        0 => warn!("Guest panic!"),
        1 => warn!("Guest assertion failed!"),
        2 => warn!("Guest user break"),
        _ => warn!("Guest break (unknown reason)"),
    }

    // For Phase 1, stop execution on Break
    kernel.stop();
}

/// SVC 0x07: ExitProcess
pub fn svc_exit_process(kernel: &mut KernelCore) {
    info!("ExitProcess called");
    if let Some(process) = kernel.process_mut() {
        process.is_running = false;
    }
    kernel.stop();
}
