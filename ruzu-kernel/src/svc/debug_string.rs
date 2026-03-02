// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use log::info;
use ruzu_common::{error, ResultCode, VAddr};

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
