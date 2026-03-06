// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use log::{debug, warn};
use common::{error, Handle, ResultCode, VAddr};

use crate::kernel::KernelCore;

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

/// SVC 0x33: GetThreadContext3
pub fn svc_get_thread_context3(
    kernel: &mut KernelCore,
    thread_handle: Handle,
    context_addr: VAddr,
) -> ResultCode {
    debug!(
        "GetThreadContext3: thread_handle={}, context_addr=0x{:X}",
        thread_handle, context_addr
    );

    let process = match kernel.process_mut() {
        Some(p) => p,
        None => return error::INVALID_STATE,
    };

    let target_idx = process
        .threads
        .iter()
        .position(|t| t.handle == thread_handle);

    let idx = match target_idx {
        Some(i) => i,
        None => return error::INVALID_HANDLE,
    };

    let thread = &process.threads[idx];
    let cpu = &thread.cpu_state;

    // Layout: X0-X30 (31*8=248 bytes), SP (8 bytes), PC (8 bytes),
    //         NZCV (4 bytes), padding (4 bytes), V0-V31 (32*16=512 bytes)
    // Total: 248 + 8 + 8 + 4 + 4 + 512 = 784 bytes
    let mut offset = context_addr;

    // X0-X30
    for i in 0..31 {
        let _ = process.memory.write_u64(offset, cpu.x[i]);
        offset += 8;
    }

    // SP
    let _ = process.memory.write_u64(offset, cpu.sp);
    offset += 8;

    // PC
    let _ = process.memory.write_u64(offset, cpu.pc);
    offset += 8;

    // NZCV (4 bytes) + padding (4 bytes)
    let _ = process.memory.write_u32(offset, cpu.nzcv);
    offset += 4;
    let _ = process.memory.write_u32(offset, 0); // padding
    offset += 4;

    // V0-V31 (128-bit each, stored as [lo, hi])
    for i in 0..32 {
        let lo = cpu.v[i][0];
        let hi = cpu.v[i][1];
        let _ = process.memory.write_u64(offset, lo);
        let _ = process.memory.write_u64(offset + 8, hi);
        offset += 16;
    }

    ResultCode::SUCCESS
}
