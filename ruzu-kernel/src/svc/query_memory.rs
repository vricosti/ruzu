// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use common::{error, ResultCode, VAddr};
use log::debug;

use crate::kernel::KernelCore;
use ruzu_cpu::CpuState;

/// SVC 0x06: QueryMemory
/// X0 = memory_info address, X2 = query address
/// Returns: X0 = result, X1 = page_info
pub fn svc_query_memory(
    kernel: &mut KernelCore,
    _cpu: &mut CpuState,
    info_addr: VAddr,
    query_addr: VAddr,
) -> ResultCode {
    debug!(
        "QueryMemory: info_addr=0x{:X}, query_addr=0x{:X}",
        info_addr, query_addr
    );

    let process = match kernel.process_mut() {
        Some(p) => p,
        None => return error::INVALID_STATE,
    };

    let mem_info = process.memory.query_memory(query_addr);

    // Write MemoryInfo struct to guest memory (0x28 bytes):
    // [0x00] base_addr: u64
    // [0x08] size: u64
    // [0x10] state: u32
    // [0x14] attribute: u32
    // [0x18] permission: u32
    // [0x1C] ipc_count: u32
    // [0x20] device_count: u32
    // [0x24] padding: u32
    let _ = process.memory.write_u64(info_addr, mem_info.base_addr);
    let _ = process.memory.write_u64(info_addr + 0x08, mem_info.size);
    let _ = process
        .memory
        .write_u32(info_addr + 0x10, mem_info.state as u32);
    let _ = process.memory.write_u32(info_addr + 0x14, 0); // attribute
    let _ = process
        .memory
        .write_u32(info_addr + 0x18, mem_info.permission.bits());
    let _ = process.memory.write_u32(info_addr + 0x1C, 0); // ipc_count
    let _ = process.memory.write_u32(info_addr + 0x20, 0); // device_count
    let _ = process.memory.write_u32(info_addr + 0x24, 0); // padding

    ResultCode::SUCCESS
}
