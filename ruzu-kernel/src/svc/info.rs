// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use log::debug;
use ruzu_common::error;
use ruzu_common::{Handle, ResultCode, CNTFRQ_HZ};

use crate::kernel::KernelCore;

/// GetInfo info IDs (from Switch kernel).
mod info_id {
    pub const CORE_MASK: u32 = 0;
    pub const PRIORITY_MASK: u32 = 1;
    pub const ALIAS_REGION_BASE: u32 = 2;
    pub const ALIAS_REGION_SIZE: u32 = 3;
    pub const HEAP_REGION_BASE: u32 = 4;
    pub const HEAP_REGION_SIZE: u32 = 5;
    pub const TOTAL_MEMORY_SIZE: u32 = 6;
    pub const USED_MEMORY_SIZE: u32 = 7;
    pub const DEBUG_ATTACHED: u32 = 8;
    pub const RESOURCE_LIMIT: u32 = 9;
    pub const IDLE_TICK_COUNT: u32 = 10;
    pub const RANDOM_ENTROPY: u32 = 11;
    pub const ASLR_REGION_BASE: u32 = 12;
    pub const ASLR_REGION_SIZE: u32 = 13;
    pub const STACK_REGION_BASE: u32 = 14;
    pub const STACK_REGION_SIZE: u32 = 15;
    pub const SYSTEM_RESOURCE_SIZE: u32 = 16;
    pub const SYSTEM_RESOURCE_USAGE: u32 = 17;
    pub const PROGRAM_ID: u32 = 18;
    pub const INIT_PROCESS_ID_RANGE: u32 = 19;
    pub const USER_EXCEPTION_CONTEXT: u32 = 20;
    pub const TOTAL_NON_SYSTEM_MEMORY_SIZE: u32 = 21;
    pub const USED_NON_SYSTEM_MEMORY_SIZE: u32 = 22;
    pub const IS_APPLICATION: u32 = 23;
    pub const FREE_THREAD_COUNT: u32 = 24;
    pub const THREAD_TICK_COUNT: u32 = 25;
    pub const IS_SAFE_MODE: u32 = 26;
    pub const TRANSFER_MEMORY_BASE: u32 = 27;
    pub const TRANSFER_MEMORY_SIZE: u32 = 28;
}

/// SVC 0x1E: GetSystemTick
/// Returns the host monotonic time converted to Switch tick frequency (19.2 MHz).
pub fn svc_get_system_tick() -> u64 {
    use std::time::{SystemTime, UNIX_EPOCH};

    let elapsed = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default();

    // Convert to Switch ticks: nanoseconds * 19.2MHz / 1GHz = ns * 19.2 / 1000
    let ns = elapsed.as_nanos() as u64;
    (ns as u128 * CNTFRQ_HZ as u128 / 1_000_000_000u128) as u64
}

/// SVC 0x24: GetProcessId
pub fn svc_get_process_id(kernel: &KernelCore) -> Result<u64, ResultCode> {
    let process = kernel.process().ok_or(error::INVALID_STATE)?;
    Ok(process.pid)
}

/// SVC 0x25: GetThreadId
pub fn svc_get_thread_id(kernel: &KernelCore) -> Result<u64, ResultCode> {
    let process = kernel.process().ok_or(error::INVALID_STATE)?;
    let thread = process.current_thread().ok_or(error::INVALID_STATE)?;
    Ok(thread.thread_id)
}

/// SVC 0x29: GetInfo
/// Returns various system information based on info_id and info_sub_id.
pub fn svc_get_info(
    kernel: &KernelCore,
    info_id: u32,
    handle: Handle,
    info_sub_id: u64,
) -> Result<u64, ResultCode> {
    debug!(
        "GetInfo: id={}, handle={}, sub_id={}",
        info_id, handle, info_sub_id
    );

    let process = kernel.process().ok_or(error::INVALID_STATE)?;
    let layout = &process.layout;

    match info_id {
        info_id::CORE_MASK => {
            // CPU core mask: just core 0 for Phase 1
            Ok(0x1)
        }
        info_id::PRIORITY_MASK => {
            // All priorities available
            Ok(0xFFFF_FFFF_FFFF_FFFF)
        }
        info_id::ALIAS_REGION_BASE => Ok(layout.map_region_base),
        info_id::ALIAS_REGION_SIZE => {
            Ok(layout.map_region_end - layout.map_region_base)
        }
        info_id::HEAP_REGION_BASE => Ok(layout.heap_base),
        info_id::HEAP_REGION_SIZE => {
            // Max heap size: 4 GiB
            Ok(0x1_0000_0000)
        }
        info_id::TOTAL_MEMORY_SIZE => {
            // Report 4 GiB total
            Ok(0x1_0000_0000)
        }
        info_id::USED_MEMORY_SIZE => {
            // Report heap usage
            Ok(layout.heap_end - layout.heap_base)
        }
        info_id::DEBUG_ATTACHED => Ok(0),
        info_id::RESOURCE_LIMIT => Ok(0), // No resource limit handle
        info_id::IDLE_TICK_COUNT => Ok(0),
        info_id::RANDOM_ENTROPY => {
            // Return pseudo-random entropy
            Ok(0x12345678_DEADBEEF_u64.wrapping_add(info_sub_id))
        }
        info_id::ASLR_REGION_BASE => {
            // No ASLR in Phase 1, return code base
            Ok(0x0800_0000)
        }
        info_id::ASLR_REGION_SIZE => {
            // 39-bit address space minus code base
            Ok(layout.address_space_size - 0x0800_0000)
        }
        info_id::STACK_REGION_BASE => Ok(layout.stack_base),
        info_id::STACK_REGION_SIZE => {
            Ok(0x1000_0000) // 256 MiB stack region
        }
        info_id::SYSTEM_RESOURCE_SIZE => Ok(0),
        info_id::SYSTEM_RESOURCE_USAGE => Ok(0),
        info_id::PROGRAM_ID => Ok(process.title_id),
        info_id::INIT_PROCESS_ID_RANGE => {
            match info_sub_id {
                0 => Ok(1),  // min process ID
                1 => Ok(80), // max process ID
                _ => Err(error::INVALID_COMBINATION),
            }
        }
        info_id::USER_EXCEPTION_CONTEXT => Ok(0),
        info_id::TOTAL_NON_SYSTEM_MEMORY_SIZE => Ok(0x1_0000_0000),
        info_id::USED_NON_SYSTEM_MEMORY_SIZE => {
            Ok(layout.heap_end - layout.heap_base)
        }
        info_id::IS_APPLICATION => Ok(1), // Always application for homebrew
        info_id::FREE_THREAD_COUNT => Ok(256),
        info_id::THREAD_TICK_COUNT => Ok(svc_get_system_tick()),
        info_id::IS_SAFE_MODE => Ok(0),
        info_id::TRANSFER_MEMORY_BASE => Ok(layout.map_region_base),
        info_id::TRANSFER_MEMORY_SIZE => {
            Ok(layout.map_region_end - layout.map_region_base)
        }
        _ => {
            debug!("GetInfo: unknown info_id {}", info_id);
            Err(error::INVALID_ENUM_VALUE)
        }
    }
}
