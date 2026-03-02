// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

// --- SVC module organization (matching zuyu's svc/ directory) ---
pub mod activity;
pub mod address_arbiter;
pub mod cache;
pub mod code_memory;
pub mod condition_variable;
pub mod debug;
pub mod debug_string;
pub mod event;
pub mod exception;
pub mod info;
pub mod ipc;
pub mod light_ipc;
pub mod lock;
pub mod memory;
pub mod physical_memory;
pub mod port;
pub mod process;
pub mod processor;
pub mod query_memory;
pub mod resource_limit;
pub mod secure_monitor_call;
pub mod session;
pub mod shared_memory;
pub mod synchronization;
pub mod thread;
pub mod tick;
pub mod transfer_memory;

use log::{debug, warn};
use ruzu_common::ResultCode;

use crate::kernel::KernelCore;
use ruzu_cpu::CpuState;

/// SVC numbers (from Switch kernel).
pub mod svc_number {
    pub const SET_HEAP_SIZE: u32 = 0x01;
    pub const SET_MEMORY_PERMISSION: u32 = 0x02;
    pub const SET_MEMORY_ATTRIBUTE: u32 = 0x03;
    pub const MAP_MEMORY: u32 = 0x04;
    pub const UNMAP_MEMORY: u32 = 0x05;
    pub const QUERY_MEMORY: u32 = 0x06;
    pub const EXIT_PROCESS: u32 = 0x07;
    pub const CREATE_THREAD: u32 = 0x08;
    pub const START_THREAD: u32 = 0x09;
    pub const EXIT_THREAD: u32 = 0x0A;
    pub const SLEEP_THREAD: u32 = 0x0B;
    pub const GET_THREAD_PRIORITY: u32 = 0x0C;
    pub const SET_THREAD_PRIORITY: u32 = 0x0D;
    pub const SET_THREAD_CORE_MASK: u32 = 0x0E;
    pub const GET_THREAD_CORE_MASK: u32 = 0x0F;
    pub const GET_CURRENT_PROCESSOR_NUMBER: u32 = 0x10;
    pub const SIGNAL_EVENT: u32 = 0x11;
    pub const CLEAR_EVENT: u32 = 0x12;
    pub const MAP_SHARED_MEMORY: u32 = 0x13;
    pub const UNMAP_SHARED_MEMORY: u32 = 0x14;
    pub const CREATE_TRANSFER_MEMORY: u32 = 0x15;
    pub const CLOSE_HANDLE: u32 = 0x16;
    pub const RESET_SIGNAL: u32 = 0x17;
    pub const WAIT_SYNCHRONIZATION: u32 = 0x18;
    pub const CANCEL_SYNCHRONIZATION: u32 = 0x19;
    pub const ARBITRATE_LOCK: u32 = 0x1A;
    pub const ARBITRATE_UNLOCK: u32 = 0x1B;
    pub const WAIT_PROCESS_WIDE_KEY_ATOMIC: u32 = 0x1C;
    pub const SIGNAL_PROCESS_WIDE_KEY: u32 = 0x1D;
    pub const GET_SYSTEM_TICK: u32 = 0x1E;
    pub const CONNECT_TO_NAMED_PORT: u32 = 0x1F;
    pub const SEND_SYNC_REQUEST_LIGHT: u32 = 0x20;
    pub const SEND_SYNC_REQUEST: u32 = 0x21;
    pub const SEND_SYNC_REQUEST_WITH_USER_BUFFER: u32 = 0x22;
    pub const GET_PROCESS_INFO: u32 = 0x23;
    pub const GET_PROCESS_ID: u32 = 0x24;
    pub const GET_THREAD_ID: u32 = 0x25;
    pub const BREAK: u32 = 0x26;
    pub const OUTPUT_DEBUG_STRING: u32 = 0x27;
    pub const RETURN_FROM_EXCEPTION: u32 = 0x28;
    pub const GET_INFO: u32 = 0x29;
    pub const FLUSH_ENTIRE_DATA_CACHE: u32 = 0x2A;
    pub const FLUSH_DATA_CACHE: u32 = 0x2B;
    pub const MAP_PHYSICAL_MEMORY: u32 = 0x2C;
    pub const UNMAP_PHYSICAL_MEMORY: u32 = 0x2D;
    pub const GET_RESOURCE_LIMIT_LIMIT_VALUE: u32 = 0x30;
    pub const GET_RESOURCE_LIMIT_CURRENT_VALUE: u32 = 0x31;
    pub const SET_THREAD_ACTIVITY: u32 = 0x32;
    pub const GET_THREAD_CONTEXT3: u32 = 0x33;
    pub const WAIT_FOR_ADDRESS: u32 = 0x34;
    pub const SIGNAL_TO_ADDRESS: u32 = 0x35;
    pub const CREATE_SESSION: u32 = 0x40;
    pub const ACCEPT_SESSION: u32 = 0x41;
    pub const REPLY_AND_RECEIVE_LIGHT: u32 = 0x42;
    pub const REPLY_AND_RECEIVE: u32 = 0x43;
    pub const CREATE_EVENT: u32 = 0x45;
    pub const CREATE_CODE_MEMORY: u32 = 0x4B;
    pub const CONTROL_CODE_MEMORY: u32 = 0x4C;
    pub const CREATE_RESOURCE_LIMIT: u32 = 0x7D;
    pub const SET_RESOURCE_LIMIT_LIMIT_VALUE: u32 = 0x7E;
    pub const CALL_SECURE_MONITOR: u32 = 0x7F;
}

// ---------------------------------------------------------------------------
// 32-bit register-pair helpers (matching zuyu's Convert<int64_t> gather/scatter)
// ---------------------------------------------------------------------------

/// Gather two 32-bit register values into a 64-bit value (little-endian).
/// Matching zuyu's `Convert<int64_t>({GetArg32(lo), GetArg32(hi)})`.
#[inline]
fn gather64(cpu: &CpuState, lo_reg: usize, hi_reg: usize) -> u64 {
    (cpu.x[lo_reg] as u32 as u64) | ((cpu.x[hi_reg] as u32 as u64) << 32)
}

/// Scatter a 64-bit value into two 32-bit register slots.
/// Matching zuyu's `Convert<std::array<uint32_t,2>>(val)` + SetArg32.
#[inline]
fn scatter64(cpu: &mut CpuState, lo_reg: usize, hi_reg: usize, val: u64) {
    cpu.x[lo_reg] = val as u32 as u64;
    cpu.x[hi_reg] = (val >> 32) as u32 as u64;
}

/// Dispatch an SVC call. Reads arguments from CPU state, writes results back.
///
/// `is_64bit`: true for AArch64 processes, false for AArch32.
/// When false, 64-bit SVC arguments are gathered from register pairs matching
/// zuyu's `Call32()` wrapper layout.
pub fn dispatch_svc(kernel: &mut KernelCore, cpu: &mut CpuState, svc_num: u32, is_64bit: bool) {
    debug!(
        "SVC 0x{:02X} called (PC=0x{:016X}, {})",
        svc_num,
        cpu.pc,
        if is_64bit { "A64" } else { "A32" }
    );

    if is_64bit {
        dispatch_svc_64(kernel, cpu, svc_num);
    } else {
        dispatch_svc_32(kernel, cpu, svc_num);
    }
}

/// 64-bit SVC dispatch — arguments map 1:1 from x0-x7.
fn dispatch_svc_64(kernel: &mut KernelCore, cpu: &mut CpuState, svc_num: u32) {
    match svc_num {
        svc_number::SET_HEAP_SIZE => {
            let size = cpu.x[1];
            let result = memory::svc_set_heap_size(kernel, size);
            match result {
                Ok(addr) => {
                    cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
                    cpu.x[1] = addr;
                }
                Err(rc) => {
                    cpu.x[0] = rc.raw() as u64;
                    cpu.x[1] = 0;
                }
            }
        }

        svc_number::SET_MEMORY_PERMISSION => {
            let addr = cpu.x[0];
            let size = cpu.x[1];
            let perm = cpu.x[2] as u32;
            cpu.x[0] =
                memory::svc_set_memory_permission(kernel, addr, size, perm).raw() as u64;
        }

        svc_number::SET_MEMORY_ATTRIBUTE => {
            let addr = cpu.x[0];
            let size = cpu.x[1];
            let mask = cpu.x[2] as u32;
            let attr = cpu.x[3] as u32;
            cpu.x[0] =
                memory::svc_set_memory_attribute(kernel, addr, size, mask, attr).raw() as u64;
        }

        svc_number::MAP_MEMORY => {
            let dst = cpu.x[0];
            let src = cpu.x[1];
            let size = cpu.x[2];
            cpu.x[0] = memory::svc_map_memory(kernel, dst, src, size).raw() as u64;
        }

        svc_number::UNMAP_MEMORY => {
            let dst = cpu.x[0];
            let src = cpu.x[1];
            let size = cpu.x[2];
            cpu.x[0] = memory::svc_unmap_memory(kernel, dst, src, size).raw() as u64;
        }

        svc_number::QUERY_MEMORY => {
            let info_addr = cpu.x[0];
            let query_addr = cpu.x[2];
            let result = query_memory::svc_query_memory(kernel, cpu, info_addr, query_addr);
            cpu.x[0] = result.raw() as u64;
            cpu.x[1] = 0; // page_info
        }

        svc_number::EXIT_PROCESS => {
            process::svc_exit_process(kernel);
        }

        svc_number::CREATE_THREAD => {
            let entry = cpu.x[1];
            let arg = cpu.x[2];
            let stack_top = cpu.x[3];
            let priority = cpu.x[4] as u32;
            let core_id = cpu.x[5] as i32;
            let result =
                thread::svc_create_thread(kernel, entry, arg, stack_top, priority, core_id);
            match result {
                Ok(handle) => {
                    cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
                    cpu.x[1] = handle as u64;
                }
                Err(rc) => {
                    cpu.x[0] = rc.raw() as u64;
                    cpu.x[1] = 0;
                }
            }
        }

        svc_number::START_THREAD => {
            let handle = cpu.x[0] as u32;
            let result = thread::svc_start_thread(kernel, handle);
            cpu.x[0] = result.raw() as u64;
        }

        svc_number::EXIT_THREAD => {
            thread::svc_exit_thread(kernel);
            cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
        }

        svc_number::SLEEP_THREAD => {
            let ns = cpu.x[0] as i64;
            thread::svc_sleep_thread(kernel, cpu, ns);
        }

        svc_number::GET_THREAD_PRIORITY => {
            let handle = cpu.x[1] as u32;
            let result = thread::svc_get_thread_priority(kernel, handle);
            match result {
                Ok(priority) => {
                    cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
                    cpu.x[1] = priority as u64;
                }
                Err(rc) => {
                    cpu.x[0] = rc.raw() as u64;
                }
            }
        }

        svc_number::SET_THREAD_PRIORITY => {
            let handle = cpu.x[0] as u32;
            let priority = cpu.x[1] as u32;
            let result = thread::svc_set_thread_priority(kernel, handle, priority);
            cpu.x[0] = result.raw() as u64;
        }

        svc_number::SET_THREAD_CORE_MASK => {
            let handle = cpu.x[0] as u32;
            let ideal_core = cpu.x[1] as u32;
            let affinity_mask = cpu.x[2];
            cpu.x[0] =
                thread::svc_set_thread_core_mask(handle, ideal_core, affinity_mask).raw() as u64;
        }

        svc_number::GET_THREAD_CORE_MASK => {
            let handle = cpu.x[2] as u32;
            match thread::svc_get_thread_core_mask(handle) {
                Ok((ideal_core, affinity_mask)) => {
                    cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
                    cpu.x[1] = ideal_core as u64;
                    cpu.x[2] = affinity_mask;
                }
                Err(rc) => {
                    cpu.x[0] = rc.raw() as u64;
                }
            }
        }

        svc_number::GET_CURRENT_PROCESSOR_NUMBER => {
            cpu.x[0] = processor::svc_get_current_processor_number() as u64;
        }

        svc_number::SIGNAL_EVENT => {
            let handle = cpu.x[0] as u32;
            event::svc_signal_event(kernel, cpu, handle);
        }

        svc_number::CLEAR_EVENT => {
            let handle = cpu.x[0] as u32;
            event::svc_clear_event(kernel, cpu, handle);
        }

        svc_number::MAP_SHARED_MEMORY => {
            let handle = cpu.x[0] as u32;
            let addr = cpu.x[1];
            let size = cpu.x[2];
            let perm = cpu.x[3] as u32;
            let result = shared_memory::svc_map_shared_memory(kernel, handle, addr, size, perm);
            cpu.x[0] = result.raw() as u64;
        }

        svc_number::UNMAP_SHARED_MEMORY => {
            let handle = cpu.x[0] as u32;
            let addr = cpu.x[1];
            let size = cpu.x[2];
            let result = shared_memory::svc_unmap_shared_memory(kernel, handle, addr, size);
            cpu.x[0] = result.raw() as u64;
        }

        svc_number::CREATE_TRANSFER_MEMORY => {
            let addr = cpu.x[1];
            let size = cpu.x[2];
            let perm = cpu.x[3] as u32;
            match transfer_memory::svc_create_transfer_memory(kernel, addr, size, perm) {
                Ok(handle) => {
                    cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
                    cpu.x[1] = handle as u64;
                }
                Err(rc) => {
                    cpu.x[0] = rc.raw() as u64;
                    cpu.x[1] = 0;
                }
            }
        }

        svc_number::CLOSE_HANDLE => {
            let handle = cpu.x[0] as u32;
            let result = synchronization::svc_close_handle(kernel, handle);
            cpu.x[0] = result.raw() as u64;
        }

        svc_number::RESET_SIGNAL => {
            let handle = cpu.x[0] as u32;
            event::svc_clear_event(kernel, cpu, handle);
        }

        svc_number::WAIT_SYNCHRONIZATION => {
            let handles_ptr = cpu.x[1];
            let handles_count = cpu.x[2] as usize;
            let timeout_ns = cpu.x[3] as i64;
            synchronization::svc_wait_synchronization(
                kernel,
                cpu,
                handles_ptr,
                handles_count,
                timeout_ns,
            );
        }

        svc_number::CANCEL_SYNCHRONIZATION => {
            let thread_handle = cpu.x[0] as u32;
            synchronization::svc_cancel_synchronization(kernel, cpu, thread_handle);
        }

        svc_number::ARBITRATE_LOCK => {
            let owner_handle = cpu.x[0] as u32;
            let mutex_addr = cpu.x[1];
            let requester_handle = cpu.x[2] as u32;
            lock::svc_arbitrate_lock(kernel, cpu, owner_handle, mutex_addr, requester_handle);
        }

        svc_number::ARBITRATE_UNLOCK => {
            let mutex_addr = cpu.x[0];
            lock::svc_arbitrate_unlock(kernel, cpu, mutex_addr);
        }

        svc_number::WAIT_PROCESS_WIDE_KEY_ATOMIC => {
            let mutex_addr = cpu.x[0];
            let condvar_addr = cpu.x[1];
            let tag = cpu.x[2] as u32;
            let timeout_ns = cpu.x[3] as i64;
            condition_variable::svc_wait_process_wide_key_atomic(
                kernel,
                cpu,
                mutex_addr,
                condvar_addr,
                tag,
                timeout_ns,
            );
        }

        svc_number::SIGNAL_PROCESS_WIDE_KEY => {
            let condvar_addr = cpu.x[0];
            let count = cpu.x[1] as i32;
            condition_variable::svc_signal_process_wide_key(kernel, cpu, condvar_addr, count);
        }

        svc_number::GET_SYSTEM_TICK => {
            let ticks = kernel.tick_counter;
            cpu.x[0] = ticks;
            cpu.x[1] = ticks;
        }

        svc_number::CONNECT_TO_NAMED_PORT => {
            let name_addr = cpu.x[1];
            let result = port::svc_connect_to_named_port(kernel, cpu, name_addr);
            match result {
                Ok(handle) => {
                    cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
                    cpu.x[1] = handle as u64;
                }
                Err(rc) => {
                    cpu.x[0] = rc.raw() as u64;
                    cpu.x[1] = 0;
                }
            }
        }

        svc_number::SEND_SYNC_REQUEST_LIGHT => {
            cpu.x[0] = light_ipc::svc_send_sync_request_light().raw() as u64;
        }

        svc_number::SEND_SYNC_REQUEST => {
            let handle = cpu.x[0] as u32;
            let result = ipc::svc_send_sync_request(kernel, cpu, handle);
            cpu.x[0] = result.raw() as u64;
        }

        svc_number::SEND_SYNC_REQUEST_WITH_USER_BUFFER => {
            let buffer_addr = cpu.x[0];
            let buffer_size = cpu.x[1];
            let session_handle = cpu.x[2] as u32;
            let result = ipc::svc_send_sync_request_with_user_buffer(
                kernel,
                cpu,
                buffer_addr,
                buffer_size,
                session_handle,
            );
            cpu.x[0] = result.raw() as u64;
        }

        svc_number::GET_PROCESS_INFO => {
            let _handle = cpu.x[0] as u32;
            let info_type = cpu.x[1] as u32;
            let value = process::svc_get_process_info(info_type);
            cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
            cpu.x[1] = value;
        }

        svc_number::GET_PROCESS_ID => {
            let result = process::svc_get_process_id(kernel);
            match result {
                Ok(pid) => {
                    cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
                    cpu.x[1] = pid;
                }
                Err(rc) => {
                    cpu.x[0] = rc.raw() as u64;
                }
            }
        }

        svc_number::GET_THREAD_ID => {
            let result = thread::svc_get_thread_id(kernel);
            match result {
                Ok(tid) => {
                    cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
                    cpu.x[1] = tid;
                }
                Err(rc) => {
                    cpu.x[0] = rc.raw() as u64;
                }
            }
        }

        svc_number::BREAK => {
            let reason = cpu.x[0];
            let info1 = cpu.x[1];
            let info2 = cpu.x[2];
            debug::svc_break(kernel, reason, info1, info2);
        }

        svc_number::OUTPUT_DEBUG_STRING => {
            let addr = cpu.x[0];
            let len = cpu.x[1] as usize;
            let result = debug_string::svc_output_debug_string(kernel, addr, len);
            cpu.x[0] = result.raw() as u64;
        }

        svc_number::RETURN_FROM_EXCEPTION => {
            let result_val = cpu.x[0];
            cpu.x[0] = exception::svc_return_from_exception(result_val).raw() as u64;
        }

        svc_number::GET_INFO => {
            let info_id = cpu.x[1] as u32;
            let handle = cpu.x[2] as u32;
            let info_sub_id = cpu.x[3] as u64;
            let result = info::svc_get_info(kernel, info_id, handle, info_sub_id);
            match result {
                Ok(value) => {
                    cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
                    cpu.x[1] = value;
                }
                Err(rc) => {
                    cpu.x[0] = rc.raw() as u64;
                    cpu.x[1] = 0;
                }
            }
        }

        svc_number::FLUSH_ENTIRE_DATA_CACHE => {
            cpu.x[0] = cache::svc_flush_entire_data_cache().raw() as u64;
        }

        svc_number::FLUSH_DATA_CACHE => {
            let addr = cpu.x[0];
            let size = cpu.x[1];
            cpu.x[0] = cache::svc_flush_data_cache(addr, size).raw() as u64;
        }

        svc_number::MAP_PHYSICAL_MEMORY => {
            let addr = cpu.x[0];
            let size = cpu.x[1];
            let result = physical_memory::svc_map_physical_memory(kernel, addr, size);
            cpu.x[0] = result.raw() as u64;
        }

        svc_number::UNMAP_PHYSICAL_MEMORY => {
            let addr = cpu.x[0];
            let size = cpu.x[1];
            let result = physical_memory::svc_unmap_physical_memory(kernel, addr, size);
            cpu.x[0] = result.raw() as u64;
        }

        svc_number::GET_RESOURCE_LIMIT_LIMIT_VALUE => {
            let handle = cpu.x[1] as u32;
            let resource_type = cpu.x[2] as u32;
            match resource_limit::svc_get_resource_limit_limit_value(handle, resource_type) {
                Ok(value) => {
                    cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
                    cpu.x[1] = value;
                }
                Err(rc) => {
                    cpu.x[0] = rc.raw() as u64;
                }
            }
        }

        svc_number::GET_RESOURCE_LIMIT_CURRENT_VALUE => {
            let handle = cpu.x[1] as u32;
            let resource_type = cpu.x[2] as u32;
            match resource_limit::svc_get_resource_limit_current_value(handle, resource_type) {
                Ok(value) => {
                    cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
                    cpu.x[1] = value;
                }
                Err(rc) => {
                    cpu.x[0] = rc.raw() as u64;
                }
            }
        }

        svc_number::SET_THREAD_ACTIVITY => {
            let thread_handle = cpu.x[0] as u32;
            let act = cpu.x[1] as u32;
            let result = activity::svc_set_thread_activity(kernel, thread_handle, act);
            cpu.x[0] = result.raw() as u64;
        }

        svc_number::GET_THREAD_CONTEXT3 => {
            let thread_handle = cpu.x[0] as u32;
            let context_addr = cpu.x[1];
            let result = debug::svc_get_thread_context3(kernel, thread_handle, context_addr);
            cpu.x[0] = result.raw() as u64;
        }

        svc_number::WAIT_FOR_ADDRESS => {
            let addr = cpu.x[0];
            let arb_type = cpu.x[1] as u32;
            let value = cpu.x[2] as u32;
            let timeout_ns = cpu.x[3] as i64;
            address_arbiter::svc_wait_for_address(kernel, cpu, addr, arb_type, value, timeout_ns);
        }

        svc_number::SIGNAL_TO_ADDRESS => {
            let addr = cpu.x[0];
            let signal_type = cpu.x[1] as u32;
            let value = cpu.x[2] as u32;
            let count = cpu.x[3] as i32;
            address_arbiter::svc_signal_to_address(kernel, cpu, addr, signal_type, value, count);
        }

        svc_number::CREATE_EVENT => {
            event::svc_create_event(kernel, cpu);
        }

        svc_number::CALL_SECURE_MONITOR => {
            secure_monitor_call::svc_call_secure_monitor(cpu);
        }

        _ => {
            warn!(
                "Unimplemented SVC 0x{:02X} (PC=0x{:016X})",
                svc_num, cpu.pc
            );
            cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
        }
    }
}

/// 32-bit SVC dispatch — arguments use ARM32 register layout.
///
/// Matching zuyu's `Call32()` in `svc.cpp`. Key differences from 64-bit:
/// - 64-bit arguments are gathered from register pairs (e.g., r0:r1 = lo:hi)
/// - Register assignments may differ (e.g., CreateThread: priority in r0, not r4)
/// - 64-bit results are scattered into register pairs
fn dispatch_svc_32(kernel: &mut KernelCore, cpu: &mut CpuState, svc_num: u32) {
    match svc_num {
        // ---- SVCs with IDENTICAL register layout in 32-bit and 64-bit ----

        svc_number::SET_MEMORY_PERMISSION => {
            let addr = cpu.x[0];
            let size = cpu.x[1];
            let perm = cpu.x[2] as u32;
            cpu.x[0] =
                memory::svc_set_memory_permission(kernel, addr, size, perm).raw() as u64;
        }

        svc_number::SET_MEMORY_ATTRIBUTE => {
            let addr = cpu.x[0];
            let size = cpu.x[1];
            let mask = cpu.x[2] as u32;
            let attr = cpu.x[3] as u32;
            cpu.x[0] =
                memory::svc_set_memory_attribute(kernel, addr, size, mask, attr).raw() as u64;
        }

        svc_number::MAP_MEMORY => {
            let dst = cpu.x[0];
            let src = cpu.x[1];
            let size = cpu.x[2];
            cpu.x[0] = memory::svc_map_memory(kernel, dst, src, size).raw() as u64;
        }

        svc_number::UNMAP_MEMORY => {
            let dst = cpu.x[0];
            let src = cpu.x[1];
            let size = cpu.x[2];
            cpu.x[0] = memory::svc_unmap_memory(kernel, dst, src, size).raw() as u64;
        }

        svc_number::EXIT_PROCESS => {
            process::svc_exit_process(kernel);
        }

        svc_number::START_THREAD => {
            let handle = cpu.x[0] as u32;
            let result = thread::svc_start_thread(kernel, handle);
            cpu.x[0] = result.raw() as u64;
        }

        svc_number::EXIT_THREAD => {
            thread::svc_exit_thread(kernel);
            cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
        }

        svc_number::SET_THREAD_PRIORITY => {
            let handle = cpu.x[0] as u32;
            let priority = cpu.x[1] as u32;
            let result = thread::svc_set_thread_priority(kernel, handle, priority);
            cpu.x[0] = result.raw() as u64;
        }

        svc_number::GET_CURRENT_PROCESSOR_NUMBER => {
            cpu.x[0] = processor::svc_get_current_processor_number() as u64;
        }

        svc_number::SIGNAL_EVENT => {
            let handle = cpu.x[0] as u32;
            event::svc_signal_event(kernel, cpu, handle);
        }

        svc_number::CLEAR_EVENT => {
            let handle = cpu.x[0] as u32;
            event::svc_clear_event(kernel, cpu, handle);
        }

        svc_number::RESET_SIGNAL => {
            let handle = cpu.x[0] as u32;
            event::svc_clear_event(kernel, cpu, handle);
        }

        svc_number::CLOSE_HANDLE => {
            let handle = cpu.x[0] as u32;
            let result = synchronization::svc_close_handle(kernel, handle);
            cpu.x[0] = result.raw() as u64;
        }

        svc_number::CANCEL_SYNCHRONIZATION => {
            let thread_handle = cpu.x[0] as u32;
            synchronization::svc_cancel_synchronization(kernel, cpu, thread_handle);
        }

        svc_number::ARBITRATE_LOCK => {
            let owner_handle = cpu.x[0] as u32;
            let mutex_addr = cpu.x[1];
            let requester_handle = cpu.x[2] as u32;
            lock::svc_arbitrate_lock(kernel, cpu, owner_handle, mutex_addr, requester_handle);
        }

        svc_number::ARBITRATE_UNLOCK => {
            let mutex_addr = cpu.x[0];
            lock::svc_arbitrate_unlock(kernel, cpu, mutex_addr);
        }

        svc_number::SIGNAL_PROCESS_WIDE_KEY => {
            let condvar_addr = cpu.x[0];
            let count = cpu.x[1] as i32;
            condition_variable::svc_signal_process_wide_key(kernel, cpu, condvar_addr, count);
        }

        svc_number::SEND_SYNC_REQUEST => {
            let handle = cpu.x[0] as u32;
            let result = ipc::svc_send_sync_request(kernel, cpu, handle);
            cpu.x[0] = result.raw() as u64;
        }

        svc_number::OUTPUT_DEBUG_STRING => {
            let addr = cpu.x[0];
            let len = cpu.x[1] as usize;
            let result = debug_string::svc_output_debug_string(kernel, addr, len);
            cpu.x[0] = result.raw() as u64;
        }

        svc_number::BREAK => {
            let reason = cpu.x[0];
            let info1 = cpu.x[1];
            let info2 = cpu.x[2];
            debug::svc_break(kernel, reason, info1, info2);
        }

        svc_number::MAP_PHYSICAL_MEMORY => {
            let addr = cpu.x[0];
            let size = cpu.x[1];
            let result = physical_memory::svc_map_physical_memory(kernel, addr, size);
            cpu.x[0] = result.raw() as u64;
        }

        svc_number::UNMAP_PHYSICAL_MEMORY => {
            let addr = cpu.x[0];
            let size = cpu.x[1];
            let result = physical_memory::svc_unmap_physical_memory(kernel, addr, size);
            cpu.x[0] = result.raw() as u64;
        }

        svc_number::SET_THREAD_ACTIVITY => {
            let thread_handle = cpu.x[0] as u32;
            let act = cpu.x[1] as u32;
            let result = activity::svc_set_thread_activity(kernel, thread_handle, act);
            cpu.x[0] = result.raw() as u64;
        }

        svc_number::GET_THREAD_CONTEXT3 => {
            let thread_handle = cpu.x[0] as u32;
            let context_addr = cpu.x[1];
            let result = debug::svc_get_thread_context3(kernel, thread_handle, context_addr);
            cpu.x[0] = result.raw() as u64;
        }

        svc_number::FLUSH_ENTIRE_DATA_CACHE => {
            cpu.x[0] = cache::svc_flush_entire_data_cache().raw() as u64;
        }

        svc_number::FLUSH_DATA_CACHE => {
            let addr = cpu.x[0];
            let size = cpu.x[1];
            cpu.x[0] = cache::svc_flush_data_cache(addr, size).raw() as u64;
        }

        svc_number::RETURN_FROM_EXCEPTION => {
            let result_val = cpu.x[0];
            cpu.x[0] = exception::svc_return_from_exception(result_val).raw() as u64;
        }

        svc_number::SEND_SYNC_REQUEST_LIGHT => {
            cpu.x[0] = light_ipc::svc_send_sync_request_light().raw() as u64;
        }

        svc_number::CALL_SECURE_MONITOR => {
            secure_monitor_call::svc_call_secure_monitor(cpu);
        }

        // ---- SVCs with DIFFERENT register layout in 32-bit ----

        svc_number::SET_HEAP_SIZE => {
            // 32-bit: r1=size (u32), out: r0=Result, r1=out_address (u32)
            let size = cpu.x[1];
            let result = memory::svc_set_heap_size(kernel, size);
            match result {
                Ok(addr) => {
                    cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
                    cpu.x[1] = addr as u32 as u64; // truncate to 32-bit
                }
                Err(rc) => {
                    cpu.x[0] = rc.raw() as u64;
                    cpu.x[1] = 0;
                }
            }
        }

        svc_number::QUERY_MEMORY => {
            // 32-bit: r0=out_memory_info, r2=address, out: r0=Result, r1=page_info
            let info_addr = cpu.x[0];
            let query_addr = cpu.x[2];
            let result = query_memory::svc_query_memory(kernel, cpu, info_addr, query_addr);
            cpu.x[0] = result.raw() as u64;
            cpu.x[1] = 0; // page_info
        }

        svc_number::MAP_SHARED_MEMORY => {
            let handle = cpu.x[0] as u32;
            let addr = cpu.x[1];
            let size = cpu.x[2];
            let perm = cpu.x[3] as u32;
            let result = shared_memory::svc_map_shared_memory(kernel, handle, addr, size, perm);
            cpu.x[0] = result.raw() as u64;
        }

        svc_number::UNMAP_SHARED_MEMORY => {
            let handle = cpu.x[0] as u32;
            let addr = cpu.x[1];
            let size = cpu.x[2];
            let result = shared_memory::svc_unmap_shared_memory(kernel, handle, addr, size);
            cpu.x[0] = result.raw() as u64;
        }

        svc_number::CREATE_THREAD => {
            // 32-bit layout (zuyu Call32): r1=func, r2=arg, r3=stack_bottom,
            //   r0=priority, r4=core_id
            let entry = cpu.x[1];
            let arg = cpu.x[2];
            let stack_top = cpu.x[3];
            let priority = cpu.x[0] as u32; // r0, not r4
            let core_id = cpu.x[4] as i32;
            let result =
                thread::svc_create_thread(kernel, entry, arg, stack_top, priority, core_id);
            match result {
                Ok(handle) => {
                    cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
                    cpu.x[1] = handle as u64;
                }
                Err(rc) => {
                    cpu.x[0] = rc.raw() as u64;
                    cpu.x[1] = 0;
                }
            }
        }

        svc_number::SLEEP_THREAD => {
            // 32-bit: gather r0(lo):r1(hi) → ns (i64)
            let ns = gather64(cpu, 0, 1) as i64;
            thread::svc_sleep_thread(kernel, cpu, ns);
        }

        svc_number::GET_THREAD_PRIORITY => {
            let handle = cpu.x[1] as u32;
            let result = thread::svc_get_thread_priority(kernel, handle);
            match result {
                Ok(priority) => {
                    cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
                    cpu.x[1] = priority as u64;
                }
                Err(rc) => {
                    cpu.x[0] = rc.raw() as u64;
                }
            }
        }

        svc_number::GET_THREAD_CORE_MASK => {
            // 32-bit: r2=handle, out: r0=Result, r1=core_id, scatter r2(lo):r3(hi)=affinity
            let handle = cpu.x[2] as u32;
            match thread::svc_get_thread_core_mask(handle) {
                Ok((ideal_core, affinity_mask)) => {
                    cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
                    cpu.x[1] = ideal_core as u64;
                    scatter64(cpu, 2, 3, affinity_mask);
                }
                Err(rc) => {
                    cpu.x[0] = rc.raw() as u64;
                }
            }
        }

        svc_number::SET_THREAD_CORE_MASK => {
            // 32-bit: r0=handle, r1=core_id, gather r2(lo):r3(hi)=affinity_mask
            let handle = cpu.x[0] as u32;
            let ideal_core = cpu.x[1] as u32;
            let affinity_mask = gather64(cpu, 2, 3);
            cpu.x[0] =
                thread::svc_set_thread_core_mask(handle, ideal_core, affinity_mask).raw() as u64;
        }

        svc_number::CREATE_TRANSFER_MEMORY => {
            let addr = cpu.x[1];
            let size = cpu.x[2];
            let perm = cpu.x[3] as u32;
            match transfer_memory::svc_create_transfer_memory(kernel, addr, size, perm) {
                Ok(handle) => {
                    cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
                    cpu.x[1] = handle as u64;
                }
                Err(rc) => {
                    cpu.x[0] = rc.raw() as u64;
                    cpu.x[1] = 0;
                }
            }
        }

        svc_number::CREATE_EVENT => {
            event::svc_create_event(kernel, cpu);
        }

        svc_number::WAIT_SYNCHRONIZATION => {
            // 32-bit: r1=handles_ptr, r2=num_handles, gather r0(lo):r3(hi)=timeout_ns
            let handles_ptr = cpu.x[1];
            let handles_count = cpu.x[2] as usize;
            let timeout_ns = gather64(cpu, 0, 3) as i64;
            synchronization::svc_wait_synchronization(
                kernel,
                cpu,
                handles_ptr,
                handles_count,
                timeout_ns,
            );
        }

        svc_number::WAIT_PROCESS_WIDE_KEY_ATOMIC => {
            // 32-bit: r0=address, r1=cv_key, r2=tag, gather r3(lo):r4(hi)=timeout_ns
            let mutex_addr = cpu.x[0];
            let condvar_addr = cpu.x[1];
            let tag = cpu.x[2] as u32;
            let timeout_ns = gather64(cpu, 3, 4) as i64;
            condition_variable::svc_wait_process_wide_key_atomic(
                kernel,
                cpu,
                mutex_addr,
                condvar_addr,
                tag,
                timeout_ns,
            );
        }

        svc_number::GET_SYSTEM_TICK => {
            // 32-bit: out: scatter r0(lo):r1(hi)=tick (i64)
            let ticks = kernel.tick_counter;
            scatter64(cpu, 0, 1, ticks);
        }

        svc_number::CONNECT_TO_NAMED_PORT => {
            let name_addr = cpu.x[1];
            let result = port::svc_connect_to_named_port(kernel, cpu, name_addr);
            match result {
                Ok(handle) => {
                    cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
                    cpu.x[1] = handle as u64;
                }
                Err(rc) => {
                    cpu.x[0] = rc.raw() as u64;
                    cpu.x[1] = 0;
                }
            }
        }

        svc_number::SEND_SYNC_REQUEST_WITH_USER_BUFFER => {
            let buffer_addr = cpu.x[0];
            let buffer_size = cpu.x[1];
            let session_handle = cpu.x[2] as u32;
            let result = ipc::svc_send_sync_request_with_user_buffer(
                kernel,
                cpu,
                buffer_addr,
                buffer_size,
                session_handle,
            );
            cpu.x[0] = result.raw() as u64;
        }

        svc_number::GET_PROCESS_INFO => {
            // 32-bit: r1=process_handle, r2=info_type
            // out: r0=Result, scatter r1(lo):r2(hi)=info (i64)
            let info_type = cpu.x[2] as u32;
            let value = process::svc_get_process_info(info_type);
            cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
            scatter64(cpu, 1, 2, value);
        }

        svc_number::GET_PROCESS_ID => {
            // 32-bit: r1=process_handle, out: r0=Result, scatter r1(lo):r2(hi)=pid
            let result = process::svc_get_process_id(kernel);
            match result {
                Ok(pid) => {
                    cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
                    scatter64(cpu, 1, 2, pid);
                }
                Err(rc) => {
                    cpu.x[0] = rc.raw() as u64;
                }
            }
        }

        svc_number::GET_THREAD_ID => {
            // 32-bit: r1=thread_handle, out: r0=Result, scatter r1(lo):r2(hi)=tid
            let result = thread::svc_get_thread_id(kernel);
            match result {
                Ok(tid) => {
                    cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
                    scatter64(cpu, 1, 2, tid);
                }
                Err(rc) => {
                    cpu.x[0] = rc.raw() as u64;
                }
            }
        }

        svc_number::GET_INFO => {
            // 32-bit: r1=info_type, r2=handle, gather r0(lo):r3(hi)=info_subtype
            // out: r0=Result, scatter r1(lo):r2(hi)=out (u64)
            let info_id = cpu.x[1] as u32;
            let handle = cpu.x[2] as u32;
            let info_sub_id = gather64(cpu, 0, 3);
            let result = info::svc_get_info(kernel, info_id, handle, info_sub_id);
            match result {
                Ok(value) => {
                    cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
                    scatter64(cpu, 1, 2, value);
                }
                Err(rc) => {
                    cpu.x[0] = rc.raw() as u64;
                    scatter64(cpu, 1, 2, 0);
                }
            }
        }

        svc_number::WAIT_FOR_ADDRESS => {
            // 32-bit: r0=address, r1=arb_type, r2=value, gather r3(lo):r4(hi)=timeout_ns
            let addr = cpu.x[0];
            let arb_type = cpu.x[1] as u32;
            let value = cpu.x[2] as u32;
            let timeout_ns = gather64(cpu, 3, 4) as i64;
            address_arbiter::svc_wait_for_address(kernel, cpu, addr, arb_type, value, timeout_ns);
        }

        svc_number::SIGNAL_TO_ADDRESS => {
            let addr = cpu.x[0];
            let signal_type = cpu.x[1] as u32;
            let value = cpu.x[2] as u32;
            let count = cpu.x[3] as i32;
            address_arbiter::svc_signal_to_address(kernel, cpu, addr, signal_type, value, count);
        }

        svc_number::GET_RESOURCE_LIMIT_LIMIT_VALUE => {
            let handle = cpu.x[1] as u32;
            let resource_type = cpu.x[2] as u32;
            match resource_limit::svc_get_resource_limit_limit_value(handle, resource_type) {
                Ok(value) => {
                    cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
                    scatter64(cpu, 1, 2, value);
                }
                Err(rc) => {
                    cpu.x[0] = rc.raw() as u64;
                }
            }
        }

        svc_number::GET_RESOURCE_LIMIT_CURRENT_VALUE => {
            let handle = cpu.x[1] as u32;
            let resource_type = cpu.x[2] as u32;
            match resource_limit::svc_get_resource_limit_current_value(handle, resource_type) {
                Ok(value) => {
                    cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
                    scatter64(cpu, 1, 2, value);
                }
                Err(rc) => {
                    cpu.x[0] = rc.raw() as u64;
                }
            }
        }

        _ => {
            warn!(
                "Unimplemented SVC 0x{:02X} (PC=0x{:08X}, A32)",
                svc_num, cpu.pc as u32
            );
            cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
        }
    }
}
