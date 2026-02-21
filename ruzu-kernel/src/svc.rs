// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

pub mod debug;
pub mod info;
pub mod ipc;
pub mod memory;
pub mod thread;

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
    pub const SEND_SYNC_REQUEST: u32 = 0x21;
    pub const SEND_SYNC_REQUEST_WITH_USER_BUFFER: u32 = 0x22;
    pub const GET_PROCESS_ID: u32 = 0x24;
    pub const GET_THREAD_ID: u32 = 0x25;
    pub const BREAK: u32 = 0x26;
    pub const OUTPUT_DEBUG_STRING: u32 = 0x27;
    pub const RETURN_FROM_EXCEPTION: u32 = 0x28;
    pub const GET_INFO: u32 = 0x29;
    pub const MAP_PHYSICAL_MEMORY: u32 = 0x2C;
    pub const UNMAP_PHYSICAL_MEMORY: u32 = 0x2D;
    pub const SET_THREAD_ACTIVITY: u32 = 0x32;
    pub const GET_THREAD_CONTEXT3: u32 = 0x33;
    pub const WAIT_FOR_ADDRESS: u32 = 0x34;
    pub const SIGNAL_TO_ADDRESS: u32 = 0x35;
    pub const CREATE_SESSION: u32 = 0x40;
    pub const ACCEPT_SESSION: u32 = 0x41;
    pub const REPLY_AND_RECEIVE: u32 = 0x43;
    pub const CREATE_EVENT: u32 = 0x45;
}

/// Dispatch an SVC call. Reads arguments from CPU state, writes results back.
pub fn dispatch_svc(kernel: &mut KernelCore, cpu: &mut CpuState, svc_num: u32) {
    debug!("SVC 0x{:02X} called (PC=0x{:016X})", svc_num, cpu.pc);

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

        svc_number::QUERY_MEMORY => {
            let info_addr = cpu.x[0];
            let query_addr = cpu.x[2];
            let result = memory::svc_query_memory(kernel, cpu, info_addr, query_addr);
            cpu.x[0] = result.raw() as u64;
            cpu.x[1] = 0; // page_info
        }

        svc_number::MAP_SHARED_MEMORY => {
            let handle = cpu.x[0] as u32;
            let addr = cpu.x[1];
            let size = cpu.x[2];
            let perm = cpu.x[3] as u32;
            let result = memory::svc_map_shared_memory(kernel, handle, addr, size, perm);
            cpu.x[0] = result.raw() as u64;
        }

        svc_number::EXIT_PROCESS => {
            debug::svc_exit_process(kernel);
        }

        svc_number::CREATE_THREAD => {
            let entry = cpu.x[1];
            let arg = cpu.x[2];
            let stack_top = cpu.x[3];
            let priority = cpu.x[4] as u32;
            let core_id = cpu.x[5] as i32;
            let result = thread::svc_create_thread(kernel, entry, arg, stack_top, priority, core_id);
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
            thread::svc_sleep_thread(ns);
            cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
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

        svc_number::GET_CURRENT_PROCESSOR_NUMBER => {
            cpu.x[0] = 0; // Always core 0 in Phase 1
        }

        svc_number::CLOSE_HANDLE => {
            let handle = cpu.x[0] as u32;
            let result = ipc::svc_close_handle(kernel, handle);
            cpu.x[0] = result.raw() as u64;
        }

        svc_number::WAIT_SYNCHRONIZATION => {
            // Stub: return timeout
            cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
            cpu.x[1] = 0;
        }

        svc_number::ARBITRATE_LOCK => {
            // Stub: return success (single-threaded Phase 1)
            cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
        }

        svc_number::ARBITRATE_UNLOCK => {
            // Stub: return success
            cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
        }

        svc_number::WAIT_PROCESS_WIDE_KEY_ATOMIC => {
            // Stub: return success
            cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
        }

        svc_number::SIGNAL_PROCESS_WIDE_KEY => {
            // Stub: return success
            cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
        }

        svc_number::GET_SYSTEM_TICK => {
            let ticks = info::svc_get_system_tick();
            cpu.x[0] = ticks;
            cpu.x[1] = ticks;
        }

        svc_number::CONNECT_TO_NAMED_PORT => {
            let name_addr = cpu.x[1];
            let result = ipc::svc_connect_to_named_port(kernel, cpu, name_addr);
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

        svc_number::SEND_SYNC_REQUEST => {
            let handle = cpu.x[0] as u32;
            let result = ipc::svc_send_sync_request(kernel, cpu, handle);
            cpu.x[0] = result.raw() as u64;
        }

        svc_number::GET_PROCESS_ID => {
            let result = info::svc_get_process_id(kernel);
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
            let result = info::svc_get_thread_id(kernel);
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
            let result = debug::svc_output_debug_string(kernel, addr, len);
            cpu.x[0] = result.raw() as u64;
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

        svc_number::MAP_PHYSICAL_MEMORY => {
            // Stub: treat like heap allocation
            let addr = cpu.x[0];
            let size = cpu.x[1];
            debug!("MapPhysicalMemory: addr=0x{:X}, size=0x{:X}", addr, size);
            cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
        }

        svc_number::SIGNAL_EVENT | svc_number::CLEAR_EVENT | svc_number::RESET_SIGNAL => {
            // Stub: return success
            cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
        }

        svc_number::CREATE_EVENT => {
            // Stub: return two dummy handles
            cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
            cpu.x[1] = 0xDEAD0001;
            cpu.x[2] = 0xDEAD0002;
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
