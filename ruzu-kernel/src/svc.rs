// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

pub mod debug;
pub mod info;
pub mod ipc;
pub mod memory;
pub mod thread;

use log::{debug, warn};
use ruzu_common::{error, Handle, ResultCode, VAddr};

use crate::kernel::KernelCore;
use crate::objects::{KEvent, KernelObject};
use crate::thread::{ThreadState, WaitReason};
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
///
/// `thread_idx` is the index of the calling thread in the process's `threads` vec.
/// For SVCs that block the calling thread, the thread state is modified directly
/// on `kernel.process.threads[thread_idx]` rather than through the cloned `cpu`.
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

        svc_number::SET_MEMORY_PERMISSION => {
            let addr = cpu.x[0];
            let size = cpu.x[1];
            let perm = cpu.x[2] as u32;
            cpu.x[0] = memory::svc_set_memory_permission(kernel, addr, size, perm).raw() as u64;
        }

        svc_number::SET_MEMORY_ATTRIBUTE => {
            debug!("SetMemoryAttribute: addr=0x{:X}, size=0x{:X}", cpu.x[0], cpu.x[1]);
            cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
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
            svc_sleep_thread(kernel, cpu, ns);
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
            cpu.x[0] = 0; // Always core 0
        }

        svc_number::CREATE_TRANSFER_MEMORY => {
            let addr = cpu.x[1];
            let size = cpu.x[2];
            let perm = cpu.x[3] as u32;
            match memory::svc_create_transfer_memory(kernel, addr, size, perm) {
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
            let result = ipc::svc_close_handle(kernel, handle);
            cpu.x[0] = result.raw() as u64;
        }

        svc_number::CREATE_EVENT => {
            svc_create_event(kernel, cpu);
        }

        svc_number::SIGNAL_EVENT => {
            let handle = cpu.x[0] as u32;
            svc_signal_event(kernel, cpu, handle);
        }

        svc_number::CLEAR_EVENT => {
            let handle = cpu.x[0] as u32;
            svc_clear_event(kernel, cpu, handle);
        }

        svc_number::RESET_SIGNAL => {
            let handle = cpu.x[0] as u32;
            svc_clear_event(kernel, cpu, handle);
        }

        svc_number::WAIT_SYNCHRONIZATION => {
            let handles_ptr = cpu.x[1];
            let handles_count = cpu.x[2] as usize;
            let timeout_ns = cpu.x[3] as i64;
            svc_wait_synchronization(kernel, cpu, handles_ptr, handles_count, timeout_ns);
        }

        svc_number::ARBITRATE_LOCK => {
            let owner_handle = cpu.x[0] as u32;
            let mutex_addr = cpu.x[1];
            let requester_handle = cpu.x[2] as u32;
            svc_arbitrate_lock(kernel, cpu, owner_handle, mutex_addr, requester_handle);
        }

        svc_number::ARBITRATE_UNLOCK => {
            let mutex_addr = cpu.x[0];
            svc_arbitrate_unlock(kernel, cpu, mutex_addr);
        }

        svc_number::WAIT_PROCESS_WIDE_KEY_ATOMIC => {
            let mutex_addr = cpu.x[0];
            let condvar_addr = cpu.x[1];
            let tag = cpu.x[2] as u32;
            let timeout_ns = cpu.x[3] as i64;
            svc_wait_process_wide_key_atomic(
                kernel, cpu, mutex_addr, condvar_addr, tag, timeout_ns,
            );
        }

        svc_number::SIGNAL_PROCESS_WIDE_KEY => {
            let condvar_addr = cpu.x[0];
            let count = cpu.x[1] as i32;
            svc_signal_process_wide_key(kernel, cpu, condvar_addr, count);
        }

        svc_number::GET_SYSTEM_TICK => {
            let ticks = kernel.tick_counter;
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

        svc_number::WAIT_FOR_ADDRESS => {
            let addr = cpu.x[0];
            let arb_type = cpu.x[1] as u32;
            let value = cpu.x[2] as u32;
            let timeout_ns = cpu.x[3] as i64;
            svc_wait_for_address(kernel, cpu, addr, arb_type, value, timeout_ns);
        }

        svc_number::SIGNAL_TO_ADDRESS => {
            let addr = cpu.x[0];
            let signal_type = cpu.x[1] as u32;
            let value = cpu.x[2] as u32;
            let count = cpu.x[3] as i32;
            svc_signal_to_address(kernel, cpu, addr, signal_type, value, count);
        }

        svc_number::MAP_PHYSICAL_MEMORY => {
            let addr = cpu.x[0];
            let size = cpu.x[1];
            debug!("MapPhysicalMemory: addr=0x{:X}, size=0x{:X}", addr, size);
            cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
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

// ---------------------------------------------------------------------------
// SVC 0x45: CreateEvent
// ---------------------------------------------------------------------------

fn svc_create_event(kernel: &mut KernelCore, cpu: &mut CpuState) {
    debug!("CreateEvent");

    let process = match kernel.process_mut() {
        Some(p) => p,
        None => {
            cpu.x[0] = error::INVALID_STATE.raw() as u64;
            return;
        }
    };

    // Create the event and add writable + readable handles.
    let event = KEvent::new();
    let writable_handle = match process.handle_table.add(KernelObject::Event(event.clone())) {
        Ok(h) => h,
        Err(rc) => {
            cpu.x[0] = rc.raw() as u64;
            return;
        }
    };

    let mut readable_event = KEvent::new();
    readable_event.writable_handle = Some(writable_handle);
    let readable_handle = match process
        .handle_table
        .add(KernelObject::Event(readable_event))
    {
        Ok(h) => h,
        Err(rc) => {
            let _ = process.handle_table.close(writable_handle);
            cpu.x[0] = rc.raw() as u64;
            return;
        }
    };

    // Update writable event with cross-reference.
    if let Ok(KernelObject::Event(ev)) = process.handle_table.get_mut(writable_handle) {
        ev.readable_handle = Some(readable_handle);
        ev.writable_handle = Some(writable_handle);
    }

    cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
    cpu.x[1] = writable_handle as u64;
    cpu.x[2] = readable_handle as u64;
    debug!(
        "CreateEvent: writable={}, readable={}",
        writable_handle, readable_handle
    );
}

// ---------------------------------------------------------------------------
// SVC 0x11: SignalEvent
// ---------------------------------------------------------------------------

fn svc_signal_event(kernel: &mut KernelCore, cpu: &mut CpuState, handle: Handle) {
    debug!("SignalEvent: handle={}", handle);

    let process = match kernel.process_mut() {
        Some(p) => p,
        None => {
            cpu.x[0] = error::INVALID_STATE.raw() as u64;
            return;
        }
    };

    // Look up the event and get its readable handle, then signal it.
    let readable_handle = match process.handle_table.get_mut(handle) {
        Ok(KernelObject::Event(ev)) => {
            ev.signaled = true;
            ev.readable_handle
        }
        _ => {
            cpu.x[0] = error::INVALID_HANDLE.raw() as u64;
            return;
        }
    };

    // Also mark the readable event as signaled.
    if let Some(rh) = readable_handle {
        if rh != handle {
            if let Ok(KernelObject::Event(rev)) = process.handle_table.get_mut(rh) {
                rev.signaled = true;
            }
        }
    }

    // Wake any threads waiting on the readable handle (or writable handle).
    let handles_to_check: Vec<Handle> = [Some(handle), readable_handle]
        .iter()
        .filter_map(|h| *h)
        .collect();

    for thread in process.threads.iter_mut() {
        if thread.state != ThreadState::Waiting {
            continue;
        }
        if let WaitReason::Synchronization { ref handles, .. } = thread.wait_reason {
            for (idx, h) in handles.iter().enumerate() {
                if handles_to_check.contains(h) {
                    // wake() sets state to Runnable and writes x[0]/x[1].
                    thread.wake(ResultCode::SUCCESS.raw(), idx as i32);
                    break;
                }
            }
        }
    }

    cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
}

// ---------------------------------------------------------------------------
// SVC 0x12: ClearEvent / SVC 0x17: ResetSignal
// ---------------------------------------------------------------------------

fn svc_clear_event(kernel: &mut KernelCore, cpu: &mut CpuState, handle: Handle) {
    debug!("ClearEvent/ResetSignal: handle={}", handle);

    let process = match kernel.process_mut() {
        Some(p) => p,
        None => {
            cpu.x[0] = error::INVALID_STATE.raw() as u64;
            return;
        }
    };

    match process.handle_table.get_mut(handle) {
        Ok(KernelObject::Event(ev)) => {
            ev.signaled = false;
            cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
        }
        _ => {
            cpu.x[0] = error::INVALID_HANDLE.raw() as u64;
        }
    }
}

// ---------------------------------------------------------------------------
// SVC 0x18: WaitSynchronization
// ---------------------------------------------------------------------------

fn svc_wait_synchronization(
    kernel: &mut KernelCore,
    cpu: &mut CpuState,
    handles_ptr: VAddr,
    handles_count: usize,
    timeout_ns: i64,
) {
    debug!(
        "WaitSynchronization: ptr=0x{:X}, count={}, timeout={}",
        handles_ptr, handles_count, timeout_ns
    );

    if handles_count == 0 || handles_count > 64 {
        cpu.x[0] = error::OUT_OF_RANGE.raw() as u64;
        return;
    }

    let thread_idx = match kernel.current_thread_idx {
        Some(idx) => idx,
        None => {
            cpu.x[0] = error::INVALID_STATE.raw() as u64;
            return;
        }
    };

    let process = match kernel.process_mut() {
        Some(p) => p,
        None => {
            cpu.x[0] = error::INVALID_STATE.raw() as u64;
            return;
        }
    };

    // Read handle array from guest memory.
    let mut handles = Vec::with_capacity(handles_count);
    for i in 0..handles_count {
        let addr = handles_ptr + (i as u64) * 4;
        match process.memory.read_u32(addr) {
            Ok(h) => handles.push(h),
            Err(_) => {
                cpu.x[0] = error::INVALID_ADDRESS.raw() as u64;
                return;
            }
        }
    }

    // Check if any handle is already signaled.
    for (idx, &handle) in handles.iter().enumerate() {
        let is_signaled = match process.handle_table.get(handle) {
            Ok(KernelObject::Event(ev)) => ev.signaled,
            _ => false,
        };

        if is_signaled {
            // Auto-clear the event.
            if let Ok(KernelObject::Event(ev)) = process.handle_table.get_mut(handle) {
                ev.signaled = false;
            }
            cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
            cpu.x[1] = idx as u64;
            debug!("WaitSynchronization: handle {} already signaled (idx={})", handle, idx);
            return;
        }
    }

    // None signaled.
    if timeout_ns == 0 {
        cpu.x[0] = error::TIMEOUT.raw() as u64;
        cpu.x[1] = u64::MAX; // invalid index
        return;
    }

    // Block the calling thread.
    let tick = kernel.tick_counter;
    let process = kernel.process_mut().unwrap();
    process.threads[thread_idx].begin_wait(
        WaitReason::Synchronization {
            handles,
            timeout_ns,
        },
        tick,
    );
    // Don't set cpu.x[0] yet — will be set when woken by SignalEvent or timeout.
    debug!(
        "WaitSynchronization: thread {} blocked (timeout_ns={})",
        thread_idx, timeout_ns
    );
}

// ---------------------------------------------------------------------------
// SVC 0x1A: ArbitrateLock
// ---------------------------------------------------------------------------

fn svc_arbitrate_lock(
    kernel: &mut KernelCore,
    cpu: &mut CpuState,
    _owner_handle: Handle,
    mutex_addr: VAddr,
    requester_handle: Handle,
) {
    debug!(
        "ArbitrateLock: mutex_addr=0x{:X}, requester={}",
        mutex_addr, requester_handle
    );

    let thread_idx = match kernel.current_thread_idx {
        Some(idx) => idx,
        None => {
            cpu.x[0] = error::INVALID_STATE.raw() as u64;
            return;
        }
    };

    let process = match kernel.process_mut() {
        Some(p) => p,
        None => {
            cpu.x[0] = error::INVALID_STATE.raw() as u64;
            return;
        }
    };

    // Read the mutex word from guest memory.
    let mutex_word = match process.memory.read_u32(mutex_addr) {
        Ok(v) => v,
        Err(_) => {
            cpu.x[0] = error::INVALID_ADDRESS.raw() as u64;
            return;
        }
    };

    let tag = mutex_word & 0x3FFF_FFFF; // Lower 30 bits: owner thread handle

    if tag == 0 || tag == requester_handle {
        // Unowned or already owned by requester: acquire.
        let _ = process
            .memory
            .write_u32(mutex_addr, requester_handle & 0x3FFF_FFFF);
        cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
    } else {
        // Owned by another thread: set has-waiters bit and block.
        let new_word = mutex_word | (1 << 30); // Set bit 30 (has waiters)
        let _ = process.memory.write_u32(mutex_addr, new_word);

        let tick = kernel.tick_counter;
        let process = kernel.process_mut().unwrap();
        process.threads[thread_idx].begin_wait(
            WaitReason::ArbitrateLock {
                mutex_addr,
                tag: requester_handle,
            },
            tick,
        );
        // Result will be set when woken.
    }
}

// ---------------------------------------------------------------------------
// SVC 0x1B: ArbitrateUnlock
// ---------------------------------------------------------------------------

fn svc_arbitrate_unlock(kernel: &mut KernelCore, cpu: &mut CpuState, mutex_addr: VAddr) {
    debug!("ArbitrateUnlock: mutex_addr=0x{:X}", mutex_addr);

    let process = match kernel.process_mut() {
        Some(p) => p,
        None => {
            cpu.x[0] = error::INVALID_STATE.raw() as u64;
            return;
        }
    };

    // Find the highest-priority waiting thread for this mutex.
    let best_waiter = process
        .threads
        .iter()
        .enumerate()
        .filter(|(_, t)| {
            t.state == ThreadState::Waiting
                && matches!(t.wait_reason, WaitReason::ArbitrateLock { mutex_addr: addr, .. } if addr == mutex_addr)
        })
        .min_by_key(|(_, t)| t.priority)
        .map(|(i, t)| (i, t.handle));

    if let Some((waiter_idx, waiter_handle)) = best_waiter {
        // Check if there are more waiters.
        let more_waiters = process
            .threads
            .iter()
            .enumerate()
            .filter(|(i, t)| {
                *i != waiter_idx
                    && t.state == ThreadState::Waiting
                    && matches!(t.wait_reason, WaitReason::ArbitrateLock { mutex_addr: addr, .. } if addr == mutex_addr)
            })
            .count()
            > 0;

        let new_word = (waiter_handle & 0x3FFF_FFFF)
            | if more_waiters { 1 << 30 } else { 0 };
        let _ = process.memory.write_u32(mutex_addr, new_word);

        // Wake the waiter — wake() sets x[0] to SUCCESS.
        process.threads[waiter_idx].wake(ResultCode::SUCCESS.raw(), -1);
    } else {
        // No waiters: clear the mutex word.
        let _ = process.memory.write_u32(mutex_addr, 0);
    }

    cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
}

// ---------------------------------------------------------------------------
// SVC 0x1C: WaitProcessWideKeyAtomic (condition variable wait)
// ---------------------------------------------------------------------------

fn svc_wait_process_wide_key_atomic(
    kernel: &mut KernelCore,
    cpu: &mut CpuState,
    mutex_addr: VAddr,
    condvar_addr: VAddr,
    _tag: u32,
    timeout_ns: i64,
) {
    debug!(
        "WaitProcessWideKeyAtomic: mutex=0x{:X}, condvar=0x{:X}, timeout={}",
        mutex_addr, condvar_addr, timeout_ns
    );

    let thread_idx = match kernel.current_thread_idx {
        Some(idx) => idx,
        None => {
            cpu.x[0] = error::INVALID_STATE.raw() as u64;
            return;
        }
    };

    // Step 1: Unlock the mutex (same logic as ArbitrateUnlock).
    svc_arbitrate_unlock(kernel, cpu, mutex_addr);

    // Step 2: Block the calling thread on the condvar.
    let tick = kernel.tick_counter;
    let process = kernel.process_mut().unwrap();
    process.threads[thread_idx].begin_wait(
        WaitReason::CondVar {
            condvar_addr,
            mutex_addr,
        },
        tick,
    );
    // Result will be set when woken by SignalProcessWideKey.
}

// ---------------------------------------------------------------------------
// SVC 0x1D: SignalProcessWideKey (condition variable signal)
// ---------------------------------------------------------------------------

fn svc_signal_process_wide_key(
    kernel: &mut KernelCore,
    cpu: &mut CpuState,
    condvar_addr: VAddr,
    count: i32,
) {
    debug!(
        "SignalProcessWideKey: condvar=0x{:X}, count={}",
        condvar_addr, count
    );

    let process = match kernel.process_mut() {
        Some(p) => p,
        None => {
            cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
            return;
        }
    };

    // Collect indices of threads waiting on this condvar, sorted by priority.
    let mut waiters: Vec<usize> = process
        .threads
        .iter()
        .enumerate()
        .filter(|(_, t)| {
            t.state == ThreadState::Waiting
                && matches!(t.wait_reason, WaitReason::CondVar { condvar_addr: addr, .. } if addr == condvar_addr)
        })
        .map(|(i, _)| i)
        .collect();

    waiters.sort_by_key(|&i| process.threads[i].priority);

    let wake_count = if count < 0 {
        waiters.len()
    } else {
        (count as usize).min(waiters.len())
    };

    for &idx in waiters.iter().take(wake_count) {
        // wake() sets state to Runnable and writes x[0] = SUCCESS.
        process.threads[idx].wake(ResultCode::SUCCESS.raw(), -1);
    }

    cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
}

// ---------------------------------------------------------------------------
// SVC 0x0B: SleepThread (improved)
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// SVC 0x34: WaitForAddress (address arbiter)
// ---------------------------------------------------------------------------

fn svc_wait_for_address(
    kernel: &mut KernelCore,
    cpu: &mut CpuState,
    addr: VAddr,
    arb_type: u32,
    value: u32,
    timeout_ns: i64,
) {
    debug!(
        "WaitForAddress: addr=0x{:X}, type={}, value={}, timeout={}",
        addr, arb_type, value, timeout_ns
    );

    let thread_idx = match kernel.current_thread_idx {
        Some(i) => i,
        None => {
            cpu.x[0] = error::INVALID_STATE.raw() as u64;
            return;
        }
    };

    let process = match kernel.process_mut() {
        Some(p) => p,
        None => {
            cpu.x[0] = error::INVALID_STATE.raw() as u64;
            return;
        }
    };

    let mem_val = process.memory.read_u32(addr).unwrap_or(0);

    let should_wait = match arb_type {
        // WaitIfLessThan
        0 => mem_val < value,
        // DecrementAndWaitIfLessThan
        1 => {
            if mem_val > 0 && mem_val <= value {
                let _ = process.memory.write_u32(addr, mem_val.wrapping_sub(1));
                true
            } else {
                false
            }
        }
        // WaitIfEqual
        2 => mem_val == value,
        _ => {
            cpu.x[0] = error::INVALID_ENUM_VALUE.raw() as u64;
            return;
        }
    };

    if should_wait {
        if timeout_ns == 0 {
            cpu.x[0] = error::TIMEOUT.raw() as u64;
            return;
        }
        let tick = kernel.tick_counter;
        let process = kernel.process_mut().unwrap();
        process.threads[thread_idx].begin_wait(WaitReason::AddressArbiter { addr }, tick);
        // Result will be set when woken.
    } else {
        cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
    }
}

// ---------------------------------------------------------------------------
// SVC 0x35: SignalToAddress (address arbiter)
// ---------------------------------------------------------------------------

fn svc_signal_to_address(
    kernel: &mut KernelCore,
    cpu: &mut CpuState,
    addr: VAddr,
    signal_type: u32,
    value: u32,
    count: i32,
) {
    debug!(
        "SignalToAddress: addr=0x{:X}, type={}, value={}, count={}",
        addr, signal_type, value, count
    );

    let process = match kernel.process_mut() {
        Some(p) => p,
        None => {
            cpu.x[0] = error::INVALID_STATE.raw() as u64;
            return;
        }
    };

    match signal_type {
        // Signal: just wake waiters
        0 => {}
        // SignalAndIncrementIfEqual
        1 => {
            let mem_val = process.memory.read_u32(addr).unwrap_or(0);
            if mem_val != value {
                cpu.x[0] = error::INVALID_STATE.raw() as u64;
                return;
            }
            let _ = process.memory.write_u32(addr, value.wrapping_add(1));
        }
        // SignalAndModifyByWaitingCountIfEqual
        2 => {
            let mem_val = process.memory.read_u32(addr).unwrap_or(0);
            if mem_val != value {
                cpu.x[0] = error::INVALID_STATE.raw() as u64;
                return;
            }
            // Count waiters on this address.
            let waiter_count = process
                .threads
                .iter()
                .filter(|t| {
                    t.state == ThreadState::Waiting
                        && matches!(t.wait_reason, WaitReason::AddressArbiter { addr: a } if a == addr)
                })
                .count() as u32;
            let new_val = if waiter_count > 0 {
                value.wrapping_add(1)
            } else {
                value.wrapping_sub(1)
            };
            let _ = process.memory.write_u32(addr, new_val);
        }
        _ => {
            cpu.x[0] = error::INVALID_ENUM_VALUE.raw() as u64;
            return;
        }
    }

    // Wake waiters sorted by priority.
    let mut waiters: Vec<usize> = process
        .threads
        .iter()
        .enumerate()
        .filter(|(_, t)| {
            t.state == ThreadState::Waiting
                && matches!(t.wait_reason, WaitReason::AddressArbiter { addr: a } if a == addr)
        })
        .map(|(i, _)| i)
        .collect();
    waiters.sort_by_key(|&i| process.threads[i].priority);

    let wake_count = if count < 0 {
        waiters.len()
    } else {
        (count as usize).min(waiters.len())
    };

    for &idx in waiters.iter().take(wake_count) {
        process.threads[idx].wake(ResultCode::SUCCESS.raw(), -1);
    }

    cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
}

// ---------------------------------------------------------------------------
// SVC 0x0B: SleepThread (improved)
// ---------------------------------------------------------------------------

fn svc_sleep_thread(kernel: &mut KernelCore, cpu: &mut CpuState, ns: i64) {
    debug!("SleepThread: ns={}", ns);

    let thread_idx = match kernel.current_thread_idx {
        Some(idx) => idx,
        None => {
            // No thread context — fallback to success.
            cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
            return;
        }
    };

    if ns == 0 || ns == -2 {
        // Yield: set thread to Runnable and let scheduler pick another.
        // The thread stays Runnable so it can be re-scheduled immediately.
        cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
    } else if ns > 0 {
        // Timed sleep: convert ns to ticks and block.
        let current_tick = kernel.tick_counter;
        let timeout_ticks = (ns as u128 * 19_200_000 / 1_000_000_000) as u64;
        let wake_tick = current_tick + timeout_ticks;

        let process = kernel.process_mut().unwrap();
        process.threads[thread_idx].begin_wait(WaitReason::Sleep { wake_tick }, current_tick);
        // Result will be set when woken by timeout check.
    } else {
        // ns == -1: sleep until explicitly woken (rare).
        cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
    }
}
