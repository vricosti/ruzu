// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use common::{error, Handle, ResultCode, VAddr};
use log::debug;

use crate::kernel::KernelCore;
use crate::thread::{ThreadState, WaitReason};
use ruzu_cpu::CpuState;

/// SVC 0x1A: ArbitrateLock
pub fn svc_arbitrate_lock(
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

/// SVC 0x1B: ArbitrateUnlock
pub fn svc_arbitrate_unlock(kernel: &mut KernelCore, cpu: &mut CpuState, mutex_addr: VAddr) {
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

        let new_word = (waiter_handle & 0x3FFF_FFFF) | if more_waiters { 1 << 30 } else { 0 };
        let _ = process.memory.write_u32(mutex_addr, new_word);

        // Wake the waiter — wake() sets x[0] to SUCCESS.
        process.threads[waiter_idx].wake(ResultCode::SUCCESS.raw(), -1);
    } else {
        // No waiters: clear the mutex word.
        let _ = process.memory.write_u32(mutex_addr, 0);
    }

    cpu.x[0] = ResultCode::SUCCESS.raw() as u64;
}
