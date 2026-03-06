// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use common::{error, ResultCode, VAddr};
use log::debug;

use crate::kernel::KernelCore;
use crate::thread::{ThreadState, WaitReason};
use ruzu_cpu::CpuState;

/// SVC 0x34: WaitForAddress (address arbiter)
pub fn svc_wait_for_address(
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

/// SVC 0x35: SignalToAddress (address arbiter)
pub fn svc_signal_to_address(
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
