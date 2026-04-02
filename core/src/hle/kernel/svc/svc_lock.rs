//! Port of zuyu/src/core/hle/kernel/svc/svc_lock.cpp
//! Status: COMPLET (stubs for kernel calls)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for mutex arbitration (ArbitrateLock, ArbitrateUnlock).

use crate::core::System;
use crate::hle::kernel::k_condition_variable::KConditionVariable;
use crate::hle::kernel::k_memory_layout::is_kernel_address;
use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc_common::Handle;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// Attempts to lock a mutex.
pub fn arbitrate_lock(
    system: &System,
    thread_handle: Handle,
    address: u64,
    tag: u32,
) -> ResultCode {
    log::info!(
        "svc::ArbitrateLock called thread_handle=0x{:08X}, address=0x{:X}, tag=0x{:08X}",
        thread_handle,
        address,
        tag
    );

    // Validate the input address.
    if is_kernel_address(address as usize) {
        return RESULT_INVALID_CURRENT_MEMORY;
    }
    if address % 4 != 0 {
        return RESULT_INVALID_ADDRESS;
    }

    let Some(current_thread) = system.current_thread() else {
        return RESULT_INVALID_HANDLE;
    };

    let result = KConditionVariable::wait_for_address(
        system.current_process_arc(),
        &current_thread,
        thread_handle,
        address,
        tag,
    );

    log::info!(
        "svc::ArbitrateLock return thread_handle=0x{:08X}, address=0x{:X}, tag=0x{:08X}, result={:#x}",
        thread_handle,
        address,
        tag,
        result.get_inner_value()
    );

    result
}

/// Unlocks a mutex.
pub fn arbitrate_unlock(system: &System, address: u64) -> ResultCode {
    log::info!("svc::ArbitrateUnlock called address=0x{:X}", address);

    // Validate the input address.
    if is_kernel_address(address as usize) {
        return RESULT_INVALID_CURRENT_MEMORY;
    }
    if address % 4 != 0 {
        return RESULT_INVALID_ADDRESS;
    }

    let Some(current_thread) = system.current_thread() else {
        return RESULT_INVALID_HANDLE;
    };

    let result = KConditionVariable::signal_to_address(
        system.current_process_arc(),
        &current_thread,
        address,
    );

    log::info!(
        "svc::ArbitrateUnlock return address=0x{:X}, result={:#x}",
        address,
        result.get_inner_value()
    );

    result
}
