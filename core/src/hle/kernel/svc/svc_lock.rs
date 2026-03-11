//! Port of zuyu/src/core/hle/kernel/svc/svc_lock.cpp
//! Status: COMPLET (stubs for kernel calls)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for mutex arbitration (ArbitrateLock, ArbitrateUnlock).

use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc_common::Handle;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// Attempts to lock a mutex.
pub fn arbitrate_lock(thread_handle: Handle, address: u64, tag: u32) -> ResultCode {
    log::trace!(
        "svc::ArbitrateLock called thread_handle=0x{:08X}, address=0x{:X}, tag=0x{:08X}",
        thread_handle, address, tag
    );

    // Validate the input address.
    // TODO: R_UNLESS(!IsKernelAddress(address), ResultInvalidCurrentMemory)
    if address % 4 != 0 {
        return RESULT_INVALID_ADDRESS;
    }

    // TODO: KConditionVariable::WaitForAddress(kernel, thread_handle, address, tag)
    log::warn!("svc::ArbitrateLock: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Unlocks a mutex.
pub fn arbitrate_unlock(address: u64) -> ResultCode {
    log::trace!("svc::ArbitrateUnlock called address=0x{:X}", address);

    // Validate the input address.
    // TODO: R_UNLESS(!IsKernelAddress(address), ResultInvalidCurrentMemory)
    if address % 4 != 0 {
        return RESULT_INVALID_ADDRESS;
    }

    // TODO: KConditionVariable::SignalToAddress(kernel, address)
    log::warn!("svc::ArbitrateUnlock: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}
