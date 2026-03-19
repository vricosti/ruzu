//! Port of zuyu/src/core/hle/kernel/svc/svc_address_arbiter.cpp
//! Status: COMPLET (stubs for kernel calls)
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for address arbiter operations (WaitForAddress, SignalToAddress).

use crate::hle::kernel::k_memory_layout::is_kernel_address;
use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

fn is_valid_signal_type(signal_type: SignalType) -> bool {
    matches!(
        signal_type,
        SignalType::Signal
            | SignalType::SignalAndIncrementIfEqual
            | SignalType::SignalAndModifyByWaitingCountIfEqual
    )
}

fn is_valid_arbitration_type(arb_type: ArbitrationType) -> bool {
    matches!(
        arb_type,
        ArbitrationType::WaitIfLessThan
            | ArbitrationType::DecrementAndWaitIfLessThan
            | ArbitrationType::WaitIfEqual
    )
}

/// Wait for an address (via Address Arbiter).
pub fn wait_for_address(
    address: u64,
    arb_type: ArbitrationType,
    value: i32,
    timeout_ns: i64,
) -> ResultCode {
    log::trace!(
        "svc::WaitForAddress called, address=0x{:X}, arb_type={:?}, value=0x{:X}, timeout_ns={}",
        address, arb_type, value, timeout_ns
    );

    // Validate input.
    if is_kernel_address(address as usize) {
        return RESULT_INVALID_CURRENT_MEMORY;
    }
    if address % 4 != 0 {
        return RESULT_INVALID_ADDRESS;
    }
    if !is_valid_arbitration_type(arb_type) {
        return RESULT_INVALID_ENUM_VALUE;
    }

    // Convert timeout from nanoseconds to ticks.
    let _timeout: i64 = if timeout_ns > 0 {
        let offset_tick = timeout_ns;
        if offset_tick > 0 {
            // TODO: system.Kernel().HardwareTimer().GetTick() + offset_tick + 2
            let t = offset_tick + 2;
            if t <= 0 { i64::MAX } else { t }
        } else {
            i64::MAX
        }
    } else {
        timeout_ns
    };

    // TODO: GetCurrentProcess(kernel).WaitAddressArbiter(address, arb_type, value, timeout)
    log::warn!("svc::WaitForAddress: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}

/// Signals to an address (via Address Arbiter).
pub fn signal_to_address(
    address: u64,
    signal_type: SignalType,
    value: i32,
    count: i32,
) -> ResultCode {
    log::trace!(
        "svc::SignalToAddress called, address=0x{:X}, signal_type={:?}, value=0x{:X}, count=0x{:X}",
        address, signal_type, value, count
    );

    // Validate input.
    if is_kernel_address(address as usize) {
        return RESULT_INVALID_CURRENT_MEMORY;
    }
    if address % 4 != 0 {
        return RESULT_INVALID_ADDRESS;
    }
    if !is_valid_signal_type(signal_type) {
        return RESULT_INVALID_ENUM_VALUE;
    }

    // TODO: GetCurrentProcess(kernel).SignalAddressArbiter(address, signal_type, value, count)
    log::warn!("svc::SignalToAddress: kernel object access not yet implemented");
    RESULT_NOT_IMPLEMENTED
}
