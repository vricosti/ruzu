//! Port of zuyu/src/core/hle/kernel/svc/svc_address_arbiter.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-20
//!
//! SVC handlers for address arbiter operations (WaitForAddress, SignalToAddress).
//!
//! Upstream delegates to GetCurrentProcess(kernel).WaitAddressArbiter / SignalAddressArbiter,
//! which in turn delegate to KProcess::m_address_arbiter (a KAddressArbiter).
//! The Rust KProcess does not yet have an address_arbiter field — its comment says:
//!   // m_address_arbiter — KAddressArbiter
//! The KAddressArbiter struct exists but its methods are stubs.
//! Once KProcess gains an address_arbiter field, the delegation below will be complete.

use crate::core::System;
use crate::hle::kernel::k_address_arbiter::{ArbitrationType as KArbType, SignalType as KSigType};
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

/// Convert SVC ArbitrationType to KAddressArbiter ArbitrationType.
fn to_k_arb_type(arb_type: ArbitrationType) -> KArbType {
    match arb_type {
        ArbitrationType::WaitIfLessThan => KArbType::WaitIfLessThan,
        ArbitrationType::DecrementAndWaitIfLessThan => KArbType::DecrementAndWaitIfLessThan,
        ArbitrationType::WaitIfEqual => KArbType::WaitIfEqual,
    }
}

/// Convert SVC SignalType to KAddressArbiter SignalType.
fn to_k_sig_type(signal_type: SignalType) -> KSigType {
    match signal_type {
        SignalType::Signal => KSigType::Signal,
        SignalType::SignalAndIncrementIfEqual => KSigType::SignalAndIncrementIfEqual,
        SignalType::SignalAndModifyByWaitingCountIfEqual => {
            KSigType::SignalAndModifyByWaitingCountIfEqual
        }
    }
}

/// Wait for an address (via Address Arbiter).
pub fn wait_for_address(
    system: &System,
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
    // Upstream: kernel.HardwareTimer().GetTick() + offset_tick + 2
    let timeout: i64 = if timeout_ns > 0 {
        let offset_tick = timeout_ns;
        if offset_tick > 0 {
            let hardware_tick = system
                .kernel()
                .and_then(|k| k.hardware_timer())
                .map(|ht| ht.lock().unwrap().get_tick())
                .unwrap_or(0);
            let t = hardware_tick + offset_tick + 2;
            if t <= 0 { i64::MAX } else { t }
        } else {
            i64::MAX
        }
    } else {
        timeout_ns
    };

    let result = system
        .current_process_arc()
        .lock()
        .unwrap()
        .wait_address_arbiter(address, to_k_arb_type(arb_type), value, timeout);
    ResultCode::new(result)
}

/// Signals to an address (via Address Arbiter).
pub fn signal_to_address(
    system: &System,
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

    let result = system
        .current_process_arc()
        .lock()
        .unwrap()
        .signal_address_arbiter(address, to_k_sig_type(signal_type), value, count);
    ResultCode::new(result)
}
