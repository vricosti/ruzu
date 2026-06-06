//! Port of zuyu/src/core/hle/kernel/svc/svc_address_arbiter.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-20
//!
//! SVC handlers for address arbiter operations (WaitForAddress, SignalToAddress).
//!
//! Upstream delegates to GetCurrentProcess(kernel).WaitAddressArbiter / SignalAddressArbiter,
//! which in turn delegate to KProcess::m_address_arbiter (a KAddressArbiter).
//! Rust mirrors that ownership through `KProcess::address_arbiter`.

use crate::core::System;
use crate::hle::kernel::k_address_arbiter::{ArbitrationType as KArbType, SignalType as KSigType};
use crate::hle::kernel::k_memory_layout::is_kernel_address;
use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::result::{ResultCode, RESULT_SUCCESS};

fn should_trace_sync_debug() -> bool {
    std::env::var_os("RUZU_TRACE_SYNC").is_some()
}

fn should_trace_sync_backtrace_once(tid: u64) -> bool {
    static DID_TRACE_TID73: std::sync::atomic::AtomicBool =
        std::sync::atomic::AtomicBool::new(false);
    tid == 73 && !DID_TRACE_TID73.swap(true, std::sync::atomic::Ordering::Relaxed)
}

fn log_sync_context(system: &System, label: &str) {
    let Some(current_thread_id) = system.current_thread_id() else {
        return;
    };
    let Some(current_thread) = system.current_thread() else {
        return;
    };
    let core_index = current_thread.lock().unwrap().get_current_core().max(0) as usize;
    let process = system.current_process_arc().lock().unwrap();
    let Some(cpu) = process.get_arm_interface(core_index) else {
        return;
    };
    let mut ctx = crate::arm::arm_interface::ThreadContext::default();
    cpu.get_context(&mut ctx);
    log::info!(
        "svc::{} ctx tid={} pc=0x{:08X} lr=0x{:08X} sp=0x{:08X} r0=0x{:08X} r1=0x{:08X} r2=0x{:08X} r3=0x{:08X}",
        label,
        current_thread_id,
        ctx.pc,
        ctx.lr,
        ctx.sp,
        ctx.r[0] as u32,
        ctx.r[1] as u32,
        ctx.r[2] as u32,
        ctx.r[3] as u32,
    );
    if should_trace_sync_backtrace_once(current_thread_id) {
        let bt = crate::arm::debug::get_backtrace_from_context(&process, &ctx);
        for (index, entry) in bt.iter().take(12).enumerate() {
            log::info!(
                "svc::{} bt[{}]: tid={} module={} addr=0x{:X} orig=0x{:X} off=0x{:X} symbol={}",
                label,
                index,
                current_thread_id,
                entry.module,
                entry.address,
                entry.original_address,
                entry.offset,
                entry.name,
            );
        }
    }
}

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
        address,
        arb_type,
        value,
        timeout_ns
    );
    if should_trace_sync_debug() {
        log::info!(
            "svc::WaitForAddress tid={:?} address=0x{:X} arb_type={:?} value=0x{:X} timeout_ns={}",
            system.current_thread_id(),
            address,
            arb_type,
            value,
            timeout_ns
        );
        log_sync_context(system, "WaitForAddress");
    }

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
                .and_then(|_| crate::hle::kernel::kernel::get_current_hardware_tick())
                .unwrap_or(0);
            let t = hardware_tick + offset_tick + 2;
            if t <= 0 {
                i64::MAX
            } else {
                t
            }
        } else {
            i64::MAX
        }
    } else {
        timeout_ns
    };

    let process_arc = system.current_process_arc().clone();
    let Some(current_thread) = system.current_thread() else {
        return RESULT_INVALID_HANDLE;
    };
    let result = crate::hle::kernel::k_process::KProcess::wait_address_arbiter(
        &process_arc,
        &current_thread,
        address,
        to_k_arb_type(arb_type),
        value,
        timeout,
    );
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
        address,
        signal_type,
        value,
        count
    );
    if should_trace_sync_debug() {
        log::info!(
            "svc::SignalToAddress tid={:?} address=0x{:X} signal_type={:?} value=0x{:X} count=0x{:X}",
            system.current_thread_id(),
            address,
            signal_type,
            value,
            count
        );
        log_sync_context(system, "SignalToAddress");
    }

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
