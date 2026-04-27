//! Port of zuyu/src/core/hle/kernel/svc/svc_condition_variable.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-20
//!
//! SVC handlers for condition variable operations.

use crate::core::System;
use crate::hle::kernel::k_memory_layout::is_kernel_address;
use crate::hle::kernel::svc::svc_results::*;
use crate::hle::result::ResultCode;

fn should_trace_cv_debug() -> bool {
    std::env::var_os("RUZU_TRACE_CV").is_some()
}

fn should_trace_cv_backtrace_once(tid: u64) -> bool {
    static DID_TRACE_TID96: std::sync::atomic::AtomicBool =
        std::sync::atomic::AtomicBool::new(false);
    static DID_TRACE_TID98: std::sync::atomic::AtomicBool =
        std::sync::atomic::AtomicBool::new(false);
    static DID_TRACE_TID99: std::sync::atomic::AtomicBool =
        std::sync::atomic::AtomicBool::new(false);
    static TID73_SIGNAL_COUNT: std::sync::atomic::AtomicU64 =
        std::sync::atomic::AtomicU64::new(0);
    match tid {
        96 => !DID_TRACE_TID96.swap(true, std::sync::atomic::Ordering::Relaxed),
        98 => !DID_TRACE_TID98.swap(true, std::sync::atomic::Ordering::Relaxed),
        99 => !DID_TRACE_TID99.swap(true, std::sync::atomic::Ordering::Relaxed),
        73 => {
            let n = TID73_SIGNAL_COUNT.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            // Post-MiiFix stall lands around ~198 signals. Capture the tail
            // window to see the last game PC before tid=73 goes silent.
            (190..=200).contains(&n)
        }
        _ => false,
    }
}

/// Wait process wide key atomic.
pub fn wait_process_wide_key_atomic(
    system: &System,
    address: u64,
    cv_key: u64,
    tag: u32,
    timeout_ns: i64,
) -> ResultCode {
    if should_trace_cv_debug() {
        log::info!(
            "svc::WaitProcessWideKeyAtomic tid={:?} address=0x{:X} cv_key=0x{:X} tag=0x{:08X} timeout_ns={}",
            system.current_thread_id(),
            address,
            cv_key,
            tag,
            timeout_ns
        );
        if let Some(current_thread_id) = system.current_thread_id() {
            if let Some(current_thread) = system.current_thread() {
                let core_index = current_thread.lock().unwrap().get_current_core().max(0) as usize;
                let process = system.current_process_arc().lock().unwrap();
                if let Some(cpu) = process.get_arm_interface(core_index) {
                    let mut ctx = crate::arm::arm_interface::ThreadContext::default();
                    cpu.get_context(&mut ctx);
                    log::info!(
                        "svc::WaitProcessWideKeyAtomic ctx tid={} pc=0x{:08X} lr=0x{:08X} sp=0x{:08X}",
                        current_thread_id,
                        ctx.pc,
                        ctx.lr,
                        ctx.sp
                    );
                    if should_trace_cv_backtrace_once(current_thread_id) {
                        let bt = crate::arm::debug::get_backtrace_from_context(&process, &ctx);
                        for (index, entry) in bt.iter().take(12).enumerate() {
                            log::info!(
                                "svc::WaitProcessWideKeyAtomic bt[{}]: tid={} module={} addr=0x{:X} orig=0x{:X} off=0x{:X} symbol={}",
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
            }
        }
    }
    log::trace!(
        "svc::WaitProcessWideKeyAtomic called address=0x{:X}, cv_key=0x{:X}, tag=0x{:08X}, timeout_ns={}",
        address, cv_key, tag, timeout_ns
    );

    // Validate input.
    if is_kernel_address(address as usize) {
        return RESULT_INVALID_CURRENT_MEMORY;
    }
    if address % 4 != 0 {
        return RESULT_INVALID_ADDRESS;
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

    let Some(current_thread) = system.current_thread() else {
        return RESULT_INVALID_HANDLE;
    };

    // Upstream: Common::AlignDown(cv_key, sizeof(u32)) — aligns down to 4-byte boundary.
    let aligned_cv_key = cv_key & !3u64;
    let result = crate::hle::kernel::k_process::KProcess::wait_condition_variable(
        &system.current_process_arc(),
        &current_thread,
        address,
        aligned_cv_key,
        tag,
        timeout,
    );

    log::trace!(
        "svc::WaitProcessWideKeyAtomic return address=0x{:X}, cv_key=0x{:X}, result={:#x}",
        address,
        aligned_cv_key,
        result
    );
    if should_trace_cv_debug() {
        log::info!(
            "svc::WaitProcessWideKeyAtomic return tid={:?} address=0x{:X} cv_key=0x{:X} result=0x{:08X}",
            system.current_thread_id(),
            address,
            aligned_cv_key,
            result
        );
    }

    ResultCode::new(result)
}

/// Signal process wide key.
pub fn signal_process_wide_key(system: &System, cv_key: u64, count: i32) {
    if should_trace_cv_debug() {
        log::info!(
            "svc::SignalProcessWideKey tid={:?} cv_key=0x{:X} count={}",
            system.current_thread_id(),
            cv_key,
            count
        );
        if let Some(current_thread_id) = system.current_thread_id() {
            if should_trace_cv_backtrace_once(current_thread_id) {
                if let Some(current_thread) = system.current_thread() {
                    let core_index =
                        current_thread.lock().unwrap().get_current_core().max(0) as usize;
                    let process = system.current_process_arc().lock().unwrap();
                    if let Some(cpu) = process.get_arm_interface(core_index) {
                        let mut ctx = crate::arm::arm_interface::ThreadContext::default();
                        cpu.get_context(&mut ctx);
                        log::info!(
                            "svc::SignalProcessWideKey ctx tid={} pc=0x{:08X} lr=0x{:08X} sp=0x{:08X}",
                            current_thread_id,
                            ctx.pc,
                            ctx.lr,
                            ctx.sp
                        );
                        let bt = crate::arm::debug::get_backtrace_from_context(&process, &ctx);
                        for (index, entry) in bt.iter().take(12).enumerate() {
                            log::info!(
                                "svc::SignalProcessWideKey bt[{}]: tid={} module={} addr=0x{:X} orig=0x{:X} off=0x{:X} symbol={}",
                                index,
                                current_thread_id,
                                entry.module,
                                entry.address,
                                entry.original_address,
                                entry.offset,
                                entry.name,
                            );
                        }
                        // Dump general-purpose registers r0..r12.
                        let regs = ctx.r;
                        log::info!(
                            "svc::SignalProcessWideKey regs tid={} r0=0x{:08X} r1=0x{:08X} r2=0x{:08X} r3=0x{:08X} r4=0x{:08X} r5=0x{:08X} r6=0x{:08X} r7=0x{:08X} r8=0x{:08X} r9=0x{:08X} r10=0x{:08X} r11=0x{:08X} r12=0x{:08X}",
                            current_thread_id,
                            regs[0] as u32, regs[1] as u32, regs[2] as u32, regs[3] as u32,
                            regs[4] as u32, regs[5] as u32, regs[6] as u32, regs[7] as u32,
                            regs[8] as u32, regs[9] as u32, regs[10] as u32, regs[11] as u32,
                            regs[12] as u32,
                        );
                        // Dump 20 ARM32 insns around LR (the BL return site — LR-4 is the BL itself).
                        if let Some(memory) = process.page_table.get_base().m_memory.as_ref() {
                            let m = memory.lock().unwrap();
                            let start = ctx.lr.saturating_sub(4 * 4);
                            let mut disasm = String::new();
                            for offset in 0..20u64 {
                                let addr = start + offset * 4;
                                let word = m.read_32(addr);
                                disasm.push_str(&format!(" {:08X}:{:08X}", addr as u32, word));
                            }
                            log::info!(
                                "svc::SignalProcessWideKey insns tid={} lr=0x{:08X}:{}",
                                current_thread_id, ctx.lr, disasm
                            );
                            // Dump vtable lookup if R5 is non-null.
                            let r5 = regs[5] as u32;
                            if r5 != 0 {
                                let vtable_ptr = m.read_32(r5 as u64);
                                let vmethod0 = m.read_32(vtable_ptr as u64);
                                log::info!(
                                    "svc::SignalProcessWideKey vtable tid={} r5=0x{:08X} *r5=vtable=0x{:08X} vtable[0]=0x{:08X}",
                                    current_thread_id, r5, vtable_ptr, vmethod0
                                );
                            }
                        }
                    }
                }
            }
        }
    }
    log::trace!(
        "svc::SignalProcessWideKey called, cv_key=0x{:X}, count=0x{:08X}",
        cv_key,
        count
    );

    // Upstream: Common::AlignDown(cv_key, sizeof(u32))
    let aligned_cv_key = cv_key & !3u64;
    system
        .current_process_arc()
        .lock()
        .unwrap()
        .signal_condition_variable(aligned_cv_key, count);
    log::trace!(
        "svc::SignalProcessWideKey return cv_key=0x{:X}, count={}",
        aligned_cv_key,
        count
    );
}
