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
    match tid {
        96 => !DID_TRACE_TID96.swap(true, std::sync::atomic::Ordering::Relaxed),
        98 => !DID_TRACE_TID98.swap(true, std::sync::atomic::Ordering::Relaxed),
        99 => !DID_TRACE_TID99.swap(true, std::sync::atomic::Ordering::Relaxed),
        _ => false,
    }
}

/// Backtrace gate for `SignalProcessWideKey`. When `RUZU_TRACE_CV_BT_HEAP=1`,
/// log a backtrace for every signal whose cv_key lives in the heap region
/// (>= 0x40000000). The first ~50 such signals from tid=73 are the
/// chain-wakeup call sites — analyzing those identifies the predicate
/// that gates the producer-side "wake worker" path.
///
/// `RUZU_TRACE_CV_BT_AT=0xCV_KEY` captures backtrace+stack for a specific
/// cv_key (max 5 hits). Used to identify the call site of producer-thread
/// signals that hit no-waiter cv_keys (e.g., 0x22C17A4 in MK8D).
fn should_trace_cv_backtrace_for_signal(tid: u64, cv_key: u64) -> bool {
    if std::env::var_os("RUZU_TRACE_CV_BT_HEAP").is_some() && tid == 73 && cv_key >= 0x4000_0000 {
        static N: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
        let n = N.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        return n < 50;
    }
    if let Ok(spec) = std::env::var("RUZU_TRACE_CV_BT_AT") {
        if let Ok(target_cv) = u64::from_str_radix(spec.trim_start_matches("0x"), 16) {
            if cv_key == target_cv {
                static N: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
                let n = N.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                let _ = tid;
                return n < 5;
            }
        }
    }
    false
}

/// One-shot literal-pool scan triggered on the first SignalProcessWideKey.
/// Reads `RUZU_FIND_WORD_AT_SIGNAL=0xVALUE:0xSTART:0xLEN` and scans guest
/// memory for any 4-byte word equal to VALUE within [START..START+LEN).
/// Used to find code/data that references a known guest address (e.g., a
/// .data cv_key that has signals but no waiters — finding load sites of
/// that address identifies the missing waiter's code path).
fn maybe_scan_word_at_signal(system: &System) {
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::sync::OnceLock;
    static FIRED: AtomicBool = AtomicBool::new(false);
    static SPEC: OnceLock<Option<(u32, u64, u64)>> = OnceLock::new();
    let spec = *SPEC.get_or_init(|| {
        let raw = std::env::var("RUZU_FIND_WORD_AT_SIGNAL").ok()?;
        let parts: Vec<&str> = raw.split(':').collect();
        if parts.len() != 3 {
            return None;
        }
        let value = u32::from_str_radix(parts[0].trim_start_matches("0x"), 16).ok()?;
        let start = u64::from_str_radix(parts[1].trim_start_matches("0x"), 16).ok()?;
        let len = u64::from_str_radix(parts[2].trim_start_matches("0x"), 16).ok()?;
        Some((value, start, len))
    });
    let Some((value, start, len)) = spec else {
        return;
    };
    if FIRED.swap(true, Ordering::SeqCst) {
        return;
    }
    let process = system.current_process_arc();
    let process = process.lock().unwrap();
    let Some(memory) = process.page_table.get_base().m_memory.as_ref() else {
        eprintln!("[FIND_WORD_AT_SIGNAL] no memory");
        return;
    };
    let m = memory.lock().unwrap();
    let end = start + len;
    let mut addr = start;
    let mut hits = 0u32;
    while addr + 4 <= end {
        let w = m.read_32(addr);
        if w == value {
            let ctx_start = addr.saturating_sub(8);
            let mut ctx = String::with_capacity(48);
            for i in 0..16u64 {
                use std::fmt::Write;
                let _ = write!(ctx, "{:02x}", m.read_8(ctx_start + i));
                if i == 7 {
                    ctx.push('|');
                }
            }
            eprintln!(
                "[WORD_HIT] addr=0x{:08X} val=0x{:08X} ctx=[{}]",
                addr, value, ctx
            );
            hits += 1;
            if hits > 64 {
                eprintln!("[WORD_HIT] (more hits suppressed)");
                break;
            }
        }
        addr += 4;
    }
    eprintln!(
        "[WORD_HIT] scan done: {} hits in [0x{:X}..0x{:X}] for value=0x{:08X}",
        hits, start, end, value
    );
    // Also scan for ARM32 MOVW + MOVT pairs that materialize the same value.
    let target_lo = value & 0xFFFF;
    let target_hi = (value >> 16) & 0xFFFF;
    let mut pc = start;
    let mut movw_hits = 0u32;
    while pc + 4 <= end {
        let insn = (m.read_8(pc) as u32)
            | ((m.read_8(pc + 1) as u32) << 8)
            | ((m.read_8(pc + 2) as u32) << 16)
            | ((m.read_8(pc + 3) as u32) << 24);
        if (insn & 0x0FF00000) == 0x03000000 {
            let imm4 = (insn >> 16) & 0xF;
            let imm12 = insn & 0xFFF;
            let imm16 = (imm4 << 12) | imm12;
            let rd = ((insn >> 12) & 0xF) as u8;
            if imm16 == target_lo {
                let mut q = pc + 4;
                let q_end = (pc + 32).min(end);
                while q + 4 <= q_end {
                    let qi = (m.read_8(q) as u32)
                        | ((m.read_8(q + 1) as u32) << 8)
                        | ((m.read_8(q + 2) as u32) << 16)
                        | ((m.read_8(q + 3) as u32) << 24);
                    if (qi & 0x0FF00000) == 0x03400000 {
                        let q_rd = ((qi >> 12) & 0xF) as u8;
                        if q_rd == rd {
                            let q_imm16 = (((qi >> 16) & 0xF) << 12) | (qi & 0xFFF);
                            if q_imm16 == target_hi {
                                eprintln!(
                                    "[MOVW_HIT] movw_pc=0x{:08X} movt_pc=0x{:08X} rd=r{} value=0x{:08X}",
                                    pc, q, rd, value
                                );
                                movw_hits += 1;
                                if movw_hits > 64 {
                                    eprintln!("[MOVW_HIT] (more hits suppressed)");
                                    break;
                                }
                            }
                            break;
                        }
                    }
                    q += 4;
                }
            }
        }
        pc += 4;
    }
    eprintln!(
        "[MOVW_HIT] scan done: {} hits in [0x{:X}..0x{:X}] for value=0x{:08X}",
        movw_hits, start, end, value
    );
}

/// Wait process wide key atomic.
pub fn wait_process_wide_key_atomic(
    system: &System,
    address: u64,
    cv_key: u64,
    tag: u32,
    timeout_ns: i64,
) -> ResultCode {
    maybe_dump_mem_at_wait(system, cv_key);
    dump_cv_state(system, "WAIT", address, cv_key, Some(tag), Some(timeout_ns));
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
                    if should_trace_cv_backtrace_once(current_thread_id)
                        || should_trace_cv_backtrace_for_signal(current_thread_id, cv_key)
                    {
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
                        // Stack dump (48 words) for manual return-address walking.
                        if let Some(memory) = process.page_table.get_base().m_memory.as_ref() {
                            let m = memory.lock().unwrap();
                            let sp = ctx.sp;
                            let mut stack = String::new();
                            for offset in 0..48u64 {
                                use std::fmt::Write;
                                let addr = sp.saturating_add(offset * 4);
                                let _ = write!(stack, " {:08X}", m.read_32(addr));
                            }
                            log::info!(
                                "svc::WaitProcessWideKeyAtomic stack tid={} sp=0x{:08X}:{}",
                                current_thread_id,
                                sp,
                                stack
                            );
                            // Also dump 20 ARM32 insns around LR for caller context.
                            let start = ctx.lr.saturating_sub(4 * 4);
                            let mut disasm = String::new();
                            for i in 0..20u64 {
                                use std::fmt::Write;
                                let addr = start + i * 4;
                                let word = m.read_32(addr);
                                let _ = write!(disasm, " {:08X}:{:08X}", addr as u32, word);
                            }
                            log::info!(
                                "svc::WaitProcessWideKeyAtomic insns tid={} lr=0x{:08X}:{}",
                                current_thread_id,
                                ctx.lr,
                                disasm
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

/// Repeating CV-state dumper.
///
/// `RUZU_DUMP_CV=0xCVKEY` (single value or comma-separated list of cv_keys)
/// enables this. Fires on EVERY WaitProcessWideKeyAtomic and EVERY
/// SignalProcessWideKey whose cv_key matches one of the watched keys.
///
/// For each fire, logs:
///   - kind (WAIT or SIGNAL)
///   - tid + core
///   - mutex_addr value (32-bit word at mutex_addr)
///   - cv_key value (32-bit word at cv_key)
///   - 16 words of context starting at mutex_addr (to spot predicate fields
///     near the mutex/cv pair)
///   - extra arg (tag for WAIT, count for SIGNAL)
///   - PC/LR/SP from the current thread's ARM context (so we can correlate
///     with the guest stack trace)
///
/// Used to diagnose lost-wakeup races: order WAIT/SIGNAL events and observe
/// whether the predicate near the mutex is set when WAIT enters.
fn dump_cv_state(
    system: &System,
    kind: &'static str,
    mutex_addr: u64,
    cv_key: u64,
    extra: Option<u32>,
    timeout_ns: Option<i64>,
) {
    use std::sync::OnceLock;
    static TARGETS: OnceLock<Vec<u64>> = OnceLock::new();
    let targets = TARGETS.get_or_init(|| {
        std::env::var("RUZU_DUMP_CV")
            .ok()
            .map(|raw| {
                raw.split(',')
                    .map(|s| s.trim())
                    .filter(|s| !s.is_empty())
                    .filter_map(|tok| u64::from_str_radix(tok.trim_start_matches("0x"), 16).ok())
                    .collect()
            })
            .unwrap_or_default()
    });
    if targets.is_empty() || !targets.contains(&cv_key) {
        return;
    }

    let tid = system.current_thread_id().unwrap_or(0);
    let core = system
        .current_thread()
        .and_then(|t| Some(t.lock().ok()?.get_current_core().max(0) as usize))
        .unwrap_or(usize::MAX);

    let process = system.current_process_arc();
    let process = process.lock().unwrap();

    // PC/LR/SP for guest backtrace correlation.
    let (pc, lr, sp) = if core < crate::hardware_properties::NUM_CPU_CORES as usize {
        if let Some(cpu) = process.get_arm_interface(core) {
            let mut ctx = crate::arm::arm_interface::ThreadContext::default();
            cpu.get_context(&mut ctx);
            (ctx.pc, ctx.lr, ctx.sp)
        } else {
            (0, 0, 0)
        }
    } else {
        (0, 0, 0)
    };

    let mutex_val: u32;
    let cv_val: u32;
    let mut ctx_words: [u32; 16] = [0; 16];
    if let Some(memory) = process.page_table.get_base().m_memory.as_ref() {
        let m = memory.lock().unwrap();
        mutex_val = m.read_32(mutex_addr);
        cv_val = m.read_32(cv_key);
        for i in 0..16u64 {
            ctx_words[i as usize] = m.read_32(mutex_addr.wrapping_add(i * 4));
        }
    } else {
        mutex_val = 0;
        cv_val = 0;
    }

    let extra_str = match (extra, timeout_ns) {
        (Some(v), Some(t)) => format!(" tag=0x{:08X} timeout_ns={}", v, t),
        (Some(v), None) => format!(" count={}", v as i32),
        _ => String::new(),
    };

    log::info!(
        "[DUMP_CV] {} tid={} core={} cv_key=0x{:X} mutex_addr=0x{:X} mutex_val=0x{:08X} cv_val=0x{:08X}{} pc=0x{:08X} lr=0x{:08X} sp=0x{:08X}",
        kind, tid, core, cv_key, mutex_addr, mutex_val, cv_val, extra_str, pc, lr, sp
    );
    log::info!(
        "[DUMP_CV] {} tid={} ctx@0x{:X}: {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X}",
        kind, tid, mutex_addr,
        ctx_words[0], ctx_words[1], ctx_words[2], ctx_words[3],
        ctx_words[4], ctx_words[5], ctx_words[6], ctx_words[7],
        ctx_words[8], ctx_words[9], ctx_words[10], ctx_words[11],
        ctx_words[12], ctx_words[13], ctx_words[14], ctx_words[15],
    );

    // Wider mutex-region dump: 32 words BEFORE mutex_addr (catches the
    // surrounding parent struct), and the code AT the calling PC
    // (4 ARM32 instructions = 16 bytes, around the predicate check).
    if let Some(memory) = process.page_table.get_base().m_memory.as_ref() {
        let m = memory.lock().unwrap();
        let region_base = mutex_addr.wrapping_sub(128);
        let mut region_words: [u32; 32] = [0; 32];
        for i in 0..32u64 {
            region_words[i as usize] = m.read_32(region_base.wrapping_add(i * 4));
        }
        log::info!(
            "[DUMP_CV] {} tid={} region@0x{:X}: {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X}",
            kind, tid, region_base,
            region_words[0], region_words[1], region_words[2], region_words[3],
            region_words[4], region_words[5], region_words[6], region_words[7],
            region_words[8], region_words[9], region_words[10], region_words[11],
            region_words[12], region_words[13], region_words[14], region_words[15],
        );
        log::info!(
            "[DUMP_CV] {} tid={} region@0x{:X}+64: {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X}",
            kind, tid, region_base.wrapping_add(64),
            region_words[16], region_words[17], region_words[18], region_words[19],
            region_words[20], region_words[21], region_words[22], region_words[23],
            region_words[24], region_words[25], region_words[26], region_words[27],
            region_words[28], region_words[29], region_words[30], region_words[31],
        );

        // Dump 8 ARM32 instructions around the guest PC (the svc wrapper).
        let code_base = (pc as u64).wrapping_sub(28);
        let mut insns: [u32; 8] = [0; 8];
        for i in 0..8u64 {
            insns[i as usize] = m.read_32(code_base.wrapping_add(i * 4));
        }
        log::info!(
            "[DUMP_CV] {} tid={} insns@0x{:X} (PC=0x{:X}): {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X}",
            kind, tid, code_base, pc,
            insns[0], insns[1], insns[2], insns[3],
            insns[4], insns[5], insns[6], insns[7],
        );

        // Dump 16 ARM32 instructions BEFORE the caller LR — that's where
        // the predicate check and the marker write live (the svc wrapper
        // itself is a thin <4-instruction stub).
        let caller_base = (lr as u64).wrapping_sub(60);
        let mut caller_insns: [u32; 16] = [0; 16];
        for i in 0..16u64 {
            caller_insns[i as usize] = m.read_32(caller_base.wrapping_add(i * 4));
        }
        log::info!(
            "[DUMP_CV] {} tid={} caller_insns@0x{:X} (LR=0x{:X}): {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X}",
            kind, tid, caller_base, lr,
            caller_insns[0], caller_insns[1], caller_insns[2], caller_insns[3],
            caller_insns[4], caller_insns[5], caller_insns[6], caller_insns[7],
            caller_insns[8], caller_insns[9], caller_insns[10], caller_insns[11],
            caller_insns[12], caller_insns[13], caller_insns[14], caller_insns[15],
        );
    }

    // Stack dump: the caller's register-spilled predicate locals
    // live on its stack. 32 words above SP catches the recent
    // frame's locals.
    if let Some(memory) = process.page_table.get_base().m_memory.as_ref() {
        let m = memory.lock().unwrap();
        let mut stack_words: [u32; 32] = [0; 32];
        for i in 0..32u64 {
            stack_words[i as usize] = m.read_32(sp.wrapping_add(i * 4) as u64);
        }
        log::info!(
            "[DUMP_CV] {} tid={} stack@SP+0..128: {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X}",
            kind, tid,
            stack_words[0], stack_words[1], stack_words[2], stack_words[3],
            stack_words[4], stack_words[5], stack_words[6], stack_words[7],
            stack_words[8], stack_words[9], stack_words[10], stack_words[11],
            stack_words[12], stack_words[13], stack_words[14], stack_words[15],
        );
        log::info!(
            "[DUMP_CV] {} tid={} stack@SP+64..128: {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X}",
            kind, tid,
            stack_words[16], stack_words[17], stack_words[18], stack_words[19],
            stack_words[20], stack_words[21], stack_words[22], stack_words[23],
            stack_words[24], stack_words[25], stack_words[26], stack_words[27],
            stack_words[28], stack_words[29], stack_words[30], stack_words[31],
        );
    }

    // Optional fixed code-window dump: `RUZU_DUMP_CV_CODE=0xADDR:N` reads
    // N ARM32 instructions starting at ADDR. Used to extract the function
    // body around return addresses found in the stack chain (e.g. the
    // wait-routine the application is blocked in). Comma-separated for
    // multiple ranges: `RUZU_DUMP_CV_CODE=0x758280:80,0x9A6800:32`.
    {
        use std::sync::OnceLock;
        static CODE_SPECS: OnceLock<Vec<(u64, u64)>> = OnceLock::new();
        let specs = CODE_SPECS.get_or_init(|| {
            std::env::var("RUZU_DUMP_CV_CODE")
                .ok()
                .map(|raw| {
                    raw.split(',')
                        .map(|s| s.trim())
                        .filter(|s| !s.is_empty())
                        .filter_map(|tok| {
                            let (a, n) = tok.split_once(':')?;
                            let addr =
                                u64::from_str_radix(a.trim_start_matches("0x"), 16).ok()?;
                            let len = n.parse::<u64>().ok()?;
                            Some((addr, len))
                        })
                        .collect()
                })
                .unwrap_or_default()
        });
        if !specs.is_empty() {
            if let Some(memory) = process.page_table.get_base().m_memory.as_ref() {
                let m = memory.lock().unwrap();
                for &(addr, n) in specs {
                    // Print 8 insns per line.
                    let mut i = 0u64;
                    while i < n {
                        let chunk_n = (n - i).min(8);
                        let mut line = format!(
                            "[DUMP_CV] {} tid={} code@0x{:08X}:",
                            kind,
                            tid,
                            addr.wrapping_add(i * 4)
                        );
                        for j in 0..chunk_n {
                            let w = m.read_32(addr.wrapping_add((i + j) * 4));
                            use std::fmt::Write;
                            let _ = write!(line, " {:08X}", w);
                        }
                        log::info!("{}", line);
                        i += chunk_n;
                    }
                }
            }
        }
    }

    // Stack-walked call chain: scan SP..SP+128 for words that look like
    // code-region return addresses (typical guest code lives below
    // 0x02000000 for MK8D's ARM32 image). For each such candidate dump
    // 8 ARM32 instructions ending at the candidate — the BL whose
    // return-address that is, plus a few prior instructions. Gives a
    // multi-frame call trace at the wedge moment without symbolic info.
    if let Some(memory) = process.page_table.get_base().m_memory.as_ref() {
        let m = memory.lock().unwrap();
        let mut shown: Vec<u32> = Vec::with_capacity(16);
        for slot in 0..32u64 {
            let word = m.read_32(sp.wrapping_add(slot * 4) as u64);
            // Heuristic: ARM32 code addresses in MK8D's image are in the
            // 0x00100000..0x02000000 range. Word-aligned (low 2 bits 0).
            if word < 0x0010_0000 || word >= 0x0200_0000 || word & 0x3 != 0 {
                continue;
            }
            if shown.contains(&word) {
                continue;
            }
            shown.push(word);
            // Dump 8 ARM32 instructions: 6 before the return address
            // (which contains the BL) + the return address + 1 after.
            let base = (word as u64).wrapping_sub(24);
            let mut insns: [u32; 8] = [0; 8];
            for i in 0..8u64 {
                insns[i as usize] = m.read_32(base.wrapping_add(i * 4));
            }
            log::info!(
                "[DUMP_CV] {} tid={} chain@SP+{:03} ret=0x{:08X}: {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X}",
                kind, tid, slot * 4, word,
                insns[0], insns[1], insns[2], insns[3],
                insns[4], insns[5], insns[6], insns[7],
            );
        }
    }

    // Chase inline pointers. Any ctx word that looks like a heap/stack
    // pointer (high bit set in the upper byte, or in known guest VA
    // ranges) gets a 16-word dereference dump. This catches the
    // predicate when it lives in a struct pointed-to from the
    // cv/mutex region.
    if let Some(memory) = process.page_table.get_base().m_memory.as_ref() {
        let m = memory.lock().unwrap();
        let mut seen: Vec<u32> = Vec::with_capacity(8);
        for (i, &w) in ctx_words.iter().enumerate() {
            // Heuristic: guest pointers are 32-bit non-zero values in
            // the 0x01000000..0xFF000000 range (typical NRO/heap/stack
            // mappings for MK8D's ARM32 address space). Skip small
            // counters, tags, and obvious non-pointers.
            if w < 0x0010_0000 || w == 0xFFFF_FFFF {
                continue;
            }
            if seen.contains(&w) {
                continue;
            }
            seen.push(w);
            let mut sub: [u32; 16] = [0; 16];
            for j in 0..16u64 {
                sub[j as usize] = m.read_32((w as u64).wrapping_add(j * 4));
            }
            log::info!(
                "[DUMP_CV] {} tid={} *(+{:02})=0x{:08X}: {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X}",
                kind, tid, i * 4, w,
                sub[0], sub[1], sub[2], sub[3],
                sub[4], sub[5], sub[6], sub[7],
                sub[8], sub[9], sub[10], sub[11],
                sub[12], sub[13], sub[14], sub[15],
            );
        }
    }
}

/// One-shot memory dump triggered at first WaitProcessWideKeyAtomic where
/// the cv_key matches `RUZU_DUMP_MEM_AT_WAIT_CV=0xCV_KEY`.
/// `RUZU_DUMP_MEM_AT_FIRST_SIGNAL=0xADDR:LEN[,0xADDR2:LEN2,...]` reads LEN
/// bytes starting at each ADDR via the canonical memory backing (independent
/// of fastmem) and prints as hex grouped in 16-byte rows.
fn maybe_dump_mem_at_wait(system: &System, cv_key: u64) {
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::sync::OnceLock;
    static FIRED: AtomicBool = AtomicBool::new(false);
    static TARGET_CV: OnceLock<Option<u64>> = OnceLock::new();
    let target = *TARGET_CV.get_or_init(|| {
        std::env::var("RUZU_DUMP_MEM_AT_WAIT_CV")
            .ok()
            .and_then(|s| u64::from_str_radix(s.trim_start_matches("0x"), 16).ok())
    });
    let Some(t) = target else {
        return;
    };
    if cv_key != t {
        return;
    }
    if FIRED.swap(true, Ordering::SeqCst) {
        return;
    }
    static SPECS: OnceLock<Vec<(u64, u64)>> = OnceLock::new();
    let specs = SPECS.get_or_init(|| {
        let raw = std::env::var("RUZU_DUMP_MEM_AT_FIRST_SIGNAL").unwrap_or_default();
        let mut out = Vec::new();
        for tok in raw.split(',').map(|s| s.trim()).filter(|s| !s.is_empty()) {
            if let Some((a, l)) = tok.split_once(':') {
                if let (Ok(addr), Ok(len)) = (
                    u64::from_str_radix(a.trim_start_matches("0x"), 16),
                    u64::from_str_radix(l.trim_start_matches("0x"), 16)
                        .or_else(|_| l.parse::<u64>()),
                ) {
                    out.push((addr, len));
                }
            }
        }
        out
    });
    let process = system.current_process_arc();
    let process = process.lock().unwrap();
    let Some(memory) = process.page_table.get_base().m_memory.as_ref() else {
        return;
    };
    let m = memory.lock().unwrap();
    eprintln!(
        "[MEM_DUMP] triggered at WaitProcessWideKeyAtomic(cv_key=0x{:X})",
        t
    );
    for &(start, len) in specs {
        eprintln!("[MEM_DUMP] addr=0x{:08X} len={}", start, len);
        let mut row = String::new();
        for i in 0..len {
            use std::fmt::Write;
            if i % 16 == 0 {
                if !row.is_empty() {
                    eprintln!("  0x{:08X}: {}", start + i - 16, row);
                    row.clear();
                }
            }
            let _ = write!(row, "{:02x} ", m.read_8(start + i));
        }
        if !row.is_empty() {
            let last_addr = start + len - (len % 16).max(1);
            eprintln!("  0x{:08X}: {}", last_addr, row);
        }
    }
}

fn maybe_dump_mem_at_first_signal(system: &System) {
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::sync::OnceLock;
    static FIRED: AtomicBool = AtomicBool::new(false);
    static SPECS: OnceLock<Vec<(u64, u64)>> = OnceLock::new();
    let specs = SPECS.get_or_init(|| {
        let raw = std::env::var("RUZU_DUMP_MEM_AT_FIRST_SIGNAL").unwrap_or_default();
        let mut out = Vec::new();
        for tok in raw.split(',').map(|s| s.trim()).filter(|s| !s.is_empty()) {
            if let Some((a, l)) = tok.split_once(':') {
                if let (Ok(addr), Ok(len)) = (
                    u64::from_str_radix(a.trim_start_matches("0x"), 16),
                    u64::from_str_radix(l.trim_start_matches("0x"), 16)
                        .or_else(|_| l.parse::<u64>()),
                ) {
                    out.push((addr, len));
                }
            }
        }
        out
    });
    if specs.is_empty() {
        return;
    }
    if FIRED.swap(true, Ordering::SeqCst) {
        return;
    }
    let process = system.current_process_arc();
    let process = process.lock().unwrap();
    let Some(memory) = process.page_table.get_base().m_memory.as_ref() else {
        return;
    };
    let m = memory.lock().unwrap();
    for &(start, len) in specs {
        eprintln!("[MEM_DUMP] addr=0x{:08X} len={}", start, len);
        let mut row = String::new();
        for i in 0..len {
            use std::fmt::Write;
            if i % 16 == 0 {
                if !row.is_empty() {
                    eprintln!("  0x{:08X}: {}", start + i - 16, row);
                    row.clear();
                }
            }
            let _ = write!(row, "{:02x} ", m.read_8(start + i));
        }
        if !row.is_empty() {
            let last_addr = start + len - (len % 16).max(1);
            eprintln!("  0x{:08X}: {}", last_addr, row);
        }
    }
}

/// Signal process wide key.
pub fn signal_process_wide_key(system: &System, cv_key: u64, count: i32) {
    maybe_scan_word_at_signal(system);
    maybe_dump_mem_at_first_signal(system);
    // Inverse mapping: SignalProcessWideKey only knows cv_key, not the
    // associated mutex address — pass `cv_key.wrapping_sub(4)` as the
    // probed mutex location, since libnx/nnSdk layout puts the mutex
    // immediately before the cv_key in memory. dump_cv_state itself
    // bounds-checks so a wrong guess just produces zeros (no crash).
    dump_cv_state(
        system,
        "SIGNAL",
        cv_key.wrapping_sub(4),
        cv_key,
        Some(count as u32),
        None,
    );
    if should_trace_cv_debug() {
        log::info!(
            "svc::SignalProcessWideKey tid={:?} cv_key=0x{:X} count={}",
            system.current_thread_id(),
            cv_key,
            count
        );
        if let Some(current_thread_id) = system.current_thread_id() {
            if should_trace_cv_backtrace_once(current_thread_id)
                || should_trace_cv_backtrace_for_signal(current_thread_id, cv_key)
            {
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
                                current_thread_id,
                                ctx.lr,
                                disasm
                            );
                            // Dump 48 stack words from SP — used to manually
                            // walk return addresses past the bt unwinder's
                            // 2-frame limit. Each word is 4 bytes; we want
                            // to see candidate LRs for the actual game-code
                            // caller of the libnn signal wrapper.
                            let sp = ctx.sp;
                            let mut stack = String::new();
                            for offset in 0..48u64 {
                                let addr = sp.saturating_add(offset * 4);
                                let word = m.read_32(addr);
                                stack.push_str(&format!(" {:08X}", word));
                            }
                            log::info!(
                                "svc::SignalProcessWideKey stack tid={} sp=0x{:08X}:{}",
                                current_thread_id,
                                sp,
                                stack
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
