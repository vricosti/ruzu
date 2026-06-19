//! Port of zuyu/src/core/hle/kernel/svc/svc_exception.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-20
//!
//! SVC handlers for Break and ReturnFromException.

use crate::core::System;
use crate::hle::kernel::svc::svc_results::*;
use crate::hle::kernel::svc::svc_types::*;
use crate::hle::result::ResultCode;

/// Break program execution.
///
/// Upstream logs the break reason, handles debug buffers, saves a break report,
/// and optionally notifies the debugger.
pub fn break_execution(system: &System, reason: u32, info1: u64, info2: u64) {
    let break_reason_raw = reason & !0x80000000u32;
    let notification_only = (reason & 0x80000000) != 0;

    let mut has_dumped_buffer = false;

    // Upstream: handle_debug_buffer reads from guest memory and logs the contents.
    let handle_debug_buffer = |addr: u64, sz: u64, dumped: &mut bool| {
        if sz == 0 || addr == 0 || *dumped {
            return;
        }

        if let Some(memory) = system.get_svc_memory() {
            let m = memory.lock().unwrap();
            if sz == 4 {
                // Typically an error code.
                let err_code = m.read_32(addr);
                log::error!("debug_buffer_err_code={:X}", err_code);
            } else {
                // Hexdump the buffer.
                let sz = sz as usize;
                let mut hexdump = String::new();
                for i in 0..sz {
                    let byte = m.read_8(addr + i as u64);
                    hexdump.push_str(&format!("{:02X} ", byte));
                    if i != 0 && i % 16 == 0 {
                        hexdump.push('\n');
                    }
                }
                log::error!("debug_buffer=\n{}", hexdump);
            }
        } else {
            let mem = system.shared_process_memory().read().unwrap();
            if sz == 4 {
                let err_code = mem.read_32(addr);
                log::error!("debug_buffer_err_code={:X}", err_code);
            } else {
                let sz = sz as usize;
                let mut hexdump = String::new();
                for i in 0..sz {
                    let byte = mem.read_8(addr + i as u64);
                    hexdump.push_str(&format!("{:02X} ", byte));
                    if i != 0 && i % 16 == 0 {
                        hexdump.push('\n');
                    }
                }
                log::error!("debug_buffer=\n{}", hexdump);
            }
        }
        *dumped = true;
    };

    // Match upstream break reason handling.
    match break_reason_raw {
        0 => {
            // BreakReason::Panic
            log::error!(
                "Userspace PANIC! info1=0x{:016X}, info2=0x{:016X}",
                info1,
                info2
            );
            handle_debug_buffer(info1, info2, &mut has_dumped_buffer);
        }
        1 => {
            // BreakReason::Assert
            log::error!(
                "Userspace Assertion failed! info1=0x{:016X}, info2=0x{:016X}",
                info1,
                info2
            );
            handle_debug_buffer(info1, info2, &mut has_dumped_buffer);
        }
        2 => {
            // BreakReason::User
            log::warn!(
                "Userspace Break! 0x{:016X} with size 0x{:016X}",
                info1,
                info2
            );
            handle_debug_buffer(info1, info2, &mut has_dumped_buffer);
        }
        3 => {
            // BreakReason::PreLoadDll
            log::info!(
                "Userspace Attempting to load an NRO at 0x{:016X} with size 0x{:016X}",
                info1,
                info2
            );
        }
        4 => {
            // BreakReason::PostLoadDll
            log::info!(
                "Userspace Loaded an NRO at 0x{:016X} with size 0x{:016X}",
                info1,
                info2
            );
        }
        5 => {
            // BreakReason::PreUnloadDll
            log::info!(
                "Userspace Attempting to unload an NRO at 0x{:016X} with size 0x{:016X}",
                info1,
                info2
            );
        }
        6 => {
            // BreakReason::PostUnloadDll
            log::info!(
                "Userspace Unloaded an NRO at 0x{:016X} with size 0x{:016X}",
                info1,
                info2
            );
        }
        7 => {
            // BreakReason::CppException
            log::error!("Signalling debugger. Uncaught C++ exception encountered.");
        }
        _ => {
            log::warn!(
                "Signalling debugger, Unknown break reason {:#X}, info1=0x{:016X}, info2=0x{:016X}",
                reason,
                info1,
                info2
            );
            handle_debug_buffer(info1, info2, &mut has_dumped_buffer);
        }
    }

    // Upstream: system.GetReporter().SaveSvcBreakReport(...)
    // Reporter not yet ported — when available, call:
    // system.reporter().save_svc_break_report(reason, notification_only, info1, info2, debug_buffer);

    if !notification_only {
        log::error!(
            "Emulated program broke execution! reason=0x{:016X}, info1=0x{:016X}, info2=0x{:016X}",
            reason as u64,
            info1,
            info2
        );

        handle_debug_buffer(info1, info2, &mut has_dumped_buffer);

        if let Some(kernel) = system.kernel() {
            kernel.current_physical_core().log_backtrace();
        }
    }

    // Upstream: Debugger notification.
    // const bool is_hbl = GetCurrentProcess(kernel).IsHbl();
    // const bool should_break = is_hbl || !notification_only;
    // if (system.DebuggerEnabled() && should_break) {
    //     auto* thread = system.Kernel().GetCurrentEmuThread();
    //     system.GetDebugger().NotifyThreadStopped(thread);
    //     thread->RequestSuspend(SuspendType::Debug);
    // }
    // Debugger not yet ported — when available, wire up debugger notification here.
}

/// Upstream `Break64From32` forwards to `Break`, but the Rust AArch32 dispatch
/// can also pass the captured guest argument registers for optional diagnostics.
pub fn break64_from_32(system: &System, reason: u32, arg: u32, size: u32, args: &[u64]) {
    dump_a32_break_context(system, arg as u64, size as u64, args);
    break_execution(system, reason, arg as u64, size as u64);
}

/// Upstream `Break64` wrapper.
pub fn break64(system: &System, reason: u32, arg: u64, size: u64) {
    dump_a64_break_context(system, arg, size);
    log::error!(
        "!!! svcBreak(reason={:#x}, info1={:#x}, info2={:#x}) - GAME ABORTED !!!",
        reason,
        arg,
        size
    );
    break_execution(system, reason, arg, size);
}

fn dump_a64_break_context(system: &System, info1: u64, info2: u64) {
    if std::env::var_os("RUZU_DUMP_BREAK_STACK").is_none() {
        return;
    }

    let Some(kernel) = system.kernel() else {
        return;
    };
    let core_index = kernel.current_physical_core_index() as usize;
    let Some(process_arc) = system.current_process_arc.as_ref().cloned() else {
        return;
    };
    let process = process_arc.lock().unwrap();
    let Some(jit) = process.get_arm_interface(core_index) else {
        return;
    };

    let mut ctx = crate::arm::arm_interface::ThreadContext::default();
    jit.get_context(&mut ctx);
    log::error!("=== A64 GUEST REGISTER DUMP AT BREAK ===");
    log::error!(
        "  break_pc=0x{:016X} break_lr=0x{:016X} break_sp=0x{:016X} break_fp=0x{:016X} core={}",
        ctx.pc,
        ctx.lr,
        ctx.sp,
        ctx.fp,
        core_index
    );
    for index in 0..8 {
        log::error!("  x{:2} = 0x{:016X}", index, ctx.r[index]);
    }
    for index in 19..=28 {
        log::error!("  x{:2} = 0x{:016X}", index, ctx.r[index]);
    }

    let backtrace = crate::arm::debug::get_backtrace_from_context(&process, &ctx);
    for (index, entry) in backtrace.iter().enumerate().take(32) {
        log::error!(
            "  break_bt[{}] addr=0x{:016X} orig=0x{:016X} module={} name={} offset=0x{:X}",
            index,
            entry.address,
            entry.original_address,
            entry.module,
            entry.name,
            entry.offset,
        );
    }

    if info1 != 0 && info2 > 0 && info2 < 0x200 {
        let len = info2 as usize;
        let memory = process.get_shared_memory();
        let mem = memory.read().unwrap();
        if mem.is_valid_range(info1, len) {
            let mut hexdump = String::new();
            for index in 0..len {
                let byte = mem.read_8(info1 + index as u64);
                hexdump.push_str(&format!("{:02X} ", byte));
            }
            log::error!("  break_debug_buffer={}", hexdump.trim_end());
        }
    }

    dump_a64_stack_scan(&process, ctx.sp);
}

fn dump_a64_stack_scan(process: &crate::hle::kernel::k_process::KProcess, sp: u64) {
    const STACK_SCAN_BYTES: usize = 0x400;
    const STACK_DUMP_BYTES: usize = 0x100;

    let memory = process.get_shared_memory();
    let mem = memory.read().unwrap();
    if !mem.is_valid_range(sp, 8) {
        log::error!("  break_stack: sp=0x{:016X} is not mapped", sp);
        return;
    }

    let dump_len = (0..STACK_DUMP_BYTES)
        .step_by(8)
        .take_while(|offset| mem.is_valid_range(sp + *offset as u64, 8))
        .count()
        * 8;
    log::error!(
        "  break_stack_qwords sp=0x{:016X} dump_len=0x{:X}",
        sp,
        dump_len
    );
    for offset in (0..dump_len).step_by(0x20) {
        let mut values = [0u64; 4];
        for (index, value) in values.iter_mut().enumerate() {
            let addr = sp + offset as u64 + (index as u64 * 8);
            if mem.is_valid_range(addr, 8) {
                *value = mem.read_64(addr);
            }
        }
        log::error!(
            "  stack[+0x{:03X}] {:016X} {:016X} {:016X} {:016X}",
            offset,
            values[0],
            values[1],
            values[2],
            values[3]
        );
    }

    for offset in (0..STACK_SCAN_BYTES).step_by(8) {
        let addr = sp + offset as u64;
        if !mem.is_valid_range(addr, 8) {
            break;
        }
        let value = mem.read_64(addr);
        if looks_like_animus_code_address(value) {
            log::error!(
                "  stack_code_candidate sp+0x{:03X}=0x{:016X}",
                offset,
                value
            );
        }
    }
}

fn looks_like_animus_code_address(value: u64) -> bool {
    // ANIMUS' executable NSOs observed in this investigation are mapped in
    // the 0x8000_0000..0x8520_0000 region. Keep the check deliberately broad
    // so this diagnostic still works across ASLR-disabled local runs.
    (0x8000_0000..0x8600_0000).contains(&value)
}

fn dump_a32_break_context(system: &System, info1: u64, info2: u64, args: &[u64]) {
    log::error!("=== GUEST REGISTER DUMP AT BREAK ===");
    for (index, value) in args.iter().enumerate() {
        log::error!("  r{:2} = {:#010x}", index, value);
    }

    if let Some(kernel) = system.kernel() {
        let core_index = kernel.current_physical_core_index() as usize;
        if let Some(process_arc) = system.current_process_arc.as_ref().cloned() {
            let process = process_arc.lock().unwrap();
            if let Some(jit) = process.get_arm_interface(core_index) {
                let mut ctx = crate::arm::arm_interface::ThreadContext::default();
                jit.get_context(&mut ctx);
                log::error!(
                    "  break_pc=0x{:08X} break_lr=0x{:08X} break_sp=0x{:08X} core={}",
                    ctx.r[15] as u32,
                    ctx.r[14] as u32,
                    ctx.r[13] as u32,
                    core_index
                );
                if std::env::var_os("RUZU_DUMP_BREAK_STACK").is_some() {
                    let backtrace = crate::arm::debug::get_backtrace_from_context(&process, &ctx);
                    for (index, entry) in backtrace.iter().enumerate().take(32) {
                        log::error!(
                            "  break_bt[{}] addr=0x{:08X} orig=0x{:08X} module={} name={} offset=0x{:X}",
                            index,
                            entry.address as u32,
                            entry.original_address as u32,
                            entry.module,
                            entry.name,
                            entry.offset,
                        );
                    }
                }
            }
        }
    }

    if info1 != 0 && info2 > 0 && info2 < 0x200 {
        let len = info2 as usize;
        let mut buf = vec![0u8; len];
        if let Some(memory) = system.get_svc_memory() {
            let m = memory.lock().unwrap();
            m.read_block(info1, &mut buf);
        } else {
            let process = system.current_process_arc().lock().unwrap();
            let mem = process.process_memory.read().unwrap();
            if mem.is_valid_range(info1, len) {
                for (index, byte) in buf.iter_mut().enumerate() {
                    *byte = mem.read_8(info1 + index as u64);
                }
            }
        }
        if let Ok(msg) = String::from_utf8(buf) {
            log::error!("  Break message: {}", msg.trim_end_matches('\0'));
        }
    }

    if std::env::var_os("RUZU_DUMP_BREAK_STACK").is_some() {
        dump_known_break_strings(system);
    }

    log::error!(
        "!!! svcBreak(reason={:#x}, info1={:#x}, info2={:#x}) - GAME ABORTED !!!",
        args.first().copied().unwrap_or_default() as u32,
        info1,
        info2
    );
}

fn dump_known_break_strings(system: &System) {
    let Some(memory) = system.get_svc_memory() else {
        return;
    };
    let m = memory.lock().unwrap();
    for addr in [0x20bc7fau64, 0x20bc827, 0x2244ec7] {
        let mut buf = vec![0u8; 128];
        for index in 0..128u64 {
            buf[index as usize] = m.read_8(addr + index);
        }
        if let Some(end) = buf.iter().position(|&byte| byte == 0) {
            buf.truncate(end);
        }
        if let Ok(value) = String::from_utf8(buf) {
            if !value.is_empty() && value.len() < 120 {
                log::error!("  [{addr:#x}] = \"{value}\"");
            }
        }
    }
}

/// Return from exception.
/// Upstream: UNIMPLEMENTED() — intentionally unimplemented.
pub fn return_from_exception(_result: ResultCode) {
    log::warn!("svc::ReturnFromException: Upstream UNIMPLEMENTED");
}
