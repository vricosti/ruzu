// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/arm/dynarmic/arm_dynarmic_64.h and arm_dynarmic_64.cpp
//! ARM64 dynarmic backend.

use std::sync::atomic::{AtomicU32, AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

use crate::arm::arm_interface::{
    Architecture, ArmInterface, ArmInterfaceBase, DebugWatchpoint, HaltReason, KProcess, KThread,
    ThreadContext,
};
use crate::hle::kernel::k_process::SharedProcessMemory;
use crate::memory::memory::Memory;

use rdynarmic::backend::x64::jit_state::A64JitState;
use rdynarmic::jit_config::{JitConfig, OptimizationFlag, UserCallbacks};

fn optimization_flags_from_mask(mask: u32) -> OptimizationFlag {
    let mut flags = OptimizationFlag::NO_OPTIMIZATIONS;

    if mask & OptimizationFlag::BLOCK_LINKING.bits() != 0 {
        flags |= OptimizationFlag::BLOCK_LINKING;
    }
    if mask & OptimizationFlag::RETURN_STACK_BUFFER.bits() != 0 {
        flags |= OptimizationFlag::RETURN_STACK_BUFFER;
    }
    if mask & OptimizationFlag::FAST_DISPATCH.bits() != 0 {
        flags |= OptimizationFlag::FAST_DISPATCH;
    }
    if mask & OptimizationFlag::GET_SET_ELIMINATION.bits() != 0 {
        flags |= OptimizationFlag::GET_SET_ELIMINATION;
    }
    if mask & OptimizationFlag::CONST_PROP.bits() != 0 {
        flags |= OptimizationFlag::CONST_PROP;
    }
    if mask & OptimizationFlag::MISC_IR_OPT.bits() != 0 {
        flags |= OptimizationFlag::MISC_IR_OPT;
    }
    if mask & OptimizationFlag::UNSAFE_UNFUSE_FMA.bits() != 0 {
        flags |= OptimizationFlag::UNSAFE_UNFUSE_FMA;
    }
    if mask & OptimizationFlag::UNSAFE_REDUCED_ERROR_FP.bits() != 0 {
        flags |= OptimizationFlag::UNSAFE_REDUCED_ERROR_FP;
    }
    if mask & OptimizationFlag::UNSAFE_INACCURATE_NAN.bits() != 0 {
        flags |= OptimizationFlag::UNSAFE_INACCURATE_NAN;
    }
    if mask & OptimizationFlag::UNSAFE_IGNORE_STANDARD_FPCR_VALUE.bits() != 0 {
        flags |= OptimizationFlag::UNSAFE_IGNORE_STANDARD_FPCR_VALUE;
    }
    if mask & OptimizationFlag::UNSAFE_IGNORE_GLOBAL_MONITOR.bits() != 0 {
        flags |= OptimizationFlag::UNSAFE_IGNORE_GLOBAL_MONITOR;
    }

    flags
}

fn parse_optimization_mask_env(name: &str) -> Option<u32> {
    std::env::var(name).ok().and_then(|value| {
        let trimmed = value.trim();
        let digits = trimmed
            .strip_prefix("0x")
            .or_else(|| trimmed.strip_prefix("0X"))
            .unwrap_or(trimmed);
        u32::from_str_radix(digits, 16)
            .ok()
            .or_else(|| trimmed.parse::<u32>().ok())
    })
}

fn parse_watch_ranges(raw: &str) -> Vec<(u64, u64)> {
    let mut out = Vec::new();
    for tok in raw.split(',').map(|s| s.trim()).filter(|s| !s.is_empty()) {
        let (addr_s, size) = match tok.split_once(':') {
            Some((a, s)) => (a, s.parse::<u64>().unwrap_or(8)),
            None => (tok, 8u64),
        };
        let addr = if let Some(stripped) = addr_s
            .strip_prefix("0x")
            .or_else(|| addr_s.strip_prefix("0X"))
        {
            u64::from_str_radix(stripped, 16).unwrap_or(0)
        } else {
            addr_s.parse::<u64>().unwrap_or(0)
        };
        if addr != 0 {
            out.push((addr, addr.saturating_add(size)));
        }
    }
    out
}

fn watched_ranges_64() -> &'static [(u64, u64)] {
    use std::sync::OnceLock;
    static RANGES: OnceLock<Vec<(u64, u64)>> = OnceLock::new();
    RANGES.get_or_init(|| {
        let raw = std::env::var("RUZU_WATCH_ADDR").unwrap_or_default();
        parse_watch_ranges(&raw)
    })
}

fn parse_u64_env(name: &str) -> Option<u64> {
    std::env::var(name).ok().and_then(|value| {
        let trimmed = value.trim();
        let digits = trimmed
            .strip_prefix("0x")
            .or_else(|| trimmed.strip_prefix("0X"))
            .unwrap_or(trimmed);
        u64::from_str_radix(digits, 16)
            .ok()
            .or_else(|| trimmed.parse::<u64>().ok())
    })
}

fn trace_a64_access_pc() -> Option<u64> {
    use std::sync::OnceLock;
    static PC: OnceLock<Option<u64>> = OnceLock::new();
    *PC.get_or_init(|| parse_u64_env("RUZU_TRACE_A64_ACCESS_PC"))
}

/// Parse `RUZU_DUMP_VEC_AT_PC=PC[,PC,...]` (hex). On every memory read,
/// if the JIT PC matches one of the listed PCs we dump V17/V18 (full
/// 128-bit) plus X3/X5 to stderr. Used to inspect strchr-style
/// vectorized scan state at loop boundaries (LD1 at 0x80E3C6A4 /
/// 0x80E3C6F0 — V17/V18/X5 carry over from the previous iteration's
/// ADDP/UMOV chain since the load only writes V1/V2).
fn dump_vec_at_pcs() -> &'static [u64] {
    use std::sync::OnceLock;
    static PCS: OnceLock<Vec<u64>> = OnceLock::new();
    PCS.get_or_init(|| {
        let Ok(raw) = std::env::var("RUZU_DUMP_VEC_AT_PC") else {
            return Vec::new();
        };
        raw.split(',')
            .map(str::trim)
            .filter(|s| !s.is_empty())
            .filter_map(|s| {
                let s = s.strip_prefix("0x").or_else(|| s.strip_prefix("0X")).unwrap_or(s);
                u64::from_str_radix(s, 16).ok()
            })
            .collect()
    })
    .as_slice()
}

#[inline(always)]
fn dump_vec_at_pc(cb: &DynarmicCallbacks64) {
    let pcs = dump_vec_at_pcs();
    if pcs.is_empty() {
        return;
    }
    let Some(pc_ptr) = cb.jit_pc_ptr else { return; };
    let jit_state_ptr = unsafe {
        (pc_ptr as *const u8).sub(A64JitState::offset_of_pc()) as *const A64JitState
    };
    let jit_state = unsafe { &*jit_state_ptr };
    if !pcs.contains(&jit_state.pc) {
        return;
    }
    // V<i> = (vec[2i], vec[2i+1]) — low/high 64 bits.
    let vlo = |i: usize| jit_state.vec[2 * i];
    let vhi = |i: usize| jit_state.vec[2 * i + 1];
    let x3 = jit_state.reg[3];
    let x5 = jit_state.reg[5];
    eprintln!(
        "[VEC_DUMP] pc=0x{:016X} v3=0x{:016X}{:016X} v4=0x{:016X}{:016X} v7=0x{:016X}{:016X} v16=0x{:016X}{:016X} v17=0x{:016X}{:016X} v18=0x{:016X}{:016X} x3=0x{:016X} x5=0x{:016X}",
        jit_state.pc,
        vhi(3), vlo(3),
        vhi(4), vlo(4),
        vhi(7), vlo(7),
        vhi(16), vlo(16),
        vhi(17), vlo(17),
        vhi(18), vlo(18),
        x3, x5
    );
}

fn trace_a64_access_budget() -> &'static AtomicU32 {
    use std::sync::OnceLock;
    static BUDGET: OnceLock<AtomicU32> = OnceLock::new();
    BUDGET.get_or_init(|| {
        AtomicU32::new(
            parse_u64_env("RUZU_TRACE_A64_ACCESS_LIMIT")
                .unwrap_or(128)
                .min(u32::MAX as u64) as u32,
        )
    })
}

fn trace_a64_svc_regs_enabled() -> bool {
    use std::sync::OnceLock;
    static ENABLED: OnceLock<bool> = OnceLock::new();
    *ENABLED.get_or_init(|| std::env::var_os("RUZU_TRACE_A64_SVC_REGS").is_some())
}

#[inline(always)]
fn trace_a64_access_64(cb: &DynarmicCallbacks64, kind: &str, vaddr: u64, size: u64, value: u128) {
    let Some(target_pc) = trace_a64_access_pc() else {
        return;
    };
    let (pc, lr, x0, x1, x4, x5, x6, x7, x8, x19, x20, x21, x22, x25) = cb.watch_context();
    if pc != target_pc {
        return;
    }

    let budget = trace_a64_access_budget();
    let remaining = budget.load(Ordering::Relaxed);
    if remaining == 0 {
        return;
    }
    budget.fetch_sub(1, Ordering::Relaxed);

    eprintln!(
        "[A64_TRACE_ACCESS_{kind}] pc=0x{pc:016X} lr=0x{lr:016X} x0=0x{x0:016X} x1=0x{x1:016X} x4=0x{x4:016X} x5=0x{x5:016X} x6=0x{x6:016X} x7=0x{x7:016X} x8=0x{x8:016X} x19=0x{x19:016X} x20=0x{x20:016X} x21=0x{x21:016X} x22=0x{x22:016X} x25=0x{x25:016X} vaddr=0x{vaddr:016X} size={size} value=0x{value:032X}"
    );
}

#[inline(always)]
fn watch_read_64(cb: &DynarmicCallbacks64, vaddr: u64, size: u64, value: u128) {
    let ranges = watched_ranges_64();
    if ranges.is_empty() {
        return;
    }
    let end = vaddr.saturating_add(size);
    if !ranges.iter().any(|(s, e)| vaddr < *e && end > *s) {
        return;
    }
    let (pc, lr, x0, x1, x4, x5, x6, x7, x8, x19, x20, x21, x22, x25) = cb.watch_context();
    eprintln!(
        "[WATCH_READ ] pc=0x{:016X} lr=0x{:016X} x0=0x{:016X} x1=0x{:016X} x4=0x{:016X} x5=0x{:016X} x6=0x{:016X} x7=0x{:016X} x8=0x{:016X} x19=0x{:016X} x20=0x{:016X} x21=0x{:016X} x22=0x{:016X} x25=0x{:016X} vaddr=0x{:X} size={} value=0x{:032X}",
        pc, lr, x0, x1, x4, x5, x6, x7, x8, x19, x20, x21, x22, x25, vaddr, size, value
    );
}

#[inline(always)]
fn watch_write_64(cb: &DynarmicCallbacks64, vaddr: u64, size: u64, value: u128) {
    let ranges = watched_ranges_64();
    if ranges.is_empty() {
        return;
    }
    let end = vaddr.saturating_add(size);
    if !ranges.iter().any(|(s, e)| vaddr < *e && end > *s) {
        return;
    }
    let (pc, lr, x0, x1, x4, x5, x6, x7, x8, x19, x20, x21, x22, x25) = cb.watch_context();
    eprintln!(
        "[WATCH_WRITE] pc=0x{:016X} lr=0x{:016X} x0=0x{:016X} x1=0x{:016X} x4=0x{:016X} x5=0x{:016X} x6=0x{:016X} x7=0x{:016X} x8=0x{:016X} x19=0x{:016X} x20=0x{:016X} x21=0x{:016X} x22=0x{:016X} x25=0x{:016X} vaddr=0x{:X} size={} value=0x{:032X}",
        pc, lr, x0, x1, x4, x5, x6, x7, x8, x19, x20, x21, x22, x25, vaddr, size, value
    );
}

/// Translate rdynarmic's HaltReason to core's HaltReason.
///
/// rdynarmic uses its own compact halt reason bits:
///   STEP=1, SVC=2, BREAKPOINT=4, EXCEPTION_RAISED=8,
///   CACHE_INVALIDATION=16, EXTERNAL_HALT=32
///
/// Corresponds to upstream `Core::TranslateHaltReason`.
fn translate_halt_reason(hr: rdynarmic::halt_reason::HaltReason) -> HaltReason {
    let mut result = HaltReason::empty();

    if hr.contains(rdynarmic::halt_reason::HaltReason::STEP) {
        result |= HaltReason::STEP_THREAD;
    }
    if hr.contains(rdynarmic::halt_reason::HaltReason::MEMORY_ABORT) {
        result |= HaltReason::DATA_ABORT;
    }
    if hr.contains(rdynarmic::halt_reason::HaltReason::SVC) {
        result |= HaltReason::SUPERVISOR_CALL;
    }
    if hr.contains(rdynarmic::halt_reason::HaltReason::BREAKPOINT) {
        result |= HaltReason::INSTRUCTION_BREAKPOINT;
    }
    if hr.contains(rdynarmic::halt_reason::HaltReason::PREFETCH_ABORT) {
        result |= HaltReason::PREFETCH_ABORT;
    }
    if hr.contains(rdynarmic::halt_reason::HaltReason::EXTERNAL_HALT) {
        result |= HaltReason::BREAK_LOOP;
    }

    result
}

/// JIT callbacks for ARM64.
///
/// Corresponds to upstream `DynarmicCallbacks64`.
///
/// Holds a shared reference to guest process memory, matching upstream's
/// `Core::Memory::Memory& m_memory` obtained from `process->GetMemory()`.
struct DynarmicCallbacks64 {
    /// Shared guest memory reference (ProcessMemoryData fallback).
    memory: SharedProcessMemory,
    /// Core::Memory::Memory bridge (reads/writes via PageTable → DeviceMemory).
    /// Matches upstream `Core::Memory::Memory& m_memory`.
    core_memory: Option<Arc<std::sync::Mutex<Memory>>>,
    /// SVC number from last supervisor call, shared with parent ArmDynarmic64.
    /// Upstream: callback writes to m_parent.m_svc via back-reference.
    svc: Arc<AtomicU32>,
    /// TPIDRRO_EL0 system register (read-only thread ID)
    tpidrro_el0: u64,
    /// TPIDR_EL0 system register (read-write thread ID)
    tpidr_el0: u64,
    /// Whether wall clock is used (if true, ticking is disabled).
    /// Matches upstream `m_parent.m_uses_wall_clock`.
    uses_wall_clock: bool,
    /// Core timing reference for tick management.
    /// Matches upstream `m_parent.m_system.CoreTiming()`.
    core_timing: Arc<crate::core_timing::CoreTiming>,
    /// Last exception address reported by dynarmic.
    last_exception_address: Arc<AtomicU64>,
    /// Saved guest context for upstream-style `ReturnException(pc, hr)`.
    breakpoint_context: Arc<Mutex<ThreadContext>>,
    /// Shared exclusive monitor backing Dynarmic's global monitor state.
    exclusive_monitor:
        *mut crate::arm::dynarmic::dynarmic_exclusive_monitor::DynarmicExclusiveMonitor,
    /// CPU core index associated with this callback/JIT instance.
    core_index: usize,
    /// Raw pointer to rdynarmic's `halt_reason` field.
    /// Set after JIT creation via `set_halt_reason_ptr()`.
    halt_reason_ptr: Option<*const AtomicU32>,
    /// Raw pointer to `A64JitState::pc` for diagnostic logging.
    /// Set after JIT creation via `set_pc_ptr()`.
    jit_pc_ptr: Option<*const u64>,
}

// Safety: exclusive_monitor points to process-owned state that outlives the callback/JIT.
// Each callback is still used by only one JIT/core.
unsafe impl Send for DynarmicCallbacks64 {}

impl DynarmicCallbacks64 {
    fn new(
        memory: SharedProcessMemory,
        core_memory: Option<Arc<std::sync::Mutex<Memory>>>,
        svc: Arc<AtomicU32>,
        uses_wall_clock: bool,
        core_timing: Arc<crate::core_timing::CoreTiming>,
        last_exception_address: Arc<AtomicU64>,
        breakpoint_context: Arc<Mutex<ThreadContext>>,
        exclusive_monitor: *mut crate::arm::dynarmic::dynarmic_exclusive_monitor::DynarmicExclusiveMonitor,
        core_index: usize,
    ) -> Self {
        Self {
            memory,
            core_memory,
            svc,
            tpidrro_el0: 0,
            tpidr_el0: 0,
            uses_wall_clock,
            core_timing,
            last_exception_address,
            breakpoint_context,
            exclusive_monitor,
            core_index,
            halt_reason_ptr: None,
            jit_pc_ptr: None,
        }
    }

    /// Rust equivalent of upstream `m_parent.m_jit->HaltExecution(hr)`.
    fn halt_execution(&self, reason: rdynarmic::halt_reason::HaltReason) {
        if let Some(ptr) = self.halt_reason_ptr {
            unsafe { (&*ptr).fetch_or(reason.bits(), Ordering::SeqCst) };
        }
    }

    fn watch_context(
        &self,
    ) -> (
        u64,
        u64,
        u64,
        u64,
        u64,
        u64,
        u64,
        u64,
        u64,
        u64,
        u64,
        u64,
        u64,
        u64,
    ) {
        let Some(pc_ptr) = self.jit_pc_ptr else {
            return (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
        };
        let jit_state_ptr =
            unsafe { (pc_ptr as *const u8).sub(A64JitState::offset_of_pc()) as *const A64JitState };
        let jit_state = unsafe { &*jit_state_ptr };
        (
            jit_state.pc,
            jit_state.reg[30],
            jit_state.reg[0],
            jit_state.reg[1],
            jit_state.reg[4],
            jit_state.reg[5],
            jit_state.reg[6],
            jit_state.reg[7],
            jit_state.reg[8],
            jit_state.reg[19],
            jit_state.reg[20],
            jit_state.reg[21],
            jit_state.reg[22],
            jit_state.reg[25],
        )
    }

    fn snapshot_context(&self, pc: u64) {
        let Some(pc_ptr) = self.jit_pc_ptr else {
            return;
        };
        let jit_state_ptr =
            unsafe { (pc_ptr as *const u8).sub(A64JitState::offset_of_pc()) as *const A64JitState };
        let jit_state = unsafe { &*jit_state_ptr };
        let mut ctx = self.breakpoint_context.lock().unwrap();
        *ctx = thread_context_from_jit_state(jit_state, pc);
    }

    fn try_read_guest_u64_for_diag(&self, vaddr: u64) -> Option<u64> {
        if let Some(ref cm) = self.core_memory {
            let memory = cm.lock().unwrap();
            if !memory.is_valid_virtual_address_range(vaddr, 8) {
                return None;
            }
            Some(memory.read_64(vaddr))
        } else {
            let memory = self.memory.read().unwrap();
            if !memory.is_valid_range(vaddr, 8) {
                return None;
            }
            Some(memory.read_64(vaddr))
        }
    }

    fn format_guest_frame_chain_for_diag(&self, fp: u64) -> String {
        if fp == 0 {
            return "fp=<null>".to_string();
        }

        let prev_fp0 = self.try_read_guest_u64_for_diag(fp);
        let ret0 = self.try_read_guest_u64_for_diag(fp.saturating_add(8));

        let (prev_fp1, ret1) = if let Some(prev_fp0) = prev_fp0 {
            (
                self.try_read_guest_u64_for_diag(prev_fp0),
                self.try_read_guest_u64_for_diag(prev_fp0.saturating_add(8)),
            )
        } else {
            (None, None)
        };

        format!(
            "fp0=0x{fp:016X} prev_fp0={} ret0={} prev_fp1={} ret1={}",
            prev_fp0
                .map(|value| format!("0x{value:016X}"))
                .unwrap_or_else(|| "<invalid>".to_string()),
            ret0.map(|value| format!("0x{value:016X}"))
                .unwrap_or_else(|| "<invalid>".to_string()),
            prev_fp1
                .map(|value| format!("0x{value:016X}"))
                .unwrap_or_else(|| "<invalid>".to_string()),
            ret1.map(|value| format!("0x{value:016X}"))
                .unwrap_or_else(|| "<invalid>".to_string()),
        )
    }

    /// Matches upstream `DynarmicCallbacks64::CheckMemoryAccess`.
    ///
    /// Upstream behavior: `m_check_memory_access` is only true when
    /// `debugger_enabled || !cpuopt_ignore_memory_aborts`. The default is
    /// `cpuopt_ignore_memory_aborts = true`, so `m_check_memory_access = false`,
    /// meaning this function returns true immediately without checking.
    ///
    /// Memory access validation is a debugger feature, not used in normal play.
    /// The JIT uses page table fastmem for actual memory protection.
    fn check_memory_access(&self, _addr: u64, _size: u64) -> bool {
        true
    }
}

impl UserCallbacks for DynarmicCallbacks64 {
    fn memory_read_code(&self, vaddr: u64) -> Option<u32> {
        if vaddr == 0 && std::env::var_os("RUZU_TRACE_A64_NULL_FETCH").is_some() {
            if let Some(pc_ptr) = self.jit_pc_ptr {
                let jit_state_ptr = unsafe {
                    (pc_ptr as *const u8).sub(A64JitState::offset_of_pc()) as *const A64JitState
                };
                let jit_state = unsafe { &*jit_state_ptr };
                eprintln!(
                    "[A64_NULL_FETCH] fetch=0x0000000000000000 pc=0x{:016X} lr=0x{:016X} sp=0x{:016X} \
x0=0x{:016X} x1=0x{:016X} x2=0x{:016X} x3=0x{:016X} x4=0x{:016X} x5=0x{:016X} \
x19=0x{:016X} x20=0x{:016X} x21=0x{:016X} x22=0x{:016X} x25=0x{:016X}",
                    jit_state.pc,
                    jit_state.reg[30],
                    jit_state.sp,
                    jit_state.reg[0],
                    jit_state.reg[1],
                    jit_state.reg[2],
                    jit_state.reg[3],
                    jit_state.reg[4],
                    jit_state.reg[5],
                    jit_state.reg[19],
                    jit_state.reg[20],
                    jit_state.reg[21],
                    jit_state.reg[22],
                    jit_state.reg[25],
                );
            } else {
                eprintln!("[A64_NULL_FETCH] fetch=0x0000000000000000 jit_state=<unavailable>");
            }
        }
        // Upstream: returns std::nullopt if IsValidVirtualAddressRange fails.
        if let Some(ref cm) = self.core_memory {
            let m = cm.lock().unwrap();
            if m.is_valid_virtual_address_range(vaddr, 4) {
                Some(m.read_32(vaddr))
            } else {
                None
            }
        } else {
            let mem = self.memory.read().unwrap();
            if mem.is_valid_range(vaddr, 4) {
                Some(mem.read_32(vaddr))
            } else {
                None
            }
        }
    }

    fn memory_read_8(&self, vaddr: u64) -> u8 {
        self.check_memory_access(vaddr, 1);
        let value = if let Some(ref cm) = self.core_memory {
            cm.lock().unwrap().read_8(vaddr)
        } else {
            self.memory.read().unwrap().read_8(vaddr)
        };
        watch_read_64(self, vaddr, 1, value as u128);
        trace_a64_access_64(self, "READ", vaddr, 1, value as u128);
        value
    }

    fn memory_read_16(&self, vaddr: u64) -> u16 {
        self.check_memory_access(vaddr, 2);
        let value = if let Some(ref cm) = self.core_memory {
            cm.lock().unwrap().read_16(vaddr)
        } else {
            self.memory.read().unwrap().read_16(vaddr)
        };
        watch_read_64(self, vaddr, 2, value as u128);
        trace_a64_access_64(self, "READ", vaddr, 2, value as u128);
        value
    }

    fn memory_read_32(&self, vaddr: u64) -> u32 {
        self.check_memory_access(vaddr, 4);
        let value = if let Some(ref cm) = self.core_memory {
            cm.lock().unwrap().read_32(vaddr)
        } else {
            self.memory.read().unwrap().read_32(vaddr)
        };
        watch_read_64(self, vaddr, 4, value as u128);
        trace_a64_access_64(self, "READ", vaddr, 4, value as u128);
        value
    }

    fn memory_read_64(&self, vaddr: u64) -> u64 {
        self.check_memory_access(vaddr, 8);
        dump_vec_at_pc(self);
        // RUZU_TRACE_R64_VALUE=0xVAL — log on Read64 that RETURNS the
        // target value. Inverse of W64_VALUE; used to track where a
        // sentinel value enters guest registers from memory.
        if let Ok(target_str) = std::env::var("RUZU_TRACE_R64_VALUE") {
            let target = u64::from_str_radix(target_str.trim_start_matches("0x"), 16).unwrap_or(0);
            let value_pre = if let Some(ref cm) = self.core_memory {
                cm.lock().unwrap().read_64(vaddr)
            } else {
                self.memory.read().unwrap().read_64(vaddr)
            };
            if value_pre == target {
                use std::sync::atomic::{AtomicU32, Ordering};
                static SHOWN: AtomicU32 = AtomicU32::new(0);
                let n = SHOWN.fetch_add(1, Ordering::Relaxed);
                if n < 8 {
                    if let Some(pc_ptr) = self.jit_pc_ptr {
                        let jit_state_ptr = unsafe {
                            (pc_ptr as *const u8).sub(A64JitState::offset_of_pc())
                                as *const A64JitState
                        };
                        let s = unsafe { &*jit_state_ptr };
                        eprintln!(
                            "[R64_VALUE #{}] vaddr=0x{:016X} value=0x{:016X} pc=0x{:016X} lr=0x{:016X} sp=0x{:016X} \
x0=0x{:016X} x1=0x{:016X} x2=0x{:016X} x19=0x{:016X} x20=0x{:016X} x21=0x{:016X} x22=0x{:016X}",
                            n, vaddr, value_pre, s.pc, s.reg[30], s.sp,
                            s.reg[0], s.reg[1], s.reg[2],
                            s.reg[19], s.reg[20], s.reg[21], s.reg[22],
                        );
                    }
                }
            }
        }
        if std::env::var_os("RUZU_TRACE_A64_INVALID_DATA_READ").is_some() {
            let is_valid = if let Some(ref cm) = self.core_memory {
                cm.lock().unwrap().is_valid_virtual_address_range(vaddr, 8)
            } else {
                self.memory.read().unwrap().is_valid_range(vaddr, 8)
            };
            if !is_valid || vaddr < 0x1000 {
                if let Some(pc_ptr) = self.jit_pc_ptr {
                    let jit_state_ptr = unsafe {
                        (pc_ptr as *const u8).sub(A64JitState::offset_of_pc()) as *const A64JitState
                    };
                    let jit_state = unsafe { &*jit_state_ptr };
                    let frame_chain = self.format_guest_frame_chain_for_diag(jit_state.reg[29]);
                    eprintln!(
                        "[A64_INVALID_READ64] vaddr=0x{:016X} pc=0x{:016X} lr=0x{:016X} sp=0x{:016X} \
x29=0x{:016X} x0=0x{:016X} x1=0x{:016X} x2=0x{:016X} x3=0x{:016X} x4=0x{:016X} x5=0x{:016X} \
x19=0x{:016X} x20=0x{:016X} x21=0x{:016X} x22=0x{:016X} x25=0x{:016X} {}",
                        vaddr,
                        jit_state.pc,
                        jit_state.reg[30],
                        jit_state.sp,
                        jit_state.reg[29],
                        jit_state.reg[0],
                        jit_state.reg[1],
                        jit_state.reg[2],
                        jit_state.reg[3],
                        jit_state.reg[4],
                        jit_state.reg[5],
                        jit_state.reg[19],
                        jit_state.reg[20],
                        jit_state.reg[21],
                        jit_state.reg[22],
                        jit_state.reg[25],
                        frame_chain,
                    );
                    if std::env::var_os("RUZU_DUMP_INVALID_X19").is_some() {
                        let base = jit_state.reg[19];
                        let mut bytes = Vec::with_capacity(96);
                        for i in 0..96u64 {
                            let addr = base + i;
                            let valid = if let Some(ref cm) = self.core_memory {
                                cm.lock().unwrap().is_valid_virtual_address_range(addr, 1)
                            } else {
                                self.memory.read().unwrap().is_valid_range(addr, 1)
                            };
                            if !valid {
                                break;
                            }
                            let b = if let Some(ref cm) = self.core_memory {
                                cm.lock().unwrap().read_8(addr)
                            } else {
                                self.memory.read().unwrap().read_8(addr)
                            };
                            bytes.push(b);
                        }
                        let ascii: String = bytes
                            .iter()
                            .map(|&b| if (0x20..=0x7e).contains(&b) { b as char } else { '.' })
                            .collect();
                        eprintln!("[A64_INVALID_READ64_X19_BYTES] addr=0x{base:016X} bytes={bytes:02X?}");
                        eprintln!("[A64_INVALID_READ64_X19_ASCII] {ascii}");
                    }
                } else {
                    eprintln!(
                        "[A64_INVALID_READ64] vaddr=0x{:016X} jit_state=<unavailable>",
                        vaddr
                    );
                }
            }
        }
        let value = if let Some(ref cm) = self.core_memory {
            cm.lock().unwrap().read_64(vaddr)
        } else {
            self.memory.read().unwrap().read_64(vaddr)
        };
        watch_read_64(self, vaddr, 8, value as u128);
        trace_a64_access_64(self, "READ", vaddr, 8, value as u128);
        value
    }

    fn memory_read_128(&self, vaddr: u64) -> (u64, u64) {
        self.check_memory_access(vaddr, 16);
        // RUZU_DUMP_VEC_AT_PC=PC[,PC,...] — for vectorized strchr/strpbrk
        // scans, dump V17/V18/X3/X5 at LD1 entry. The reduction state
        // from the previous iteration is still live in V17/V18/X5
        // because LD1 only writes V1/V2.
        dump_vec_at_pc(self);
        // RUZU_TRACE_R128_PC=1 — log the JIT PC + key registers on the
        // FIRST few 128-bit reads. Used to identify the guest code that
        // emits a long sequential vector-load scan (e.g. STK's STK
        // unmapped-hole linear scan starting at 0x815E6000).
        if std::env::var_os("RUZU_TRACE_R128_PC").is_some() {
            use std::sync::atomic::{AtomicU32, Ordering};
            static SHOWN: AtomicU32 = AtomicU32::new(0);
            // Trace reads at the known scan-loop PC 0x80E3B720, but only the
            // first 16 to keep noise low. Drop the per-vaddr filter so we
            // see the very first iteration including its starting x1.
            let pc_match = if let Some(pc_ptr) = self.jit_pc_ptr {
                let p = unsafe {
                    (pc_ptr as *const u8).sub(A64JitState::offset_of_pc()) as *const A64JitState
                };
                let live_pc = unsafe { (*p).pc };
                live_pc == 0x80E3B720
            } else {
                false
            };
            let n = if pc_match {
                SHOWN.fetch_add(1, Ordering::Relaxed)
            } else {
                u32::MAX
            };
            if n < 16 {
                if let Some(pc_ptr) = self.jit_pc_ptr {
                    let jit_state_ptr = unsafe {
                        (pc_ptr as *const u8).sub(A64JitState::offset_of_pc()) as *const A64JitState
                    };
                    let s = unsafe { &*jit_state_ptr };
                    eprintln!(
                        "[R128 #{}] vaddr=0x{:016X} pc=0x{:016X} lr=0x{:016X} sp=0x{:016X} \
x0=0x{:016X} x1=0x{:016X} x2=0x{:016X} x3=0x{:016X} x19=0x{:016X} x20=0x{:016X} x21=0x{:016X} x22=0x{:016X} x29=0x{:016X}",
                        n, vaddr, s.pc, s.reg[30], s.sp,
                        s.reg[0], s.reg[1], s.reg[2], s.reg[3],
                        s.reg[19], s.reg[20], s.reg[21], s.reg[22], s.reg[29],
                    );
                }
            }
        }
        let value = if let Some(ref cm) = self.core_memory {
            let m = cm.lock().unwrap();
            (m.read_64(vaddr), m.read_64(vaddr + 8))
        } else {
            let mem = self.memory.read().unwrap();
            (mem.read_64(vaddr), mem.read_64(vaddr + 8))
        };
        watch_read_64(
            self,
            vaddr,
            16,
            ((value.1 as u128) << 64) | (value.0 as u128),
        );
        trace_a64_access_64(
            self,
            "READ",
            vaddr,
            16,
            ((value.1 as u128) << 64) | (value.0 as u128),
        );
        value
    }

    fn memory_write_8(&mut self, vaddr: u64, value: u8) {
        if !self.check_memory_access(vaddr, 1) {
            return;
        }
        watch_write_64(self, vaddr, 1, value as u128);
        trace_a64_access_64(self, "WRITE", vaddr, 1, value as u128);
        if let Some(ref cm) = self.core_memory {
            cm.lock().unwrap().write_8(vaddr, value);
        } else {
            self.memory.write().unwrap().write_8(vaddr, value);
        }
    }

    fn memory_write_16(&mut self, vaddr: u64, value: u16) {
        if !self.check_memory_access(vaddr, 2) {
            return;
        }
        watch_write_64(self, vaddr, 2, value as u128);
        trace_a64_access_64(self, "WRITE", vaddr, 2, value as u128);
        if let Some(ref cm) = self.core_memory {
            cm.lock().unwrap().write_16(vaddr, value);
        } else {
            self.memory.write().unwrap().write_16(vaddr, value);
        }
    }

    fn memory_write_32(&mut self, vaddr: u64, value: u32) {
        if !self.check_memory_access(vaddr, 4) {
            return;
        }
        watch_write_64(self, vaddr, 4, value as u128);
        trace_a64_access_64(self, "WRITE", vaddr, 4, value as u128);
        if let Some(ref cm) = self.core_memory {
            cm.lock().unwrap().write_32(vaddr, value);
        } else {
            self.memory.write().unwrap().write_32(vaddr, value);
        }
    }

    fn memory_write_64(&mut self, vaddr: u64, value: u64) {
        if !self.check_memory_access(vaddr, 8) {
            return;
        }
        // RUZU_TRACE_W64_VALUE=0xVAL — fire on every 64-bit write whose
        // value matches VAL. Used to find the source of a sentinel value
        // being placed into a struct field (e.g. STK's mysterious
        // 0x0000FF00FFFF0000 ending up in a refcount-table entry).
        if let Ok(target_str) = std::env::var("RUZU_TRACE_W64_VALUE") {
            let target = u64::from_str_radix(target_str.trim_start_matches("0x"), 16).unwrap_or(0);
            if value == target {
                use std::sync::atomic::{AtomicU32, Ordering};
                static SHOWN: AtomicU32 = AtomicU32::new(0);
                let n = SHOWN.fetch_add(1, Ordering::Relaxed);
                if n < 8 {
                    if let Some(pc_ptr) = self.jit_pc_ptr {
                        let jit_state_ptr = unsafe {
                            (pc_ptr as *const u8).sub(A64JitState::offset_of_pc())
                                as *const A64JitState
                        };
                        let s = unsafe { &*jit_state_ptr };
                        eprintln!(
                            "[W64_VALUE #{}] vaddr=0x{:016X} value=0x{:016X} pc=0x{:016X} lr=0x{:016X} sp=0x{:016X} \
x0=0x{:016X} x1=0x{:016X} x2=0x{:016X} x3=0x{:016X} x19=0x{:016X} x20=0x{:016X} x21=0x{:016X} x22=0x{:016X} x29=0x{:016X}",
                            n, vaddr, value, s.pc, s.reg[30], s.sp,
                            s.reg[0], s.reg[1], s.reg[2], s.reg[3],
                            s.reg[19], s.reg[20], s.reg[21], s.reg[22], s.reg[29],
                        );
                    }
                }
            }
        }
        watch_write_64(self, vaddr, 8, value as u128);
        trace_a64_access_64(self, "WRITE", vaddr, 8, value as u128);
        if let Some(ref cm) = self.core_memory {
            cm.lock().unwrap().write_64(vaddr, value);
        } else {
            self.memory.write().unwrap().write_64(vaddr, value);
        }
    }

    fn memory_write_128(&mut self, vaddr: u64, value_lo: u64, value_hi: u64) {
        if !self.check_memory_access(vaddr, 16) {
            return;
        }
        // RUZU_TRACE_W64_VALUE also checks 128-bit writes (lo OR hi half).
        if let Ok(target_str) = std::env::var("RUZU_TRACE_W64_VALUE") {
            let target = u64::from_str_radix(target_str.trim_start_matches("0x"), 16).unwrap_or(0);
            if value_lo == target || value_hi == target {
                use std::sync::atomic::{AtomicU32, Ordering};
                static SHOWN: AtomicU32 = AtomicU32::new(0);
                let n = SHOWN.fetch_add(1, Ordering::Relaxed);
                if n < 8 {
                    if let Some(pc_ptr) = self.jit_pc_ptr {
                        let jit_state_ptr = unsafe {
                            (pc_ptr as *const u8).sub(A64JitState::offset_of_pc())
                                as *const A64JitState
                        };
                        let s = unsafe { &*jit_state_ptr };
                        eprintln!(
                            "[W128_VALUE #{}] vaddr=0x{:016X} lo=0x{:016X} hi=0x{:016X} pc=0x{:016X} lr=0x{:016X} \
x0=0x{:016X} x1=0x{:016X} x19=0x{:016X} x20=0x{:016X} x21=0x{:016X}",
                            n, vaddr, value_lo, value_hi, s.pc, s.reg[30],
                            s.reg[0], s.reg[1], s.reg[19], s.reg[20], s.reg[21],
                        );
                    }
                }
            }
        }
        watch_write_64(
            self,
            vaddr,
            16,
            ((value_hi as u128) << 64) | (value_lo as u128),
        );
        trace_a64_access_64(
            self,
            "WRITE",
            vaddr,
            16,
            ((value_hi as u128) << 64) | (value_lo as u128),
        );
        if let Some(ref cm) = self.core_memory {
            let m = cm.lock().unwrap();
            m.write_64(vaddr, value_lo);
            m.write_64(vaddr + 8, value_hi);
        } else {
            let mut mem = self.memory.write().unwrap();
            mem.write_64(vaddr, value_lo);
            mem.write_64(vaddr + 8, value_hi);
        }
    }

    fn exclusive_read_8(&self, vaddr: u64) -> u8 {
        self.memory_read_8(vaddr)
    }

    fn exclusive_read_16(&self, vaddr: u64) -> u16 {
        self.memory_read_16(vaddr)
    }

    fn exclusive_read_32(&self, vaddr: u64) -> u32 {
        self.memory_read_32(vaddr)
    }

    fn exclusive_read_64(&self, vaddr: u64) -> u64 {
        self.memory_read_64(vaddr)
    }

    fn exclusive_read_128(&self, vaddr: u64) -> (u64, u64) {
        self.memory_read_128(vaddr)
    }

    fn exclusive_write_8(&mut self, vaddr: u64, value: u8, expected: u8) -> bool {
        self.check_memory_access(vaddr, 1)
            && if let Some(ref cm) = self.core_memory {
                cm.lock().unwrap().write_exclusive_8(vaddr, value, expected)
            } else {
                self.memory_write_8(vaddr, value);
                true
            }
    }

    fn exclusive_write_16(&mut self, vaddr: u64, value: u16, expected: u16) -> bool {
        self.check_memory_access(vaddr, 2)
            && if let Some(ref cm) = self.core_memory {
                cm.lock()
                    .unwrap()
                    .write_exclusive_16(vaddr, value, expected)
            } else {
                self.memory_write_16(vaddr, value);
                true
            }
    }

    fn exclusive_write_32(&mut self, vaddr: u64, value: u32, expected: u32) -> bool {
        // RUZU_TRACE_CAS32_PC=1 — log the JIT PC + key registers on the
        // FIRST few exclusive-write-32 calls. Used to identify the guest
        // code that emits a tight CAS spinloop (e.g. STK's atomic CAS on
        // 0x0000ff00ffff0008 right after Connect).
        if std::env::var_os("RUZU_TRACE_CAS32_PC").is_some() {
            use std::sync::atomic::{AtomicU32, Ordering};
            static SHOWN: AtomicU32 = AtomicU32::new(0);
            // Only trace CAS at non-canonical / suspicious addresses
            // (outside the normal libnx mutex region 0x80000000..0x90000000).
            let suspicious = vaddr < 0x8000_0000 || vaddr >= 0x9000_0000;
            let n = if suspicious {
                SHOWN.fetch_add(1, Ordering::Relaxed)
            } else {
                u32::MAX
            };
            if n < 12 {
                if let Some(pc_ptr) = self.jit_pc_ptr {
                    let jit_state_ptr = unsafe {
                        (pc_ptr as *const u8).sub(A64JitState::offset_of_pc()) as *const A64JitState
                    };
                    let s = unsafe { &*jit_state_ptr };
                    eprintln!(
                        "[CAS32 #{}] vaddr=0x{:016X} value=0x{:08X} expected=0x{:08X} pc=0x{:016X} lr=0x{:016X} sp=0x{:016X} \
x0=0x{:016X} x1=0x{:016X} x2=0x{:016X} x3=0x{:016X} x19=0x{:016X} x20=0x{:016X} x21=0x{:016X} x22=0x{:016X} x29=0x{:016X}",
                        n, vaddr, value, expected, s.pc, s.reg[30], s.sp,
                        s.reg[0], s.reg[1], s.reg[2], s.reg[3],
                        s.reg[19], s.reg[20], s.reg[21], s.reg[22], s.reg[29],
                    );
                }
            }
        }
        self.check_memory_access(vaddr, 4)
            && if let Some(ref cm) = self.core_memory {
                cm.lock()
                    .unwrap()
                    .write_exclusive_32(vaddr, value, expected)
            } else {
                self.memory_write_32(vaddr, value);
                true
            }
    }

    fn exclusive_write_64(&mut self, vaddr: u64, value: u64, expected: u64) -> bool {
        self.check_memory_access(vaddr, 8)
            && if let Some(ref cm) = self.core_memory {
                cm.lock()
                    .unwrap()
                    .write_exclusive_64(vaddr, value, expected)
            } else {
                self.memory_write_64(vaddr, value);
                true
            }
    }

    fn exclusive_write_128(
        &mut self,
        vaddr: u64,
        value_lo: u64,
        value_hi: u64,
        expected_lo: u64,
        expected_hi: u64,
    ) -> bool {
        self.check_memory_access(vaddr, 16)
            && if let Some(ref cm) = self.core_memory {
                cm.lock().unwrap().write_exclusive_128(
                    vaddr,
                    value_lo,
                    value_hi,
                    expected_lo,
                    expected_hi,
                )
            } else {
                self.memory_write_128(vaddr, value_lo, value_hi);
                true
            }
    }

    fn exclusive_clear(&mut self) {
        if !self.exclusive_monitor.is_null() {
            unsafe {
                (*self.exclusive_monitor)
                    .get_monitor()
                    .clear_processor(self.core_index)
            };
        }
    }

    fn instruction_cache_operation(&mut self, op: u64, vaddr: u64) {
        // Upstream IC operations:
        // 0 = InvalidateByVAToPoU (IC IVAU) — invalidate cache line at vaddr
        // 1 = InvalidateAllToPoU (IC IALLU) — invalidate entire icache
        match op {
            0 => {
                log::trace!(
                    "IC IVAU @ {:#x} (no-op, cache invalidation handled at JIT level)",
                    vaddr
                );
            }
            1 => {
                log::trace!("IC IALLU (no-op, cache invalidation handled at JIT level)");
            }
            _ => {
                log::warn!(
                    "Unknown instruction_cache_operation op={} vaddr={:#x}",
                    op,
                    vaddr
                );
            }
        }
    }

    fn call_supervisor(&mut self, svc_num: u32) {
        self.svc.store(svc_num, Ordering::Relaxed);
        self.halt_execution(rdynarmic::halt_reason::HaltReason::SVC);
    }

    fn interpreter_fallback(&mut self, pc: u64, num_instructions: usize) {
        self.last_exception_address.store(pc, Ordering::Relaxed);
        self.snapshot_context(pc);
        let instr = if let Some(ref cm) = self.core_memory {
            let m = cm.lock().unwrap();
            if m.is_valid_virtual_address_range(pc, 4) {
                m.read_32(pc)
            } else {
                0
            }
        } else {
            let mem = self.memory.read().unwrap();
            if mem.is_valid_range(pc, 4) {
                mem.read_32(pc)
            } else {
                0
            }
        };
        log::error!(
            "Unimplemented instruction @ 0x{:X} for {} instructions (instr = {:08X})",
            pc,
            num_instructions,
            instr
        );
        self.halt_execution(rdynarmic::halt_reason::HaltReason::PREFETCH_ABORT);
    }

    fn exception_raised(&mut self, pc: u64, exception: u64) {
        use rdynarmic::frontend::a64::types::Exception;

        match exception {
            x if x == Exception::WaitForInterrupt as u64
                || x == Exception::WaitForEvent as u64
                || x == Exception::SendEvent as u64
                || x == Exception::SendEventLocal as u64
                || x == Exception::Yield as u64 =>
            {
                return;
            }
            x if x == Exception::NoExecuteFault as u64 => {
                self.last_exception_address.store(pc, Ordering::Relaxed);
                self.snapshot_context(pc);
                log::error!(
                    "Cannot execute instruction at unmapped address {:#016x}",
                    pc
                );
                self.halt_execution(rdynarmic::halt_reason::HaltReason::PREFETCH_ABORT);
                return;
            }
            x if x == Exception::Breakpoint as u64 => {
                self.last_exception_address.store(pc, Ordering::Relaxed);
                self.snapshot_context(pc);
                self.halt_execution(rdynarmic::halt_reason::HaltReason::BREAKPOINT);
                return;
            }
            _ => {}
        }

        self.last_exception_address.store(pc, Ordering::Relaxed);
        self.snapshot_context(pc);
        log::error!(
            "DynarmicCallbacks64::exception_raised(pc={:#x}, exception={:#x})",
            pc,
            exception
        );
        // Dump instruction window around exception PC
        if let Some(ref cm) = self.core_memory {
            let m = cm.lock().unwrap();
            let start = pc.saturating_sub(0x10);
            for addr in (start..=pc.saturating_add(0x10)).step_by(4) {
                if !m.is_valid_virtual_address(addr) {
                    log::error!("  [{:#018x}] <unmapped>", addr);
                    continue;
                }
                let insn = m.read_32(addr);
                let marker = if addr == pc { " <EXC>" } else { "" };
                log::error!("  [{:#018x}] {:#010x}{}", addr, insn, marker);
            }
        }
    }

    /// Matches upstream `DynarmicCallbacks64::GetCNTPCT`.
    fn get_cntpct(&self) -> u64 {
        self.core_timing.get_clock_ticks()
    }

    /// Matches upstream `DynarmicCallbacks64::AddTicks`:
    /// Divides ticks by NUM_CPU_CORES (4), passes to CoreTiming::AddTicks.
    fn add_ticks(&mut self, ticks: u64) {
        if self.uses_wall_clock {
            return;
        }
        let amortized_ticks =
            std::cmp::max(ticks / crate::hardware_properties::NUM_CPU_CORES as u64, 1);
        self.core_timing.add_ticks(amortized_ticks);
    }

    /// Matches upstream `DynarmicCallbacks64::GetTicksRemaining`:
    /// Returns max(CoreTiming::GetDowncount(), 0).
    fn get_ticks_remaining(&self) -> u64 {
        if self.uses_wall_clock {
            return u64::MAX;
        }
        std::cmp::max(self.core_timing.get_downcount(), 0) as u64
    }

    fn set_halt_reason_ptr(&mut self, ptr: *const u32) {
        self.halt_reason_ptr = Some(ptr as *const AtomicU32);
    }

    fn set_pc_ptr(&mut self, ptr: *const u32) {
        self.jit_pc_ptr = Some(ptr as *const u64);
    }
}

/// ARM64 Dynarmic JIT backend.
///
/// Corresponds to upstream `Core::ArmDynarmic64`.
pub struct ArmDynarmic64 {
    pub base: ArmInterfaceBase,

    // Upstream holds `System& m_system` for accessing CoreTiming, DebuggerEnabled,
    // Settings, etc. Currently these are passed individually (core_timing, uses_wall_clock)
    // to avoid circular dependency with System which owns the ARM backends.
    // When System stabilizes, this can be replaced with a reference.
    /// Upstream: `DynarmicExclusiveMonitor& m_exclusive_monitor`.
    /// Passed to JitConfig::global_monitor for cross-core LDXR/STXR synchronization.
    exclusive_monitor:
        *mut crate::arm::dynarmic::dynarmic_exclusive_monitor::DynarmicExclusiveMonitor,

    /// Core index for this CPU
    core_index: usize,

    /// SVC callback number, shared with DynarmicCallbacks64.
    /// Upstream: m_svc written by callback via m_parent reference.
    svc: Arc<AtomicU32>,

    /// Watchpoint that caused a halt
    halted_watchpoint: Option<DebugWatchpoint>,

    /// Context saved at breakpoint
    breakpoint_context: Arc<Mutex<ThreadContext>>,

    /// The rdynarmic A64 JIT instance
    jit: Option<rdynarmic::A64Jit>,

    /// TPIDRRO_EL0 system register value (stored here since callbacks
    /// are moved into the JIT and we need to track it externally)
    tpidrro_el0: u64,

    /// TPIDR_EL0 system register value
    tpidr_el0: u64,

    /// Last exception address reported by dynarmic for the current halt.
    last_exception_address: Arc<AtomicU64>,
}

impl ArmDynarmic64 {
    /// Create a new ARM64 dynarmic backend.
    ///
    /// Corresponds to upstream `ArmDynarmic64::ArmDynarmic64`.
    ///
    /// `shared_memory` corresponds to upstream's `process->GetMemory()` —
    /// the JIT callbacks use it for all guest memory access.
    pub fn new(
        _system: &dyn std::any::Any,
        uses_wall_clock: bool,
        _process: &KProcess,
        exclusive_monitor: *mut crate::arm::dynarmic::dynarmic_exclusive_monitor::DynarmicExclusiveMonitor,
        core_index: usize,
        shared_memory: SharedProcessMemory,
        core_timing: Arc<crate::core_timing::CoreTiming>,
        core_memory: Option<Arc<std::sync::Mutex<Memory>>>,
    ) -> Self {
        // Fetch fastmem pointer from core_memory BEFORE it gets moved
        // into the callbacks. Mirrors A32's pattern
        // (arm_dynarmic_32.rs:2010-2013). The pointer is the base of a
        // 512 GiB host VM region whose pages mirror guest physical
        // memory; the JIT emits direct `mov [r13+vaddr]` instructions
        // against it for fast guest memory access.
        //
        // `RUZU_NO_FASTMEM=1` env-var disables fastmem entirely (forces
        // the slow callback path for all memory accesses) — useful for
        // debugging fastmem-related issues without rebuilding.
        let fastmem_pointer: Option<*mut u8> = if std::env::var("RUZU_NO_FASTMEM").is_ok() {
            log::warn!("ArmDynarmic64: RUZU_NO_FASTMEM set — fastmem disabled");
            None
        } else {
            core_memory
                .as_ref()
                .map(|cm| cm.lock().unwrap().fastmem_pointer())
                .filter(|p| !p.is_null())
        };

        // Create JIT callbacks with shared memory reference
        let svc = Arc::new(AtomicU32::new(0));
        let last_exception_address = Arc::new(AtomicU64::new(0));
        let breakpoint_context = Arc::new(Mutex::new(ThreadContext::default()));
        let callbacks = DynarmicCallbacks64::new(
            shared_memory,
            core_memory,
            svc.clone(),
            uses_wall_clock,
            core_timing,
            last_exception_address.clone(),
            breakpoint_context.clone(),
            exclusive_monitor,
            core_index,
        );

        log::warn!(
            "ArmDynarmic64: fastmem_pointer={:?} for core {}",
            fastmem_pointer.map(|p| p as usize),
            core_index
        );

        let optimizations =
            if let Some(mask) = parse_optimization_mask_env("RUZU_A64_OPTIMIZATION_MASK") {
                optimization_flags_from_mask(mask)
            } else if std::env::var("RUZU_A64_NO_OPTIMIZATIONS")
                .ok()
                .is_some_and(|value| value != "0")
            {
                OptimizationFlag::NO_OPTIMIZATIONS
            } else {
                OptimizationFlag::ALL_SAFE_OPTIMIZATIONS
            };

        // Configure JIT
        // Upstream: enable_cycle_counting = !uses_wall_clock
        let config = JitConfig {
            callbacks: Box::new(callbacks),
            enable_cycle_counting: !uses_wall_clock,
            code_cache_size: 512 * 1024 * 1024, // 512 MiB on x86_64 (upstream default)
            optimizations,
            unsafe_optimizations: false,
            global_monitor: if exclusive_monitor.is_null() {
                None
            } else {
                Some(unsafe { (*exclusive_monitor).get_monitor() as *mut _ })
            },
            fastmem_pointer,
            define_unpredictable_behaviour: true,
            processor_id: core_index as usize,
            wall_clock_cntpct: uses_wall_clock,
            // Memory emit options matching upstream zuyu's
            // `ArmDynarmic64::MakeJit` setup
            // (zuyu/src/core/arm/dynarmic/arm_dynarmic_64.cpp:225-248).
            // We default to 39-bit guest AS (Switch's extended user space
            // and the size of ruzu's `VIRTUAL_RESERVE_SIZE = 1<<39` host
            // fastmem region) with `silently_mirror_fastmem=false` so
            // out-of-range accesses fall through to the callback path
            // rather than aliasing into the fastmem region.
            memory: rdynarmic::backend::x64::emit_context::MemoryEmitConfig {
                fastmem_address_space_bits: 39,
                silently_mirror_fastmem: false,
                fastmem_exclusive_access: false,
                recompile_on_fastmem_failure: false,
                page_table_present: false,
                page_table_address_space_bits: 39,
                silently_mirror_page_table: false,
                absolute_offset_page_table: true,
                page_table_pointer_mask_bits: 0,
                detect_misaligned_access_via_page_table: 0,
                only_detect_misalignment_via_page_table_on_page_boundary: false,
                check_halt_on_memory_access: false,
                processor_id: core_index as usize,
            },
        };

        // Create the JIT
        let jit = match rdynarmic::A64Jit::new(config) {
            Ok(jit) => {
                log::info!(
                    "ArmDynarmic64: JIT created successfully for core {}",
                    core_index
                );
                Some(jit)
            }
            Err(e) => {
                log::error!(
                    "ArmDynarmic64: Failed to create JIT for core {}: {}",
                    core_index,
                    e
                );
                None
            }
        };

        Self {
            base: ArmInterfaceBase::new(uses_wall_clock),
            exclusive_monitor,
            core_index,
            svc,
            halted_watchpoint: None,
            breakpoint_context,
            jit,
            tpidrro_el0: 0,
            tpidr_el0: 0,
            last_exception_address,
        }
    }
}

// SAFETY: ArmDynarmic64 holds raw pointers to long-lived objects
// (exclusive_monitor, watchpoints) that are valid for the lifetime of the process.
// The JIT is single-threaded per core — only one thread runs each ArmDynarmic64.
unsafe impl Send for ArmDynarmic64 {}

impl ArmInterface for ArmDynarmic64 {
    fn run_thread(&mut self, _thread: &mut KThread) -> HaltReason {
        self.last_exception_address.store(0, Ordering::Relaxed);
        let jit = match self.jit.as_mut() {
            Some(jit) => jit,
            None => {
                log::error!("ArmDynarmic64::run_thread: JIT not available");
                return HaltReason::BREAK_LOOP;
            }
        };

        jit.clear_exclusive_state();
        let rdynarmic_hr = jit.run();
        if trace_a64_svc_regs_enabled()
            && rdynarmic_hr.contains(rdynarmic::halt_reason::HaltReason::SVC)
        {
            eprintln!(
                "[A64_SVC_HALT] svc=0x{:x} pc=0x{:016x} lr=0x{:016x} sp=0x{:016x} x0=0x{:016x} x1=0x{:016x} x2=0x{:016x} x3=0x{:016x} x4=0x{:016x} x5=0x{:016x} x6=0x{:016x} x7=0x{:016x} x21=0x{:016x} x22=0x{:016x} x24=0x{:016x} x25=0x{:016x}",
                self.svc.load(Ordering::Relaxed),
                jit.get_pc(),
                jit.get_register(30),
                jit.get_sp(),
                jit.get_register(0),
                jit.get_register(1),
                jit.get_register(2),
                jit.get_register(3),
                jit.get_register(4),
                jit.get_register(5),
                jit.get_register(6),
                jit.get_register(7),
                jit.get_register(21),
                jit.get_register(22),
                jit.get_register(24),
                jit.get_register(25),
            );
        }
        translate_halt_reason(rdynarmic_hr)
    }

    fn step_thread(&mut self, _thread: &mut KThread) -> HaltReason {
        self.last_exception_address.store(0, Ordering::Relaxed);
        let jit = match self.jit.as_mut() {
            Some(jit) => jit,
            None => {
                log::error!("ArmDynarmic64::step_thread: JIT not available");
                return HaltReason::BREAK_LOOP;
            }
        };

        jit.clear_exclusive_state();
        let rdynarmic_hr = jit.step();
        if trace_a64_svc_regs_enabled()
            && rdynarmic_hr.contains(rdynarmic::halt_reason::HaltReason::SVC)
        {
            eprintln!(
                "[A64_SVC_STEP] svc=0x{:x} pc=0x{:016x} lr=0x{:016x} sp=0x{:016x} x0=0x{:016x} x1=0x{:016x} x2=0x{:016x} x3=0x{:016x} x4=0x{:016x} x5=0x{:016x} x6=0x{:016x} x7=0x{:016x} x21=0x{:016x} x22=0x{:016x} x24=0x{:016x} x25=0x{:016x}",
                self.svc.load(Ordering::Relaxed),
                jit.get_pc(),
                jit.get_register(30),
                jit.get_sp(),
                jit.get_register(0),
                jit.get_register(1),
                jit.get_register(2),
                jit.get_register(3),
                jit.get_register(4),
                jit.get_register(5),
                jit.get_register(6),
                jit.get_register(7),
                jit.get_register(21),
                jit.get_register(22),
                jit.get_register(24),
                jit.get_register(25),
            );
        }
        translate_halt_reason(rdynarmic_hr)
    }

    fn clear_instruction_cache(&mut self) {
        if let Some(jit) = self.jit.as_mut() {
            jit.clear_cache();
        }
    }

    fn invalidate_cache_range(&mut self, addr: u64, size: usize) {
        if let Some(jit) = self.jit.as_mut() {
            jit.invalidate_cache_range(addr, size as u64);
        }
    }

    fn get_architecture(&self) -> Architecture {
        Architecture::AArch64
    }

    fn get_context(&self, ctx: &mut ThreadContext) {
        let jit = match self.jit.as_ref() {
            Some(jit) => jit,
            None => {
                log::warn!("ArmDynarmic64::get_context: JIT not available");
                return;
            }
        };

        // Upstream: GPRs[0..29] -> ctx.r[0..29], GPR[29] -> ctx.fp, GPR[30] -> ctx.lr
        for i in 0..29 {
            ctx.r[i] = jit.get_register(i);
        }
        ctx.fp = jit.get_register(29);
        ctx.lr = jit.get_register(30);

        ctx.sp = jit.get_sp();
        ctx.pc = jit.get_pc();
        ctx.pstate = jit.get_pstate();

        // Vector registers
        for i in 0..32 {
            let (lo, hi) = jit.get_vector(i);
            ctx.v[i] = (lo as u128) | ((hi as u128) << 64);
        }

        ctx.fpcr = jit.get_fpcr();
        ctx.fpsr = jit.get_fpsr();
        ctx.tpidr = jit.get_tpidr_el0();
    }

    fn set_context(&mut self, ctx: &ThreadContext) {
        let jit = match self.jit.as_mut() {
            Some(jit) => jit,
            None => {
                log::warn!("ArmDynarmic64::set_context: JIT not available");
                return;
            }
        };

        // Upstream: ctx.r[0..29] -> GPRs[0..29], ctx.fp -> GPR[29], ctx.lr -> GPR[30]
        for i in 0..29 {
            jit.set_register(i, ctx.r[i]);
        }
        jit.set_register(29, ctx.fp);
        jit.set_register(30, ctx.lr);

        jit.set_sp(ctx.sp);
        jit.set_pc(ctx.pc);
        jit.set_pstate(ctx.pstate);

        // Vector registers
        for i in 0..32 {
            let lo = ctx.v[i] as u64;
            let hi = (ctx.v[i] >> 64) as u64;
            jit.set_vector(i, lo, hi);
        }

        jit.set_fpcr(ctx.fpcr);
        jit.set_fpsr(ctx.fpsr);
        jit.set_tpidr_el0(ctx.tpidr);
    }

    fn set_tpidrro_el0(&mut self, value: u64) {
        // Upstream: m_cb->m_tpidrro_el0 = value, but the callback's value is
        // read by the JIT through the JitState's tpidrro_el0 field — so we
        // must propagate to the JIT. Without this, MRS x, tpidrro_el0 returns
        // zero (libnx's TLS code reads at offset 0x1E0 from null and panics).
        self.tpidrro_el0 = value;
        if let Some(jit) = self.jit.as_mut() {
            jit.set_tpidrro_el0(value);
        }
    }

    fn get_svc_arguments(&self, args: &mut [u64; 8]) {
        let jit = match self.jit.as_ref() {
            Some(jit) => jit,
            None => {
                log::warn!("ArmDynarmic64::get_svc_arguments: JIT not available");
                return;
            }
        };

        // Upstream: reads j.GetRegister(0..8)
        for i in 0..8 {
            args[i] = jit.get_register(i);
        }
        if trace_a64_svc_regs_enabled() {
            eprintln!(
                "[A64_SVC_ARGS] svc=0x{:x} pc=0x{:016x} lr=0x{:016x} sp=0x{:016x} x0=0x{:016x} x1=0x{:016x} x2=0x{:016x} x3=0x{:016x} x4=0x{:016x} x5=0x{:016x} x6=0x{:016x} x7=0x{:016x} x21=0x{:016x} x22=0x{:016x} x24=0x{:016x} x25=0x{:016x}",
                self.svc.load(Ordering::Relaxed),
                jit.get_pc(),
                jit.get_register(30),
                jit.get_sp(),
                args[0],
                args[1],
                args[2],
                args[3],
                args[4],
                args[5],
                args[6],
                args[7],
                jit.get_register(21),
                jit.get_register(22),
                jit.get_register(24),
                jit.get_register(25),
            );
        }
    }

    fn set_svc_arguments(&mut self, args: &[u64; 8]) {
        let jit = match self.jit.as_mut() {
            Some(jit) => jit,
            None => {
                log::warn!("ArmDynarmic64::set_svc_arguments: JIT not available");
                return;
            }
        };

        // Upstream: writes j.SetRegister(0..8, args[i])
        for i in 0..8 {
            jit.set_register(i, args[i]);
        }
    }

    fn get_svc_number(&self) -> u32 {
        self.svc.load(Ordering::Relaxed)
    }

    fn get_last_exception_address(&self) -> Option<u64> {
        let address = self.last_exception_address.load(Ordering::Relaxed);
        if address == 0 {
            None
        } else {
            Some(address)
        }
    }

    fn signal_interrupt(&mut self, _thread: &mut KThread) {
        // Upstream: m_jit->HaltExecution(BreakLoop)
        if let Some(jit) = self.jit.as_ref() {
            jit.halt_execution(rdynarmic::halt_reason::HaltReason::EXTERNAL_HALT);
        }
    }

    fn halted_watchpoint(&self) -> Option<&DebugWatchpoint> {
        self.halted_watchpoint.as_ref()
    }

    fn rewind_breakpoint_instruction(&mut self) {
        // Upstream: this->SetContext(m_breakpoint_context)
        let ctx = self.breakpoint_context.lock().unwrap().clone();
        self.set_context(&ctx);
    }
}

fn thread_context_from_jit_state(jit_state: &A64JitState, pc: u64) -> ThreadContext {
    let mut ctx = ThreadContext::default();
    ctx.r.copy_from_slice(&jit_state.reg[..29]);
    ctx.fp = jit_state.reg[29];
    ctx.lr = jit_state.reg[30];
    ctx.sp = jit_state.sp;
    ctx.pc = pc;
    ctx.pstate = jit_state.get_pstate();
    for i in 0..32 {
        let lo = jit_state.vec[i * 2] as u128;
        let hi = (jit_state.vec[i * 2 + 1] as u128) << 64;
        ctx.v[i] = lo | hi;
    }
    ctx.fpcr = jit_state.get_fpcr();
    ctx.fpsr = jit_state.get_fpsr();
    ctx.tpidr = jit_state.tpidr_el0;
    ctx
}

#[cfg(test)]
mod tests {
    use super::{
        parse_optimization_mask_env, parse_watch_ranges, thread_context_from_jit_state,
        translate_halt_reason,
    };
    use crate::arm::arm_interface::HaltReason;
    use rdynarmic::backend::x64::jit_state::A64JitState;

    #[test]
    fn parse_watch_ranges_accepts_hex_and_default_size() {
        assert_eq!(
            parse_watch_ranges("0x10,0x20:16"),
            vec![(0x10, 0x18), (0x20, 0x30)]
        );
    }

    #[test]
    fn thread_context_snapshot_matches_jit_state_layout() {
        let mut jit_state = A64JitState::new();
        for i in 0..31 {
            jit_state.reg[i] = 0x1000 + i as u64;
        }
        jit_state.sp = 0x2222;
        jit_state.set_pstate(0xA000_0000);
        jit_state.vec[0] = 0x0123_4567_89AB_CDEF;
        jit_state.vec[1] = 0x0FED_CBA9_7654_3210;
        jit_state.set_fpcr(0x0100_0000);
        jit_state.set_fpsr(0x0800_001F);
        jit_state.tpidr_el0 = 0x3333;

        let ctx = thread_context_from_jit_state(&jit_state, 0x4444);
        assert_eq!(ctx.r[0], 0x1000);
        assert_eq!(ctx.r[28], 0x1000 + 28);
        assert_eq!(ctx.fp, 0x1000 + 29);
        assert_eq!(ctx.lr, 0x1000 + 30);
        assert_eq!(ctx.sp, 0x2222);
        assert_eq!(ctx.pc, 0x4444);
        assert_eq!(ctx.pstate, 0xA000_0000);
        assert_eq!(ctx.v[0], 0x0FED_CBA9_7654_3210_0123_4567_89AB_CDEFu128);
        assert_eq!(ctx.fpcr, jit_state.get_fpcr());
        assert_eq!(ctx.fpsr, jit_state.get_fpsr());
        assert_eq!(ctx.tpidr, 0x3333);
    }

    #[test]
    fn parse_optimization_mask_env_accepts_hex() {
        unsafe {
            std::env::set_var("RUZU_A64_TEST_OPT_MASK", "0x3f");
        }
        assert_eq!(
            parse_optimization_mask_env("RUZU_A64_TEST_OPT_MASK"),
            Some(0x3f)
        );
        unsafe {
            std::env::remove_var("RUZU_A64_TEST_OPT_MASK");
        }
    }

    #[test]
    fn translate_halt_reason_uses_upstream_a64_bits() {
        let hr = rdynarmic::halt_reason::HaltReason::STEP
            | rdynarmic::halt_reason::HaltReason::MEMORY_ABORT
            | rdynarmic::halt_reason::HaltReason::SVC
            | rdynarmic::halt_reason::HaltReason::BREAKPOINT
            | rdynarmic::halt_reason::HaltReason::PREFETCH_ABORT
            | rdynarmic::halt_reason::HaltReason::EXTERNAL_HALT;
        let translated = translate_halt_reason(hr);
        assert!(translated.contains(HaltReason::STEP_THREAD));
        assert!(translated.contains(HaltReason::DATA_ABORT));
        assert!(translated.contains(HaltReason::SUPERVISOR_CALL));
        assert!(translated.contains(HaltReason::INSTRUCTION_BREAKPOINT));
        assert!(translated.contains(HaltReason::PREFETCH_ABORT));
        assert!(translated.contains(HaltReason::BREAK_LOOP));
    }

    #[test]
    fn translate_halt_reason_ignores_legacy_exception_raised_bit() {
        let translated =
            translate_halt_reason(rdynarmic::halt_reason::HaltReason::EXCEPTION_RAISED);
        assert!(translated.is_empty());
    }
}
