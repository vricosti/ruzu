// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/arm/dynarmic/arm_dynarmic_32.h and arm_dynarmic_32.cpp
//! ARM32 dynarmic backend.

use std::sync::atomic::{AtomicBool, AtomicU32, AtomicU64, Ordering};
use std::sync::Arc;

use crate::arm::arm_interface::{
    ArmInterface, ArmInterfaceBase, Architecture, DebugWatchpoint, HaltReason, KProcess,
    KThread, ThreadContext,
};
use crate::hle::kernel::k_process::{KProcess as RealKProcess, SharedProcessMemory};
use crate::memory::memory::Memory;

use rdynarmic::jit_config::{JitCallbacks, JitConfig, OptimizationFlag};

/// Translate rdynarmic's HaltReason to core's HaltReason.
///
/// Same mapping as in arm_dynarmic_64.rs.
fn translate_halt_reason(hr: rdynarmic::halt_reason::HaltReason) -> HaltReason {
    let mut result = HaltReason::empty();

    if hr.contains(rdynarmic::halt_reason::HaltReason::STEP) {
        result |= HaltReason::STEP_THREAD;
    }
    if hr.contains(rdynarmic::halt_reason::HaltReason::SVC) {
        result |= HaltReason::SUPERVISOR_CALL;
    }
    if hr.contains(rdynarmic::halt_reason::HaltReason::BREAKPOINT) {
        result |= HaltReason::INSTRUCTION_BREAKPOINT;
    }
    if hr.contains(rdynarmic::halt_reason::HaltReason::EXCEPTION_RAISED) {
        result |= HaltReason::PREFETCH_ABORT;
    }
    if hr.contains(rdynarmic::halt_reason::HaltReason::EXTERNAL_HALT) {
        result |= HaltReason::BREAK_LOOP;
    }

    result
}

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

fn parse_trace_hex_env(name: &str) -> Option<u32> {
    let value = std::env::var(name).ok()?;
    let trimmed = value.trim();
    let trimmed = trimmed
        .strip_prefix("0x")
        .or_else(|| trimmed.strip_prefix("0X"))
        .unwrap_or(trimmed);
    u32::from_str_radix(trimmed, 16).ok()
}

fn parse_trace_u32_env(name: &str) -> Option<u32> {
    std::env::var(name).ok()?.trim().parse().ok()
}

/// JIT callbacks for ARM32.
///
/// Corresponds to upstream `DynarmicCallbacks32`.
///
/// Holds a shared reference to guest process memory, matching upstream's
/// `Core::Memory::Memory& m_memory` obtained from `process->GetMemory()`.
///
/// Also holds a reference to CoreTiming for tick management, matching upstream's
/// `m_parent.m_system.CoreTiming()` access pattern.
struct DynarmicCallbacks32 {
    /// Shared guest memory reference (ProcessMemoryData).
    /// Used as fallback when core_memory is None (tests).
    memory: SharedProcessMemory,
    /// Core::Memory::Memory bridge (reads/writes via PageTable → DeviceMemory).
    /// Matches upstream `Core::Memory::Memory& m_memory`.
    /// None in tests where Memory is not wired.
    core_memory: Option<Arc<std::sync::Mutex<Memory>>>,
    /// SVC/SWI number from last supervisor call, shared with parent ArmDynarmic32.
    svc_swi: Arc<AtomicU32>,
    /// Whether wall clock is used (if true, ticking is disabled).
    /// Matches upstream `m_parent.m_uses_wall_clock`.
    uses_wall_clock: bool,
    /// Core timing reference for tick management.
    /// Matches upstream `m_parent.m_system.CoreTiming()`.
    core_timing: Arc<std::sync::Mutex<crate::core_timing::CoreTiming>>,
    /// Last exception address reported by dynarmic.
    last_exception_address: Arc<AtomicU64>,
    /// Raw pointer to the jit's halt_reason field (AtomicU32).
    /// Used by exception_raised() to halt execution exactly as upstream does
    /// via `m_parent.m_jit->HaltExecution(hr)`.
    /// Set after jit creation via `set_halt_reason_ptr()`.
    /// Safety: valid for the lifetime of the A32Jit that owns this callback.
    jit_halt_reason_ptr: Option<*const u32>,
    /// Raw pointer to jit_state.reg[15] (PC) for diagnostic logging.
    /// No performance impact — only read when logging unmapped accesses.
    /// Set after jit creation via `set_pc_ptr()`.
    jit_pc_ptr: Option<*const u32>,
    /// Raw pointer to jit_state.upper_location_descriptor for A32 mode diagnostics.
    /// Used to reconstruct Thumb/E/IT state when logging unmapped accesses.
    /// Ensures the optional diagnostic code dump only happens once.
    dumped_unmapped_window: AtomicBool,
    /// Shared exclusive monitor backing Dynarmic's global monitor state.
    exclusive_monitor: *mut crate::arm::dynarmic::dynarmic_exclusive_monitor::DynarmicExclusiveMonitor,
    /// CPU core index associated with this callback/JIT instance.
    core_index: usize,
}

// Safety: jit_halt_reason_ptr points to an AtomicU32 inside the heap-allocated
// JitInner, which is stable for the jit's lifetime. The pointer is only used
// for atomic operations (fetch_or) which are thread-safe.
unsafe impl Send for DynarmicCallbacks32 {}

impl DynarmicCallbacks32 {
    fn new(
        memory: SharedProcessMemory,
        core_memory: Option<Arc<std::sync::Mutex<Memory>>>,
        svc_swi: Arc<AtomicU32>,
        uses_wall_clock: bool,
        core_timing: Arc<std::sync::Mutex<crate::core_timing::CoreTiming>>,
        last_exception_address: Arc<AtomicU64>,
        exclusive_monitor: *mut crate::arm::dynarmic::dynarmic_exclusive_monitor::DynarmicExclusiveMonitor,
        core_index: usize,
    ) -> Self {
        log::info!("DynarmicCallbacks32: Arc memory ptr = {:?}, base = {:#x}, core_memory={}",
            std::sync::Arc::as_ptr(&memory),
            memory.read().unwrap().base,
            if core_memory.is_some() { "wired" } else { "fallback" });
        Self {
            memory,
            core_memory,
            svc_swi,
            uses_wall_clock,
            core_timing,
            last_exception_address,
            jit_halt_reason_ptr: None,
            jit_pc_ptr: None,
            dumped_unmapped_window: AtomicBool::new(false),
            exclusive_monitor,
            core_index,
        }
    }

    /// Halt the jit by atomically OR-ing a halt reason into the jit's halt_reason field.
    /// This is the Rust equivalent of upstream's `m_parent.m_jit->HaltExecution(hr)`.
    fn halt_execution(&self, reason: rdynarmic::halt_reason::HaltReason) {
        if let Some(ptr) = self.jit_halt_reason_ptr {
            let atomic = unsafe { &*(ptr as *const std::sync::atomic::AtomicU32) };
            atomic.fetch_or(reason.bits(), Ordering::Release);
        }
    }

    /// Matches upstream `DynarmicCallbacks32::CheckMemoryAccess`.
    ///
    /// Upstream behavior: `m_check_memory_access` is only true when
    /// `debugger_enabled || !cpuopt_ignore_memory_aborts`. The default is
    /// `cpuopt_ignore_memory_aborts = true`, so `m_check_memory_access = false`,
    /// meaning this function returns true immediately without checking.
    ///
    /// Memory access validation is a debugger feature, not used in normal play.
    /// The JIT uses page table fastmem for actual memory protection.
    fn check_memory_access(&self, _addr: u64, _size: u64) -> bool {
        // Upstream default: m_check_memory_access = false → return true.
        // When debugger support is added, this should check
        // is_valid_virtual_address_range and call halt_execution(EXCEPTION_RAISED)
        // on failure, matching upstream lines 150-174.
        true
    }

    fn log_guest_pc_on_unmapped_access(&self, access_kind: &str, vaddr: u64) {
        let enabled = std::env::var("RUZU_LOG_UNMAPPED_ACCESS_PC")
            .ok()
            .is_some_and(|value| value != "0");
        if !enabled {
            return;
        }

        let Some(pc_ptr) = self.jit_pc_ptr else {
            log::error!(
                "DynarmicCallbacks32 {} @ {:#010x}: jit_pc_ptr unavailable",
                access_kind,
                vaddr
            );
            return;
        };

        let pc = unsafe { *pc_ptr as u64 };
        let insn = if let Some(ref cm) = self.core_memory {
            let memory = cm.lock().unwrap();
            if memory.is_valid_virtual_address_range(pc, 4) {
                Some(memory.read_32(pc))
            } else {
                None
            }
        } else {
            let memory = self.memory.read().unwrap();
            if memory.is_valid_range(pc, 4) {
                Some(memory.read_32(pc))
            } else {
                None
            }
        };

        match insn {
            Some(insn) => log::error!(
                "DynarmicCallbacks32 {} @ {:#010x}: guest pc=0x{:08x} insn=0x{:08x}",
                access_kind,
                vaddr,
                pc,
                insn
            ),
            None => log::error!(
                "DynarmicCallbacks32 {} @ {:#010x}: guest pc=0x{:08x} insn=<unmapped>",
                access_kind,
                vaddr,
                pc
            ),
        }

        let log_regs = std::env::var("RUZU_LOG_UNMAPPED_ACCESS_REGS")
            .ok()
            .is_some_and(|value| value != "0");
        if log_regs {
            self.log_guest_registers_on_unmapped_access(access_kind, vaddr);
        }

        self.dump_guest_code_window_on_unmapped_access();
    }

    fn log_guest_registers_on_unmapped_access(&self, access_kind: &str, vaddr: u64) {
        let Some(pc_ptr) = self.jit_pc_ptr else {
            return;
        };

        // rdynarmic exposes jit_state.reg[15] as the PC pointer. The full
        // GPR bank is contiguous, so walk back to reg[0] for diagnostics.
        let regs_ptr = unsafe { pc_ptr.sub(15) };
        let regs = unsafe { std::slice::from_raw_parts(regs_ptr, 16) };
        let upper_location_descriptor = 0u32;
        let thumb = (upper_location_descriptor & 1) != 0;
        let it_state = ((upper_location_descriptor >> 8) & 0x3)
            | (((upper_location_descriptor >> 10) & 0x3f) << 2);
        log::error!(
            "DynarmicCallbacks32 {} @ {:#010x}: r0={:#010x} r1={:#010x} r2={:#010x} r3={:#010x} r4={:#010x} r5={:#010x} r6={:#010x} r7={:#010x}",
            access_kind,
            vaddr,
            regs[0],
            regs[1],
            regs[2],
            regs[3],
            regs[4],
            regs[5],
            regs[6],
            regs[7],
        );
        log::error!(
            "DynarmicCallbacks32 {} @ {:#010x}: r8={:#010x} r9={:#010x} r10={:#010x} r11={:#010x} r12={:#010x} sp={:#010x} lr={:#010x} pc={:#010x} upper={:#010x} thumb={} it={:#04x}",
            access_kind,
            vaddr,
            regs[8],
            regs[9],
            regs[10],
            regs[11],
            regs[12],
            regs[13],
            regs[14],
            regs[15],
            upper_location_descriptor,
            thumb,
            it_state,
        );
    }

    fn dump_guest_code_window_on_unmapped_access(&self) {
        let Ok(path) = std::env::var("RUZU_DUMP_UNMAPPED_CODE_PATH") else {
            return;
        };
        if path.is_empty() {
            return;
        }
        if self
            .dumped_unmapped_window
            .compare_exchange(false, true, Ordering::Relaxed, Ordering::Relaxed)
            .is_err()
        {
            return;
        }

        let Some(pc_ptr) = self.jit_pc_ptr else {
            return;
        };
        let pc = unsafe { *pc_ptr as u64 };
        let window_size = 0x100usize;
        let window_start = pc.saturating_sub(0x40) & !1;
        let mut bytes = vec![0u8; window_size];

        if let Some(ref cm) = self.core_memory {
            let memory = cm.lock().unwrap();
            if !memory.is_valid_virtual_address_range(window_start, window_size as u64) {
                return;
            }
            for (index, byte) in bytes.iter_mut().enumerate() {
                *byte = memory.read_8(window_start + index as u64);
            }
        } else {
            let memory = self.memory.read().unwrap();
            if !memory.is_valid_range(window_start, window_size) {
                return;
            }
            for (index, byte) in bytes.iter_mut().enumerate() {
                *byte = memory.read_8(window_start + index as u64);
            }
        }

        match std::fs::write(&path, &bytes) {
            Ok(()) => {
                log::error!(
                    "DynarmicCallbacks32 dumped guest code window: pc=0x{:08x} start=0x{:08x} size=0x{:x} path={}",
                    pc,
                    window_start,
                    window_size,
                    path
                );
            }
            Err(err) => {
                log::error!(
                    "DynarmicCallbacks32 failed to write guest code window: pc=0x{:08x} start=0x{:08x} size=0x{:x} path={} err={}",
                    pc,
                    window_start,
                    window_size,
                    path,
                    err
                );
            }
        }
    }
}

impl JitCallbacks for DynarmicCallbacks32 {
    fn memory_read_code(&self, vaddr: u64) -> Option<u32> {
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
        if !self
            .core_memory
            .as_ref()
            .map(|cm| cm.lock().unwrap().is_valid_virtual_address_range(vaddr, 1))
            .unwrap_or_else(|| self.memory.read().unwrap().is_valid_range(vaddr, 1))
        {
            self.log_guest_pc_on_unmapped_access("memory_read_8", vaddr);
        }
        if let Some(ref cm) = self.core_memory {
            cm.lock().unwrap().read_8(vaddr)
        } else {
            self.memory.read().unwrap().read_8(vaddr)
        }
    }

    fn memory_read_16(&self, vaddr: u64) -> u16 {
        self.check_memory_access(vaddr, 2);
        if !self
            .core_memory
            .as_ref()
            .map(|cm| cm.lock().unwrap().is_valid_virtual_address_range(vaddr, 2))
            .unwrap_or_else(|| self.memory.read().unwrap().is_valid_range(vaddr, 2))
        {
            self.log_guest_pc_on_unmapped_access("memory_read_16", vaddr);
        }
        if let Some(ref cm) = self.core_memory {
            cm.lock().unwrap().read_16(vaddr)
        } else {
            self.memory.read().unwrap().read_16(vaddr)
        }
    }

    fn memory_read_32(&self, vaddr: u64) -> u32 {
        self.check_memory_access(vaddr, 4);
        if !self
            .core_memory
            .as_ref()
            .map(|cm| cm.lock().unwrap().is_valid_virtual_address_range(vaddr, 4))
            .unwrap_or_else(|| self.memory.read().unwrap().is_valid_range(vaddr, 4))
        {
            self.log_guest_pc_on_unmapped_access("memory_read_32", vaddr);
        }
        let value = if let Some(ref cm) = self.core_memory {
            cm.lock().unwrap().read_32(vaddr)
        } else {
            self.memory.read().unwrap().read_32(vaddr)
        };

        value
    }

    fn memory_read_64(&self, vaddr: u64) -> u64 {
        self.check_memory_access(vaddr, 8);
        if let Some(ref cm) = self.core_memory {
            cm.lock().unwrap().read_64(vaddr)
        } else {
            self.memory.read().unwrap().read_64(vaddr)
        }
    }

    fn memory_read_128(&self, vaddr: u64) -> (u64, u64) {
        self.check_memory_access(vaddr, 16);
        if let Some(ref cm) = self.core_memory {
            let m = cm.lock().unwrap();
            (m.read_64(vaddr), m.read_64(vaddr + 8))
        } else {
            let mem = self.memory.read().unwrap();
            (mem.read_64(vaddr), mem.read_64(vaddr + 8))
        }
    }

    fn memory_write_8(&mut self, vaddr: u64, value: u8) {
        if !self.check_memory_access(vaddr, 1) { return; }
        if !self
            .core_memory
            .as_ref()
            .map(|cm| cm.lock().unwrap().is_valid_virtual_address_range(vaddr, 1))
            .unwrap_or_else(|| self.memory.read().unwrap().is_valid_range(vaddr, 1))
        {
            self.log_guest_pc_on_unmapped_access("memory_write_8", vaddr);
        }
        if let Some(ref cm) = self.core_memory {
            cm.lock().unwrap().write_8(vaddr, value);
        } else {
            self.memory.write().unwrap().write_8(vaddr, value);
        }
    }

    fn memory_write_16(&mut self, vaddr: u64, value: u16) {
        if !self.check_memory_access(vaddr, 2) { return; }
        if !self
            .core_memory
            .as_ref()
            .map(|cm| cm.lock().unwrap().is_valid_virtual_address_range(vaddr, 2))
            .unwrap_or_else(|| self.memory.read().unwrap().is_valid_range(vaddr, 2))
        {
            self.log_guest_pc_on_unmapped_access("memory_write_16", vaddr);
        }
        if let Some(ref cm) = self.core_memory {
            cm.lock().unwrap().write_16(vaddr, value);
        } else {
            self.memory.write().unwrap().write_16(vaddr, value);
        }
    }

    fn memory_write_32(&mut self, vaddr: u64, value: u32) {
        if !self.check_memory_access(vaddr, 4) { return; }
        if !self
            .core_memory
            .as_ref()
            .map(|cm| cm.lock().unwrap().is_valid_virtual_address_range(vaddr, 4))
            .unwrap_or_else(|| self.memory.read().unwrap().is_valid_range(vaddr, 4))
        {
            self.log_guest_pc_on_unmapped_access("memory_write_32", vaddr);
        }
        if let Some(ref cm) = self.core_memory {
            cm.lock().unwrap().write_32(vaddr, value);
        } else {
            self.memory.write().unwrap().write_32(vaddr, value);
        }
    }

    fn memory_write_64(&mut self, vaddr: u64, value: u64) {
        if !self.check_memory_access(vaddr, 8) { return; }
        if let Some(ref cm) = self.core_memory {
            cm.lock().unwrap().write_64(vaddr, value);
        } else {
            self.memory.write().unwrap().write_64(vaddr, value);
        }
    }

    fn memory_write_128(&mut self, vaddr: u64, value_lo: u64, value_hi: u64) {
        if !self.check_memory_access(vaddr, 16) { return; }
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
                cm.lock().unwrap().write_exclusive_16(vaddr, value, expected)
            } else {
                self.memory_write_16(vaddr, value);
                true
            }
    }

    fn exclusive_write_32(&mut self, vaddr: u64, value: u32, expected: u32) -> bool {
        if !self
            .core_memory
            .as_ref()
            .map(|cm| cm.lock().unwrap().is_valid_virtual_address_range(vaddr, 4))
            .unwrap_or_else(|| self.memory.read().unwrap().is_valid_range(vaddr, 4))
        {
            self.log_guest_pc_on_unmapped_access("exclusive_write_32", vaddr);
        }
        self.check_memory_access(vaddr, 4)
            && if let Some(ref cm) = self.core_memory {
                cm.lock().unwrap().write_exclusive_32(vaddr, value, expected)
            } else {
                self.memory_write_32(vaddr, value);
                true
            }
    }

    fn exclusive_write_64(&mut self, vaddr: u64, value: u64, expected: u64) -> bool {
        self.check_memory_access(vaddr, 8)
            && if let Some(ref cm) = self.core_memory {
                cm.lock().unwrap().write_exclusive_64(vaddr, value, expected)
            } else {
                self.memory_write_64(vaddr, value);
                true
            }
    }

    fn exclusive_write_128(&mut self, vaddr: u64, value_lo: u64, value_hi: u64, expected_lo: u64, expected_hi: u64) -> bool {
        self.check_memory_access(vaddr, 16)
            && if let Some(ref cm) = self.core_memory {
                cm.lock().unwrap().write_exclusive_128(vaddr, value_lo, value_hi, expected_lo, expected_hi)
            } else {
                self.memory_write_128(vaddr, value_lo, value_hi);
                true
            }
    }

    fn exclusive_clear(&mut self) {
        if !self.exclusive_monitor.is_null() {
            unsafe { (*self.exclusive_monitor).get_monitor().clear_processor(self.core_index) };
        }
    }

    fn call_supervisor(&mut self, svc_num: u32) {
        self.svc_swi.store(svc_num, Ordering::Relaxed);
        // Upstream DynarmicCallbacks32::CallSVC stores the SVC number and halts
        // JIT execution with SupervisorCall so the host can dispatch the SVC.
        self.halt_execution(rdynarmic::halt_reason::HaltReason::SVC);
    }

    fn exception_raised(&mut self, pc: u64, exception: u64) {
        // Port of upstream ExceptionRaised (arm_dynarmic_32.cpp:92-109).
        //
        // A32 Exception enum values:
        //   0=UndefinedInstruction, 1=UnpredictableInstruction, 2=DecodeError,
        //   3=SendEvent, 4=SendEventLocal, 5=WaitForInterrupt, 6=WaitForEvent,
        //   7=Yield, 8=Breakpoint, 9=PreloadData, 10=PreloadDataWithIntentToWrite,
        //   11=PreloadInstruction, 12=NoExecuteFault
        //
        // Upstream behavior:
        //   NoExecuteFault (12): log critical + ReturnException(pc, PrefetchAbort) → halts
        //   default (all others): if debugger, ReturnException(pc, InstructionBreakpoint)
        //                         else just log critical (NO halt — execution continues)
        match exception {
            12 => {
                // NoExecuteFault — halt execution.
                self.last_exception_address.store(pc, Ordering::Relaxed);
                log::error!("Cannot execute instruction at unmapped address {:#08x}", pc);
                self.halt_execution(rdynarmic::halt_reason::HaltReason::EXCEPTION_RAISED);
            }
            _ => {
                // All other exceptions: log and continue (no halt).
                // Upstream only halts here if debugger is enabled (not implemented).
                // Use trace level to avoid log spam — UndefinedInstruction (0) is
                // common during normal execution.
                log::trace!(
                    "ExceptionRaised(exception={}, pc={:#08x})",
                    exception, pc
                );
            }
        }
    }

    /// Matches upstream `DynarmicCallbacks32::AddTicks`:
    /// Divides ticks by NUM_CPU_CORES (4), passes to CoreTiming::AddTicks.
    fn add_ticks(&mut self, ticks: u64) {
        if self.uses_wall_clock {
            return;
        }
        // Divide by number of CPU cores, minimum 1 tick.
        // Matches upstream: amortized_ticks = max(ticks / NUM_CPU_CORES, 1)
        let amortized_ticks = std::cmp::max(ticks / crate::hardware_properties::NUM_CPU_CORES as u64, 1);
        self.core_timing.lock().unwrap().add_ticks(amortized_ticks);
    }

    /// Matches upstream `DynarmicCallbacks32::GetTicksRemaining`:
    /// Returns max(CoreTiming::GetDowncount(), 0).
    fn get_ticks_remaining(&self) -> u64 {
        if self.uses_wall_clock {
            return u64::MAX;
        }
        let ct = self.core_timing.lock().unwrap();
        std::cmp::max(ct.get_downcount(), 0) as u64
    }

    fn set_halt_reason_ptr(&mut self, ptr: *const u32) {
        self.jit_halt_reason_ptr = Some(ptr);
    }

    fn set_pc_ptr(&mut self, ptr: *const u32) {
        self.jit_pc_ptr = Some(ptr);
    }
}

/// ARM32 Dynarmic JIT backend.
///
/// Corresponds to upstream `Core::ArmDynarmic32`.
pub struct ArmDynarmic32 {
    pub base: ArmInterfaceBase,

    // Upstream holds `System& m_system` for accessing CoreTiming, DebuggerEnabled,
    // Settings, etc. Currently these are passed individually (core_timing, uses_wall_clock)
    // to avoid circular dependency with System which owns the ARM backends.
    // When System stabilizes, this can be replaced with a reference.

    /// Upstream: `DynarmicExclusiveMonitor& m_exclusive_monitor`.
    /// Passed to JitConfig::global_monitor for cross-core LDXR/STXR synchronization.
    exclusive_monitor: *mut crate::arm::dynarmic::dynarmic_exclusive_monitor::DynarmicExclusiveMonitor,

    /// Core index for this CPU
    core_index: usize,

    /// SVC callback number, shared with DynarmicCallbacks32.
    /// Upstream: m_svc_swi written by callback via m_parent reference.
    svc_swi: Arc<AtomicU32>,

    /// Watchpoint that caused a halt
    halted_watchpoint: Option<DebugWatchpoint>,

    /// Context saved at breakpoint
    breakpoint_context: ThreadContext,

    /// The rdynarmic A32 JIT instance
    jit: Option<rdynarmic::A32Jit>,

    /// CP15 user-read-only register (TPIDRURO), upstream: m_cp15->uro
    cp15_uro: u32,

    /// Last exception address reported by dynarmic for the current halt.
    last_exception_address: Arc<AtomicU64>,
}

impl ArmDynarmic32 {
    /// Create a new ARM32 dynarmic backend.
    ///
    /// Corresponds to upstream `ArmDynarmic32::ArmDynarmic32`.
    pub fn new(
        _system: &dyn std::any::Any,
        uses_wall_clock: bool,
        process: &KProcess,
        exclusive_monitor: *mut crate::arm::dynarmic::dynarmic_exclusive_monitor::DynarmicExclusiveMonitor,
        core_index: usize,
        shared_memory: SharedProcessMemory,
        core_timing: Arc<std::sync::Mutex<crate::core_timing::CoreTiming>>,
        core_memory: Option<Arc<std::sync::Mutex<Memory>>>,
    ) -> Self {
        // Get fastmem pointer from core_memory before moving it into callbacks.
        // Matches upstream: process->GetPageTable().GetBasePageTable().GetImpl().fastmem_arena
        let fastmem_pointer: Option<*mut u8> = core_memory
            .as_ref()
            .map(|cm| cm.lock().unwrap().fastmem_pointer())
            .filter(|p| !p.is_null());

        let svc_swi = Arc::new(AtomicU32::new(0));
        let last_exception_address = Arc::new(AtomicU64::new(0));
        let callbacks = DynarmicCallbacks32::new(
            shared_memory,
            core_memory,
            svc_swi.clone(),
            uses_wall_clock,
            core_timing,
            last_exception_address.clone(),
            exclusive_monitor,
            core_index,
        );

        let optimizations = if let Some(mask) = std::env::var("RUZU_A32_OPTIMIZATION_MASK")
            .ok()
            .and_then(|value| {
                let trimmed = value.trim();
                let digits = trimmed
                    .strip_prefix("0x")
                    .or_else(|| trimmed.strip_prefix("0X"))
                    .unwrap_or(trimmed);
                u32::from_str_radix(digits, 16)
                    .ok()
                    .or_else(|| trimmed.parse::<u32>().ok())
            })
        {
            optimization_flags_from_mask(mask)
        } else if std::env::var("RUZU_A32_NO_OPTIMIZATIONS")
            .ok()
            .is_some_and(|value| value != "0")
        {
            OptimizationFlag::NO_OPTIMIZATIONS
        } else {
            // All safe optimizations enabled, matching upstream default.
            optimization_flags_from_mask(0x3F)
        };

        let config = JitConfig {
            callbacks: Box::new(callbacks),
            enable_cycle_counting: !uses_wall_clock,
            code_cache_size: 512 * 1024 * 1024,
            optimizations,
            unsafe_optimizations: false,
            global_monitor: if exclusive_monitor.is_null() {
                None
            } else {
                Some(unsafe { (*exclusive_monitor).get_monitor() as *mut _ })
            },
            fastmem_pointer,
        };

        // A32Jit::new() internally calls callbacks.set_halt_reason_ptr() with a
        // pointer to jit_state.halt_reason, so exception_raised() can halt execution
        // exactly as upstream's m_parent.m_jit->HaltExecution(hr).
        let jit = match rdynarmic::A32Jit::new(config) {
            Ok(jit) => {
                log::info!("ArmDynarmic32: JIT created successfully for core {}", core_index);
                Some(jit)
            }
            Err(e) => {
                log::error!("ArmDynarmic32: Failed to create JIT for core {}: {}", core_index, e);
                None
            }
        };

        Self {
            base: ArmInterfaceBase::new(uses_wall_clock),
            exclusive_monitor,
            core_index,
            svc_swi,
            halted_watchpoint: None,
            breakpoint_context: ThreadContext::default(),
            jit,
            cp15_uro: 0,
            last_exception_address,
        }
    }

    /// Check if CPU is in Thumb mode.
    ///
    /// Corresponds to upstream `ArmDynarmic32::IsInThumbMode`.
    pub fn is_in_thumb_mode(&self) -> bool {
        if let Some(jit) = self.jit.as_ref() {
            // Thumb bit is bit 5 of CPSR
            (jit.get_cpsr() & 0x20) != 0
        } else {
            log::warn!("ArmDynarmic32::is_in_thumb_mode: JIT not available");
            false
        }
    }

    /// Convert FPSCR to separate FPSR and FPCR values.
    ///
    /// Corresponds to upstream `FpscrToFpsrFpcr`.
    fn fpscr_to_fpsr_fpcr(fpscr: u32) -> (u32, u32) {
        // FPSCR bits [31:27] -> FPSR[31:27]
        // FPSCR bit [7] -> FPSR[7]
        // FPSCR bits [4:0] -> FPSR[4:0]
        let nzcv = fpscr & 0xf800_0000;
        let idc = fpscr & 0x80;
        let fiq = fpscr & 0x1f;
        let fpsr = nzcv | idc | fiq;

        // FPSCR bits [26:15] -> FPCR[26:15]
        // FPSCR bits [12:8] -> FPCR[12:8]
        let round = fpscr & 0x07ff_8000;
        let trap = fpscr & 0x1f00;
        let fpcr = round | trap;

        (fpsr, fpcr)
    }

    /// Convert separate FPSR and FPCR values back to FPSCR.
    ///
    /// Corresponds to upstream `FpsrFpcrToFpscr`.
    fn fpsr_fpcr_to_fpscr(fpsr: u64, fpcr: u64) -> u32 {
        let combined = (fpsr as u32) | (fpcr as u32);
        let (s, c) = Self::fpscr_to_fpsr_fpcr(combined);
        s | c
    }
}

// SAFETY: ArmDynarmic32 holds raw pointers to long-lived objects
// (exclusive_monitor, watchpoints) that are valid for the lifetime of the process.
// The JIT is single-threaded per core — only one thread runs each ArmDynarmic32.
unsafe impl Send for ArmDynarmic32 {}

impl ArmInterface for ArmDynarmic32 {
    fn run_thread(&mut self, _thread: &mut KThread) -> HaltReason {
        self.last_exception_address.store(0, Ordering::Relaxed);
        let jit = match self.jit.as_mut() {
            Some(jit) => jit,
            None => {
                log::error!("ArmDynarmic32::run_thread: JIT not available");
                return HaltReason::BREAK_LOOP;
            }
        };

        jit.clear_exclusive_state();

        let trace_start = parse_trace_hex_env("RUZU_A32_TRACE_RANGE_START");
        let trace_end = parse_trace_hex_env("RUZU_A32_TRACE_RANGE_END");
        let trace_limit = parse_trace_u32_env("RUZU_A32_TRACE_LIMIT").unwrap_or(0);
        let trace_search_limit =
            parse_trace_u32_env("RUZU_A32_TRACE_SEARCH_LIMIT").unwrap_or(0);
        if let (Some(start), Some(end)) = (trace_start, trace_end) {
            let current_pc = jit.get_register(15);
            if trace_limit > 0 && (current_pc >= start && current_pc < end || trace_search_limit > 0) {
                let mut last_hr = rdynarmic::halt_reason::HaltReason::empty();
                let mut entered_range = current_pc >= start && current_pc < end;
                let mut logged_steps = 0u32;
                let total_limit = if entered_range {
                    trace_limit
                } else {
                    trace_search_limit.saturating_add(trace_limit)
                };
                for step in 0..total_limit {
                    let pc = jit.get_register(15);
                    if !entered_range {
                        log::info!(
                            "[A32TRACE] search_step={} pc=0x{:08x} cpsr=0x{:08x} r0=0x{:08x} r1=0x{:08x} r2=0x{:08x} r3=0x{:08x} sp=0x{:08x} lr=0x{:08x}",
                            step,
                            pc,
                            jit.get_cpsr(),
                            jit.get_register(0),
                            jit.get_register(1),
                            jit.get_register(2),
                            jit.get_register(3),
                            jit.get_register(13),
                            jit.get_register(14),
                        );
                        if pc >= start && pc < end {
                            entered_range = true;
                            log::info!(
                                "[A32TRACE] entered range at search_step={} pc=0x{:08x}",
                                step,
                                pc
                            );
                        } else {
                            last_hr = jit.step();
                            if !last_hr.is_empty() {
                                log::info!("[A32TRACE] halt while searching: {:?}", last_hr);
                                break;
                            }
                            if step + 1 >= trace_search_limit {
                                break;
                            }
                            continue;
                        }
                    }
                    if pc < start || pc >= end || logged_steps >= trace_limit {
                        break;
                    }
                    let cpsr = jit.get_cpsr();
                    log::info!(
                        "[A32TRACE] step={} search_step={} pc=0x{:08x} cpsr=0x{:08x} r0=0x{:08x} r1=0x{:08x} r2=0x{:08x} r3=0x{:08x} r4=0x{:08x} r5=0x{:08x} r6=0x{:08x} r7=0x{:08x} r8=0x{:08x} r9=0x{:08x} r10=0x{:08x} r11=0x{:08x} r12=0x{:08x} sp=0x{:08x} lr=0x{:08x}",
                        logged_steps,
                        step,
                        pc,
                        cpsr,
                        jit.get_register(0),
                        jit.get_register(1),
                        jit.get_register(2),
                        jit.get_register(3),
                        jit.get_register(4),
                        jit.get_register(5),
                        jit.get_register(6),
                        jit.get_register(7),
                        jit.get_register(8),
                        jit.get_register(9),
                        jit.get_register(10),
                        jit.get_register(11),
                        jit.get_register(12),
                        jit.get_register(13),
                        jit.get_register(14),
                    );
                    logged_steps += 1;
                    last_hr = jit.step();
                    if !last_hr.is_empty() {
                        log::info!("[A32TRACE] halt={:?}", last_hr);
                        break;
                    }
                }
                return translate_halt_reason(last_hr);
            }
        }

        let rdynarmic_hr = jit.run();
        translate_halt_reason(rdynarmic_hr)
    }

    fn step_thread(&mut self, _thread: &mut KThread) -> HaltReason {
        self.last_exception_address.store(0, Ordering::Relaxed);
        let jit = match self.jit.as_mut() {
            Some(jit) => jit,
            None => {
                log::error!("ArmDynarmic32::step_thread: JIT not available");
                return HaltReason::BREAK_LOOP;
            }
        };

        jit.clear_exclusive_state();
        let rdynarmic_hr = jit.step();
        translate_halt_reason(rdynarmic_hr)
    }

    fn clear_instruction_cache(&mut self) {
        if let Some(jit) = self.jit.as_mut() {
            jit.clear_cache();
        }
    }

    fn invalidate_cache_range(&mut self, addr: u64, size: usize) {
        if let Some(jit) = self.jit.as_mut() {
            // Upstream casts addr to u32 for A32
            jit.invalidate_cache_range(addr, size as u64);
        }
    }

    fn get_architecture(&self) -> Architecture {
        Architecture::AArch32
    }

    fn get_context(&self, ctx: &mut ThreadContext) {
        let jit = match self.jit.as_ref() {
            Some(jit) => jit,
            None => {
                log::warn!("ArmDynarmic32::get_context: JIT not available");
                return;
            }
        };

        // Upstream maps A32 GPRs to ThreadContext:
        // GPR[0..15] -> ctx.r[0..15], rest zeroed
        for i in 0..16 {
            ctx.r[i] = jit.get_register(i) as u64;
        }
        ctx.fp = jit.get_register(11) as u64;
        // r[15] is PC in A32
        ctx.pc = jit.get_register(15) as u64;
        ctx.sp = jit.get_register(13) as u64;
        ctx.lr = jit.get_register(14) as u64;

        ctx.pstate = jit.get_cpsr();

        // ExtRegs -> Vectors (A32 uses VFP/NEON extension registers)
        // Upstream reads 64 ExtRegs (u32 each) and maps groups of 4 to u128 vectors.
        // ext_reg layout: 64 x u32, where ext_reg[i*4..i*4+4] maps to ctx.v[i].
        for i in 0..16 {
            let e0 = jit.get_ext_reg(i * 4) as u128;
            let e1 = jit.get_ext_reg(i * 4 + 1) as u128;
            let e2 = jit.get_ext_reg(i * 4 + 2) as u128;
            let e3 = jit.get_ext_reg(i * 4 + 3) as u128;
            ctx.v[i] = e0 | (e1 << 32) | (e2 << 64) | (e3 << 96);
        }
        // A32 only has 16 Q-registers (D0-D31 / S0-S63)
        for i in 16..32 {
            ctx.v[i] = 0;
        }

        let (fpsr, fpcr) = Self::fpscr_to_fpsr_fpcr(jit.get_fpscr());
        ctx.fpcr = fpcr;
        ctx.fpsr = fpsr;
        ctx.tpidr = jit.get_cp15_uprw() as u64; // Upstream: ctx.tpidr = m_cp15->uprw
    }

    fn set_context(&mut self, ctx: &ThreadContext) {
        let jit = match self.jit.as_mut() {
            Some(jit) => jit,
            None => {
                log::warn!("ArmDynarmic32::set_context: JIT not available");
                return;
            }
        };

        // Upstream maps ThreadContext back to A32 GPRs.
        // Upstream loops 0..16, reading all 16 GPRs (including R15/PC) from ctx.r[i].
        for i in 0..16 {
            jit.set_register(i, ctx.r[i] as u32);
        }

        jit.set_cpsr(ctx.pstate);

        // Vectors -> ExtRegs
        for i in 0..16 {
            jit.set_ext_reg(i * 4, ctx.v[i] as u32);
            jit.set_ext_reg(i * 4 + 1, (ctx.v[i] >> 32) as u32);
            jit.set_ext_reg(i * 4 + 2, (ctx.v[i] >> 64) as u32);
            jit.set_ext_reg(i * 4 + 3, (ctx.v[i] >> 96) as u32);
        }

        let fpscr = Self::fpsr_fpcr_to_fpscr(ctx.fpsr as u64, ctx.fpcr as u64);
        jit.set_fpscr(fpscr);
        // Upstream: m_cp15->uprw = static_cast<u32>(ctx.tpidr)
        jit.set_cp15_uprw(ctx.tpidr as u32);
    }

    fn set_tpidrro_el0(&mut self, value: u64) {
        // Upstream: m_cp15->uro = static_cast<u32>(value)
        self.cp15_uro = value as u32;
        if let Some(jit) = self.jit.as_mut() {
            jit.set_cp15_uro(value as u32);
        }
    }

    fn get_svc_arguments(&self, args: &mut [u64; 8]) {
        let jit = match self.jit.as_ref() {
            Some(jit) => jit,
            None => {
                log::warn!("ArmDynarmic32::get_svc_arguments: JIT not available");
                return;
            }
        };

        // Upstream reads GPR[0..8] from JIT
        for i in 0..8 {
            args[i] = jit.get_register(i) as u64;
        }
    }

    fn set_svc_arguments(&mut self, args: &[u64; 8]) {
        let jit = match self.jit.as_mut() {
            Some(jit) => jit,
            None => {
                log::warn!("ArmDynarmic32::set_svc_arguments: JIT not available");
                return;
            }
        };

        // Upstream writes GPR[0..8] to JIT as u32
        for i in 0..8 {
            jit.set_register(i, args[i] as u32);
        }
    }

    fn get_svc_number(&self) -> u32 {
        self.svc_swi.load(Ordering::Relaxed)
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
        if let Some(jit) = self.jit.as_ref() {
            jit.halt_execution(rdynarmic::halt_reason::HaltReason::EXTERNAL_HALT);
        }
    }

    fn halted_watchpoint(&self) -> Option<&DebugWatchpoint> {
        self.halted_watchpoint.as_ref()
    }

    fn rewind_breakpoint_instruction(&mut self) {
        let ctx = self.breakpoint_context.clone();
        self.set_context(&ctx);
    }
}
