// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/arm/dynarmic/arm_dynarmic_32.h and arm_dynarmic_32.cpp
//! ARM32 dynarmic backend.

use std::sync::atomic::{AtomicBool, AtomicPtr, AtomicU32, AtomicU64, Ordering};
use std::sync::Arc;

use crate::arm::arm_interface::{
    Architecture, ArmInterface, ArmInterfaceBase, DebugWatchpoint, HaltReason, KProcess, KThread,
    ThreadContext,
};
use crate::hle::kernel::k_process::SharedProcessMemory;
use crate::memory::memory::Memory;

use rdynarmic::jit_config::{JitConfig, OptimizationFlag, UserCallbacks};

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
/// Upstream fields: `m_parent`, `m_memory`, `m_process`, `m_debugger_enabled`,
/// `m_check_memory_access`. All other state (svc_swi, core_timing, exclusive_monitor,
/// core_index, etc.) is accessed through `m_parent`.
///
/// In Rust, `parent` is a raw pointer set post-construction via `set_parent_ptr()`,
/// matching upstream's reference-based `m_parent`. The pointer is null during JIT
/// construction but is set immediately after. All callback methods are only called
/// by the JIT during `run_thread()`, at which point parent is guaranteed to be set.
struct DynarmicCallbacks32 {
    /// Upstream: `ArmDynarmic32& m_parent`.
    /// Shared atomic pointer set post-construction by the parent ArmDynarmic32.
    /// Uses AtomicPtr so the parent can set it after JIT creation without needing
    /// mutable access to the callbacks (which are consumed by the JIT).
    /// Safety: once set, valid for the lifetime of the parent ArmDynarmic32.
    parent: Arc<AtomicPtr<ArmDynarmic32>>,
    /// Upstream: `Core::Memory::Memory& m_memory`.
    /// None in tests where Memory is not wired.
    core_memory: Option<Arc<std::sync::Mutex<Memory>>>,
    /// Upstream: `Kernel::KProcess* m_process`.
    /// Raw pointer to the owning process, used for LogBacktrace.
    /// Safety: valid for the lifetime of the KProcess that owns this JIT.
    process: *const crate::hle::kernel::k_process::KProcess,
    /// Shared guest memory reference (ProcessMemoryData).
    /// Used as fallback when core_memory is None (tests).
    /// Not in upstream, but needed for Rust fallback path.
    memory: SharedProcessMemory,
    /// Cached fastmem pointer for lock-free memory reads during code translation.
    /// Matches upstream's direct `m_memory` reference (no synchronization).
    /// Safety: valid for the lifetime of the DeviceMemory backing.
    fastmem_ptr: *const u8,
    /// Raw pointer to jit_state.reg[15] (PC) for diagnostic logging.
    /// Set after jit creation via `set_pc_ptr()`.
    /// Not in upstream, but needed since we don't have debugger.
    jit_pc_ptr: Option<*const u32>,
}

// Safety: The raw pointers (parent, process, fastmem_ptr, jit_pc_ptr) all point to
// objects that are stable for the JIT's lifetime. The JIT is single-threaded per core.
unsafe impl Send for DynarmicCallbacks32 {}

impl DynarmicCallbacks32 {
    fn new(
        memory: SharedProcessMemory,
        core_memory: Option<Arc<std::sync::Mutex<Memory>>>,
        process: *const crate::hle::kernel::k_process::KProcess,
        parent_ptr: Arc<AtomicPtr<ArmDynarmic32>>,
    ) -> Self {
        // Cache fastmem pointer for lock-free code reads during JIT compilation.
        // Matches upstream's direct m_memory reference.
        let fastmem_ptr = core_memory
            .as_ref()
            .map(|cm| cm.lock().unwrap().fastmem_pointer())
            .unwrap_or(std::ptr::null_mut()) as *const u8;
        log::info!(
            "DynarmicCallbacks32: core_memory={} fastmem_ptr={:?}",
            if core_memory.is_some() {
                "wired"
            } else {
                "fallback"
            },
            fastmem_ptr
        );
        Self {
            parent: parent_ptr,
            core_memory,
            process,
            memory,
            fastmem_ptr,
            jit_pc_ptr: None,
        }
    }

    /// Get a reference to the parent ArmDynarmic32.
    /// All callback methods are only called by the JIT during run_thread(),
    /// at which point parent is guaranteed to be set by ArmDynarmic32::new().
    ///
    /// Corresponds to upstream's `m_parent` reference.
    fn parent(&self) -> &ArmDynarmic32 {
        let ptr = self.parent.load(Ordering::Acquire);
        debug_assert!(
            !ptr.is_null(),
            "DynarmicCallbacks32::parent() called before parent pointer was set"
        );
        unsafe { &*ptr }
    }

    /// Halt the jit execution with the given reason.
    /// This is the Rust equivalent of upstream's `m_parent.m_jit->HaltExecution(hr)`.
    fn halt_execution(&self, reason: rdynarmic::halt_reason::HaltReason) {
        if let Some(jit) = self.parent().jit.as_ref() {
            jit.halt_execution(reason);
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
    /// Access `m_memory` — returns a lock guard on the Memory bridge.
    /// Matches upstream's `m_memory` reference (Core::Memory::Memory&).
    /// Panics if core_memory is not wired (only happens in tests).
    fn mem(&self) -> std::sync::MutexGuard<'_, Memory> {
        self.core_memory
            .as_ref()
            .expect("core_memory not wired")
            .lock()
            .unwrap()
    }

    /// Matches upstream `DynarmicCallbacks32::CheckMemoryAccess`.
    /// Default: no check (m_check_memory_access = false).
    fn check_memory_access(&self, _addr: u64, _size: u64) -> bool {
        true
    }
}

impl UserCallbacks for DynarmicCallbacks32 {
    fn memory_read_code(&self, vaddr: u64) -> Option<u32> {
        // Lock-free fast path using cached fastmem pointer, matching upstream's
        // direct m_memory.Read32(vaddr) without any synchronization.
        if !self.fastmem_ptr.is_null() {
            let value =
                unsafe { (self.fastmem_ptr.add(vaddr as usize) as *const u32).read_unaligned() };
            return Some(value);
        }
        // Fallback (tests without fastmem): lock and read
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
        self.mem().read_8(vaddr)
    }

    fn memory_read_16(&self, vaddr: u64) -> u16 {
        self.check_memory_access(vaddr, 2);
        self.mem().read_16(vaddr)
    }

    fn memory_read_32(&self, vaddr: u64) -> u32 {
        self.check_memory_access(vaddr, 4);
        self.mem().read_32(vaddr)
    }

    fn memory_read_64(&self, vaddr: u64) -> u64 {
        self.check_memory_access(vaddr, 8);
        self.mem().read_64(vaddr)
    }

    fn memory_read_128(&self, vaddr: u64) -> (u64, u64) {
        self.check_memory_access(vaddr, 16);
        let m = self.mem();
        (m.read_64(vaddr), m.read_64(vaddr + 8))
    }

    fn memory_write_8(&mut self, vaddr: u64, value: u8) {
        if self.check_memory_access(vaddr, 1) {
            self.mem().write_8(vaddr, value);
        }
    }

    fn memory_write_16(&mut self, vaddr: u64, value: u16) {
        if self.check_memory_access(vaddr, 2) {
            self.mem().write_16(vaddr, value);
        }
    }

    fn memory_write_32(&mut self, vaddr: u64, value: u32) {
        if self.check_memory_access(vaddr, 4) {
            self.mem().write_32(vaddr, value);
        }
    }

    fn memory_write_64(&mut self, vaddr: u64, value: u64) {
        if self.check_memory_access(vaddr, 8) {
            self.mem().write_64(vaddr, value);
        }
    }

    fn memory_write_128(&mut self, vaddr: u64, value_lo: u64, value_hi: u64) {
        if self.check_memory_access(vaddr, 16) {
            let m = self.mem();
            m.write_64(vaddr, value_lo);
            m.write_64(vaddr + 8, value_hi);
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
        self.check_memory_access(vaddr, 1) && self.mem().write_exclusive_8(vaddr, value, expected)
    }

    fn exclusive_write_16(&mut self, vaddr: u64, value: u16, expected: u16) -> bool {
        self.check_memory_access(vaddr, 2) && self.mem().write_exclusive_16(vaddr, value, expected)
    }

    fn exclusive_write_32(&mut self, vaddr: u64, value: u32, expected: u32) -> bool {
        self.check_memory_access(vaddr, 4) && self.mem().write_exclusive_32(vaddr, value, expected)
    }

    fn exclusive_write_64(&mut self, vaddr: u64, value: u64, expected: u64) -> bool {
        self.check_memory_access(vaddr, 8) && self.mem().write_exclusive_64(vaddr, value, expected)
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
            && self
                .mem()
                .write_exclusive_128(vaddr, value_lo, value_hi, expected_lo, expected_hi)
    }

    fn exclusive_clear(&mut self) {
        // Upstream: m_parent.m_exclusive_monitor.ClearProcessor(m_parent.m_core_index)
        let parent = self.parent();
        if !parent.exclusive_monitor.is_null() {
            unsafe {
                (*parent.exclusive_monitor)
                    .get_monitor()
                    .clear_processor(parent.core_index)
            };
        }
    }

    fn call_supervisor(&mut self, svc_num: u32) {
        // Upstream: m_parent.m_svc_swi = swi;
        //           m_parent.m_jit->HaltExecution(SupervisorCall);
        self.parent().svc_swi.store(svc_num, Ordering::Relaxed);
        self.halt_execution(rdynarmic::halt_reason::HaltReason::SVC);
    }

    fn exception_raised(&mut self, pc: u64, exception: u64) {
        use rdynarmic::frontend::a32::types::Exception;

        // Port of upstream ExceptionRaised (arm_dynarmic_32.cpp:92-109).
        //
        // Upstream behavior:
        //   NoExecuteFault: ReturnException(pc, PrefetchAbort) -> halts
        //   default:        if debugger -> ReturnException(pc, InstructionBreakpoint)
        //                   else -> LogBacktrace + LOG_CRITICAL (NO halt, continues)
        //   Hints (SEV/WFI/WFE/Yield): handled by IR as no-ops, never reach here
        match exception {
            x if x == Exception::NoExecuteFault.as_u32() as u64 => {
                log::error!("Cannot execute instruction at unmapped address {:#08x}", pc);
                // Upstream: ReturnException(pc, PrefetchAbort)
                // Store the exception address so the parent can retrieve it.
                self.parent()
                    .last_exception_address
                    .store(pc, Ordering::Relaxed);
                self.halt_execution(rdynarmic::halt_reason::HaltReason::EXCEPTION_RAISED);
            }
            _ => {
                static LOGGED_UDF_CONTEXT: AtomicBool = AtomicBool::new(false);
                let mut ctx = ThreadContext::default();
                self.parent().get_context(&mut ctx);

                if pc as u32 == 0x01D1_DD20 && !LOGGED_UDF_CONTEXT.swap(true, Ordering::Relaxed) {
                    let read32 = |addr: u64, this: &mut Self| -> u32 {
                        if this.check_memory_access(addr, 4) {
                            this.mem().read_32(addr)
                        } else {
                            0
                        }
                    };
                    log::error!(
                        "UDF context pc={:08X} sp={:08X} lr={:08X} fp={:08X} cpsr={:08X} r0={:08X} r1={:08X} r2={:08X} r3={:08X} r4={:08X} r5={:08X} r6={:08X} r7={:08X}",
                        pc as u32,
                        ctx.sp,
                        ctx.lr,
                        ctx.r[11],
                        ctx.pstate,
                        ctx.r[0],
                        ctx.r[1],
                        ctx.r[2],
                        ctx.r[3],
                        ctx.r[4],
                        ctx.r[5],
                        ctx.r[6],
                        ctx.r[7],
                    );
                    log::error!(
                        "UDF code window pc=[{:08X} {:08X} {:08X}] lr=[{:08X} {:08X} {:08X} {:08X}]",
                        read32(pc.saturating_sub(4), self),
                        read32(pc, self),
                        read32(pc + 4, self),
                        read32(ctx.lr.saturating_sub(8), self),
                        read32(ctx.lr.saturating_sub(4), self),
                        read32(ctx.lr, self),
                        read32(ctx.lr + 4, self),
                    );
                    log::error!(
                        "UDF lr-24..lr+8 [{:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X}]",
                        read32(ctx.lr.saturating_sub(24), self),
                        read32(ctx.lr.saturating_sub(20), self),
                        read32(ctx.lr.saturating_sub(16), self),
                        read32(ctx.lr.saturating_sub(12), self),
                        read32(ctx.lr.saturating_sub(8), self),
                        read32(ctx.lr.saturating_sub(4), self),
                        read32(ctx.lr, self),
                        read32(ctx.lr + 4, self),
                        read32(ctx.lr + 8, self),
                    );
                    log::error!(
                        "UDF lr-40..lr-28 [{:08X} {:08X} {:08X} {:08X}]",
                        read32(ctx.lr.saturating_sub(40), self),
                        read32(ctx.lr.saturating_sub(36), self),
                        read32(ctx.lr.saturating_sub(32), self),
                        read32(ctx.lr.saturating_sub(28), self),
                    );
                    log::error!(
                        "UDF sp..sp+32 [{:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X}]",
                        read32(ctx.sp as u64, self),
                        read32(ctx.sp as u64 + 4, self),
                        read32(ctx.sp as u64 + 8, self),
                        read32(ctx.sp as u64 + 12, self),
                        read32(ctx.sp as u64 + 16, self),
                        read32(ctx.sp as u64 + 20, self),
                        read32(ctx.sp as u64 + 24, self),
                        read32(ctx.sp as u64 + 28, self),
                        read32(ctx.sp as u64 + 32, self),
                    );
                    log::error!(
                        "UDF r1..r1+32 [{:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X}]",
                        read32(ctx.r[1] as u64, self),
                        read32(ctx.r[1] as u64 + 4, self),
                        read32(ctx.r[1] as u64 + 8, self),
                        read32(ctx.r[1] as u64 + 12, self),
                        read32(ctx.r[1] as u64 + 16, self),
                        read32(ctx.r[1] as u64 + 20, self),
                        read32(ctx.r[1] as u64 + 24, self),
                        read32(ctx.r[1] as u64 + 28, self),
                        read32(ctx.r[1] as u64 + 32, self),
                    );
                    log::error!(
                        "UDF r4..r4+32 [{:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X}]",
                        read32(ctx.r[4] as u64, self),
                        read32(ctx.r[4] as u64 + 4, self),
                        read32(ctx.r[4] as u64 + 8, self),
                        read32(ctx.r[4] as u64 + 12, self),
                        read32(ctx.r[4] as u64 + 16, self),
                        read32(ctx.r[4] as u64 + 20, self),
                        read32(ctx.r[4] as u64 + 24, self),
                        read32(ctx.r[4] as u64 + 28, self),
                        read32(ctx.r[4] as u64 + 32, self),
                    );
                    log::error!(
                        "UDF r4 fields [r4+8]={:08X} [r4+18]={:02X} [r4+34]={:08X}",
                        read32(ctx.r[4] as u64 + 8, self),
                        if self.check_memory_access(ctx.r[4] as u64 + 0x18, 1) {
                            self.mem().read_8(ctx.r[4] as u64 + 0x18)
                        } else {
                            0
                        },
                        read32(ctx.r[4] as u64 + 0x34, self),
                    );
                    log::error!(
                        "UDF r6..r6+32 [{:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X}]",
                        read32(ctx.r[6] as u64, self),
                        read32(ctx.r[6] as u64 + 4, self),
                        read32(ctx.r[6] as u64 + 8, self),
                        read32(ctx.r[6] as u64 + 12, self),
                        read32(ctx.r[6] as u64 + 16, self),
                        read32(ctx.r[6] as u64 + 20, self),
                        read32(ctx.r[6] as u64 + 24, self),
                        read32(ctx.r[6] as u64 + 28, self),
                        read32(ctx.r[6] as u64 + 32, self),
                    );
                    log::error!(
                        "UDF fp-0x140..fp-0x100 [{:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X}]",
                        read32(ctx.r[11] as u64 - 0x140, self),
                        read32(ctx.r[11] as u64 - 0x13C, self),
                        read32(ctx.r[11] as u64 - 0x138, self),
                        read32(ctx.r[11] as u64 - 0x134, self),
                        read32(ctx.r[11] as u64 - 0x130, self),
                        read32(ctx.r[11] as u64 - 0x12C, self),
                        read32(ctx.r[11] as u64 - 0x128, self),
                        read32(ctx.r[11] as u64 - 0x124, self),
                        read32(ctx.r[11] as u64 - 0x120, self),
                        read32(ctx.r[11] as u64 - 0x11C, self),
                        read32(ctx.r[11] as u64 - 0x118, self),
                        read32(ctx.r[11] as u64 - 0x114, self),
                        read32(ctx.r[11] as u64 - 0x110, self),
                        read32(ctx.r[11] as u64 - 0x10C, self),
                        read32(ctx.r[11] as u64 - 0x108, self),
                        read32(ctx.r[11] as u64 - 0x104, self),
                        read32(ctx.r[11] as u64 - 0x100, self),
                    );
                    log::error!(
                        "UDF fp chain [fp]={:08X} [fp+4]={:08X} [fp+8]={:08X} [fp+12]={:08X}",
                        read32(ctx.r[11] as u64, self),
                        read32(ctx.r[11] as u64 + 4, self),
                        read32(ctx.r[11] as u64 + 8, self),
                        read32(ctx.r[11] as u64 + 12, self),
                    );
                    let caller_fp = read32(ctx.r[11] as u64, self) as u64;
                    if caller_fp != 0 {
                        log::error!(
                            "UDF caller fp chain [caller_fp]={:08X} [caller_fp+4]={:08X} [caller_fp+8]={:08X} [caller_fp+12]={:08X}",
                            read32(caller_fp, self),
                            read32(caller_fp + 4, self),
                            read32(caller_fp + 8, self),
                            read32(caller_fp + 12, self),
                        );
                        let caller2_fp = read32(caller_fp, self) as u64;
                        if caller2_fp != 0 {
                            log::error!(
                                "UDF caller2 fp chain [caller2_fp]={:08X} [caller2_fp+4]={:08X} [caller2_fp+8]={:08X} [caller2_fp+12]={:08X}",
                                read32(caller2_fp, self),
                                read32(caller2_fp + 4, self),
                                read32(caller2_fp + 8, self),
                                read32(caller2_fp + 12, self),
                            );
                        }
                    }
                    log::error!(
                        "UDF guest 0x2499B70.. [{:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X}]",
                        read32(0x0249_9B70, self),
                        read32(0x0249_9B74, self),
                        read32(0x0249_9B78, self),
                        read32(0x0249_9B7C, self),
                        read32(0x0249_9B80, self),
                        read32(0x0249_9B84, self),
                        read32(0x0249_9B88, self),
                        read32(0x0249_9B8C, self),
                    );
                    log::error!(
                        "UDF guest 0x2499B90.. [{:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X}]",
                        read32(0x0249_9B90, self),
                        read32(0x0249_9B94, self),
                        read32(0x0249_9B98, self),
                        read32(0x0249_9B9C, self),
                        read32(0x0249_9BA0, self),
                        read32(0x0249_9BA4, self),
                        read32(0x0249_9BA8, self),
                        read32(0x0249_9BAC, self),
                    );
                    log::error!(
                        "UDF guest 0x2499BB0.. [{:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X}]",
                        read32(0x0249_9BB0, self),
                        read32(0x0249_9BB4, self),
                        read32(0x0249_9BB8, self),
                        read32(0x0249_9BBC, self),
                        read32(0x0249_9BC0, self),
                        read32(0x0249_9BC4, self),
                        read32(0x0249_9BC8, self),
                        read32(0x0249_9BCC, self),
                    );
                    log::error!(
                        "UDF bl1 0x020202C8 [{:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X}]",
                        read32(0x0202_02C8, self),
                        read32(0x0202_02CC, self),
                        read32(0x0202_02D0, self),
                        read32(0x0202_02D4, self),
                        read32(0x0202_02D8, self),
                        read32(0x0202_02DC, self),
                        read32(0x0202_02E0, self),
                        read32(0x0202_02E4, self),
                    );
                    log::error!(
                        "UDF bl2 0x02022014 [{:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X}]",
                        read32(0x0202_2014, self),
                        read32(0x0202_2018, self),
                        read32(0x0202_201C, self),
                        read32(0x0202_2020, self),
                        read32(0x0202_2024, self),
                        read32(0x0202_2028, self),
                        read32(0x0202_202C, self),
                        read32(0x0202_2030, self),
                    );
                    log::error!(
                        "UDF bl slots [0x022C3380]={:08X} [0x022C3D44]={:08X}",
                        read32(0x022C_3380, self),
                        read32(0x022C_3D44, self),
                    );
                    log::error!(
                        "UDF bl1 target 0x0201D5E4 [{:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X}]",
                        read32(0x0201_D5E4, self),
                        read32(0x0201_D5E8, self),
                        read32(0x0201_D5EC, self),
                        read32(0x0201_D5F0, self),
                        read32(0x0201_D5F4, self),
                        read32(0x0201_D5F8, self),
                        read32(0x0201_D5FC, self),
                        read32(0x0201_D600, self),
                    );
                    log::error!(
                        "UDF pre-abort helper1 0x02020F1C [{:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X}]",
                        read32(0x0202_0F1C, self),
                        read32(0x0202_0F20, self),
                        read32(0x0202_0F24, self),
                        read32(0x0202_0F28, self),
                        read32(0x0202_0F2C, self),
                        read32(0x0202_0F30, self),
                        read32(0x0202_0F34, self),
                        read32(0x0202_0F38, self),
                    );
                    log::error!(
                        "UDF pre-abort helper2 0x02020F24 [{:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X}]",
                        read32(0x0202_0F24, self),
                        read32(0x0202_0F28, self),
                        read32(0x0202_0F2C, self),
                        read32(0x0202_0F30, self),
                        read32(0x0202_0F34, self),
                        read32(0x0202_0F38, self),
                        read32(0x0202_0F3C, self),
                        read32(0x0202_0F40, self),
                    );
                    log::error!(
                        "UDF pre-abort helper0 0x020293BC [{:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X}]",
                        read32(0x0202_93BC, self),
                        read32(0x0202_93C0, self),
                        read32(0x0202_93C4, self),
                        read32(0x0202_93C8, self),
                        read32(0x0202_93CC, self),
                        read32(0x0202_93D0, self),
                        read32(0x0202_93D4, self),
                        read32(0x0202_93D8, self),
                    );
                    let helper_slot_0 = read32(0x022C_379C, self);
                    let helper_slot_1 = read32(0x022C_37A0, self);
                    let helper_slot_2 = read32(0x022C_37A4, self);
                    log::error!(
                        "UDF pre-abort helper slots [0x022C379C]={:08X} [0x022C37A0]={:08X} [0x022C37A4]={:08X}",
                        helper_slot_0,
                        helper_slot_1,
                        helper_slot_2,
                    );
                    log::error!(
                        "UDF pre-abort helper slot0 target [{:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X}]",
                        read32(helper_slot_0 as u64, self),
                        read32(helper_slot_0 as u64 + 4, self),
                        read32(helper_slot_0 as u64 + 8, self),
                        read32(helper_slot_0 as u64 + 12, self),
                        read32(helper_slot_0 as u64 + 16, self),
                        read32(helper_slot_0 as u64 + 20, self),
                        read32(helper_slot_0 as u64 + 24, self),
                        read32(helper_slot_0 as u64 + 28, self),
                    );
                    log::error!(
                        "UDF pre-abort helper slot1 target [{:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X}]",
                        read32(helper_slot_1 as u64, self),
                        read32(helper_slot_1 as u64 + 4, self),
                        read32(helper_slot_1 as u64 + 8, self),
                        read32(helper_slot_1 as u64 + 12, self),
                        read32(helper_slot_1 as u64 + 16, self),
                        read32(helper_slot_1 as u64 + 20, self),
                        read32(helper_slot_1 as u64 + 24, self),
                        read32(helper_slot_1 as u64 + 28, self),
                    );
                    log::error!(
                        "UDF pre-abort helper slot2 target [{:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X}]",
                        read32(helper_slot_2 as u64, self),
                        read32(helper_slot_2 as u64 + 4, self),
                        read32(helper_slot_2 as u64 + 8, self),
                        read32(helper_slot_2 as u64 + 12, self),
                        read32(helper_slot_2 as u64 + 16, self),
                        read32(helper_slot_2 as u64 + 20, self),
                        read32(helper_slot_2 as u64 + 24, self),
                        read32(helper_slot_2 as u64 + 28, self),
                    );
                    let pre_check_slot = read32(0x022C_2F2C, self);
                    let pre_abort_ctor_slot = read32(0x022C_63D4, self);
                    log::error!(
                        "UDF helper slots extra [0x022C2F2C]={:08X} [0x022C63D4]={:08X}",
                        pre_check_slot,
                        pre_abort_ctor_slot,
                    );
                    log::error!(
                        "UDF helper 0x201F5CC target [{:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X}]",
                        read32(pre_check_slot as u64, self),
                        read32(pre_check_slot as u64 + 4, self),
                        read32(pre_check_slot as u64 + 8, self),
                        read32(pre_check_slot as u64 + 12, self),
                        read32(pre_check_slot as u64 + 16, self),
                        read32(pre_check_slot as u64 + 20, self),
                        read32(pre_check_slot as u64 + 24, self),
                        read32(pre_check_slot as u64 + 28, self),
                    );
                    log::error!(
                        "UDF helper 0x20293C4 target [{:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X}]",
                        read32(pre_abort_ctor_slot as u64, self),
                        read32(pre_abort_ctor_slot as u64 + 4, self),
                        read32(pre_abort_ctor_slot as u64 + 8, self),
                        read32(pre_abort_ctor_slot as u64 + 12, self),
                        read32(pre_abort_ctor_slot as u64 + 16, self),
                        read32(pre_abort_ctor_slot as u64 + 20, self),
                        read32(pre_abort_ctor_slot as u64 + 24, self),
                        read32(pre_abort_ctor_slot as u64 + 28, self),
                    );
                    log::error!(
                        "UDF pre-check helper 0x0201F5CC [{:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X}]",
                        read32(0x0201_F5CC, self),
                        read32(0x0201_F5D0, self),
                        read32(0x0201_F5D4, self),
                        read32(0x0201_F5D8, self),
                        read32(0x0201_F5DC, self),
                        read32(0x0201_F5E0, self),
                        read32(0x0201_F5E4, self),
                        read32(0x0201_F5E8, self),
                    );
                    let bl1_resolved = read32(0x022C_248C, self);
                    log::error!(
                        "UDF bl1 slot2 [0x022C248C]={:08X} target=[{:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X}]",
                        bl1_resolved,
                        read32(bl1_resolved as u64, self),
                        read32(bl1_resolved as u64 + 4, self),
                        read32(bl1_resolved as u64 + 8, self),
                        read32(bl1_resolved as u64 + 12, self),
                        read32(bl1_resolved as u64 + 16, self),
                        read32(bl1_resolved as u64 + 20, self),
                        read32(bl1_resolved as u64 + 24, self),
                        read32(bl1_resolved as u64 + 28, self),
                    );
                    log::error!(
                        "UDF bl1 callee 0x00200994 [{:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X}]",
                        read32(0x0020_0994, self),
                        read32(0x0020_0998, self),
                        read32(0x0020_099C, self),
                        read32(0x0020_09A0, self),
                        read32(0x0020_09A4, self),
                        read32(0x0020_09A8, self),
                        read32(0x0020_09AC, self),
                        read32(0x0020_09B0, self),
                    );
                    log::error!(
                        "UDF bl1 real 0x00200D94 [{:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X}]",
                        read32(0x0020_0D94, self),
                        read32(0x0020_0D98, self),
                        read32(0x0020_0D9C, self),
                        read32(0x0020_0DA0, self),
                        read32(0x0020_0DA4, self),
                        read32(0x0020_0DA8, self),
                        read32(0x0020_0DAC, self),
                        read32(0x0020_0DB0, self),
                    );
                    for base in [
                        0x01D2_2BA0u64,
                        0x01D2_2BC0,
                        0x01D2_2BE0,
                        0x01D2_2C00u64,
                        0x01D2_2C20u64,
                        0x01D2_2C30,
                        0x01D2_2C40,
                        0x01D2_2C60,
                        0x0202_18A0,
                        0x0202_18C0,
                        0x0201_F5CC,
                        0x0020_0D94,
                        0x0020_0DB4,
                        0x0020_0DD4,
                        0x0020_0F18,
                        0x0020_0F38,
                    ] {
                        log::error!(
                            "UDF code dump @{:08X} [{:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X}]",
                            base as u32,
                            read32(base, self),
                            read32(base + 4, self),
                            read32(base + 8, self),
                            read32(base + 12, self),
                            read32(base + 16, self),
                            read32(base + 20, self),
                            read32(base + 24, self),
                            read32(base + 28, self),
                        );
                    }
                    if self.check_memory_access(0x0020_0000, 4) {
                        let syms = crate::arm::symbols::get_symbols_from_memory(
                            0x0020_0000,
                            &self.mem(),
                            false,
                        );
                        for &addr in &[
                            0x01D1_DD20u64,
                            0x01D2_2C00,
                            0x0201_F5CC,
                            0x0202_0F28,
                            0x0202_93C4,
                        ] {
                            log::error!(
                                "UDF symbol {:08X} -> {}",
                                addr as u32,
                                crate::arm::symbols::get_symbol_name(&syms, addr)
                                    .unwrap_or("<none>"),
                            );
                        }
                    }
                }

                let process = unsafe { &*self.process };
                log::error!(
                    "ExceptionRaised(pre-logbacktrace, exception = {}, pc = {:08X}, thumb = {})",
                    exception,
                    pc as u32,
                    self.parent().is_in_thumb_mode()
                );
                self.parent().base.log_backtrace(process, &ctx);

                let code = self.mem().read_32(pc);

                log::error!(
                    "ExceptionRaised(exception = {}, pc = {:08X}, code = {:08X}, thumb = {})",
                    exception,
                    pc as u32,
                    code,
                    self.parent().is_in_thumb_mode()
                );
            }
        }
    }

    /// Matches upstream `DynarmicCallbacks32::AddTicks`:
    /// Divides ticks by NUM_CPU_CORES (4), passes to CoreTiming::AddTicks.
    fn add_ticks(&mut self, ticks: u64) {
        // Upstream: ASSERT_MSG(!m_parent.m_uses_wall_clock, ...)
        if self.parent().base.uses_wall_clock {
            return;
        }
        // Divide by number of CPU cores, minimum 1 tick.
        // Matches upstream: amortized_ticks = max(ticks / NUM_CPU_CORES, 1)
        let amortized_ticks =
            std::cmp::max(ticks / crate::hardware_properties::NUM_CPU_CORES as u64, 1);
        self.parent()
            .core_timing
            .lock()
            .unwrap()
            .add_ticks(amortized_ticks);
    }

    /// Matches upstream `DynarmicCallbacks32::GetTicksRemaining`:
    /// Returns max(CoreTiming::GetDowncount(), 0).
    fn get_ticks_remaining(&self) -> u64 {
        // Upstream: ASSERT_MSG(!m_parent.m_uses_wall_clock, ...)
        if self.parent().base.uses_wall_clock {
            return u64::MAX;
        }
        let ct = self.parent().core_timing.lock().unwrap();
        std::cmp::max(ct.get_downcount(), 0) as u64
    }

    /// Matches upstream `DynarmicCallbacks32::GetCNTPCT`.
    /// Returns the current system counter value from CoreTiming.
    fn get_cntpct(&self) -> u64 {
        self.parent().core_timing.lock().unwrap().get_clock_ticks()
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
    exclusive_monitor:
        *mut crate::arm::dynarmic::dynarmic_exclusive_monitor::DynarmicExclusiveMonitor,

    /// Core index for this CPU.
    /// Upstream: `m_core_index`.
    core_index: usize,

    /// SVC callback number.
    /// Upstream: `m_svc_swi` written by callback via `m_parent` reference.
    svc_swi: Arc<AtomicU32>,

    /// Core timing reference for tick management.
    /// Upstream: accessed via `m_system.CoreTiming()`.
    /// Stored here so callbacks can access it via `parent().core_timing`.
    core_timing: Arc<std::sync::Mutex<crate::core_timing::CoreTiming>>,

    /// Shared atomic pointer used by callbacks to reach back to this ArmDynarmic32.
    /// The callbacks store a clone of this Arc. After JIT creation, the parent sets
    /// this to point to itself, allowing callbacks to access parent fields.
    parent_ptr: Arc<AtomicPtr<ArmDynarmic32>>,

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
        let parent_ptr = Arc::new(AtomicPtr::new(std::ptr::null_mut()));
        let callbacks = DynarmicCallbacks32::new(
            shared_memory,
            core_memory,
            process as *const _ as *const crate::hle::kernel::k_process::KProcess,
            parent_ptr.clone(),
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
            }) {
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
            fastmem_pointer: if std::env::var("RUZU_NO_FASTMEM").is_ok() {
                None
            } else {
                fastmem_pointer
            },
        };

        log::warn!(
            "ArmDynarmic32: fastmem_pointer={:?} cycle_counting={} optimizations={:#x}",
            fastmem_pointer.map(|p| p as usize),
            !uses_wall_clock,
            optimizations.bits()
        );

        let jit = match rdynarmic::A32Jit::new(config) {
            Ok(jit) => {
                log::info!(
                    "ArmDynarmic32: JIT created successfully for core {}",
                    core_index
                );
                Some(jit)
            }
            Err(e) => {
                log::error!(
                    "ArmDynarmic32: Failed to create JIT for core {}: {}",
                    core_index,
                    e
                );
                None
            }
        };

        let result = Self {
            base: ArmInterfaceBase::new(uses_wall_clock),
            exclusive_monitor,
            core_index,
            svc_swi,
            core_timing,
            parent_ptr,
            halted_watchpoint: None,
            breakpoint_context: ThreadContext::default(),
            jit,
            cp15_uro: 0,
            last_exception_address,
        };

        // NOTE: The parent pointer is NOT set here because `result` will be moved
        // by the caller (e.g. into a Box). The caller MUST call `set_parent_ptr()`
        // after placing the ArmDynarmic32 at its final stable location.
        // Until then, callbacks that access parent() will panic on the debug_assert.
        // This is safe because callbacks are only invoked during run_thread().

        result
    }

    /// Set the parent pointer so callbacks can access this ArmDynarmic32.
    ///
    /// MUST be called after the ArmDynarmic32 is placed at its final stable memory
    /// location (e.g. after Box allocation). The pointer must remain valid for the
    /// lifetime of the JIT. Callbacks will panic if parent() is called before this.
    ///
    /// Matches upstream where `m_parent` is a reference set during construction.
    /// In Rust, we defer because the callbacks are consumed by the JIT before the
    /// parent struct reaches its final location.
    pub fn set_parent_ptr(&mut self) {
        let ptr: *mut ArmDynarmic32 = self;
        self.parent_ptr.store(ptr, Ordering::Release);
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
        let trace_search_limit = parse_trace_u32_env("RUZU_A32_TRACE_SEARCH_LIMIT").unwrap_or(0);
        if let (Some(start), Some(end)) = (trace_start, trace_end) {
            let current_pc = jit.get_register(15);
            if trace_limit > 0
                && (current_pc >= start && current_pc < end || trace_search_limit > 0)
            {
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
                            if !last_hr.is_empty()
                                && last_hr
                                    != rdynarmic::halt_reason::HaltReason::STEP
                            {
                                log::info!("[A32TRACE] halt while searching: {:?}", last_hr);
                                break;
                            }
                            if step + 1 >= trace_search_limit {
                                break;
                            }
                            continue;
                        }
                    }
                    if logged_steps >= trace_limit {
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
                    if !last_hr.is_empty()
                        && last_hr
                            != rdynarmic::halt_reason::HaltReason::STEP
                    {
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
        // Upstream uses jit.Run() — full-speed block execution.
        // step() single-steps one instruction at a time, which is correct
        // but extremely slow. Use run() for normal execution.
        let rdynarmic_hr = jit.run();
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
