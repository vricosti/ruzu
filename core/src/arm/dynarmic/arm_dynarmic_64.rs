// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/arm/dynarmic/arm_dynarmic_64.h and arm_dynarmic_64.cpp
//! ARM64 dynarmic backend.

use std::sync::atomic::{AtomicU32, AtomicU64, Ordering};
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
/// rdynarmic uses its own compact halt reason bits:
///   STEP=1, SVC=2, BREAKPOINT=4, EXCEPTION_RAISED=8,
///   CACHE_INVALIDATION=16, EXTERNAL_HALT=32
///
/// Core uses upstream-matching halt reason bits:
///   STEP_THREAD=0x1, DATA_ABORT=0x4, BREAK_LOOP=0x02000000,
///   SUPERVISOR_CALL=0x04000000, INSTRUCTION_BREAKPOINT=0x08000000,
///   PREFETCH_ABORT=0x20000000
///
/// Corresponds to upstream `Core::TranslateHaltReason`.
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
    core_timing: Arc<std::sync::Mutex<crate::core_timing::CoreTiming>>,
    /// Last exception address reported by dynarmic.
    last_exception_address: Arc<AtomicU64>,
    /// Shared exclusive monitor backing Dynarmic's global monitor state.
    exclusive_monitor:
        *mut crate::arm::dynarmic::dynarmic_exclusive_monitor::DynarmicExclusiveMonitor,
    /// CPU core index associated with this callback/JIT instance.
    core_index: usize,
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
        core_timing: Arc<std::sync::Mutex<crate::core_timing::CoreTiming>>,
        last_exception_address: Arc<AtomicU64>,
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
            exclusive_monitor,
            core_index,
        }
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
        if let Some(ref cm) = self.core_memory {
            cm.lock().unwrap().read_8(vaddr)
        } else {
            self.memory.read().unwrap().read_8(vaddr)
        }
    }

    fn memory_read_16(&self, vaddr: u64) -> u16 {
        self.check_memory_access(vaddr, 2);
        if let Some(ref cm) = self.core_memory {
            cm.lock().unwrap().read_16(vaddr)
        } else {
            self.memory.read().unwrap().read_16(vaddr)
        }
    }

    fn memory_read_32(&self, vaddr: u64) -> u32 {
        self.check_memory_access(vaddr, 4);
        if let Some(ref cm) = self.core_memory {
            cm.lock().unwrap().read_32(vaddr)
        } else {
            self.memory.read().unwrap().read_32(vaddr)
        }
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
        if !self.check_memory_access(vaddr, 1) {
            return;
        }
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
        // The JIT will halt with SVC halt reason after this callback returns
    }

    fn exception_raised(&mut self, pc: u64, exception: u64) {
        // ARM64 exception types from upstream dynarmic Exception enum:
        // 0=Yield, 1=WFI, 2=WFE, 3=SendEvent, 4=SendEventLocal, 8=NoExecuteFault
        match exception {
            0 | 1 | 2 | 3 | 4 => {
                // Hint instructions (Yield, WFI, WFE, SEV, SEVL) — benign, return early.
                return;
            }
            _ => {}
        }

        self.last_exception_address.store(pc, Ordering::Relaxed);
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
        self.core_timing.lock().unwrap().get_clock_ticks()
    }

    /// Matches upstream `DynarmicCallbacks64::AddTicks`:
    /// Divides ticks by NUM_CPU_CORES (4), passes to CoreTiming::AddTicks.
    fn add_ticks(&mut self, ticks: u64) {
        if self.uses_wall_clock {
            return;
        }
        let amortized_ticks =
            std::cmp::max(ticks / crate::hardware_properties::NUM_CPU_CORES as u64, 1);
        self.core_timing.lock().unwrap().add_ticks(amortized_ticks);
    }

    /// Matches upstream `DynarmicCallbacks64::GetTicksRemaining`:
    /// Returns max(CoreTiming::GetDowncount(), 0).
    fn get_ticks_remaining(&self) -> u64 {
        if self.uses_wall_clock {
            return u64::MAX;
        }
        let ct = self.core_timing.lock().unwrap();
        std::cmp::max(ct.get_downcount(), 0) as u64
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
    breakpoint_context: ThreadContext,

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
        core_timing: Arc<std::sync::Mutex<crate::core_timing::CoreTiming>>,
        core_memory: Option<Arc<std::sync::Mutex<Memory>>>,
    ) -> Self {
        // Create JIT callbacks with shared memory reference
        let svc = Arc::new(AtomicU32::new(0));
        let last_exception_address = Arc::new(AtomicU64::new(0));
        let callbacks = DynarmicCallbacks64::new(
            shared_memory,
            core_memory,
            svc.clone(),
            uses_wall_clock,
            core_timing,
            last_exception_address.clone(),
            exclusive_monitor,
            core_index,
        );

        // Configure JIT
        // Upstream: enable_cycle_counting = !uses_wall_clock
        let config = JitConfig {
            callbacks: Box::new(callbacks),
            enable_cycle_counting: !uses_wall_clock,
            code_cache_size: 512 * 1024 * 1024, // 512 MiB on x86_64 (upstream default)
            optimizations: OptimizationFlag::ALL_SAFE_OPTIMIZATIONS,
            unsafe_optimizations: false,
            global_monitor: if exclusive_monitor.is_null() {
                None
            } else {
                Some(unsafe { (*exclusive_monitor).get_monitor() as *mut _ })
            },
            fastmem_pointer: None, // TODO: wire for A64
            define_unpredictable_behaviour: true,
            processor_id: core_index as usize,
            wall_clock_cntpct: uses_wall_clock,
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
            breakpoint_context: ThreadContext::default(),
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
        // Upstream: m_cb->m_tpidrro_el0 = value
        // The callback owns this but since it's moved into the JIT,
        // we track it here. When guest memory is wired, this will
        // be passed through properly.
        self.tpidrro_el0 = value;
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
        let ctx = self.breakpoint_context.clone();
        self.set_context(&ctx);
    }
}
