// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/arm/dynarmic/arm_dynarmic_64.h and arm_dynarmic_64.cpp
//! ARM64 dynarmic backend.

use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::Arc;

use crate::arm::arm_interface::{
    ArmInterface, ArmInterfaceBase, Architecture, DebugWatchpoint, HaltReason, KProcess,
    KThread, ThreadContext,
};
use crate::hle::kernel::k_process::SharedProcessMemory;

use rdynarmic::jit_config::{JitCallbacks, JitConfig, OptimizationFlag};

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
    /// Shared guest memory reference.
    /// Corresponds to upstream `m_memory` (Core::Memory::Memory&).
    memory: SharedProcessMemory,
    /// SVC number from last supervisor call, shared with parent ArmDynarmic64.
    /// Upstream: callback writes to m_parent.m_svc via back-reference.
    svc: Arc<AtomicU32>,
    /// TPIDRRO_EL0 system register (read-only thread ID)
    tpidrro_el0: u64,
    /// TPIDR_EL0 system register (read-write thread ID)
    tpidr_el0: u64,
}

impl DynarmicCallbacks64 {
    fn new(memory: SharedProcessMemory, svc: Arc<AtomicU32>) -> Self {
        Self {
            memory,
            svc,
            tpidrro_el0: 0,
            tpidr_el0: 0,
        }
    }
}

impl JitCallbacks for DynarmicCallbacks64 {
    fn memory_read_code(&self, vaddr: u64) -> Option<u32> {
        // Corresponds to upstream DynarmicCallbacks64::MemoryReadCode.
        // Reads a 32-bit instruction from guest code memory.
        let mem = self.memory.read().unwrap();
        if mem.is_valid_range(vaddr, 4) {
            Some(mem.read_32(vaddr))
        } else {
            // Return UDF (permanently undefined) instruction to trigger a clean
            // exception rather than hanging when code branches to unmapped memory.
            log::warn!("memory_read_code({:#x}): unmapped, returning UDF", vaddr);
            Some(0x00000000) // A64: UDF #0
        }
    }

    fn memory_read_8(&self, vaddr: u64) -> u8 {
        let mem = self.memory.read().unwrap();
        mem.read_8(vaddr)
    }

    fn memory_read_16(&self, vaddr: u64) -> u16 {
        let mem = self.memory.read().unwrap();
        mem.read_16(vaddr)
    }

    fn memory_read_32(&self, vaddr: u64) -> u32 {
        let mem = self.memory.read().unwrap();
        mem.read_32(vaddr)
    }

    fn memory_read_64(&self, vaddr: u64) -> u64 {
        let mem = self.memory.read().unwrap();
        mem.read_64(vaddr)
    }

    fn memory_read_128(&self, vaddr: u64) -> (u64, u64) {
        let mem = self.memory.read().unwrap();
        let lo = mem.read_64(vaddr);
        let hi = mem.read_64(vaddr + 8);
        (lo, hi)
    }

    fn memory_write_8(&mut self, vaddr: u64, value: u8) {
        let mut mem = self.memory.write().unwrap();
        mem.write_8(vaddr, value);
    }

    fn memory_write_16(&mut self, vaddr: u64, value: u16) {
        let mut mem = self.memory.write().unwrap();
        mem.write_16(vaddr, value);
    }

    fn memory_write_32(&mut self, vaddr: u64, value: u32) {
        let mut mem = self.memory.write().unwrap();
        mem.write_32(vaddr, value);
    }

    fn memory_write_64(&mut self, vaddr: u64, value: u64) {
        let mut mem = self.memory.write().unwrap();
        mem.write_64(vaddr, value);
    }

    fn memory_write_128(&mut self, vaddr: u64, value_lo: u64, value_hi: u64) {
        let mut mem = self.memory.write().unwrap();
        mem.write_64(vaddr, value_lo);
        mem.write_64(vaddr + 8, value_hi);
    }

    fn exclusive_read_8(&self, vaddr: u64) -> u8 {
        // TODO: Wire exclusive monitor. For now, do a regular read.
        let mem = self.memory.read().unwrap();
        mem.read_8(vaddr)
    }

    fn exclusive_read_16(&self, vaddr: u64) -> u16 {
        let mem = self.memory.read().unwrap();
        mem.read_16(vaddr)
    }

    fn exclusive_read_32(&self, vaddr: u64) -> u32 {
        let mem = self.memory.read().unwrap();
        mem.read_32(vaddr)
    }

    fn exclusive_read_64(&self, vaddr: u64) -> u64 {
        let mem = self.memory.read().unwrap();
        mem.read_64(vaddr)
    }

    fn exclusive_read_128(&self, vaddr: u64) -> (u64, u64) {
        let mem = self.memory.read().unwrap();
        let lo = mem.read_64(vaddr);
        let hi = mem.read_64(vaddr + 8);
        (lo, hi)
    }

    fn exclusive_write_8(&mut self, vaddr: u64, value: u8) -> bool {
        // TODO: Wire exclusive monitor for proper LDXR/STXR emulation.
        // For now, always succeed (optimistic).
        let mut mem = self.memory.write().unwrap();
        mem.write_8(vaddr, value);
        true
    }

    fn exclusive_write_16(&mut self, vaddr: u64, value: u16) -> bool {
        let mut mem = self.memory.write().unwrap();
        mem.write_16(vaddr, value);
        true
    }

    fn exclusive_write_32(&mut self, vaddr: u64, value: u32) -> bool {
        let mut mem = self.memory.write().unwrap();
        mem.write_32(vaddr, value);
        true
    }

    fn exclusive_write_64(&mut self, vaddr: u64, value: u64) -> bool {
        let mut mem = self.memory.write().unwrap();
        mem.write_64(vaddr, value);
        true
    }

    fn exclusive_write_128(&mut self, vaddr: u64, value_lo: u64, value_hi: u64) -> bool {
        let mut mem = self.memory.write().unwrap();
        mem.write_64(vaddr, value_lo);
        mem.write_64(vaddr + 8, value_hi);
        true
    }

    fn exclusive_clear(&mut self) {
        // No-op until exclusive monitor is wired
    }

    fn call_supervisor(&mut self, svc_num: u32) {
        self.svc.store(svc_num, Ordering::Relaxed);
        // The JIT will halt with SVC halt reason after this callback returns
    }

    fn exception_raised(&mut self, pc: u64, exception: u64) {
        log::error!(
            "DynarmicCallbacks64::exception_raised(pc={:#x}, exception={:#x})",
            pc, exception
        );
    }

    fn add_ticks(&mut self, _ticks: u64) {
        // TODO: Wire to CoreTiming when available.
        // Upstream divides by NUM_CPU_CORES and passes to CoreTiming::AddTicks.
    }

    fn get_ticks_remaining(&self) -> u64 {
        // TODO: Wire to CoreTiming when available.
        // Upstream returns max(CoreTiming::GetDowncount(), 0).
        // Return a reasonable default tick budget.
        1000
    }
}

/// ARM64 Dynarmic JIT backend.
///
/// Corresponds to upstream `Core::ArmDynarmic64`.
pub struct ArmDynarmic64 {
    pub base: ArmInterfaceBase,

    // TODO: Replace with actual System reference
    // m_system: &System,

    // TODO: Replace with actual DynarmicExclusiveMonitor reference
    // m_exclusive_monitor: &DynarmicExclusiveMonitor,

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
        _exclusive_monitor: &dyn std::any::Any,
        core_index: usize,
        shared_memory: SharedProcessMemory,
    ) -> Self {
        // Create JIT callbacks with shared memory reference
        let svc = Arc::new(AtomicU32::new(0));
        let callbacks = DynarmicCallbacks64::new(shared_memory, svc.clone());

        // Configure JIT
        // Upstream: enable_cycle_counting = !uses_wall_clock
        let config = JitConfig {
            callbacks: Box::new(callbacks),
            enable_cycle_counting: !uses_wall_clock,
            code_cache_size: 512 * 1024 * 1024, // 512 MiB on x86_64 (upstream default)
            optimizations: OptimizationFlag::ALL_SAFE_OPTIMIZATIONS,
            unsafe_optimizations: false,
        };

        // Create the JIT
        let jit = match rdynarmic::A64Jit::new(config) {
            Ok(jit) => {
                log::info!("ArmDynarmic64: JIT created successfully for core {}", core_index);
                Some(jit)
            }
            Err(e) => {
                log::error!("ArmDynarmic64: Failed to create JIT for core {}: {}", core_index, e);
                None
            }
        };

        Self {
            base: ArmInterfaceBase::new(uses_wall_clock),
            core_index,
            svc,
            halted_watchpoint: None,
            breakpoint_context: ThreadContext::default(),
            jit,
            tpidrro_el0: 0,
            tpidr_el0: 0,
        }
    }
}

impl ArmInterface for ArmDynarmic64 {
    fn run_thread(&mut self, _thread: &mut KThread) -> HaltReason {
        // ScopedJitExecution sj(thread->GetOwnerProcess());
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
        // ScopedJitExecution sj(thread->GetOwnerProcess());
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
