// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/arm/dynarmic/arm_dynarmic_32.h and arm_dynarmic_32.cpp
//! ARM32 dynarmic backend.

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
    /// Shared guest memory reference.
    memory: SharedProcessMemory,
    /// SVC/SWI number from last supervisor call, shared with parent ArmDynarmic32.
    svc_swi: Arc<AtomicU32>,
    /// Whether wall clock is used (if true, ticking is disabled).
    /// Matches upstream `m_parent.m_uses_wall_clock`.
    uses_wall_clock: bool,
    /// Core timing reference for tick management.
    /// Matches upstream `m_parent.m_system.CoreTiming()`.
    core_timing: Arc<std::sync::Mutex<crate::core_timing::CoreTiming>>,
}

impl DynarmicCallbacks32 {
    fn new(
        memory: SharedProcessMemory,
        svc_swi: Arc<AtomicU32>,
        uses_wall_clock: bool,
        core_timing: Arc<std::sync::Mutex<crate::core_timing::CoreTiming>>,
    ) -> Self {
        log::info!("DynarmicCallbacks32: Arc memory ptr = {:?}, base = {:#x}",
            std::sync::Arc::as_ptr(&memory),
            memory.read().unwrap().base);
        Self { memory, svc_swi, uses_wall_clock, core_timing }
    }
}

impl JitCallbacks for DynarmicCallbacks32 {
    fn memory_read_code(&self, vaddr: u64) -> Option<u32> {
        let mem = self.memory.read().unwrap();
        if mem.is_valid_range(vaddr, 4) {
            Some(mem.read_32(vaddr))
        } else {
            // Return UDF (permanently undefined) instruction to trigger a clean
            // exception rather than hanging when code branches to unmapped memory.
            // ARM encoding: 0xE7F000F0 = UDF #0
            log::warn!("DynarmicCallbacks32::memory_read_code({:#x}): unmapped, returning UDF", vaddr);
            Some(0xE7F000F0)
        }
    }

    fn memory_read_8(&self, vaddr: u64) -> u8 {
        self.memory.read().unwrap().read_8(vaddr)
    }

    fn memory_read_16(&self, vaddr: u64) -> u16 {
        self.memory.read().unwrap().read_16(vaddr)
    }

    fn memory_read_32(&self, vaddr: u64) -> u32 {
        self.memory.read().unwrap().read_32(vaddr)
    }

    fn memory_read_64(&self, vaddr: u64) -> u64 {
        self.memory.read().unwrap().read_64(vaddr)
    }

    fn memory_read_128(&self, vaddr: u64) -> (u64, u64) {
        let mem = self.memory.read().unwrap();
        (mem.read_64(vaddr), mem.read_64(vaddr + 8))
    }

    fn memory_write_8(&mut self, vaddr: u64, value: u8) {
        let mut mem = self.memory.write().unwrap();
        if !mem.is_writable(vaddr) {
            log::trace!("JIT write8 blocked (read-only): [{:#010x}]", vaddr);
            return;
        }
        mem.write_8(vaddr, value);
    }

    fn memory_write_16(&mut self, vaddr: u64, value: u16) {
        let mut mem = self.memory.write().unwrap();
        if !mem.is_writable(vaddr) {
            log::trace!("JIT write16 blocked (read-only): [{:#010x}]", vaddr);
            return;
        }
        mem.write_16(vaddr, value);
    }

    fn memory_write_32(&mut self, vaddr: u64, value: u32) {
        let mut mem = self.memory.write().unwrap();
        if !mem.is_writable(vaddr) {
            log::trace!("JIT write32 blocked (read-only): [{:#010x}]", vaddr);
            return;
        }
        mem.write_32(vaddr, value);
    }

    fn memory_write_64(&mut self, vaddr: u64, value: u64) {
        let mut mem = self.memory.write().unwrap();
        if !mem.is_writable(vaddr) {
            log::trace!("JIT write64 blocked (read-only): [{:#010x}]", vaddr);
            return;
        }
        mem.write_64(vaddr, value);
    }

    fn memory_write_128(&mut self, vaddr: u64, value_lo: u64, value_hi: u64) {
        let mut mem = self.memory.write().unwrap();
        if !mem.is_writable(vaddr) {
            log::trace!("JIT write128 blocked (read-only): [{:#010x}]", vaddr);
            return;
        }
        mem.write_64(vaddr, value_lo);
        mem.write_64(vaddr + 8, value_hi);
    }

    fn exclusive_read_8(&self, vaddr: u64) -> u8 {
        self.memory.read().unwrap().read_8(vaddr)
    }

    fn exclusive_read_16(&self, vaddr: u64) -> u16 {
        self.memory.read().unwrap().read_16(vaddr)
    }

    fn exclusive_read_32(&self, vaddr: u64) -> u32 {
        self.memory.read().unwrap().read_32(vaddr)
    }

    fn exclusive_read_64(&self, vaddr: u64) -> u64 {
        self.memory.read().unwrap().read_64(vaddr)
    }

    fn exclusive_read_128(&self, vaddr: u64) -> (u64, u64) {
        let mem = self.memory.read().unwrap();
        (mem.read_64(vaddr), mem.read_64(vaddr + 8))
    }

    fn exclusive_write_8(&mut self, vaddr: u64, value: u8) -> bool {
        let mut mem = self.memory.write().unwrap();
        if !mem.is_writable(vaddr) {
            log::warn!("JIT excl_write8 BLOCKED (read-only): [{:#010x}]", vaddr);
            return false;
        }
        mem.write_8(vaddr, value);
        true
    }

    fn exclusive_write_16(&mut self, vaddr: u64, value: u16) -> bool {
        let mut mem = self.memory.write().unwrap();
        if !mem.is_writable(vaddr) {
            log::warn!("JIT excl_write16 BLOCKED (read-only): [{:#010x}]", vaddr);
            return false;
        }
        mem.write_16(vaddr, value);
        true
    }

    fn exclusive_write_32(&mut self, vaddr: u64, value: u32) -> bool {
        let mut mem = self.memory.write().unwrap();
        if !mem.is_writable(vaddr) {
            log::warn!("JIT excl_write32 BLOCKED (read-only): [{:#010x}]", vaddr);
            return false;
        }
        mem.write_32(vaddr, value);
        true
    }

    fn exclusive_write_64(&mut self, vaddr: u64, value: u64) -> bool {
        let mut mem = self.memory.write().unwrap();
        if !mem.is_writable(vaddr) {
            log::warn!("JIT excl_write64 BLOCKED (read-only): [{:#010x}]", vaddr);
            return false;
        }
        mem.write_64(vaddr, value);
        true
    }

    fn exclusive_write_128(&mut self, vaddr: u64, value_lo: u64, value_hi: u64) -> bool {
        let mut mem = self.memory.write().unwrap();
        if !mem.is_writable(vaddr) {
            log::warn!("JIT excl_write128 BLOCKED (read-only): [{:#010x}]", vaddr);
            return false;
        }
        mem.write_64(vaddr, value_lo);
        mem.write_64(vaddr + 8, value_hi);
        true
    }

    fn exclusive_clear(&mut self) {
        // No-op until exclusive monitor is wired
    }

    fn call_supervisor(&mut self, svc_num: u32) {
        self.svc_swi.store(svc_num, Ordering::Relaxed);
    }

    fn exception_raised(&mut self, pc: u64, exception: u64) {
        log::error!(
            "DynarmicCallbacks32::exception_raised(pc={:#x}, exception={:#x})",
            pc, exception
        );
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
    ///
    /// Also calls ResetTicks when downcount is depleted, matching the
    /// upstream scheduler loop that calls ResetTicks() after each yield.
    fn get_ticks_remaining(&self) -> u64 {
        if self.uses_wall_clock {
            return u64::MAX;
        }
        let mut ct = self.core_timing.lock().unwrap();
        let downcount = ct.get_downcount();
        if downcount <= 0 {
            ct.reset_ticks();
            return std::cmp::max(ct.get_downcount(), 0) as u64;
        }
        downcount as u64
    }
}

/// ARM32 Dynarmic JIT backend.
///
/// Corresponds to upstream `Core::ArmDynarmic32`.
pub struct ArmDynarmic32 {
    pub base: ArmInterfaceBase,

    // TODO: Replace with actual System reference
    // m_system: &System,

    // TODO: Replace with actual DynarmicExclusiveMonitor reference
    // m_exclusive_monitor: &DynarmicExclusiveMonitor,

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
}

impl ArmDynarmic32 {
    /// Create a new ARM32 dynarmic backend.
    ///
    /// Corresponds to upstream `ArmDynarmic32::ArmDynarmic32`.
    pub fn new(
        _system: &dyn std::any::Any,
        uses_wall_clock: bool,
        _process: &KProcess,
        _exclusive_monitor: &dyn std::any::Any,
        core_index: usize,
        shared_memory: SharedProcessMemory,
    ) -> Self {
        // Create a CoreTiming instance for tick management.
        // In upstream, this would come from System::CoreTiming().
        // For now, each JIT instance has its own (single-core mode).
        let core_timing = Arc::new(std::sync::Mutex::new(crate::core_timing::CoreTiming::new()));

        let svc_swi = Arc::new(AtomicU32::new(0));
        let callbacks = DynarmicCallbacks32::new(
            shared_memory, svc_swi.clone(), uses_wall_clock, core_timing,
        );

        let config = JitConfig {
            callbacks: Box::new(callbacks),
            enable_cycle_counting: !uses_wall_clock,
            code_cache_size: 512 * 1024 * 1024,
            optimizations: OptimizationFlag::ALL_SAFE_OPTIMIZATIONS,
            unsafe_optimizations: false,
        };

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
            core_index,
            svc_swi,
            halted_watchpoint: None,
            breakpoint_context: ThreadContext::default(),
            jit,
            cp15_uro: 0,
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

impl ArmInterface for ArmDynarmic32 {
    fn run_thread(&mut self, _thread: &mut KThread) -> HaltReason {
        let jit = match self.jit.as_mut() {
            Some(jit) => jit,
            None => {
                log::error!("ArmDynarmic32::run_thread: JIT not available");
                return HaltReason::BREAK_LOOP;
            }
        };

        jit.clear_exclusive_state();
        let rdynarmic_hr = jit.run();
        translate_halt_reason(rdynarmic_hr)
    }

    fn step_thread(&mut self, _thread: &mut KThread) -> HaltReason {
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
        for i in 0..15 {
            ctx.r[i] = jit.get_register(i) as u64;
        }
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
        ctx.tpidr = 0; // A32 uses CP15 for thread pointer
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
