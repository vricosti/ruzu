// SPDX-FileCopyrightText: Copyright 2020 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/arm/dynarmic/arm_dynarmic_32.h and arm_dynarmic_32.cpp
//! ARM32 dynarmic backend.

use std::sync::atomic::{AtomicU32, AtomicU64, Ordering};
use std::sync::Arc;

use crate::arm::arm_interface::{
    ArmInterface, ArmInterfaceBase, Architecture, DebugWatchpoint, HaltReason, KProcess,
    KThread, ThreadContext,
};
use crate::hle::kernel::k_process::SharedProcessMemory;
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
}

impl DynarmicCallbacks32 {
    fn new(
        memory: SharedProcessMemory,
        core_memory: Option<Arc<std::sync::Mutex<Memory>>>,
        svc_swi: Arc<AtomicU32>,
        uses_wall_clock: bool,
        core_timing: Arc<std::sync::Mutex<crate::core_timing::CoreTiming>>,
        last_exception_address: Arc<AtomicU64>,
    ) -> Self {
        log::info!("DynarmicCallbacks32: Arc memory ptr = {:?}, base = {:#x}, core_memory={}",
            std::sync::Arc::as_ptr(&memory),
            memory.read().unwrap().base,
            if core_memory.is_some() { "wired" } else { "fallback" });
        Self { memory, core_memory, svc_swi, uses_wall_clock, core_timing, last_exception_address }
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
        if let Some(ref cm) = self.core_memory {
            cm.lock().unwrap().read_8(vaddr)
        } else {
            self.memory.read().unwrap().read_8(vaddr)
        }
    }

    fn memory_read_16(&self, vaddr: u64) -> u16 {
        if let Some(ref cm) = self.core_memory {
            cm.lock().unwrap().read_16(vaddr)
        } else {
            self.memory.read().unwrap().read_16(vaddr)
        }
    }

    fn memory_read_32(&self, vaddr: u64) -> u32 {
        if let Some(ref cm) = self.core_memory {
            cm.lock().unwrap().read_32(vaddr)
        } else {
            self.memory.read().unwrap().read_32(vaddr)
        }
    }

    fn memory_read_64(&self, vaddr: u64) -> u64 {
        if let Some(ref cm) = self.core_memory {
            cm.lock().unwrap().read_64(vaddr)
        } else {
            self.memory.read().unwrap().read_64(vaddr)
        }
    }

    fn memory_read_128(&self, vaddr: u64) -> (u64, u64) {
        if let Some(ref cm) = self.core_memory {
            let m = cm.lock().unwrap();
            (m.read_64(vaddr), m.read_64(vaddr + 8))
        } else {
            let mem = self.memory.read().unwrap();
            (mem.read_64(vaddr), mem.read_64(vaddr + 8))
        }
    }

    fn memory_write_8(&mut self, vaddr: u64, value: u8) {
        if let Some(ref cm) = self.core_memory {
            cm.lock().unwrap().write_8(vaddr, value);
        } else {
            self.memory.write().unwrap().write_8(vaddr, value);
        }
    }

    fn memory_write_16(&mut self, vaddr: u64, value: u16) {
        if let Some(ref cm) = self.core_memory {
            cm.lock().unwrap().write_16(vaddr, value);
        } else {
            self.memory.write().unwrap().write_16(vaddr, value);
        }
    }

    fn memory_write_32(&mut self, vaddr: u64, value: u32) {
        if let Some(ref cm) = self.core_memory {
            cm.lock().unwrap().write_32(vaddr, value);
        } else {
            self.memory.write().unwrap().write_32(vaddr, value);
        }
    }

    fn memory_write_64(&mut self, vaddr: u64, value: u64) {
        if let Some(ref cm) = self.core_memory {
            cm.lock().unwrap().write_64(vaddr, value);
        } else {
            self.memory.write().unwrap().write_64(vaddr, value);
        }
    }

    fn memory_write_128(&mut self, vaddr: u64, value_lo: u64, value_hi: u64) {
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

    fn exclusive_write_8(&mut self, vaddr: u64, value: u8) -> bool {
        self.memory_write_8(vaddr, value);
        true
    }

    fn exclusive_write_16(&mut self, vaddr: u64, value: u16) -> bool {
        self.memory_write_16(vaddr, value);
        true
    }

    fn exclusive_write_32(&mut self, vaddr: u64, value: u32) -> bool {
        self.memory_write_32(vaddr, value);
        true
    }

    fn exclusive_write_64(&mut self, vaddr: u64, value: u64) -> bool {
        self.memory_write_64(vaddr, value);
        true
    }

    fn exclusive_write_128(&mut self, vaddr: u64, value_lo: u64, value_hi: u64) -> bool {
        self.memory_write_128(vaddr, value_lo, value_hi);
        true
    }

    fn exclusive_clear(&mut self) {
        // No-op until exclusive monitor is wired
    }

    fn call_supervisor(&mut self, svc_num: u32) {
        self.svc_swi.store(svc_num, Ordering::Relaxed);
    }

    fn exception_raised(&mut self, pc: u64, exception: u64) {
        self.last_exception_address
            .store(pc, Ordering::Relaxed);
        log::error!(
            "DynarmicCallbacks32::exception_raised(pc={:#x}, exception={:#x})",
            pc, exception
        );
        let start = pc.saturating_sub(0x10);
        log::error!("DynarmicCallbacks32 exception window around pc={:#x}", pc);
        if let Some(ref cm) = self.core_memory {
            let m = cm.lock().unwrap();
            for addr in (start..=pc.saturating_add(0x10)).step_by(4) {
                if !m.is_valid_virtual_address(addr) {
                    log::error!("  [{:#010x}] <unmapped>", addr);
                    continue;
                }
                let insn = m.read_32(addr);
                let marker = if addr == pc { " <EXC>" } else { "" };
                log::error!("  [{:#010x}] {:#010x}{}", addr, insn, marker);
            }
        } else {
            let mem = self.memory.read().unwrap();
            for addr in (start..=pc.saturating_add(0x10)).step_by(4) {
                if !mem.is_valid_range(addr, 4) {
                    log::error!("  [{:#010x}] <unmapped>", addr);
                    continue;
                }
                let insn = mem.read_32(addr);
                let marker = if addr == pc { " <EXC>" } else { "" };
                log::error!("  [{:#010x}] {:#010x}{}", addr, insn, marker);
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
        _process: &KProcess,
        _exclusive_monitor: &dyn std::any::Any,
        core_index: usize,
        shared_memory: SharedProcessMemory,
        core_timing: Arc<std::sync::Mutex<crate::core_timing::CoreTiming>>,
        core_memory: Option<Arc<std::sync::Mutex<Memory>>>,
    ) -> Self {
        let svc_swi = Arc::new(AtomicU32::new(0));
        let last_exception_address = Arc::new(AtomicU64::new(0));
        let callbacks = DynarmicCallbacks32::new(
            shared_memory,
            core_memory,
            svc_swi.clone(),
            uses_wall_clock,
            core_timing,
            last_exception_address.clone(),
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
