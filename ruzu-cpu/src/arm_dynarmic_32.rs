// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! ARM32 JIT wrapper using rdynarmic (Rust port of dynarmic).
//!
//! This is the Rust equivalent of zuyu's `arm_dynarmic_32.cpp`.
//! Wraps `A32Jit` and provides context transfer between `CpuState`
//! (used by the kernel for thread switching) and the JIT's register file.
//!
//! Memory access is performed through a raw pointer to an opaque type and
//! a vtable of function pointers, avoiding a circular dependency between
//! ruzu-cpu and ruzu-kernel.

use crate::arm_dynarmic_64::MemoryVtable;
use crate::state::CpuState;
use rdynarmic::halt_reason::HaltReason;
use rdynarmic::jit::A32Jit;
use rdynarmic::jit_config::{JitCallbacks, JitConfig, OptimizationFlag};

// ---------------------------------------------------------------------------
// Shared callback state
// ---------------------------------------------------------------------------

struct CallbackState {
    memory_ctx: *mut u8,
    vtable: MemoryVtable,
    ticks_remaining: u64,
    /// SVC number stored by the CallSVC callback.
    /// Matching zuyu's `m_svc_swi` in ArmDynarmic32.
    svc_swi: u32,
}

// ---------------------------------------------------------------------------
// JIT callbacks
// ---------------------------------------------------------------------------

struct DynarmicCallbacks {
    state: *mut CallbackState,
}

unsafe impl Send for DynarmicCallbacks {}

impl DynarmicCallbacks {
    #[inline]
    fn s(&self) -> &CallbackState {
        unsafe { &*self.state }
    }

    #[inline]
    fn s_mut(&mut self) -> &mut CallbackState {
        unsafe { &mut *self.state }
    }
}

impl JitCallbacks for DynarmicCallbacks {
    fn memory_read_code(&self, vaddr: u64) -> Option<u32> {
        let s = self.s();
        Some(unsafe { (s.vtable.read_u32)(s.memory_ctx as *const u8, vaddr) })
    }

    fn memory_read_8(&self, vaddr: u64) -> u8 {
        let s = self.s();
        unsafe { (s.vtable.read_u8)(s.memory_ctx as *const u8, vaddr) }
    }
    fn memory_read_16(&self, vaddr: u64) -> u16 {
        let s = self.s();
        unsafe { (s.vtable.read_u16)(s.memory_ctx as *const u8, vaddr) }
    }
    fn memory_read_32(&self, vaddr: u64) -> u32 {
        let s = self.s();
        unsafe { (s.vtable.read_u32)(s.memory_ctx as *const u8, vaddr) }
    }
    fn memory_read_64(&self, vaddr: u64) -> u64 {
        let s = self.s();
        unsafe { (s.vtable.read_u64)(s.memory_ctx as *const u8, vaddr) }
    }
    fn memory_read_128(&self, vaddr: u64) -> (u64, u64) {
        let s = self.s();
        let lo = unsafe { (s.vtable.read_u64)(s.memory_ctx as *const u8, vaddr) };
        let hi = unsafe { (s.vtable.read_u64)(s.memory_ctx as *const u8, vaddr + 8) };
        (lo, hi)
    }

    fn memory_write_8(&mut self, vaddr: u64, value: u8) {
        let s = self.s();
        unsafe { (s.vtable.write_u8)(s.memory_ctx, vaddr, value) };
    }
    fn memory_write_16(&mut self, vaddr: u64, value: u16) {
        let s = self.s();
        unsafe { (s.vtable.write_u16)(s.memory_ctx, vaddr, value) };
    }
    fn memory_write_32(&mut self, vaddr: u64, value: u32) {
        let s = self.s();
        unsafe { (s.vtable.write_u32)(s.memory_ctx, vaddr, value) };
    }
    fn memory_write_64(&mut self, vaddr: u64, value: u64) {
        let s = self.s();
        unsafe { (s.vtable.write_u64)(s.memory_ctx, vaddr, value) };
    }
    fn memory_write_128(&mut self, vaddr: u64, value_lo: u64, value_hi: u64) {
        let s = self.s();
        unsafe {
            (s.vtable.write_u64)(s.memory_ctx, vaddr, value_lo);
            (s.vtable.write_u64)(s.memory_ctx, vaddr + 8, value_hi);
        }
    }

    fn exclusive_read_8(&self, vaddr: u64) -> u8 { self.memory_read_8(vaddr) }
    fn exclusive_read_16(&self, vaddr: u64) -> u16 { self.memory_read_16(vaddr) }
    fn exclusive_read_32(&self, vaddr: u64) -> u32 { self.memory_read_32(vaddr) }
    fn exclusive_read_64(&self, vaddr: u64) -> u64 { self.memory_read_64(vaddr) }
    fn exclusive_read_128(&self, vaddr: u64) -> (u64, u64) { self.memory_read_128(vaddr) }

    fn exclusive_write_8(&mut self, vaddr: u64, value: u8) -> bool {
        self.memory_write_8(vaddr, value); true
    }
    fn exclusive_write_16(&mut self, vaddr: u64, value: u16) -> bool {
        self.memory_write_16(vaddr, value); true
    }
    fn exclusive_write_32(&mut self, vaddr: u64, value: u32) -> bool {
        self.memory_write_32(vaddr, value); true
    }
    fn exclusive_write_64(&mut self, vaddr: u64, value: u64) -> bool {
        self.memory_write_64(vaddr, value); true
    }
    fn exclusive_write_128(&mut self, vaddr: u64, value_lo: u64, value_hi: u64) -> bool {
        self.memory_write_128(vaddr, value_lo, value_hi); true
    }
    fn exclusive_clear(&mut self) {}

    fn call_supervisor(&mut self, svc_num: u32) {
        // Store SVC number matching zuyu's DynarmicCallbacks32::CallSVC.
        // The JIT halts with HaltReason::SVC and the host reads the number
        // via get_svc_number().
        self.s_mut().svc_swi = svc_num;
    }

    fn exception_raised(&mut self, pc: u64, exception: u64) {
        log::error!("A32 JIT exception: PC=0x{:08X}, exception=0x{:X}", pc, exception);
    }

    fn add_ticks(&mut self, ticks: u64) {
        let s = self.s_mut();
        s.ticks_remaining = s.ticks_remaining.saturating_sub(ticks);
    }

    fn get_ticks_remaining(&self) -> u64 {
        self.s().ticks_remaining
    }
}

// ---------------------------------------------------------------------------
// Public JIT wrapper
// ---------------------------------------------------------------------------

/// ARM32 JIT executor wrapping `rdynarmic::A32Jit`.
///
/// Matches zuyu's `ArmDynarmic32` class. Provides context transfer between
/// the kernel's `CpuState` and the JIT register file.
pub struct ArmDynarmic32 {
    jit: A32Jit,
    _callback_state: Box<CallbackState>,
}

impl ArmDynarmic32 {
    /// Create a new A32 JIT instance.
    ///
    /// # Safety
    /// `memory_ctx` must remain valid for the lifetime of this struct.
    pub fn new(memory_ctx: *mut u8, vtable: MemoryVtable) -> Result<Self, String> {
        let mut callback_state = Box::new(CallbackState {
            memory_ctx,
            vtable,
            ticks_remaining: 0,
            svc_swi: 0,
        });

        let state_ptr: *mut CallbackState = &mut *callback_state;
        let callbacks = DynarmicCallbacks { state: state_ptr };

        let config = JitConfig {
            callbacks: Box::new(callbacks),
            enable_cycle_counting: true,
            code_cache_size: JitConfig::DEFAULT_CODE_CACHE_SIZE,
            optimizations: OptimizationFlag::ALL_SAFE_OPTIMIZATIONS,
            unsafe_optimizations: false,
        };

        let jit = A32Jit::new(config)?;

        Ok(Self {
            jit,
            _callback_state: callback_state,
        })
    }

    /// Run the JIT until a halt condition.
    pub fn run(&mut self) -> HaltReason {
        self.jit.run()
    }

    /// Single-step one instruction.
    pub fn step(&mut self) -> HaltReason {
        self.jit.step()
    }

    /// Request external halt (thread-safe).
    pub fn halt(&self) {
        self.jit.halt_execution(HaltReason::EXTERNAL_HALT);
    }

    /// Set the remaining tick budget.
    pub fn set_ticks_remaining(&mut self, ticks: u64) {
        self._callback_state.ticks_remaining = ticks;
    }

    /// Get the SVC number after the JIT halted with `HaltReason::SVC`.
    ///
    /// Matching zuyu: returns the number stored by the CallSVC callback,
    /// not read from memory.
    pub fn get_svc_number(&self) -> Option<u32> {
        Some(self._callback_state.svc_swi)
    }

    /// Load CPU state into the JIT register file (thread switch-in).
    ///
    /// Matching zuyu's `ArmDynarmic32::SetContext()`:
    /// - R0-R15 from ctx.r[0..16]
    /// - CPSR from ctx.pstate
    /// - Extension registers from ctx.v[]
    /// - FPSCR from ctx.fpsr/fpcr
    /// - TPIDR from ctx.tpidr → CP15 uprw
    ///
    /// CpuState uses u64 for registers; A32 truncates to u32.
    pub fn load_context(&mut self, state: &CpuState) {
        // R0-R12 from x[0..13]
        for i in 0..13 {
            self.jit.set_register(i, state.x[i] as u32);
        }
        // R13 (SP) from CpuState.sp
        self.jit.set_register(13, state.sp as u32);
        // R14 (LR) from x[14]
        self.jit.set_register(14, state.x[14] as u32);
        // R15 (PC) from state.pc
        self.jit.set_pc(state.pc as u32);

        // CPSR from nzcv (PSTATE → CPSR, matching zuyu's j.SetCpsr(ctx.pstate))
        self.jit.set_cpsr(state.nzcv);

        // Extension registers from SIMD/FP v[] array
        // Each D register = 2 consecutive S registers in ext_reg backing store
        for i in 0..32 {
            let lo = state.v[i][0];
            self.jit.set_ext_reg(i * 2, lo as u32);
            self.jit.set_ext_reg(i * 2 + 1, (lo >> 32) as u32);
        }

        // CP15 TPIDR_UPRW from CpuState.tpidr_el0
        // Matching zuyu's `m_cp15->uprw = static_cast<u32>(ctx.tpidr)`
        self.jit.set_cp15_uprw(state.tpidr_el0 as u32);

        self.jit.clear_halt(
            HaltReason::SVC
                | HaltReason::STEP
                | HaltReason::BREAKPOINT
                | HaltReason::EXCEPTION_RAISED
                | HaltReason::EXTERNAL_HALT
                | HaltReason::CACHE_INVALIDATION,
        );
    }

    /// Get the JIT's current PC (as u64 for compatibility).
    pub fn get_pc(&self) -> u64 {
        self.jit.get_pc() as u64
    }

    /// Set the JIT's PC.
    pub fn set_pc(&mut self, pc: u64) {
        self.jit.set_pc(pc as u32);
    }

    /// Clear specific halt reason bits.
    pub fn clear_halt(&self, reason: HaltReason) {
        self.jit.clear_halt(reason);
    }

    /// Save JIT register file into CPU state (thread switch-out).
    ///
    /// Matching zuyu's `ArmDynarmic32::GetContext()`:
    /// - ctx.r[0..16] from R0-R15
    /// - ctx.sp from R13, ctx.lr from R14, ctx.pc from R15
    /// - ctx.pstate from CPSR
    /// - ctx.v[] from extension registers
    /// - ctx.tpidr from CP15 uprw
    pub fn save_context(&self, state: &mut CpuState) {
        // R0-R12
        for i in 0..13 {
            state.x[i] = self.jit.get_register(i) as u64;
        }
        // R14 (LR)
        if state.x.len() > 14 {
            state.x[14] = self.jit.get_register(14) as u64;
        }
        // R13 (SP)
        state.sp = self.jit.get_register(13) as u64;
        // R15 (PC)
        state.pc = self.jit.get_pc() as u64;
        // CPSR → PSTATE
        state.nzcv = self.jit.get_cpsr();

        // Extension registers → v[]
        for i in 0..32 {
            let lo = self.jit.get_ext_reg(i * 2) as u64;
            let hi = self.jit.get_ext_reg(i * 2 + 1) as u64;
            state.v[i] = [lo | (hi << 32), 0];
        }

        // CP15 TPIDR_UPRW → CpuState.tpidr_el0
        // Matching zuyu's `ctx.tpidr = m_cp15->uprw`
        state.tpidr_el0 = self.jit.get_cp15_uprw() as u64;
    }

    /// Invalidate JIT-compiled blocks in a memory range.
    pub fn invalidate_cache_range(&mut self, addr: u64, size: u64) {
        self.jit.invalidate_cache_range(addr, size);
    }
}

impl crate::ArmJit for ArmDynarmic32 {
    fn run(&mut self) -> HaltReason { self.run() }
    fn step(&mut self) -> HaltReason { self.step() }
    fn halt(&self) { self.halt() }
    fn set_ticks_remaining(&mut self, ticks: u64) { self.set_ticks_remaining(ticks) }
    fn get_svc_number(&self) -> Option<u32> { self.get_svc_number() }
    fn load_context(&mut self, state: &CpuState) { self.load_context(state) }
    fn save_context(&self, state: &mut CpuState) { self.save_context(state) }
    fn get_pc(&self) -> u64 { self.get_pc() }
    fn set_pc(&mut self, pc: u64) { self.set_pc(pc) }
    fn clear_halt(&self, reason: HaltReason) { self.clear_halt(reason) }
    fn invalidate_cache_range(&mut self, addr: u64, size: u64) { self.invalidate_cache_range(addr, size) }
}
