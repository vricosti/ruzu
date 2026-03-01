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
use rdynarmic::jit_config::{JitCallbacks, JitConfig};

// ---------------------------------------------------------------------------
// Shared callback state
// ---------------------------------------------------------------------------

struct CallbackState {
    memory_ctx: *mut u8,
    vtable: MemoryVtable,
    ticks_remaining: u64,
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

    fn call_supervisor(&mut self, _svc_num: u32) {
        // The JIT sets halt_reason = SVC. The SVC number is extracted from
        // the instruction at PC-4 (ARM) or PC-2 (Thumb) after run() returns.
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
        });

        let state_ptr: *mut CallbackState = &mut *callback_state;
        let callbacks = DynarmicCallbacks { state: state_ptr };

        let config = JitConfig {
            callbacks: Box::new(callbacks),
            enable_cycle_counting: true,
            code_cache_size: JitConfig::DEFAULT_CODE_CACHE_SIZE,
            enable_optimizations: true,
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
    /// ARM32: SVC instruction at PC-4, encoding `0xEF000000 | imm24`.
    /// Thumb: SVC instruction at PC-2, encoding `0xDF00 | imm8`.
    /// The CPSR T flag determines the instruction set.
    pub fn get_svc_number(&self) -> Option<u32> {
        let pc = self.jit.get_pc() as u64;
        let cpsr = self.jit.get_cpsr();
        let thumb = cpsr & (1 << 5) != 0;
        let s = &*self._callback_state;

        if thumb {
            if pc < 2 { return None; }
            let instr = unsafe { (s.vtable.read_u16)(s.memory_ctx as *const u8, pc - 2) };
            // Thumb SVC: 1101_1111 imm8
            if instr & 0xFF00 != 0xDF00 {
                log::warn!("Expected Thumb SVC at 0x{:08X} but got 0x{:04X}", pc - 2, instr);
                return None;
            }
            Some((instr & 0xFF) as u32)
        } else {
            if pc < 4 { return None; }
            let instr = unsafe { (s.vtable.read_u32)(s.memory_ctx as *const u8, pc - 4) };
            // ARM SVC: cond 1111 imm24
            if instr & 0x0F00_0000 != 0x0F00_0000 {
                log::warn!("Expected ARM SVC at 0x{:08X} but got 0x{:08X}", pc - 4, instr);
                return None;
            }
            Some(instr & 0x00FF_FFFF)
        }
    }

    /// Load CPU state into the JIT register file (thread switch-in).
    ///
    /// CpuState uses u64 for registers; A32 truncates to u32.
    pub fn load_context(&mut self, state: &CpuState) {
        // R0-R14 from x[0..15], R15 (PC) from state.pc
        for i in 0..15 {
            self.jit.set_register(i, state.x[i] as u32);
        }
        self.jit.set_pc(state.pc as u32);

        // CPSR from nzcv (in ARM format: bits [31:28] = NZCV)
        self.jit.set_cpsr(state.nzcv);

        // Extension registers from SIMD/FP v[] array (S regs = low 32 bits)
        for i in 0..32 {
            let lo = state.v[i][0];
            // Each D register = 2 consecutive S registers in ext_reg backing store
            self.jit.set_ext_reg(i * 2, lo as u32);
            self.jit.set_ext_reg(i * 2 + 1, (lo >> 32) as u32);
        }

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
    pub fn save_context(&self, state: &mut CpuState) {
        for i in 0..15 {
            state.x[i] = self.jit.get_register(i) as u64;
        }
        state.sp = self.jit.get_register(13) as u64;
        state.pc = self.jit.get_pc() as u64;
        state.nzcv = self.jit.get_cpsr();

        for i in 0..32 {
            let lo = self.jit.get_ext_reg(i * 2) as u64;
            let hi = self.jit.get_ext_reg(i * 2 + 1) as u64;
            state.v[i] = [lo | (hi << 32), 0];
        }
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
