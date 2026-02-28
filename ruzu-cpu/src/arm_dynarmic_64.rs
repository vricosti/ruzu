// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! ARM64 JIT wrapper using rdynarmic (Rust port of dynarmic).
//!
//! This is the Rust equivalent of zuyu's `arm_dynarmic_64.cpp`.
//! Wraps `A64Jit` and provides context transfer between `CpuState`
//! (used by the kernel for thread switching) and the JIT's register file.
//!
//! Memory access is performed through a raw pointer to an opaque type and
//! a vtable of function pointers, avoiding a circular dependency between
//! ruzu-cpu and ruzu-kernel.

use crate::state::CpuState;
use rdynarmic::halt_reason::HaltReason;
use rdynarmic::jit::A64Jit;
use rdynarmic::jit_config::{JitCallbacks, JitConfig};

// ---------------------------------------------------------------------------
// Memory access vtable (avoids circular crate dependency)
// ---------------------------------------------------------------------------

/// Function-pointer table for guest memory access.
///
/// The caller (e.g. `main.rs`) fills this in with closures that forward to
/// `MemoryManager`.  All functions take a raw context pointer as the first
/// argument and return a default value on unmapped access.
#[derive(Clone, Copy)]
pub struct MemoryVtable {
    pub read_u8:   unsafe fn(*const u8, u64) -> u8,
    pub read_u16:  unsafe fn(*const u8, u64) -> u16,
    pub read_u32:  unsafe fn(*const u8, u64) -> u32,
    pub read_u64:  unsafe fn(*const u8, u64) -> u64,
    pub write_u8:  unsafe fn(*mut u8, u64, u8),
    pub write_u16: unsafe fn(*mut u8, u64, u16),
    pub write_u32: unsafe fn(*mut u8, u64, u32),
    pub write_u64: unsafe fn(*mut u8, u64, u64),
}

// ---------------------------------------------------------------------------
// Shared callback state
// ---------------------------------------------------------------------------

/// Mutable state shared between `DynarmicCallbacks` (owned by the JIT) and
/// `ArmDynarmic64` (the public wrapper).  Heap-allocated for pointer stability.
struct CallbackState {
    /// Opaque memory context pointer (points to MemoryManager).
    memory_ctx: *mut u8,
    /// Function-pointer table for memory access.
    vtable: MemoryVtable,
    /// Remaining ticks in the current time slice.
    ticks_remaining: u64,
}

// ---------------------------------------------------------------------------
// JIT callbacks
// ---------------------------------------------------------------------------

/// Implements `rdynarmic::JitCallbacks` by delegating memory operations
/// through the vtable and managing the cycle budget.
struct DynarmicCallbacks {
    state: *mut CallbackState,
}

// SAFETY: Single-threaded emulation — the JIT, callbacks, and memory manager
// are never accessed from multiple threads simultaneously.
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
    // -- Code read -----------------------------------------------------------

    fn memory_read_code(&self, vaddr: u64) -> Option<u32> {
        let s = self.s();
        Some(unsafe { (s.vtable.read_u32)(s.memory_ctx as *const u8, vaddr) })
    }

    // -- Data reads ----------------------------------------------------------

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

    // -- Data writes ---------------------------------------------------------

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

    // -- Exclusive memory (single-core: always succeed) ----------------------

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

    // -- System callbacks ----------------------------------------------------

    fn call_supervisor(&mut self, _svc_num: u32) {
        // The JIT-generated code already sets halt_reason = SVC before
        // calling this callback. The SVC number is extracted from the
        // instruction at PC-4 after run() returns.
    }

    fn exception_raised(&mut self, pc: u64, exception: u64) {
        log::error!("Exception raised at PC=0x{:016X}, exception=0x{:X}", pc, exception);
    }

    // -- Tick management -----------------------------------------------------

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

/// ARM64 JIT executor wrapping `rdynarmic::A64Jit`.
///
/// Matches zuyu's `ArmDynarmic64` class.  Provides context transfer between
/// the kernel's `CpuState` and the JIT register file.
pub struct ArmDynarmic64 {
    jit: A64Jit,
    /// Heap-allocated shared state. The callbacks hold a raw pointer to this.
    /// Must outlive the JIT (dropped after `jit` since fields drop in order).
    _callback_state: Box<CallbackState>,
}

impl ArmDynarmic64 {
    /// Create a new JIT instance.
    ///
    /// `memory_ctx` is an opaque pointer forwarded to every vtable function
    /// (typically `&mut MemoryManager as *mut _ as *mut u8`).
    ///
    /// # Safety
    /// `memory_ctx` must remain valid for the lifetime of this struct, and the
    /// vtable functions must be safe to call with it.
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

        let jit = A64Jit::new(config)?;

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

    /// Set the remaining tick budget for the current time slice.
    pub fn set_ticks_remaining(&mut self, ticks: u64) {
        self._callback_state.ticks_remaining = ticks;
    }

    /// Get the SVC number after the JIT halted with `HaltReason::SVC`.
    ///
    /// Extracts the immediate from the SVC instruction at PC-4 (the frontend
    /// advances PC past the SVC before halting).
    pub fn get_svc_number(&self) -> Option<u32> {
        let pc = self.jit.get_pc();
        if pc < 4 {
            return None;
        }
        // Read the SVC instruction at PC-4.
        let s = &*self._callback_state;
        let instr = unsafe { (s.vtable.read_u32)(s.memory_ctx as *const u8, pc - 4) };
        // ARM64 SVC encoding: 1101_0100_000 imm16 000_01
        if instr & 0xFFE0_001F != 0xD400_0001 {
            log::warn!(
                "Expected SVC at 0x{:016X} but got 0x{:08X}",
                pc - 4, instr
            );
            return None;
        }
        Some((instr >> 5) & 0xFFFF)
    }

    /// Load CPU state into the JIT register file (thread switch-in).
    pub fn load_context(&mut self, state: &CpuState) {
        for i in 0..31 {
            self.jit.set_register(i, state.x[i]);
        }
        self.jit.set_sp(state.sp);
        self.jit.set_pc(state.pc);
        self.jit.set_pstate(state.nzcv);
        for i in 0..32 {
            self.jit.set_vector(i, state.v[i][0], state.v[i][1]);
        }
        self.jit.set_tpidr_el0(state.tpidr_el0);

        // Clear any stale halt reason from previous execution.
        self.jit.clear_halt(
            HaltReason::SVC
                | HaltReason::STEP
                | HaltReason::BREAKPOINT
                | HaltReason::EXCEPTION_RAISED
                | HaltReason::EXTERNAL_HALT
                | HaltReason::CACHE_INVALIDATION,
        );
    }

    /// Save JIT register file into CPU state (thread switch-out).
    pub fn save_context(&self, state: &mut CpuState) {
        for i in 0..31 {
            state.x[i] = self.jit.get_register(i);
        }
        state.sp = self.jit.get_sp();
        state.pc = self.jit.get_pc();
        state.nzcv = self.jit.get_pstate();
        for i in 0..32 {
            let (lo, hi) = self.jit.get_vector(i);
            state.v[i] = [lo, hi];
        }
        state.tpidr_el0 = self.jit.get_tpidr_el0();
    }

    /// Invalidate JIT-compiled blocks in a memory range.
    pub fn invalidate_cache_range(&mut self, addr: u64, size: u64) {
        self.jit.invalidate_cache_range(addr, size);
    }
}
