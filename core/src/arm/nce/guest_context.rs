// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/arm/nce/guest_context.h
//! Guest and host context structures for NCE execution.

use std::sync::atomic::AtomicU64;

/// 128-bit value for vector registers.
///
/// Corresponds to upstream `u128` (Common::u128).
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
#[repr(C, align(16))]
pub struct U128 {
    pub lo: u64,
    pub hi: u64,
}

/// Host context saved during guest execution.
///
/// Corresponds to upstream `Core::HostContext`.
#[derive(Debug)]
#[repr(C)]
pub struct HostContext {
    /// Callee-saved general-purpose registers (x19-x30).
    /// 12 registers saved.
    pub host_saved_regs: [u64; 12],

    /// Callee-saved vector registers (v8-v15).
    /// 8 x 128-bit registers.
    pub host_saved_vregs: [U128; 8],

    /// Host stack pointer.
    pub host_sp: u64,

    /// Host TPIDR_EL0 value.
    pub host_tpidr_el0: *mut std::ffi::c_void,
}

// SAFETY: Raw pointer field is only used for thread-local storage restoration.
unsafe impl Send for HostContext {}
unsafe impl Sync for HostContext {}

impl Default for HostContext {
    fn default() -> Self {
        Self {
            host_saved_regs: [0u64; 12],
            host_saved_vregs: [U128::default(); 8],
            host_sp: 0,
            host_tpidr_el0: std::ptr::null_mut(),
        }
    }
}

/// Guest CPU context for NCE execution.
///
/// Corresponds to upstream `Core::GuestContext`.
#[repr(C)]
pub struct GuestContext {
    /// General-purpose registers x0-x30.
    pub cpu_registers: [u64; 31],

    /// Stack pointer.
    pub sp: u64,

    /// Program counter.
    pub pc: u64,

    /// Floating-point control register.
    pub fpcr: u32,

    /// Floating-point status register.
    pub fpsr: u32,

    /// Vector registers v0-v31.
    pub vector_registers: [U128; 32],

    /// Processor state (NZCV flags, etc.).
    pub pstate: u32,

    /// Host context saved during guest execution.
    pub host_ctx: HostContext,

    /// Thread-local storage register (read-only, EL0).
    pub tpidrro_el0: u64,

    /// Thread-local storage register (read-write, EL0).
    pub tpidr_el0: u64,

    /// Exception syndrome register (EL1), used for halt reason signaling.
    pub esr_el1: AtomicU64,

    /// NZCV condition flags (separate from pstate for atomic access).
    pub nzcv: u32,

    /// SVC number being executed.
    pub svc: u32,

    /// Back-pointer to the owning System.
    /// Corresponds to upstream `System* system`.
    pub system: *mut std::ffi::c_void,

    /// Back-pointer to the owning ArmNce instance.
    /// Corresponds to upstream `ArmNce* parent`.
    pub parent: *mut std::ffi::c_void,
}

// SAFETY: Raw pointer fields are only accessed from the owning thread during execution.
unsafe impl Send for GuestContext {}
unsafe impl Sync for GuestContext {}

impl Default for GuestContext {
    fn default() -> Self {
        Self {
            cpu_registers: [0u64; 31],
            sp: 0,
            pc: 0,
            fpcr: 0,
            fpsr: 0,
            vector_registers: [U128::default(); 32],
            pstate: 0,
            host_ctx: HostContext::default(),
            tpidrro_el0: 0,
            tpidr_el0: 0,
            esr_el1: AtomicU64::new(0),
            nzcv: 0,
            svc: 0,
            system: std::ptr::null_mut(),
            parent: std::ptr::null_mut(),
        }
    }
}

// NOTE: Upstream verifies assembly offsets with static_assert:
//   static_assert(offsetof(GuestContext, sp) == GuestContextSp);
//   static_assert(offsetof(GuestContext, host_ctx) == GuestContextHostContext);
//   static_assert(offsetof(HostContext, host_sp) == HostContextSpTpidrEl0);
//   etc.
// These would need to be verified at build time or via tests if the struct
// is used for direct memory mapping with assembly code.
