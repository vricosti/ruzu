// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use ruzu_common::VAddr;

/// ARM64 CPU state: general-purpose registers, SIMD/FP registers, flags.
#[derive(Debug, Clone)]
pub struct CpuState {
    /// General-purpose registers X0-X30.
    pub x: [u64; 31],
    /// Stack pointer (SP).
    pub sp: u64,
    /// Program counter (PC).
    pub pc: u64,
    /// PSTATE flags: Negative, Zero, Carry, Overflow.
    pub nzcv: u32,
    /// SIMD/FP registers V0-V31 (128-bit each, stored as two u64s).
    pub v: [[u64; 2]; 32],
    /// TPIDR_EL0 (thread pointer, used for TLS).
    pub tpidr_el0: u64,
    /// Exclusive monitor address (for LDXR/STXR).
    pub exclusive_addr: Option<VAddr>,
    /// Exclusive monitor value.
    pub exclusive_value: u64,
}

impl Default for CpuState {
    fn default() -> Self {
        Self {
            x: [0; 31],
            sp: 0,
            pc: 0,
            nzcv: 0,
            v: [[0; 2]; 32],
            tpidr_el0: 0,
            exclusive_addr: None,
            exclusive_value: 0,
        }
    }
}

impl CpuState {
    pub fn new() -> Self {
        Self::default()
    }

    /// Get a general-purpose register by index (0-30).
    /// Index 31 returns SP (matching ARM64 encoding).
    #[inline]
    pub fn get_reg(&self, index: u32) -> u64 {
        if index == 31 {
            self.sp
        } else {
            self.x[index as usize]
        }
    }

    /// Set a general-purpose register by index (0-30).
    /// Index 31 sets SP.
    #[inline]
    pub fn set_reg(&mut self, index: u32, value: u64) {
        if index == 31 {
            self.sp = value;
        } else {
            self.x[index as usize] = value;
        }
    }

    /// Get the link register (X30).
    #[inline]
    pub fn lr(&self) -> u64 {
        self.x[30]
    }

    /// Set the link register (X30).
    #[inline]
    pub fn set_lr(&mut self, value: u64) {
        self.x[30] = value;
    }

    /// Get the frame pointer (X29).
    #[inline]
    pub fn fp(&self) -> u64 {
        self.x[29]
    }
}

/// Trait for CPU execution backend.
pub trait CpuExecutor: Send {
    /// Run the CPU until a halt condition (SVC, exception, or step count).
    fn run(&mut self, state: &mut CpuState) -> HaltReason;

    /// Halt CPU execution.
    fn halt(&mut self);

    /// Invalidate cached JIT code for the given address range.
    fn invalidate_cache_range(&mut self, addr: VAddr, size: u64);
}

/// Reason the CPU halted execution.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HaltReason {
    /// Supervisor call (SVC instruction). The SVC number is stored.
    Svc(u32),
    /// Breakpoint or software breakpoint.
    Breakpoint,
    /// Data abort (invalid memory access).
    DataAbort { addr: VAddr },
    /// Instruction abort (invalid fetch).
    InstructionAbort { addr: VAddr },
    /// Step completed (single-step mode).
    Step,
    /// CPU was externally halted.
    ExternalHalt,
}
