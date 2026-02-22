// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use ruzu_common::VAddr;

use crate::memory::MemoryAccess;

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

    // -- NZCV flag helpers --------------------------------------------------

    /// Negative flag (bit 31 of nzcv).
    #[inline]
    pub fn n(&self) -> bool {
        self.nzcv & (1 << 31) != 0
    }

    #[inline]
    pub fn set_n(&mut self, v: bool) {
        if v {
            self.nzcv |= 1 << 31;
        } else {
            self.nzcv &= !(1 << 31);
        }
    }

    /// Zero flag (bit 30 of nzcv).
    #[inline]
    pub fn z(&self) -> bool {
        self.nzcv & (1 << 30) != 0
    }

    #[inline]
    pub fn set_z(&mut self, v: bool) {
        if v {
            self.nzcv |= 1 << 30;
        } else {
            self.nzcv &= !(1 << 30);
        }
    }

    /// Carry flag (bit 29 of nzcv).
    #[inline]
    pub fn c(&self) -> bool {
        self.nzcv & (1 << 29) != 0
    }

    #[inline]
    pub fn set_c(&mut self, v: bool) {
        if v {
            self.nzcv |= 1 << 29;
        } else {
            self.nzcv &= !(1 << 29);
        }
    }

    /// Overflow flag (bit 28 of nzcv).
    #[inline]
    pub fn v_flag(&self) -> bool {
        self.nzcv & (1 << 28) != 0
    }

    #[inline]
    pub fn set_v(&mut self, v: bool) {
        if v {
            self.nzcv |= 1 << 28;
        } else {
            self.nzcv &= !(1 << 28);
        }
    }

    /// Set NZCV from a 64-bit addition: result = a + b.
    #[inline]
    pub fn set_nzcv_from_add64(&mut self, result: u64, a: u64, b: u64) {
        self.set_n(result & (1u64 << 63) != 0);
        self.set_z(result == 0);
        self.set_c(result < a); // unsigned overflow
        self.set_v(((a ^ !b) & (a ^ result)) >> 63 != 0); // signed overflow
    }

    /// Set NZCV from a 32-bit addition: result = a + b (all masked to 32 bits).
    #[inline]
    pub fn set_nzcv_from_add32(&mut self, result: u32, a: u32, b: u32) {
        self.set_n(result & (1u32 << 31) != 0);
        self.set_z(result == 0);
        self.set_c((result as u64) < (a as u64) + (b as u64).min(1));
        // More precise: carry if a + b > u32::MAX
        self.set_c((a as u64).wrapping_add(b as u64) > u32::MAX as u64);
        self.set_v(((a ^ !b) & (a ^ result)) >> 31 != 0);
    }

    /// Set NZCV from a 64-bit subtraction: result = a - b.
    #[inline]
    pub fn set_nzcv_from_sub64(&mut self, result: u64, a: u64, b: u64) {
        self.set_n(result & (1u64 << 63) != 0);
        self.set_z(result == 0);
        self.set_c(a >= b); // borrow: no borrow if a >= b
        self.set_v(((a ^ b) & (a ^ result)) >> 63 != 0);
    }

    /// Set NZCV from a 32-bit subtraction: result = a - b.
    #[inline]
    pub fn set_nzcv_from_sub32(&mut self, result: u32, a: u32, b: u32) {
        self.set_n(result & (1u32 << 31) != 0);
        self.set_z(result == 0);
        self.set_c(a >= b);
        self.set_v(((a ^ b) & (a ^ result)) >> 31 != 0);
    }

    /// Evaluate an ARM64 condition code against the current NZCV flags.
    #[inline]
    pub fn check_condition(&self, cond: u8) -> bool {
        let result = match cond >> 1 {
            0 => self.z(),                                    // EQ/NE
            1 => self.c(),                                    // CS/CC
            2 => self.n(),                                    // MI/PL
            3 => self.v_flag(),                               // VS/VC
            4 => self.c() && !self.z(),                       // HI/LS
            5 => self.n() == self.v_flag(),                   // GE/LT
            6 => self.n() == self.v_flag() && !self.z(),      // GT/LE
            7 => true,                                        // AL
            _ => unreachable!(),
        };
        // Invert for odd condition codes (NE, CC, PL, VC, LS, LT, LE)
        if cond & 1 != 0 && cond != 0xF {
            !result
        } else {
            result
        }
    }

    // -- SIMD/FP register helpers -------------------------------------------

    /// Get the low 64-bit lane of a V register.
    #[inline]
    pub fn get_vreg_u64(&self, reg: u32, lane: u32) -> u64 {
        self.v[reg as usize][lane as usize]
    }

    /// Set the low 64-bit lane of a V register.
    #[inline]
    pub fn set_vreg_u64(&mut self, reg: u32, lane: u32, val: u64) {
        self.v[reg as usize][lane as usize] = val;
    }

    /// Get a V register as a 128-bit value.
    #[inline]
    pub fn get_vreg_u128(&self, reg: u32) -> u128 {
        let lo = self.v[reg as usize][0] as u128;
        let hi = self.v[reg as usize][1] as u128;
        lo | (hi << 64)
    }

    /// Set a V register from a 128-bit value.
    #[inline]
    pub fn set_vreg_u128(&mut self, reg: u32, val: u128) {
        self.v[reg as usize][0] = val as u64;
        self.v[reg as usize][1] = (val >> 64) as u64;
    }
}

/// Trait for CPU execution backend.
pub trait CpuExecutor: Send {
    /// Run the CPU until a halt condition (SVC, exception, or step count).
    fn run(
        &mut self,
        state: &mut CpuState,
        mem: &mut dyn MemoryAccess,
    ) -> HaltReason;

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
    /// Instruction budget exhausted (time slice done).
    BudgetExhausted,
}
