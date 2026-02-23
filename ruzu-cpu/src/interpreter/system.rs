// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! System instruction execution (MRS, MSR, SVC, barriers).

use crate::state::CpuState;
use super::StepResult;

/// Known system register encodings.
///
/// These are the packed (op0:op1:CRn:CRm:op2) values from MRS/MSR decoding.
mod sys_reg {
    // TPIDR_EL0: op0=3, op1=3, CRn=13, CRm=0, op2=2
    pub const TPIDR_EL0: u32 = (3 << 14) | (3 << 11) | (13 << 7) | (0 << 3) | 2;

    // CNTFRQ_EL0: op0=3, op1=3, CRn=14, CRm=0, op2=0
    pub const CNTFRQ_EL0: u32 = (3 << 14) | (3 << 11) | (14 << 7) | (0 << 3) | 0;

    // CNTPCT_EL0: op0=3, op1=3, CRn=14, CRm=0, op2=1
    pub const CNTPCT_EL0: u32 = (3 << 14) | (3 << 11) | (14 << 7) | (0 << 3) | 1;

    // CNTVCT_EL0: op0=3, op1=3, CRn=14, CRm=0, op2=2 (virtual counter)
    pub const CNTVCT_EL0: u32 = (3 << 14) | (3 << 11) | (14 << 7) | (0 << 3) | 2;

    // FPCR: op0=3, op1=3, CRn=4, CRm=4, op2=0
    pub const FPCR: u32 = (3 << 14) | (3 << 11) | (4 << 7) | (4 << 3) | 0;

    // FPSR: op0=3, op1=3, CRn=4, CRm=4, op2=1
    pub const FPSR: u32 = (3 << 14) | (3 << 11) | (4 << 7) | (4 << 3) | 1;

    // NZCV: op0=3, op1=3, CRn=4, CRm=2, op2=0
    pub const NZCV: u32 = (3 << 14) | (3 << 11) | (4 << 7) | (2 << 3) | 0;

    // CTR_EL0 (cache type register): op0=3, op1=3, CRn=0, CRm=0, op2=1
    pub const CTR_EL0: u32 = (3 << 14) | (3 << 11) | (0 << 7) | (0 << 3) | 1;

    // DCZID_EL0: op0=3, op1=3, CRn=0, CRm=0, op2=7
    pub const DCZID_EL0: u32 = (3 << 14) | (3 << 11) | (0 << 7) | (0 << 3) | 7;

    // MIDR_EL1: op0=3, op1=0, CRn=0, CRm=0, op2=0
    pub const MIDR_EL1: u32 = (3 << 14) | (0 << 11) | (0 << 7) | (0 << 3) | 0;

    // MPIDR_EL1: op0=3, op1=0, CRn=0, CRm=0, op2=5
    pub const MPIDR_EL1: u32 = (3 << 14) | (0 << 11) | (0 << 7) | (0 << 3) | 5;

    // ID_AA64ISAR0_EL1: op0=3, op1=0, CRn=0, CRm=6, op2=0
    pub const ID_AA64ISAR0_EL1: u32 = (3 << 14) | (0 << 11) | (0 << 7) | (6 << 3) | 0;

    // ID_AA64MMFR0_EL1: op0=3, op1=0, CRn=0, CRm=7, op2=0
    pub const ID_AA64MMFR0_EL1: u32 = (3 << 14) | (0 << 11) | (0 << 7) | (7 << 3) | 0;

    // ID_AA64PFR0_EL1: op0=3, op1=0, CRn=0, CRm=4, op2=0
    pub const ID_AA64PFR0_EL1: u32 = (3 << 14) | (0 << 11) | (0 << 7) | (4 << 3) | 0;

    // PMUSERENR_EL0: op0=3, op1=3, CRn=9, CRm=14, op2=0
    pub const PMUSERENR_EL0: u32 = (3 << 14) | (3 << 11) | (9 << 7) | (14 << 3) | 0;
}

/// Monotonic counter for system ticks (approximation).
static TICK_COUNTER: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

fn get_tick() -> u64 {
    TICK_COUNTER.fetch_add(100, std::sync::atomic::Ordering::Relaxed)
}

pub fn exec_mrs(state: &mut CpuState, rt: u8, sys_reg_enc: u32) -> StepResult {
    let value = match sys_reg_enc {
        sys_reg::TPIDR_EL0 => state.tpidr_el0,
        sys_reg::CNTFRQ_EL0 => ruzu_common::CNTFRQ_HZ,
        sys_reg::CNTPCT_EL0 | sys_reg::CNTVCT_EL0 => get_tick(),
        sys_reg::FPCR => 0, // Default FPCR
        sys_reg::FPSR => 0, // Default FPSR
        sys_reg::NZCV => (state.nzcv as u64) & 0xF000_0000,
        sys_reg::CTR_EL0 => {
            // Cache Type Register: report 64-byte cache lines
            // IminLine=4 (16 words = 64 bytes), DminLine=4
            0x8444_C004
        }
        sys_reg::DCZID_EL0 => {
            // Data Cache Zero ID: block size = 64 bytes (log2(64/4) = 4)
            4
        }
        sys_reg::MIDR_EL1 => {
            // Fake CPU ID: Cortex-A57 like the Switch
            0x411F_D073
        }
        sys_reg::MPIDR_EL1 => {
            // Single core, cluster 0
            0x8000_0000
        }
        sys_reg::ID_AA64ISAR0_EL1 => {
            // AES+SHA1+SHA256+CRC32 supported
            0x0000_0000_0011_1112
        }
        sys_reg::ID_AA64MMFR0_EL1 => {
            // 4KB granule, 48-bit PA
            0x0000_0000_0000_0F10
        }
        sys_reg::ID_AA64PFR0_EL1 => {
            // EL0-EL3 all AArch64
            0x0000_0000_0000_1111
        }
        sys_reg::PMUSERENR_EL0 => {
            // PMU not accessible from EL0
            0
        }
        _ => {
            log::warn!(
                "MRS: unknown system register 0x{:04X} at PC=0x{:016X}",
                sys_reg_enc, state.pc
            );
            0
        }
    };

    state.set_reg(rt as u32, value);
    StepResult::Continue
}

pub fn exec_msr(state: &mut CpuState, rt: u8, sys_reg_enc: u32) -> StepResult {
    let value = state.get_reg(rt as u32);

    match sys_reg_enc {
        sys_reg::TPIDR_EL0 => state.tpidr_el0 = value,
        sys_reg::FPCR => { /* Ignore FPCR writes for now */ }
        sys_reg::FPSR => { /* Ignore FPSR writes for now */ }
        sys_reg::NZCV => state.nzcv = (value as u32) & 0xF000_0000,
        _ => {
            log::warn!(
                "MSR: unknown system register 0x{:04X} = 0x{:016X} at PC=0x{:016X}",
                sys_reg_enc, value, state.pc
            );
        }
    }

    StepResult::Continue
}

pub fn exec_svc(imm: u16) -> StepResult {
    StepResult::Svc(imm)
}

pub fn exec_nop() -> StepResult {
    StepResult::Continue
}

pub fn exec_clrex(state: &mut CpuState) -> StepResult {
    state.exclusive_addr = None;
    StepResult::Continue
}

// Barriers are no-ops in a single-core interpreter.
pub fn exec_dmb() -> StepResult {
    StepResult::Continue
}

pub fn exec_dsb() -> StepResult {
    StepResult::Continue
}

pub fn exec_isb() -> StepResult {
    StepResult::Continue
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mrs_tpidr() {
        let mut s = CpuState::new();
        s.tpidr_el0 = 0x1234_5678;
        exec_mrs(&mut s, 0, sys_reg::TPIDR_EL0);
        assert_eq!(s.get_reg(0), 0x1234_5678);
    }

    #[test]
    fn test_msr_tpidr() {
        let mut s = CpuState::new();
        s.set_reg(0, 0xABCD);
        exec_msr(&mut s, 0, sys_reg::TPIDR_EL0);
        assert_eq!(s.tpidr_el0, 0xABCD);
    }

    #[test]
    fn test_mrs_nzcv() {
        let mut s = CpuState::new();
        s.set_n(true);
        s.set_z(true);
        exec_mrs(&mut s, 0, sys_reg::NZCV);
        assert_eq!(s.get_reg(0) & 0xF000_0000, 0xC000_0000); // N=1, Z=1
    }

    #[test]
    fn test_msr_nzcv() {
        let mut s = CpuState::new();
        s.set_reg(0, 0x6000_0000); // Z=1, C=1
        exec_msr(&mut s, 0, sys_reg::NZCV);
        assert!(s.z());
        assert!(s.c());
        assert!(!s.n());
    }

    #[test]
    fn test_svc() {
        let result = exec_svc(0x29);
        assert!(matches!(result, StepResult::Svc(0x29)));
    }

    #[test]
    fn test_clrex() {
        let mut s = CpuState::new();
        s.exclusive_addr = Some(0x1000);
        exec_clrex(&mut s);
        assert!(s.exclusive_addr.is_none());
    }

    #[test]
    fn test_mrs_mpidr_el1() {
        let mut s = CpuState::new();
        exec_mrs(&mut s, 0, sys_reg::MPIDR_EL1);
        assert_eq!(s.get_reg(0), 0x8000_0000);
    }

    #[test]
    fn test_mrs_id_aa64isar0() {
        let mut s = CpuState::new();
        exec_mrs(&mut s, 0, sys_reg::ID_AA64ISAR0_EL1);
        assert_eq!(s.get_reg(0), 0x0000_0000_0011_1112);
    }

    #[test]
    fn test_mrs_id_aa64mmfr0() {
        let mut s = CpuState::new();
        exec_mrs(&mut s, 0, sys_reg::ID_AA64MMFR0_EL1);
        assert_eq!(s.get_reg(0), 0x0000_0000_0000_0F10);
    }

    #[test]
    fn test_mrs_id_aa64pfr0() {
        let mut s = CpuState::new();
        exec_mrs(&mut s, 0, sys_reg::ID_AA64PFR0_EL1);
        assert_eq!(s.get_reg(0), 0x0000_0000_0000_1111);
    }

    #[test]
    fn test_mrs_pmuserenr() {
        let mut s = CpuState::new();
        exec_mrs(&mut s, 0, sys_reg::PMUSERENR_EL0);
        assert_eq!(s.get_reg(0), 0);
    }
}
