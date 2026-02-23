// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Branch instruction execution.

use crate::state::CpuState;
use super::StepResult;

pub fn exec_b(state: &mut CpuState, imm: i64) -> StepResult {
    state.pc = (state.pc as i64).wrapping_add(imm) as u64;
    StepResult::BranchTaken
}

pub fn exec_bl(state: &mut CpuState, imm: i64) -> StepResult {
    state.set_lr(state.pc.wrapping_add(4));
    state.pc = (state.pc as i64).wrapping_add(imm) as u64;
    StepResult::BranchTaken
}

pub fn exec_br(state: &mut CpuState, rn: u8) -> StepResult {
    state.pc = state.get_reg(rn as u32);
    StepResult::BranchTaken
}

pub fn exec_blr(state: &mut CpuState, rn: u8) -> StepResult {
    let target = state.get_reg(rn as u32);
    state.set_lr(state.pc.wrapping_add(4));
    state.pc = target;
    StepResult::BranchTaken
}

pub fn exec_ret(state: &mut CpuState, rn: u8) -> StepResult {
    state.pc = state.get_reg(rn as u32);
    StepResult::BranchTaken
}

pub fn exec_bcond(state: &mut CpuState, cond: u8, imm: i64) -> StepResult {
    if state.check_condition(cond) {
        state.pc = (state.pc as i64).wrapping_add(imm) as u64;
        StepResult::BranchTaken
    } else {
        StepResult::Continue
    }
}

pub fn exec_cbz(state: &mut CpuState, sf: bool, rt: u8, imm: i64) -> StepResult {
    let val = state.get_reg(rt as u32);
    let is_zero = if sf { val == 0 } else { val as u32 == 0 };

    if is_zero {
        state.pc = (state.pc as i64).wrapping_add(imm) as u64;
        StepResult::BranchTaken
    } else {
        StepResult::Continue
    }
}

pub fn exec_cbnz(state: &mut CpuState, sf: bool, rt: u8, imm: i64) -> StepResult {
    let val = state.get_reg(rt as u32);
    let is_zero = if sf { val == 0 } else { val as u32 == 0 };

    if !is_zero {
        state.pc = (state.pc as i64).wrapping_add(imm) as u64;
        StepResult::BranchTaken
    } else {
        StepResult::Continue
    }
}

pub fn exec_tbz(state: &mut CpuState, rt: u8, bit: u8, imm: i64) -> StepResult {
    let val = state.get_reg(rt as u32);
    if val & (1u64 << bit) == 0 {
        state.pc = (state.pc as i64).wrapping_add(imm) as u64;
        StepResult::BranchTaken
    } else {
        StepResult::Continue
    }
}

pub fn exec_tbnz(state: &mut CpuState, rt: u8, bit: u8, imm: i64) -> StepResult {
    let val = state.get_reg(rt as u32);
    if val & (1u64 << bit) != 0 {
        state.pc = (state.pc as i64).wrapping_add(imm) as u64;
        StepResult::BranchTaken
    } else {
        StepResult::Continue
    }
}

pub fn exec_brk(_imm: u16) -> StepResult {
    StepResult::Breakpoint
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_b() {
        let mut s = CpuState::new();
        s.pc = 0x1000;
        let result = exec_b(&mut s, 16);
        assert!(matches!(result, StepResult::BranchTaken));
        assert_eq!(s.pc, 0x1010);
    }

    #[test]
    fn test_bl_saves_lr() {
        let mut s = CpuState::new();
        s.pc = 0x1000;
        let result = exec_bl(&mut s, 0x100);
        assert!(matches!(result, StepResult::BranchTaken));
        assert_eq!(s.pc, 0x1100);
        assert_eq!(s.lr(), 0x1004); // return address
    }

    #[test]
    fn test_cbz_taken() {
        let mut s = CpuState::new();
        s.pc = 0x1000;
        s.set_reg(0, 0);
        let result = exec_cbz(&mut s, true, 0, 8);
        assert!(matches!(result, StepResult::BranchTaken));
        assert_eq!(s.pc, 0x1008);
    }

    #[test]
    fn test_cbz_not_taken() {
        let mut s = CpuState::new();
        s.pc = 0x1000;
        s.set_reg(0, 42);
        let result = exec_cbz(&mut s, true, 0, 8);
        assert!(matches!(result, StepResult::Continue));
        assert_eq!(s.pc, 0x1000); // unchanged
    }

    #[test]
    fn test_ret() {
        let mut s = CpuState::new();
        s.set_lr(0x2000);
        let result = exec_ret(&mut s, 30);
        assert!(matches!(result, StepResult::BranchTaken));
        assert_eq!(s.pc, 0x2000);
    }
}
