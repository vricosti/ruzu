// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Load/Store instruction execution.

use crate::decoder::*;
use crate::memory::MemoryAccess;
use crate::state::CpuState;
use super::StepResult;

/// Helper: compute effective address for register-offset loads/stores.
fn reg_offset(state: &CpuState, rm: u8, extend: ExtendType, shift: bool, size: u8) -> u64 {
    let val = state.get_reg(rm as u32);
    let extended = match extend {
        ExtendType::UXTB => val & 0xFF,
        ExtendType::UXTH => val & 0xFFFF,
        ExtendType::UXTW => val & 0xFFFF_FFFF,
        ExtendType::UXTX => val,
        ExtendType::SXTB => (val as i8) as i64 as u64,
        ExtendType::SXTH => (val as i16) as i64 as u64,
        ExtendType::SXTW => (val as i32) as i64 as u64,
        ExtendType::SXTX => val,
    };
    if shift { extended << size } else { extended }
}

/// Helper: apply addressing mode (update base register for pre/post-index).
fn apply_addr_mode(state: &mut CpuState, rn: u8, base: u64, offset: i64,
                   mode: AddrMode) -> u64 {
    match mode {
        AddrMode::Offset => (base as i64).wrapping_add(offset) as u64,
        AddrMode::PreIndex => {
            let addr = (base as i64).wrapping_add(offset) as u64;
            state.set_reg(rn as u32, addr);
            addr
        }
        AddrMode::PostIndex => {
            // Access at base, then update base
            let wb = (base as i64).wrapping_add(offset) as u64;
            state.set_reg(rn as u32, wb);
            base
        }
    }
}

/// Read a value of the given byte size, optionally sign-extending.
fn mem_read(mem: &dyn MemoryAccess, addr: u64, size: u8,
            sign_extend: bool, to_64: bool) -> Result<u64, u64> {
    let val = match size {
        0 => mem.read_u8(addr).map(|v| v as u64),
        1 => mem.read_u16(addr).map(|v| v as u64),
        2 => mem.read_u32(addr).map(|v| v as u64),
        3 => mem.read_u64(addr).map(|v| v),
        _ => return Err(addr),
    };

    match val {
        Ok(v) => {
            if sign_extend {
                let extended = match size {
                    0 => (v as i8) as i64 as u64,
                    1 => (v as i16) as i64 as u64,
                    2 => (v as i32) as i64 as u64,
                    _ => v,
                };
                Ok(if to_64 { extended } else { extended & 0xFFFF_FFFF })
            } else {
                Ok(v)
            }
        }
        Err(_) => Err(addr),
    }
}

/// Write a value of the given byte size.
fn mem_write(mem: &mut dyn MemoryAccess, addr: u64, val: u64, size: u8) -> Result<(), u64> {
    let result = match size {
        0 => mem.write_u8(addr, val as u8),
        1 => mem.write_u16(addr, val as u16),
        2 => mem.write_u32(addr, val as u32),
        3 => mem.write_u64(addr, val),
        _ => return Err(addr),
    };
    result.map_err(|_| addr)
}

pub fn exec_ldr_imm(state: &mut CpuState, mem: &dyn MemoryAccess,
                     _sf: bool, rt: u8, rn: u8, imm: i64, size: u8,
                     mode: AddrMode, sign_ext: bool) -> StepResult {
    let base = state.get_reg(rn as u32);
    let addr = apply_addr_mode(state, rn, base, imm, mode);
    let to_64 = size == 3 || (sign_ext && size < 3);

    match mem_read(mem, addr, size, sign_ext, to_64) {
        Ok(val) => {
            state.set_reg(rt as u32, val);
            StepResult::Continue
        }
        Err(a) => StepResult::MemoryFault(a),
    }
}

pub fn exec_str_imm(state: &mut CpuState, mem: &mut dyn MemoryAccess,
                     _sf: bool, rt: u8, rn: u8, imm: i64, size: u8,
                     mode: AddrMode) -> StepResult {
    let base = state.get_reg(rn as u32);
    let addr = apply_addr_mode(state, rn, base, imm, mode);
    let val = state.get_reg(rt as u32);

    match mem_write(mem, addr, val, size) {
        Ok(()) => StepResult::Continue,
        Err(a) => StepResult::MemoryFault(a),
    }
}

pub fn exec_ldr_reg(state: &mut CpuState, mem: &dyn MemoryAccess,
                     _sf: bool, rt: u8, rn: u8, rm: u8, size: u8,
                     extend: ExtendType, shift: bool, sign_ext: bool) -> StepResult {
    let base = state.get_reg(rn as u32);
    let offset = reg_offset(state, rm, extend, shift, size);
    let addr = base.wrapping_add(offset);
    let to_64 = size == 3 || (sign_ext && size < 3);

    match mem_read(mem, addr, size, sign_ext, to_64) {
        Ok(val) => {
            state.set_reg(rt as u32, val);
            StepResult::Continue
        }
        Err(a) => StepResult::MemoryFault(a),
    }
}

pub fn exec_str_reg(state: &mut CpuState, mem: &mut dyn MemoryAccess,
                     _sf: bool, rt: u8, rn: u8, rm: u8, size: u8,
                     extend: ExtendType, shift: bool) -> StepResult {
    let base = state.get_reg(rn as u32);
    let offset = reg_offset(state, rm, extend, shift, size);
    let addr = base.wrapping_add(offset);
    let val = state.get_reg(rt as u32);

    match mem_write(mem, addr, val, size) {
        Ok(()) => StepResult::Continue,
        Err(a) => StepResult::MemoryFault(a),
    }
}

pub fn exec_ldr_lit(state: &mut CpuState, mem: &dyn MemoryAccess,
                     sf: bool, rt: u8, imm: i64, sign_ext: bool) -> StepResult {
    let addr = (state.pc as i64).wrapping_add(imm) as u64;
    let size = if sf { 3 } else { 2 };

    match mem_read(mem, addr, size, sign_ext, sf) {
        Ok(val) => {
            state.set_reg(rt as u32, val);
            StepResult::Continue
        }
        Err(a) => StepResult::MemoryFault(a),
    }
}

pub fn exec_ldp(state: &mut CpuState, mem: &dyn MemoryAccess,
                 sf: bool, rt: u8, rt2: u8, rn: u8, imm: i64,
                 mode: AddrMode) -> StepResult {
    let base = state.get_reg(rn as u32);
    let addr = apply_addr_mode(state, rn, base, imm, mode);
    let size: u8 = if sf { 3 } else { 2 };
    let bytes = 1u64 << size;

    let val1 = match mem_read(mem, addr, size, false, sf) {
        Ok(v) => v,
        Err(a) => return StepResult::MemoryFault(a),
    };
    let val2 = match mem_read(mem, addr.wrapping_add(bytes), size, false, sf) {
        Ok(v) => v,
        Err(a) => return StepResult::MemoryFault(a),
    };

    state.set_reg(rt as u32, val1);
    state.set_reg(rt2 as u32, val2);
    StepResult::Continue
}

pub fn exec_stp(state: &mut CpuState, mem: &mut dyn MemoryAccess,
                 sf: bool, rt: u8, rt2: u8, rn: u8, imm: i64,
                 mode: AddrMode) -> StepResult {
    let base = state.get_reg(rn as u32);
    let addr = apply_addr_mode(state, rn, base, imm, mode);
    let size: u8 = if sf { 3 } else { 2 };
    let bytes = 1u64 << size;

    let val1 = state.get_reg(rt as u32);
    let val2 = state.get_reg(rt2 as u32);

    if mem_write(mem, addr, val1, size).is_err() {
        return StepResult::MemoryFault(addr);
    }
    if mem_write(mem, addr.wrapping_add(bytes), val2, size).is_err() {
        return StepResult::MemoryFault(addr.wrapping_add(bytes));
    }
    StepResult::Continue
}

pub fn exec_ldxr(state: &mut CpuState, mem: &dyn MemoryAccess,
                  _sf: bool, rt: u8, rn: u8, size: u8) -> StepResult {
    let addr = state.get_reg(rn as u32);
    match mem_read(mem, addr, size, false, size == 3) {
        Ok(val) => {
            state.set_reg(rt as u32, val);
            state.exclusive_addr = Some(addr);
            state.exclusive_value = val;
            StepResult::Continue
        }
        Err(a) => StepResult::MemoryFault(a),
    }
}

pub fn exec_stxr(state: &mut CpuState, mem: &mut dyn MemoryAccess,
                  _sf: bool, rt: u8, rn: u8, rs: u8, size: u8) -> StepResult {
    let addr = state.get_reg(rn as u32);
    let val = state.get_reg(rt as u32);

    // Check exclusive monitor
    if state.exclusive_addr == Some(addr) {
        match mem_write(mem, addr, val, size) {
            Ok(()) => {
                state.set_reg(rs as u32, 0); // success
                state.exclusive_addr = None;
            }
            Err(a) => return StepResult::MemoryFault(a),
        }
    } else {
        state.set_reg(rs as u32, 1); // failed
    }
    StepResult::Continue
}

// LDAXR = LDXR + acquire semantics (same behavior in single-threaded interpreter)
pub fn exec_ldaxr(state: &mut CpuState, mem: &dyn MemoryAccess,
                   sf: bool, rt: u8, rn: u8, size: u8) -> StepResult {
    exec_ldxr(state, mem, sf, rt, rn, size)
}

// STLXR = STXR + release semantics
pub fn exec_stlxr(state: &mut CpuState, mem: &mut dyn MemoryAccess,
                   sf: bool, rt: u8, rn: u8, rs: u8, size: u8) -> StepResult {
    exec_stxr(state, mem, sf, rt, rn, rs, size)
}

// LDAR = load-acquire (treat as regular load in single-threaded interpreter)
pub fn exec_ldar(state: &mut CpuState, mem: &dyn MemoryAccess,
                  _sf: bool, rt: u8, rn: u8, size: u8) -> StepResult {
    let addr = state.get_reg(rn as u32);
    match mem_read(mem, addr, size, false, size == 3) {
        Ok(val) => {
            state.set_reg(rt as u32, val);
            StepResult::Continue
        }
        Err(a) => StepResult::MemoryFault(a),
    }
}

// STLR = store-release
pub fn exec_stlr(state: &mut CpuState, mem: &mut dyn MemoryAccess,
                  _sf: bool, rt: u8, rn: u8, size: u8) -> StepResult {
    let addr = state.get_reg(rn as u32);
    let val = state.get_reg(rt as u32);
    match mem_write(mem, addr, val, size) {
        Ok(()) => StepResult::Continue,
        Err(a) => StepResult::MemoryFault(a),
    }
}

// CAS = compare and swap (single-threaded, no actual atomicity needed)
pub fn exec_cas(state: &mut CpuState, mem: &mut dyn MemoryAccess,
                size: u8, rs: u8, rt: u8, rn: u8) -> StepResult {
    let addr = state.get_reg(rn as u32);
    let mask = match size {
        0 => 0xFFu64,
        1 => 0xFFFFu64,
        2 => 0xFFFF_FFFFu64,
        _ => u64::MAX,
    };
    let expected = state.get_reg(rs as u32) & mask;
    let new_val = state.get_reg(rt as u32) & mask;

    let actual = match mem_read(mem, addr, size, false, size == 3) {
        Ok(v) => v & mask,
        Err(a) => return StepResult::MemoryFault(a),
    };

    if actual == expected {
        if let Err(a) = mem_write(mem, addr, new_val, size) {
            return StepResult::MemoryFault(a);
        }
    }
    // Rs always gets the original memory value
    state.set_reg(rs as u32, actual);
    StepResult::Continue
}

// SWP = atomic swap
pub fn exec_swp(state: &mut CpuState, mem: &mut dyn MemoryAccess,
                size: u8, rs: u8, rt: u8, rn: u8) -> StepResult {
    let addr = state.get_reg(rn as u32);
    let swap_val = state.get_reg(rs as u32);

    let original = match mem_read(mem, addr, size, false, size == 3) {
        Ok(v) => v,
        Err(a) => return StepResult::MemoryFault(a),
    };
    if let Err(a) = mem_write(mem, addr, swap_val, size) {
        return StepResult::MemoryFault(a);
    }
    state.set_reg(rt as u32, original);
    StepResult::Continue
}

// LDADD/LDCLR/LDEOR/LDSET = atomic arithmetic
pub fn exec_atomic_op(state: &mut CpuState, mem: &mut dyn MemoryAccess,
                      size: u8, rs: u8, rt: u8, rn: u8, op: u8) -> StepResult {
    let addr = state.get_reg(rn as u32);
    let operand = state.get_reg(rs as u32);

    let original = match mem_read(mem, addr, size, false, size == 3) {
        Ok(v) => v,
        Err(a) => return StepResult::MemoryFault(a),
    };
    let result = match op {
        0 => original.wrapping_add(operand), // LDADD
        1 => original & !operand,            // LDCLR (BIC)
        2 => original ^ operand,             // LDEOR
        3 => original | operand,             // LDSET (ORR)
        _ => unreachable!(),
    };
    if let Err(a) = mem_write(mem, addr, result, size) {
        return StepResult::MemoryFault(a);
    }
    state.set_reg(rt as u32, original); // Rt gets original value
    StepResult::Continue
}

// LDAXP = load-acquire exclusive pair
pub fn exec_ldaxp(state: &mut CpuState, mem: &dyn MemoryAccess,
                   sf: bool, rt: u8, rt2: u8, rn: u8) -> StepResult {
    let addr = state.get_reg(rn as u32);
    let size: u8 = if sf { 3 } else { 2 };
    let bytes = 1u64 << size;

    let val1 = match mem_read(mem, addr, size, false, sf) {
        Ok(v) => v,
        Err(a) => return StepResult::MemoryFault(a),
    };
    let val2 = match mem_read(mem, addr.wrapping_add(bytes), size, false, sf) {
        Ok(v) => v,
        Err(a) => return StepResult::MemoryFault(a),
    };

    state.set_reg(rt as u32, val1);
    state.set_reg(rt2 as u32, val2);
    state.exclusive_addr = Some(addr);
    state.exclusive_value = val1;
    StepResult::Continue
}

// STLXP = store-release exclusive pair
pub fn exec_stlxp(state: &mut CpuState, mem: &mut dyn MemoryAccess,
                   sf: bool, rt: u8, rt2: u8, rn: u8, rs: u8) -> StepResult {
    let addr = state.get_reg(rn as u32);
    let size: u8 = if sf { 3 } else { 2 };
    let bytes = 1u64 << size;

    if state.exclusive_addr == Some(addr) {
        let val1 = state.get_reg(rt as u32);
        let val2 = state.get_reg(rt2 as u32);
        if mem_write(mem, addr, val1, size).is_err() {
            return StepResult::MemoryFault(addr);
        }
        if mem_write(mem, addr.wrapping_add(bytes), val2, size).is_err() {
            return StepResult::MemoryFault(addr.wrapping_add(bytes));
        }
        state.set_reg(rs as u32, 0); // success
        state.exclusive_addr = None;
    } else {
        state.set_reg(rs as u32, 1); // failed
    }
    StepResult::Continue
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::memory::{MemoryAccess, MemoryFault};

    struct TestMem {
        data: Vec<u8>,
    }

    impl TestMem {
        fn new() -> Self {
            Self { data: vec![0u8; 4096] }
        }
    }

    impl MemoryAccess for TestMem {
        fn read_u8(&self, addr: u64) -> Result<u8, MemoryFault> {
            self.data.get(addr as usize).copied().ok_or(MemoryFault::Unmapped(addr))
        }
        fn read_u16(&self, addr: u64) -> Result<u16, MemoryFault> {
            let o = addr as usize;
            let s = self.data.get(o..o + 2).ok_or(MemoryFault::Unmapped(addr))?;
            Ok(u16::from_le_bytes(s.try_into().unwrap()))
        }
        fn read_u32(&self, addr: u64) -> Result<u32, MemoryFault> {
            let o = addr as usize;
            let s = self.data.get(o..o + 4).ok_or(MemoryFault::Unmapped(addr))?;
            Ok(u32::from_le_bytes(s.try_into().unwrap()))
        }
        fn read_u64(&self, addr: u64) -> Result<u64, MemoryFault> {
            let o = addr as usize;
            let s = self.data.get(o..o + 8).ok_or(MemoryFault::Unmapped(addr))?;
            Ok(u64::from_le_bytes(s.try_into().unwrap()))
        }
        fn read_u128(&self, addr: u64) -> Result<u128, MemoryFault> {
            let o = addr as usize;
            let s = self.data.get(o..o + 16).ok_or(MemoryFault::Unmapped(addr))?;
            Ok(u128::from_le_bytes(s.try_into().unwrap()))
        }
        fn write_u8(&mut self, addr: u64, val: u8) -> Result<(), MemoryFault> {
            *self.data.get_mut(addr as usize).ok_or(MemoryFault::Unmapped(addr))? = val;
            Ok(())
        }
        fn write_u16(&mut self, addr: u64, val: u16) -> Result<(), MemoryFault> {
            let o = addr as usize;
            let s = self.data.get_mut(o..o + 2).ok_or(MemoryFault::Unmapped(addr))?;
            s.copy_from_slice(&val.to_le_bytes());
            Ok(())
        }
        fn write_u32(&mut self, addr: u64, val: u32) -> Result<(), MemoryFault> {
            let o = addr as usize;
            let s = self.data.get_mut(o..o + 4).ok_or(MemoryFault::Unmapped(addr))?;
            s.copy_from_slice(&val.to_le_bytes());
            Ok(())
        }
        fn write_u64(&mut self, addr: u64, val: u64) -> Result<(), MemoryFault> {
            let o = addr as usize;
            let s = self.data.get_mut(o..o + 8).ok_or(MemoryFault::Unmapped(addr))?;
            s.copy_from_slice(&val.to_le_bytes());
            Ok(())
        }
        fn write_u128(&mut self, addr: u64, val: u128) -> Result<(), MemoryFault> {
            let o = addr as usize;
            let s = self.data.get_mut(o..o + 16).ok_or(MemoryFault::Unmapped(addr))?;
            s.copy_from_slice(&val.to_le_bytes());
            Ok(())
        }
    }

    #[test]
    fn test_cas_success() {
        let mut s = CpuState::new();
        let mut mem = TestMem::new();
        mem.write_u32(0x100, 42).unwrap();
        s.set_reg(2, 0x100);  // Rn = address
        s.set_reg(0, 42);     // Rs = expected (compare value)
        s.set_reg(1, 99);     // Rt = new value
        let r = exec_cas(&mut s, &mut mem, 2, 0, 1, 2); // size=2 (32-bit)
        assert!(matches!(r, StepResult::Continue));
        assert_eq!(mem.read_u32(0x100).unwrap(), 99);
        assert_eq!(s.get_reg(0), 42); // Rs = old value
    }

    #[test]
    fn test_cas_fail() {
        let mut s = CpuState::new();
        let mut mem = TestMem::new();
        mem.write_u32(0x100, 42).unwrap();
        s.set_reg(2, 0x100);
        s.set_reg(0, 10);     // Rs = wrong expected
        s.set_reg(1, 99);
        let r = exec_cas(&mut s, &mut mem, 2, 0, 1, 2);
        assert!(matches!(r, StepResult::Continue));
        assert_eq!(mem.read_u32(0x100).unwrap(), 42); // unchanged
        assert_eq!(s.get_reg(0), 42); // Rs = actual value
    }

    #[test]
    fn test_swp() {
        let mut s = CpuState::new();
        let mut mem = TestMem::new();
        mem.write_u32(0x100, 42).unwrap();
        s.set_reg(2, 0x100);
        s.set_reg(0, 99);     // Rs = swap value
        let r = exec_swp(&mut s, &mut mem, 2, 0, 1, 2);
        assert!(matches!(r, StepResult::Continue));
        assert_eq!(mem.read_u32(0x100).unwrap(), 99);
        assert_eq!(s.get_reg(1), 42); // Rt = old value
    }

    #[test]
    fn test_ldadd() {
        let mut s = CpuState::new();
        let mut mem = TestMem::new();
        mem.write_u32(0x100, 10).unwrap();
        s.set_reg(2, 0x100);
        s.set_reg(0, 5);
        let r = exec_atomic_op(&mut s, &mut mem, 2, 0, 1, 2, 0);
        assert!(matches!(r, StepResult::Continue));
        assert_eq!(mem.read_u32(0x100).unwrap(), 15);
        assert_eq!(s.get_reg(1), 10);
    }

    #[test]
    fn test_ldclr() {
        let mut s = CpuState::new();
        let mut mem = TestMem::new();
        mem.write_u32(0x100, 0xFF).unwrap();
        s.set_reg(2, 0x100);
        s.set_reg(0, 0x0F);
        let r = exec_atomic_op(&mut s, &mut mem, 2, 0, 1, 2, 1);
        assert!(matches!(r, StepResult::Continue));
        assert_eq!(mem.read_u32(0x100).unwrap(), 0xF0);
        assert_eq!(s.get_reg(1), 0xFF);
    }

    #[test]
    fn test_ldeor() {
        let mut s = CpuState::new();
        let mut mem = TestMem::new();
        mem.write_u32(0x100, 0xAA).unwrap();
        s.set_reg(2, 0x100);
        s.set_reg(0, 0xFF);
        let r = exec_atomic_op(&mut s, &mut mem, 2, 0, 1, 2, 2);
        assert!(matches!(r, StepResult::Continue));
        assert_eq!(mem.read_u32(0x100).unwrap(), 0x55);
        assert_eq!(s.get_reg(1), 0xAA);
    }

    #[test]
    fn test_ldset() {
        let mut s = CpuState::new();
        let mut mem = TestMem::new();
        mem.write_u32(0x100, 0xA0).unwrap();
        s.set_reg(2, 0x100);
        s.set_reg(0, 0x0F);
        let r = exec_atomic_op(&mut s, &mut mem, 2, 0, 1, 2, 3);
        assert!(matches!(r, StepResult::Continue));
        assert_eq!(mem.read_u32(0x100).unwrap(), 0xAF);
        assert_eq!(s.get_reg(1), 0xA0);
    }
}
