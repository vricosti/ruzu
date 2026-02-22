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
