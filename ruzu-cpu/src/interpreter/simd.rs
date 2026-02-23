// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Minimal SIMD/FP instruction execution.
//!
//! Supports enough floating-point operations to avoid crashing on FP code.
//! Only single (f32) and double (f64) precision are implemented.

use crate::decoder::*;
use crate::memory::MemoryAccess;
use crate::state::CpuState;
use super::StepResult;

/// Get the byte size for an ftype value: 0=single(4), 1=double(8), 3=half(2).
#[allow(dead_code)]
fn fp_size(ftype: u8) -> u8 {
    match ftype {
        0 => 2, // 32-bit = size code 2 (4 bytes)
        1 => 3, // 64-bit = size code 3 (8 bytes)
        3 => 1, // 16-bit = size code 1 (2 bytes)
        _ => 2,
    }
}

/// Read the FP value from a V register as f64.
fn read_fp(state: &CpuState, reg: u8, ftype: u8) -> f64 {
    let bits = state.get_vreg_u64(reg as u32, 0);
    match ftype {
        0 => f32::from_bits(bits as u32) as f64,
        1 => f64::from_bits(bits),
        _ => 0.0,
    }
}

/// Write an f64 value to a V register, clearing upper bits.
fn write_fp(state: &mut CpuState, reg: u8, ftype: u8, val: f64) {
    match ftype {
        0 => {
            let bits = (val as f32).to_bits() as u64;
            state.set_vreg_u128(reg as u32, bits as u128);
        }
        1 => {
            let bits = val.to_bits();
            state.set_vreg_u128(reg as u32, bits as u128);
        }
        _ => {
            state.set_vreg_u128(reg as u32, 0);
        }
    }
}

pub fn exec_fmov_reg(state: &mut CpuState, rd: u8, rn: u8, ftype: u8) -> StepResult {
    let val = state.get_vreg_u128(rn as u32);
    // FMOV preserves full register for same-type move, clear upper for scalar
    match ftype {
        0 => {
            let bits = val as u64 & 0xFFFF_FFFF;
            state.set_vreg_u128(rd as u32, bits as u128);
        }
        1 => {
            let bits = val as u64;
            state.set_vreg_u128(rd as u32, bits as u128);
        }
        _ => state.set_vreg_u128(rd as u32, val),
    }
    StepResult::Continue
}

pub fn exec_fmov_to_gp(state: &mut CpuState, rd: u8, rn: u8, sf: bool,
                         _ftype: u8) -> StepResult {
    // FMOV Xd, Dn or FMOV Wd, Sn — raw bit transfer
    let bits = state.get_vreg_u64(rn as u32, 0);
    let val = if sf {
        bits
    } else {
        bits & 0xFFFF_FFFF
    };
    state.set_reg(rd as u32, val);
    StepResult::Continue
}

pub fn exec_fmov_from_gp(state: &mut CpuState, rd: u8, rn: u8, sf: bool,
                           _ftype: u8) -> StepResult {
    // FMOV Dd, Xn or FMOV Sn, Wn — raw bit transfer
    let val = state.get_reg(rn as u32);
    if sf {
        state.set_vreg_u128(rd as u32, val as u128);
    } else {
        state.set_vreg_u128(rd as u32, (val & 0xFFFF_FFFF) as u128);
    }
    StepResult::Continue
}

pub fn exec_farith(state: &mut CpuState, op: FpOp, rd: u8, rn: u8, rm: u8,
                    ftype: u8) -> StepResult {
    let a = read_fp(state, rn, ftype);
    let b = read_fp(state, rm, ftype);

    let result = match op {
        FpOp::Add => a + b,
        FpOp::Sub => a - b,
        FpOp::Mul => a * b,
        FpOp::Div => a / b,
        FpOp::Max => a.max(b),
        FpOp::Min => a.min(b),
        FpOp::MaxNum => if a.is_nan() { b } else if b.is_nan() { a } else { a.max(b) },
        FpOp::MinNum => if a.is_nan() { b } else if b.is_nan() { a } else { a.min(b) },
    };

    write_fp(state, rd, ftype, result);
    StepResult::Continue
}

pub fn exec_fcmp(state: &mut CpuState, rn: u8, rm: u8, ftype: u8,
                  with_zero: bool) -> StepResult {
    let a = read_fp(state, rn, ftype);
    let b = if with_zero { 0.0 } else { read_fp(state, rm, ftype) };

    // Set NZCV according to FP comparison rules
    if a.is_nan() || b.is_nan() {
        // Unordered: C=1, V=1
        state.set_n(false);
        state.set_z(false);
        state.set_c(true);
        state.set_v(true);
    } else if a == b {
        // Equal: Z=1, C=1
        state.set_n(false);
        state.set_z(true);
        state.set_c(true);
        state.set_v(false);
    } else if a < b {
        // Less: N=1
        state.set_n(true);
        state.set_z(false);
        state.set_c(false);
        state.set_v(false);
    } else {
        // Greater: C=1
        state.set_n(false);
        state.set_z(false);
        state.set_c(true);
        state.set_v(false);
    }
    StepResult::Continue
}

pub fn exec_fcsel(state: &mut CpuState, rd: u8, rn: u8, rm: u8, ftype: u8,
                   cond: u8) -> StepResult {
    let src = if state.check_condition(cond) { rn } else { rm };
    let val = state.get_vreg_u128(src as u32);
    match ftype {
        0 => state.set_vreg_u128(rd as u32, (val as u64 & 0xFFFF_FFFF) as u128),
        1 => state.set_vreg_u128(rd as u32, (val as u64) as u128),
        _ => state.set_vreg_u128(rd as u32, val),
    }
    StepResult::Continue
}

pub fn exec_fcvt(state: &mut CpuState, rd: u8, rn: u8, src_type: u8,
                  dst_type: u8) -> StepResult {
    let val = read_fp(state, rn, src_type);
    write_fp(state, rd, dst_type, val);
    StepResult::Continue
}

pub fn exec_scvtf_int(state: &mut CpuState, sf: bool, rd: u8, rn: u8,
                       ftype: u8) -> StepResult {
    let int_val = if sf {
        state.get_reg(rn as u32) as i64
    } else {
        state.get_reg(rn as u32) as i32 as i64
    };
    write_fp(state, rd, ftype, int_val as f64);
    StepResult::Continue
}

pub fn exec_ucvtf_int(state: &mut CpuState, sf: bool, rd: u8, rn: u8,
                       ftype: u8) -> StepResult {
    let int_val = if sf {
        state.get_reg(rn as u32)
    } else {
        state.get_reg(rn as u32) & 0xFFFF_FFFF
    };
    write_fp(state, rd, ftype, int_val as f64);
    StepResult::Continue
}

pub fn exec_fcvtzs_int(state: &mut CpuState, sf: bool, rd: u8, rn: u8,
                        ftype: u8) -> StepResult {
    let fp_val = read_fp(state, rn, ftype);
    let result = if sf {
        // Truncate toward zero, clamp to i64 range
        let clamped = fp_val.clamp(i64::MIN as f64, i64::MAX as f64);
        clamped as i64 as u64
    } else {
        let clamped = fp_val.clamp(i32::MIN as f64, i32::MAX as f64);
        clamped as i32 as u32 as u64
    };
    state.set_reg(rd as u32, result);
    StepResult::Continue
}

pub fn exec_fcvtzu_int(state: &mut CpuState, sf: bool, rd: u8, rn: u8,
                        ftype: u8) -> StepResult {
    let fp_val = read_fp(state, rn, ftype);
    let result = if sf {
        let clamped = fp_val.clamp(0.0, u64::MAX as f64);
        clamped as u64
    } else {
        let clamped = fp_val.clamp(0.0, u32::MAX as f64);
        clamped as u32 as u64
    };
    state.set_reg(rd as u32, result);
    StepResult::Continue
}

pub fn exec_fneg(state: &mut CpuState, rd: u8, rn: u8, ftype: u8) -> StepResult {
    let val = read_fp(state, rn, ftype);
    write_fp(state, rd, ftype, -val);
    StepResult::Continue
}

pub fn exec_fabs(state: &mut CpuState, rd: u8, rn: u8, ftype: u8) -> StepResult {
    let val = read_fp(state, rn, ftype);
    write_fp(state, rd, ftype, val.abs());
    StepResult::Continue
}

pub fn exec_fsqrt(state: &mut CpuState, rd: u8, rn: u8, ftype: u8) -> StepResult {
    let val = read_fp(state, rn, ftype);
    write_fp(state, rd, ftype, val.sqrt());
    StepResult::Continue
}

pub fn exec_fma(state: &mut CpuState, ftype: u8, rd: u8, rn: u8, rm: u8,
                ra: u8, op: u8) -> StepResult {
    let a = read_fp(state, rn, ftype);
    let b = read_fp(state, rm, ftype);
    let c = read_fp(state, ra, ftype);
    let result = match op {
        0 => a.mul_add(b, c),      // FMADD:  Rd = Ra + Rn*Rm
        1 => (-a).mul_add(b, c),   // FMSUB:  Rd = Ra - Rn*Rm
        2 => (-a).mul_add(b, -c),  // FNMADD: Rd = -(Ra + Rn*Rm)
        3 => a.mul_add(b, -c),     // FNMSUB: Rd = -(Ra - Rn*Rm)
        _ => unreachable!(),
    };
    write_fp(state, rd, ftype, result);
    StepResult::Continue
}

pub fn exec_frint(state: &mut CpuState, ftype: u8, rd: u8, rn: u8,
                  mode: u8) -> StepResult {
    let val = read_fp(state, rn, ftype);
    let result = match mode {
        0 => val.round_ties_even(),  // FRINTN — nearest, ties to even
        1 => val.ceil(),             // FRINTP — toward +inf
        2 => val.floor(),            // FRINTM — toward -inf
        3 => val.trunc(),            // FRINTZ — toward zero
        4 => val.round(),            // FRINTA — nearest, ties away from zero
        6 | 7 => val.round_ties_even(), // FRINTX/FRINTI — current rounding mode
        _ => val,
    };
    write_fp(state, rd, ftype, result);
    StepResult::Continue
}

pub fn exec_ldr_simd(state: &mut CpuState, mem: &dyn MemoryAccess,
                      rt: u8, rn: u8, imm: i64, size: u8,
                      mode: AddrMode, pc: u64) -> StepResult {
    // rn == 0xFF is sentinel for PC-relative literal load
    let base = if rn == 0xFF {
        pc
    } else {
        state.get_reg(rn as u32)
    };

    let addr = if rn != 0xFF {
        match mode {
            AddrMode::Offset => (base as i64).wrapping_add(imm) as u64,
            AddrMode::PreIndex => {
                let a = (base as i64).wrapping_add(imm) as u64;
                state.set_reg(rn as u32, a);
                a
            }
            AddrMode::PostIndex => {
                let wb = (base as i64).wrapping_add(imm) as u64;
                state.set_reg(rn as u32, wb);
                base
            }
        }
    } else {
        (base as i64).wrapping_add(imm) as u64
    };

    let val = match size {
        2 => mem.read_u32(addr).map(|v| v as u128).map_err(|_| addr),
        3 => mem.read_u64(addr).map(|v| v as u128).map_err(|_| addr),
        4 => mem.read_u128(addr).map_err(|_| addr),
        _ => mem.read_u32(addr).map(|v| v as u128).map_err(|_| addr),
    };

    match val {
        Ok(v) => {
            state.set_vreg_u128(rt as u32, v);
            StepResult::Continue
        }
        Err(a) => StepResult::MemoryFault(a),
    }
}

pub fn exec_str_simd(state: &mut CpuState, mem: &mut dyn MemoryAccess,
                      rt: u8, rn: u8, imm: i64, size: u8,
                      mode: AddrMode) -> StepResult {
    let base = state.get_reg(rn as u32);
    let addr = match mode {
        AddrMode::Offset => (base as i64).wrapping_add(imm) as u64,
        AddrMode::PreIndex => {
            let a = (base as i64).wrapping_add(imm) as u64;
            state.set_reg(rn as u32, a);
            a
        }
        AddrMode::PostIndex => {
            let wb = (base as i64).wrapping_add(imm) as u64;
            state.set_reg(rn as u32, wb);
            base
        }
    };

    let val = state.get_vreg_u128(rt as u32);
    let result = match size {
        2 => mem.write_u32(addr, val as u32),
        3 => mem.write_u64(addr, val as u64),
        4 => mem.write_u128(addr, val),
        _ => mem.write_u32(addr, val as u32),
    };

    match result {
        Ok(()) => StepResult::Continue,
        Err(_) => StepResult::MemoryFault(addr),
    }
}

pub fn exec_ldp_simd(state: &mut CpuState, mem: &dyn MemoryAccess,
                      rt: u8, rt2: u8, rn: u8, imm: i64, size: u8,
                      mode: AddrMode) -> StepResult {
    let base = state.get_reg(rn as u32);
    let addr = match mode {
        AddrMode::Offset => (base as i64).wrapping_add(imm) as u64,
        AddrMode::PreIndex => {
            let a = (base as i64).wrapping_add(imm) as u64;
            state.set_reg(rn as u32, a);
            a
        }
        AddrMode::PostIndex => {
            let wb = (base as i64).wrapping_add(imm) as u64;
            state.set_reg(rn as u32, wb);
            base
        }
    };

    let elem_bytes = 1u64 << size;

    let read_elem = |a: u64| -> Result<u128, u64> {
        match size {
            2 => mem.read_u32(a).map(|v| v as u128).map_err(|_| a),
            3 => mem.read_u64(a).map(|v| v as u128).map_err(|_| a),
            4 => mem.read_u128(a).map_err(|_| a),
            _ => Err(a),
        }
    };

    let v1 = match read_elem(addr) {
        Ok(v) => v,
        Err(a) => return StepResult::MemoryFault(a),
    };
    let v2 = match read_elem(addr.wrapping_add(elem_bytes)) {
        Ok(v) => v,
        Err(a) => return StepResult::MemoryFault(a),
    };

    state.set_vreg_u128(rt as u32, v1);
    state.set_vreg_u128(rt2 as u32, v2);
    StepResult::Continue
}

pub fn exec_stp_simd(state: &mut CpuState, mem: &mut dyn MemoryAccess,
                      rt: u8, rt2: u8, rn: u8, imm: i64, size: u8,
                      mode: AddrMode) -> StepResult {
    let base = state.get_reg(rn as u32);
    let addr = match mode {
        AddrMode::Offset => (base as i64).wrapping_add(imm) as u64,
        AddrMode::PreIndex => {
            let a = (base as i64).wrapping_add(imm) as u64;
            state.set_reg(rn as u32, a);
            a
        }
        AddrMode::PostIndex => {
            let wb = (base as i64).wrapping_add(imm) as u64;
            state.set_reg(rn as u32, wb);
            base
        }
    };

    let elem_bytes = 1u64 << size;
    let v1 = state.get_vreg_u128(rt as u32);
    let v2 = state.get_vreg_u128(rt2 as u32);

    let write_elem = |m: &mut dyn MemoryAccess, a: u64, v: u128| -> Result<(), u64> {
        match size {
            2 => m.write_u32(a, v as u32).map_err(|_| a),
            3 => m.write_u64(a, v as u64).map_err(|_| a),
            4 => m.write_u128(a, v).map_err(|_| a),
            _ => Err(a),
        }
    };

    if let Err(a) = write_elem(mem, addr, v1) {
        return StepResult::MemoryFault(a);
    }
    if let Err(a) = write_elem(mem, addr.wrapping_add(elem_bytes), v2) {
        return StepResult::MemoryFault(a);
    }
    StepResult::Continue
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fadd_f64() {
        let mut s = CpuState::new();
        // Write 1.0 to V0
        s.set_vreg_u128(0, 1.0f64.to_bits() as u128);
        // Write 2.0 to V1
        s.set_vreg_u128(1, 2.0f64.to_bits() as u128);
        exec_farith(&mut s, FpOp::Add, 2, 0, 1, 1);
        let result = f64::from_bits(s.get_vreg_u64(2, 0));
        assert!((result - 3.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_fcmp_equal() {
        let mut s = CpuState::new();
        s.set_vreg_u128(0, 1.0f64.to_bits() as u128);
        s.set_vreg_u128(1, 1.0f64.to_bits() as u128);
        exec_fcmp(&mut s, 0, 1, 1, false);
        assert!(s.z()); // equal
        assert!(s.c());
        assert!(!s.n());
        assert!(!s.v_flag());
    }

    #[test]
    fn test_fcmp_less() {
        let mut s = CpuState::new();
        s.set_vreg_u128(0, 1.0f64.to_bits() as u128);
        s.set_vreg_u128(1, 2.0f64.to_bits() as u128);
        exec_fcmp(&mut s, 0, 1, 1, false);
        assert!(s.n()); // less than
        assert!(!s.z());
    }

    #[test]
    fn test_scvtf() {
        let mut s = CpuState::new();
        s.set_reg(0, (-42i64) as u64);
        exec_scvtf_int(&mut s, true, 1, 0, 1); // double
        let result = f64::from_bits(s.get_vreg_u64(1, 0));
        assert!((result - (-42.0)).abs() < f64::EPSILON);
    }

    #[test]
    fn test_fcvtzs() {
        let mut s = CpuState::new();
        s.set_vreg_u128(0, (-3.7f64).to_bits() as u128);
        exec_fcvtzs_int(&mut s, true, 1, 0, 1); // double to i64
        assert_eq!(s.get_reg(1) as i64, -3);
    }

    #[test]
    fn test_fneg() {
        let mut s = CpuState::new();
        s.set_vreg_u128(0, 5.0f64.to_bits() as u128);
        exec_fneg(&mut s, 1, 0, 1);
        let result = f64::from_bits(s.get_vreg_u64(1, 0));
        assert!((result - (-5.0)).abs() < f64::EPSILON);
    }

    #[test]
    fn test_fmadd() {
        // FMADD: Rd = Ra + Rn*Rm
        let mut s = CpuState::new();
        s.set_vreg_u128(1, 2.0f64.to_bits() as u128); // Rn = 2.0
        s.set_vreg_u128(2, 3.0f64.to_bits() as u128); // Rm = 3.0
        s.set_vreg_u128(3, 4.0f64.to_bits() as u128); // Ra = 4.0
        exec_fma(&mut s, 1, 0, 1, 2, 3, 0); // ftype=1(double), rd=0, rn=1, rm=2, ra=3, op=0
        let result = f64::from_bits(s.get_vreg_u64(0, 0));
        assert!((result - 10.0).abs() < f64::EPSILON); // 4.0 + 2.0*3.0 = 10.0
    }

    #[test]
    fn test_fmsub() {
        // FMSUB: Rd = Ra - Rn*Rm
        let mut s = CpuState::new();
        s.set_vreg_u128(1, 2.0f64.to_bits() as u128);
        s.set_vreg_u128(2, 3.0f64.to_bits() as u128);
        s.set_vreg_u128(3, 4.0f64.to_bits() as u128);
        exec_fma(&mut s, 1, 0, 1, 2, 3, 1); // op=1 (FMSUB)
        let result = f64::from_bits(s.get_vreg_u64(0, 0));
        assert!((result - (-2.0)).abs() < f64::EPSILON); // 4.0 - 6.0 = -2.0
    }

    #[test]
    fn test_fnmadd() {
        // FNMADD: Rd = -(Ra + Rn*Rm)
        let mut s = CpuState::new();
        s.set_vreg_u128(1, 2.0f64.to_bits() as u128);
        s.set_vreg_u128(2, 3.0f64.to_bits() as u128);
        s.set_vreg_u128(3, 4.0f64.to_bits() as u128);
        exec_fma(&mut s, 1, 0, 1, 2, 3, 2); // op=2 (FNMADD)
        let result = f64::from_bits(s.get_vreg_u64(0, 0));
        assert!((result - (-10.0)).abs() < f64::EPSILON); // -(4.0 + 6.0) = -10.0
    }

    #[test]
    fn test_fnmsub() {
        // FNMSUB: Rd = -(Ra - Rn*Rm) = Rn*Rm - Ra
        let mut s = CpuState::new();
        s.set_vreg_u128(1, 2.0f64.to_bits() as u128);
        s.set_vreg_u128(2, 3.0f64.to_bits() as u128);
        s.set_vreg_u128(3, 4.0f64.to_bits() as u128);
        exec_fma(&mut s, 1, 0, 1, 2, 3, 3); // op=3 (FNMSUB)
        let result = f64::from_bits(s.get_vreg_u64(0, 0));
        assert!((result - 2.0).abs() < f64::EPSILON); // 6.0 - 4.0 = 2.0
    }

    #[test]
    fn test_frintn() {
        // FRINTN: round to nearest, ties to even
        let mut s = CpuState::new();
        s.set_vreg_u128(0, 2.5f64.to_bits() as u128);
        exec_frint(&mut s, 1, 1, 0, 0); // ftype=1, rd=1, rn=0, mode=0
        let result = f64::from_bits(s.get_vreg_u64(1, 0));
        assert!((result - 2.0).abs() < f64::EPSILON); // ties to even → 2.0
    }

    #[test]
    fn test_frintp() {
        // FRINTP: round toward +inf (ceil)
        let mut s = CpuState::new();
        s.set_vreg_u128(0, 2.3f64.to_bits() as u128);
        exec_frint(&mut s, 1, 1, 0, 1); // mode=1
        let result = f64::from_bits(s.get_vreg_u64(1, 0));
        assert!((result - 3.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_frintm() {
        // FRINTM: round toward -inf (floor)
        let mut s = CpuState::new();
        s.set_vreg_u128(0, 2.7f64.to_bits() as u128);
        exec_frint(&mut s, 1, 1, 0, 2); // mode=2
        let result = f64::from_bits(s.get_vreg_u64(1, 0));
        assert!((result - 2.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_frintz() {
        // FRINTZ: round toward zero (truncate)
        let mut s = CpuState::new();
        s.set_vreg_u128(0, (-2.7f64).to_bits() as u128);
        exec_frint(&mut s, 1, 1, 0, 3); // mode=3
        let result = f64::from_bits(s.get_vreg_u64(1, 0));
        assert!((result - (-2.0)).abs() < f64::EPSILON);
    }

    #[test]
    fn test_frinta() {
        // FRINTA: round to nearest, ties away from zero
        let mut s = CpuState::new();
        s.set_vreg_u128(0, 2.5f64.to_bits() as u128);
        exec_frint(&mut s, 1, 1, 0, 4); // mode=4
        let result = f64::from_bits(s.get_vreg_u64(1, 0));
        assert!((result - 3.0).abs() < f64::EPSILON); // ties away → 3.0
    }
}
