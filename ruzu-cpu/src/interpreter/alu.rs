// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Arithmetic and logical instruction execution.

use crate::decoder::*;
use crate::state::CpuState;
use super::StepResult;

/// Apply a shift operation to a value.
#[inline]
fn apply_shift(value: u64, shift: ShiftType, amount: u8, datasize: u32) -> u64 {
    if amount == 0 {
        return value;
    }
    let amount = amount as u32;
    match shift {
        ShiftType::LSL => value << amount,
        ShiftType::LSR => value >> amount,
        ShiftType::ASR => {
            if datasize == 32 {
                ((value as i32) >> amount) as u32 as u64
            } else {
                ((value as i64) >> amount) as u64
            }
        }
        ShiftType::ROR => {
            if datasize == 32 {
                let v = value as u32;
                (v.rotate_right(amount)) as u64
            } else {
                value.rotate_right(amount)
            }
        }
    }
}

/// Apply an extend operation to a value.
#[inline]
fn apply_extend(value: u64, ext: ExtendType, shift: u8) -> u64 {
    let extended = match ext {
        ExtendType::UXTB => value & 0xFF,
        ExtendType::UXTH => value & 0xFFFF,
        ExtendType::UXTW => value & 0xFFFF_FFFF,
        ExtendType::UXTX => value,
        ExtendType::SXTB => (value as i8) as i64 as u64,
        ExtendType::SXTH => (value as i16) as i64 as u64,
        ExtendType::SXTW => (value as i32) as i64 as u64,
        ExtendType::SXTX => value,
    };
    extended << shift
}

/// Mask to 32-bit if not 64-bit mode.
#[inline]
fn mask(val: u64, sf: bool) -> u64 {
    if sf { val } else { val & 0xFFFF_FFFF }
}

pub fn exec_add_imm(state: &mut CpuState, sf: bool, rd: u8, rn: u8, imm12: u32,
                     shift: bool, set_flags: bool) -> StepResult {
    let imm = if shift { (imm12 as u64) << 12 } else { imm12 as u64 };
    let a = state.get_reg(rn as u32);

    if sf {
        let result = a.wrapping_add(imm);
        if set_flags {
            state.set_nzcv_from_add64(result, a, imm);
        }
        state.set_reg(rd as u32, result);
    } else {
        let a32 = a as u32;
        let imm32 = imm as u32;
        let result = a32.wrapping_add(imm32);
        if set_flags {
            state.set_nzcv_from_add32(result, a32, imm32);
        }
        state.set_reg(rd as u32, result as u64);
    }
    StepResult::Continue
}

pub fn exec_sub_imm(state: &mut CpuState, sf: bool, rd: u8, rn: u8, imm12: u32,
                     shift: bool, set_flags: bool) -> StepResult {
    let imm = if shift { (imm12 as u64) << 12 } else { imm12 as u64 };
    let a = state.get_reg(rn as u32);

    if sf {
        let result = a.wrapping_sub(imm);
        if set_flags {
            state.set_nzcv_from_sub64(result, a, imm);
        }
        // CMP is SUBS with rd=XZR (31) - still writes flags but discards result
        state.set_reg(rd as u32, result);
    } else {
        let a32 = a as u32;
        let imm32 = imm as u32;
        let result = a32.wrapping_sub(imm32);
        if set_flags {
            state.set_nzcv_from_sub32(result, a32, imm32);
        }
        state.set_reg(rd as u32, result as u64);
    }
    StepResult::Continue
}

pub fn exec_add_reg(state: &mut CpuState, sf: bool, rd: u8, rn: u8, rm: u8,
                     shift: ShiftType, amount: u8, set_flags: bool) -> StepResult {
    let datasize = if sf { 64 } else { 32 };
    let a = state.get_reg(rn as u32);
    let b = apply_shift(state.get_reg(rm as u32), shift, amount, datasize);

    if sf {
        let result = a.wrapping_add(b);
        if set_flags { state.set_nzcv_from_add64(result, a, b); }
        state.set_reg(rd as u32, result);
    } else {
        let (a32, b32) = (a as u32, b as u32);
        let result = a32.wrapping_add(b32);
        if set_flags { state.set_nzcv_from_add32(result, a32, b32); }
        state.set_reg(rd as u32, result as u64);
    }
    StepResult::Continue
}

pub fn exec_sub_reg(state: &mut CpuState, sf: bool, rd: u8, rn: u8, rm: u8,
                     shift: ShiftType, amount: u8, set_flags: bool) -> StepResult {
    let datasize = if sf { 64 } else { 32 };
    let a = state.get_reg(rn as u32);
    let b = apply_shift(state.get_reg(rm as u32), shift, amount, datasize);

    if sf {
        let result = a.wrapping_sub(b);
        if set_flags { state.set_nzcv_from_sub64(result, a, b); }
        state.set_reg(rd as u32, result);
    } else {
        let (a32, b32) = (a as u32, b as u32);
        let result = a32.wrapping_sub(b32);
        if set_flags { state.set_nzcv_from_sub32(result, a32, b32); }
        state.set_reg(rd as u32, result as u64);
    }
    StepResult::Continue
}

pub fn exec_add_ext_reg(state: &mut CpuState, sf: bool, rd: u8, rn: u8, rm: u8,
                         ext: ExtendType, amount: u8, set_flags: bool) -> StepResult {
    let a = state.get_reg(rn as u32);
    let b = apply_extend(state.get_reg(rm as u32), ext, amount);

    if sf {
        let result = a.wrapping_add(b);
        if set_flags { state.set_nzcv_from_add64(result, a, b); }
        state.set_reg(rd as u32, result);
    } else {
        let (a32, b32) = (a as u32, b as u32);
        let result = a32.wrapping_add(b32);
        if set_flags { state.set_nzcv_from_add32(result, a32, b32); }
        state.set_reg(rd as u32, result as u64);
    }
    StepResult::Continue
}

pub fn exec_sub_ext_reg(state: &mut CpuState, sf: bool, rd: u8, rn: u8, rm: u8,
                         ext: ExtendType, amount: u8, set_flags: bool) -> StepResult {
    let a = state.get_reg(rn as u32);
    let b = apply_extend(state.get_reg(rm as u32), ext, amount);

    if sf {
        let result = a.wrapping_sub(b);
        if set_flags { state.set_nzcv_from_sub64(result, a, b); }
        state.set_reg(rd as u32, result);
    } else {
        let (a32, b32) = (a as u32, b as u32);
        let result = a32.wrapping_sub(b32);
        if set_flags { state.set_nzcv_from_sub32(result, a32, b32); }
        state.set_reg(rd as u32, result as u64);
    }
    StepResult::Continue
}

pub fn exec_logical_imm(state: &mut CpuState, sf: bool, opc: u8, rd: u8, rn: u8,
                         imm: u64) -> StepResult {
    let a = state.get_reg(rn as u32);
    let result = match opc {
        0b00 => a & imm,         // AND
        0b01 => a | imm,         // ORR
        0b10 => a ^ imm,         // EOR
        0b11 => {                // ANDS
            let r = a & imm;
            let r = mask(r, sf);
            if sf {
                state.set_n(r & (1u64 << 63) != 0);
                state.set_z(r == 0);
            } else {
                state.set_n(r & (1u64 << 31) != 0);
                state.set_z(r as u32 == 0);
            }
            state.set_c(false);
            state.set_v(false);
            r
        }
        _ => unreachable!(),
    };
    let result = mask(result, sf);
    state.set_reg(rd as u32, result);
    StepResult::Continue
}

pub fn exec_logical_reg(state: &mut CpuState, sf: bool, opc: u8, rd: u8, rn: u8,
                         rm: u8, shift: ShiftType, amount: u8, invert: bool) -> StepResult {
    let datasize = if sf { 64 } else { 32 };
    let a = state.get_reg(rn as u32);
    let mut b = apply_shift(state.get_reg(rm as u32), shift, amount, datasize);
    if invert {
        b = !b;
    }
    b = mask(b, sf);

    let result = match opc {
        0b00 => a & b,    // AND / BIC
        0b01 => a | b,    // ORR / ORN
        0b10 => a ^ b,    // EOR / EON
        0b11 => {         // ANDS / BICS
            let r = mask(a & b, sf);
            if sf {
                state.set_n(r & (1u64 << 63) != 0);
                state.set_z(r == 0);
            } else {
                state.set_n(r & (1u64 << 31) != 0);
                state.set_z(r as u32 == 0);
            }
            state.set_c(false);
            state.set_v(false);
            r
        }
        _ => unreachable!(),
    };
    state.set_reg(rd as u32, mask(result, sf));
    StepResult::Continue
}

pub fn exec_movz(state: &mut CpuState, sf: bool, rd: u8, imm16: u16, hw: u8) -> StepResult {
    let val = (imm16 as u64) << (hw as u64 * 16);
    state.set_reg(rd as u32, mask(val, sf));
    StepResult::Continue
}

pub fn exec_movk(state: &mut CpuState, _sf: bool, rd: u8, imm16: u16, hw: u8) -> StepResult {
    let shift = hw as u64 * 16;
    let mask_bits = 0xFFFFu64 << shift;
    let old = state.get_reg(rd as u32);
    let val = (old & !mask_bits) | ((imm16 as u64) << shift);
    state.set_reg(rd as u32, val);
    StepResult::Continue
}

pub fn exec_movn(state: &mut CpuState, sf: bool, rd: u8, imm16: u16, hw: u8) -> StepResult {
    let val = !((imm16 as u64) << (hw as u64 * 16));
    state.set_reg(rd as u32, mask(val, sf));
    StepResult::Continue
}

pub fn exec_bitfield_move(state: &mut CpuState, sf: bool, opc: u8, rd: u8, rn: u8,
                           immr: u8, imms: u8) -> StepResult {
    let datasize = if sf { 64u32 } else { 32 };
    let src = state.get_reg(rn as u32);
    let dst = state.get_reg(rd as u32);
    let r = immr as u32;
    let s = imms as u32;

    let result = match opc {
        // SBFM (signed bitfield move): handles ASR, SXTB, SXTH, SXTW, SBFX, SBFIZ
        0b00 => {
            if s >= r {
                // Extract bitfield, sign extend from bit s
                let width = s - r + 1;
                let field = (src >> r) & ((1u64 << width) - 1);
                // Sign extend from 'width' bits
                let shift = 64 - width;
                let result = ((field as i64) << shift) >> shift;
                mask(result as u64, sf)
            } else {
                // Insert bitfield (SBFIZ-like)
                let width = s + 1;
                let field = src & ((1u64 << width) - 1);
                let pos = datasize - r;
                let shifted = field << pos;
                // Sign extend from (pos + width) bits
                let sign_bit = pos + width;
                let shift = 64 - sign_bit;
                let result = ((shifted as i64) << shift) >> shift;
                mask(result as u64, sf)
            }
        }
        // BFM (bitfield move): handles BFI, BFXIL
        0b01 => {
            if s >= r {
                // BFXIL: extract bitfield and insert at LSB
                let width = s - r + 1;
                let field = (src >> r) & ((1u64 << width) - 1);
                let mask_val = (1u64 << width) - 1;
                (dst & !mask_val) | field
            } else {
                // BFI: extract from LSB and insert at position
                let width = s + 1;
                let pos = datasize - r;
                let field = src & ((1u64 << width) - 1);
                let mask_val = ((1u64 << width) - 1) << pos;
                (dst & !mask_val) | (field << pos)
            }
        }
        // UBFM (unsigned bitfield move): handles LSL, LSR, UBFX, UBFIZ, UXTB, UXTH
        0b10 => {
            if s >= r {
                // UBFX / LSR
                let width = s - r + 1;
                let field = (src >> r) & ((1u64 << width) - 1);
                mask(field, sf)
            } else {
                // UBFIZ / LSL
                let width = s + 1;
                let pos = datasize - r;
                let field = src & ((1u64 << width) - 1);
                mask(field << pos, sf)
            }
        }
        _ => return StepResult::InvalidInstruction(0),
    };

    state.set_reg(rd as u32, result);
    StepResult::Continue
}

pub fn exec_madd(state: &mut CpuState, sf: bool, rd: u8, rn: u8, rm: u8, ra: u8) -> StepResult {
    let a = state.get_reg(rn as u32);
    let b = state.get_reg(rm as u32);
    let addend = state.get_reg(ra as u32);
    let result = addend.wrapping_add(a.wrapping_mul(b));
    state.set_reg(rd as u32, mask(result, sf));
    StepResult::Continue
}

pub fn exec_msub(state: &mut CpuState, sf: bool, rd: u8, rn: u8, rm: u8, ra: u8) -> StepResult {
    let a = state.get_reg(rn as u32);
    let b = state.get_reg(rm as u32);
    let addend = state.get_reg(ra as u32);
    let result = addend.wrapping_sub(a.wrapping_mul(b));
    state.set_reg(rd as u32, mask(result, sf));
    StepResult::Continue
}

pub fn exec_smaddl(state: &mut CpuState, rd: u8, rn: u8, rm: u8, ra: u8) -> StepResult {
    let a = state.get_reg(rn as u32) as i32 as i64;
    let b = state.get_reg(rm as u32) as i32 as i64;
    let addend = state.get_reg(ra as u32) as i64;
    let result = addend.wrapping_add(a.wrapping_mul(b));
    state.set_reg(rd as u32, result as u64);
    StepResult::Continue
}

pub fn exec_umaddl(state: &mut CpuState, rd: u8, rn: u8, rm: u8, ra: u8) -> StepResult {
    let a = state.get_reg(rn as u32) as u32 as u64;
    let b = state.get_reg(rm as u32) as u32 as u64;
    let addend = state.get_reg(ra as u32);
    let result = addend.wrapping_add(a.wrapping_mul(b));
    state.set_reg(rd as u32, result);
    StepResult::Continue
}

pub fn exec_smulh(state: &mut CpuState, rd: u8, rn: u8, rm: u8) -> StepResult {
    let a = state.get_reg(rn as u32) as i64 as i128;
    let b = state.get_reg(rm as u32) as i64 as i128;
    let result = (a.wrapping_mul(b) >> 64) as u64;
    state.set_reg(rd as u32, result);
    StepResult::Continue
}

pub fn exec_umulh(state: &mut CpuState, rd: u8, rn: u8, rm: u8) -> StepResult {
    let a = state.get_reg(rn as u32) as u128;
    let b = state.get_reg(rm as u32) as u128;
    let result = (a.wrapping_mul(b) >> 64) as u64;
    state.set_reg(rd as u32, result);
    StepResult::Continue
}

pub fn exec_sdiv(state: &mut CpuState, sf: bool, rd: u8, rn: u8, rm: u8) -> StepResult {
    if sf {
        let a = state.get_reg(rn as u32) as i64;
        let b = state.get_reg(rm as u32) as i64;
        let result = if b == 0 { 0 }
            else if a == i64::MIN && b == -1 { a } // overflow
            else { a / b };
        state.set_reg(rd as u32, result as u64);
    } else {
        let a = state.get_reg(rn as u32) as i32;
        let b = state.get_reg(rm as u32) as i32;
        let result = if b == 0 { 0 }
            else if a == i32::MIN && b == -1 { a }
            else { a / b };
        state.set_reg(rd as u32, result as u32 as u64);
    }
    StepResult::Continue
}

pub fn exec_udiv(state: &mut CpuState, sf: bool, rd: u8, rn: u8, rm: u8) -> StepResult {
    if sf {
        let a = state.get_reg(rn as u32);
        let b = state.get_reg(rm as u32);
        let result = if b == 0 { 0 } else { a / b };
        state.set_reg(rd as u32, result);
    } else {
        let a = state.get_reg(rn as u32) as u32;
        let b = state.get_reg(rm as u32) as u32;
        let result = if b == 0 { 0 } else { a / b };
        state.set_reg(rd as u32, result as u64);
    }
    StepResult::Continue
}

pub fn exec_csel(state: &mut CpuState, sf: bool, rd: u8, rn: u8, rm: u8, cond: u8,
                  op2: u8) -> StepResult {
    let cond_met = state.check_condition(cond);

    let result = if cond_met {
        state.get_reg(rn as u32)
    } else {
        let val = state.get_reg(rm as u32);
        match op2 {
            0b00 => val,                      // CSEL
            0b01 => val.wrapping_add(1),      // CSINC
            0b10 => !val,                     // CSINV
            0b11 => (!val).wrapping_add(1),   // CSNEG (negate)
            _ => val,
        }
    };
    state.set_reg(rd as u32, mask(result, sf));
    StepResult::Continue
}

pub fn exec_ccmp(state: &mut CpuState, sf: bool, rn: u8, rm_or_imm: u8, nzcv: u8,
                  cond: u8, is_imm: bool, is_neg: bool) -> StepResult {
    if state.check_condition(cond) {
        let a = state.get_reg(rn as u32);
        let b = if is_imm {
            rm_or_imm as u64
        } else {
            state.get_reg(rm_or_imm as u32)
        };

        if sf {
            if is_neg {
                let result = a.wrapping_add(b);
                state.set_nzcv_from_add64(result, a, b);
            } else {
                let result = a.wrapping_sub(b);
                state.set_nzcv_from_sub64(result, a, b);
            }
        } else {
            let (a32, b32) = (a as u32, b as u32);
            if is_neg {
                let result = a32.wrapping_add(b32);
                state.set_nzcv_from_add32(result, a32, b32);
            } else {
                let result = a32.wrapping_sub(b32);
                state.set_nzcv_from_sub32(result, a32, b32);
            }
        }
    } else {
        // Set flags to the immediate nzcv value
        state.nzcv = (nzcv as u32) << 28;
    }
    StepResult::Continue
}

pub fn exec_clz(state: &mut CpuState, sf: bool, rd: u8, rn: u8) -> StepResult {
    let val = state.get_reg(rn as u32);
    let result = if sf {
        val.leading_zeros() as u64
    } else {
        (val as u32).leading_zeros() as u64
    };
    state.set_reg(rd as u32, result);
    StepResult::Continue
}

pub fn exec_cls(state: &mut CpuState, sf: bool, rd: u8, rn: u8) -> StepResult {
    let val = state.get_reg(rn as u32);
    let result = if sf {
        let signed = val as i64;
        if signed >= 0 { signed.leading_zeros().saturating_sub(1) as u64 }
        else { (!signed as u64).leading_zeros().saturating_sub(1) as u64 }
    } else {
        let signed = val as i32;
        if signed >= 0 { signed.leading_zeros().saturating_sub(1) as u64 }
        else { (!signed as u32).leading_zeros().saturating_sub(1) as u64 }
    };
    state.set_reg(rd as u32, result);
    StepResult::Continue
}

pub fn exec_rev(state: &mut CpuState, sf: bool, rd: u8, rn: u8, opc: u8) -> StepResult {
    let val = state.get_reg(rn as u32);
    let result = match opc {
        1 => {
            // REV16: reverse bytes in each halfword
            if sf {
                ((val & 0xFF00FF00FF00FF00) >> 8) | ((val & 0x00FF00FF00FF00FF) << 8)
            } else {
                let v = val as u32;
                let r = ((v & 0xFF00FF00) >> 8) | ((v & 0x00FF00FF) << 8);
                r as u64
            }
        }
        2 => {
            if sf {
                // REV32: reverse bytes in each word (64-bit)
                let lo = (val as u32).swap_bytes() as u64;
                let hi = ((val >> 32) as u32).swap_bytes() as u64;
                lo | (hi << 32)
            } else {
                // REV: reverse bytes (32-bit)
                (val as u32).swap_bytes() as u64
            }
        }
        3 => {
            // REV: reverse all bytes (64-bit)
            val.swap_bytes()
        }
        _ => val,
    };
    state.set_reg(rd as u32, result);
    StepResult::Continue
}

pub fn exec_rbit(state: &mut CpuState, sf: bool, rd: u8, rn: u8) -> StepResult {
    let val = state.get_reg(rn as u32);
    let result = if sf {
        val.reverse_bits()
    } else {
        (val as u32).reverse_bits() as u64
    };
    state.set_reg(rd as u32, result);
    StepResult::Continue
}

pub fn exec_extr(state: &mut CpuState, sf: bool, rd: u8, rn: u8, rm: u8,
                  imms: u8) -> StepResult {
    let high = state.get_reg(rn as u32);
    let low = state.get_reg(rm as u32);
    let lsb = imms as u32;

    let result = if sf {
        if lsb == 0 {
            low
        } else {
            (high << (64 - lsb)) | (low >> lsb)
        }
    } else {
        let h = high as u32;
        let l = low as u32;
        if lsb == 0 {
            l as u64
        } else {
            ((h << (32 - lsb)) | (l >> lsb)) as u64
        }
    };
    state.set_reg(rd as u32, mask(result, sf));
    StepResult::Continue
}

pub fn exec_adr(state: &mut CpuState, rd: u8, imm: i64) -> StepResult {
    let result = (state.pc as i64).wrapping_add(imm) as u64;
    state.set_reg(rd as u32, result);
    StepResult::Continue
}

pub fn exec_adrp(state: &mut CpuState, rd: u8, imm: i64) -> StepResult {
    let base = state.pc & !0xFFF; // page-align PC
    let result = (base as i64).wrapping_add(imm) as u64;
    state.set_reg(rd as u32, result);
    StepResult::Continue
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_state() -> CpuState {
        CpuState::new()
    }

    #[test]
    fn test_add_imm_basic() {
        let mut s = make_state();
        s.set_reg(0, 10);
        exec_add_imm(&mut s, true, 1, 0, 5, false, false);
        assert_eq!(s.get_reg(1), 15);
    }

    #[test]
    fn test_sub_imm_flags() {
        let mut s = make_state();
        s.set_reg(0, 5);
        exec_sub_imm(&mut s, true, 1, 0, 5, false, true);
        assert_eq!(s.get_reg(1), 0);
        assert!(s.z());
        assert!(!s.n());
    }

    #[test]
    fn test_movz_movk() {
        let mut s = make_state();
        exec_movz(&mut s, true, 0, 0x1234, 0);
        assert_eq!(s.get_reg(0), 0x1234);
        exec_movk(&mut s, true, 0, 0x5678, 1);
        assert_eq!(s.get_reg(0), 0x5678_1234);
    }

    #[test]
    fn test_movn() {
        let mut s = make_state();
        exec_movn(&mut s, true, 0, 0, 0);
        assert_eq!(s.get_reg(0), u64::MAX); // NOT(0) = all ones
    }

    #[test]
    fn test_sdiv_by_zero() {
        let mut s = make_state();
        s.set_reg(0, 42);
        s.set_reg(1, 0);
        exec_sdiv(&mut s, true, 2, 0, 1);
        assert_eq!(s.get_reg(2), 0);
    }

    #[test]
    fn test_clz() {
        let mut s = make_state();
        s.set_reg(0, 1);
        exec_clz(&mut s, true, 1, 0);
        assert_eq!(s.get_reg(1), 63);

        s.set_reg(0, 1);
        exec_clz(&mut s, false, 1, 0);
        assert_eq!(s.get_reg(1), 31);
    }

    #[test]
    fn test_rev_32() {
        let mut s = make_state();
        s.set_reg(0, 0x01020304);
        exec_rev(&mut s, false, 1, 0, 2);
        assert_eq!(s.get_reg(1), 0x04030201);
    }
}
