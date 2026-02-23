// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! NEON/Advanced SIMD instruction execution.
//!
//! Implements ~80 NEON vector instructions covering integer arithmetic, FP,
//! bitwise, shifts, permutes, comparisons, copy/move, and load/store multi.

use crate::memory::MemoryAccess;
use crate::state::CpuState;
use super::StepResult;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Get element size in bits from the 2-bit `size` field: 0=8, 1=16, 2=32, 3=64.
#[inline]
fn esize_from_size(size: u8) -> u32 {
    8 << size
}

/// Sign-extend a lane value from `esize` bits to i64.
#[inline]
fn sign_extend_lane(val: u64, esize: u32) -> i64 {
    if esize >= 64 {
        val as i64
    } else {
        let shift = 64 - esize;
        ((val as i64) << shift) >> shift
    }
}

/// Execute an operation on each lane of a vector register.
/// Clears the destination register first, then writes each lane result.
#[inline]
fn for_each_lane<F>(state: &mut CpuState, q: bool, esize: u32, rd: u8, rn: u8, rm: u8, f: F)
where
    F: Fn(u64, u64) -> u64,
{
    let lanes = if q { 128 / esize } else { 64 / esize };
    // Read all source lanes first (in case rd aliases rn or rm)
    let mut results = [0u64; 16];
    for i in 0..lanes {
        let a = state.get_vreg_lane(rn as u32, i, esize);
        let b = state.get_vreg_lane(rm as u32, i, esize);
        results[i as usize] = f(a, b);
    }
    state.set_vreg_u128(rd as u32, 0);
    for i in 0..lanes {
        state.set_vreg_lane(rd as u32, i, esize, results[i as usize]);
    }
}

/// Execute an unary operation on each lane.
#[inline]
fn for_each_lane_unary<F>(state: &mut CpuState, q: bool, esize: u32, rd: u8, rn: u8, f: F)
where
    F: Fn(u64) -> u64,
{
    let lanes = if q { 128 / esize } else { 64 / esize };
    let mut results = [0u64; 16];
    for i in 0..lanes {
        let a = state.get_vreg_lane(rn as u32, i, esize);
        results[i as usize] = f(a);
    }
    state.set_vreg_u128(rd as u32, 0);
    for i in 0..lanes {
        state.set_vreg_lane(rd as u32, i, esize, results[i as usize]);
    }
}

/// Mask a value to esize bits.
#[inline]
fn mask(val: u64, esize: u32) -> u64 {
    if esize >= 64 { val } else { val & ((1u64 << esize) - 1) }
}

/// All-ones mask for a given element size.
#[inline]
fn all_ones(esize: u32) -> u64 {
    if esize >= 64 { u64::MAX } else { (1u64 << esize) - 1 }
}

/// Decode imm5 to (element_size_bits, lane_index).
fn decode_imm5_lane(imm5: u8) -> (u32, u32) {
    if imm5 & 1 != 0 {
        (8, (imm5 >> 1) as u32)
    } else if imm5 & 2 != 0 {
        (16, (imm5 >> 2) as u32)
    } else if imm5 & 4 != 0 {
        (32, (imm5 >> 3) as u32)
    } else if imm5 & 8 != 0 {
        (64, (imm5 >> 4) as u32)
    } else {
        (8, 0) // fallback
    }
}

/// Replicate a value of `esize` bits across 128 bits.
fn replicate_to_u128(val: u64, esize: u32) -> u128 {
    let val = if esize < 64 { val & ((1u64 << esize) - 1) } else { val };
    let mut result = val as u128;
    let mut sz = esize;
    while sz < 128 {
        result |= result << sz;
        sz *= 2;
    }
    result
}

// ---------------------------------------------------------------------------
// Three same (vector arithmetic)
// ---------------------------------------------------------------------------

pub fn exec_simd_three_same(
    state: &mut CpuState, q: bool, u: bool, size: u8,
    opcode: u8, rd: u8, rn: u8, rm: u8,
) -> StepResult {
    let esize = esize_from_size(size);

    // Bitwise ops (opcode=0b00011) use size to select operation
    if opcode == 0b00011 {
        let a = state.get_vreg_u128(rn as u32);
        let b = state.get_vreg_u128(rm as u32);
        let q_mask = if q { u128::MAX } else { (1u128 << 64) - 1 };
        let result = if !u {
            match size {
                0 => a & b,             // AND
                1 => a & !b,            // BIC
                2 => a | b,             // ORR
                _ => a | !b,            // ORN
            }
        } else {
            match size {
                0 => a ^ b,                                                     // EOR
                1 => (a & state.get_vreg_u128(rd as u32)) | (!a & b),          // BSL
                2 => (a & b) | (!a & state.get_vreg_u128(rd as u32)),          // BIT
                _ => (!a & b) | (a & state.get_vreg_u128(rd as u32)),          // BIF
            }
        };
        state.set_vreg_u128(rd as u32, result & q_mask);
        return StepResult::Continue;
    }

    // FP three same: opcodes >= 0b11000
    if opcode >= 0b11000 {
        return exec_simd_three_same_fp(state, q, u, size, opcode, rd, rn, rm);
    }

    match (u, opcode) {
        // ADD vector
        (false, 0b10000) => {
            for_each_lane(state, q, esize, rd, rn, rm, |a, b| mask(a.wrapping_add(b), esize));
        }
        // SUB vector
        (true, 0b10000) => {
            for_each_lane(state, q, esize, rd, rn, rm, |a, b| mask(a.wrapping_sub(b), esize));
        }
        // MUL vector
        (false, 0b10011) => {
            for_each_lane(state, q, esize, rd, rn, rm, |a, b| mask(a.wrapping_mul(b), esize));
        }
        // CMEQ vector
        (true, 0b10001) => {
            for_each_lane(state, q, esize, rd, rn, rm, |a, b| {
                if a == b { all_ones(esize) } else { 0 }
            });
        }
        // CMTST (AND then check nonzero)
        (false, 0b10001) => {
            for_each_lane(state, q, esize, rd, rn, rm, |a, b| {
                if (a & b) != 0 { all_ones(esize) } else { 0 }
            });
        }
        // CMGT (signed)
        (false, 0b00110) => {
            for_each_lane(state, q, esize, rd, rn, rm, |a, b| {
                if sign_extend_lane(a, esize) > sign_extend_lane(b, esize) { all_ones(esize) } else { 0 }
            });
        }
        // CMGE (signed)
        (false, 0b00111) => {
            for_each_lane(state, q, esize, rd, rn, rm, |a, b| {
                if sign_extend_lane(a, esize) >= sign_extend_lane(b, esize) { all_ones(esize) } else { 0 }
            });
        }
        // CMHI (unsigned >)
        (true, 0b00110) => {
            for_each_lane(state, q, esize, rd, rn, rm, |a, b| {
                if a > b { all_ones(esize) } else { 0 }
            });
        }
        // CMHS (unsigned >=)
        (true, 0b00111) => {
            for_each_lane(state, q, esize, rd, rn, rm, |a, b| {
                if a >= b { all_ones(esize) } else { 0 }
            });
        }
        // SMAX
        (false, 0b01100) => {
            for_each_lane(state, q, esize, rd, rn, rm, |a, b| {
                let sa = sign_extend_lane(a, esize);
                let sb = sign_extend_lane(b, esize);
                mask(if sa > sb { a } else { b }, esize)
            });
        }
        // SMIN
        (false, 0b01101) => {
            for_each_lane(state, q, esize, rd, rn, rm, |a, b| {
                let sa = sign_extend_lane(a, esize);
                let sb = sign_extend_lane(b, esize);
                mask(if sa < sb { a } else { b }, esize)
            });
        }
        // UMAX
        (true, 0b01100) => {
            for_each_lane(state, q, esize, rd, rn, rm, |a, b| if a > b { a } else { b });
        }
        // UMIN
        (true, 0b01101) => {
            for_each_lane(state, q, esize, rd, rn, rm, |a, b| if a < b { a } else { b });
        }
        // ADDP (pairwise add)
        (false, 0b10111) => {
            exec_addp(state, q, esize, rd, rn, rm);
        }
        // UMAXP
        (true, 0b10100) => {
            exec_pairwise(state, q, esize, rd, rn, rm, |a, b| if a > b { a } else { b });
        }
        // UMINP
        (true, 0b10101) => {
            exec_pairwise(state, q, esize, rd, rn, rm, |a, b| if a < b { a } else { b });
        }
        // SMAXP
        (false, 0b10100) => {
            exec_pairwise(state, q, esize, rd, rn, rm, |a, b| {
                // unsigned values, but compare signed
                let sa = sign_extend_lane(a, esize);
                let sb = sign_extend_lane(b, esize);
                if sa > sb { a } else { b }
            });
        }
        // SMINP
        (false, 0b10101) => {
            exec_pairwise(state, q, esize, rd, rn, rm, |a, b| {
                let sa = sign_extend_lane(a, esize);
                let sb = sign_extend_lane(b, esize);
                if sa < sb { a } else { b }
            });
        }
        // MLA: Rd += Rn * Rm
        (false, 0b10010) => {
            let lanes = if q { 128 / esize } else { 64 / esize };
            let mut results = [0u64; 16];
            for i in 0..lanes {
                let a = state.get_vreg_lane(rn as u32, i, esize);
                let b = state.get_vreg_lane(rm as u32, i, esize);
                let d = state.get_vreg_lane(rd as u32, i, esize);
                results[i as usize] = mask(d.wrapping_add(a.wrapping_mul(b)), esize);
            }
            // Only clear the relevant part
            if !q {
                state.v[rd as usize][1] = 0;
            }
            for i in 0..lanes {
                state.set_vreg_lane(rd as u32, i, esize, results[i as usize]);
            }
        }
        // MLS: Rd -= Rn * Rm
        (true, 0b10010) => {
            let lanes = if q { 128 / esize } else { 64 / esize };
            let mut results = [0u64; 16];
            for i in 0..lanes {
                let a = state.get_vreg_lane(rn as u32, i, esize);
                let b = state.get_vreg_lane(rm as u32, i, esize);
                let d = state.get_vreg_lane(rd as u32, i, esize);
                results[i as usize] = mask(d.wrapping_sub(a.wrapping_mul(b)), esize);
            }
            if !q {
                state.v[rd as usize][1] = 0;
            }
            for i in 0..lanes {
                state.set_vreg_lane(rd as u32, i, esize, results[i as usize]);
            }
        }
        // SQADD (signed saturating add)
        (false, 0b00001) => {
            for_each_lane(state, q, esize, rd, rn, rm, |a, b| {
                let sa = sign_extend_lane(a, esize);
                let sb = sign_extend_lane(b, esize);
                let result = sa.saturating_add(sb);
                mask(result as u64, esize)
            });
        }
        // UQADD (unsigned saturating add)
        (true, 0b00001) => {
            for_each_lane(state, q, esize, rd, rn, rm, |a, b| {
                let sum = a.wrapping_add(b);
                let max = all_ones(esize);
                if sum > max || sum < a { max } else { sum }
            });
        }
        // SQSUB (signed saturating sub)
        (false, 0b00101) => {
            for_each_lane(state, q, esize, rd, rn, rm, |a, b| {
                let sa = sign_extend_lane(a, esize);
                let sb = sign_extend_lane(b, esize);
                let result = sa.saturating_sub(sb);
                mask(result as u64, esize)
            });
        }
        // UQSUB (unsigned saturating sub)
        (true, 0b00101) => {
            for_each_lane(state, q, esize, rd, rn, rm, |a, b| {
                if a >= b { a - b } else { 0 }
            });
        }
        // SSHL (signed shift left by register)
        (false, 0b01000) => {
            for_each_lane(state, q, esize, rd, rn, rm, |a, b| {
                let shift = sign_extend_lane(b, esize) as i8;
                if shift >= 0 {
                    mask(a << (shift as u32).min(esize - 1), esize)
                } else {
                    let neg_shift = ((-shift) as u32).min(esize);
                    mask(sign_extend_lane(a, esize) as u64 >> neg_shift, esize)
                }
            });
        }
        // USHL (unsigned shift left by register)
        (true, 0b01000) => {
            for_each_lane(state, q, esize, rd, rn, rm, |a, b| {
                let shift = sign_extend_lane(b, esize) as i8;
                if shift >= 0 {
                    mask(a << (shift as u32).min(esize - 1), esize)
                } else {
                    let neg_shift = ((-shift) as u32).min(esize);
                    a >> neg_shift
                }
            });
        }
        // SABD (signed absolute difference)
        (false, 0b01110) => {
            for_each_lane(state, q, esize, rd, rn, rm, |a, b| {
                let sa = sign_extend_lane(a, esize);
                let sb = sign_extend_lane(b, esize);
                mask((sa - sb).unsigned_abs(), esize)
            });
        }
        // Default: log and continue
        _ => {
            log::warn!("Unimplemented SimdThreeSame: u={}, opcode={:#07b}, size={}", u, opcode, size);
        }
    }
    StepResult::Continue
}

/// Pairwise add.
fn exec_addp(state: &mut CpuState, q: bool, esize: u32, rd: u8, rn: u8, rm: u8) {
    exec_pairwise(state, q, esize, rd, rn, rm, |a, b| mask(a.wrapping_add(b), esize));
}

/// Generic pairwise operation.
fn exec_pairwise<F>(state: &mut CpuState, q: bool, esize: u32, rd: u8, rn: u8, rm: u8, f: F)
where
    F: Fn(u64, u64) -> u64,
{
    let lanes = if q { 128 / esize } else { 64 / esize };
    let pairs = lanes / 2;
    let mut results = [0u64; 16];
    // First half: pairs from Vn
    for i in 0..pairs {
        let a = state.get_vreg_lane(rn as u32, i * 2, esize);
        let b = state.get_vreg_lane(rn as u32, i * 2 + 1, esize);
        results[i as usize] = f(a, b);
    }
    // Second half: pairs from Vm
    for i in 0..pairs {
        let a = state.get_vreg_lane(rm as u32, i * 2, esize);
        let b = state.get_vreg_lane(rm as u32, i * 2 + 1, esize);
        results[(pairs + i) as usize] = f(a, b);
    }
    state.set_vreg_u128(rd as u32, 0);
    for i in 0..lanes {
        state.set_vreg_lane(rd as u32, i, esize, results[i as usize]);
    }
}

// ---------------------------------------------------------------------------
// FP three same
// ---------------------------------------------------------------------------

fn exec_simd_three_same_fp(
    state: &mut CpuState, q: bool, u: bool, size: u8,
    opcode: u8, rd: u8, rn: u8, rm: u8,
) -> StepResult {
    let is_double = size & 1 != 0;
    let esize = if is_double { 64u32 } else { 32u32 };
    let lanes = if q { 128 / esize } else { 64 / esize };

    // For FMLA/FMLS we need to read rd first, so collect all results
    let mut results = [0u64; 4];

    for i in 0..lanes {
        let a_bits = state.get_vreg_lane(rn as u32, i, esize);
        let b_bits = state.get_vreg_lane(rm as u32, i, esize);
        let d_bits = state.get_vreg_lane(rd as u32, i, esize);

        results[i as usize] = if is_double {
            let a = f64::from_bits(a_bits);
            let b = f64::from_bits(b_bits);
            let d = f64::from_bits(d_bits);
            let r = match (u, opcode) {
                (false, 0b11010) => a + b,                  // FADD
                (true, 0b11010) => a - b,                   // FSUB
                (false, 0b11011) => a * b,                  // FMULX
                (true, 0b11011) => a * b,                   // FMUL
                (true, 0b11111) => a / b,                   // FDIV
                (false, 0b11110) => a.max(b),               // FMAX
                (true, 0b11110) => a.min(b),                // FMIN
                (false, 0b11001) => a.mul_add(b, d),        // FMLA: Rd += Rn*Rm
                (false, 0b11101) => (-a).mul_add(b, d),     // FMLS: Rd -= Rn*Rm
                (false, 0b11100) => {                       // FCMEQ
                    (if a == b { u64::MAX } else { 0 }) as f64
                }
                (true, 0b11100) => {                        // FCMGE
                    (if a >= b { u64::MAX } else { 0 }) as f64
                }
                (true, 0b11001) => {                        // FCMGT
                    (if a > b { u64::MAX } else { 0 }) as f64
                }
                (false, 0b10111) => {                       // FADDP
                    // This shouldn't normally be here; FADDP is pairwise
                    a + b
                }
                _ => {
                    log::warn!("Unimpl FP three same: u={}, opcode={:#07b}", u, opcode);
                    0.0
                }
            };
            // For compare ops, result is already bits
            match (u, opcode) {
                (false, 0b11100) | (true, 0b11100) | (true, 0b11001) => {
                    if a == b && opcode == 0b11100 && !u { u64::MAX }
                    else if a >= b && opcode == 0b11100 && u { u64::MAX }
                    else if a > b && opcode == 0b11001 && u { u64::MAX }
                    else { 0 }
                }
                _ => r.to_bits(),
            }
        } else {
            let a = f32::from_bits(a_bits as u32);
            let b = f32::from_bits(b_bits as u32);
            let d = f32::from_bits(d_bits as u32);
            let r = match (u, opcode) {
                (false, 0b11010) => a + b,
                (true, 0b11010) => a - b,
                (false, 0b11011) | (true, 0b11011) => a * b,
                (true, 0b11111) => a / b,
                (false, 0b11110) => a.max(b),
                (true, 0b11110) => a.min(b),
                (false, 0b11001) => a.mul_add(b, d),
                (false, 0b11101) => (-a).mul_add(b, d),
                (false, 0b11100) => {
                    return_val_u32(if a == b { u32::MAX } else { 0 })
                }
                (true, 0b11100) => {
                    return_val_u32(if a >= b { u32::MAX } else { 0 })
                }
                (true, 0b11001) => {
                    return_val_u32(if a > b { u32::MAX } else { 0 })
                }
                _ => {
                    log::warn!("Unimpl FP three same f32: u={}, opcode={:#07b}", u, opcode);
                    0.0
                }
            };
            match (u, opcode) {
                (false, 0b11100) => { if a == b { u32::MAX as u64 } else { 0 } }
                (true, 0b11100) => { if a >= b { u32::MAX as u64 } else { 0 } }
                (true, 0b11001) => { if a > b { u32::MAX as u64 } else { 0 } }
                _ => r.to_bits() as u64,
            }
        };
    }

    state.set_vreg_u128(rd as u32, 0);
    for i in 0..lanes {
        state.set_vreg_lane(rd as u32, i, esize, results[i as usize]);
    }
    StepResult::Continue
}

/// Dummy helper for f32 compare results.
#[inline]
fn return_val_u32(v: u32) -> f32 {
    f32::from_bits(v)
}

// ---------------------------------------------------------------------------
// Two-register misc (unary)
// ---------------------------------------------------------------------------

pub fn exec_simd_two_reg(
    state: &mut CpuState, q: bool, u: bool, size: u8,
    opcode: u8, rd: u8, rn: u8,
) -> StepResult {
    let esize = esize_from_size(size);

    match (u, opcode) {
        // REV64 (U=0, opcode=0b00000)
        (false, 0b00000) => {
            let lanes_64 = if q { 2u32 } else { 1u32 };
            state.set_vreg_u128(rd as u32, 0);
            for dw in 0..lanes_64 {
                let elems = 64 / esize;
                for i in 0..elems {
                    let val = state.get_vreg_lane(rn as u32, dw * elems + i, esize);
                    state.set_vreg_lane(rd as u32, dw * elems + (elems - 1 - i), esize, val);
                }
            }
        }
        // REV32 (U=1, opcode=0b00000)
        (true, 0b00000) => {
            let lanes_32 = if q { 4u32 } else { 2u32 };
            state.set_vreg_u128(rd as u32, 0);
            for dw in 0..lanes_32 {
                let elems = 32 / esize;
                for i in 0..elems {
                    let val = state.get_vreg_lane(rn as u32, dw * elems + i, esize);
                    state.set_vreg_lane(rd as u32, dw * elems + (elems - 1 - i), esize, val);
                }
            }
        }
        // REV16 (U=0, opcode=0b00001, size=0)
        (false, 0b00001) if size == 0 => {
            // Reverse bytes within 16-bit halfwords
            for_each_lane_unary(state, q, 16, rd, rn, |v| {
                ((v & 0xFF) << 8) | ((v >> 8) & 0xFF)
            });
        }
        // NOT (U=1, opcode=0b00101, size=0b00)
        (true, 0b00101) if size == 0 => {
            let v = state.get_vreg_u128(rn as u32);
            let q_mask = if q { u128::MAX } else { (1u128 << 64) - 1 };
            state.set_vreg_u128(rd as u32, !v & q_mask);
        }
        // RBIT (U=1, opcode=0b00101, size=0b01)
        (true, 0b00101) if size == 1 => {
            for_each_lane_unary(state, q, 8, rd, rn, |v| {
                (v as u8).reverse_bits() as u64
            });
        }
        // CNT (U=0, opcode=0b00101, size=0b00)
        (false, 0b00101) if size == 0 => {
            for_each_lane_unary(state, q, 8, rd, rn, |v| v.count_ones() as u64);
        }
        // ABS (U=0, opcode=0b01011)
        (false, 0b01011) => {
            for_each_lane_unary(state, q, esize, rd, rn, |v| {
                let sv = sign_extend_lane(v, esize);
                mask(sv.unsigned_abs(), esize)
            });
        }
        // NEG (U=1, opcode=0b01011)
        (true, 0b01011) => {
            for_each_lane_unary(state, q, esize, rd, rn, |v| {
                mask(0u64.wrapping_sub(v), esize)
            });
        }
        // XTN (U=0, opcode=0b10010) — extract narrow
        (false, 0b10010) => {
            // Narrows elements from 2*esize in Vn down to esize in lower/upper half of Vd
            let src_esize = esize * 2;
            let src_lanes = if q { 128 / src_esize } else { 64 / src_esize };
            // XTN: q=0 writes lower half; XTN2: q=1 writes upper half
            if !q {
                state.set_vreg_u128(rd as u32, 0);
            }
            let dst_offset = if q { 64 / esize } else { 0 };
            for i in 0..src_lanes {
                let val = state.get_vreg_lane(rn as u32, i, src_esize);
                state.set_vreg_lane(rd as u32, dst_offset + i, esize, mask(val, esize));
            }
        }
        // CMGT zero (U=0, opcode=0b01000)
        (false, 0b01000) => {
            for_each_lane_unary(state, q, esize, rd, rn, |v| {
                if sign_extend_lane(v, esize) > 0 { all_ones(esize) } else { 0 }
            });
        }
        // CMEQ zero (U=0, opcode=0b01001)
        (false, 0b01001) => {
            for_each_lane_unary(state, q, esize, rd, rn, |v| {
                if v == 0 { all_ones(esize) } else { 0 }
            });
        }
        // CMLT zero (U=0, opcode=0b01010)
        (false, 0b01010) => {
            for_each_lane_unary(state, q, esize, rd, rn, |v| {
                if sign_extend_lane(v, esize) < 0 { all_ones(esize) } else { 0 }
            });
        }
        // CMGE zero (U=1, opcode=0b01000)
        (true, 0b01000) => {
            for_each_lane_unary(state, q, esize, rd, rn, |v| {
                if sign_extend_lane(v, esize) >= 0 { all_ones(esize) } else { 0 }
            });
        }
        // CMLE zero (U=1, opcode=0b01001)
        (true, 0b01001) => {
            for_each_lane_unary(state, q, esize, rd, rn, |v| {
                if sign_extend_lane(v, esize) <= 0 { all_ones(esize) } else { 0 }
            });
        }
        // FNEG vector (U=1, opcode=0b01111)
        (true, 0b01111) => {
            let is_double = size & 1 != 0;
            let fp_esize = if is_double { 64u32 } else { 32u32 };
            for_each_lane_unary(state, q, fp_esize, rd, rn, |v| {
                if is_double {
                    (-f64::from_bits(v)).to_bits()
                } else {
                    (-f32::from_bits(v as u32)).to_bits() as u64
                }
            });
        }
        // FABS vector (U=0, opcode=0b01111)
        (false, 0b01111) => {
            let is_double = size & 1 != 0;
            let fp_esize = if is_double { 64u32 } else { 32u32 };
            for_each_lane_unary(state, q, fp_esize, rd, rn, |v| {
                if is_double {
                    f64::from_bits(v).abs().to_bits()
                } else {
                    f32::from_bits(v as u32).abs().to_bits() as u64
                }
            });
        }
        // FSQRT vector (U=1, opcode=0b11111)
        (true, 0b11111) => {
            let is_double = size & 1 != 0;
            let fp_esize = if is_double { 64u32 } else { 32u32 };
            for_each_lane_unary(state, q, fp_esize, rd, rn, |v| {
                if is_double {
                    f64::from_bits(v).sqrt().to_bits()
                } else {
                    f32::from_bits(v as u32).sqrt().to_bits() as u64
                }
            });
        }
        // FCVTZS vector (U=0, opcode=0b11011)
        (false, 0b11011) => {
            let is_double = size & 1 != 0;
            let fp_esize = if is_double { 64u32 } else { 32u32 };
            for_each_lane_unary(state, q, fp_esize, rd, rn, |v| {
                if is_double {
                    let f = f64::from_bits(v);
                    let clamped = f.clamp(i64::MIN as f64, i64::MAX as f64);
                    clamped as i64 as u64
                } else {
                    let f = f32::from_bits(v as u32);
                    let clamped = f.clamp(i32::MIN as f32, i32::MAX as f32);
                    clamped as i32 as u32 as u64
                }
            });
        }
        // FCVTZU vector (U=1, opcode=0b11011)
        (true, 0b11011) => {
            let is_double = size & 1 != 0;
            let fp_esize = if is_double { 64u32 } else { 32u32 };
            for_each_lane_unary(state, q, fp_esize, rd, rn, |v| {
                if is_double {
                    let f = f64::from_bits(v);
                    let clamped = f.clamp(0.0, u64::MAX as f64);
                    clamped as u64
                } else {
                    let f = f32::from_bits(v as u32);
                    let clamped = f.clamp(0.0, u32::MAX as f32);
                    clamped as u32 as u64
                }
            });
        }
        // SCVTF vector (U=0, opcode=0b11101)
        (false, 0b11101) => {
            let is_double = size & 1 != 0;
            let fp_esize = if is_double { 64u32 } else { 32u32 };
            for_each_lane_unary(state, q, fp_esize, rd, rn, |v| {
                if is_double {
                    (v as i64 as f64).to_bits()
                } else {
                    (v as i32 as f32).to_bits() as u64
                }
            });
        }
        // UCVTF vector (U=1, opcode=0b11101)
        (true, 0b11101) => {
            let is_double = size & 1 != 0;
            let fp_esize = if is_double { 64u32 } else { 32u32 };
            for_each_lane_unary(state, q, fp_esize, rd, rn, |v| {
                if is_double {
                    (v as f64).to_bits()
                } else {
                    (v as u32 as f32).to_bits() as u64
                }
            });
        }
        _ => {
            log::warn!("Unimpl SimdTwoReg u={} opcode={:#07b} size={}", u, opcode, size);
        }
    }
    StepResult::Continue
}

// ---------------------------------------------------------------------------
// Copy (DUP, INS, UMOV, SMOV)
// ---------------------------------------------------------------------------

pub fn exec_simd_copy(
    state: &mut CpuState, q: bool, op: u8, imm5: u8,
    imm4: u8, rd: u8, rn: u8,
) -> StepResult {
    match (op, imm4) {
        // DUP (element): op=0, imm4=0000
        (0, 0b0000) => {
            let (esize, index) = decode_imm5_lane(imm5);
            let val = state.get_vreg_lane(rn as u32, index, esize);
            let lanes = if q { 128 / esize } else { 64 / esize };
            state.set_vreg_u128(rd as u32, 0);
            for i in 0..lanes {
                state.set_vreg_lane(rd as u32, i, esize, val);
            }
        }
        // DUP (general): op=0, imm4=0001
        (0, 0b0001) => {
            let val = state.get_reg(rn as u32);
            let (esize, _) = decode_imm5_lane(imm5);
            let lanes = if q { 128 / esize } else { 64 / esize };
            state.set_vreg_u128(rd as u32, 0);
            for i in 0..lanes {
                state.set_vreg_lane(rd as u32, i, esize, mask(val, esize));
            }
        }
        // UMOV: op=0, imm4=0111 (64-bit) or 0011 (8/16/32-bit)
        (0, 0b0111) | (0, 0b0011) => {
            let (esize, index) = decode_imm5_lane(imm5);
            let val = state.get_vreg_lane(rn as u32, index, esize);
            state.set_reg(rd as u32, val);
        }
        // SMOV: op=0, imm4=0101 (32-bit result) or 0101 with q=1 (64-bit result)
        (0, 0b0101) => {
            let (esize, index) = decode_imm5_lane(imm5);
            let val = state.get_vreg_lane(rn as u32, index, esize);
            let extended = sign_extend_lane(val, esize) as u64;
            state.set_reg(rd as u32, extended);
        }
        // INS (element): op=1
        (1, _) => {
            let (esize, dst_index) = decode_imm5_lane(imm5);
            // imm4 encodes source lane: src_index = imm4 >> lowest_set_bit_pos(imm5)
            let shift = imm5.trailing_zeros();
            let src_index = (imm4 as u32) >> shift;
            let val = state.get_vreg_lane(rn as u32, src_index, esize);
            state.set_vreg_lane(rd as u32, dst_index, esize, val);
        }
        _ => {
            // INS (general register): op=0, imm4=0011 with u=1 path
            // This may also be reached for INS from GP register
            // 0_Q_1_01110000_imm5_0_0011_1_Rn_Rd → op would be 1 actually
            log::warn!("Unimpl SimdCopy op={} imm4={:#06b}", op, imm4);
        }
    }
    StepResult::Continue
}

// ---------------------------------------------------------------------------
// Modified immediate (MOVI, MVNI)
// ---------------------------------------------------------------------------

pub fn exec_simd_mod_imm(
    state: &mut CpuState, q: bool, op: u8, cmode: u8,
    rd: u8, imm8: u8,
) -> StepResult {
    let imm = expand_simd_imm(op, cmode, imm8);
    let q_mask = if q { u128::MAX } else { (1u128 << 64) - 1 };
    state.set_vreg_u128(rd as u32, imm & q_mask);
    StepResult::Continue
}

/// Expand a SIMD modified immediate to 128 bits based on cmode and op.
fn expand_simd_imm(op: u8, cmode: u8, imm8: u8) -> u128 {
    let imm = imm8 as u64;
    let cmode_hi = cmode >> 1;

    match cmode_hi {
        // 32-bit shifted immediate
        0b000 => replicate_to_u128(imm, 32),
        0b001 => replicate_to_u128(imm << 8, 32),
        0b010 => replicate_to_u128(imm << 16, 32),
        0b011 => replicate_to_u128(imm << 24, 32),
        // 16-bit shifted immediate
        0b100 => replicate_to_u128(imm, 16),
        0b101 => replicate_to_u128(imm << 8, 16),
        // 32-bit shifting ones
        0b110 => {
            if cmode & 1 == 0 {
                replicate_to_u128((imm << 8) | 0xFF, 32)
            } else {
                replicate_to_u128((imm << 16) | 0xFFFF, 32)
            }
        }
        // byte or FP immediate
        0b111 => {
            if op == 0 && cmode & 1 == 0 {
                // 8-bit replicate
                replicate_to_u128(imm, 8)
            } else if op == 0 && cmode & 1 == 1 {
                // MOVI Dd, #imm — 64-bit scalar
                // Each bit of imm8 expands to 8 bits
                let mut val = 0u64;
                for i in 0..8 {
                    if (imm >> i) & 1 != 0 {
                        val |= 0xFFu64 << (i * 8);
                    }
                }
                val as u128
            } else if op == 1 && cmode & 1 == 0 {
                // MVNI 32-bit
                replicate_to_u128(!imm & 0xFFFF_FFFF, 32)
            } else {
                // FMOV vector immediate (op=1, cmode=1111)
                // Expand to f32 immediate replicated
                let sign = (imm >> 7) & 1;
                let exp = ((!((imm >> 6) & 1) & 1) << 7) | (if (imm >> 6) & 1 != 0 { 0b0011111 } else { 0b1000000 }) << 0;
                let frac = (imm & 0x3F) << 19;
                let f32_bits = (sign << 31) | ((exp as u64) << 23) | frac;
                replicate_to_u128(f32_bits, 32)
            }
        }
        _ => 0,
    }
}

// ---------------------------------------------------------------------------
// Shift by immediate (SHL, SSHR, USHR, SSHLL, etc.)
// ---------------------------------------------------------------------------

pub fn exec_simd_shift_imm(
    state: &mut CpuState, q: bool, u: bool, immh: u8,
    immb: u8, opcode: u8, rd: u8, rn: u8,
) -> StepResult {
    let immhb = ((immh as u32) << 3) | (immb as u32);
    let (esize, _) = decode_shift_immhb(immh);

    match (u, opcode) {
        // SSHR (U=0, opcode=0b00000)
        (false, 0b00000) => {
            let shift = (esize * 2) - immhb;
            for_each_lane_unary(state, q, esize, rd, rn, |v| {
                let sv = sign_extend_lane(v, esize);
                mask((sv >> shift.min(63)) as u64, esize)
            });
        }
        // USHR (U=1, opcode=0b00000)
        (true, 0b00000) => {
            let shift = (esize * 2) - immhb;
            for_each_lane_unary(state, q, esize, rd, rn, |v| {
                if shift >= esize { 0 } else { v >> shift }
            });
        }
        // SSRA (U=0, opcode=0b00010) — shift right and accumulate
        (false, 0b00010) => {
            let shift = (esize * 2) - immhb;
            let lanes = if q { 128 / esize } else { 64 / esize };
            let mut results = [0u64; 16];
            for i in 0..lanes {
                let src = sign_extend_lane(state.get_vreg_lane(rn as u32, i, esize), esize);
                let dst = state.get_vreg_lane(rd as u32, i, esize);
                results[i as usize] = mask(dst.wrapping_add((src >> shift.min(63)) as u64), esize);
            }
            if !q { state.v[rd as usize][1] = 0; }
            for i in 0..lanes {
                state.set_vreg_lane(rd as u32, i, esize, results[i as usize]);
            }
        }
        // USRA (U=1, opcode=0b00010) — unsigned shift right and accumulate
        (true, 0b00010) => {
            let shift = (esize * 2) - immhb;
            let lanes = if q { 128 / esize } else { 64 / esize };
            let mut results = [0u64; 16];
            for i in 0..lanes {
                let src = state.get_vreg_lane(rn as u32, i, esize);
                let dst = state.get_vreg_lane(rd as u32, i, esize);
                let shifted = if shift >= esize { 0 } else { src >> shift };
                results[i as usize] = mask(dst.wrapping_add(shifted), esize);
            }
            if !q { state.v[rd as usize][1] = 0; }
            for i in 0..lanes {
                state.set_vreg_lane(rd as u32, i, esize, results[i as usize]);
            }
        }
        // SHL (U=0, opcode=0b01010)
        (false, 0b01010) => {
            let shift = immhb - esize;
            for_each_lane_unary(state, q, esize, rd, rn, |v| {
                mask(v << shift.min(esize - 1), esize)
            });
        }
        // SLI (U=1, opcode=0b01010) — shift left and insert
        (true, 0b01010) => {
            let shift = immhb - esize;
            let lanes = if q { 128 / esize } else { 64 / esize };
            let mut results = [0u64; 16];
            let insert_mask = if shift == 0 {
                all_ones(esize)
            } else {
                all_ones(esize) << shift & all_ones(esize)
            };
            for i in 0..lanes {
                let src = state.get_vreg_lane(rn as u32, i, esize);
                let dst = state.get_vreg_lane(rd as u32, i, esize);
                results[i as usize] = (dst & !insert_mask) | (mask(src << shift, esize) & insert_mask);
            }
            if !q { state.v[rd as usize][1] = 0; }
            for i in 0..lanes {
                state.set_vreg_lane(rd as u32, i, esize, results[i as usize]);
            }
        }
        // SSHLL/SSHLL2 (U=0, opcode=0b10100) — signed shift left long
        (false, 0b10100) => {
            let shift = immhb - esize;
            let dst_esize = esize * 2;
            let src_lanes = 64 / esize;  // Always reads from 64-bit half
            let src_offset = if q { src_lanes } else { 0 };
            let mut results = [0u64; 8];
            for i in 0..src_lanes {
                let val = sign_extend_lane(
                    state.get_vreg_lane(rn as u32, src_offset + i, esize),
                    esize,
                );
                results[i as usize] = mask((val as u64) << shift, dst_esize);
            }
            state.set_vreg_u128(rd as u32, 0);
            for i in 0..src_lanes {
                state.set_vreg_lane(rd as u32, i, dst_esize, results[i as usize]);
            }
        }
        // USHLL/USHLL2 (U=1, opcode=0b10100) — unsigned shift left long
        (true, 0b10100) => {
            let shift = immhb - esize;
            let dst_esize = esize * 2;
            let src_lanes = 64 / esize;
            let src_offset = if q { src_lanes } else { 0 };
            let mut results = [0u64; 8];
            for i in 0..src_lanes {
                let val = state.get_vreg_lane(rn as u32, src_offset + i, esize);
                results[i as usize] = mask(val << shift, dst_esize);
            }
            state.set_vreg_u128(rd as u32, 0);
            for i in 0..src_lanes {
                state.set_vreg_lane(rd as u32, i, dst_esize, results[i as usize]);
            }
        }
        // SHRN/SHRN2 (U=0, opcode=0b10000) — shift right narrow
        (false, 0b10000) => {
            let shift = (esize * 2) - immhb;
            let src_esize = esize * 2;
            let src_lanes = 128 / src_esize;
            if !q { state.set_vreg_u128(rd as u32, 0); }
            let dst_offset = if q { 64 / esize } else { 0 };
            for i in 0..src_lanes {
                let val = state.get_vreg_lane(rn as u32, i, src_esize);
                let shifted = if shift >= src_esize { 0 } else { val >> shift };
                state.set_vreg_lane(rd as u32, dst_offset + i, esize, mask(shifted, esize));
            }
        }
        _ => {
            log::warn!("Unimpl SimdShiftImm u={} opcode={:#07b} immh={}", u, opcode, immh);
        }
    }
    StepResult::Continue
}

/// Decode immh to element size.
fn decode_shift_immhb(immh: u8) -> (u32, u32) {
    if immh & 0b1000 != 0 {
        (64, 0)
    } else if immh & 0b0100 != 0 {
        (32, 0)
    } else if immh & 0b0010 != 0 {
        (16, 0)
    } else {
        (8, 0)
    }
}

// ---------------------------------------------------------------------------
// Permute (ZIP, UZP, TRN)
// ---------------------------------------------------------------------------

pub fn exec_simd_permute(
    state: &mut CpuState, q: bool, size: u8, opcode: u8,
    rd: u8, rn: u8, rm: u8,
) -> StepResult {
    let esize = esize_from_size(size);
    let lanes = if q { 128 / esize } else { 64 / esize };
    let half = lanes / 2;

    // Read all source values first (rd may alias rn or rm)
    let mut rn_vals = [0u64; 16];
    let mut rm_vals = [0u64; 16];
    for i in 0..lanes {
        rn_vals[i as usize] = state.get_vreg_lane(rn as u32, i, esize);
        rm_vals[i as usize] = state.get_vreg_lane(rm as u32, i, esize);
    }

    state.set_vreg_u128(rd as u32, 0);

    match opcode {
        // UZP1 (even elements): opcode=001
        0b001 => {
            for i in 0..half {
                state.set_vreg_lane(rd as u32, i, esize, rn_vals[(i * 2) as usize]);
                state.set_vreg_lane(rd as u32, half + i, esize, rm_vals[(i * 2) as usize]);
            }
        }
        // TRN1 (even elements interleaved): opcode=010
        0b010 => {
            for i in 0..half {
                state.set_vreg_lane(rd as u32, i * 2, esize, rn_vals[(i * 2) as usize]);
                state.set_vreg_lane(rd as u32, i * 2 + 1, esize, rm_vals[(i * 2) as usize]);
            }
        }
        // ZIP1 (interleave low halves): opcode=011
        0b011 => {
            for i in 0..half {
                state.set_vreg_lane(rd as u32, i * 2, esize, rn_vals[i as usize]);
                state.set_vreg_lane(rd as u32, i * 2 + 1, esize, rm_vals[i as usize]);
            }
        }
        // UZP2 (odd elements): opcode=101
        0b101 => {
            for i in 0..half {
                state.set_vreg_lane(rd as u32, i, esize, rn_vals[(i * 2 + 1) as usize]);
                state.set_vreg_lane(rd as u32, half + i, esize, rm_vals[(i * 2 + 1) as usize]);
            }
        }
        // TRN2 (odd elements interleaved): opcode=110
        0b110 => {
            for i in 0..half {
                state.set_vreg_lane(rd as u32, i * 2, esize, rn_vals[(i * 2 + 1) as usize]);
                state.set_vreg_lane(rd as u32, i * 2 + 1, esize, rm_vals[(i * 2 + 1) as usize]);
            }
        }
        // ZIP2 (interleave high halves): opcode=111
        0b111 => {
            for i in 0..half {
                state.set_vreg_lane(rd as u32, i * 2, esize, rn_vals[(half + i) as usize]);
                state.set_vreg_lane(rd as u32, i * 2 + 1, esize, rm_vals[(half + i) as usize]);
            }
        }
        _ => {
            log::warn!("Unimpl permute opcode={}", opcode);
        }
    }
    StepResult::Continue
}

// ---------------------------------------------------------------------------
// Extract (EXT)
// ---------------------------------------------------------------------------

pub fn exec_simd_extract(
    state: &mut CpuState, q: bool, imm4: u8,
    rd: u8, rn: u8, rm: u8,
) -> StepResult {
    let n = state.get_vreg_u128(rn as u32);
    let m = state.get_vreg_u128(rm as u32);
    let shift = (imm4 as u32) * 8;

    // Concatenate [Vm:Vn] and extract from byte position imm4
    let result = if shift == 0 {
        n
    } else if shift >= 128 {
        m >> (shift - 128)
    } else {
        (n >> shift) | (m << (128 - shift))
    };
    let q_mask = if q { u128::MAX } else { (1u128 << 64) - 1 };
    state.set_vreg_u128(rd as u32, result & q_mask);
    StepResult::Continue
}

// ---------------------------------------------------------------------------
// Across lanes (ADDV, SMAXV, SMINV, etc.)
// ---------------------------------------------------------------------------

pub fn exec_simd_across_lanes(
    state: &mut CpuState, q: bool, u: bool, size: u8,
    opcode: u8, rd: u8, rn: u8,
) -> StepResult {
    let esize = esize_from_size(size);
    let lanes = if q { 128 / esize } else { 64 / esize };

    match (u, opcode) {
        // ADDV (U=0, opcode=0b11011)
        (false, 0b11011) | (true, 0b11011) => {
            let mut sum = 0u64;
            for i in 0..lanes {
                sum = sum.wrapping_add(state.get_vreg_lane(rn as u32, i, esize));
            }
            state.set_vreg_u128(rd as u32, 0);
            state.set_vreg_lane(rd as u32, 0, esize, mask(sum, esize));
        }
        // SMAXV (U=0, opcode=0b01010)
        (false, 0b01010) => {
            let mut max_val = sign_extend_lane(state.get_vreg_lane(rn as u32, 0, esize), esize);
            for i in 1..lanes {
                let v = sign_extend_lane(state.get_vreg_lane(rn as u32, i, esize), esize);
                if v > max_val { max_val = v; }
            }
            state.set_vreg_u128(rd as u32, 0);
            state.set_vreg_lane(rd as u32, 0, esize, mask(max_val as u64, esize));
        }
        // SMINV (U=0, opcode=0b11010)
        (false, 0b11010) => {
            let mut min_val = sign_extend_lane(state.get_vreg_lane(rn as u32, 0, esize), esize);
            for i in 1..lanes {
                let v = sign_extend_lane(state.get_vreg_lane(rn as u32, i, esize), esize);
                if v < min_val { min_val = v; }
            }
            state.set_vreg_u128(rd as u32, 0);
            state.set_vreg_lane(rd as u32, 0, esize, mask(min_val as u64, esize));
        }
        // UMAXV (U=1, opcode=0b01010)
        (true, 0b01010) => {
            let mut max_val = state.get_vreg_lane(rn as u32, 0, esize);
            for i in 1..lanes {
                let v = state.get_vreg_lane(rn as u32, i, esize);
                if v > max_val { max_val = v; }
            }
            state.set_vreg_u128(rd as u32, 0);
            state.set_vreg_lane(rd as u32, 0, esize, max_val);
        }
        // UMINV (U=1, opcode=0b11010)
        (true, 0b11010) => {
            let mut min_val = state.get_vreg_lane(rn as u32, 0, esize);
            for i in 1..lanes {
                let v = state.get_vreg_lane(rn as u32, i, esize);
                if v < min_val { min_val = v; }
            }
            state.set_vreg_u128(rd as u32, 0);
            state.set_vreg_lane(rd as u32, 0, esize, min_val);
        }
        _ => {
            log::warn!("Unimpl SimdAcrossLanes u={} opcode={:#07b} size={}", u, opcode, size);
        }
    }
    StepResult::Continue
}

// ---------------------------------------------------------------------------
// Three different (widening/narrowing)
// ---------------------------------------------------------------------------

pub fn exec_simd_three_diff(
    state: &mut CpuState, q: bool, u: bool, size: u8,
    opcode: u8, rd: u8, rn: u8, rm: u8,
) -> StepResult {
    let src_esize = esize_from_size(size);
    let dst_esize = src_esize * 2;
    let src_lanes = 64 / src_esize;
    let src_offset = if q { src_lanes } else { 0 };

    match (u, opcode) {
        // SADDL/SADDL2 (U=0, opcode=0b0000)
        (false, 0b0000) => {
            state.set_vreg_u128(rd as u32, 0);
            for i in 0..src_lanes {
                let a = sign_extend_lane(state.get_vreg_lane(rn as u32, src_offset + i, src_esize), src_esize);
                let b = sign_extend_lane(state.get_vreg_lane(rm as u32, src_offset + i, src_esize), src_esize);
                state.set_vreg_lane(rd as u32, i, dst_esize, mask((a + b) as u64, dst_esize));
            }
        }
        // UADDL/UADDL2 (U=1, opcode=0b0000)
        (true, 0b0000) => {
            state.set_vreg_u128(rd as u32, 0);
            for i in 0..src_lanes {
                let a = state.get_vreg_lane(rn as u32, src_offset + i, src_esize);
                let b = state.get_vreg_lane(rm as u32, src_offset + i, src_esize);
                state.set_vreg_lane(rd as u32, i, dst_esize, a + b);
            }
        }
        // SSUBL/SSUBL2 (U=0, opcode=0b0010)
        (false, 0b0010) => {
            state.set_vreg_u128(rd as u32, 0);
            for i in 0..src_lanes {
                let a = sign_extend_lane(state.get_vreg_lane(rn as u32, src_offset + i, src_esize), src_esize);
                let b = sign_extend_lane(state.get_vreg_lane(rm as u32, src_offset + i, src_esize), src_esize);
                state.set_vreg_lane(rd as u32, i, dst_esize, mask((a - b) as u64, dst_esize));
            }
        }
        // USUBL/USUBL2 (U=1, opcode=0b0010)
        (true, 0b0010) => {
            state.set_vreg_u128(rd as u32, 0);
            for i in 0..src_lanes {
                let a = state.get_vreg_lane(rn as u32, src_offset + i, src_esize);
                let b = state.get_vreg_lane(rm as u32, src_offset + i, src_esize);
                state.set_vreg_lane(rd as u32, i, dst_esize, mask(a.wrapping_sub(b), dst_esize));
            }
        }
        // SMULL/SMULL2 (U=0, opcode=0b1100)
        (false, 0b1100) => {
            state.set_vreg_u128(rd as u32, 0);
            for i in 0..src_lanes {
                let a = sign_extend_lane(state.get_vreg_lane(rn as u32, src_offset + i, src_esize), src_esize);
                let b = sign_extend_lane(state.get_vreg_lane(rm as u32, src_offset + i, src_esize), src_esize);
                state.set_vreg_lane(rd as u32, i, dst_esize, mask((a.wrapping_mul(b)) as u64, dst_esize));
            }
        }
        // UMULL/UMULL2 (U=1, opcode=0b1100)
        (true, 0b1100) => {
            state.set_vreg_u128(rd as u32, 0);
            for i in 0..src_lanes {
                let a = state.get_vreg_lane(rn as u32, src_offset + i, src_esize);
                let b = state.get_vreg_lane(rm as u32, src_offset + i, src_esize);
                state.set_vreg_lane(rd as u32, i, dst_esize, mask(a.wrapping_mul(b), dst_esize));
            }
        }
        // SADDW/SADDW2 (U=0, opcode=0b0001)
        (false, 0b0001) => {
            state.set_vreg_u128(rd as u32, 0);
            for i in 0..src_lanes {
                let a = sign_extend_lane(state.get_vreg_lane(rn as u32, i, dst_esize), dst_esize);
                let b = sign_extend_lane(state.get_vreg_lane(rm as u32, src_offset + i, src_esize), src_esize);
                state.set_vreg_lane(rd as u32, i, dst_esize, mask((a + b) as u64, dst_esize));
            }
        }
        // UADDW/UADDW2 (U=1, opcode=0b0001)
        (true, 0b0001) => {
            state.set_vreg_u128(rd as u32, 0);
            for i in 0..src_lanes {
                let a = state.get_vreg_lane(rn as u32, i, dst_esize);
                let b = state.get_vreg_lane(rm as u32, src_offset + i, src_esize);
                state.set_vreg_lane(rd as u32, i, dst_esize, mask(a + b, dst_esize));
            }
        }
        _ => {
            log::warn!("Unimpl SimdThreeDiff u={} opcode={:#06b} size={}", u, opcode, size);
        }
    }
    StepResult::Continue
}

// ---------------------------------------------------------------------------
// Vector x indexed element
// ---------------------------------------------------------------------------

pub fn exec_simd_vec_indexed(
    state: &mut CpuState, q: bool, u: bool, size: u8,
    opcode: u8, rd: u8, rn: u8, rm: u8, h: u8, l: u8, m: u8,
) -> StepResult {
    // Decode element index from H:L:M depending on size
    let is_double = size & 1 != 0;
    let esize = if is_double { 64u32 } else { 32u32 };
    let lanes = if q { 128 / esize } else { 64 / esize };

    // For size=10 (single): index = H:L, Rm = M:Rm
    // For size=11 (double): index = H, Rm = Rm
    let (index, rm_reg) = if is_double {
        (h as u32, rm)
    } else {
        ((h as u32) << 1 | (l as u32), (m << 4) | rm)
    };

    let elem = state.get_vreg_lane(rm_reg as u32, index, esize);

    match opcode {
        // FMUL by element (opcode=0b1001)
        0b1001 => {
            let mut results = [0u64; 4];
            for i in 0..lanes {
                let a = state.get_vreg_lane(rn as u32, i, esize);
                results[i as usize] = if is_double {
                    (f64::from_bits(a) * f64::from_bits(elem)).to_bits()
                } else {
                    (f32::from_bits(a as u32) * f32::from_bits(elem as u32)).to_bits() as u64
                };
            }
            state.set_vreg_u128(rd as u32, 0);
            for i in 0..lanes {
                state.set_vreg_lane(rd as u32, i, esize, results[i as usize]);
            }
        }
        // FMLA by element (opcode=0b0001)
        0b0001 => {
            let mut results = [0u64; 4];
            for i in 0..lanes {
                let a = state.get_vreg_lane(rn as u32, i, esize);
                let d = state.get_vreg_lane(rd as u32, i, esize);
                results[i as usize] = if is_double {
                    f64::from_bits(a).mul_add(f64::from_bits(elem), f64::from_bits(d)).to_bits()
                } else {
                    f32::from_bits(a as u32).mul_add(f32::from_bits(elem as u32), f32::from_bits(d as u32)).to_bits() as u64
                };
            }
            if !q { state.v[rd as usize][1] = 0; }
            for i in 0..lanes {
                state.set_vreg_lane(rd as u32, i, esize, results[i as usize]);
            }
        }
        // FMLS by element (opcode=0b0101)
        0b0101 => {
            let mut results = [0u64; 4];
            for i in 0..lanes {
                let a = state.get_vreg_lane(rn as u32, i, esize);
                let d = state.get_vreg_lane(rd as u32, i, esize);
                results[i as usize] = if is_double {
                    (-f64::from_bits(a)).mul_add(f64::from_bits(elem), f64::from_bits(d)).to_bits()
                } else {
                    (-f32::from_bits(a as u32)).mul_add(f32::from_bits(elem as u32), f32::from_bits(d as u32)).to_bits() as u64
                };
            }
            if !q { state.v[rd as usize][1] = 0; }
            for i in 0..lanes {
                state.set_vreg_lane(rd as u32, i, esize, results[i as usize]);
            }
        }
        // MUL by element (opcode=0b1000) — integer
        0b1000 if !u => {
            let int_esize = esize_from_size(size);
            let _int_lanes = if q { 128 / int_esize } else { 64 / int_esize };
            // Re-decode index for integer
            let (int_idx, int_rm) = if size == 1 {
                ((h as u32) << 2 | (l as u32) << 1 | (m as u32), rm)
            } else {
                ((h as u32) << 1 | (l as u32), (m << 4) | rm)
            };
            let int_elem = state.get_vreg_lane(int_rm as u32, int_idx, int_esize);
            for_each_lane_unary(state, q, int_esize, rd, rn, |a| {
                mask(a.wrapping_mul(int_elem), int_esize)
            });
        }
        _ => {
            log::warn!("Unimpl SimdVecIndexed u={} opcode={:#06b} size={}", u, opcode, size);
        }
    }
    StepResult::Continue
}

// ---------------------------------------------------------------------------
// SIMD load/store multiple structures
// ---------------------------------------------------------------------------

pub fn exec_simd_ldst_multi(
    state: &mut CpuState, mem: &mut dyn MemoryAccess,
    q: bool, load: bool, opcode: u8, size: u8,
    rn: u8, rt: u8, rm: Option<u8>,
) -> StepResult {
    let esize = 8u32 << size;
    let datasize = if q { 128u32 } else { 64u32 };
    let elem_bytes = (esize / 8) as u64;

    // Decode opcode to get (nregs, interleave)
    let (nregs, interleave) = match opcode {
        0b0111 => (1, false),   // LD1/ST1 (1 reg)
        0b1010 => (2, false),   // LD1/ST1 (2 regs)
        0b0110 => (3, false),   // LD1/ST1 (3 regs)
        0b0010 => (4, false),   // LD1/ST1 (4 regs)
        0b1000 => (2, true),    // LD2/ST2
        0b0100 => (3, true),    // LD3/ST3
        0b0000 => (4, true),    // LD4/ST4
        _ => {
            log::warn!("Unimpl SIMD ldst multi opcode={:#06b}", opcode);
            return StepResult::Continue;
        }
    };

    let base_addr = state.get_reg(rn as u32);
    let total_bytes = (nregs as u64) * (datasize as u64 / 8);

    if !interleave {
        // LD1/ST1 with N consecutive registers
        let mut addr = base_addr;
        for reg_offset in 0..nregs {
            let reg = ((rt as usize + reg_offset) % 32) as u32;
            if load {
                let val = if q {
                    match mem.read_u128(addr) {
                        Ok(v) => v,
                        Err(_) => return StepResult::MemoryFault(addr),
                    }
                } else {
                    match mem.read_u64(addr) {
                        Ok(v) => v as u128,
                        Err(_) => return StepResult::MemoryFault(addr),
                    }
                };
                state.set_vreg_u128(reg, val);
            } else {
                let val = state.get_vreg_u128(reg);
                if q {
                    if mem.write_u128(addr, val).is_err() {
                        return StepResult::MemoryFault(addr);
                    }
                } else {
                    if mem.write_u64(addr, val as u64).is_err() {
                        return StepResult::MemoryFault(addr);
                    }
                }
            }
            addr = addr.wrapping_add(datasize as u64 / 8);
        }
    } else {
        // Interleaving load/store (LD2/LD3/LD4)
        let lanes = datasize / esize;
        let mut addr = base_addr;
        for i in 0..lanes as usize {
            for s in 0..nregs {
                let reg = ((rt as usize + s) % 32) as u32;
                if load {
                    let val = match elem_bytes {
                        1 => mem.read_u8(addr).map(|v| v as u64),
                        2 => mem.read_u16(addr).map(|v| v as u64),
                        4 => mem.read_u32(addr).map(|v| v as u64),
                        8 => mem.read_u64(addr),
                        _ => return StepResult::MemoryFault(addr),
                    };
                    match val {
                        Ok(v) => state.set_vreg_lane(reg, i as u32, esize, v),
                        Err(_) => return StepResult::MemoryFault(addr),
                    }
                } else {
                    let val = state.get_vreg_lane(reg, i as u32, esize);
                    let result = match elem_bytes {
                        1 => mem.write_u8(addr, val as u8),
                        2 => mem.write_u16(addr, val as u16),
                        4 => mem.write_u32(addr, val as u32),
                        8 => mem.write_u64(addr, val),
                        _ => return StepResult::MemoryFault(addr),
                    };
                    if result.is_err() {
                        return StepResult::MemoryFault(addr);
                    }
                }
                addr = addr.wrapping_add(elem_bytes);
            }
        }
    }

    // Post-index writeback
    if let Some(rm_reg) = rm {
        let wb = if rm_reg == 31 {
            base_addr.wrapping_add(total_bytes)
        } else {
            base_addr.wrapping_add(state.get_reg(rm_reg as u32))
        };
        state.set_reg(rn as u32, wb);
    }

    StepResult::Continue
}

// ---------------------------------------------------------------------------
// SIMD load/store single structure
// ---------------------------------------------------------------------------

pub fn exec_simd_ldst_single(
    state: &mut CpuState, mem: &mut dyn MemoryAccess,
    q: bool, load: bool, _r: u8, opcode: u8, s: u8, size: u8,
    rn: u8, rt: u8, rm: Option<u8>,
) -> StepResult {
    // Decode element size and index from opcode/size/S/Q
    let (esize, index) = match (opcode, size) {
        // B variant: opcode=0b000, esize=8, index=Q:S:size
        (0b000, _) => (8u32, ((q as u32) << 3) | ((s as u32) << 2) | (size as u32)),
        // H variant: opcode=0b010, esize=16, index=Q:S:size[1]
        (0b010, _) => (16u32, ((q as u32) << 2) | ((s as u32) << 1) | ((size as u32) >> 1)),
        // D variant: opcode=0b100, size=01, esize=64, index=Q
        (0b100, 1) => (64u32, q as u32),
        // S variant: opcode=0b100, esize=32, index=Q:S
        (0b100, _) => (32u32, ((q as u32) << 1) | (s as u32)),
        _ => {
            log::warn!("Unimpl SimdLdStSingle opcode={:#05b} size={}", opcode, size);
            return StepResult::Continue;
        }
    };

    let elem_bytes = (esize / 8) as u64;
    let base_addr = state.get_reg(rn as u32);

    if load {
        let val = match elem_bytes {
            1 => mem.read_u8(base_addr).map(|v| v as u64),
            2 => mem.read_u16(base_addr).map(|v| v as u64),
            4 => mem.read_u32(base_addr).map(|v| v as u64),
            8 => mem.read_u64(base_addr),
            _ => return StepResult::MemoryFault(base_addr),
        };
        match val {
            Ok(v) => state.set_vreg_lane(rt as u32, index, esize, v),
            Err(_) => return StepResult::MemoryFault(base_addr),
        }
    } else {
        let val = state.get_vreg_lane(rt as u32, index, esize);
        let result = match elem_bytes {
            1 => mem.write_u8(base_addr, val as u8),
            2 => mem.write_u16(base_addr, val as u16),
            4 => mem.write_u32(base_addr, val as u32),
            8 => mem.write_u64(base_addr, val),
            _ => return StepResult::MemoryFault(base_addr),
        };
        if result.is_err() {
            return StepResult::MemoryFault(base_addr);
        }
    }

    // Post-index writeback
    if let Some(rm_reg) = rm {
        let wb = if rm_reg == 31 {
            base_addr.wrapping_add(elem_bytes)
        } else {
            base_addr.wrapping_add(state.get_reg(rm_reg as u32))
        };
        state.set_reg(rn as u32, wb);
    }

    StepResult::Continue
}

// ---------------------------------------------------------------------------
// Scalar three same
// ---------------------------------------------------------------------------

pub fn exec_simd_scalar_three_same(
    state: &mut CpuState, u: bool, size: u8,
    opcode: u8, rd: u8, rn: u8, rm: u8,
) -> StepResult {
    let is_double = size & 1 != 0;
    let esize = if is_double { 64u32 } else { 32u32 };

    // Most scalar three-same are FP operations
    if opcode >= 0b11000 {
        let a_bits = state.get_vreg_lane(rn as u32, 0, esize);
        let b_bits = state.get_vreg_lane(rm as u32, 0, esize);

        let result = if is_double {
            let a = f64::from_bits(a_bits);
            let b = f64::from_bits(b_bits);
            match (u, opcode) {
                (false, 0b11010) => (a + b).to_bits(),
                (true, 0b11010) => (a - b).to_bits(),
                (false, 0b11011) | (true, 0b11011) => (a * b).to_bits(),
                (true, 0b11111) => (a / b).to_bits(),
                (false, 0b11110) => a.max(b).to_bits(),
                (true, 0b11110) => a.min(b).to_bits(),
                (false, 0b11100) => if a == b { u64::MAX } else { 0 },
                (true, 0b11100) => if a >= b { u64::MAX } else { 0 },
                (true, 0b11001) => if a > b { u64::MAX } else { 0 },
                _ => {
                    log::warn!("Unimpl scalar three same FP: u={} opcode={:#07b}", u, opcode);
                    0
                }
            }
        } else {
            let a = f32::from_bits(a_bits as u32);
            let b = f32::from_bits(b_bits as u32);
            match (u, opcode) {
                (false, 0b11010) => (a + b).to_bits() as u64,
                (true, 0b11010) => (a - b).to_bits() as u64,
                (false, 0b11011) | (true, 0b11011) => (a * b).to_bits() as u64,
                (true, 0b11111) => (a / b).to_bits() as u64,
                (false, 0b11110) => a.max(b).to_bits() as u64,
                (true, 0b11110) => a.min(b).to_bits() as u64,
                (false, 0b11100) => if a == b { u32::MAX as u64 } else { 0 },
                (true, 0b11100) => if a >= b { u32::MAX as u64 } else { 0 },
                (true, 0b11001) => if a > b { u32::MAX as u64 } else { 0 },
                _ => {
                    log::warn!("Unimpl scalar three same FP f32: u={} opcode={:#07b}", u, opcode);
                    0
                }
            }
        };

        state.set_vreg_u128(rd as u32, 0);
        state.set_vreg_lane(rd as u32, 0, esize, result);
        return StepResult::Continue;
    }

    // Integer scalar ops
    match (u, opcode) {
        // CMEQ scalar
        (true, 0b10001) => {
            let a = state.get_vreg_lane(rn as u32, 0, 64);
            let b = state.get_vreg_lane(rm as u32, 0, 64);
            state.set_vreg_u128(rd as u32, 0);
            state.set_vreg_lane(rd as u32, 0, 64, if a == b { u64::MAX } else { 0 });
        }
        // ADD scalar (64-bit)
        (false, 0b10000) => {
            let a = state.get_vreg_lane(rn as u32, 0, 64);
            let b = state.get_vreg_lane(rm as u32, 0, 64);
            state.set_vreg_u128(rd as u32, 0);
            state.set_vreg_lane(rd as u32, 0, 64, a.wrapping_add(b));
        }
        // SUB scalar (64-bit)
        (true, 0b10000) => {
            let a = state.get_vreg_lane(rn as u32, 0, 64);
            let b = state.get_vreg_lane(rm as u32, 0, 64);
            state.set_vreg_u128(rd as u32, 0);
            state.set_vreg_lane(rd as u32, 0, 64, a.wrapping_sub(b));
        }
        _ => {
            log::warn!("Unimpl SimdScalarThreeSame u={} opcode={:#07b} size={}", u, opcode, size);
        }
    }
    StepResult::Continue
}

// ---------------------------------------------------------------------------
// Scalar two-reg misc
// ---------------------------------------------------------------------------

pub fn exec_simd_scalar_two_reg(
    state: &mut CpuState, u: bool, size: u8,
    opcode: u8, rd: u8, rn: u8,
) -> StepResult {
    let is_double = size & 1 != 0;
    let esize = if is_double { 64u32 } else { 32u32 };

    match (u, opcode) {
        // FCVTZS scalar
        (false, 0b11011) => {
            let val = state.get_vreg_lane(rn as u32, 0, esize);
            let result = if is_double {
                let f = f64::from_bits(val);
                f.clamp(i64::MIN as f64, i64::MAX as f64) as i64 as u64
            } else {
                let f = f32::from_bits(val as u32);
                f.clamp(i32::MIN as f32, i32::MAX as f32) as i32 as u32 as u64
            };
            state.set_vreg_u128(rd as u32, 0);
            state.set_vreg_lane(rd as u32, 0, esize, result);
        }
        // FCVTZU scalar
        (true, 0b11011) => {
            let val = state.get_vreg_lane(rn as u32, 0, esize);
            let result = if is_double {
                let f = f64::from_bits(val);
                f.clamp(0.0, u64::MAX as f64) as u64
            } else {
                let f = f32::from_bits(val as u32);
                f.clamp(0.0, u32::MAX as f32) as u32 as u64
            };
            state.set_vreg_u128(rd as u32, 0);
            state.set_vreg_lane(rd as u32, 0, esize, result);
        }
        // ABS scalar (64-bit)
        (false, 0b01011) => {
            let val = state.get_vreg_lane(rn as u32, 0, 64) as i64;
            state.set_vreg_u128(rd as u32, 0);
            state.set_vreg_lane(rd as u32, 0, 64, val.unsigned_abs());
        }
        // NEG scalar (64-bit)
        (true, 0b01011) => {
            let val = state.get_vreg_lane(rn as u32, 0, 64);
            state.set_vreg_u128(rd as u32, 0);
            state.set_vreg_lane(rd as u32, 0, 64, 0u64.wrapping_sub(val));
        }
        _ => {
            log::warn!("Unimpl SimdScalarTwoReg u={} opcode={:#07b} size={}", u, opcode, size);
        }
    }
    StepResult::Continue
}

// ---------------------------------------------------------------------------
// Scalar pairwise
// ---------------------------------------------------------------------------

pub fn exec_simd_scalar_pairwise(
    state: &mut CpuState, u: bool, size: u8,
    opcode: u8, rd: u8, rn: u8,
) -> StepResult {
    let is_double = size & 1 != 0;
    let esize = if is_double { 64u32 } else { 32u32 };

    // FADDP scalar: takes two elements from Vn and adds them
    match (u, opcode) {
        (false, 0b11011) | (true, 0b01101) => {
            // FADDP scalar
            let a_bits = state.get_vreg_lane(rn as u32, 0, esize);
            let b_bits = state.get_vreg_lane(rn as u32, 1, esize);
            let result = if is_double {
                (f64::from_bits(a_bits) + f64::from_bits(b_bits)).to_bits()
            } else {
                (f32::from_bits(a_bits as u32) + f32::from_bits(b_bits as u32)).to_bits() as u64
            };
            state.set_vreg_u128(rd as u32, 0);
            state.set_vreg_lane(rd as u32, 0, esize, result);
        }
        // ADDP scalar (integer, 64-bit pair)
        (false, 0b10111) => {
            let a = state.get_vreg_lane(rn as u32, 0, 64);
            let b = state.get_vreg_lane(rn as u32, 1, 64);
            state.set_vreg_u128(rd as u32, 0);
            state.set_vreg_lane(rd as u32, 0, 64, a.wrapping_add(b));
        }
        _ => {
            log::warn!("Unimpl SimdScalarPairwise u={} opcode={:#07b} size={}", u, opcode, size);
        }
    }
    StepResult::Continue
}

// ---------------------------------------------------------------------------
// TBL / TBX
// ---------------------------------------------------------------------------

/// Execute TBL/TBX (table lookup).
///
/// Concatenates `len+1` consecutive V registers starting from `rn` into a
/// lookup table. For each byte in the source (`rm`), uses it as an index
/// into the table. Out-of-range indices: TBL writes 0, TBX keeps destination.
pub fn exec_simd_tbl(
    state: &mut CpuState,
    q: bool,
    rd: u8,
    rn: u8,
    rm: u8,
    len: u8,
    op: u8,
) -> StepResult {
    let num_regs = (len as usize) + 1; // 1-4 registers
    let bytes = if q { 16 } else { 8 };

    // Build the lookup table from consecutive registers.
    let table_size = num_regs * 16;
    let mut table = vec![0u8; table_size];
    for r in 0..num_regs {
        let reg_idx = ((rn as usize) + r) % 32;
        let lo = state.v[reg_idx][0];
        let hi = state.v[reg_idx][1];
        let offset = r * 16;
        table[offset..offset + 8].copy_from_slice(&lo.to_le_bytes());
        table[offset + 8..offset + 16].copy_from_slice(&hi.to_le_bytes());
    }

    // Read source indices from Vm.
    let src_lo = state.v[rm as usize][0];
    let src_hi = state.v[rm as usize][1];
    let mut src_bytes = [0u8; 16];
    src_bytes[..8].copy_from_slice(&src_lo.to_le_bytes());
    src_bytes[8..].copy_from_slice(&src_hi.to_le_bytes());

    // Read current destination (for TBX).
    let dst_lo = state.v[rd as usize][0];
    let dst_hi = state.v[rd as usize][1];
    let mut dst_bytes = [0u8; 16];
    dst_bytes[..8].copy_from_slice(&dst_lo.to_le_bytes());
    dst_bytes[8..].copy_from_slice(&dst_hi.to_le_bytes());

    let mut result = [0u8; 16];
    for i in 0..bytes {
        let idx = src_bytes[i] as usize;
        if idx < table_size {
            result[i] = table[idx];
        } else if op == 1 {
            // TBX: keep destination byte
            result[i] = dst_bytes[i];
        }
        // TBL: result[i] stays 0 for out-of-range
    }

    // Write result.
    let lo = u64::from_le_bytes(result[..8].try_into().unwrap());
    let hi = if q {
        u64::from_le_bytes(result[8..16].try_into().unwrap())
    } else {
        0
    };
    state.v[rd as usize] = [lo, hi];

    StepResult::Continue
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::decoder;

    // Helper to create a CpuState with V registers set
    fn state_with_vreg(reg: u32, val: u128) -> CpuState {
        let mut s = CpuState::new();
        s.set_vreg_u128(reg, val);
        s
    }

    #[test]
    fn test_get_set_vreg_lane_u32() {
        let mut s = CpuState::new();
        s.set_vreg_lane(0, 0, 32, 0xDEAD_BEEF);
        s.set_vreg_lane(0, 1, 32, 0xCAFE_BABE);
        s.set_vreg_lane(0, 2, 32, 0x1234_5678);
        s.set_vreg_lane(0, 3, 32, 0x9ABC_DEF0);
        assert_eq!(s.get_vreg_lane(0, 0, 32), 0xDEAD_BEEF);
        assert_eq!(s.get_vreg_lane(0, 1, 32), 0xCAFE_BABE);
        assert_eq!(s.get_vreg_lane(0, 2, 32), 0x1234_5678);
        assert_eq!(s.get_vreg_lane(0, 3, 32), 0x9ABC_DEF0);
    }

    #[test]
    fn test_get_set_vreg_lane_u8() {
        let mut s = CpuState::new();
        for i in 0..16u32 {
            s.set_vreg_lane(0, i, 8, i as u64 + 1);
        }
        for i in 0..16u32 {
            assert_eq!(s.get_vreg_lane(0, i, 8), i as u64 + 1);
        }
    }

    #[test]
    fn test_get_set_vreg_lane_u64() {
        let mut s = CpuState::new();
        s.set_vreg_lane(0, 0, 64, 0x1111_2222_3333_4444);
        s.set_vreg_lane(0, 1, 64, 0x5555_6666_7777_8888);
        assert_eq!(s.get_vreg_lane(0, 0, 64), 0x1111_2222_3333_4444);
        assert_eq!(s.get_vreg_lane(0, 1, 64), 0x5555_6666_7777_8888);
    }

    #[test]
    fn test_decode_add_4s() {
        // ADD V0.4S, V1.4S, V2.4S = 0x4EA28420
        let inst = decoder::decode(0x4EA28420);
        match inst {
            decoder::Instruction::SimdThreeSame { q: true, u: false, size: 2, opcode: 0b10000, rd: 0, rn: 1, rm: 2 } => {}
            other => panic!("Expected SimdThreeSame ADD.4S, got {:?}", other),
        }
    }

    #[test]
    fn test_exec_add_4s() {
        let mut s = CpuState::new();
        // V1 = [1, 2, 3, 4] as u32 lanes
        s.set_vreg_lane(1, 0, 32, 1);
        s.set_vreg_lane(1, 1, 32, 2);
        s.set_vreg_lane(1, 2, 32, 3);
        s.set_vreg_lane(1, 3, 32, 4);
        // V2 = [10, 20, 30, 40]
        s.set_vreg_lane(2, 0, 32, 10);
        s.set_vreg_lane(2, 1, 32, 20);
        s.set_vreg_lane(2, 2, 32, 30);
        s.set_vreg_lane(2, 3, 32, 40);

        exec_simd_three_same(&mut s, true, false, 2, 0b10000, 0, 1, 2);
        assert_eq!(s.get_vreg_lane(0, 0, 32), 11);
        assert_eq!(s.get_vreg_lane(0, 1, 32), 22);
        assert_eq!(s.get_vreg_lane(0, 2, 32), 33);
        assert_eq!(s.get_vreg_lane(0, 3, 32), 44);
    }

    #[test]
    fn test_exec_sub_4s() {
        let mut s = CpuState::new();
        s.set_vreg_lane(1, 0, 32, 100);
        s.set_vreg_lane(1, 1, 32, 200);
        s.set_vreg_lane(2, 0, 32, 30);
        s.set_vreg_lane(2, 1, 32, 50);

        exec_simd_three_same(&mut s, false, true, 2, 0b10000, 0, 1, 2);
        assert_eq!(s.get_vreg_lane(0, 0, 32), 70);
        assert_eq!(s.get_vreg_lane(0, 1, 32), 150);
    }

    #[test]
    fn test_exec_mul_4s() {
        let mut s = CpuState::new();
        s.set_vreg_lane(1, 0, 32, 3);
        s.set_vreg_lane(1, 1, 32, 7);
        s.set_vreg_lane(2, 0, 32, 4);
        s.set_vreg_lane(2, 1, 32, 5);

        exec_simd_three_same(&mut s, false, false, 2, 0b10011, 0, 1, 2);
        assert_eq!(s.get_vreg_lane(0, 0, 32), 12);
        assert_eq!(s.get_vreg_lane(0, 1, 32), 35);
    }

    #[test]
    fn test_exec_cmeq_4s() {
        let mut s = CpuState::new();
        s.set_vreg_lane(1, 0, 32, 5);
        s.set_vreg_lane(1, 1, 32, 10);
        s.set_vreg_lane(2, 0, 32, 5);
        s.set_vreg_lane(2, 1, 32, 20);

        exec_simd_three_same(&mut s, false, true, 2, 0b10001, 0, 1, 2);
        assert_eq!(s.get_vreg_lane(0, 0, 32), 0xFFFF_FFFF);
        assert_eq!(s.get_vreg_lane(0, 1, 32), 0);
    }

    #[test]
    fn test_exec_orr_16b() {
        let mut s = CpuState::new();
        s.set_vreg_u128(1, 0xFF00_FF00_FF00_FF00_FF00_FF00_FF00_FF00);
        s.set_vreg_u128(2, 0x00FF_00FF_00FF_00FF_00FF_00FF_00FF_00FF);

        exec_simd_three_same(&mut s, true, false, 2, 0b00011, 0, 1, 2);
        assert_eq!(s.get_vreg_u128(0), 0xFFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF);
    }

    #[test]
    fn test_exec_eor_16b() {
        let mut s = CpuState::new();
        s.set_vreg_u128(1, 0xAAAA_AAAA_AAAA_AAAA_AAAA_AAAA_AAAA_AAAA);
        s.set_vreg_u128(2, 0xFFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF);

        exec_simd_three_same(&mut s, true, true, 0, 0b00011, 0, 1, 2);
        assert_eq!(s.get_vreg_u128(0), 0x5555_5555_5555_5555_5555_5555_5555_5555);
    }

    #[test]
    fn test_exec_not_16b() {
        let mut s = CpuState::new();
        s.set_vreg_u128(1, 0xAAAA_AAAA_AAAA_AAAA_AAAA_AAAA_AAAA_AAAA);

        exec_simd_two_reg(&mut s, true, true, 0, 0b00101, 0, 1);
        assert_eq!(s.get_vreg_u128(0), 0x5555_5555_5555_5555_5555_5555_5555_5555);
    }

    #[test]
    fn test_exec_dup_general() {
        let mut s = CpuState::new();
        s.set_reg(5, 42);

        // DUP V0.4S, W5: op=0, imm4=0001, imm5=0b00100 (32-bit element)
        exec_simd_copy(&mut s, true, 0, 0b00100, 0b0001, 0, 5);
        assert_eq!(s.get_vreg_lane(0, 0, 32), 42);
        assert_eq!(s.get_vreg_lane(0, 1, 32), 42);
        assert_eq!(s.get_vreg_lane(0, 2, 32), 42);
        assert_eq!(s.get_vreg_lane(0, 3, 32), 42);
    }

    #[test]
    fn test_exec_dup_element() {
        let mut s = CpuState::new();
        s.set_vreg_lane(1, 0, 32, 10);
        s.set_vreg_lane(1, 1, 32, 20);
        s.set_vreg_lane(1, 2, 32, 30);

        // DUP V0.4S, V1.S[1]: op=0, imm4=0000, imm5=0b01100 (S[1] => index=1, esize=32)
        exec_simd_copy(&mut s, true, 0, 0b01100, 0b0000, 0, 1);
        assert_eq!(s.get_vreg_lane(0, 0, 32), 20);
        assert_eq!(s.get_vreg_lane(0, 1, 32), 20);
        assert_eq!(s.get_vreg_lane(0, 2, 32), 20);
        assert_eq!(s.get_vreg_lane(0, 3, 32), 20);
    }

    #[test]
    fn test_exec_movi() {
        let mut s = CpuState::new();
        // MOVI V0.4S, #0x12 — cmode=0000, op=0, imm8=0x12
        exec_simd_mod_imm(&mut s, true, 0, 0b0000, 0, 0x12);
        assert_eq!(s.get_vreg_lane(0, 0, 32), 0x12);
        assert_eq!(s.get_vreg_lane(0, 1, 32), 0x12);
        assert_eq!(s.get_vreg_lane(0, 2, 32), 0x12);
        assert_eq!(s.get_vreg_lane(0, 3, 32), 0x12);
    }

    #[test]
    fn test_exec_shl_4s() {
        let mut s = CpuState::new();
        s.set_vreg_lane(1, 0, 32, 1);
        s.set_vreg_lane(1, 1, 32, 0xFF);

        // SHL V0.4S, V1.4S, #3: immh=0100, immb=011, opcode=01010
        // immhb = 0b0100_011 = 35, esize=32, shift = 35 - 32 = 3
        exec_simd_shift_imm(&mut s, false, false, 0b0100, 0b011, 0b01010, 0, 1);
        assert_eq!(s.get_vreg_lane(0, 0, 32), 8);
        assert_eq!(s.get_vreg_lane(0, 1, 32), 0xFF << 3);
    }

    #[test]
    fn test_exec_ushr_4s() {
        let mut s = CpuState::new();
        s.set_vreg_lane(1, 0, 32, 0x80);
        s.set_vreg_lane(1, 1, 32, 0xFF00);

        // USHR V0.4S, V1.4S, #4: immh=0100, immb=100, opcode=00000
        // immhb = 0b0100_100 = 36, esize=32, shift = 64 - 36 = 28... no
        // For USHR: shift = 2*esize - immhb = 64 - 36 = 28
        // That's wrong. Let me recalculate:
        // USHR shift = (2 * esize) - immhb but immhb encodes (esize + shift)
        // For SHL:  shift = immhb - esize
        // For USHR: shift = (2 * esize) - immhb
        // To get shift=4 with esize=32: immhb = 64 - 4 = 60 = 0b0111100
        // immh=0b0111, immb=0b100 → but immh=0111 means esize=64...
        // Actually: immh=0b0100 means esize=32, immhb range is [32, 63]
        // shift = 64 - immhb. For shift=4: immhb=60=0b0111100
        // immh=0b0111, which has bit 2 set → esize=32. immb = 100.
        // Hmm, immh=0b0111 has bit 3 not set, bit 2 set → esize=32. OK.
        // But wait immh = bits[22:19] so 4 bits. immh=0b0111 → bit[22]=0, [21]=1, [20]=1, [19]=1
        // With immb = bits[18:16] = 0b100
        // immhb = 0b0111_100 = 60. shift = 64-60 = 4. Correct!
        exec_simd_shift_imm(&mut s, false, true, 0b0111, 0b100, 0b00000, 0, 1);
        assert_eq!(s.get_vreg_lane(0, 0, 32), 0x80 >> 4);
        assert_eq!(s.get_vreg_lane(0, 1, 32), 0xFF00 >> 4);
    }

    #[test]
    fn test_exec_zip1_4s() {
        let mut s = CpuState::new();
        s.set_vreg_lane(1, 0, 32, 1);
        s.set_vreg_lane(1, 1, 32, 2);
        s.set_vreg_lane(1, 2, 32, 3);
        s.set_vreg_lane(1, 3, 32, 4);
        s.set_vreg_lane(2, 0, 32, 10);
        s.set_vreg_lane(2, 1, 32, 20);
        s.set_vreg_lane(2, 2, 32, 30);
        s.set_vreg_lane(2, 3, 32, 40);

        exec_simd_permute(&mut s, true, 2, 0b011, 0, 1, 2);
        assert_eq!(s.get_vreg_lane(0, 0, 32), 1);
        assert_eq!(s.get_vreg_lane(0, 1, 32), 10);
        assert_eq!(s.get_vreg_lane(0, 2, 32), 2);
        assert_eq!(s.get_vreg_lane(0, 3, 32), 20);
    }

    #[test]
    fn test_exec_ext() {
        let mut s = CpuState::new();
        s.set_vreg_u128(1, 0x0F0E0D0C0B0A09080706050403020100);
        s.set_vreg_u128(2, 0x1F1E1D1C1B1A19181716151413121110);

        exec_simd_extract(&mut s, true, 4, 0, 1, 2);
        // Extract bytes [4..20) from [V2:V1]
        // Shift by 4 bytes = 32 bits
        let r = s.get_vreg_u128(0);
        // Lower bytes: V1 >> 32 = 0x0F0E0D0C0B0A0908070605040
        // + V2 << 96
        assert_eq!(r & 0xFF, 0x04);
    }

    #[test]
    fn test_exec_fadd_4s() {
        let mut s = CpuState::new();
        s.set_vreg_lane(1, 0, 32, 1.0f32.to_bits() as u64);
        s.set_vreg_lane(1, 1, 32, 2.0f32.to_bits() as u64);
        s.set_vreg_lane(2, 0, 32, 3.0f32.to_bits() as u64);
        s.set_vreg_lane(2, 1, 32, 4.0f32.to_bits() as u64);

        // FADD V0.4S, V1.4S, V2.4S: u=0, size=0b10, opcode=0b11010
        exec_simd_three_same(&mut s, false, false, 0b10, 0b11010, 0, 1, 2);
        let r0 = f32::from_bits(s.get_vreg_lane(0, 0, 32) as u32);
        let r1 = f32::from_bits(s.get_vreg_lane(0, 1, 32) as u32);
        assert!((r0 - 4.0).abs() < f32::EPSILON);
        assert!((r1 - 6.0).abs() < f32::EPSILON);
    }

    #[test]
    fn test_exec_addv() {
        let mut s = CpuState::new();
        s.set_vreg_lane(1, 0, 32, 10);
        s.set_vreg_lane(1, 1, 32, 20);
        s.set_vreg_lane(1, 2, 32, 30);
        s.set_vreg_lane(1, 3, 32, 40);

        exec_simd_across_lanes(&mut s, true, false, 2, 0b11011, 0, 1);
        assert_eq!(s.get_vreg_lane(0, 0, 32), 100);
    }

    #[test]
    fn test_exec_cnt() {
        let mut s = CpuState::new();
        s.set_vreg_lane(1, 0, 8, 0xFF);
        s.set_vreg_lane(1, 1, 8, 0x0F);
        s.set_vreg_lane(1, 2, 8, 0x01);

        exec_simd_two_reg(&mut s, false, false, 0, 0b00101, 0, 1);
        assert_eq!(s.get_vreg_lane(0, 0, 8), 8);
        assert_eq!(s.get_vreg_lane(0, 1, 8), 4);
        assert_eq!(s.get_vreg_lane(0, 2, 8), 1);
    }

    #[test]
    fn test_exec_neg_4s() {
        let mut s = CpuState::new();
        s.set_vreg_lane(1, 0, 32, 5);
        s.set_vreg_lane(1, 1, 32, 0xFFFF_FFFF); // -1 as u32

        exec_simd_two_reg(&mut s, false, true, 2, 0b01011, 0, 1);
        assert_eq!(s.get_vreg_lane(0, 0, 32), 0xFFFF_FFFB); // -5 as u32
        assert_eq!(s.get_vreg_lane(0, 1, 32), 1);
    }

    #[test]
    fn test_decode_ld1_multi() {
        // LD1 {V0.4S}, [X1]: 0x4C407820
        let inst = decoder::decode(0x4C407820);
        match inst {
            decoder::Instruction::SimdLdStMulti {
                q: true, load: true, opcode: 0b0111, size: 2, rn: 1, rt: 0, rm: None,
            } => {}
            other => panic!("Expected SimdLdStMulti LD1, got {:?}", other),
        }
    }

    #[test]
    fn test_decode_movi() {
        // MOVI V0.4S, #0: 0x4F000400
        let inst = decoder::decode(0x4F000400);
        match inst {
            decoder::Instruction::SimdModImm { q: true, .. } => {}
            other => panic!("Expected SimdModImm MOVI, got {:?}", other),
        }
    }

    #[test]
    fn test_exec_addp() {
        let mut s = CpuState::new();
        // V1 = [1, 2, 3, 4]
        s.set_vreg_lane(1, 0, 32, 1);
        s.set_vreg_lane(1, 1, 32, 2);
        s.set_vreg_lane(1, 2, 32, 3);
        s.set_vreg_lane(1, 3, 32, 4);
        // V2 = [10, 20, 30, 40]
        s.set_vreg_lane(2, 0, 32, 10);
        s.set_vreg_lane(2, 1, 32, 20);
        s.set_vreg_lane(2, 2, 32, 30);
        s.set_vreg_lane(2, 3, 32, 40);

        exec_simd_three_same(&mut s, true, false, 2, 0b10111, 0, 1, 2);
        // ADDP: pairs from V1: [1+2, 3+4] = [3, 7]; pairs from V2: [10+20, 30+40] = [30, 70]
        assert_eq!(s.get_vreg_lane(0, 0, 32), 3);
        assert_eq!(s.get_vreg_lane(0, 1, 32), 7);
        assert_eq!(s.get_vreg_lane(0, 2, 32), 30);
        assert_eq!(s.get_vreg_lane(0, 3, 32), 70);
    }

    #[test]
    fn test_exec_umov() {
        let mut s = CpuState::new();
        s.set_vreg_lane(1, 2, 32, 0xDEAD_BEEF);

        // UMOV W0, V1.S[2]: op=0, imm4=0111, imm5=0b10100 (S[2])
        exec_simd_copy(&mut s, false, 0, 0b10100, 0b0111, 0, 1);
        assert_eq!(s.get_reg(0), 0xDEAD_BEEF);
    }

    #[test]
    fn test_tbl_basic() {
        let mut s = CpuState::new();
        // V1 = table: bytes 0..15 = [0x10, 0x11, ... 0x1F]
        let mut table_bytes = [0u8; 16];
        for i in 0..16 {
            table_bytes[i] = 0x10 + i as u8;
        }
        let lo = u64::from_le_bytes(table_bytes[..8].try_into().unwrap());
        let hi = u64::from_le_bytes(table_bytes[8..].try_into().unwrap());
        s.v[1] = [lo, hi];

        // V2 = indices: [0, 3, 15, 0xFF, 0, 0, 0, 0, ...]
        s.v[2] = [
            u64::from_le_bytes([0, 3, 15, 0xFF, 0, 0, 0, 0]),
            0,
        ];

        // TBL V0, {V1}, V2 (q=false, len=0, op=0)
        exec_simd_tbl(&mut s, false, 0, 1, 2, 0, 0);

        let result_lo = s.v[0][0].to_le_bytes();
        assert_eq!(result_lo[0], 0x10); // index 0 -> 0x10
        assert_eq!(result_lo[1], 0x13); // index 3 -> 0x13
        assert_eq!(result_lo[2], 0x1F); // index 15 -> 0x1F
        assert_eq!(result_lo[3], 0x00); // index 0xFF -> out of range, TBL=0
    }

    #[test]
    fn test_tbx_keeps_dest() {
        let mut s = CpuState::new();
        // V1 = table: all zeros
        s.v[1] = [0, 0];

        // V2 = indices: [0xFF, 0, ...]
        s.v[2] = [0xFF, 0];

        // V0 = destination with known byte pattern
        s.v[0] = [0x42, 0];

        // TBX V0, {V1}, V2 (q=false, len=0, op=1)
        exec_simd_tbl(&mut s, false, 0, 1, 2, 0, 1);

        let result_lo = s.v[0][0].to_le_bytes();
        // index 0xFF is out of range -> TBX keeps destination byte
        assert_eq!(result_lo[0], 0x42);
        // index 0 is in range -> lookup table[0] = 0
        assert_eq!(result_lo[1], 0x00);
    }
}
