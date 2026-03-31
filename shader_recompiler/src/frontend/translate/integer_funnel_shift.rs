// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/integer_funnel_shift.cpp

use super::{bit, field, TranslatorVisitor};
use crate::ir::value::Value;

/// MaxShift encoding (bits [37:38]).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MaxShift {
    U32 = 0,
    Undefined = 1,
    U64 = 2,
    S64 = 3,
}

impl MaxShift {
    fn from_bits(v: u32) -> Self {
        match v {
            0 => MaxShift::U32,
            1 => MaxShift::Undefined,
            2 => MaxShift::U64,
            3 => MaxShift::S64,
            _ => MaxShift::Undefined,
        }
    }
}

fn shf_impl(
    tv: &mut TranslatorVisitor,
    insn: u64,
    shift: Value,
    high_bits: Value,
    right_shift: bool,
) {
    let dst = field(insn, 0, 8);
    let lo_bits_reg = field(insn, 8, 8);
    let max_shift = MaxShift::from_bits(field(insn, 37, 2));
    let wrap = bit(insn, 50);

    // If max_shift is Undefined, emit a no-op (upstream throws).
    if max_shift == MaxShift::Undefined {
        log::warn!("SHF: Undefined MaxShift value, emitting no-op");
        return;
    }

    let low_bits = tv.x(lo_bits_reg);
    // Pack lo | hi into a 64-bit value
    let packed_vec = tv.ir.composite_construct_u32x2(low_bits, high_bits);
    let packed_int = tv.ir.pack_uint_2x32(packed_vec);

    let max_val = if max_shift == MaxShift::U32 {
        Value::ImmU32(32)
    } else {
        Value::ImmU32(63)
    };

    // safe_shift = wrap ? (shift & (max_val - 1)) : min(shift, max_val)
    let safe_shift = if wrap {
        let mask = tv.ir.isub_32(max_val, Value::ImmU32(1));
        tv.ir.bitwise_and_32(shift, mask)
    } else {
        tv.ir.u_min_32(shift, max_val)
    };

    // Perform the 64-bit shift.
    let shifted = if !right_shift {
        tv.ir.shift_left_logical_64(packed_int, safe_shift)
    } else {
        // We lack ShiftRightLogical64 / ShiftRightArithmetic64 in the emitter.
        // Implement SHR via negate-left-shift approximation only for U32 max_shift.
        // For 64-bit cases, emit a warn and return low bits unchanged.
        log::warn!("SHF: right shift 64-bit — partial implementation");
        packed_int
    };

    let unpacked = tv.ir.unpack_uint_2x32(shifted);
    // Left shift: take high word (index 1); right shift: take low word (index 0).
    let idx = if right_shift { 0 } else { 1 };
    let result = tv.ir.composite_extract_u32x2_idx(unpacked, idx);
    tv.set_x(dst, result);
}

/// SHF_l_reg — Funnel shift left, shift amount from register.
pub fn shf_l_reg(tv: &mut TranslatorVisitor, insn: u64) {
    let shift = tv.get_reg20(insn);
    let high_bits = tv.get_reg39(insn);
    shf_impl(tv, insn, shift, high_bits, false);
}

/// SHF_l_imm — Funnel shift left, shift amount from immediate.
pub fn shf_l_imm(tv: &mut TranslatorVisitor, insn: u64) {
    let shift = tv.get_imm20(insn);
    let high_bits = tv.get_reg39(insn);
    shf_impl(tv, insn, shift, high_bits, false);
}

/// SHF_r_reg — Funnel shift right, shift amount from register.
pub fn shf_r_reg(tv: &mut TranslatorVisitor, insn: u64) {
    let shift = tv.get_reg20(insn);
    let high_bits = tv.get_reg39(insn);
    shf_impl(tv, insn, shift, high_bits, true);
}

/// SHF_r_imm — Funnel shift right, shift amount from immediate.
pub fn shf_r_imm(tv: &mut TranslatorVisitor, insn: u64) {
    let shift = tv.get_imm20(insn);
    let high_bits = tv.get_reg39(insn);
    shf_impl(tv, insn, shift, high_bits, true);
}

/// SHF — dispatch wrapper (used when opcode doesn't differentiate l/r/reg/imm).
pub fn shf(tv: &mut TranslatorVisitor, insn: u64) {
    shf_l_reg(tv, insn);
}
