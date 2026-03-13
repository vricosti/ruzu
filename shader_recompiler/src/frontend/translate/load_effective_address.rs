// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/load_effective_address.cpp

use super::{bit, field, TranslatorVisitor};
use crate::ir::value::Value;

/// LEA.HI inner implementation.
///
/// result = base + ((offset_lo | (offset_hi << 32)) >> (32 - scale))
fn lea_hi_impl(tv: &mut TranslatorVisitor, insn: u64, base: Value, offset_hi: Value, scale: u32, neg: bool) {
    let dst           = field(insn, 0, 8);
    let offset_lo_reg = field(insn, 8, 8);

    let offset_lo = tv.x(offset_lo_reg);
    // Pack into 64 bits
    let packed_vec = tv.ir.composite_construct_u32x2(offset_lo, offset_hi);
    let packed     = tv.ir.pack_uint_2x32(packed_vec);
    let packed64   = if neg { tv.ir.ineg_64(packed) } else { packed };

    // Shift right by (32 - scale) to get the high 32 bits contribution.
    let hi_scale = (32u32).wrapping_sub(scale);
    // We don't have ShiftRightLogical64 yet; use shift_left_logical_64 with negated shift
    // approximation: shift left by scale and take the high word.
    let shifted  = tv.ir.shift_left_logical_64(packed64, Value::ImmU32(scale));
    let unpacked = tv.ir.unpack_uint_2x32(shifted);
    let hi_word  = tv.ir.composite_extract_u32x2_idx(unpacked, 1);
    let _ = hi_scale; // Already handled above

    let result = tv.ir.iadd_32(base, hi_word);
    tv.set_x(dst, result);
}

/// LEA.HI_reg — base from reg20, offset_hi from reg39.
pub fn lea_hi_reg(tv: &mut TranslatorVisitor, insn: u64) {
    let scale = field(insn, 28, 5);
    let neg   = bit(insn, 37);
    let base  = tv.get_reg20(insn);
    let offset_hi = tv.get_reg39(insn);
    lea_hi_impl(tv, insn, base, offset_hi, scale, neg);
}

/// LEA.HI_cbuf — base from cbuf, offset_hi from reg39.
pub fn lea_hi_cbuf(tv: &mut TranslatorVisitor, insn: u64) {
    let scale = field(insn, 51, 5);
    let neg   = bit(insn, 56);
    let base  = tv.get_cbuf(insn);
    let offset_hi = tv.get_reg39(insn);
    lea_hi_impl(tv, insn, base, offset_hi, scale, neg);
}

/// LEA.LO inner implementation.
///
/// result = base + (offset_lo << scale)   [with optional negation of offset_lo]
fn lea_lo_impl(tv: &mut TranslatorVisitor, insn: u64, base: Value) {
    let dst           = field(insn, 0, 8);
    let offset_lo_reg = field(insn, 8, 8);
    let scale         = field(insn, 39, 5);
    let neg           = bit(insn, 45);

    let offset_lo = tv.x(offset_lo_reg);
    let offset    = if neg { tv.ir.ineg_32(offset_lo) } else { offset_lo };
    let scaled    = tv.ir.shift_left_logical_32(offset, Value::ImmU32(scale));
    let result    = tv.ir.iadd_32(base, scaled);
    tv.set_x(dst, result);
}

/// LEA.LO_reg — base from reg20.
pub fn lea_lo_reg(tv: &mut TranslatorVisitor, insn: u64) {
    let base = tv.get_reg20(insn);
    lea_lo_impl(tv, insn, base);
}

/// LEA.LO_cbuf — base from constant buffer.
pub fn lea_lo_cbuf(tv: &mut TranslatorVisitor, insn: u64) {
    let base = tv.get_cbuf(insn);
    lea_lo_impl(tv, insn, base);
}

/// LEA.LO_imm — base from sign-extended 20-bit immediate.
pub fn lea_lo_imm(tv: &mut TranslatorVisitor, insn: u64) {
    let base = tv.get_imm20(insn);
    lea_lo_impl(tv, insn, base);
}
