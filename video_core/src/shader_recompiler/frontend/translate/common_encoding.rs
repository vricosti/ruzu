// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `frontend/maxwell/translate/impl/common_encoding.h`
//!
//! Common instruction encoding helpers shared across all translation files.

use super::{bit, field, sfield};

/// Decode the CC (condition code) field from an instruction.
pub fn decode_cc(insn: u64) -> u32 {
    field(insn, 47, 1)
}

/// Decode the saturate flag.
pub fn decode_saturate(insn: u64, pos: u32) -> bool {
    bit(insn, pos)
}

/// Decode a 20-bit immediate with optional negation.
pub fn decode_imm20(insn: u64) -> i32 {
    sfield(insn, 20, 20)
}

/// Decode a 32-bit immediate (used by 32I-type instructions).
pub fn decode_imm32(insn: u64) -> u32 {
    (insn & 0xFFFF_FFFF) as u32
}
