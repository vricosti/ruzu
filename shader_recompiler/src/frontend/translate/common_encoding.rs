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
    field(insn, 20, 32)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn decode_imm32_matches_upstream_bitfield_20_32() {
        let insn = 0xFEDC_BA98u64 << 20;
        assert_eq!(decode_imm32(insn), 0xFEDC_BA98);
    }

    #[test]
    fn decode_imm32_ignores_low_instruction_bits() {
        let insn = (0x1234_5678u64 << 20) | 0xFFFFF;
        assert_eq!(decode_imm32(insn), 0x1234_5678);
    }
}
