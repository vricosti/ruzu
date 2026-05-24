// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `frontend/maxwell/translate/impl/common_encoding.h`
//!
//! Common instruction encoding helpers shared across all translation files.

use super::{bit, field, sfield};
use crate::ir::types::{FmzMode, FpRounding};

/// Maxwell floating-point rounding mode (2-bit field in the instruction).
///
/// Port of upstream `Shader::Maxwell::FpRounding` (common_encoding.h:13).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum MaxwellFpRounding {
    Rn = 0,
    Rm = 1,
    Rp = 2,
    Rz = 3,
}

impl MaxwellFpRounding {
    /// Decode a 2-bit field value into a `MaxwellFpRounding`. Values 0..3 map
    /// 1:1 to upstream's `BitField<*, 2, FpRounding>`; values outside that
    /// range cannot be produced by a 2-bit field but are rejected here to
    /// mirror upstream's exhaustive switch.
    pub fn from_field(value: u32) -> Self {
        match value & 0x3 {
            0 => Self::Rn,
            1 => Self::Rm,
            2 => Self::Rp,
            3 => Self::Rz,
            _ => unreachable!("masked to 2 bits"),
        }
    }
}

/// Maxwell flush-to-zero / flush-multiply-to-zero mode (2-bit field).
///
/// Port of upstream `Shader::Maxwell::FmzMode` (common_encoding.h:20).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum MaxwellFmzMode {
    None = 0,
    Ftz = 1,
    Fmz = 2,
    InvalidFmz3 = 3,
}

/// Port of upstream `CastFpRounding(FpRounding)` (common_encoding.h:27).
///
/// Maps the 2-bit Maxwell rounding-mode field directly to the IR rounding
/// enum used by `FpControl`.
pub fn cast_fp_rounding(rounding: MaxwellFpRounding) -> FpRounding {
    match rounding {
        MaxwellFpRounding::Rn => FpRounding::RN,
        MaxwellFpRounding::Rm => FpRounding::RM,
        MaxwellFpRounding::Rp => FpRounding::RP,
        MaxwellFpRounding::Rz => FpRounding::RZ,
    }
}

/// Port of upstream `CastFmzMode(FmzMode)` (common_encoding.h:43).
///
/// Note: upstream maps `FmzMode::FMZ` to `IR::FmzMode::FTZ` because FMZ is
/// handled manually at instruction level. The `INVALIDFMZ3` case panics,
/// matching upstream's `throw NotImplementedException("Invalid FMZ mode")`.
pub fn cast_fmz_mode(mode: MaxwellFmzMode) -> FmzMode {
    match mode {
        MaxwellFmzMode::None => FmzMode::None,
        MaxwellFmzMode::Ftz => FmzMode::FTZ,
        MaxwellFmzMode::Fmz => FmzMode::FTZ,
        MaxwellFmzMode::InvalidFmz3 => panic!("Invalid FMZ mode INVALIDFMZ3"),
    }
}

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
