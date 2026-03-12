// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `frontend/maxwell/decode.h` and `frontend/maxwell/decode.cpp`
//!
//! Maxwell instruction decoder. Maps raw 64-bit instruction words to
//! opcode enum values. The existing `maxwell_opcodes.rs` already contains
//! the decoder logic; this module provides the upstream-matching function
//! signature.

use super::maxwell_opcodes::{decode_opcode, MaxwellOpcode};

/// Decode a Maxwell instruction into its opcode.
///
/// Returns `None` for unrecognized instructions.
pub fn decode(insn: u64) -> Option<MaxwellOpcode> {
    decode_opcode(insn)
}
