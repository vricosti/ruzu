// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of upstream warp-level instruction translate files:
//! - `impl/vote.cpp`
//! - `impl/warp_shuffle.cpp`

use super::TranslatorVisitor;

/// VOTE - Warp vote operations (ALL, ANY, EQ).
pub fn vote(_v: &mut TranslatorVisitor<'_>, _insn: u64) {
    todo!("VOTE: warp vote operations")
}

/// SHFL - Warp shuffle.
pub fn shfl(_v: &mut TranslatorVisitor<'_>, _insn: u64) {
    todo!("SHFL: warp shuffle")
}
