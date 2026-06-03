// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/move_predicate_to_register.cpp
//!
//! Upstream `TranslatorVisitor::P2R_*` all throw `NotImplementedException`
//! (P2R is rarely-used). Ruzu mirrors by panicking — silent no-op
//! produces incorrect values in the destination register, which is
//! harder to debug than a crash.

use super::TranslatorVisitor;

/// P2R (reg) — Not implemented upstream.
pub fn p2r_reg(_tv: &mut TranslatorVisitor, _insn: u64) {
    panic!("P2R_reg not implemented (upstream NotImplementedException)");
}

/// P2R (cbuf) — Not implemented upstream.
pub fn p2r_cbuf(_tv: &mut TranslatorVisitor, _insn: u64) {
    panic!("P2R_cbuf not implemented (upstream NotImplementedException)");
}

/// P2R (imm) — Not implemented upstream.
pub fn p2r_imm(_tv: &mut TranslatorVisitor, _insn: u64) {
    panic!("P2R_imm not implemented (upstream NotImplementedException)");
}
