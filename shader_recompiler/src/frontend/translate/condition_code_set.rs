// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/condition_code_set.cpp
//!
//! CSET and CSETP test the Maxwell condition code flags (Z, S, C, O) via a
//! flow-test selector. Full implementation requires the condition code flag
//! infrastructure (GetZFlag, GetSFlag, …) which is not yet ported to IR.
//! Upstream's translation throws `NotImplementedException` for the
//! flow-test path when the IR doesn't support it; we panic to match
//! instead of silently emitting wrong results.

use super::TranslatorVisitor;

/// CSET — Condition code set register.
pub fn cset(_tv: &mut TranslatorVisitor, _insn: u64) {
    panic!("CSET: condition code IR not implemented");
}

/// CSETP — Condition code set predicate.
pub fn csetp(_tv: &mut TranslatorVisitor, _insn: u64) {
    panic!("CSETP: condition code IR not implemented");
}
