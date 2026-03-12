// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of upstream flow control translate files:
//! - `impl/exit_program.cpp`
//! - `impl/branch_indirect.cpp`
//! - `impl/condition_code_set.cpp`

use super::TranslatorVisitor;

/// EXIT - Exit the shader program.
pub fn exit_program(_v: &mut TranslatorVisitor<'_>, _insn: u64) {
    todo!("EXIT: exit the shader program")
}

/// BRX - Branch indirect (using a register or table).
pub fn brx(_v: &mut TranslatorVisitor<'_>, _insn: u64) {
    todo!("BRX: indirect branch")
}

/// CSET - Condition code set (set register from CC).
pub fn cset(_v: &mut TranslatorVisitor<'_>, _insn: u64) {
    todo!("CSET: condition code set")
}

/// CSETP - Condition code set predicate.
pub fn csetp(_v: &mut TranslatorVisitor<'_>, _insn: u64) {
    todo!("CSETP: condition code set predicate")
}
