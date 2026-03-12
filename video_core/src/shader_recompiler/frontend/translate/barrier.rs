// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `frontend/maxwell/translate/impl/barrier_operations.cpp`

use super::TranslatorVisitor;

/// BAR - Barrier synchronization.
pub fn bar(_v: &mut TranslatorVisitor<'_>, _insn: u64) {
    todo!("BAR: barrier synchronization")
}
