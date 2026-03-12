// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of upstream surface operation translate files:
//! - `impl/surface_atomic_operations.cpp`
//! - `impl/surface_load_store.cpp`

use super::TranslatorVisitor;

/// SUATOM - Surface atomic operation.
pub fn suatom(_v: &mut TranslatorVisitor<'_>, _insn: u64) {
    todo!("SUATOM: surface atomic operation")
}

/// SULD - Surface load.
pub fn suld(_v: &mut TranslatorVisitor<'_>, _insn: u64) {
    todo!("SULD: surface load")
}

/// SUST - Surface store.
pub fn sust(_v: &mut TranslatorVisitor<'_>, _insn: u64) {
    todo!("SUST: surface store")
}
