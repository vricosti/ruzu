// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of upstream atomic operation translate files:
//! - `impl/atomic_operations_global_memory.cpp`
//! - `impl/atomic_operations_shared_memory.cpp`

use super::TranslatorVisitor;

/// ATOM - Atomic operation on global memory.
pub fn atom(_v: &mut TranslatorVisitor<'_>, _insn: u64) {
    todo!("ATOM: atomic operation on global memory")
}

/// ATOM_CAS - Atomic compare-and-swap on global memory.
pub fn atom_cas(_v: &mut TranslatorVisitor<'_>, _insn: u64) {
    todo!("ATOM_CAS: atomic compare-and-swap on global memory")
}

/// ATOMS - Atomic operation on shared memory.
pub fn atoms(_v: &mut TranslatorVisitor<'_>, _insn: u64) {
    todo!("ATOMS: atomic operation on shared memory")
}

/// ATOMS_CAS - Atomic compare-and-swap on shared memory.
pub fn atoms_cas(_v: &mut TranslatorVisitor<'_>, _insn: u64) {
    todo!("ATOMS_CAS: atomic compare-and-swap on shared memory")
}
