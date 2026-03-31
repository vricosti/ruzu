// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/atomic_operations_shared_memory.cpp
//!
//! Implements ATOMS (atomic read-modify-write on shared memory).

use super::TranslatorVisitor;

/// ATOMS — Atomic Operation on Shared Memory.
///
/// Not yet implemented: requires shared atomic IR opcodes (SharedAtomicIAdd, etc.).
/// Emits a no-op result and logs a warning.
pub fn atoms(tv: &mut TranslatorVisitor<'_>, insn: u64) {
    log::warn!(
        "ATOMS: shared memory atomic IR not yet ported — emitting no-op (insn={:#018x})",
        insn
    );
    let dst = tv.dst_reg(insn);
    tv.set_x(dst, crate::ir::value::Value::ImmU32(0));
}

/// ATOMS_CAS — Atomic Compare-and-Swap on Shared Memory.
///
/// Not yet implemented: requires shared atomic IR opcodes.
/// Emits a no-op result and logs a warning.
pub fn atoms_cas(tv: &mut TranslatorVisitor<'_>, insn: u64) {
    log::warn!(
        "ATOMS_CAS: shared memory atomic CAS IR not yet ported — emitting no-op (insn={:#018x})",
        insn
    );
    let dst = tv.dst_reg(insn);
    tv.set_x(dst, crate::ir::value::Value::ImmU32(0));
}
