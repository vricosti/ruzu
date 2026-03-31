// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/atomic_operations_global_memory.cpp
//!
//! Implements ATOM and RED (atomic read-modify-write on global memory).

use super::TranslatorVisitor;

/// ATOM — Atomic Operation on Global Memory.
///
/// Not yet implemented: requires global atomic IR opcodes (GlobalAtomicIAdd, etc.).
/// Emits a no-op result and logs a warning.
pub fn atom(tv: &mut TranslatorVisitor<'_>, insn: u64) {
    log::warn!(
        "ATOM: global memory atomic IR not yet ported — emitting no-op (insn={:#018x})",
        insn
    );
    let dst = tv.dst_reg(insn);
    tv.set_x(dst, crate::ir::value::Value::ImmU32(0));
}

/// RED — Reduction on Global Memory.
///
/// Not yet implemented: requires global atomic IR opcodes.
/// Emits a no-op and logs a warning.
pub fn red(_tv: &mut TranslatorVisitor<'_>, insn: u64) {
    log::warn!(
        "RED: global memory reduction IR not yet ported — emitting no-op (insn={:#018x})",
        insn
    );
}
