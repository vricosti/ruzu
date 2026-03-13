// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/surface_atomic_operations.cpp
//!
//! Implements SUATOM and SURED (surface atomic operations).

use super::TranslatorVisitor;

/// SUATOM — Surface Atomic Operation.
///
/// Not yet implemented: requires surface atomic IR opcodes (ImageAtomicIAdd, etc.).
/// Emits a no-op and logs a warning.
pub fn suatom(tv: &mut TranslatorVisitor, insn: u64) {
    log::warn!("SUATOM: surface atomic IR not yet ported — emitting no-op (insn={:#018x})", insn);
    let dst = tv.dst_reg(insn);
    tv.set_x(dst, crate::ir::value::Value::ImmU32(0));
}

/// SURED — Surface Reduction.
///
/// Not yet implemented: requires surface atomic IR opcodes.
/// Emits a no-op and logs a warning.
pub fn sured(_tv: &mut TranslatorVisitor, insn: u64) {
    log::warn!("SURED: surface reduction IR not yet ported — emitting no-op (insn={:#018x})", insn);
}
