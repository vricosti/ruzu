// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/surface_load_store.cpp
//!
//! Implements SULD and SUST (surface load and surface store).

use super::TranslatorVisitor;

/// SULD — Surface Load.
///
/// Loads texel data from a surface (image) at given integer coordinates.
///
/// Not yet implemented: requires surface/image IR opcodes (ImageRead, etc.).
/// Emits a no-op and logs a warning.
pub fn suld(tv: &mut TranslatorVisitor, insn: u64) {
    let _ = insn;
    log::warn!("SULD: surface load IR not yet ported — emitting no-op (insn={:#018x})", insn);
    let dst = tv.dst_reg(insn);
    tv.set_x(dst, crate::ir::value::Value::ImmU32(0));
}

/// SUST — Surface Store.
///
/// Stores texel data into a surface (image) at given integer coordinates.
///
/// Not yet implemented: requires surface/image IR opcodes (ImageWrite, etc.).
/// Emits a no-op and logs a warning.
pub fn sust(_tv: &mut TranslatorVisitor, insn: u64) {
    log::warn!("SUST: surface store IR not yet ported — emitting no-op (insn={:#018x})", insn);
}
