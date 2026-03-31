// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/texture_gradient.cpp
//!
//! Implements TXD and TXD_b (texture gradient — explicit derivative sampling).

use super::TranslatorVisitor;

/// TXD — Texture Gradient (bound form).
///
/// Not yet implemented: requires explicit-derivative texture IR opcode.
/// Emits a no-op and logs a warning.
pub fn txd(tv: &mut TranslatorVisitor, insn: u64) {
    log::warn!(
        "TXD: texture gradient fetch not yet implemented — emitting no-op (insn={:#018x})",
        insn
    );
    let dst = tv.dst_reg(insn);
    tv.set_x(dst, crate::ir::value::Value::ImmU32(0));
}

/// TXD_b — Texture Gradient (bindless form).
///
/// Not yet implemented: requires explicit-derivative texture IR opcode.
/// Emits a no-op and logs a warning.
pub fn txd_b(tv: &mut TranslatorVisitor, insn: u64) {
    log::warn!("TXD_b: texture gradient fetch (bindless) not yet implemented — emitting no-op (insn={:#018x})", insn);
    let dst = tv.dst_reg(insn);
    tv.set_x(dst, crate::ir::value::Value::ImmU32(0));
}
