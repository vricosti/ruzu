// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/texture_mipmap_level.cpp
//!
//! Implements TMML and TMML_b (texture mipmap level query).

use super::TranslatorVisitor;

/// TMML — Texture Mipmap Level (bound form).
///
/// Not yet implemented: requires ImageQueryLod IR opcode.
/// Emits a no-op and logs a warning.
pub fn tmml(tv: &mut TranslatorVisitor, insn: u64) {
    log::warn!("TMML: texture mipmap level query not yet implemented — emitting no-op (insn={:#018x})", insn);
    let dst = tv.dst_reg(insn);
    tv.set_x(dst, crate::ir::value::Value::ImmU32(0));
}

/// TMML_b — Texture Mipmap Level (bindless form).
///
/// Not yet implemented: requires ImageQueryLod IR opcode.
/// Emits a no-op and logs a warning.
pub fn tmml_b(tv: &mut TranslatorVisitor, insn: u64) {
    log::warn!("TMML_b: texture mipmap level query (bindless) not yet implemented — emitting no-op (insn={:#018x})", insn);
    let dst = tv.dst_reg(insn);
    tv.set_x(dst, crate::ir::value::Value::ImmU32(0));
}
