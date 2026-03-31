// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/texture_gather_swizzled.cpp
//!
//! Implements TLD4S (texture gather swizzled, compact dual-destination encoding).

use super::TranslatorVisitor;

/// TLD4S — Texture Gather Swizzled.
///
/// Not yet implemented: requires the TEXS-style compact dual-destination texture
/// infrastructure.  Emits a no-op and logs a warning.
pub fn tld4s(tv: &mut TranslatorVisitor, insn: u64) {
    log::warn!(
        "TLD4S: texture gather swizzled not yet implemented — emitting no-op (insn={:#018x})",
        insn
    );
    let dst = tv.dst_reg(insn);
    tv.set_x(dst, crate::ir::value::Value::ImmU32(0));
}
