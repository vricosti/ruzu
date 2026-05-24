// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/texture_gather_swizzled.cpp
//!
//! Upstream `TLD4S` translates into `ImageGather`. Ruzu has the emit
//! handler but the SASS decode → `ImageGather` IR generation isn't
//! bridged yet. Panic instead of silently storing 0.

use super::TranslatorVisitor;

/// TLD4S — Texture Gather Swizzled.
pub fn tld4s(_tv: &mut TranslatorVisitor, _insn: u64) {
    panic!("TLD4S: texture gather swizzled not implemented");
}
