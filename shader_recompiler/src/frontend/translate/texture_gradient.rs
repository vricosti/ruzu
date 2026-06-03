// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/texture_gradient.cpp
//!
//! Upstream `TXD`/`TXD_b` translate into `ImageGradient` IR via the
//! texture-pass; the GLSL backend has an `emit_image_gradient_inst`
//! handler (see `emit_glsl_image.rs`) but the SASS-decode path that
//! routes TXD → `ImageGradient` is not yet bridged. Panic to match
//! upstream's `throw NotImplementedException("TXD")`.

use super::TranslatorVisitor;

/// TXD — Texture Gradient (bound form).
pub fn txd(_tv: &mut TranslatorVisitor, _insn: u64) {
    panic!("TXD: texture gradient fetch not implemented");
}

/// TXD_b — Texture Gradient (bindless form).
pub fn txd_b(_tv: &mut TranslatorVisitor, _insn: u64) {
    panic!("TXD_b: texture gradient fetch (bindless) not implemented");
}
