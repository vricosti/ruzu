// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/surface_load_store.cpp
//!
//! Upstream `SULD`/`SUST` translation throws `NotImplementedException`
//! until the full storage-image dispatch is wired. The GLSL backend
//! has `ImageRead`/`ImageWrite` emit handlers (see `emit_glsl_image.rs`)
//! but the SULD/SUST → `ImageRead`/`ImageWrite` IR generation path is
//! not yet bridged. Panic instead of silently storing 0 in the dst.

use super::TranslatorVisitor;

/// SULD — Surface Load.
pub fn suld(_tv: &mut TranslatorVisitor, _insn: u64) {
    panic!("SULD: surface load not implemented");
}

/// SUST — Surface Store.
pub fn sust(_tv: &mut TranslatorVisitor, _insn: u64) {
    panic!("SUST: surface store not implemented");
}
