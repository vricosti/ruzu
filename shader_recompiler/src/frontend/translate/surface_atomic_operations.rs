// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/surface_atomic_operations.cpp
//!
//! Upstream `TranslatorVisitor::SUATOM`/`SURED` throw `NotImplementedException`
//! until the full surface (storage-image) atomic path is wired. Ruzu's
//! GLSL backend has `ImageAtomic*` emit handlers but the translation
//! path from SUATOM/SURED → `ImageAtomic*` IR opcodes isn't wired yet.
//! Panic to match upstream's "compilation aborts" behaviour.

use super::TranslatorVisitor;

/// SUATOM — Surface Atomic Operation.
pub fn suatom(_tv: &mut TranslatorVisitor, _insn: u64) {
    panic!("SUATOM: surface atomic not implemented");
}

/// SURED — Surface Reduction.
pub fn sured(_tv: &mut TranslatorVisitor, _insn: u64) {
    panic!("SURED: surface reduction not implemented");
}
