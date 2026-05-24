// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/atomic_operations_global_memory.cpp
//!
//! Upstream `TranslatorVisitor::ATOM`/`RED` throw `NotImplementedException`
//! when the full global-atomic IR path isn't wired. We mirror by
//! panicking — the prior silent log-and-return produced incorrect
//! shader output (the destination register kept its previous value)
//! that's worse than a visible crash.

use super::TranslatorVisitor;

/// ATOM — Atomic Operation on Global Memory.
pub fn atom(_tv: &mut TranslatorVisitor<'_>, _insn: u64) {
    panic!("ATOM: global memory atomic not implemented");
}

/// RED — Reduction on Global Memory.
pub fn red(_tv: &mut TranslatorVisitor<'_>, _insn: u64) {
    panic!("RED: global memory reduction not implemented");
}
