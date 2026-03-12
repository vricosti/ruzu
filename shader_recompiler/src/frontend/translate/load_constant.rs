// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! LDC instruction translation — maps to zuyu's
//! `frontend/maxwell/translate/impl/load_constant.cpp` and
//! `load_constant.h`.
//!
//! Handles the LDC instruction for loading from constant buffers with
//! various addressing modes.

use super::{field, TranslatorVisitor};
use crate::ir::value::Value;

/// LDC addressing mode.
#[derive(Debug, Clone, Copy)]
enum LdcMode {
    /// Indexed load: base[offset]
    Indexed,
    /// Indexed load with segment
    IndexedSegmented,
}

impl<'a> TranslatorVisitor<'a> {
    /// Translate the LDC instruction (already handled in memory.rs).
    ///
    /// Matches upstream `TranslatorVisitor::LDC(u64)`.
    /// This provides the upstream-matching file location; the actual
    /// implementation delegates to the memory module's ldc handler.
    pub fn translate_ldc(&mut self, insn: u64) {
        super::memory::ldc(self, insn);
    }
}
