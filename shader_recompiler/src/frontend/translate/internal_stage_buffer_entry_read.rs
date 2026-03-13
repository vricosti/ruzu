// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! ISBERD instruction translation — maps to zuyu's
//! `frontend/maxwell/translate/impl/internal_stage_buffer_entry_read.cpp`.
//!
//! Handles the ISBERD instruction for reading internal stage buffer entries.

use super::{field, TranslatorVisitor};
use crate::ir::value::Value;

impl<'a> TranslatorVisitor<'a> {
    /// Translate the ISBERD instruction.
    ///
    /// Matches upstream `TranslatorVisitor::ISBERD(u64)`.
    ///
    /// This instruction reads an entry from the internal stage buffer.
    /// It is primarily used in geometry shaders for input vertex access.
    pub fn translate_isberd(&mut self, insn: u64) {
        let dst = self.dst_reg(insn);
        let src = self.src_a_reg(insn);
        // ISBERD effectively passes through the register value
        // with some internal stage buffer indexing
        let val = self.x(src);
        self.set_x(dst, val);
    }
}
