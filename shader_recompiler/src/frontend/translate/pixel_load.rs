// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! PIXLD instruction translation — maps to zuyu's
//! `frontend/maxwell/translate/impl/pixel_load.cpp`.
//!
//! Handles pixel-level loads for fragment shader helper/coverage queries.

use super::{field, TranslatorVisitor};

impl<'a> TranslatorVisitor<'a> {
    /// Translate the PIXLD instruction.
    ///
    /// Matches upstream `TranslatorVisitor::PIXLD(u64)`.
    pub fn translate_pixld(&mut self, insn: u64) {
        let dst = self.dst_reg(insn);
        let addr_reg = field(insn, 8, 8);
        let addr_offset = field(insn, 20, 8);
        let mode = field(insn, 31, 3);
        let dest_pred = field(insn, 45, 3);

        if dest_pred != 7 {
            panic!("PIXLD destination predicate not implemented");
        }
        if addr_reg != 255 || addr_offset != 0 {
            panic!("PIXLD non-zero source register not implemented");
        }
        if mode != 5 {
            panic!("PIXLD mode {mode} not implemented");
        }
        let sample_id = self.ir.sample_id();
        self.set_x(dst, sample_id);
    }
}
