// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! OUT instruction translation — maps to zuyu's
//! `frontend/maxwell/translate/impl/output_geometry.cpp`.
//!
//! Handles the OUT instruction for geometry shader output (emit vertex,
//! end primitive).

use super::{bit, field, TranslatorVisitor};
use crate::ir::value::Value;

impl<'a> TranslatorVisitor<'a> {
    /// Translate the OUT instruction (geometry shader emit/cut).
    ///
    /// Matches upstream `TranslatorVisitor::OUT(u64)`.
    ///
    /// Encoding:
    /// - bit 0:    emit (1 = EmitVertex)
    /// - bit 1:    cut (1 = EndPrimitive)
    /// - bits [7:2]: stream index
    pub fn translate_out(&mut self, insn: u64) {
        let do_emit = bit(insn, 0);
        let do_cut = bit(insn, 1);
        let stream = field(insn, 2, 6);

        if do_emit {
            self.ir.emit_vertex(Value::ImmU32(stream));
        }
        if do_cut {
            self.ir.end_primitive(Value::ImmU32(stream));
        }
    }
}
