// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! PIXLD instruction translation — maps to zuyu's
//! `frontend/maxwell/translate/impl/pixel_load.cpp`.
//!
//! Handles pixel-level loads for fragment shader helper/coverage queries.

use super::{field, TranslatorVisitor};
use crate::ir::value::Value;

/// PIXLD mode, matching upstream pixel load modes.
#[repr(u32)]
#[derive(Debug, Clone, Copy)]
enum PixelLoadMode {
    CoverageMask = 1,
    CoveredThreadCount = 2,
    CoveredThreadIndex = 3,
    InnerCoverage = 4,
    StencilValue = 5,
}

impl<'a> TranslatorVisitor<'a> {
    /// Translate the PIXLD instruction.
    ///
    /// Matches upstream `TranslatorVisitor::PIXLD(u64)`.
    pub fn translate_pixld(&mut self, insn: u64) {
        let dst = self.dst_reg(insn);
        let mode_raw = field(insn, 8, 3);

        match mode_raw {
            1 => {
                // CoverageMask — returns sample mask
                let val = self.ir.sample_id();
                self.set_x(dst, val);
            }
            4 => {
                // InnerCoverage — returns whether this is a helper invocation
                let helper = self.ir.is_helper_invocation();
                let not_helper = self.ir.logical_not(helper);
                let result =
                    self.ir
                        .select_u32(not_helper, Value::ImmU32(0xFFFFFFFF), Value::ImmU32(0));
                self.set_x(dst, result);
            }
            _ => {
                log::warn!("PIXLD mode {} not implemented, writing zero", mode_raw);
                self.set_x(dst, Value::ImmU32(0));
            }
        }
    }
}
