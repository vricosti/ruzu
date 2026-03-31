// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! EXIT instruction translation — maps to zuyu's
//! `frontend/maxwell/translate/impl/exit_program.cpp`.
//!
//! Handles the EXIT instruction which terminates shader execution and writes
//! fragment shader outputs (render targets, sample mask, depth).

use super::TranslatorVisitor;
use crate::ir::types::ShaderStage;
use crate::ir::value::{Reg, Value};

impl<'a> TranslatorVisitor<'a> {
    /// Translate the EXIT instruction.
    ///
    /// Matches upstream `TranslatorVisitor::EXIT()`.
    /// For fragment shaders, writes render target outputs before exiting.
    pub fn translate_exit(&mut self, _insn: u64) {
        match self.stage {
            ShaderStage::Fragment => {
                self.exit_fragment();
            }
            _ => {
                // For non-fragment shaders, EXIT is handled by the CFG builder.
            }
        }
    }

    /// Write fragment shader outputs.
    ///
    /// Matches upstream anonymous `ExitFragment(TranslatorVisitor&)`.
    /// Reads the shader program header to determine which render targets are
    /// enabled and writes their color values, plus optional sample mask and depth.
    fn exit_fragment(&mut self) {
        // In the full implementation, this reads the ProgramHeader to determine
        // which render target components are enabled, then iterates over them
        // writing SetFragColor for each component.
        //
        // For now, write a default single-RT output from R0..R3.
        let r0 = self.f(0);
        let r1 = self.f(1);
        let r2 = self.f(2);
        let r3 = self.f(3);
        self.ir
            .set_frag_color(Value::ImmU32(0), Value::ImmU32(0), r0);
        self.ir
            .set_frag_color(Value::ImmU32(0), Value::ImmU32(1), r1);
        self.ir
            .set_frag_color(Value::ImmU32(0), Value::ImmU32(2), r2);
        self.ir
            .set_frag_color(Value::ImmU32(0), Value::ImmU32(3), r3);
    }
}
