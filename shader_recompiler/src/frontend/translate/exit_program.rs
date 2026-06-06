// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! EXIT instruction translation — maps to zuyu's
//! `frontend/maxwell/translate/impl/exit_program.cpp`.
//!
//! Handles the EXIT instruction which terminates shader execution and writes
//! fragment shader outputs (render targets, sample mask, depth).

use super::TranslatorVisitor;
use crate::ir::types::ShaderStage;
use crate::ir::value::Value;

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
        let Some(sph) = self.sph.clone() else {
            for component in 0..4 {
                let value = self.f(component);
                self.ir
                    .set_frag_color(Value::ImmU32(0), Value::ImmU32(component), value);
            }
            return;
        };

        let mut src_reg = 0;
        for render_target in 0..8 {
            if !sph.ps_has_output_components(render_target) {
                continue;
            }
            let mask = sph.ps_enabled_output_components(render_target);
            for (component, enabled) in mask.into_iter().enumerate() {
                if enabled {
                    let value = self.f(src_reg);
                    self.ir.set_frag_color(
                        Value::ImmU32(render_target),
                        Value::ImmU32(component as u32),
                        value,
                    );
                }
                src_reg += 1;
            }
        }
        if sph.ps_omap_sample_mask() {
            let value = self.x(src_reg);
            self.ir.set_sample_mask(value);
        }
        if sph.ps_omap_depth() {
            let value = self.f(src_reg + 1);
            self.ir.set_frag_depth(value);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::opcodes::Opcode;
    use crate::ir::program::Program;
    use crate::program_header::ProgramHeader;

    #[test]
    fn exit_fragment_uses_ps_omap_outputs_sample_mask_and_depth() {
        let mut program = Program::new(ShaderStage::Fragment);
        let block = program.add_block();
        let mut sph = ProgramHeader::default();
        sph.raw[18] = 0b0101 | (0b0010 << 4);
        sph.raw[19] = 0b11;

        {
            let mut visitor = TranslatorVisitor::new_with_sph(&mut program, block, Some(sph));
            visitor.translate_exit(0);
        }

        let opcodes: Vec<_> = program
            .blocks
            .iter()
            .flat_map(|block| block.iter().map(|inst| inst.opcode))
            .filter(|opcode| {
                matches!(
                    opcode,
                    Opcode::SetFragColor | Opcode::SetSampleMask | Opcode::SetFragDepth
                )
            })
            .collect();
        assert_eq!(
            opcodes,
            vec![
                Opcode::SetFragColor,
                Opcode::SetFragColor,
                Opcode::SetFragColor,
                Opcode::SetSampleMask,
                Opcode::SetFragDepth,
            ]
        );
    }
}
