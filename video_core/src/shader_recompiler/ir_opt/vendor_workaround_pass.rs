// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `ir_opt/vendor_workaround_pass.cpp`
//!
//! Applies vendor-specific workarounds to the IR. Currently handles
//! an NVIDIA bug in Super Mario RPG where byte swap patterns using
//! IAdd32 need to be replaced with BitwiseOr32.

use crate::shader_recompiler::ir::opcodes::Opcode;
use crate::shader_recompiler::ir::program::Program;

/// Apply vendor-specific workarounds to the IR.
pub fn vendor_workaround_pass(program: &mut Program) {
    for block in &mut program.blocks {
        for inst in &mut block.instructions {
            match inst.opcode {
                Opcode::IAdd32 => {
                    adding_byte_swaps_workaround(inst);
                }
                _ => {}
            }
        }
    }
}

/// Workaround for an NVIDIA bug where byte swap patterns using IAdd32
/// should use BitwiseOr32 instead.
///
/// Pattern:
///   %lhs_bfe = BitFieldUExtract %factor_a, #0, #16
///   %lhs_mul = IMul32 %lhs_bfe, %factor_b
///   %lhs_shl = ShiftLeftLogical32 %lhs_mul, #16
///   %rhs_bfe = BitFieldUExtract %factor_a, #16, #16
///   %result  = IAdd32 %lhs_shl, %rhs_bfe
///
/// Replacement: %result = BitwiseOr32 %lhs_shl, %rhs_bfe
fn adding_byte_swaps_workaround(_inst: &mut crate::shader_recompiler::ir::instruction::Inst) {
    // TODO: Implement pattern matching for byte swap workaround.
    // This requires traversing the instruction graph to check argument
    // patterns, which needs the full SSA graph traversal infrastructure.
}
