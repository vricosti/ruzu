// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `ir_opt/vendor_workaround_pass.cpp`
//!
//! Applies vendor-specific workarounds to the IR. Currently handles
//! an NVIDIA bug in Super Mario RPG where byte swap patterns using
//! IAdd32 need to be replaced with BitwiseOr32.

use crate::ir::instruction::Inst;
use crate::ir::opcodes::Opcode;
use crate::ir::program::Program;
use crate::ir::value::Value;

/// Apply vendor-specific workarounds to the IR.
///
/// Upstream: `VendorWorkaroundPass` (vendor_workaround_pass.cpp:65-77).
pub fn vendor_workaround_pass(program: &mut Program) {
    // Collect indices of IAdd32 instructions to patch.
    // We collect first to avoid borrow issues when mutating.
    let mut patches: Vec<(usize, usize)> = Vec::new();

    for (block_idx, block) in program.blocks.iter().enumerate() {
        for (inst_idx, inst) in block.instructions.iter().enumerate() {
            if inst.opcode == Opcode::IAdd32 {
                if should_replace_with_or(program, inst) {
                    patches.push((block_idx, inst_idx));
                }
            }
        }
    }

    for (block_idx, inst_idx) in patches {
        program.blocks[block_idx].instructions[inst_idx].opcode = Opcode::BitwiseOr32;
    }
}

/// Resolve an instruction Value through Identity chains.
/// Upstream: `Value::TryInstRecursive()`.
/// Returns the final non-Identity instruction reference, or None.
fn try_inst_recursive<'a>(program: &'a Program, val: &Value) -> Option<&'a Inst> {
    let mut inst_ref = match val {
        Value::Inst(r) => *r,
        _ => return None,
    };

    // Follow Identity chains.
    loop {
        let block = program.blocks.get(inst_ref.block as usize)?;
        let inst = block.instructions.get(inst_ref.inst as usize)?;
        if inst.opcode == Opcode::Identity && !inst.args.is_empty() {
            if let Value::Inst(next) = &inst.args[0] {
                inst_ref = *next;
                continue;
            }
        }
        return Some(inst);
    }
}

/// Check if an IAdd32 instruction matches the byte-swap pattern and should
/// be replaced with BitwiseOr32.
///
/// Upstream: `AddingByteSwapsWorkaround` (vendor_workaround_pass.cpp:12-63).
///
/// Pattern:
///   %lhs_bfe = BitFieldUExtract %factor_a, #0, #16
///   %lhs_mul = IMul32 %lhs_bfe, %factor_b           // potentially optional
///   %lhs_shl = ShiftLeftLogical32 %lhs_mul, #16
///   %rhs_bfe = BitFieldUExtract %factor_a, #16, #16
///   %result  = IAdd32 %lhs_shl, %rhs_bfe
fn should_replace_with_or(program: &Program, inst: &Inst) -> bool {
    if inst.args.len() < 2 {
        return false;
    }

    let lhs_shl = match try_inst_recursive(program, &inst.args[0]) {
        Some(i) => i,
        None => return false,
    };
    let rhs_bfe = match try_inst_recursive(program, &inst.args[1]) {
        Some(i) => i,
        None => return false,
    };

    // Check lhs_shl: ShiftLeftLogical32 with shift amount 16.
    if lhs_shl.opcode != Opcode::ShiftLeftLogical32 {
        return false;
    }
    if lhs_shl.args.len() < 2 || lhs_shl.args[1] != Value::ImmU32(16) {
        return false;
    }

    // Check rhs_bfe: BitFieldUExtract with offset=16, count=16.
    if rhs_bfe.opcode != Opcode::BitFieldUExtract {
        return false;
    }
    if rhs_bfe.args.len() < 3
        || rhs_bfe.args[1] != Value::ImmU32(16)
        || rhs_bfe.args[2] != Value::ImmU32(16)
    {
        return false;
    }

    // Check lhs_mul: the source of the shift should be IMul32 or BitFieldUExtract.
    let lhs_mul = match try_inst_recursive(program, &lhs_shl.args[0]) {
        Some(i) => i,
        None => return false,
    };
    let lhs_mul_optional = lhs_mul.opcode == Opcode::BitFieldUExtract;
    if lhs_mul.opcode != Opcode::IMul32 && !lhs_mul_optional {
        return false;
    }

    // Check lhs_bfe: the first input to the multiply (or the BFE itself if optional).
    let lhs_bfe = if lhs_mul_optional {
        lhs_mul
    } else {
        match try_inst_recursive(program, &lhs_mul.args[0]) {
            Some(i) => i,
            None => return false,
        }
    };

    if lhs_bfe.opcode != Opcode::BitFieldUExtract {
        return false;
    }
    if lhs_bfe.args.len() < 3
        || lhs_bfe.args[1] != Value::ImmU32(0)
        || lhs_bfe.args[2] != Value::ImmU32(16)
    {
        return false;
    }

    // Verify both BFEs extract from the same source.
    if lhs_bfe.args.is_empty() || rhs_bfe.args.is_empty() {
        return false;
    }
    if lhs_bfe.args[0] != rhs_bfe.args[0] {
        return false;
    }

    true
}
