// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `ir_opt/vendor_workaround_pass.cpp`
//!
//! Applies vendor-specific workarounds to the IR. Currently handles
//! a vendor-driver bug where byte-swap patterns using `IAdd32` need
//! to be replaced with `BitwiseOr32`.

use crate::ir::instruction::Inst;
use crate::ir::opcodes::Opcode;
use crate::ir::program::Program;
use crate::ir::value::Value;

/// Apply vendor-specific workarounds to the IR.
///
/// Upstream: `VendorWorkaroundPass` (vendor_workaround_pass.cpp:65-77).
pub fn vendor_workaround_pass(program: &mut Program) {
    for block_idx in program.post_order_blocks.clone() {
        let instructions = program
            .block(block_idx)
            .indexed_iter()
            .map(|(inst_idx, _)| inst_idx)
            .collect::<Vec<_>>();
        for inst_idx in instructions {
            if program.block(block_idx).inst(inst_idx).opcode == Opcode::IAdd32 {
                adding_byte_swaps_workaround(program, block_idx, inst_idx);
            }
        }
    }
    program.recompute_use_counts();
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
        let inst = block.instructions.get(inst_ref.inst as usize)?.as_ref()?;
        if inst.opcode == Opcode::Identity && !inst.args.is_empty() {
            if let Value::Inst(next) = &inst.args[0] {
                inst_ref = *next;
                continue;
            }
        }
        return Some(inst);
    }
}

/// Replace an `IAdd32` matching the byte-swap pattern with `BitwiseOr32`.
///
/// Upstream: `AddingByteSwapsWorkaround` (vendor_workaround_pass.cpp:12-63).
///
/// Pattern:
///   %lhs_bfe = BitFieldUExtract %factor_a, #0, #16
///   %lhs_mul = IMul32 %lhs_bfe, %factor_b           // potentially optional
///   %lhs_shl = ShiftLeftLogical32 %lhs_mul, #16
///   %rhs_bfe = BitFieldUExtract %factor_a, #16, #16
///   %result  = IAdd32 %lhs_shl, %rhs_bfe
fn adding_byte_swaps_workaround(program: &mut Program, block_idx: u32, inst_idx: u32) {
    let inst = program.block(block_idx).inst(inst_idx);
    if inst.args.len() < 2 {
        return;
    }

    let lhs_shl = match try_inst_recursive(program, &inst.args[0]) {
        Some(i) => i,
        None => return,
    };
    let rhs_bfe = match try_inst_recursive(program, &inst.args[1]) {
        Some(i) => i,
        None => return,
    };

    // Check lhs_shl: ShiftLeftLogical32 with shift amount 16.
    if lhs_shl.opcode != Opcode::ShiftLeftLogical32 {
        return;
    }
    if lhs_shl.args.len() < 2 || lhs_shl.args[1] != Value::ImmU32(16) {
        return;
    }

    // Check rhs_bfe: BitFieldUExtract with offset=16, count=16.
    if rhs_bfe.opcode != Opcode::BitFieldUExtract {
        return;
    }
    if rhs_bfe.args.len() < 3
        || rhs_bfe.args[1] != Value::ImmU32(16)
        || rhs_bfe.args[2] != Value::ImmU32(16)
    {
        return;
    }

    // Check lhs_mul: the source of the shift should be IMul32 or BitFieldUExtract.
    let lhs_mul = match try_inst_recursive(program, &lhs_shl.args[0]) {
        Some(i) => i,
        None => return,
    };
    let lhs_mul_optional = lhs_mul.opcode == Opcode::BitFieldUExtract;
    if lhs_mul.opcode != Opcode::IMul32 && !lhs_mul_optional {
        return;
    }

    // Check lhs_bfe: the first input to the multiply (or the BFE itself if optional).
    let lhs_bfe = if lhs_mul_optional {
        lhs_mul
    } else {
        match try_inst_recursive(program, &lhs_mul.args[0]) {
            Some(i) => i,
            None => return,
        }
    };

    if lhs_bfe.opcode != Opcode::BitFieldUExtract {
        return;
    }
    if lhs_bfe.args.len() < 3
        || lhs_bfe.args[1] != Value::ImmU32(0)
        || lhs_bfe.args[2] != Value::ImmU32(16)
    {
        return;
    }

    let args = program.block(block_idx).inst(inst_idx).args[..2].to_vec();
    let replacement_idx = program
        .block_mut(block_idx)
        .insert_inst_before(inst_idx, Inst::new(Opcode::BitwiseOr32, args));
    let replacement = Value::Inst(crate::ir::value::InstRef {
        block: block_idx,
        inst: replacement_idx,
    });
    let inst = program.block_mut(block_idx).inst_mut(inst_idx);
    inst.opcode = Opcode::Identity;
    inst.args = vec![replacement];
    inst.phi_args.clear();
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::basic_block::Block;
    use crate::ir::types::ShaderStage;
    use crate::ir::value::InstRef;

    fn inst_value(inst: u32) -> Value {
        Value::Inst(InstRef { block: 0, inst })
    }

    #[test]
    fn byte_swap_workaround_inserts_or_and_replaces_add_with_identity() {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        program.post_order_blocks = vec![0];
        let source = program
            .block_mut(0)
            .append_inst(Inst::new(Opcode::GetRegister, vec![Value::ImmU32(1)]));
        let lhs_bfe = program.block_mut(0).append_inst(Inst::new(
            Opcode::BitFieldUExtract,
            vec![inst_value(source), Value::ImmU32(0), Value::ImmU32(16)],
        ));
        let lhs_shl = program.block_mut(0).append_inst(Inst::new(
            Opcode::ShiftLeftLogical32,
            vec![inst_value(lhs_bfe), Value::ImmU32(16)],
        ));
        let rhs_bfe = program.block_mut(0).append_inst(Inst::new(
            Opcode::BitFieldUExtract,
            vec![inst_value(source), Value::ImmU32(16), Value::ImmU32(16)],
        ));
        let add = program.block_mut(0).append_inst(Inst::new(
            Opcode::IAdd32,
            vec![inst_value(lhs_shl), inst_value(rhs_bfe)],
        ));

        vendor_workaround_pass(&mut program);

        let identity = program.block(0).inst(add);
        assert_eq!(identity.opcode, Opcode::Identity);
        let Value::Inst(or_ref) = identity.args[0] else {
            panic!("workaround identity must refer to the inserted OR");
        };
        let or = program.block(or_ref.block).inst(or_ref.inst);
        assert_eq!(or.opcode, Opcode::BitwiseOr32);
        assert_eq!(or.args, vec![inst_value(lhs_shl), inst_value(rhs_bfe)]);
        assert_eq!(or.use_count, 1);
    }
}
