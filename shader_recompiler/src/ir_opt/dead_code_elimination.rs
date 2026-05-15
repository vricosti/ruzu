// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Dead code elimination pass — remove unused instructions with no side effects.
//!
//! Matches zuyu's `dead_code_elimination_pass.cpp`.
//!
//! Iterates instructions in reverse order within each block. An instruction is
//! removed if it has no uses (`use_count == 0`) and may not have side effects.

use crate::ir::program::Program;
use crate::ir::value::Value;

/// Remove dead (unused, side-effect-free) instructions.
pub fn dead_code_elimination_pass(program: &mut Program) {
    // Recompute use counts from scratch.
    recompute_use_counts(program);

    // Eliminate dead code by iterating blocks, then instructions in reverse.
    // We do multiple passes to catch cascading dead code.
    let mut changed = true;
    while changed {
        changed = false;
        for block in &mut program.blocks {
            let slots: Vec<u32> = block.indexed_iter().map(|(idx, _)| idx).collect();
            for idx in slots.into_iter().rev() {
                let inst = block.inst(idx);
                if inst.opcode != crate::ir::opcodes::Opcode::Void
                    && inst.use_count == 0
                    && !inst.may_have_side_effects()
                {
                    block.erase_inst(idx);
                    changed = true;
                }
            }
        }
        if changed {
            recompute_use_counts(program);
        }
    }
}

/// Recompute all use counts by scanning every instruction's arguments.
fn recompute_use_counts(program: &mut Program) {
    // Reset all use counts to zero
    for block in &mut program.blocks {
        for inst in block.iter_mut() {
            inst.use_count = 0;
        }
    }

    // Build use count map
    let mut use_counts: Vec<Vec<u32>> = program
        .blocks
        .iter()
        .map(|b| vec![0u32; b.instructions.len()])
        .collect();

    for block in &program.blocks {
        for inst in block.iter() {
            for arg in &inst.args {
                if let Value::Inst(r) = arg {
                    if let Some(block_counts) = use_counts.get_mut(r.block as usize) {
                        if let Some(count) = block_counts.get_mut(r.inst as usize) {
                            *count += 1;
                        }
                    }
                }
            }
        }
    }

    // Apply computed counts
    for (block_idx, block) in program.blocks.iter_mut().enumerate() {
        for (inst_idx, inst) in block.indexed_iter_mut() {
            if let Some(block_counts) = use_counts.get(block_idx) {
                if let Some(&count) = block_counts.get(inst_idx as usize) {
                    inst.use_count = count;
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::dead_code_elimination_pass;
    use crate::ir::basic_block::Block;
    use crate::ir::instruction::Inst;
    use crate::ir::opcodes::Opcode;
    use crate::ir::program::Program;
    use crate::ir::types::ShaderStage;
    use crate::ir::value::{InstRef, Value};

    #[test]
    fn dce_preserves_instref_indices_with_tombstones() {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        program.blocks[0].append_inst(Inst::new(
            Opcode::IAdd32,
            vec![Value::ImmU32(1), Value::ImmU32(2)],
        ));
        program.blocks[0].append_inst(Inst::new(
            Opcode::IAdd32,
            vec![Value::ImmU32(3), Value::ImmU32(4)],
        ));
        program.blocks[0].append_inst(Inst::new(
            Opcode::SetAttribute,
            vec![
                Value::Attribute(crate::ir::value::Attribute::POSITION_X),
                Value::Inst(InstRef { block: 0, inst: 1 }),
                Value::ImmU32(0),
            ],
        ));

        dead_code_elimination_pass(&mut program);

        assert_eq!(program.blocks[0].instructions.len(), 3);
        assert!(program.blocks[0].instructions[0].is_none());
        assert_eq!(program.blocks[0].inst(1).opcode, Opcode::IAdd32);
        assert_eq!(
            program.blocks[0].inst(2).args[1],
            Value::Inst(InstRef { block: 0, inst: 1 })
        );
    }
}
