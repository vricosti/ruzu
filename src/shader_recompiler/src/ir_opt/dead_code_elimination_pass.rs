// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Dead code elimination pass — remove unused instructions with no side effects.
//!
//! Matches upstream `dead_code_elimination_pass.cpp`.
//!
//! Iterates instructions in reverse order within each block. An instruction is
//! removed if it has no uses (`use_count == 0`) and may not have side effects.

use crate::ir::program::Program;

/// Remove dead (unused, side-effect-free) instructions.
pub fn dead_code_elimination_pass(program: &mut Program) {
    // Recompute use counts from scratch.
    program.recompute_use_counts();

    // Eliminate dead code by iterating blocks, then instructions in reverse.
    // We do multiple passes to catch cascading dead code.
    let mut changed = true;
    while changed {
        changed = false;
        for block_idx in 0..program.blocks.len() {
            let slots: Vec<u32> = program.blocks[block_idx]
                .indexed_iter()
                .map(|(idx, _)| idx)
                .collect();
            for idx in slots.into_iter().rev() {
                let should_erase = {
                    let inst = program.blocks[block_idx].inst(idx);
                    inst.opcode != crate::ir::opcodes::Opcode::Void
                        && inst.use_count == 0
                        && !inst.may_have_side_effects()
                };
                if should_erase {
                    program.erase_inst(crate::ir::value::InstRef {
                        block: block_idx as u32,
                        inst: idx,
                    });
                    changed = true;
                }
            }
        }
        if changed {
            program.recompute_use_counts();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::dead_code_elimination_pass;
    use crate::ir::basic_block::Block;
    use crate::ir::instruction::Inst;
    use crate::ir::opcodes::Opcode;
    use crate::ir::program::{Program, SyntaxNode};
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

    #[test]
    fn dce_counts_phi_args_as_uses() {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        program.blocks[0].append_inst(Inst::new(
            Opcode::IAdd32,
            vec![Value::ImmU32(1), Value::ImmU32(2)],
        ));
        let mut phi = Inst::new(Opcode::Phi, Vec::new());
        phi.add_phi_operand(0, Value::Inst(InstRef { block: 0, inst: 0 }));
        let phi_ref = InstRef {
            block: 0,
            inst: program.blocks[0].append_inst(phi),
        };
        program.blocks[0].append_inst(Inst::new(
            Opcode::SetAttribute,
            vec![
                Value::Attribute(crate::ir::value::Attribute::POSITION_X),
                Value::Inst(phi_ref),
                Value::ImmU32(0),
            ],
        ));

        dead_code_elimination_pass(&mut program);

        assert_eq!(program.blocks[0].inst(0).opcode, Opcode::IAdd32);
        assert_eq!(program.blocks[0].inst(1).opcode, Opcode::Phi);
    }

    #[test]
    fn dce_counts_syntax_conditions_as_uses() {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        let cond = program.blocks[0]
            .append_inst(Inst::new(Opcode::ConditionRef, vec![Value::ImmU1(true)]));
        program.syntax_list.push(SyntaxNode::If {
            cond: Value::Inst(InstRef {
                block: 0,
                inst: cond,
            }),
            body: 0,
            merge: 0,
        });

        dead_code_elimination_pass(&mut program);

        assert_eq!(program.blocks[0].inst(cond).opcode, Opcode::ConditionRef);
    }

    #[test]
    fn dce_unlinks_dead_pseudo_flags_from_their_parent() {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        let parent = program.blocks[0].append_inst(Inst::new(
            Opcode::IAdd32,
            vec![Value::ImmU32(1), Value::ImmU32(2)],
        ));
        for pseudo_op in [
            Opcode::GetZeroFromOp,
            Opcode::GetSignFromOp,
            Opcode::GetCarryFromOp,
            Opcode::GetOverflowFromOp,
        ] {
            let pseudo = program.blocks[0].append_inst(Inst::new(
                pseudo_op,
                vec![Value::Inst(InstRef {
                    block: 0,
                    inst: parent,
                })],
            ));
            program.blocks[0].inst_mut(parent).set_associated_pseudo(
                pseudo_op,
                InstRef {
                    block: 0,
                    inst: pseudo,
                },
            );
        }
        program.blocks[0].append_inst(Inst::new(
            Opcode::SetAttribute,
            vec![
                Value::Attribute(crate::ir::value::Attribute::POSITION_X),
                Value::Inst(InstRef {
                    block: 0,
                    inst: parent,
                }),
                Value::ImmU32(0),
            ],
        ));

        dead_code_elimination_pass(&mut program);

        let parent = program.blocks[0].inst(parent);
        for pseudo_op in [
            Opcode::GetZeroFromOp,
            Opcode::GetSignFromOp,
            Opcode::GetCarryFromOp,
            Opcode::GetOverflowFromOp,
        ] {
            assert_eq!(parent.get_associated_pseudo(pseudo_op), None);
        }
    }
}
