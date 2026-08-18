// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Identity removal pass — remove redundant Identity/Void instructions.
//!
//! Matches upstream `identity_removal_pass.cpp`.
//!
//! Two phases:
//! 1. For each instruction argument, resolve chains of Identity instructions
//!    so that `Identity(Identity(x))` becomes just `x`.
//! 2. Erase Identity/Void slots after all users have been rewritten.

use crate::ir::opcodes::Opcode;
use crate::ir::program::{Program, SyntaxNode};
use crate::ir::value::Value;

/// Remove identity/void instructions and resolve identity chains in arguments.
pub fn identity_removal_pass(program: &mut Program) {
    // Rust construction currently writes indexed InstRefs directly instead of
    // routing every mutation through upstream's intrusive `Inst::Use` chain.
    // Rebuild that invariant before `SetArg`/`Invalidate`-equivalent updates.
    program.recompute_use_counts();

    // Phase 1: Build a map of identity instruction targets.
    // For each instruction that is Identity, record what it resolves to.
    let mut identity_targets: std::collections::HashMap<(u32, u32), Value> =
        std::collections::HashMap::new();
    let mut void_insts = Vec::new();

    for (block_idx, block) in program.blocks.iter().enumerate() {
        for (inst_idx, inst) in block.indexed_iter() {
            if inst.opcode == Opcode::Identity && !inst.args.is_empty() {
                identity_targets.insert((block_idx as u32, inst_idx), inst.args[0]);
            } else if inst.opcode == Opcode::Void {
                void_insts.push((block_idx as u32, inst_idx));
            }
        }
    }

    // Resolve transitive identity chains in the map itself
    let mut changed = true;
    while changed {
        changed = false;
        let keys: Vec<(u32, u32)> = identity_targets.keys().cloned().collect();
        for key in keys {
            if let Some(&value) = identity_targets.get(&key) {
                if let Value::Inst(r) = value {
                    if let Some(&target) = identity_targets.get(&(r.block, r.inst)) {
                        identity_targets.insert(key, target);
                        changed = true;
                    }
                }
            }
        }
    }

    // Phase 2: Replace identity references in all instruction arguments,
    // including phi-node operands. Upstream `ReplaceUsesWith` walks every
    // user via the use-def chain; ruzu does it indirectly by scanning all
    // `args` and `phi_args` here.
    let mut replaced_uses = Vec::new();
    for block in &mut program.blocks {
        for inst in block.iter_mut() {
            for arg in &mut inst.args {
                if let Some(replacement) = resolve_identity_arg(arg, &identity_targets) {
                    replaced_uses.push(replacement);
                }
            }
            for (_, val) in &mut inst.phi_args {
                if let Some(replacement) = resolve_identity_arg(val, &identity_targets) {
                    replaced_uses.push(replacement);
                }
            }
        }
    }

    for node in &mut program.syntax_list {
        match node {
            SyntaxNode::If { cond, .. }
            | SyntaxNode::Repeat { cond, .. }
            | SyntaxNode::Break { cond, .. } => {
                if let Some(replacement) = resolve_identity_arg(cond, &identity_targets) {
                    replaced_uses.push(replacement);
                }
            }
            _ => {}
        }
    }

    // Upstream performs these updates in `Inst::SetArg`: replacing an
    // identity reference removes one use from that identity and adds one use
    // to the resolved instruction. Apply them after the scan to preserve
    // Rust's exclusive borrows while keeping the same use-count lifecycle.
    for (identity, replacement) in replaced_uses {
        let identity_uses = &mut program
            .block_mut(identity.block)
            .inst_mut(identity.inst)
            .use_count;
        assert!(*identity_uses > 0, "identity use count underflow");
        *identity_uses -= 1;
        if let Value::Inst(resolved) = replacement {
            program
                .block_mut(resolved.block)
                .inst_mut(resolved.inst)
                .use_count += 1;
        }
    }

    // Phase 3: erase list nodes whose users have already been rewritten.
    // `Block` keeps stable slots, so this matches upstream physical erase
    // semantics without shifting later `InstRef` identities.
    let mut erased: Vec<(u32, u32)> = identity_targets.keys().copied().collect();
    erased.extend(void_insts);
    for (block_idx, inst_idx) in erased {
        if program
            .blocks
            .get(block_idx as usize)
            .is_some_and(|block| (inst_idx as usize) < block.instructions.len())
        {
            program.erase_inst(crate::ir::value::InstRef {
                block: block_idx,
                inst: inst_idx,
            });
        }
    }
}

fn resolve_identity_arg(
    value: &mut Value,
    identity_targets: &std::collections::HashMap<(u32, u32), Value>,
) -> Option<(crate::ir::value::InstRef, Value)> {
    if let Value::Inst(r) = value {
        if let Some(&resolved) = identity_targets.get(&(r.block, r.inst)) {
            let identity = *r;
            *value = resolved;
            return Some((identity, resolved));
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::identity_removal_pass;
    use crate::ir::basic_block::Block;
    use crate::ir::instruction::Inst;
    use crate::ir::opcodes::Opcode;
    use crate::ir::program::{Program, SyntaxNode};
    use crate::ir::types::ShaderStage;
    use crate::ir::value::{InstRef, Value};

    #[test]
    fn identity_removal_rewrites_syntax_conditions_before_erasing() {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        let source = program.blocks[0]
            .append_inst(Inst::new(Opcode::ConditionRef, vec![Value::ImmU1(true)]));
        let identity = program.blocks[0].append_inst(Inst::new(
            Opcode::Identity,
            vec![Value::Inst(InstRef {
                block: 0,
                inst: source,
            })],
        ));
        program.syntax_list.push(SyntaxNode::If {
            cond: Value::Inst(InstRef {
                block: 0,
                inst: identity,
            }),
            body: 0,
            merge: 0,
        });

        identity_removal_pass(&mut program);

        assert!(program.blocks[0].instructions[identity as usize].is_none());
        let SyntaxNode::If { cond, .. } = program.syntax_list[0] else {
            panic!("expected If syntax node");
        };
        assert_eq!(
            cond,
            Value::Inst(InstRef {
                block: 0,
                inst: source,
            })
        );
        assert_eq!(program.blocks[0].inst(source).use_count, 1);
    }

    #[test]
    fn identity_removal_erases_void_instructions() {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        let live = program.blocks[0].append_inst(Inst::new(Opcode::IAdd32, vec![]));
        let void = program.blocks[0].append_inst(Inst::new(Opcode::Void, vec![]));

        identity_removal_pass(&mut program);

        assert!(program.blocks[0].instructions[void as usize].is_none());
        let order: Vec<u32> = program.blocks[0]
            .indexed_iter()
            .map(|(index, _)| index)
            .collect();
        assert_eq!(order, vec![live]);
    }
}
