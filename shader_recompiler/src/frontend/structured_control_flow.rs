// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Structured control flow conversion: flat CFG → nested If/Loop/Break AST.
//!
//! Converts the flat CFG from `control_flow.rs` into the `SyntaxNode` tree
//! that the SPIR-V backend can directly emit as structured control flow.
//!
//! Ref: zuyu `frontend/maxwell/structured_control_flow.cpp`

use super::control_flow::{CfgBlock, EndClass};
use crate::ir::program::SyntaxNode;
use crate::ir::value::Value;

/// Convert a flat CFG into a structured abstract syntax list.
///
/// This is a simplified version of zuyu's algorithm. It produces a list of
/// `SyntaxNode` entries that represent the nested control flow structure.
pub fn structure_cfg(cfg_blocks: &[CfgBlock]) -> Vec<SyntaxNode> {
    let mut syntax = Vec::new();

    if cfg_blocks.is_empty() {
        return syntax;
    }

    // Simple linearization: walk blocks in order, detect branches as if/then.
    // A full implementation would do dominance analysis and loop detection.
    // For now, we do a single-pass linearization that handles:
    // - Unconditional fall-through → Block
    // - Conditional forward branch → If/EndIf
    // - Backward branch → Loop/Repeat
    // - EXIT → Return

    let num_blocks = cfg_blocks.len();
    let mut visited = vec![false; num_blocks];

    structure_region(&cfg_blocks, 0, num_blocks, &mut visited, &mut syntax);

    syntax
}

fn structure_region(
    blocks: &[CfgBlock],
    start: usize,
    end: usize,
    visited: &mut [bool],
    syntax: &mut Vec<SyntaxNode>,
) {
    let mut i = start;
    while i < end && i < blocks.len() {
        if visited[i] {
            i += 1;
            continue;
        }
        visited[i] = true;

        let block = &blocks[i];
        let block_idx = i as u32;

        match block.end_class {
            EndClass::Exit => {
                syntax.push(SyntaxNode::Block(block_idx));
                if !block.cond.is_always() {
                    // Upstream `AnalyzeCondInst` splits a predicated EXIT into
                    // an exit branch and a fallthrough label. Preserve that
                    // control-flow shape here so instructions after the EXIT do
                    // not execute when the predicate takes the exit path.
                    let merge = block.branch_false.unwrap_or(i + 1) as u32;
                    syntax.push(SyntaxNode::If {
                        cond: Value::ImmU1(true),
                        body: block_idx,
                        merge,
                    });
                    syntax.push(SyntaxNode::Return);
                    syntax.push(SyntaxNode::EndIf { merge });
                    i += 1;
                    continue;
                }
                syntax.push(SyntaxNode::Return);
                i += 1;
            }
            EndClass::Kill => {
                syntax.push(SyntaxNode::Block(block_idx));
                // Upstream inserts a conditional demote-to-helper merge here.
                // This simplified SCF pass cannot model that branch yet; do
                // not lower KIL to an unconditional return, which discards all
                // fragments in shaders whose KIL is predicate-guarded.
                i += 1;
            }
            EndClass::Return => {
                syntax.push(SyntaxNode::Block(block_idx));
                syntax.push(SyntaxNode::Return);
                i += 1;
            }
            EndClass::Branch => {
                if let Some(true_target) = block.branch_true {
                    if !block.cond.is_always() {
                        // Conditional branch
                        if true_target > i {
                            // Forward branch → If/Then
                            let merge = true_target as u32;
                            syntax.push(SyntaxNode::Block(block_idx));
                            // Upstream `GotoPass::EliminateAsConditional`
                            // moves the statements between a forward goto and
                            // its label under `if (!goto_cond)`.
                            syntax.push(SyntaxNode::If {
                                cond: Value::ImmU1(false),
                                body: (i + 1) as u32,
                                merge,
                            });

                            // Emit the if body (blocks between here and target)
                            if let Some(false_target) = block.branch_false {
                                if false_target < blocks.len() {
                                    structure_region(
                                        blocks,
                                        false_target,
                                        true_target,
                                        visited,
                                        syntax,
                                    );
                                }
                            }

                            syntax.push(SyntaxNode::EndIf { merge });
                            i = true_target;
                        } else if true_target < i {
                            // Backward branch → Loop
                            syntax.push(SyntaxNode::Block(block_idx));
                            syntax.push(SyntaxNode::Repeat {
                                cond: Value::ImmU1(true),
                                loop_header: true_target as u32,
                                merge: (i + 1) as u32,
                            });
                            i += 1;
                        } else {
                            syntax.push(SyntaxNode::Block(block_idx));
                            i += 1;
                        }
                    } else {
                        // Unconditional branch
                        if true_target == i + 1 {
                            // Fall-through
                            syntax.push(SyntaxNode::Block(block_idx));
                            i += 1;
                        } else if true_target < i {
                            // Backward jump → loop back-edge
                            syntax.push(SyntaxNode::Block(block_idx));
                            syntax.push(SyntaxNode::Repeat {
                                cond: Value::ImmU1(true),
                                loop_header: true_target as u32,
                                merge: (i + 1) as u32,
                            });
                            i += 1;
                        } else {
                            // Forward unconditional jump
                            syntax.push(SyntaxNode::Block(block_idx));
                            i = true_target;
                        }
                    }
                } else {
                    syntax.push(SyntaxNode::Block(block_idx));
                    i += 1;
                }
            }
            EndClass::IndirectBranch => {
                // Indirect branches are complex; emit block and unreachable for now.
                syntax.push(SyntaxNode::Block(block_idx));
                syntax.push(SyntaxNode::Unreachable);
                i += 1;
            }
            EndClass::Call => {
                syntax.push(SyntaxNode::Block(block_idx));
                i += 1;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::control_flow::Condition;

    fn block(end_class: EndClass, cond: Condition) -> CfgBlock {
        CfgBlock {
            begin: 0,
            end: 1,
            end_class,
            branch_true: None,
            branch_false: None,
            cond,
            stack_depth: 0,
        }
    }

    #[test]
    fn conditional_exit_does_not_emit_flat_return() {
        let mut cfg_block = block(
            EndClass::Exit,
            Condition {
                pred: 0,
                negated: false,
            },
        );
        cfg_block.branch_false = Some(1);
        let syntax = structure_cfg(&[cfg_block, block(EndClass::Return, Condition::always())]);

        assert!(matches!(
            syntax.as_slice(),
            [
                SyntaxNode::Block(0),
                SyntaxNode::If { .. },
                SyntaxNode::Return,
                SyntaxNode::EndIf { merge: 1 },
                SyntaxNode::Block(1),
                SyntaxNode::Return
            ]
        ));
    }

    #[test]
    fn unconditional_exit_still_returns() {
        let syntax = structure_cfg(&[block(EndClass::Exit, Condition::always())]);

        assert!(matches!(
            syntax.as_slice(),
            [SyntaxNode::Block(0), SyntaxNode::Return]
        ));
    }

    #[test]
    fn conditional_forward_branch_bodies_use_inverted_condition() {
        let mut branch = block(
            EndClass::Branch,
            Condition {
                pred: 0,
                negated: false,
            },
        );
        branch.branch_true = Some(2);
        branch.branch_false = Some(1);

        let syntax = structure_cfg(&[
            branch,
            block(EndClass::Branch, Condition::always()),
            block(EndClass::Return, Condition::always()),
        ]);

        assert!(matches!(
            syntax.as_slice(),
            [
                SyntaxNode::Block(0),
                SyntaxNode::If {
                    cond: Value::ImmU1(false),
                    body: 1,
                    merge: 2,
                },
                SyntaxNode::Block(1),
                SyntaxNode::EndIf { merge: 2 },
                SyntaxNode::Block(2),
                SyntaxNode::Return,
            ]
        ));
    }
}
