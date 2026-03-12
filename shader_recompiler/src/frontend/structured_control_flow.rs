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
                    // Conditional exit: emit as if { exit } else { continue }
                    // For now, just emit the block and a return.
                }
                syntax.push(SyntaxNode::Return);
                i += 1;
            }
            EndClass::Kill => {
                syntax.push(SyntaxNode::Block(block_idx));
                syntax.push(SyntaxNode::Return);
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
                            let cond = Value::ImmU1(true);
                            let merge = true_target as u32;
                            syntax.push(SyntaxNode::Block(block_idx));
                            syntax.push(SyntaxNode::If {
                                cond,
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
                            let cond = Value::ImmU1(true);
                            syntax.push(SyntaxNode::Block(block_idx));
                            syntax.push(SyntaxNode::Repeat {
                                cond,
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
