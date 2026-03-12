// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `ir_opt/conditional_barrier_pass.cpp`
//!
//! Removes barrier instructions that appear inside conditional control flow,
//! since barriers in divergent control flow cause undefined behavior on some
//! GPUs.

use crate::shader_recompiler::ir::opcodes::Opcode;
use crate::shader_recompiler::ir::program::{Program, SyntaxNode};

/// Remove barriers inside conditional control flow by replacing them with Identity.
pub fn conditional_barrier_pass(program: &mut Program) {
    let mut conditional_control_flow_count: i32 = 0;
    let mut conditional_return_count: i32 = 0;

    for node in &program.syntax_list {
        match node {
            SyntaxNode::If { .. } | SyntaxNode::Loop { .. } => {
                conditional_control_flow_count += 1;
            }
            SyntaxNode::EndIf { .. } | SyntaxNode::Repeat { .. } => {
                conditional_control_flow_count -= 1;
            }
            SyntaxNode::Unreachable | SyntaxNode::Return => {
                if conditional_control_flow_count > 0 {
                    conditional_return_count += 1;
                }
            }
            SyntaxNode::Block(block_idx) => {
                let block_idx = *block_idx as usize;
                if block_idx < program.blocks.len() {
                    for inst in &mut program.blocks[block_idx].instructions {
                        if (conditional_control_flow_count > 0 || conditional_return_count > 0)
                            && inst.opcode == Opcode::Barrier
                        {
                            log::warn!("Barrier within conditional control flow");
                            inst.opcode = Opcode::Identity;
                        }
                    }
                }
            }
            _ => {}
        }
    }
    debug_assert_eq!(conditional_control_flow_count, 0);
}
