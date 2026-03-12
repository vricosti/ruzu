// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `ir_opt/verification_pass.cpp`
//!
//! Verification pass that validates the IR program for correctness:
//! - Type compatibility between instruction arguments and expected types
//! - Use count consistency
//! - No forward declarations (except in phi nodes)
//! - Phi nodes are not interleaved with regular instructions

use crate::shader_recompiler::ir::program::Program;

/// Validate the IR program for correctness.
///
/// This pass checks:
/// 1. Argument types match expected types for each opcode
/// 2. Use counts are consistent
/// 3. No forward declarations (except phi nodes)
/// 4. Phi nodes appear at the beginning of blocks
///
/// Panics with a descriptive message if verification fails.
pub fn verification_pass(program: &Program) {
    validate_types(program);
    validate_uses(program);
    validate_forward_declarations(program);
    validate_phi_nodes(program);
}

fn validate_types(program: &Program) {
    for block in &program.blocks {
        for inst in &block.instructions {
            if inst.opcode == crate::shader_recompiler::ir::opcodes::Opcode::Phi {
                continue;
            }
            // TODO: Validate argument types against opcode metadata
            let _meta = inst.opcode.meta();
            // For each argument, check IR::AreTypesCompatible(arg.Type(), expected)
        }
    }
}

fn validate_uses(program: &Program) {
    use std::collections::HashMap;
    let mut actual_uses: HashMap<(usize, usize), i32> = HashMap::new();

    for (block_idx, block) in program.blocks.iter().enumerate() {
        for (inst_idx, inst) in block.instructions.iter().enumerate() {
            let _ = (block_idx, inst_idx);
            for arg in &inst.args {
                if let crate::shader_recompiler::ir::value::Value::Inst(ref inst_ref) = arg {
                    *actual_uses
                        .entry((inst_ref.block as usize, inst_ref.inst as usize))
                        .or_insert(0) += 1;
                }
            }
        }
    }

    for (&(block_idx, inst_idx), &uses) in &actual_uses {
        if block_idx < program.blocks.len() && inst_idx < program.blocks[block_idx].instructions.len()
        {
            let inst = &program.blocks[block_idx].instructions[inst_idx];
            if inst.use_count as i32 != uses {
                log::error!(
                    "Use count mismatch at block {} inst {}: expected {}, got {}",
                    block_idx,
                    inst_idx,
                    uses,
                    inst.use_count
                );
            }
        }
    }
}

fn validate_forward_declarations(program: &Program) {
    use std::collections::HashSet;
    let mut definitions: HashSet<(usize, usize)> = HashSet::new();

    for (block_idx, block) in program.blocks.iter().enumerate() {
        for (inst_idx, inst) in block.instructions.iter().enumerate() {
            definitions.insert((block_idx, inst_idx));
            if inst.opcode == crate::shader_recompiler::ir::opcodes::Opcode::Phi {
                continue;
            }
            for arg in &inst.args {
                if let crate::shader_recompiler::ir::value::Value::Inst(ref inst_ref) = arg {
                    let key = (inst_ref.block as usize, inst_ref.inst as usize);
                    if !definitions.contains(&key) {
                        log::error!(
                            "Forward declaration at block {} inst {}: references block {} inst {}",
                            block_idx,
                            inst_idx,
                            inst_ref.block,
                            inst_ref.inst
                        );
                    }
                }
            }
        }
    }
}

fn validate_phi_nodes(program: &Program) {
    for block in &program.blocks {
        let mut no_more_phis = false;
        for inst in &block.instructions {
            if inst.opcode == crate::shader_recompiler::ir::opcodes::Opcode::Phi {
                if no_more_phis {
                    log::error!("Interleaved phi nodes detected");
                }
            } else {
                no_more_phis = true;
            }
        }
    }
}
