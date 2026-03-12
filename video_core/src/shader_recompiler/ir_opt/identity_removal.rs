// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Identity removal pass — remove redundant Identity/Void instructions.
//!
//! Matches zuyu's `identity_removal_pass.cpp`.
//!
//! Two phases:
//! 1. For each instruction argument, resolve chains of Identity instructions
//!    so that `Identity(Identity(x))` becomes just `x`.
//! 2. Remove Identity and Void instructions from the block.

use crate::shader_recompiler::ir::opcodes::Opcode;
use crate::shader_recompiler::ir::program::Program;
use crate::shader_recompiler::ir::value::Value;

/// Remove identity/void instructions and resolve identity chains in arguments.
pub fn identity_removal_pass(program: &mut Program) {
    // Phase 1: Build a map of identity instruction targets.
    // For each instruction that is Identity, record what it resolves to.
    let mut identity_targets: std::collections::HashMap<(u32, u32), Value> =
        std::collections::HashMap::new();

    for (block_idx, block) in program.blocks.iter().enumerate() {
        for (inst_idx, inst) in block.instructions.iter().enumerate() {
            if inst.opcode == Opcode::Identity && !inst.args.is_empty() {
                identity_targets.insert((block_idx as u32, inst_idx as u32), inst.args[0]);
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

    // Phase 2: Replace identity references in all instruction arguments.
    for block in &mut program.blocks {
        for inst in &mut block.instructions {
            for arg in &mut inst.args {
                if let Value::Inst(r) = arg {
                    if let Some(&resolved) = identity_targets.get(&(r.block, r.inst)) {
                        *arg = resolved;
                    }
                }
            }
        }
    }

    // Phase 3: Remove Identity and Void instructions.
    for block in &mut program.blocks {
        block.instructions.retain(|inst| {
            inst.opcode != Opcode::Identity && inst.opcode != Opcode::Void
        });
    }
}
