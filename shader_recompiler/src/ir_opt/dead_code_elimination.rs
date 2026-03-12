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
            let mut i = block.instructions.len();
            while i > 0 {
                i -= 1;
                if block.instructions[i].use_count == 0
                    && !block.instructions[i].may_have_side_effects()
                {
                    block.instructions.remove(i);
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
        for inst in &mut block.instructions {
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
        for inst in &block.instructions {
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
        for (inst_idx, inst) in block.instructions.iter_mut().enumerate() {
            if let Some(block_counts) = use_counts.get(block_idx) {
                if let Some(&count) = block_counts.get(inst_idx) {
                    inst.use_count = count;
                }
            }
        }
    }
}
