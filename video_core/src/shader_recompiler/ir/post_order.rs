// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `frontend/ir/post_order.h` and `frontend/ir/post_order.cpp`
//!
//! Computes a post-order traversal of basic blocks from the structured
//! control flow AST.

use std::collections::BTreeSet;

use super::basic_block::Block;

/// Compute the post-order traversal of blocks.
///
/// Takes a list of blocks with their successor information and returns
/// the block indices in post-order. This is used by optimization passes
/// that need to visit blocks in reverse post-order.
pub fn post_order(blocks: &[Block], entry: u32) -> Vec<u32> {
    let mut block_stack: Vec<u32> = Vec::with_capacity(16);
    let mut visited: BTreeSet<u32> = BTreeSet::new();
    let mut result: Vec<u32> = Vec::new();

    visited.insert(entry);
    block_stack.push(entry);

    while let Some(block_idx) = block_stack.pop() {
        let block = &blocks[block_idx as usize];
        let mut pushed = false;

        for &succ in &block.imm_successors {
            if visited.insert(succ) {
                // Push current block back, then the successor
                block_stack.push(block_idx);
                block_stack.push(succ);
                pushed = true;
                break;
            }
        }

        if !pushed {
            result.push(block_idx);
        }
    }

    result
}
