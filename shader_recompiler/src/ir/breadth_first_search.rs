// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `frontend/ir/breadth_first_search.h`
//!
//! Generic breadth-first search over the SSA instruction graph.
//! Used by optimization passes to find specific instruction patterns
//! by traversing arguments.

use std::collections::VecDeque;

use super::instruction::Inst;
use super::value::Value;

/// Perform a breadth-first search over the SSA graph starting from `value`.
///
/// The predicate `pred` is called for each instruction encountered.
/// If it returns `Some(result)`, the search terminates and returns that result.
/// If the entire SSA tree is traversed without finding a match, returns `None`.
///
/// Visits the rightmost arguments first, matching upstream behavior.
pub fn breadth_first_search<T, F>(value: &Value, instructions: &[Vec<Inst>], pred: F) -> Option<T>
where
    F: Fn(&Inst) -> Option<T>,
{
    if value.is_immediate() {
        return None;
    }

    let mut visited: Vec<(usize, usize)> = Vec::with_capacity(2);
    let mut queue: VecDeque<(usize, usize)> = VecDeque::new();

    if let Value::Inst(inst_ref) = value {
        let key = (inst_ref.block as usize, inst_ref.inst as usize);
        queue.push_back(key);
    } else {
        return None;
    }

    while let Some((block_idx, inst_idx)) = queue.pop_front() {
        if block_idx >= instructions.len() || inst_idx >= instructions[block_idx].len() {
            continue;
        }
        let inst = &instructions[block_idx][inst_idx];

        if let Some(result) = pred(inst) {
            return Some(result);
        }

        // Visit rightmost arguments first
        for i in (0..inst.args.len()).rev() {
            let arg = &inst.args[i];
            if arg.is_immediate() {
                continue;
            }
            if let Value::Inst(ref arg_ref) = arg {
                let key = (arg_ref.block as usize, arg_ref.inst as usize);
                if !visited.contains(&key) {
                    visited.push(key);
                    queue.push_back(key);
                }
            }
        }
    }

    None
}
