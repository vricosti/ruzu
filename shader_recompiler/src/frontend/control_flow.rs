// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Control flow graph builder for Maxwell shader binaries.
//!
//! Analyzes branch instructions (BRA, BRK, SYNC, EXIT, etc.) and convergence
//! stack operations (SSY, PBK, PCNT) to build a flat CFG of basic blocks.
//!
//! Ref: zuyu `frontend/maxwell/control_flow.cpp`

use super::maxwell_opcodes::{self, MaxwellOpcode};

/// How a basic block ends.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EndClass {
    /// Conditional or unconditional branch.
    Branch,
    /// Indirect branch (BRX/JMX).
    IndirectBranch,
    /// Subroutine call.
    Call,
    /// Return from subroutine.
    Return,
    /// Shader exit.
    Exit,
    /// Fragment kill/discard.
    Kill,
}

/// Branch condition.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Condition {
    /// Predicate index (0-6, or 7 for PT).
    pub pred: u8,
    /// Whether the predicate is negated.
    pub negated: bool,
}

impl Condition {
    pub fn always() -> Self {
        Self {
            pred: 7,
            negated: false,
        }
    }

    pub fn is_always(&self) -> bool {
        self.pred == 7 && !self.negated
    }
}

/// A basic block in the flat CFG.
#[derive(Debug, Clone)]
pub struct CfgBlock {
    /// Start instruction offset (instruction index, not byte offset).
    pub begin: u32,
    /// End instruction offset (exclusive).
    pub end: u32,
    /// How this block ends.
    pub end_class: EndClass,
    /// Branch target (true path) block index.
    pub branch_true: Option<usize>,
    /// Fall-through (false path) block index.
    pub branch_false: Option<usize>,
    /// Branch condition (None = unconditional).
    pub cond: Condition,
    /// Stack depth at entry (for SSY/PBK/PCNT tracking).
    pub stack_depth: u32,
}

/// Convergence stack entry — tracks SSY/PBK/PCNT push points.
#[derive(Debug, Clone)]
struct StackEntry {
    /// Type of stack push (SSY, PBK, PCNT).
    kind: StackKind,
    /// Target address (instruction index) pushed onto the stack.
    target: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StackKind {
    Ssy,
    Pbk,
    Pcnt,
}

/// Build a control flow graph from Maxwell instruction words.
///
/// Returns a list of `CfgBlock` representing the flat CFG.
pub fn build_cfg(instructions: &[u64]) -> Vec<CfgBlock> {
    if instructions.is_empty() {
        return Vec::new();
    }

    // Phase 1: Find all block boundaries (branch targets and next-after-branch).
    let mut block_starts = std::collections::BTreeSet::new();
    block_starts.insert(0u32);

    let mut stack: Vec<StackEntry> = Vec::new();

    for (pc, &insn) in instructions.iter().enumerate() {
        let pc = pc as u32;
        let opcode = maxwell_opcodes::decode_opcode(insn);

        match opcode {
            Some(MaxwellOpcode::BRA) => {
                let offset = decode_branch_offset(insn);
                let target = (pc as i32 + offset + 1) as u32;
                block_starts.insert(target);
                block_starts.insert(pc + 1);
            }
            Some(MaxwellOpcode::SSY) => {
                let offset = decode_branch_offset(insn);
                let target = (pc as i32 + offset + 1) as u32;
                stack.push(StackEntry {
                    kind: StackKind::Ssy,
                    target,
                });
                block_starts.insert(target);
            }
            Some(MaxwellOpcode::PBK) => {
                let offset = decode_branch_offset(insn);
                let target = (pc as i32 + offset + 1) as u32;
                stack.push(StackEntry {
                    kind: StackKind::Pbk,
                    target,
                });
                block_starts.insert(target);
            }
            Some(MaxwellOpcode::PCNT) => {
                let offset = decode_branch_offset(insn);
                let target = (pc as i32 + offset + 1) as u32;
                stack.push(StackEntry {
                    kind: StackKind::Pcnt,
                    target,
                });
                block_starts.insert(target);
            }
            Some(MaxwellOpcode::SYNC) | Some(MaxwellOpcode::BRK) | Some(MaxwellOpcode::CONT) => {
                block_starts.insert(pc + 1);
            }
            Some(MaxwellOpcode::EXIT) | Some(MaxwellOpcode::KIL) => {
                block_starts.insert(pc + 1);
            }
            Some(MaxwellOpcode::CAL) | Some(MaxwellOpcode::JCAL) => {
                block_starts.insert(pc + 1);
            }
            Some(MaxwellOpcode::RET) => {
                block_starts.insert(pc + 1);
            }
            _ => {}
        }
    }

    // Phase 2: Create blocks from boundaries.
    let starts: Vec<u32> = block_starts.into_iter().collect();
    let mut blocks = Vec::new();
    let num_insns = instructions.len() as u32;

    for i in 0..starts.len() {
        let begin = starts[i];
        if begin >= num_insns {
            break;
        }
        let end = if i + 1 < starts.len() {
            starts[i + 1].min(num_insns)
        } else {
            num_insns
        };

        // Determine end class from last instruction in block.
        let last_pc = end - 1;
        let last_insn = instructions[last_pc as usize];
        let last_opcode = maxwell_opcodes::decode_opcode(last_insn);

        let (end_class, cond) = match last_opcode {
            Some(MaxwellOpcode::BRA) => {
                let c = decode_predicate(last_insn);
                (EndClass::Branch, c)
            }
            Some(MaxwellOpcode::EXIT) => {
                let c = decode_predicate(last_insn);
                (EndClass::Exit, c)
            }
            Some(MaxwellOpcode::KIL) => (EndClass::Kill, Condition::always()),
            Some(MaxwellOpcode::SYNC) => {
                let c = decode_predicate(last_insn);
                (EndClass::Branch, c)
            }
            Some(MaxwellOpcode::BRK) => {
                let c = decode_predicate(last_insn);
                (EndClass::Branch, c)
            }
            Some(MaxwellOpcode::CONT) => {
                let c = decode_predicate(last_insn);
                (EndClass::Branch, c)
            }
            Some(MaxwellOpcode::CAL) | Some(MaxwellOpcode::JCAL) => {
                (EndClass::Call, Condition::always())
            }
            Some(MaxwellOpcode::RET) => (EndClass::Return, Condition::always()),
            Some(MaxwellOpcode::BRX) | Some(MaxwellOpcode::JMX) => {
                (EndClass::IndirectBranch, Condition::always())
            }
            _ => (EndClass::Branch, Condition::always()),
        };

        blocks.push(CfgBlock {
            begin,
            end,
            end_class,
            branch_true: None,
            branch_false: None,
            cond,
            stack_depth: 0,
        });
    }

    // Phase 3: Link blocks (resolve branch targets to block indices).
    let block_start_to_index: std::collections::HashMap<u32, usize> = blocks
        .iter()
        .enumerate()
        .map(|(i, b)| (b.begin, i))
        .collect();

    for i in 0..blocks.len() {
        let last_pc = blocks[i].end - 1;
        let last_insn = instructions[last_pc as usize];
        let last_opcode = maxwell_opcodes::decode_opcode(last_insn);

        match last_opcode {
            Some(MaxwellOpcode::BRA) => {
                let offset = decode_branch_offset(last_insn);
                let target = (last_pc as i32 + offset + 1) as u32;
                blocks[i].branch_true = block_start_to_index.get(&target).copied();
                // Fall-through for conditional branch
                if !blocks[i].cond.is_always() {
                    let next = blocks[i].end;
                    blocks[i].branch_false = block_start_to_index.get(&next).copied();
                }
            }
            Some(MaxwellOpcode::EXIT) | Some(MaxwellOpcode::KIL) | Some(MaxwellOpcode::RET) => {
                // No successors (or conditional fall-through)
                if !blocks[i].cond.is_always() {
                    let next = blocks[i].end;
                    blocks[i].branch_false = block_start_to_index.get(&next).copied();
                }
            }
            _ => {
                // Fall through to next block.
                let next = blocks[i].end;
                blocks[i].branch_true = block_start_to_index.get(&next).copied();
            }
        }
    }

    blocks
}

/// Decode the branch offset from a branch instruction (BRA, SSY, PBK, PCNT).
///
/// The offset is a signed 24-bit value in bits [23:5] of the instruction word,
/// or a different field depending on the encoding.
fn decode_branch_offset(insn: u64) -> i32 {
    // Maxwell branch offset: bits [23:5] sign-extended, relative to current PC.
    // Actually the encoding varies: for BRA the offset is in bits [23:20]+[19:5].
    // Simplified: extract bits [23:0] as signed offset.
    let raw = (insn & 0x00FFFFFF) as u32;
    // Sign-extend from 24 bits
    if raw & 0x800000 != 0 {
        (raw | 0xFF000000) as i32
    } else {
        raw as i32
    }
}

/// Decode the predicate condition from bits [19:16] of the instruction.
fn decode_predicate(insn: u64) -> Condition {
    let pred_bits = ((insn >> 16) & 0xF) as u8;
    let pred = pred_bits & 0x7;
    let negated = pred_bits & 0x8 != 0;
    Condition { pred, negated }
}
