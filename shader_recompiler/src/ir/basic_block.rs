// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! IR Basic Block — a sequence of instructions with single entry, single exit.
//!
//! Matches zuyu's `Block` from `basic_block.h`. Each block owns a list of
//! instructions, tracks SSA register values during construction, and records
//! CFG edges (predecessors/successors).

use super::instruction::Inst;
use super::opcodes::Opcode;
use super::value::{Reg, Value};

/// A basic block in the IR program.
#[derive(Debug, Clone)]
pub struct Block {
    /// Instructions in this block, in order of execution.
    pub instructions: Vec<Inst>,
    /// Immediate predecessor block indices.
    pub imm_predecessors: Vec<u32>,
    /// Immediate successor block indices.
    pub imm_successors: Vec<u32>,
    /// SSA register values at the current point during construction.
    /// Indexed by register number (0..255).
    pub ssa_reg_values: Vec<Value>,
    /// Whether SSA construction for this block is sealed (all predecessors known).
    pub is_ssa_sealed: bool,
    /// Block ordering for structured control flow.
    pub order: u32,
    /// Backend-specific definition (e.g., SPIR-V label ID).
    pub definition: u32,
}

impl Block {
    /// Create a new empty block.
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            imm_predecessors: Vec::new(),
            imm_successors: Vec::new(),
            ssa_reg_values: vec![Value::Void; Reg::NUM_REGS],
            is_ssa_sealed: false,
            order: 0,
            definition: 0,
        }
    }

    /// Append a new instruction to the end of this block.
    /// Returns the index of the new instruction within this block.
    pub fn append_inst(&mut self, inst: Inst) -> u32 {
        let idx = self.instructions.len() as u32;
        self.instructions.push(inst);
        idx
    }

    /// Append a new instruction with the given opcode and arguments.
    /// Returns the index of the new instruction.
    pub fn append_new_inst(&mut self, opcode: Opcode, args: Vec<Value>) -> u32 {
        self.append_inst(Inst::new(opcode, args))
    }

    /// Insert an instruction at the given position.
    pub fn insert_inst(&mut self, position: usize, inst: Inst) {
        self.instructions.insert(position, inst);
    }

    /// Add a successor block (CFG edge).
    pub fn add_successor(&mut self, block_idx: u32) {
        if !self.imm_successors.contains(&block_idx) {
            self.imm_successors.push(block_idx);
        }
    }

    /// Add a predecessor block (CFG edge).
    pub fn add_predecessor(&mut self, block_idx: u32) {
        if !self.imm_predecessors.contains(&block_idx) {
            self.imm_predecessors.push(block_idx);
        }
    }

    /// Set the SSA value for a register at the current construction point.
    pub fn set_ssa_reg_value(&mut self, reg: Reg, value: Value) {
        self.ssa_reg_values[reg.index()] = value;
    }

    /// Get the current SSA value for a register.
    pub fn ssa_reg_value(&self, reg: Reg) -> Value {
        self.ssa_reg_values[reg.index()].clone()
    }

    /// Seal this block (all predecessors are now known).
    pub fn seal(&mut self) {
        self.is_ssa_sealed = true;
    }

    /// Whether this block is empty (no instructions).
    pub fn is_empty(&self) -> bool {
        self.instructions.is_empty()
    }

    /// Number of instructions in this block.
    pub fn len(&self) -> usize {
        self.instructions.len()
    }

    /// Get instruction at index.
    pub fn inst(&self, idx: u32) -> &Inst {
        &self.instructions[idx as usize]
    }

    /// Get mutable instruction at index.
    pub fn inst_mut(&mut self, idx: u32) -> &mut Inst {
        &mut self.instructions[idx as usize]
    }

    /// Iterate over instructions.
    pub fn iter(&self) -> std::slice::Iter<'_, Inst> {
        self.instructions.iter()
    }

    /// Iterate over instructions mutably.
    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, Inst> {
        self.instructions.iter_mut()
    }
}

impl Default for Block {
    fn default() -> Self {
        Self::new()
    }
}
