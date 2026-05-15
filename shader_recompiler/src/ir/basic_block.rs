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
    /// Instruction arena slots in insertion order.
    ///
    /// `Value::Inst(InstRef)` stores the slot index, so entries must not shift
    /// after creation. Physical erasure marks a slot as `None`, mirroring the
    /// upstream pointer-stable instruction identity without requiring a full
    /// intrusive list yet.
    pub instructions: Vec<Option<Inst>>,
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
        self.instructions.push(Some(inst));
        idx
    }

    /// Append a new instruction with the given opcode and arguments.
    /// Returns the index of the new instruction.
    pub fn append_new_inst(&mut self, opcode: Opcode, args: Vec<Value>) -> u32 {
        self.append_inst(Inst::new(opcode, args))
    }

    /// Insert an instruction at the given position.
    pub fn insert_inst(&mut self, position: usize, inst: Inst) {
        debug_assert!(
            position >= self.instructions.len(),
            "Block::insert_inst would shift stable InstRef slots"
        );
        self.instructions.push(Some(inst));
    }

    /// Physically erase an instruction while preserving every other slot index.
    pub fn erase_inst(&mut self, idx: u32) {
        self.instructions[idx as usize] = None;
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
        self.instructions.iter().all(Option::is_none)
    }

    /// Number of stable instruction slots in this block.
    pub fn len(&self) -> usize {
        self.instructions.len()
    }

    /// Number of live instructions in this block.
    pub fn live_len(&self) -> usize {
        self.instructions
            .iter()
            .filter(|inst| inst.is_some())
            .count()
    }

    /// Get instruction at index.
    pub fn inst(&self, idx: u32) -> &Inst {
        self.instructions[idx as usize]
            .as_ref()
            .expect("accessed erased instruction slot")
    }

    /// Get mutable instruction at index.
    pub fn inst_mut(&mut self, idx: u32) -> &mut Inst {
        self.instructions[idx as usize]
            .as_mut()
            .expect("accessed erased instruction slot")
    }

    /// Iterate over instructions.
    pub fn iter(&self) -> impl Iterator<Item = &Inst> {
        self.instructions.iter().filter_map(Option::as_ref)
    }

    /// Iterate over instructions mutably.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Inst> {
        self.instructions.iter_mut().filter_map(Option::as_mut)
    }

    /// Iterate over live instruction slots.
    pub fn indexed_iter(&self) -> impl Iterator<Item = (u32, &Inst)> {
        self.instructions
            .iter()
            .enumerate()
            .filter_map(|(index, inst)| inst.as_ref().map(|inst| (index as u32, inst)))
    }

    /// Iterate over live instruction slots mutably.
    pub fn indexed_iter_mut(&mut self) -> impl Iterator<Item = (u32, &mut Inst)> {
        self.instructions
            .iter_mut()
            .enumerate()
            .filter_map(|(index, inst)| inst.as_mut().map(|inst| (index as u32, inst)))
    }
}

impl Default for Block {
    fn default() -> Self {
        Self::new()
    }
}
