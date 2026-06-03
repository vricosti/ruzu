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
    /// Logical instruction order.
    ///
    /// Slot indices stay stable because `Value::Inst(InstRef)` stores them.
    /// This side list lets passes insert a newly allocated slot before an
    /// existing slot, matching upstream's pointer-stable intrusive list model
    /// without shifting `instructions`.
    instruction_order: Vec<u32>,
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
            instruction_order: Vec::new(),
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
        self.instruction_order.push(idx);
        idx
    }

    /// Append a new instruction with the given opcode and arguments.
    /// Returns the index of the new instruction.
    pub fn append_new_inst(&mut self, opcode: Opcode, args: Vec<Value>) -> u32 {
        self.append_inst(Inst::new(opcode, args))
    }

    /// Insert an instruction at the given position.
    pub fn insert_inst(&mut self, position: usize, inst: Inst) {
        if position < self.instructions.len() {
            self.insert_inst_before(position as u32, inst);
        } else {
            self.append_inst(inst);
        }
    }

    /// Allocate a new stable slot and place it before `before` in logical order.
    pub fn insert_inst_before(&mut self, before: u32, inst: Inst) -> u32 {
        let idx = self.instructions.len() as u32;
        self.instructions.push(Some(inst));
        let insert_pos = self
            .instruction_order
            .iter()
            .position(|&slot| slot == before)
            .unwrap_or(self.instruction_order.len());
        self.instruction_order.insert(insert_pos, idx);
        idx
    }

    /// Physically erase an instruction while preserving every other slot index.
    pub fn erase_inst(&mut self, idx: u32) {
        self.instructions[idx as usize] = None;
        self.instruction_order.retain(|&slot| slot != idx);
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
        self.indexed_iter().map(|(_, inst)| inst)
    }

    /// Iterate over instructions mutably.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Inst> {
        self.indexed_iter_mut().map(|(_, inst)| inst)
    }

    /// Iterate over live instruction slots.
    pub fn indexed_iter(&self) -> impl Iterator<Item = (u32, &Inst)> {
        self.instruction_order.iter().copied().filter_map(|index| {
            self.instructions
                .get(index as usize)
                .and_then(Option::as_ref)
                .map(|inst| (index, inst))
        })
    }

    /// Iterate over live instruction slots mutably.
    pub fn indexed_iter_mut(&mut self) -> impl Iterator<Item = (u32, &mut Inst)> {
        let order = self.instruction_order.clone();
        let len = self.instructions.len();
        let instructions = self.instructions.as_mut_ptr();
        order.into_iter().filter_map(move |index| {
            if index as usize >= len {
                return None;
            }
            // `instruction_order` is private and maintained as a duplicate-free
            // list of live stable slots by `append_inst`, `insert_inst_before`,
            // and `erase_inst`, so yielding mutable references in this order
            // cannot alias.
            unsafe {
                (*instructions.add(index as usize))
                    .as_mut()
                    .map(|inst| (index, inst))
            }
        })
    }
}

impl Default for Block {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn insert_inst_before_preserves_slots_and_changes_logical_order() {
        let mut block = Block::new();
        let first = block.append_inst(Inst::new(Opcode::IAdd32, vec![]));
        let second = block.append_inst(Inst::new(Opcode::IMul32, vec![]));

        let inserted = block.insert_inst_before(second, Inst::new(Opcode::ISub32, vec![]));
        let order: Vec<u32> = block.indexed_iter().map(|(index, _)| index).collect();

        assert_eq!(first, 0);
        assert_eq!(second, 1);
        assert_eq!(inserted, 2);
        assert_eq!(order, vec![0, 2, 1]);
        assert_eq!(block.inst(second).opcode, Opcode::IMul32);
    }

    #[test]
    fn insert_inst_before_first_inserts_at_logical_beginning() {
        let mut block = Block::new();
        let first = block.append_inst(Inst::new(Opcode::IAdd32, vec![]));
        let second = block.append_inst(Inst::new(Opcode::IMul32, vec![]));

        let inserted = block.insert_inst_before(first, Inst::new(Opcode::ISub32, vec![]));
        let order: Vec<u32> = block.indexed_iter().map(|(index, _)| index).collect();

        assert_eq!(inserted, 2);
        assert_eq!(order, vec![2, 0, 1]);
        assert_eq!(block.inst(second).opcode, Opcode::IMul32);
    }

    #[test]
    fn repeated_insert_inst_before_same_slot_preserves_insertion_order() {
        let mut block = Block::new();
        let first = block.append_inst(Inst::new(Opcode::IAdd32, vec![]));
        let second = block.append_inst(Inst::new(Opcode::IMul32, vec![]));

        let a = block.insert_inst_before(second, Inst::new(Opcode::ISub32, vec![]));
        let b = block.insert_inst_before(second, Inst::new(Opcode::BitwiseOr32, vec![]));
        let c = block.insert_inst_before(second, Inst::new(Opcode::BitwiseAnd32, vec![]));
        let order: Vec<u32> = block.indexed_iter().map(|(index, _)| index).collect();

        assert_eq!(order, vec![first, a, b, c, second]);
    }

    #[test]
    fn erase_inst_removes_slot_from_logical_order() {
        let mut block = Block::new();
        let first = block.append_inst(Inst::new(Opcode::IAdd32, vec![]));
        let second = block.append_inst(Inst::new(Opcode::IMul32, vec![]));
        let third = block.append_inst(Inst::new(Opcode::ISub32, vec![]));

        block.erase_inst(second);
        let order: Vec<u32> = block.indexed_iter().map(|(index, _)| index).collect();

        assert_eq!(order, vec![first, third]);
        assert!(block.instructions[second as usize].is_none());
    }

    #[test]
    fn indexed_iter_mut_follows_logical_order() {
        let mut block = Block::new();
        let first = block.append_inst(Inst::new(Opcode::IAdd32, vec![]));
        let second = block.append_inst(Inst::new(Opcode::IMul32, vec![]));
        let inserted = block.insert_inst_before(second, Inst::new(Opcode::ISub32, vec![]));

        let mut visited = Vec::new();
        for (index, inst) in block.indexed_iter_mut() {
            visited.push(index);
            inst.flags = index;
        }

        assert_eq!(visited, vec![first, inserted, second]);
        assert_eq!(block.inst(first).flags, first);
        assert_eq!(block.inst(inserted).flags, inserted);
        assert_eq!(block.inst(second).flags, second);
    }
}
