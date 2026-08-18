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

/// Stable intrusive-list links for one instruction arena slot.
///
/// Eden stores these links in `Inst` through `boost::intrusive::list`. Rust
/// keeps `InstRef` as an arena index, so the equivalent links live beside the
/// arena slot and preserve constant-time insertion and removal without moving
/// the instruction itself.
#[derive(Debug, Clone, Copy)]
struct InstructionLink {
    prev: Option<u32>,
    next: Option<u32>,
}

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
    /// Intrusive logical-order links parallel to `instructions`.
    instruction_links: Vec<Option<InstructionLink>>,
    instruction_head: Option<u32>,
    instruction_tail: Option<u32>,
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
            instruction_links: Vec::new(),
            instruction_head: None,
            instruction_tail: None,
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
        let prev = self.instruction_tail;
        self.instructions.push(Some(inst));
        self.instruction_links
            .push(Some(InstructionLink { prev, next: None }));
        if let Some(prev) = prev {
            self.instruction_links[prev as usize]
                .as_mut()
                .expect("live tail must have instruction links")
                .next = Some(idx);
        } else {
            self.instruction_head = Some(idx);
        }
        self.instruction_tail = Some(idx);
        idx
    }

    /// Append a new instruction with the given opcode and arguments.
    /// Returns the index of the new instruction.
    pub fn append_new_inst(&mut self, opcode: Opcode, args: Vec<Value>) -> u32 {
        self.append_inst(Inst::new(opcode, args))
    }

    /// Insert an instruction at the given position.
    pub fn insert_inst(&mut self, position: usize, inst: Inst) {
        if position < self.instructions.len() && self.instruction_links[position].as_ref().is_some()
        {
            self.insert_inst_before(position as u32, inst);
        } else {
            self.append_inst(inst);
        }
    }

    /// Allocate a new stable slot and place it before `before` in logical order.
    pub fn insert_inst_before(&mut self, before: u32, inst: Inst) -> u32 {
        let before_link = *self
            .instruction_links
            .get(before as usize)
            .and_then(Option::as_ref)
            .expect("insert_inst_before target must be a live instruction");
        let idx = self.instructions.len() as u32;
        self.instructions.push(Some(inst));
        self.instruction_links.push(Some(InstructionLink {
            prev: before_link.prev,
            next: Some(before),
        }));
        if let Some(prev) = before_link.prev {
            self.instruction_links[prev as usize]
                .as_mut()
                .expect("live predecessor must have instruction links")
                .next = Some(idx);
        } else {
            self.instruction_head = Some(idx);
        }
        self.instruction_links[before as usize]
            .as_mut()
            .expect("insert target must remain live")
            .prev = Some(idx);
        idx
    }

    /// Allocate a new stable slot containing a copy of `base_inst` and place it
    /// before `before` in logical order.
    pub fn clone_inst_before(&mut self, before: u32, base_inst: &Inst) -> u32 {
        self.insert_inst_before(before, base_inst.clone())
    }

    /// Physically erase an instruction while preserving every other slot index.
    pub fn erase_inst(&mut self, idx: u32) {
        self.unlink_inst(idx);
        self.instructions[idx as usize] = None;
    }

    /// Unlink a stable instruction slot without destroying its instruction.
    /// This is the indexed equivalent of erasing an intrusive-list iterator
    /// before reinserting the same `Inst`, as `TryRemoveTrivialPhi` does.
    pub(crate) fn unlink_inst(&mut self, idx: u32) {
        let link = self
            .instruction_links
            .get_mut(idx as usize)
            .and_then(Option::take)
            .expect("unlink_inst target must be a live instruction");
        if let Some(prev) = link.prev {
            self.instruction_links[prev as usize]
                .as_mut()
                .expect("live predecessor must have instruction links")
                .next = link.next;
        } else {
            self.instruction_head = link.next;
        }
        if let Some(next) = link.next {
            self.instruction_links[next as usize]
                .as_mut()
                .expect("live successor must have instruction links")
                .prev = link.prev;
        } else {
            self.instruction_tail = link.prev;
        }
    }

    /// Reinsert an unlinked stable slot before `before`, or at the end when
    /// `before` is `None`.
    pub(crate) fn relink_inst_before(&mut self, idx: u32, before: Option<u32>) {
        assert!(
            self.instructions
                .get(idx as usize)
                .is_some_and(Option::is_some),
            "relink_inst_before source must retain its instruction"
        );
        assert!(
            self.instruction_links
                .get(idx as usize)
                .is_some_and(Option::is_none),
            "relink_inst_before source must be unlinked"
        );
        match before {
            Some(before) => {
                let before_link = *self
                    .instruction_links
                    .get(before as usize)
                    .and_then(Option::as_ref)
                    .expect("relink target must be a live instruction");
                self.instruction_links[idx as usize] = Some(InstructionLink {
                    prev: before_link.prev,
                    next: Some(before),
                });
                if let Some(prev) = before_link.prev {
                    self.instruction_links[prev as usize]
                        .as_mut()
                        .expect("live predecessor must have instruction links")
                        .next = Some(idx);
                } else {
                    self.instruction_head = Some(idx);
                }
                self.instruction_links[before as usize]
                    .as_mut()
                    .expect("relink target must remain live")
                    .prev = Some(idx);
            }
            None => {
                let prev = self.instruction_tail;
                self.instruction_links[idx as usize] = Some(InstructionLink { prev, next: None });
                if let Some(prev) = prev {
                    self.instruction_links[prev as usize]
                        .as_mut()
                        .expect("live tail must have instruction links")
                        .next = Some(idx);
                } else {
                    self.instruction_head = Some(idx);
                }
                self.instruction_tail = Some(idx);
            }
        }
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
        self.instruction_head.is_none()
    }

    /// Number of stable instruction slots in this block.
    pub fn len(&self) -> usize {
        self.instructions.len()
    }

    /// Number of live instructions in this block.
    pub fn live_len(&self) -> usize {
        self.indexed_iter().count()
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

    /// Iterate over instructions in reverse logical order.
    pub fn rev_iter(&self) -> impl Iterator<Item = &Inst> {
        self.indexed_rev_iter().map(|(_, inst)| inst)
    }

    /// Iterate over instructions mutably.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Inst> {
        self.indexed_iter_mut().map(|(_, inst)| inst)
    }

    /// Iterate over instructions mutably in reverse logical order.
    pub fn rev_iter_mut(&mut self) -> impl Iterator<Item = &mut Inst> {
        self.indexed_rev_iter_mut().map(|(_, inst)| inst)
    }

    /// Iterate over live instruction slots.
    pub fn indexed_iter(&self) -> impl Iterator<Item = (u32, &Inst)> {
        let links = &self.instruction_links;
        let instructions = &self.instructions;
        std::iter::successors(self.instruction_head, move |&index| {
            links[index as usize].as_ref().and_then(|link| link.next)
        })
        .map(move |index| {
            let inst = instructions[index as usize]
                .as_ref()
                .expect("linked instruction slot must be live");
            (index, inst)
        })
    }

    /// Iterate over live instruction slots mutably.
    pub fn indexed_iter_mut(&mut self) -> impl Iterator<Item = (u32, &mut Inst)> {
        let head = self.instruction_head;
        let links = &self.instruction_links;
        let len = self.instructions.len();
        let instructions = self.instructions.as_mut_ptr();
        std::iter::successors(head, move |&index| {
            links[index as usize].as_ref().and_then(|link| link.next)
        })
        .map(move |index| {
            assert!(
                (index as usize) < len,
                "linked instruction index out of bounds"
            );
            // SAFETY: the private intrusive links contain every live slot at
            // most once, so traversal never yields two references to one
            // instruction. The iterator retains the exclusive block borrow.
            let inst = unsafe {
                (*instructions.add(index as usize))
                    .as_mut()
                    .expect("linked instruction slot must be live")
            };
            (index, inst)
        })
    }

    /// Iterate over live instruction slots in reverse logical order.
    pub fn indexed_rev_iter(&self) -> impl Iterator<Item = (u32, &Inst)> {
        let links = &self.instruction_links;
        let instructions = &self.instructions;
        std::iter::successors(self.instruction_tail, move |&index| {
            links[index as usize].as_ref().and_then(|link| link.prev)
        })
        .map(move |index| {
            let inst = instructions[index as usize]
                .as_ref()
                .expect("linked instruction slot must be live");
            (index, inst)
        })
    }

    /// Iterate over live instruction slots mutably in reverse logical order.
    pub fn indexed_rev_iter_mut(&mut self) -> impl Iterator<Item = (u32, &mut Inst)> {
        let tail = self.instruction_tail;
        let links = &self.instruction_links;
        let len = self.instructions.len();
        let instructions = self.instructions.as_mut_ptr();
        std::iter::successors(tail, move |&index| {
            links[index as usize].as_ref().and_then(|link| link.prev)
        })
        .map(move |index| {
            assert!(
                (index as usize) < len,
                "linked instruction index out of bounds"
            );
            // SAFETY: see `indexed_iter_mut`; reverse traversal preserves the
            // same duplicate-free invariant.
            let inst = unsafe {
                (*instructions.add(index as usize))
                    .as_mut()
                    .expect("linked instruction slot must be live")
            };
            (index, inst)
        })
    }

    /// First live instruction in logical order.
    pub fn front(&self) -> &Inst {
        let index = self
            .instruction_head
            .expect("front() called on an empty block");
        self.inst(index)
    }

    /// First live instruction in logical order.
    pub fn front_mut(&mut self) -> &mut Inst {
        let index = self
            .instruction_head
            .expect("front_mut() called on an empty block");
        self.inst_mut(index)
    }

    /// Last live instruction in logical order.
    pub fn back(&self) -> &Inst {
        let index = self
            .instruction_tail
            .expect("back() called on an empty block");
        self.inst(index)
    }

    /// Last live instruction in logical order.
    pub fn back_mut(&mut self) -> &mut Inst {
        let index = self
            .instruction_tail
            .expect("back_mut() called on an empty block");
        self.inst_mut(index)
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
    fn clone_inst_before_copies_instruction_before_target() {
        let mut block = Block::new();
        let first = block.append_inst(Inst::new(Opcode::IAdd32, vec![]));
        let second = block.append_inst(Inst::new(Opcode::IMul32, vec![]));
        block.inst_mut(second).flags = 0x42;

        let cloned = {
            let base = block.inst(second).clone();
            block.clone_inst_before(first, &base)
        };
        let order: Vec<u32> = block.indexed_iter().map(|(index, _)| index).collect();

        assert_eq!(order, vec![cloned, first, second]);
        assert_eq!(block.inst(cloned).opcode, Opcode::IMul32);
        assert_eq!(block.inst(cloned).flags, 0x42);
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
    fn unlink_and_relink_preserve_stable_slot_identity() {
        let mut block = Block::new();
        let phi = block.append_inst(Inst::new(Opcode::Phi, vec![]));
        let first = block.append_inst(Inst::new(Opcode::IAdd32, vec![]));
        let second = block.append_inst(Inst::new(Opcode::IMul32, vec![]));

        block.unlink_inst(phi);
        assert_eq!(
            block
                .indexed_iter()
                .map(|(index, _)| index)
                .collect::<Vec<_>>(),
            vec![first, second]
        );
        assert_eq!(block.inst(phi).opcode, Opcode::Phi);

        block.relink_inst_before(phi, Some(second));
        assert_eq!(
            block
                .indexed_iter()
                .map(|(index, _)| index)
                .collect::<Vec<_>>(),
            vec![first, phi, second]
        );
        assert_eq!(phi, 0, "relinking must not change the InstRef slot");
    }

    #[test]
    fn repeated_erasure_keeps_forward_and_reverse_links_consistent() {
        let mut block = Block::new();
        let slots = (0..128)
            .map(|_| block.append_inst(Inst::new(Opcode::IAdd32, vec![])))
            .collect::<Vec<_>>();
        for &slot in slots.iter().step_by(2) {
            block.erase_inst(slot);
        }

        let forward = block
            .indexed_iter()
            .map(|(index, _)| index)
            .collect::<Vec<_>>();
        let reverse = block
            .indexed_rev_iter()
            .map(|(index, _)| index)
            .collect::<Vec<_>>();
        assert_eq!(
            forward,
            slots.iter().copied().skip(1).step_by(2).collect::<Vec<_>>()
        );
        assert_eq!(reverse, forward.iter().rev().copied().collect::<Vec<_>>());
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

    #[test]
    fn reverse_iterators_follow_logical_order() {
        let mut block = Block::new();
        let first = block.append_inst(Inst::new(Opcode::IAdd32, vec![]));
        let second = block.append_inst(Inst::new(Opcode::IMul32, vec![]));
        let inserted = block.insert_inst_before(second, Inst::new(Opcode::ISub32, vec![]));

        let order: Vec<u32> = block.indexed_rev_iter().map(|(index, _)| index).collect();
        assert_eq!(order, vec![second, inserted, first]);

        let mut mutable_order = Vec::new();
        for (index, inst) in block.indexed_rev_iter_mut() {
            mutable_order.push(index);
            inst.flags = index + 10;
        }

        assert_eq!(mutable_order, vec![second, inserted, first]);
        assert_eq!(block.inst(first).flags, first + 10);
        assert_eq!(block.inst(inserted).flags, inserted + 10);
        assert_eq!(block.inst(second).flags, second + 10);
    }

    #[test]
    fn front_and_back_follow_logical_order() {
        let mut block = Block::new();
        let first = block.append_inst(Inst::new(Opcode::IAdd32, vec![]));
        let second = block.append_inst(Inst::new(Opcode::IMul32, vec![]));
        let inserted = block.insert_inst_before(first, Inst::new(Opcode::ISub32, vec![]));

        assert_eq!(block.front().opcode, Opcode::ISub32);
        assert_eq!(block.back().opcode, Opcode::IMul32);

        block.front_mut().flags = inserted;
        block.back_mut().flags = second;

        assert_eq!(block.inst(inserted).flags, inserted);
        assert_eq!(block.inst(second).flags, second);
    }
}
