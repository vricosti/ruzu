use std::fmt;

use crate::ir::inst::Inst;
use crate::ir::location::LocationDescriptor;
use crate::ir::opcode::Opcode;
use crate::ir::terminal::Terminal;
use crate::ir::value::{InstRef, Value};

/// An IR basic block: a sequence of instructions followed by a terminal.
/// Instructions are stored in a `Vec<Inst>` arena, indexed by `InstRef(u32)`.
/// Removal is done by tombstoning (setting opcode to Void).
///
/// Matches upstream dynarmic `IR::Block`.
#[derive(Debug, Clone)]
pub struct Block {
    /// The location this block represents.
    pub location: LocationDescriptor,
    /// End location after translating the block body.
    /// Matches upstream `Block::EndLocation()`.
    pub end_location: LocationDescriptor,
    /// Arena of instructions.
    pub instructions: Vec<Inst>,
    /// Block terminator.
    pub terminal: Terminal,
    /// Number of guest cycles this block represents.
    pub cycle_count: u64,
    /// Optional condition for this block (used in conditional blocks).
    /// Set by `IsConditionPassed` when the block starts with a conditional
    /// instruction. The backend emits a condition check at block entry.
    pub cond: Option<crate::ir::cond::Cond>,
    /// Where to jump if the block condition fails. Only valid when `cond` is Some.
    /// Matches upstream `Block::ConditionFailedLocation()`.
    pub condition_failed_location: Option<LocationDescriptor>,
    /// Cycle count for the condition-failed path.
    /// Matches upstream `Block::ConditionFailedCycleCount()`.
    pub condition_failed_cycle_count: u64,
    /// Instruction count before the last conditional instruction was translated.
    /// Used to roll back side effects when a conditional non-branch instruction
    /// needs to be deferred to a new block.
    pub pre_conditional_len: usize,
}

impl Block {
    fn is_associated_pseudo_opcode(opcode: Opcode) -> bool {
        matches!(
            opcode,
            Opcode::GetCarryFromOp
                | Opcode::GetOverflowFromOp
                | Opcode::GetGEFromOp
                | Opcode::GetNZCVFromOp
                | Opcode::GetNZFromOp
                | Opcode::GetUpperFromOp
                | Opcode::GetLowerFromOp
        )
    }

    /// Create a new empty block at the given location.
    pub fn new(location: LocationDescriptor) -> Self {
        Self {
            location,
            end_location: location,
            instructions: Vec::new(),
            terminal: Terminal::Invalid,
            cycle_count: 0,
            cond: None,
            condition_failed_location: None,
            condition_failed_cycle_count: 0,
            pre_conditional_len: 0,
        }
    }

    /// Returns true if this block has a condition-failed location set.
    /// Matches upstream `Block::HasConditionFailedLocation()`.
    pub fn has_condition_failed_location(&self) -> bool {
        self.condition_failed_location.is_some()
    }

    /// Returns true if any instruction in this block writes to CPSR.
    /// Used by `cond_can_continue` to determine if a conditional block
    /// can safely continue translating.
    pub fn any_inst_writes_cpsr(&self) -> bool {
        self.instructions
            .iter()
            .any(|inst| !inst.is_tombstone() && inst.opcode.writes_cpsr())
    }

    /// Truncate the instruction list to `len`, discarding later instructions.
    /// Used to roll back side effects from a conditional instruction.
    pub fn truncate_instructions_to(&mut self, len: usize) {
        self.instructions.truncate(len);
    }

    /// Append a new instruction and return its InstRef.
    pub fn push_inst(&mut self, inst: Inst) -> InstRef {
        let idx = self.instructions.len();
        self.instructions.push(inst);
        InstRef(idx as u32)
    }

    /// Append a new instruction with the given opcode and args, return its InstRef.
    /// Also increments use_count for any InstRef arguments.
    pub fn append(&mut self, opcode: Opcode, args: &[Value]) -> InstRef {
        // Increment use counts for instruction references in arguments
        for arg in args {
            if let Value::Inst(ref_) = arg {
                self.instructions[ref_.index()].use_count += 1;
            }
        }
        let inst = Inst::new(opcode, args);
        self.push_inst(inst)
    }

    /// Insert a new instruction before `index`, renumbering later `InstRef`s.
    ///
    /// Upstream IR iterators can insert before the current instruction. The Rust
    /// arena uses indices as references, so insertion must update every existing
    /// instruction argument that points at or past the insertion point.
    pub fn insert(&mut self, index: usize, opcode: Opcode, args: &[Value]) -> InstRef {
        assert!(index <= self.instructions.len());

        for inst in &mut self.instructions {
            for arg in &mut inst.args {
                if let Value::Inst(inst_ref) = arg {
                    if inst_ref.index() >= index {
                        inst_ref.0 += 1;
                    }
                }
            }
            inst.next_pseudoop = None;
            inst.pseudo_of = inst.pseudo_of.map(|mut inst_ref| {
                if inst_ref.index() >= index {
                    inst_ref.0 += 1;
                }
                inst_ref
            });
        }

        let shifted_args: Vec<Value> = args
            .iter()
            .copied()
            .map(|value| match value {
                Value::Inst(mut inst_ref) if inst_ref.index() >= index => {
                    inst_ref.0 += 1;
                    Value::Inst(inst_ref)
                }
                value => value,
            })
            .collect();

        self.instructions
            .insert(index, Inst::new(opcode, &shifted_args));
        self.recompute_use_counts();
        InstRef(index as u32)
    }

    /// Recompute SSA use counts from current instruction arguments.
    pub fn recompute_use_counts(&mut self) {
        for inst in &mut self.instructions {
            inst.use_count = 0;
        }

        let len = self.instructions.len();
        for i in 0..len {
            let num_args = self.instructions[i].num_args();
            for arg_index in 0..num_args {
                if let Value::Inst(inst_ref) = self.instructions[i].args[arg_index] {
                    self.instructions[inst_ref.index()].use_count += 1;
                }
            }
        }
    }

    /// Get an instruction by reference.
    pub fn get(&self, r: InstRef) -> &Inst {
        &self.instructions[r.index()]
    }

    /// Get a mutable instruction by reference.
    pub fn get_mut(&mut self, r: InstRef) -> &mut Inst {
        &mut self.instructions[r.index()]
    }

    /// Resolve the *real* return type of an instruction, chasing through
    /// `Identity` chains. Mirrors upstream `Inst::GetType()`
    /// (microinstruction.cpp:624-628) — but recursive over arg references
    /// because our `Value::Inst` does not carry the inst's type inline.
    ///
    /// This must be used by anything that allocates registers (bit_width
    /// determines movsd vs movaps for spill reloads) or computes values'
    /// hardware widths. Using `Inst::return_type()` directly on an Identity
    /// returns `Opaque`, which `type_bit_width` collapses to 64 — leading to
    /// 64-bit reloads of 128-bit vectors and stale upper-half data in XMM.
    pub fn inst_real_return_type(&self, r: InstRef) -> crate::ir::Type {
        let mut cur = r;
        loop {
            let inst = &self.instructions[cur.index()];
            if inst.opcode != crate::ir::opcode::Opcode::Identity {
                return inst.return_type();
            }
            match inst.args[0] {
                Value::Inst(next) => cur = next,
                ref imm => return imm.get_type(),
            }
        }
    }

    /// Set an instruction argument while preserving SSA use counts.
    /// Matches upstream `Inst::SetArg(...)` ownership, adapted to the Rust arena.
    pub fn set_arg(&mut self, inst_ref: InstRef, idx: usize, value: Value) {
        let inst_index = inst_ref.index();
        let old_value = self.instructions[inst_index].args[idx];

        if let Value::Inst(old_ref) = old_value {
            self.instructions[old_ref.index()].use_count = self.instructions[old_ref.index()]
                .use_count
                .saturating_sub(1);
        }
        if let Value::Inst(new_ref) = value {
            self.instructions[new_ref.index()].use_count += 1;
        }

        self.instructions[inst_index].set_arg(idx, value);
    }

    /// Find the pseudo-operation of the given opcode associated with `inst_ref`.
    /// Upstream: `Inst::GetAssociatedPseudoOperation(Opcode)`.
    pub fn get_associated_pseudo_operation(
        &self,
        inst_ref: InstRef,
        opcode: Opcode,
    ) -> Option<InstRef> {
        let mut current = self.get(inst_ref).next_pseudoop;
        while let Some(pseudo_ref) = current {
            let pseudo = self.get(pseudo_ref);
            if pseudo.opcode == opcode {
                return Some(pseudo_ref);
            }
            current = pseudo.next_pseudoop;
        }
        None
    }

    /// Rebuild the intrusive pseudo-op chains from the current SSA arguments.
    ///
    /// Upstream maintains `next_pseudoop` incrementally in `Inst::Use/UndoUse`.
    /// Our Rust passes still mutate some args directly, so we restore the same
    /// invariant explicitly after optimization and before emission.
    pub fn rebuild_pseudo_op_links(&mut self) {
        for inst in &mut self.instructions {
            inst.next_pseudoop = None;
        }

        let len = self.instructions.len();
        for i in 0..len {
            if self.instructions[i].is_tombstone() {
                continue;
            }
            if !Self::is_associated_pseudo_opcode(self.instructions[i].opcode) {
                continue;
            }

            let Value::Inst(producer_ref) = self.instructions[i].args[0] else {
                continue;
            };
            if self.instructions[producer_ref.index()].is_tombstone() {
                continue;
            }

            let pseudo_ref = InstRef(i as u32);
            let mut current = producer_ref;
            loop {
                match self.instructions[current.index()].next_pseudoop {
                    Some(next_ref) => current = next_ref,
                    None => {
                        self.instructions[current.index()].next_pseudoop = Some(pseudo_ref);
                        break;
                    }
                }
            }
        }
    }

    /// Set the terminal instruction.
    pub fn set_terminal(&mut self, terminal: Terminal) {
        self.terminal = terminal;
    }

    pub fn end_location(&self) -> LocationDescriptor {
        self.end_location
    }

    pub fn set_end_location(&mut self, descriptor: LocationDescriptor) {
        self.end_location = descriptor;
    }

    /// Returns the number of (non-tombstoned) instructions.
    pub fn live_inst_count(&self) -> usize {
        self.instructions
            .iter()
            .filter(|i| !i.is_tombstone())
            .count()
    }

    /// Returns the total number of instruction slots (including tombstones).
    pub fn inst_count(&self) -> usize {
        self.instructions.len()
    }

    /// Returns true if the block has no instructions.
    pub fn is_empty(&self) -> bool {
        self.instructions.is_empty()
    }

    /// Iterate over all live (non-tombstoned) instructions with their InstRefs.
    pub fn iter_live(&self) -> impl Iterator<Item = (InstRef, &Inst)> {
        self.instructions
            .iter()
            .enumerate()
            .filter(|(_, inst)| !inst.is_tombstone())
            .map(|(i, inst)| (InstRef(i as u32), inst))
    }

    /// Replace all uses of `old` with `new_val` in instruction arguments.
    pub fn replace_uses(&mut self, old: InstRef, new_val: Value) {
        for inst in &mut self.instructions {
            for i in 0..inst.num_args() {
                if inst.args[i] == Value::Inst(old) {
                    inst.args[i] = new_val;
                }
            }
        }
    }

    /// Convert `target` into an `Identity(replacement)` instruction.
    /// Matches upstream `Inst::ReplaceUsesWith(Value)`:
    ///   1. Invalidate (clear args, decrement their use counts)
    ///   2. Set opcode to Identity
    ///   3. Set args[0] = replacement, increment replacement's use count
    ///
    /// The target's own `use_count` is preserved — other instructions still
    /// reference it. The IdentityRemovalPass later chases through Identity
    /// indirections and fixes all references.
    pub fn replace_uses_with(&mut self, target: InstRef, replacement: Value) {
        // Step 1: Invalidate — decrement use counts for target's original args
        let num_args = self.instructions[target.index()].num_args();
        for i in 0..num_args {
            if let Value::Inst(arg_ref) = self.instructions[target.index()].args[i] {
                self.instructions[arg_ref.index()].use_count = self.instructions[arg_ref.index()]
                    .use_count
                    .saturating_sub(1);
            }
        }

        // Step 2-3: Convert to Identity(replacement)
        self.instructions[target.index()].replace_with_identity(replacement);

        // Step 4: Increment replacement's use count (upstream: Use(replacement))
        if let Value::Inst(new_ref) = replacement {
            self.instructions[new_ref.index()].use_count += 1;
        }
    }

    /// Invalidate an instruction: decrement use counts for its args, then tombstone.
    /// Matches upstream `Inst::Invalidate()` — used for dead store elimination.
    pub fn invalidate(&mut self, target: InstRef) {
        let num_args = self.instructions[target.index()].num_args();
        for i in 0..num_args {
            if let Value::Inst(arg_ref) = self.instructions[target.index()].args[i] {
                self.instructions[arg_ref.index()].use_count = self.instructions[arg_ref.index()]
                    .use_count
                    .saturating_sub(1);
            }
        }
        self.instructions[target.index()].tombstone();
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Block {} (cycles: {}):", self.location, self.cycle_count)?;
        for (i, inst) in self.instructions.iter().enumerate() {
            if inst.is_tombstone() {
                continue;
            }
            let ref_ = InstRef(i as u32);
            if inst.return_type() != crate::ir::types::Type::Void {
                writeln!(f, "  {} = {}", ref_, inst)?;
            } else {
                writeln!(f, "  {}", inst)?;
            }
        }
        writeln!(f, "  terminal: {}", self.terminal)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::opcode::Opcode;

    #[test]
    fn test_block_creation_and_append() {
        let mut block = Block::new(LocationDescriptor(0x1000));

        // X2 = GetX(R2)
        let get_x2 = block.append(
            Opcode::A64GetX,
            &[Value::ImmA64Reg(crate::frontend::a64::types::Reg::R2)],
        );

        // X3 = GetX(R3)
        let get_x3 = block.append(
            Opcode::A64GetX,
            &[Value::ImmA64Reg(crate::frontend::a64::types::Reg::R3)],
        );

        // result = Add64(X2, X3, carry=false)
        let add = block.append(
            Opcode::Add64,
            &[
                Value::Inst(get_x2),
                Value::Inst(get_x3),
                Value::ImmU1(false),
            ],
        );

        // SetX(R1, result)
        block.append(
            Opcode::A64SetX,
            &[
                Value::ImmA64Reg(crate::frontend::a64::types::Reg::R1),
                Value::Inst(add),
            ],
        );

        assert_eq!(block.inst_count(), 4);
        assert_eq!(block.live_inst_count(), 4);

        // Verify use counts
        assert_eq!(block.get(get_x2).use_count, 1); // used by add
        assert_eq!(block.get(get_x3).use_count, 1); // used by add
        assert_eq!(block.get(add).use_count, 1); // used by set_x

        // Print block
        let s = format!("{}", block);
        assert!(s.contains("Add64"));
        assert!(s.contains("A64GetX"));
    }

    #[test]
    fn test_block_tombstone() {
        let mut block = Block::new(LocationDescriptor(0));
        let r = block.append(Opcode::A64GetSP, &[]);
        assert_eq!(block.live_inst_count(), 1);
        block.get_mut(r).tombstone();
        assert_eq!(block.live_inst_count(), 0);
        assert_eq!(block.inst_count(), 1); // slot still exists
    }

    #[test]
    fn test_set_arg_updates_use_counts() {
        let mut block = Block::new(LocationDescriptor(0x1000));

        let lhs = block.append(
            Opcode::A64GetX,
            &[Value::ImmA64Reg(crate::frontend::a64::types::Reg::R0)],
        );
        let rhs = block.append(
            Opcode::A64GetX,
            &[Value::ImmA64Reg(crate::frontend::a64::types::Reg::R1)],
        );
        let add = block.append(
            Opcode::Add64,
            &[Value::Inst(lhs), Value::Inst(rhs), Value::ImmU1(false)],
        );

        assert_eq!(block.instructions[lhs.index()].use_count, 1);
        assert_eq!(block.instructions[rhs.index()].use_count, 1);

        block.set_arg(add, 0, Value::Inst(rhs));

        assert_eq!(block.instructions[lhs.index()].use_count, 0);
        assert_eq!(block.instructions[rhs.index()].use_count, 2);
        assert_eq!(block.instructions[add.index()].args[0], Value::Inst(rhs));
    }

    #[test]
    fn test_rebuild_pseudo_op_links_restores_flags_chain() {
        let mut block = Block::new(LocationDescriptor(0x1000));

        let sub = block.append(
            Opcode::Sub32,
            &[Value::ImmU32(10), Value::ImmU32(1), Value::ImmU1(true)],
        );
        let nzcv = block.append(Opcode::GetNZCVFromOp, &[Value::Inst(sub)]);
        let carry = block.append(Opcode::GetCarryFromOp, &[Value::Inst(sub)]);

        block.get_mut(sub).next_pseudoop = None;
        block.get_mut(nzcv).next_pseudoop = None;
        block.get_mut(carry).next_pseudoop = None;

        block.rebuild_pseudo_op_links();

        assert_eq!(
            block.get_associated_pseudo_operation(sub, Opcode::GetNZCVFromOp),
            Some(nzcv)
        );
        assert_eq!(
            block.get_associated_pseudo_operation(sub, Opcode::GetCarryFromOp),
            Some(carry)
        );
        assert_eq!(block.get(nzcv).next_pseudoop, Some(carry));
    }
}
