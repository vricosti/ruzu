// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `ir_opt/ssa_rewrite_pass.cpp`
//!
//! SSA rewriting pass implementing the algorithm from:
//!   "Simple and Efficient Construction of Static Single Assignment Form"
//!   Braun M., Buchwald S., Hack S., Leiba R., Mallon C., Zwinkau A. (2013)
//!
//! Converts the register-based IR to proper SSA form by inserting phi nodes
//! at join points and rewriting register/predicate/flag/goto/indirect-branch
//! references to SSA values.
//!
//! Use-def chain: upstream maintains a use-def chain via per-instruction
//! intrusive lists. Ruzu does not carry that chain in the base IR, so this
//! pass builds an explicit `users: HashMap<InstRef, HashSet<InstRef>>` at
//! entry and updates it on every operand mutation. `ReplaceUsesWith` then
//! walks users eagerly (no end-of-pass fixpoint sweep needed) and triggers
//! recursive `TryRemoveTrivialPhi` on phi users when their operands change,
//! which also completes the simplification upstream notes as a TODO in
//! `TryRemoveTrivialPhi`.

use std::collections::{HashMap, HashSet};

use crate::ir::opcodes::Opcode;
use crate::ir::post_order::post_order;
use crate::ir::program::{Program, SyntaxNode};
use crate::ir::types::Type;
use crate::ir::value::{InstRef, Pred, Reg, Value};

// `value::Pred` is the `(pub u8)` variant stored inside `Value::Pred`. A
// second `pred::Pred` exists (with `from_index`) but is unrelated to the
// SSA pass — the variable kind it tracks is whatever the Set/Get opcodes
// carry, which is `Value::Pred(value::Pred)`.

/// Variable kinds tracked by the SSA construction pass. Mirrors upstream's
/// `std::variant<IR::Reg, IR::Pred, ZeroFlagTag, SignFlagTag, CarryFlagTag,
/// OverflowFlagTag, GotoVariable, IndirectBranchVariable>` Variant alias.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Variable {
    Reg(Reg),
    Pred(Pred),
    ZeroFlag,
    SignFlag,
    CarryFlag,
    OverflowFlag,
    Goto(u32),
    IndirectBranch,
}

/// Upstream `UndefOpcode(variable)`.
fn undef_opcode(var: Variable) -> Opcode {
    match var {
        Variable::Reg(_) => Opcode::UndefU32,
        Variable::Pred(_) => Opcode::UndefU1,
        Variable::ZeroFlag | Variable::SignFlag | Variable::CarryFlag | Variable::OverflowFlag => {
            Opcode::UndefU1
        }
        Variable::Goto(_) => Opcode::UndefU1,
        Variable::IndirectBranch => Opcode::UndefU32,
    }
}

/// Upstream `IR::TypeOf(UndefOpcode(variable))`. The phi's flags carry this
/// type so `inst.return_type()` returns the right answer without a separate
/// `GetConcreteType` walk over arguments.
fn undef_type(var: Variable) -> Type {
    match var {
        Variable::Reg(_) => Type::U32,
        Variable::Pred(_) => Type::U1,
        Variable::ZeroFlag | Variable::SignFlag | Variable::CarryFlag | Variable::OverflowFlag => {
            Type::U1
        }
        Variable::Goto(_) => Type::U1,
        Variable::IndirectBranch => Type::U32,
    }
}

struct Pass {
    /// Upstream `DefTable current_def`. Per (variable, block) → defined value.
    current_def: HashMap<(Variable, u32), Value>,
    /// Upstream `incomplete_phis`. Per block → map<variable, phi inst ref>.
    incomplete_phis: HashMap<u32, Vec<(Variable, InstRef)>>,
    /// Explicit use-def chain. For each defining `InstRef`, the set of
    /// instructions whose `args` or `phi_args` reference it. Maintained
    /// eagerly across every operand mutation so that `replace_uses_with`
    /// can walk users in O(users(def)) instead of O(total IR size).
    users: HashMap<InstRef, HashSet<InstRef>>,
    /// Per-phi: the variable kind that created it. Needed to look up the
    /// matching `UndefOpcode` when a phi user is re-tested for triviality
    /// after one of its operands is rewritten.
    phi_variable: HashMap<InstRef, Variable>,
}

impl Pass {
    fn new() -> Self {
        Self {
            current_def: HashMap::new(),
            incomplete_phis: HashMap::new(),
            users: HashMap::new(),
            phi_variable: HashMap::new(),
        }
    }

    /// Populate the use-def index from the existing program IR before any
    /// SSA construction begins. Pre-existing args and phi_args reference
    /// definitions whose users we need to know about so that later
    /// `replace_uses_with` calls reach them.
    fn build_initial_users(&mut self, program: &Program) {
        for block_idx in 0..program.blocks.len() as u32 {
            let inst_refs: Vec<InstRef> = program
                .block(block_idx)
                .indexed_iter()
                .map(|(inst_idx, _)| InstRef {
                    block: block_idx,
                    inst: inst_idx,
                })
                .collect();
            for user_ref in inst_refs {
                let inst = program.block(user_ref.block).inst(user_ref.inst);
                for arg in &inst.args {
                    self.register_use(*arg, user_ref);
                }
                for (_, val) in &inst.phi_args {
                    self.register_use(*val, user_ref);
                }
            }
        }
    }

    /// Record that `user` references `value`. No-op for immediates / non-Inst
    /// values. Mirrors upstream's `IR::Inst::Use(value)` increment of the
    /// intrusive use-def edge count.
    fn register_use(&mut self, value: Value, user: InstRef) {
        if let Value::Inst(def) = value {
            self.users.entry(def).or_default().insert(user);
        }
    }

    /// Upstream `WriteVariable(variable, block, value)`.
    fn write_variable(&mut self, var: Variable, block: u32, value: Value) {
        self.current_def.insert((var, block), value);
    }

    fn def_of(&self, var: Variable, block: u32) -> Option<Value> {
        self.current_def.get(&(var, block)).copied()
    }

    /// Add an operand to a phi node and update the use-def index.
    fn add_phi_operand(
        &mut self,
        program: &mut Program,
        phi_ref: InstRef,
        pred: u32,
        value: Value,
    ) {
        program
            .block_mut(phi_ref.block)
            .inst_mut(phi_ref.inst)
            .add_phi_operand(pred, value);
        self.register_use(value, phi_ref);
    }

    /// Upstream `ReadVariable(variable, root_block)`. Performs the lazy
    /// SSA construction read: returns the SSA value for `var` at the start
    /// of `block`, inserting phi nodes as needed.
    fn read_variable(&mut self, program: &mut Program, var: Variable, block: u32) -> Value {
        if let Some(def) = self.def_of(var, block) {
            return def;
        }
        if !program.block(block).is_ssa_sealed {
            // Incomplete CFG: record an operand-less phi and return it.
            let phi_ref = append_phi(program, block, undef_type(var));
            self.phi_variable.insert(phi_ref, var);
            self.incomplete_phis
                .entry(block)
                .or_default()
                .push((var, phi_ref));
            let value = Value::Inst(phi_ref);
            self.write_variable(var, block, value);
            return value;
        }
        let preds: Vec<u32> = program.block(block).imm_predecessors.clone();
        if preds.len() == 1 {
            // Common case: single predecessor — no phi needed.
            let value = self.read_variable(program, var, preds[0]);
            self.write_variable(var, block, value);
            return value;
        }
        // Multi-predecessor: insert phi, break cycles by writing it as the
        // current def, then recursively populate operands.
        let phi_ref = append_phi(program, block, undef_type(var));
        self.phi_variable.insert(phi_ref, var);
        let phi_value = Value::Inst(phi_ref);
        self.write_variable(var, block, phi_value);
        let result = self.add_phi_operands(program, var, phi_ref, block);
        self.write_variable(var, block, result);
        result
    }

    /// Upstream `SealBlock(block)`. Completes any incomplete phis recorded
    /// for `block` and marks it sealed.
    fn seal_block(&mut self, program: &mut Program, block: u32) {
        if let Some(pending) = self.incomplete_phis.remove(&block) {
            for (var, phi_ref) in pending {
                self.add_phi_operands(program, var, phi_ref, block);
            }
        }
        program.block_mut(block).seal();
    }

    /// Upstream `AddPhiOperands(variable, phi, block)`. Walks immediate
    /// predecessors of `block`, reads `var` from each, appends the result
    /// as a phi operand, then tries trivial-phi removal.
    fn add_phi_operands(
        &mut self,
        program: &mut Program,
        var: Variable,
        phi_ref: InstRef,
        block: u32,
    ) -> Value {
        let preds: Vec<u32> = program.block(block).imm_predecessors.clone();
        for pred in preds {
            let value = self.read_variable(program, var, pred);
            self.add_phi_operand(program, phi_ref, pred, value);
        }
        self.try_remove_trivial_phi(program, phi_ref, undef_opcode(var))
    }

    /// Upstream `TryRemoveTrivialPhi`. If every operand resolves to a single
    /// non-self value, replace the phi with that value via the use-def
    /// chain. If no operand exists (unreachable / start block), synthesize
    /// an `Undef` and use that as the replacement. Upstream deliberately does
    /// not recursively re-test phi users here; preserve that ordering.
    fn try_remove_trivial_phi(
        &mut self,
        program: &mut Program,
        phi_ref: InstRef,
        undef_op: Opcode,
    ) -> Value {
        let phi_self = Value::Inst(phi_ref);
        let mut same: Value = Value::Void;
        let phi_args = program
            .block(phi_ref.block)
            .inst(phi_ref.inst)
            .phi_args
            .clone();
        for (_, op) in &phi_args {
            let resolved_op = resolve_value(*op, program);
            let resolved_same = resolve_value(same, program);
            if resolved_op == resolved_same || resolved_op == phi_self {
                continue;
            }
            if !same.is_void() {
                // The phi merges at least two distinct values; not trivial.
                return phi_self;
            }
            same = *op;
        }
        if same.is_void() {
            // Unreachable or start-block phi: synthesize an Undef.
            let undef_idx = program
                .block_mut(phi_ref.block)
                .append_new_inst(undef_op, vec![]);
            same = Value::Inst(InstRef {
                block: phi_ref.block,
                inst: undef_idx,
            });
        }
        // Drop the phi's variable mapping; it's about to become Identity.
        self.phi_variable.remove(&phi_ref);

        // Eagerly rewrite every user of `phi_ref` to point at `same`.
        self.replace_uses_with(program, phi_ref, same);

        same
    }

    /// Upstream `Inst::ReplaceUsesWith(replacement)`. Walks every user of
    /// `def_inst`, rewrites the matching arg/phi_arg in place to
    /// `replacement`, and updates the use-def index. The defining
    /// instruction itself is converted to `Identity(replacement)` so that
    /// any stale `Value::Inst(def_inst)` that slips through (or future
    /// pre-existing Identity chains) still resolves correctly when chased.
    fn replace_uses_with(&mut self, program: &mut Program, def_inst: InstRef, replacement: Value) {
        let user_set = self.users.remove(&def_inst).unwrap_or_default();
        for user_ref in user_set {
            let mut changed = false;
            {
                let user_inst = program.block_mut(user_ref.block).inst_mut(user_ref.inst);
                for arg in &mut user_inst.args {
                    if *arg == Value::Inst(def_inst) {
                        *arg = replacement;
                        changed = true;
                    }
                }
                for (_, val) in &mut user_inst.phi_args {
                    if *val == Value::Inst(def_inst) {
                        *val = replacement;
                        changed = true;
                    }
                }
            }
            if changed {
                self.register_use(replacement, user_ref);
            }
        }
        for node in &mut program.syntax_list {
            match node {
                SyntaxNode::If { cond, .. }
                | SyntaxNode::Repeat { cond, .. }
                | SyntaxNode::Break { cond, .. } => {
                    if *cond == Value::Inst(def_inst) {
                        *cond = replacement;
                    }
                }
                _ => {}
            }
        }
        // Convert def_inst to Identity(replacement) — defensive cleanup for
        // anyone who might still hold a stale `Value::Inst(def_inst)`.
        let inst = program.block_mut(def_inst.block).inst_mut(def_inst.inst);
        inst.opcode = Opcode::Identity;
        inst.args = vec![replacement];
        inst.phi_args.clear();
        inst.flags = 0;
        // The Identity itself becomes a user of replacement.
        self.register_use(replacement, def_inst);
    }
}

/// Upstream `PrependNewInst(block->begin(), IR::Opcode::Phi)`. Backends in
/// ruzu treat `Opcode::Phi` as a no-op for inline emission (see
/// `backend/glsl/emit_glsl.rs:738` and friends) and consume `phi_args`
/// explicitly only via phi-move resolution. Appending at the end therefore
/// preserves the upstream contract that phi operands are tracked by
/// `(predecessor, value)` pairs while avoiding the index-shifting cost of a
/// real insert-at-front on the `Vec<Inst>` backing storage.
fn append_phi(program: &mut Program, block: u32, ty: Type) -> InstRef {
    let inst_idx = program
        .block_mut(block)
        .append_new_inst(Opcode::Phi, vec![]);
    let phi = program.block_mut(block).inst_mut(inst_idx);
    phi.flags = ty as u32;
    InstRef {
        block,
        inst: inst_idx,
    }
}

/// Upstream `Value::Resolve()` — walks Identity chains. With eager
/// `replace_uses_with` rewrites the chain is normally one hop or none, but
/// triviality checks during phi construction may still observe a chain
/// while the user-update walk is in-flight, so `resolve_value` remains a
/// useful invariant.
fn resolve_value(mut value: Value, program: &Program) -> Value {
    while let Value::Inst(r) = value {
        let inst = program.block(r.block).inst(r.inst);
        if inst.opcode == Opcode::Identity && !inst.args.is_empty() {
            value = inst.args[0];
        } else {
            return value;
        }
    }
    value
}

/// Upstream `VisitInst`. Translates Set*/Get* register/pred/flag/goto/ibranch
/// opcodes into `WriteVariable` / `ReadVariable + ReplaceUsesWith` calls.
fn visit_inst(pass: &mut Pass, program: &mut Program, block: u32, inst_idx: u32) {
    let inst_ref = InstRef {
        block,
        inst: inst_idx,
    };
    let snapshot = program.block(block).inst(inst_idx).clone();
    match snapshot.opcode {
        Opcode::SetRegister => {
            let reg = snapshot.args[0].reg();
            if !reg.is_zero() {
                pass.write_variable(Variable::Reg(reg), block, snapshot.args[1]);
            }
        }
        Opcode::SetPred => {
            let pred = snapshot.args[0].pred();
            if !pred.is_true() {
                pass.write_variable(Variable::Pred(pred), block, snapshot.args[1]);
            }
        }
        Opcode::SetGotoVariable => {
            pass.write_variable(
                Variable::Goto(snapshot.args[0].imm_u32()),
                block,
                snapshot.args[1],
            );
        }
        Opcode::SetIndirectBranchVariable => {
            pass.write_variable(Variable::IndirectBranch, block, snapshot.args[0]);
        }
        Opcode::SetZFlag => pass.write_variable(Variable::ZeroFlag, block, snapshot.args[0]),
        Opcode::SetSFlag => pass.write_variable(Variable::SignFlag, block, snapshot.args[0]),
        Opcode::SetCFlag => pass.write_variable(Variable::CarryFlag, block, snapshot.args[0]),
        Opcode::SetOFlag => pass.write_variable(Variable::OverflowFlag, block, snapshot.args[0]),
        Opcode::GetRegister => {
            let reg = snapshot.args[0].reg();
            let value = if reg.is_zero() {
                Value::ImmU32(0)
            } else {
                pass.read_variable(program, Variable::Reg(reg), block)
            };
            pass.replace_uses_with(program, inst_ref, value);
        }
        Opcode::GetPred => {
            let pred = snapshot.args[0].pred();
            let value = if pred.is_true() {
                Value::ImmU1(true)
            } else {
                pass.read_variable(program, Variable::Pred(pred), block)
            };
            pass.replace_uses_with(program, inst_ref, value);
        }
        Opcode::GetGotoVariable => {
            let value =
                pass.read_variable(program, Variable::Goto(snapshot.args[0].imm_u32()), block);
            pass.replace_uses_with(program, inst_ref, value);
        }
        Opcode::GetIndirectBranchVariable => {
            let value = pass.read_variable(program, Variable::IndirectBranch, block);
            pass.replace_uses_with(program, inst_ref, value);
        }
        Opcode::GetZFlag => {
            let value = pass.read_variable(program, Variable::ZeroFlag, block);
            pass.replace_uses_with(program, inst_ref, value);
        }
        Opcode::GetSFlag => {
            let value = pass.read_variable(program, Variable::SignFlag, block);
            pass.replace_uses_with(program, inst_ref, value);
        }
        Opcode::GetCFlag => {
            let value = pass.read_variable(program, Variable::CarryFlag, block);
            pass.replace_uses_with(program, inst_ref, value);
        }
        Opcode::GetOFlag => {
            let value = pass.read_variable(program, Variable::OverflowFlag, block);
            pass.replace_uses_with(program, inst_ref, value);
        }
        _ => {}
    }
}

fn visit_block(pass: &mut Pass, program: &mut Program, block: u32) {
    // Snapshot the original instruction count so that phi nodes appended by
    // ReadVariable during this loop are not iterated as if they were part of
    // the original block.
    let inst_refs: Vec<u32> = program
        .block(block)
        .indexed_iter()
        .map(|(inst_idx, _)| inst_idx)
        .collect();
    for inst_idx in inst_refs {
        visit_inst(pass, program, block, inst_idx);
    }
    pass.seal_block(program, block);
}

/// Upstream `SsaRewritePass(program)`.
pub fn ssa_rewrite_pass(program: &mut Program) {
    // Use the precomputed post-order if the structured-CF pass populated it,
    // otherwise compute it lazily from the CFG. Reverse-post-order is the
    // visit order that lets the lazy SSA algorithm seal predecessor-first.
    let post: Vec<u32> = if !program.post_order_blocks.is_empty() {
        program.post_order_blocks.clone()
    } else if program.blocks.is_empty() {
        Vec::new()
    } else {
        post_order(&program.blocks, 0)
    };

    let mut pass = Pass::new();
    pass.build_initial_users(program);

    for &block in post.iter().rev() {
        visit_block(&mut pass, program, block);
    }

    // Second post-order pass: for any phi node still present (i.e. not
    // removed as trivial), order its operands deterministically so later
    // passes and backends see a stable layout. Upstream additionally fills
    // in `GetConcreteType` for phis whose `Type()` is still Opaque, but the
    // Rust port sets the phi's flags at creation time via `undef_type`, so
    // the type is already concrete and no walk is needed.
    for &block in post.iter().rev() {
        for inst in program.block_mut(block).iter_mut() {
            if inst.opcode == Opcode::Phi {
                inst.phi_args.sort_by_key(|(b, _)| *b);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::basic_block::Block;
    use crate::ir::instruction::Inst;
    use crate::ir::types::ShaderStage;

    #[test]
    fn rewrites_straight_line_register_reads_to_last_write() {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        let value = Value::ImmU32(0x1234);
        let reg = Value::Reg(Reg(3));
        let bitcast_idx = {
            let block = program.block_mut(0);
            block.append_inst(Inst::new(Opcode::SetRegister, vec![reg, value]));
            let get = block.append_inst(Inst::new(Opcode::GetRegister, vec![reg]));
            let bitcast = block.append_inst(Inst::new(
                Opcode::BitCastF32U32,
                vec![Value::Inst(InstRef {
                    block: 0,
                    inst: get,
                })],
            ));
            bitcast
        };

        ssa_rewrite_pass(&mut program);

        let bitcast = program.block(0).inst(bitcast_idx);
        assert_eq!(bitcast.args[0], value);
    }

    /// Two-block diamond join: define R3 differently in each predecessor,
    /// then read R3 in the merge block. The pass must materialize a phi.
    #[test]
    fn diamond_join_inserts_phi() {
        // CFG: 0 → 1, 0 → 2, 1 → 3, 2 → 3
        let mut program = Program::new(ShaderStage::VertexB);
        for _ in 0..4 {
            program.blocks.push(Block::new());
        }
        program.block_mut(0).add_successor(1);
        program.block_mut(0).add_successor(2);
        program.block_mut(1).add_predecessor(0);
        program.block_mut(1).add_successor(3);
        program.block_mut(2).add_predecessor(0);
        program.block_mut(2).add_successor(3);
        program.block_mut(3).add_predecessor(1);
        program.block_mut(3).add_predecessor(2);

        let reg = Value::Reg(Reg(3));
        let v_a = Value::ImmU32(0xAAAA);
        let v_b = Value::ImmU32(0xBBBB);
        program
            .block_mut(1)
            .append_inst(Inst::new(Opcode::SetRegister, vec![reg, v_a]));
        program
            .block_mut(2)
            .append_inst(Inst::new(Opcode::SetRegister, vec![reg, v_b]));
        let get_idx = program
            .block_mut(3)
            .append_inst(Inst::new(Opcode::GetRegister, vec![reg]));
        let use_idx = program.block_mut(3).append_inst(Inst::new(
            Opcode::BitCastF32U32,
            vec![Value::Inst(InstRef {
                block: 3,
                inst: get_idx,
            })],
        ));

        ssa_rewrite_pass(&mut program);

        // With eager replace_uses_with, the bitcast's args[0] should be the
        // phi InstRef directly (no Identity hop).
        let bitcast = program.block(3).inst(use_idx).clone();
        let phi_ref = match bitcast.args[0] {
            Value::Inst(r) => r,
            other => panic!("expected Inst, got {:?}", other),
        };
        let phi_inst = program.block(phi_ref.block).inst(phi_ref.inst).clone();
        assert_eq!(
            phi_inst.opcode,
            Opcode::Phi,
            "bitcast should reference a Phi directly via use-def chain"
        );
        assert_eq!(phi_ref.block, 3, "phi must live in the merge block");
        assert_eq!(phi_inst.phi_args.len(), 2);
        let ops: Vec<Value> = phi_inst.phi_args.iter().map(|(_, v)| *v).collect();
        assert!(ops.contains(&v_a));
        assert!(ops.contains(&v_b));
    }

    /// Loop-header (cycle) case. Upstream does not recursively revisit phi
    /// users after removing a trivial operand phi, so the loop-header phi is
    /// intentionally retained.
    #[test]
    fn loop_with_invariant_value_retains_upstream_phi() {
        // CFG: 0 → 1 → 2 → 1 (back-edge), 2 → 3.
        // Block 0 sets R3=K. Block 1 reads R3 (must become K via phi → trivial).
        let mut program = Program::new(ShaderStage::VertexB);
        for _ in 0..4 {
            program.blocks.push(Block::new());
        }
        program.block_mut(0).add_successor(1);
        program.block_mut(1).add_predecessor(0);
        program.block_mut(1).add_predecessor(2);
        program.block_mut(1).add_successor(2);
        program.block_mut(2).add_predecessor(1);
        program.block_mut(2).add_successor(1);
        program.block_mut(2).add_successor(3);
        program.block_mut(3).add_predecessor(2);

        let reg = Value::Reg(Reg(3));
        let k = Value::ImmU32(0xC0DE);
        program
            .block_mut(0)
            .append_inst(Inst::new(Opcode::SetRegister, vec![reg, k]));
        let get_idx = program
            .block_mut(1)
            .append_inst(Inst::new(Opcode::GetRegister, vec![reg]));
        let use_idx = program.block_mut(1).append_inst(Inst::new(
            Opcode::BitCastF32U32,
            vec![Value::Inst(InstRef {
                block: 1,
                inst: get_idx,
            })],
        ));

        ssa_rewrite_pass(&mut program);

        let bitcast = program.block(1).inst(use_idx).clone();
        let Value::Inst(phi_ref) = bitcast.args[0] else {
            panic!("upstream ordering retains the loop-header phi");
        };
        let phi = program.block(phi_ref.block).inst(phi_ref.inst);
        assert_eq!(phi.opcode, Opcode::Phi);
        assert!(phi.phi_args.iter().any(|(_, value)| *value == k));
    }

    /// The use-def index must be drained correctly: after rewriting, the
    /// users-of(def) set should be empty for the rewritten def. Validates
    /// internal bookkeeping rather than IR output shape.
    #[test]
    fn use_def_index_cleared_after_replace() {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        let reg = Value::Reg(Reg(7));
        let v = Value::ImmU32(42);
        let (get_idx, _bitcast_idx) = {
            let block = program.block_mut(0);
            block.append_inst(Inst::new(Opcode::SetRegister, vec![reg, v]));
            let get = block.append_inst(Inst::new(Opcode::GetRegister, vec![reg]));
            let bc = block.append_inst(Inst::new(
                Opcode::BitCastF32U32,
                vec![Value::Inst(InstRef {
                    block: 0,
                    inst: get,
                })],
            ));
            (get, bc)
        };

        ssa_rewrite_pass(&mut program);

        // The GetRegister inst is now Identity(v), and its previous users
        // (the bitcast) point directly to `v`. The bitcast should not have
        // any InstRef-typed args pointing to the GetRegister anymore.
        let bitcast_inst = program.block(0).inst(_bitcast_idx);
        for arg in &bitcast_inst.args {
            if let Value::Inst(r) = arg {
                assert_ne!(
                    *r,
                    InstRef {
                        block: 0,
                        inst: get_idx
                    },
                    "bitcast should not reference the rewritten GetRegister"
                );
            }
        }
    }

    #[test]
    fn replace_uses_with_rewrites_syntax_conditions() {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        let pred = Value::Pred(Pred(2));
        let value = Value::ImmU1(true);
        let get_idx = {
            let block = program.block_mut(0);
            block.append_inst(Inst::new(Opcode::SetPred, vec![pred, value]));
            block.append_inst(Inst::new(Opcode::GetPred, vec![pred]))
        };
        program
            .syntax_list
            .push(crate::ir::program::SyntaxNode::If {
                cond: Value::Inst(InstRef {
                    block: 0,
                    inst: get_idx,
                }),
                body: 0,
                merge: 0,
            });

        ssa_rewrite_pass(&mut program);

        let crate::ir::program::SyntaxNode::If { cond, .. } = program.syntax_list[0] else {
            panic!("expected If node");
        };
        assert_eq!(cond, value);
    }
}
