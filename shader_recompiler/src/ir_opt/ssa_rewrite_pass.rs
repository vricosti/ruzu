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
//! Eden's `Inst::ReplaceUsesWith` is intentionally constant-time: the stable
//! instruction itself becomes `Identity(replacement)` and its users keep the
//! same instruction pointer. `InstRef` gives Rust the same stable identity, so
//! this pass must not build an eager user index or rewrite the whole IR.

use std::collections::HashMap;

use crate::ir::opcodes::Opcode;
use crate::ir::post_order::post_order;
use crate::ir::program::Program;
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

#[derive(Clone, Copy, PartialEq, Eq)]
enum ReadStatus {
    Start,
    SetValue,
    PreparePhiArgument,
    PushPhiArgument,
}

struct ReadState {
    block: Option<u32>,
    result: Value,
    phi: Option<InstRef>,
    predecessors: Vec<u32>,
    predecessor_index: usize,
    pc: ReadStatus,
}

impl ReadState {
    fn new(block: Option<u32>) -> Self {
        Self {
            block,
            result: Value::Void,
            phi: None,
            predecessors: Vec::new(),
            predecessor_index: 0,
            pc: ReadStatus::Start,
        }
    }
}

struct Pass {
    /// Upstream `DefTable current_def`. Per (variable, block) → defined value.
    current_def: HashMap<(Variable, u32), Value>,
    /// Upstream `incomplete_phis`. Per block → map<variable, phi inst ref>.
    incomplete_phis: HashMap<u32, Vec<(Variable, InstRef)>>,
}

impl Pass {
    fn new() -> Self {
        Self {
            current_def: HashMap::new(),
            incomplete_phis: HashMap::new(),
        }
    }

    /// Upstream `WriteVariable(variable, block, value)`.
    fn write_variable(&mut self, var: Variable, block: u32, value: Value) {
        self.current_def.insert((var, block), value);
    }

    fn def_of(&self, var: Variable, block: u32) -> Option<Value> {
        self.current_def.get(&(var, block)).copied()
    }

    /// Add an operand to a phi node.
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
    }

    /// Upstream `ReadVariable(variable, root_block)`. Performs the lazy
    /// SSA construction read: returns the SSA value for `var` at the start
    /// of `block`, inserting phi nodes as needed.
    fn read_variable(&mut self, program: &mut Program, var: Variable, root_block: u32) -> Value {
        // Eden deliberately uses an explicit vector state machine here. Deep
        // single-predecessor CFGs must not consume the host call stack.
        let mut stack = vec![ReadState::new(None), ReadState::new(Some(root_block))];
        while stack.len() > 1 {
            let state_index = stack.len() - 1;
            let block = stack[state_index]
                .block
                .expect("non-root ReadState must own a block");
            match stack[state_index].pc {
                ReadStatus::Start => {
                    if let Some(def) = self.def_of(var, block) {
                        stack[state_index].result = def;
                        stack[state_index].pc = ReadStatus::SetValue;
                    } else if !program.block(block).is_ssa_sealed {
                        let phi_ref = append_phi(program, block, undef_type(var));
                        self.incomplete_phis
                            .entry(block)
                            .or_default()
                            .push((var, phi_ref));
                        stack[state_index].result = Value::Inst(phi_ref);
                        stack[state_index].pc = ReadStatus::SetValue;
                    } else {
                        let predecessors = program.block(block).imm_predecessors.clone();
                        if predecessors.len() == 1 {
                            stack[state_index].pc = ReadStatus::SetValue;
                            stack.push(ReadState::new(Some(predecessors[0])));
                        } else {
                            let phi_ref = append_phi(program, block, undef_type(var));
                            self.write_variable(var, block, Value::Inst(phi_ref));
                            stack[state_index].phi = Some(phi_ref);
                            stack[state_index].predecessors = predecessors;
                            stack[state_index].pc = ReadStatus::PreparePhiArgument;
                        }
                    }
                }
                ReadStatus::SetValue => {
                    let result = stack[state_index].result;
                    self.write_variable(var, block, result);
                    stack.pop();
                    stack
                        .last_mut()
                        .expect("ReadVariable root state must remain")
                        .result = result;
                }
                ReadStatus::PushPhiArgument => {
                    let phi = stack[state_index]
                        .phi
                        .expect("phi argument state must own a phi");
                    let predecessor =
                        stack[state_index].predecessors[stack[state_index].predecessor_index];
                    let result = stack[state_index].result;
                    self.add_phi_operand(program, phi, predecessor, result);
                    stack[state_index].predecessor_index += 1;
                    stack[state_index].pc = ReadStatus::PreparePhiArgument;
                }
                ReadStatus::PreparePhiArgument => {
                    if stack[state_index].predecessor_index == stack[state_index].predecessors.len()
                    {
                        let phi = stack[state_index]
                            .phi
                            .expect("completed phi state must own a phi");
                        let result = self.try_remove_trivial_phi(program, phi, undef_opcode(var));
                        self.write_variable(var, block, result);
                        stack.pop();
                        stack
                            .last_mut()
                            .expect("ReadVariable root state must remain")
                            .result = result;
                    } else {
                        let predecessor =
                            stack[state_index].predecessors[stack[state_index].predecessor_index];
                        stack[state_index].pc = ReadStatus::PushPhiArgument;
                        stack.push(ReadState::new(Some(predecessor)));
                    }
                }
            }
        }
        stack[0].result
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
    /// non-self value, replace the phi with that value through its stable
    /// identity. If no operand exists (unreachable / start block), synthesize
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
        // Eden temporarily unlinks the phi, finds the first non-phi, then
        // reinserts the same stable instruction immediately before it.
        let reinsert_before = {
            let block = program.block_mut(phi_ref.block);
            block.unlink_inst(phi_ref.inst);
            block
                .indexed_iter()
                .find(|(_, inst)| inst.opcode != Opcode::Phi)
                .map(|(index, _)| index)
        };
        if same.is_void() {
            // Unreachable or start-block phi: synthesize an Undef.
            let undef_idx = match reinsert_before {
                Some(before) => program.block_mut(phi_ref.block).insert_inst_before(
                    before,
                    crate::ir::instruction::Inst::new(undef_op, vec![]),
                ),
                None => program
                    .block_mut(phi_ref.block)
                    .append_new_inst(undef_op, vec![]),
            };
            same = Value::Inst(InstRef {
                block: phi_ref.block,
                inst: undef_idx,
            });
        }
        program
            .block_mut(phi_ref.block)
            .relink_inst_before(phi_ref.inst, reinsert_before);
        program
            .block_mut(phi_ref.block)
            .inst_mut(phi_ref.inst)
            .replace_uses_with(same);

        same
    }
}

/// Upstream `PrependNewInst(block->begin(), IR::Opcode::Phi)`.
fn append_phi(program: &mut Program, block: u32, ty: Type) -> InstRef {
    let first = program
        .block(block)
        .indexed_iter()
        .next()
        .map(|(index, _)| index);
    let inst_idx = match first {
        Some(before) => program.block_mut(block).insert_inst_before(
            before,
            crate::ir::instruction::Inst::new(Opcode::Phi, vec![]),
        ),
        None => program
            .block_mut(block)
            .append_new_inst(Opcode::Phi, vec![]),
    };
    let phi = program.block_mut(block).inst_mut(inst_idx);
    phi.flags = ty as u32;
    InstRef {
        block,
        inst: inst_idx,
    }
}

/// Upstream `Value::Resolve()` — walks stable `Identity` chains.
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
            if !reg.is_zero() {
                let value = pass.read_variable(program, Variable::Reg(reg), block);
                program
                    .block_mut(block)
                    .inst_mut(inst_idx)
                    .replace_uses_with(value);
            }
        }
        Opcode::GetPred => {
            let pred = snapshot.args[0].pred();
            if !pred.is_true() {
                let value = pass.read_variable(program, Variable::Pred(pred), block);
                program
                    .block_mut(block)
                    .inst_mut(inst_idx)
                    .replace_uses_with(value);
            }
        }
        Opcode::GetGotoVariable => {
            let value =
                pass.read_variable(program, Variable::Goto(snapshot.args[0].imm_u32()), block);
            program
                .block_mut(block)
                .inst_mut(inst_idx)
                .replace_uses_with(value);
        }
        Opcode::GetIndirectBranchVariable => {
            let value = pass.read_variable(program, Variable::IndirectBranch, block);
            program
                .block_mut(block)
                .inst_mut(inst_idx)
                .replace_uses_with(value);
        }
        Opcode::GetZFlag => {
            let value = pass.read_variable(program, Variable::ZeroFlag, block);
            program
                .block_mut(block)
                .inst_mut(inst_idx)
                .replace_uses_with(value);
        }
        Opcode::GetSFlag => {
            let value = pass.read_variable(program, Variable::SignFlag, block);
            program
                .block_mut(block)
                .inst_mut(inst_idx)
                .replace_uses_with(value);
        }
        Opcode::GetCFlag => {
            let value = pass.read_variable(program, Variable::CarryFlag, block);
            program
                .block_mut(block)
                .inst_mut(inst_idx)
                .replace_uses_with(value);
        }
        Opcode::GetOFlag => {
            let value = pass.read_variable(program, Variable::OverflowFlag, block);
            program
                .block_mut(block)
                .inst_mut(inst_idx)
                .replace_uses_with(value);
        }
        _ => {}
    }
}

fn visit_block(pass: &mut Pass, program: &mut Program, block: u32) {
    // Snapshot the original instruction identities so that phi nodes inserted
    // by ReadVariable during this loop are not iterated as if they were part of
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

    for &block in post.iter().rev() {
        visit_block(&mut pass, program, block);
    }

    // Second post-order pass: for any phi node still present (i.e. not
    // removed as trivial), order its operands deterministically so later
    // passes and backends see a stable layout. Upstream additionally fills
    // in `GetConcreteType` for phis whose `Type()` is still Opaque, but the
    // Rust port sets the phi's flags at creation time via `undef_type`, so
    // the type is already concrete and no walk is needed.
    let block_orders = program
        .blocks
        .iter()
        .map(|block| block.order)
        .collect::<Vec<_>>();
    for &block in post.iter().rev() {
        for inst in program.block_mut(block).iter_mut() {
            if inst.opcode == Opcode::Phi {
                inst.order_phi_args(&block_orders);
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
    fn phi_operands_follow_upstream_block_order_not_block_index() {
        let mut inst = Inst::phi();
        inst.add_phi_operand(0, Value::ImmU32(10));
        inst.add_phi_operand(1, Value::ImmU32(20));

        inst.order_phi_args(&[5, 2]);

        assert_eq!(
            inst.phi_args,
            vec![(1, Value::ImmU32(20)), (0, Value::ImmU32(10))]
        );
    }

    #[test]
    fn rewrites_straight_line_register_reads_to_last_write() {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        let value = Value::ImmU32(0x1234);
        let reg = Value::Reg(Reg(3));
        let (get_idx, bitcast_idx) = {
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
            (get, bitcast)
        };

        ssa_rewrite_pass(&mut program);

        let bitcast = program.block(0).inst(bitcast_idx);
        assert_eq!(
            bitcast.args[0],
            Value::Inst(InstRef {
                block: 0,
                inst: get_idx,
            })
        );
        let get = program.block(0).inst(get_idx);
        assert_eq!(get.opcode, Opcode::Identity);
        assert_eq!(get.args, vec![value]);
        assert_eq!(resolve_value(bitcast.args[0], &program), value);
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

        // Eden preserves the GetRegister instruction identity and turns it
        // into Identity(phi); the existing bitcast reference is unchanged.
        let bitcast = program.block(3).inst(use_idx).clone();
        assert_eq!(
            bitcast.args[0],
            Value::Inst(InstRef {
                block: 3,
                inst: get_idx,
            })
        );
        let get = program.block(3).inst(get_idx);
        assert_eq!(get.opcode, Opcode::Identity);
        let Value::Inst(phi_ref) = get.args[0] else {
            panic!("GetRegister identity should point at the merge phi");
        };
        let phi_inst = program.block(phi_ref.block).inst(phi_ref.inst).clone();
        assert_eq!(
            phi_inst.opcode,
            Opcode::Phi,
            "GetRegister identity should resolve through the merge phi"
        );
        assert_eq!(phi_ref.block, 3, "phi must live in the merge block");
        assert_eq!(phi_inst.phi_args.len(), 2);
        let ops: Vec<Value> = phi_inst.phi_args.iter().map(|(_, v)| *v).collect();
        assert!(ops.contains(&v_a));
        assert!(ops.contains(&v_b));
        assert_eq!(
            program
                .block(3)
                .indexed_iter()
                .next()
                .map(|(index, _)| index),
            Some(phi_ref.inst),
            "upstream prepends non-trivial phis to the block"
        );
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
        assert_eq!(
            bitcast.args[0],
            Value::Inst(InstRef {
                block: 1,
                inst: get_idx,
            })
        );
        let get = program.block(1).inst(get_idx);
        assert_eq!(get.opcode, Opcode::Identity);
        let Value::Inst(phi_ref) = get.args[0] else {
            panic!("upstream ordering retains the loop-header phi behind the identity");
        };
        let phi = program.block(phi_ref.block).inst(phi_ref.inst);
        assert_eq!(phi.opcode, Opcode::Phi);
        assert!(phi.phi_args.iter().any(|(_, value)| *value == k));
    }

    #[test]
    fn replace_uses_with_preserves_existing_user_references() {
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

        // This is the literal Eden contract: the definition becomes Identity
        // while the user retains the same stable instruction reference.
        let bitcast_inst = program.block(0).inst(_bitcast_idx);
        let get_ref = InstRef {
            block: 0,
            inst: get_idx,
        };
        assert_eq!(bitcast_inst.args, vec![Value::Inst(get_ref)]);
        assert_eq!(program.block(0).inst(get_idx).opcode, Opcode::Identity);
        assert_eq!(program.block(0).inst(get_idx).args, vec![v]);
    }

    #[test]
    fn replace_uses_with_preserves_syntax_condition_reference() {
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
        assert_eq!(
            cond,
            Value::Inst(InstRef {
                block: 0,
                inst: get_idx,
            })
        );
        assert_eq!(program.block(0).inst(get_idx).opcode, Opcode::Identity);
        assert_eq!(program.block(0).inst(get_idx).args, vec![value]);
    }

    #[test]
    fn zero_register_and_true_predicate_are_left_for_constant_propagation() {
        let mut program = Program::new(ShaderStage::VertexB);
        program.blocks.push(Block::new());
        let get_rz = program
            .block_mut(0)
            .append_inst(Inst::new(Opcode::GetRegister, vec![Value::Reg(Reg(255))]));
        let get_pt = program
            .block_mut(0)
            .append_inst(Inst::new(Opcode::GetPred, vec![Value::Pred(Pred(7))]));

        ssa_rewrite_pass(&mut program);

        assert_eq!(program.block(0).inst(get_rz).opcode, Opcode::GetRegister);
        assert_eq!(program.block(0).inst(get_pt).opcode, Opcode::GetPred);
    }

    #[test]
    fn read_variable_uses_explicit_stack_for_deep_predecessor_chain() {
        const BLOCK_COUNT: u32 = 16_384;
        let mut program = Program::new(ShaderStage::VertexB);
        for block in 0..BLOCK_COUNT {
            program.blocks.push(Block::new());
            program.block_mut(block).seal();
            if block != 0 {
                program.block_mut(block).add_predecessor(block - 1);
            }
        }
        let variable = Variable::Reg(Reg(4));
        let expected = Value::ImmU32(0xCAFE_BABE);
        let mut pass = Pass::new();
        pass.write_variable(variable, 0, expected);

        assert_eq!(
            pass.read_variable(&mut program, variable, BLOCK_COUNT - 1),
            expected
        );
    }
}
