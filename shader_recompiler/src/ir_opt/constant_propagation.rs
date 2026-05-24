// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Constant propagation pass — fold compile-time constant expressions.
//!
//! Matches zuyu's `constant_propagation_pass.cpp`.
//!
//! For each instruction, if all arguments are immediates, evaluate the operation
//! and replace the instruction with the result immediate. Also handles special
//! cases like `GetRegister(RZ) -> 0` and `GetPred(PT) -> true`.

use crate::ir::instruction::Inst;
use crate::ir::opcodes::Opcode;
use crate::ir::program::Program;
use crate::ir::value::{InstRef, Value};

/// Run constant propagation on all instructions.
pub fn constant_propagation_pass(program: &mut Program) {
    for block in &mut program.blocks {
        for inst in block.iter_mut() {
            propagate(inst);
        }
    }
    fold_instruction_references(program);
}

fn propagate(inst: &mut Inst) {
    match inst.opcode {
        // ── Special register/predicate folding ────────────────────────
        Opcode::GetRegister => {
            if let Some(Value::Reg(r)) = inst.args.first() {
                if r.is_zero() {
                    // RZ always reads as 0
                    inst.opcode = Opcode::Identity;
                    inst.args = vec![Value::ImmU32(0)];
                }
            }
        }
        Opcode::GetPred => {
            if let Some(Value::Pred(p)) = inst.args.first() {
                if p.is_true() {
                    // PT always reads as true
                    inst.opcode = Opcode::Identity;
                    inst.args = vec![Value::ImmU1(true)];
                }
            }
        }

        // ── Integer arithmetic folding ────────────────────────────────
        Opcode::IAdd32 => {
            if let (Some(&Value::ImmU32(a)), Some(&Value::ImmU32(b))) =
                (inst.args.get(0), inst.args.get(1))
            {
                inst.opcode = Opcode::Identity;
                inst.args = vec![Value::ImmU32(a.wrapping_add(b))];
            }
        }
        Opcode::ISub32 => {
            if let (Some(&Value::ImmU32(a)), Some(&Value::ImmU32(b))) =
                (inst.args.get(0), inst.args.get(1))
            {
                inst.opcode = Opcode::Identity;
                inst.args = vec![Value::ImmU32(a.wrapping_sub(b))];
            }
        }
        Opcode::IMul32 => {
            if let (Some(&Value::ImmU32(a)), Some(&Value::ImmU32(b))) =
                (inst.args.get(0), inst.args.get(1))
            {
                inst.opcode = Opcode::Identity;
                inst.args = vec![Value::ImmU32(a.wrapping_mul(b))];
            }
            // x * 0 = 0
            else if matches!(inst.args.get(1), Some(&Value::ImmU32(0))) {
                inst.opcode = Opcode::Identity;
                inst.args = vec![Value::ImmU32(0)];
            }
            // x * 1 = x
            else if matches!(inst.args.get(1), Some(&Value::ImmU32(1))) {
                let a = inst.args[0];
                inst.opcode = Opcode::Identity;
                inst.args = vec![a];
            }
        }
        Opcode::INeg32 => {
            if let Some(&Value::ImmU32(a)) = inst.args.first() {
                inst.opcode = Opcode::Identity;
                inst.args = vec![Value::ImmU32((a as i32).wrapping_neg() as u32)];
            }
        }

        // ── Shift folding ─────────────────────────────────────────────
        Opcode::ShiftLeftLogical32 => {
            if let (Some(&Value::ImmU32(a)), Some(&Value::ImmU32(b))) =
                (inst.args.get(0), inst.args.get(1))
            {
                let shift = b & 0x1F;
                inst.opcode = Opcode::Identity;
                inst.args = vec![Value::ImmU32(a.wrapping_shl(shift))];
            }
        }
        Opcode::ShiftRightLogical32 => {
            if let (Some(&Value::ImmU32(a)), Some(&Value::ImmU32(b))) =
                (inst.args.get(0), inst.args.get(1))
            {
                let shift = b & 0x1F;
                inst.opcode = Opcode::Identity;
                inst.args = vec![Value::ImmU32(a.wrapping_shr(shift))];
            }
        }
        Opcode::ShiftRightArithmetic32 => {
            if let (Some(&Value::ImmU32(a)), Some(&Value::ImmU32(b))) =
                (inst.args.get(0), inst.args.get(1))
            {
                let shift = b & 0x1F;
                inst.opcode = Opcode::Identity;
                inst.args = vec![Value::ImmU32((a as i32).wrapping_shr(shift) as u32)];
            }
        }

        // ── Bitwise folding ──────────────────────────────────────────
        Opcode::BitwiseAnd32 => {
            if let (Some(&Value::ImmU32(a)), Some(&Value::ImmU32(b))) =
                (inst.args.get(0), inst.args.get(1))
            {
                inst.opcode = Opcode::Identity;
                inst.args = vec![Value::ImmU32(a & b)];
            }
        }
        Opcode::BitwiseOr32 => {
            if let (Some(&Value::ImmU32(a)), Some(&Value::ImmU32(b))) =
                (inst.args.get(0), inst.args.get(1))
            {
                inst.opcode = Opcode::Identity;
                inst.args = vec![Value::ImmU32(a | b)];
            }
        }
        Opcode::BitwiseXor32 => {
            if let (Some(&Value::ImmU32(a)), Some(&Value::ImmU32(b))) =
                (inst.args.get(0), inst.args.get(1))
            {
                inst.opcode = Opcode::Identity;
                inst.args = vec![Value::ImmU32(a ^ b)];
            }
        }
        Opcode::BitwiseNot32 => {
            if let Some(&Value::ImmU32(a)) = inst.args.first() {
                inst.opcode = Opcode::Identity;
                inst.args = vec![Value::ImmU32(!a)];
            }
        }

        // ── FP32 arithmetic folding ──────────────────────────────────
        Opcode::FPAdd32 => {
            if let (Some(&Value::ImmF32(a)), Some(&Value::ImmF32(b))) =
                (inst.args.get(0), inst.args.get(1))
            {
                inst.opcode = Opcode::Identity;
                inst.args = vec![Value::ImmF32(a + b)];
            }
        }
        Opcode::FPMul32 => {
            if let (Some(&Value::ImmF32(a)), Some(&Value::ImmF32(b))) =
                (inst.args.get(0), inst.args.get(1))
            {
                inst.opcode = Opcode::Identity;
                inst.args = vec![Value::ImmF32(a * b)];
            }
            // x * 1.0 = x
            else if matches!(inst.args.get(1), Some(&Value::ImmF32(v)) if v == 1.0) {
                let a = inst.args[0];
                inst.opcode = Opcode::Identity;
                inst.args = vec![a];
            }
            // x * 0.0 = 0.0 (ignoring NaN)
            else if matches!(inst.args.get(1), Some(&Value::ImmF32(v)) if v == 0.0) {
                inst.opcode = Opcode::Identity;
                inst.args = vec![Value::ImmF32(0.0)];
            }
        }
        Opcode::FPNeg32 => {
            if let Some(&Value::ImmF32(a)) = inst.args.first() {
                inst.opcode = Opcode::Identity;
                inst.args = vec![Value::ImmF32(-a)];
            }
        }
        Opcode::FPAbs32 => {
            if let Some(&Value::ImmF32(a)) = inst.args.first() {
                inst.opcode = Opcode::Identity;
                inst.args = vec![Value::ImmF32(a.abs())];
            }
        }

        // ── Bitcast folding ──────────────────────────────────────────
        Opcode::BitCastU32F32 => {
            if let Some(&Value::ImmF32(a)) = inst.args.first() {
                inst.opcode = Opcode::Identity;
                inst.args = vec![Value::ImmU32(a.to_bits())];
            }
        }
        Opcode::BitCastF32U32 => {
            if let Some(&Value::ImmU32(a)) = inst.args.first() {
                inst.opcode = Opcode::Identity;
                inst.args = vec![Value::ImmF32(f32::from_bits(a))];
            }
        }

        // ── Conversion folding ───────────────────────────────────────
        Opcode::ConvertF32U32 => {
            if let Some(&Value::ImmU32(a)) = inst.args.first() {
                inst.opcode = Opcode::Identity;
                inst.args = vec![Value::ImmF32(a as f32)];
            }
        }
        Opcode::ConvertF32S32 => {
            if let Some(&Value::ImmU32(a)) = inst.args.first() {
                inst.opcode = Opcode::Identity;
                inst.args = vec![Value::ImmF32(a as i32 as f32)];
            }
        }
        Opcode::ConvertU32F32 => {
            if let Some(&Value::ImmF32(a)) = inst.args.first() {
                inst.opcode = Opcode::Identity;
                inst.args = vec![Value::ImmU32(a as u32)];
            }
        }
        Opcode::ConvertS32F32 => {
            if let Some(&Value::ImmF32(a)) = inst.args.first() {
                inst.opcode = Opcode::Identity;
                inst.args = vec![Value::ImmU32(a as i32 as u32)];
            }
        }

        // ── Logic folding ────────────────────────────────────────────
        Opcode::LogicalAnd => {
            if let (Some(&Value::ImmU1(a)), Some(&Value::ImmU1(b))) =
                (inst.args.get(0), inst.args.get(1))
            {
                inst.opcode = Opcode::Identity;
                inst.args = vec![Value::ImmU1(a && b)];
            }
        }
        Opcode::LogicalOr => {
            if let (Some(&Value::ImmU1(a)), Some(&Value::ImmU1(b))) =
                (inst.args.get(0), inst.args.get(1))
            {
                inst.opcode = Opcode::Identity;
                inst.args = vec![Value::ImmU1(a || b)];
            }
        }
        Opcode::LogicalNot => {
            if let Some(&Value::ImmU1(a)) = inst.args.first() {
                inst.opcode = Opcode::Identity;
                inst.args = vec![Value::ImmU1(!a)];
            }
        }

        // ── Select folding ───────────────────────────────────────────
        Opcode::SelectU32 | Opcode::SelectF32 | Opcode::SelectU1 => {
            if let Some(&Value::ImmU1(cond)) = inst.args.first() {
                let result = if cond { inst.args[1] } else { inst.args[2] };
                inst.opcode = Opcode::Identity;
                inst.args = vec![result];
            }
        }

        // ── Integer comparison folding ───────────────────────────────
        Opcode::IEqual => {
            if let (Some(&Value::ImmU32(a)), Some(&Value::ImmU32(b))) =
                (inst.args.get(0), inst.args.get(1))
            {
                inst.opcode = Opcode::Identity;
                inst.args = vec![Value::ImmU1(a == b)];
            }
        }
        Opcode::INotEqual => {
            if let (Some(&Value::ImmU32(a)), Some(&Value::ImmU32(b))) =
                (inst.args.get(0), inst.args.get(1))
            {
                inst.opcode = Opcode::Identity;
                inst.args = vec![Value::ImmU1(a != b)];
            }
        }
        Opcode::SLessThan => {
            if let (Some(&Value::ImmU32(a)), Some(&Value::ImmU32(b))) =
                (inst.args.get(0), inst.args.get(1))
            {
                inst.opcode = Opcode::Identity;
                inst.args = vec![Value::ImmU1((a as i32) < (b as i32))];
            }
        }
        Opcode::ULessThan => {
            if let (Some(&Value::ImmU32(a)), Some(&Value::ImmU32(b))) =
                (inst.args.get(0), inst.args.get(1))
            {
                inst.opcode = Opcode::Identity;
                inst.args = vec![Value::ImmU1(a < b)];
            }
        }

        _ => {
            // No folding for this opcode
        }
    }
}

fn fold_instruction_references(program: &mut Program) {
    let refs: Vec<InstRef> = program
        .blocks
        .iter()
        .enumerate()
        .flat_map(|(block_idx, block)| {
            block.indexed_iter().map(move |(inst_idx, _)| InstRef {
                block: block_idx as u32,
                inst: inst_idx,
            })
        })
        .collect();

    for inst_ref in refs {
        let opcode = program.block(inst_ref.block).inst(inst_ref.inst).opcode;
        match opcode {
            Opcode::BitCastU32F32 => fold_bitcast(program, inst_ref, Opcode::BitCastF32U32),
            Opcode::BitCastF32U32 => fold_bitcast(program, inst_ref, Opcode::BitCastU32F32),
            Opcode::FPMul32 => fold_fp_mul_interpolation(program, inst_ref),
            _ => {}
        }
    }
}

fn inst_recursive(value: Value, program: &Program) -> Option<InstRef> {
    let mut current = match value {
        Value::Inst(inst_ref) => inst_ref,
        _ => return None,
    };
    loop {
        let inst = program.block(current.block).inst(current.inst);
        if inst.opcode == Opcode::Identity {
            let Some(Value::Inst(next)) = inst.args.first().copied() else {
                return Some(current);
            };
            current = next;
            continue;
        }
        return Some(current);
    }
}

fn resolve_value(mut value: Value, program: &Program) -> Value {
    while let Value::Inst(inst_ref) = value {
        let inst = program.block(inst_ref.block).inst(inst_ref.inst);
        if inst.opcode != Opcode::Identity || inst.args.is_empty() {
            return value;
        }
        value = inst.args[0];
    }
    value
}

fn replace_with_identity(program: &mut Program, inst_ref: InstRef, value: Value) {
    let inst = program.block_mut(inst_ref.block).inst_mut(inst_ref.inst);
    inst.opcode = Opcode::Identity;
    inst.args = vec![value];
}

fn fold_bitcast(program: &mut Program, inst_ref: InstRef, reverse: Opcode) {
    let arg = match program.block(inst_ref.block).inst(inst_ref.inst).args.first() {
        Some(value) => *value,
        None => return,
    };
    let Some(arg_ref) = inst_recursive(arg, program) else {
        return;
    };
    let arg_inst = program.block(arg_ref.block).inst(arg_ref.inst);
    if arg_inst.opcode == reverse {
        let Some(value) = arg_inst.args.first().copied() else {
            return;
        };
        replace_with_identity(program, inst_ref, value);
    }
}

fn fold_fp_mul_interpolation(program: &mut Program, inst_ref: InstRef) {
    let inst = program.block(inst_ref.block).inst(inst_ref.inst).clone();
    let (Some(lhs_value), Some(rhs_value)) = (inst.args.first().copied(), inst.args.get(1).copied())
    else {
        return;
    };
    if lhs_value.is_immediate() || rhs_value.is_immediate() {
        return;
    }
    let (Some(lhs_ref), Some(rhs_ref)) = (
        inst_recursive(lhs_value, program),
        inst_recursive(rhs_value, program),
    ) else {
        return;
    };
    let lhs_op = program.block(lhs_ref.block).inst(lhs_ref.inst);
    let rhs_op = program.block(rhs_ref.block).inst(rhs_ref.inst);
    if lhs_op.opcode != Opcode::FPMul32 || rhs_op.opcode != Opcode::FPRecip32 {
        return;
    }
    let Some(recip_source) = rhs_op.args.first().copied() else {
        return;
    };
    let Some(lhs_mul_source) = lhs_op.args.get(1).copied() else {
        return;
    };
    let lhs_mul_source = resolve_value(lhs_mul_source, program);
    if recip_source.is_immediate() || lhs_mul_source.is_immediate() {
        return;
    }
    let (Some(attr_a_ref), Some(attr_b_ref)) = (
        inst_recursive(recip_source, program),
        inst_recursive(lhs_mul_source, program),
    ) else {
        return;
    };
    let attr_a = program.block(attr_a_ref.block).inst(attr_a_ref.inst);
    let attr_b = program.block(attr_b_ref.block).inst(attr_b_ref.inst);
    if attr_a.opcode != Opcode::GetAttribute || attr_b.opcode != Opcode::GetAttribute {
        return;
    }
    if attr_a.args.first() != attr_b.args.first() {
        return;
    }
    let Some(replacement) = lhs_op.args.first().copied() else {
        return;
    };
    replace_with_identity(program, inst_ref, replacement);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::basic_block::Block;
    use crate::ir::value::Attribute;
    use crate::ir::types::ShaderStage;

    fn inst(block: u32, inst: u32) -> Value {
        Value::Inst(InstRef { block, inst })
    }

    #[test]
    fn bitcast_inverse_fold_matches_upstream() {
        let mut program = Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());
        let block = program.block_mut(0);
        block.append_inst(Inst::new(Opcode::GetAttribute, vec![
            Value::Attribute(Attribute::generic(1, 0)),
            Value::ImmU32(0),
        ]));
        block.append_inst(Inst::new(Opcode::BitCastU32F32, vec![inst(0, 0)]));
        block.append_inst(Inst::new(Opcode::BitCastF32U32, vec![inst(0, 1)]));

        constant_propagation_pass(&mut program);

        let folded = program.block(0).inst(2);
        assert_eq!(folded.opcode, Opcode::Identity);
        assert_eq!(folded.args, vec![inst(0, 0)]);
    }

    #[test]
    fn interpolation_correction_fold_matches_upstream() {
        let mut program = Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());
        let block = program.block_mut(0);
        block.append_inst(Inst::new(Opcode::GetAttribute, vec![
            Value::Attribute(Attribute::generic(2, 0)),
            Value::ImmU32(0),
        ]));
        block.append_inst(Inst::new(Opcode::GetAttribute, vec![
            Value::Attribute(Attribute::POSITION_W),
            Value::ImmU32(0),
        ]));
        block.append_inst(Inst::new(Opcode::FPMul32, vec![inst(0, 0), inst(0, 1)]));
        block.append_inst(Inst::new(Opcode::FPRecip32, vec![inst(0, 1)]));
        block.append_inst(Inst::new(Opcode::FPMul32, vec![inst(0, 2), inst(0, 3)]));

        constant_propagation_pass(&mut program);

        let folded = program.block(0).inst(4);
        assert_eq!(folded.opcode, Opcode::Identity);
        assert_eq!(folded.args, vec![inst(0, 0)]);
    }
}
