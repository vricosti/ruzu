// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Constant propagation pass — fold compile-time constant expressions.
//!
//! Matches zuyu's `constant_propagation_pass.cpp`.
//!
//! For each instruction, if all arguments are immediates, evaluate the operation
//! and replace the instruction with the result immediate. Also handles special
//! cases like `GetRegister(RZ) -> 0` and `GetPred(PT) -> true`.

use crate::environment::Environment;
use crate::ir::instruction::Inst;
use crate::ir::opcodes::Opcode;
use crate::ir::program::{Program, SyntaxNode};
use crate::ir::value::{Attribute, InstRef, Value};
use crate::shader_info::ReplaceConstant;

/// Run constant propagation on all instructions.
pub fn constant_propagation_pass(program: &mut Program) {
    let block_order = if program.post_order_blocks.is_empty() {
        (0..program.blocks.len() as u32).collect::<Vec<_>>()
    } else {
        program
            .post_order_blocks
            .iter()
            .rev()
            .copied()
            .collect::<Vec<_>>()
    };
    for block_index in block_order {
        let instruction_indices = program
            .block(block_index)
            .indexed_iter()
            .map(|(index, _)| index)
            .collect::<Vec<_>>();
        for inst_index in instruction_indices {
            let (resolved_args, resolved_phi_args) = {
                let inst = program.block(block_index).inst(inst_index);
                (
                    inst.args
                        .iter()
                        .map(|value| resolve_value(*value, program))
                        .collect::<Vec<_>>(),
                    inst.phi_args
                        .iter()
                        .map(|(block, value)| (*block, resolve_value(*value, program)))
                        .collect::<Vec<_>>(),
                )
            };
            {
                let inst = program.block_mut(block_index).inst_mut(inst_index);
                inst.args = resolved_args;
                inst.phi_args = resolved_phi_args;
                propagate(inst);
            }
        }
    }
    fold_instruction_references(program);
}

/// Run constant propagation with the shader environment available.
///
/// Upstream `ConstantPropagationPass(Environment&, IR::Program&)` uses the
/// environment both for HLE constant replacements and for folding driver
/// constant buffer 1 on proprietary-driver shaders. The environment-less
/// entry point remains for legacy callers that do not own an `Environment`.
pub fn constant_propagation_pass_with_env(env: &mut dyn Environment, program: &mut Program) {
    fold_environment_constant_buffers(env, program);
    constant_propagation_pass(program);
}

fn fold_environment_constant_buffers(env: &mut dyn Environment, program: &mut Program) {
    let has_hle_macro_state = env.has_hle_macro_state();
    let is_proprietary_driver = env.is_proprietary_driver();
    let mut replacements = Vec::new();

    for (block_index, block) in program.blocks.iter_mut().enumerate() {
        for (inst_index, inst) in block.indexed_iter_mut() {
            if !matches!(inst.opcode, Opcode::GetCbufU32 | Opcode::GetCbufF32) {
                continue;
            }
            if std::env::var_os("RUZU_TRACE_SHADER_WORDS").is_some()
                && env.start_address() == 0x231630
            {
                eprintln!(
                    "[CONST_CBUF_BEFORE] block={} inst={} opcode={:?} args={:?}",
                    block_index, inst_index, inst.opcode, inst.args
                );
            }
            let (Some(&Value::ImmU32(bank)), Some(&Value::ImmU32(offset))) =
                (inst.args.first(), inst.args.get(1))
            else {
                continue;
            };

            if has_hle_macro_state {
                if let Some(replacement) = env.get_replace_const_buffer(bank, offset) {
                    let attribute = match replacement {
                        ReplaceConstant::BaseInstance => Attribute::BASE_INSTANCE,
                        ReplaceConstant::BaseVertex => Attribute::BASE_VERTEX,
                        ReplaceConstant::DrawID => Attribute::DRAW_ID,
                    };
                    inst.opcode = if inst.opcode == Opcode::GetCbufU32 {
                        Opcode::GetAttributeU32
                    } else {
                        Opcode::GetAttribute
                    };
                    inst.args = vec![Value::Attribute(attribute), Value::ImmU32(0)];
                    continue;
                }
            }

            if is_proprietary_driver && bank == 1 {
                let is_float = inst.opcode == Opcode::GetCbufF32;
                let value = env.read_cbuf_value(bank, offset);
                let replacement = if is_float {
                    Value::ImmF32(f32::from_bits(value))
                } else {
                    Value::ImmU32(value)
                };
                inst.opcode = Opcode::Identity;
                inst.args = vec![replacement];
                replacements.push((
                    InstRef {
                        block: block_index as u32,
                        inst: inst_index,
                    },
                    replacement,
                ));
            }
        }
    }

    for (old, replacement) in replacements {
        replace_uses_with(program, old, replacement);
    }
}

fn replace_uses_with(program: &mut Program, old: InstRef, replacement: Value) {
    let old_value = Value::Inst(old);
    for block in &mut program.blocks {
        for inst in block.iter_mut() {
            for arg in &mut inst.args {
                if *arg == old_value {
                    *arg = replacement;
                }
            }
            for (_, value) in &mut inst.phi_args {
                if *value == old_value {
                    *value = replacement;
                }
            }
        }
    }
    for node in &mut program.syntax_list {
        match node {
            SyntaxNode::If { cond, .. }
            | SyntaxNode::Repeat { cond, .. }
            | SyntaxNode::Break { cond, .. } => {
                if *cond == old_value {
                    *cond = replacement;
                }
            }
            _ => {}
        }
    }
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
            } else if matches!(inst.args.first(), Some(Value::ImmU32(0))) {
                let rhs = inst.args[1];
                inst.opcode = Opcode::Identity;
                inst.args = vec![rhs];
            } else if matches!(inst.args.get(1), Some(Value::ImmU32(0))) {
                let lhs = inst.args[0];
                inst.opcode = Opcode::Identity;
                inst.args = vec![lhs];
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
        Opcode::BitFieldUExtract => {
            if let (
                Some(&Value::ImmU32(base)),
                Some(&Value::ImmU32(shift)),
                Some(&Value::ImmU32(count)),
            ) = (inst.args.first(), inst.args.get(1), inst.args.get(2))
            {
                assert!(
                    (shift as usize).saturating_add(count as usize) <= 32,
                    "undefined BitFieldUExtract({base}, {shift}, {count})"
                );
                let mask = if count == 32 {
                    u32::MAX
                } else {
                    (1u32 << count) - 1
                };
                inst.opcode = Opcode::Identity;
                inst.args = vec![Value::ImmU32((base >> shift) & mask)];
            }
        }
        Opcode::BitFieldSExtract => {
            if let (
                Some(&Value::ImmU32(base)),
                Some(&Value::ImmU32(shift)),
                Some(&Value::ImmU32(count)),
            ) = (inst.args.first(), inst.args.get(1), inst.args.get(2))
            {
                let back_shift = (shift as usize).saturating_add(count as usize);
                let left_shift = 32usize.saturating_sub(back_shift);
                let right_shift = 32usize.saturating_sub(count as usize);
                assert!(
                    back_shift <= 32 && left_shift < 32 && right_shift < 32,
                    "undefined BitFieldSExtract({base}, {shift}, {count})"
                );
                let value = ((base as i32) << left_shift) >> right_shift;
                inst.opcode = Opcode::Identity;
                inst.args = vec![Value::ImmU32(value as u32)];
            }
        }
        Opcode::BitFieldInsert => {
            if let (
                Some(&Value::ImmU32(base)),
                Some(&Value::ImmU32(insert)),
                Some(&Value::ImmU32(offset)),
                Some(&Value::ImmU32(bits)),
            ) = (
                inst.args.first(),
                inst.args.get(1),
                inst.args.get(2),
                inst.args.get(3),
            ) {
                assert!(
                    bits < 32 && offset < 32,
                    "undefined BitFieldInsert({base}, {insert}, {offset}, {bits})"
                );
                let field_mask = !(!0u32 << bits) << offset;
                inst.opcode = Opcode::Identity;
                inst.args = vec![Value::ImmU32(
                    (base & !field_mask) | (insert.wrapping_shl(offset) & field_mask),
                )];
            }
        }

        // ── FP32 arithmetic folding ──────────────────────────────────
        Opcode::FPAdd32 => {
            if let (Some(&Value::ImmF32(a)), Some(&Value::ImmF32(b))) =
                (inst.args.get(0), inst.args.get(1))
            {
                inst.opcode = Opcode::Identity;
                inst.args = vec![Value::ImmF32(a + b)];
            } else if matches!(inst.args.first(), Some(&Value::ImmF32(value)) if value.abs() == 0.0)
            {
                let rhs = inst.args[1];
                inst.opcode = Opcode::Identity;
                inst.args = vec![rhs];
            } else if matches!(inst.args.get(1), Some(&Value::ImmF32(value)) if value.abs() == 0.0)
            {
                let lhs = inst.args[0];
                inst.opcode = Opcode::Identity;
                inst.args = vec![lhs];
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
            } else if let Some(&Value::ImmU1(value)) = inst.args.first() {
                let rhs = inst.args[1];
                inst.opcode = Opcode::Identity;
                inst.args = vec![if value { rhs } else { Value::ImmU1(false) }];
            } else if let Some(&Value::ImmU1(value)) = inst.args.get(1) {
                let lhs = inst.args[0];
                inst.opcode = Opcode::Identity;
                inst.args = vec![if value { lhs } else { Value::ImmU1(false) }];
            }
        }
        Opcode::LogicalOr => {
            if let (Some(&Value::ImmU1(a)), Some(&Value::ImmU1(b))) =
                (inst.args.get(0), inst.args.get(1))
            {
                inst.opcode = Opcode::Identity;
                inst.args = vec![Value::ImmU1(a || b)];
            } else if let Some(&Value::ImmU1(value)) = inst.args.first() {
                let rhs = inst.args[1];
                inst.opcode = Opcode::Identity;
                inst.args = vec![if value { Value::ImmU1(true) } else { rhs }];
            } else if let Some(&Value::ImmU1(value)) = inst.args.get(1) {
                let lhs = inst.args[0];
                inst.opcode = Opcode::Identity;
                inst.args = vec![if value { Value::ImmU1(true) } else { lhs }];
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
            Opcode::LogicalNot => fold_double_logical_not(program, inst_ref),
            _ => {}
        }
    }
}

fn fold_double_logical_not(program: &mut Program, inst_ref: InstRef) {
    let Some(inner_ref) = inst_recursive(
        program.block(inst_ref.block).inst(inst_ref.inst).args[0],
        program,
    ) else {
        return;
    };
    let inner = program.block(inner_ref.block).inst(inner_ref.inst);
    if inner.opcode == Opcode::LogicalNot {
        replace_with_identity(program, inst_ref, inner.args[0]);
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
    let opcode = program.block(inst_ref.block).inst(inst_ref.inst).opcode;
    let arg = match program
        .block(inst_ref.block)
        .inst(inst_ref.inst)
        .args
        .first()
    {
        Some(value) => *value,
        None => return,
    };
    let Some(arg_ref) = inst_recursive(arg, program) else {
        return;
    };
    let arg_inst = program.block(arg_ref.block).inst(arg_ref.inst);
    let arg_opcode = arg_inst.opcode;
    let arg_args = arg_inst.args.clone();
    if arg_opcode == reverse {
        let Some(value) = arg_args.first().copied() else {
            return;
        };
        replace_with_identity(program, inst_ref, value);
        return;
    }
    if opcode == Opcode::BitCastF32U32 && arg_opcode == Opcode::GetCbufU32 {
        let [binding, offset, ..] = arg_args.as_slice() else {
            return;
        };
        let inst = program.block_mut(inst_ref.block).inst_mut(inst_ref.inst);
        inst.opcode = Opcode::GetCbufF32;
        inst.args = vec![*binding, *offset];
    }
}

fn fold_fp_mul_interpolation(program: &mut Program, inst_ref: InstRef) {
    let inst = program.block(inst_ref.block).inst(inst_ref.inst).clone();
    let (Some(lhs_value), Some(rhs_value)) =
        (inst.args.first().copied(), inst.args.get(1).copied())
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
    use crate::environment::Environment;
    use crate::ir::basic_block::Block;
    use crate::ir::types::ShaderStage;
    use crate::ir::value::Attribute;
    use crate::program_header::ProgramHeader;
    use crate::shader_info::{ReplaceConstant, TexturePixelFormat, TextureType};

    fn inst(block: u32, inst: u32) -> Value {
        Value::Inst(InstRef { block, inst })
    }

    #[test]
    fn bitcast_inverse_fold_matches_upstream() {
        let mut program = Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());
        let block = program.block_mut(0);
        block.append_inst(Inst::new(
            Opcode::GetAttribute,
            vec![Value::Attribute(Attribute::generic(1, 0)), Value::ImmU32(0)],
        ));
        block.append_inst(Inst::new(Opcode::BitCastU32F32, vec![inst(0, 0)]));
        block.append_inst(Inst::new(Opcode::BitCastF32U32, vec![inst(0, 1)]));

        constant_propagation_pass(&mut program);

        let folded = program.block(0).inst(2);
        assert_eq!(folded.opcode, Opcode::Identity);
        assert_eq!(folded.args, vec![inst(0, 0)]);
    }

    #[test]
    fn bitcast_cbuf_u32_becomes_typed_cbuf_f32_like_upstream() {
        let mut program = Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());
        let block = program.block_mut(0);
        block.append_inst(Inst::new(
            Opcode::GetCbufU32,
            vec![Value::ImmU32(3), Value::ImmU32(0x1a0)],
        ));
        block.append_inst(Inst::new(Opcode::BitCastF32U32, vec![inst(0, 0)]));

        constant_propagation_pass(&mut program);

        let folded = program.block(0).inst(1);
        assert_eq!(folded.opcode, Opcode::GetCbufF32);
        assert_eq!(folded.args, vec![Value::ImmU32(3), Value::ImmU32(0x1a0)]);
    }

    #[test]
    fn signed_bitfield_extract_folds_texture_offsets_like_upstream() {
        let mut program = Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());
        let block = program.block_mut(0);
        block.append_inst(Inst::new(
            Opcode::BitFieldSExtract,
            vec![
                Value::ImmU32(0x0000_00f1),
                Value::ImmU32(0),
                Value::ImmU32(4),
            ],
        ));
        block.append_inst(Inst::new(
            Opcode::BitFieldSExtract,
            vec![
                Value::ImmU32(0x0000_00f1),
                Value::ImmU32(4),
                Value::ImmU32(4),
            ],
        ));

        constant_propagation_pass(&mut program);

        assert_eq!(program.block(0).inst(0).args, vec![Value::ImmU32(1)]);
        assert_eq!(program.block(0).inst(1).args, vec![Value::ImmU32(u32::MAX)]);
    }

    #[test]
    fn bitfield_insert_masks_bits_outside_the_inserted_field() {
        let mut program = Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());
        program.block_mut(0).append_inst(Inst::new(
            Opcode::BitFieldInsert,
            vec![
                Value::ImmU32(0x1234_5670),
                Value::ImmU32(0xff),
                Value::ImmU32(0),
                Value::ImmU32(4),
            ],
        ));

        constant_propagation_pass(&mut program);

        let folded = program.block(0).inst(0);
        assert_eq!(folded.opcode, Opcode::Identity);
        assert_eq!(folded.args, vec![Value::ImmU32(0x1234_567f)]);
    }

    #[test]
    fn fp_add_folds_positive_and_negative_zero_like_upstream() {
        let mut program = Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());
        let block = program.block_mut(0);
        block.append_inst(Inst::new(
            Opcode::GetAttribute,
            vec![Value::Attribute(Attribute::generic(1, 0)), Value::ImmU32(0)],
        ));
        block.append_inst(Inst::new(
            Opcode::FPAdd32,
            vec![inst(0, 0), Value::ImmF32(-0.0)],
        ));
        block.append_inst(Inst::new(
            Opcode::FPAdd32,
            vec![Value::ImmF32(0.0), inst(0, 0)],
        ));

        constant_propagation_pass(&mut program);

        for index in [1, 2] {
            let folded = program.block(0).inst(index);
            assert_eq!(folded.opcode, Opcode::Identity);
            assert_eq!(folded.args, vec![inst(0, 0)]);
        }
    }

    #[test]
    fn folded_constant_is_immediately_visible_to_later_instructions() {
        let mut program = Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());
        let block = program.block_mut(0);
        block.append_inst(Inst::new(
            Opcode::GetAttribute,
            vec![Value::Attribute(Attribute::generic(1, 0)), Value::ImmU32(0)],
        ));
        block.append_inst(Inst::new(Opcode::FPNeg32, vec![Value::ImmF32(0.0)]));
        block.append_inst(Inst::new(Opcode::FPAdd32, vec![inst(0, 0), inst(0, 1)]));

        constant_propagation_pass(&mut program);

        let folded = program.block(0).inst(2);
        assert_eq!(folded.opcode, Opcode::Identity);
        assert_eq!(folded.args, vec![inst(0, 0)]);
    }

    #[test]
    fn integer_add_folds_zero_like_upstream() {
        let mut program = Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());
        let block = program.block_mut(0);
        block.append_inst(Inst::new(
            Opcode::GetAttributeU32,
            vec![Value::Attribute(Attribute::generic(1, 0)), Value::ImmU32(0)],
        ));
        block.append_inst(Inst::new(
            Opcode::IAdd32,
            vec![inst(0, 0), Value::ImmU32(0)],
        ));

        constant_propagation_pass(&mut program);

        let folded = program.block(0).inst(1);
        assert_eq!(folded.opcode, Opcode::Identity);
        assert_eq!(folded.args, vec![inst(0, 0)]);
    }

    #[test]
    fn logical_neutral_and_double_not_folds_match_upstream() {
        let mut program = Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());
        let block = program.block_mut(0);
        block.append_inst(Inst::new(Opcode::UndefU1, vec![]));
        block.append_inst(Inst::new(
            Opcode::LogicalAnd,
            vec![inst(0, 0), Value::ImmU1(true)],
        ));
        block.append_inst(Inst::new(
            Opcode::LogicalOr,
            vec![inst(0, 1), Value::ImmU1(false)],
        ));
        block.append_inst(Inst::new(Opcode::LogicalNot, vec![inst(0, 2)]));
        block.append_inst(Inst::new(Opcode::LogicalNot, vec![inst(0, 3)]));

        constant_propagation_pass(&mut program);

        for index in [1, 2, 4] {
            let folded = program.block(0).inst(index);
            assert_eq!(folded.opcode, Opcode::Identity);
            assert_eq!(folded.args, vec![inst(0, 0)]);
        }
    }

    #[test]
    fn interpolation_correction_fold_matches_upstream() {
        let mut program = Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());
        let block = program.block_mut(0);
        block.append_inst(Inst::new(
            Opcode::GetAttribute,
            vec![Value::Attribute(Attribute::generic(2, 0)), Value::ImmU32(0)],
        ));
        block.append_inst(Inst::new(
            Opcode::GetAttribute,
            vec![Value::Attribute(Attribute::POSITION_W), Value::ImmU32(0)],
        ));
        block.append_inst(Inst::new(Opcode::FPMul32, vec![inst(0, 0), inst(0, 1)]));
        block.append_inst(Inst::new(Opcode::FPRecip32, vec![inst(0, 1)]));
        block.append_inst(Inst::new(Opcode::FPMul32, vec![inst(0, 2), inst(0, 3)]));

        constant_propagation_pass(&mut program);

        let folded = program.block(0).inst(4);
        assert_eq!(folded.opcode, Opcode::Identity);
        assert_eq!(folded.args, vec![inst(0, 0)]);
    }

    struct DriverEnvironment {
        sph: ProgramHeader,
    }

    impl Default for DriverEnvironment {
        fn default() -> Self {
            Self {
                sph: ProgramHeader::default(),
            }
        }
    }

    impl Environment for DriverEnvironment {
        fn read_instruction(&mut self, _address: u32) -> u64 {
            0
        }

        fn read_cbuf_value(&mut self, bank: u32, offset: u32) -> u32 {
            assert_eq!(bank, 1);
            assert_eq!(offset, 0x10);
            1.5f32.to_bits()
        }

        fn read_texture_type(&mut self, _raw_handle: u32) -> TextureType {
            TextureType::Color2D
        }

        fn read_texture_pixel_format(&mut self, _raw_handle: u32) -> TexturePixelFormat {
            TexturePixelFormat::A8B8G8R8Unorm
        }

        fn is_texture_pixel_format_integer(&mut self, _raw_handle: u32) -> bool {
            false
        }

        fn read_viewport_transform_state(&mut self) -> u32 {
            0
        }

        fn texture_bound_buffer(&self) -> u32 {
            2
        }

        fn local_memory_size(&self) -> u32 {
            0
        }

        fn shared_memory_size(&self) -> u32 {
            0
        }

        fn workgroup_size(&self) -> [u32; 3] {
            [1, 1, 1]
        }

        fn has_hle_macro_state(&self) -> bool {
            false
        }

        fn get_replace_const_buffer(
            &mut self,
            _bank: u32,
            _offset: u32,
        ) -> Option<ReplaceConstant> {
            None
        }

        fn dump(&mut self, _pipeline_hash: u64, _shader_hash: u64) {}

        fn sph(&self) -> &ProgramHeader {
            &self.sph
        }

        fn gp_passthrough_mask(&self) -> &[u32; 8] {
            static MASK: [u32; 8] = [0; 8];
            &MASK
        }

        fn shader_stage(&self) -> ShaderStage {
            ShaderStage::Fragment
        }

        fn start_address(&self) -> u32 {
            0
        }

        fn is_proprietary_driver(&self) -> bool {
            true
        }
    }

    #[test]
    fn proprietary_driver_cbuf_one_is_folded_from_environment() {
        let mut program = Program::new(ShaderStage::Fragment);
        program.blocks.push(Block::new());
        let block = program.block_mut(0);
        block.append_inst(Inst::new(
            Opcode::GetCbufF32,
            vec![Value::ImmU32(1), Value::ImmU32(0x10)],
        ));
        block.append_inst(Inst::new(
            Opcode::FPMul32,
            vec![inst(0, 0), Value::ImmF32(2.0)],
        ));
        block.append_inst(Inst::new(
            Opcode::GetCbufF32,
            vec![Value::ImmU32(3), Value::ImmU32(0)],
        ));

        constant_propagation_pass_with_env(&mut DriverEnvironment::default(), &mut program);

        assert_eq!(program.block(0).inst(0).opcode, Opcode::Identity);
        assert_eq!(program.block(0).inst(0).args, vec![Value::ImmF32(1.5)]);
        assert_eq!(program.block(0).inst(1).opcode, Opcode::Identity);
        assert_eq!(program.block(0).inst(1).args, vec![Value::ImmF32(3.0)]);
        assert_eq!(program.block(0).inst(2).opcode, Opcode::GetCbufF32);
    }
}
