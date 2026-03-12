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
use crate::ir::value::{Pred, Reg, Value};

/// Run constant propagation on all instructions.
pub fn constant_propagation_pass(program: &mut Program) {
    for block in &mut program.blocks {
        for inst in &mut block.instructions {
            propagate(inst);
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
            // BitCast(BitCast(x)) = x
            else if let Some(&Value::Inst(r)) = inst.args.first() {
                // Would need to look up the referenced instruction — skip for now
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
