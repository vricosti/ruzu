// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V emission for integer arithmetic, bitwise, comparison, logic, and select opcodes.

use rspirv::dr::Operand;
use rspirv::spirv;

use super::spirv_context::EmitContext;
use crate::ir::{self, Opcode};

/// Helper: GLSL.std.450 extension set ID.
fn glsl_ext(_ctx: &EmitContext) -> spirv::Word {
    1
}

// ── Arithmetic ────────────────────────────────────────────────────────

pub fn emit_iadd_32(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let a = ctx.resolve_value(inst.arg(0));
    let b = ctx.resolve_value(inst.arg(1));
    let id = ctx.builder.i_add(ctx.u32_type, None, a, b).unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

pub fn emit_isub_32(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let a = ctx.resolve_value(inst.arg(0));
    let b = ctx.resolve_value(inst.arg(1));
    let id = ctx.builder.i_sub(ctx.u32_type, None, a, b).unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

pub fn emit_imul_32(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let a = ctx.resolve_value(inst.arg(0));
    let b = ctx.resolve_value(inst.arg(1));
    let id = ctx.builder.i_mul(ctx.u32_type, None, a, b).unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

pub fn emit_ineg_32(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let a = ctx.resolve_value(inst.arg(0));
    let id = ctx.builder.s_negate(ctx.i32_type, None, a).unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

pub fn emit_iabs_32(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let a = ctx.resolve_value(inst.arg(0));
    // GLSL.std.450 SAbs = opcode 5
    let id = ctx
        .builder
        .ext_inst(
            ctx.i32_type,
            None,
            glsl_ext(ctx),
            5, // SAbs
            vec![Operand::IdRef(a)],
        )
        .unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

// ── Shifts ────────────────────────────────────────────────────────────

pub fn emit_shl_32(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let a = ctx.resolve_value(inst.arg(0));
    let b = ctx.resolve_value(inst.arg(1));
    let id = ctx
        .builder
        .shift_left_logical(ctx.u32_type, None, a, b)
        .unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

pub fn emit_shr_logical_32(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let a = ctx.resolve_value(inst.arg(0));
    let b = ctx.resolve_value(inst.arg(1));
    let id = ctx
        .builder
        .shift_right_logical(ctx.u32_type, None, a, b)
        .unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

pub fn emit_shr_arith_32(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let a = ctx.resolve_value(inst.arg(0));
    let b = ctx.resolve_value(inst.arg(1));
    let id = ctx
        .builder
        .shift_right_arithmetic(ctx.i32_type, None, a, b)
        .unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

// ── Bitwise ───────────────────────────────────────────────────────────

pub fn emit_bitwise_and_32(
    ctx: &mut EmitContext,
    inst: &ir::Inst,
    block_idx: u32,
    inst_idx: u32,
) {
    let a = ctx.resolve_value(inst.arg(0));
    let b = ctx.resolve_value(inst.arg(1));
    let id = ctx
        .builder
        .bitwise_and(ctx.u32_type, None, a, b)
        .unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

pub fn emit_bitwise_or_32(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let a = ctx.resolve_value(inst.arg(0));
    let b = ctx.resolve_value(inst.arg(1));
    let id = ctx.builder.bitwise_or(ctx.u32_type, None, a, b).unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

pub fn emit_bitwise_xor_32(
    ctx: &mut EmitContext,
    inst: &ir::Inst,
    block_idx: u32,
    inst_idx: u32,
) {
    let a = ctx.resolve_value(inst.arg(0));
    let b = ctx.resolve_value(inst.arg(1));
    let id = ctx
        .builder
        .bitwise_xor(ctx.u32_type, None, a, b)
        .unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

pub fn emit_bitwise_not_32(
    ctx: &mut EmitContext,
    inst: &ir::Inst,
    block_idx: u32,
    inst_idx: u32,
) {
    let a = ctx.resolve_value(inst.arg(0));
    let id = ctx.builder.not(ctx.u32_type, None, a).unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

// ── Bit field ─────────────────────────────────────────────────────────

pub fn emit_bfi(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let base = ctx.resolve_value(inst.arg(0));
    let insert = ctx.resolve_value(inst.arg(1));
    let offset = ctx.resolve_value(inst.arg(2));
    let count = ctx.resolve_value(inst.arg(3));
    let id = ctx
        .builder
        .bit_field_insert(ctx.u32_type, None, base, insert, offset, count)
        .unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

pub fn emit_bfe_s(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let base = ctx.resolve_value(inst.arg(0));
    let offset = ctx.resolve_value(inst.arg(1));
    let count = ctx.resolve_value(inst.arg(2));
    let id = ctx
        .builder
        .bit_field_s_extract(ctx.i32_type, None, base, offset, count)
        .unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

pub fn emit_bfe_u(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let base = ctx.resolve_value(inst.arg(0));
    let offset = ctx.resolve_value(inst.arg(1));
    let count = ctx.resolve_value(inst.arg(2));
    let id = ctx
        .builder
        .bit_field_u_extract(ctx.u32_type, None, base, offset, count)
        .unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

pub fn emit_bit_count_32(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let a = ctx.resolve_value(inst.arg(0));
    let id = ctx.builder.bit_count(ctx.u32_type, None, a).unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

pub fn emit_find_msb_32(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let a = ctx.resolve_value(inst.arg(0));
    let glsl_opcode = match inst.opcode {
        Opcode::FindSMsb32 => 74, // GLSL.std.450 FindSMsb
        Opcode::FindUMsb32 => 75, // GLSL.std.450 FindUMsb
        _ => 75,
    };
    let id = ctx
        .builder
        .ext_inst(
            ctx.i32_type,
            None,
            glsl_ext(ctx),
            glsl_opcode,
            vec![Operand::IdRef(a)],
        )
        .unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

// ── Min / Max ─────────────────────────────────────────────────────────

pub fn emit_min_max_32(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let a = ctx.resolve_value(inst.arg(0));
    let b = ctx.resolve_value(inst.arg(1));
    let glsl_opcode = match inst.opcode {
        Opcode::SMin32 => 39, // GLSL.std.450 SMin
        Opcode::UMin32 => 38, // GLSL.std.450 UMin
        Opcode::SMax32 => 42, // GLSL.std.450 SMax
        Opcode::UMax32 => 41, // GLSL.std.450 UMax
        _ => 41,
    };
    let result_type = match inst.opcode {
        Opcode::SMin32 | Opcode::SMax32 => ctx.i32_type,
        _ => ctx.u32_type,
    };
    let id = ctx
        .builder
        .ext_inst(
            result_type,
            None,
            glsl_ext(ctx),
            glsl_opcode,
            vec![Operand::IdRef(a), Operand::IdRef(b)],
        )
        .unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

// ── Integer comparison ────────────────────────────────────────────────

pub fn emit_int_compare(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let a = ctx.resolve_value(inst.arg(0));
    let b = ctx.resolve_value(inst.arg(1));

    let id = match inst.opcode {
        Opcode::IEqual => ctx.builder.i_equal(ctx.bool_type, None, a, b).unwrap(),
        Opcode::INotEqual => ctx
            .builder
            .i_not_equal(ctx.bool_type, None, a, b)
            .unwrap(),
        Opcode::SLessThan => ctx
            .builder
            .s_less_than(ctx.bool_type, None, a, b)
            .unwrap(),
        Opcode::ULessThan => ctx
            .builder
            .u_less_than(ctx.bool_type, None, a, b)
            .unwrap(),
        Opcode::SLessThanEqual => ctx
            .builder
            .s_less_than_equal(ctx.bool_type, None, a, b)
            .unwrap(),
        Opcode::ULessThanEqual => ctx
            .builder
            .u_less_than_equal(ctx.bool_type, None, a, b)
            .unwrap(),
        Opcode::SGreaterThan => ctx
            .builder
            .s_greater_than(ctx.bool_type, None, a, b)
            .unwrap(),
        Opcode::UGreaterThan => ctx
            .builder
            .u_greater_than(ctx.bool_type, None, a, b)
            .unwrap(),
        Opcode::SGreaterThanEqual => ctx
            .builder
            .s_greater_than_equal(ctx.bool_type, None, a, b)
            .unwrap(),
        Opcode::UGreaterThanEqual => ctx
            .builder
            .u_greater_than_equal(ctx.bool_type, None, a, b)
            .unwrap(),
        _ => ctx.const_false,
    };

    ctx.set_value(block_idx, inst_idx, id);
}

// ── Logic ─────────────────────────────────────────────────────────────

pub fn emit_logical(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let id = match inst.opcode {
        Opcode::LogicalNot => {
            let a = ctx.resolve_value(inst.arg(0));
            ctx.builder
                .logical_not(ctx.bool_type, None, a)
                .unwrap()
        }
        Opcode::LogicalOr => {
            let a = ctx.resolve_value(inst.arg(0));
            let b = ctx.resolve_value(inst.arg(1));
            ctx.builder
                .logical_or(ctx.bool_type, None, a, b)
                .unwrap()
        }
        Opcode::LogicalAnd => {
            let a = ctx.resolve_value(inst.arg(0));
            let b = ctx.resolve_value(inst.arg(1));
            ctx.builder
                .logical_and(ctx.bool_type, None, a, b)
                .unwrap()
        }
        Opcode::LogicalXor => {
            let a = ctx.resolve_value(inst.arg(0));
            let b = ctx.resolve_value(inst.arg(1));
            ctx.builder
                .logical_not_equal(ctx.bool_type, None, a, b)
                .unwrap()
        }
        _ => ctx.const_false,
    };

    ctx.set_value(block_idx, inst_idx, id);
}

// ── Select ────────────────────────────────────────────────────────────

pub fn emit_select(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let cond = ctx.resolve_value(inst.arg(0));
    let true_val = ctx.resolve_value(inst.arg(1));
    let false_val = ctx.resolve_value(inst.arg(2));

    let result_type = match inst.opcode {
        Opcode::SelectF32 => ctx.f32_type,
        Opcode::SelectU1 => ctx.bool_type,
        _ => ctx.u32_type,
    };

    let id = ctx
        .builder
        .select(result_type, None, cond, true_val, false_val)
        .unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}
