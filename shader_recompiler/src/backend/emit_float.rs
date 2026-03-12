// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V emission for FP32 arithmetic, transcendentals, and comparison opcodes.

use rspirv::dr::Operand;
use rspirv::spirv;

use super::spirv_context::EmitContext;
use crate::ir::{self, Opcode};

/// Helper: get GLSL.std.450 extension set ID (always the first ext_inst_import).
fn glsl_ext(ctx: &EmitContext) -> spirv::Word {
    // rspirv assigns ext_inst_import IDs starting from 1; the first one is GLSL.std.450
    1
}

// ── Binary arithmetic ─────────────────────────────────────────────────

pub fn emit_fp_add_32(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let a = ctx.resolve_value(inst.arg(0));
    let b = ctx.resolve_value(inst.arg(1));
    let id = ctx.builder.f_add(ctx.f32_type, None, a, b).unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

pub fn emit_fp_sub_32(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let a = ctx.resolve_value(inst.arg(0));
    let b = ctx.resolve_value(inst.arg(1));
    let id = ctx.builder.f_sub(ctx.f32_type, None, a, b).unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

pub fn emit_fp_mul_32(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let a = ctx.resolve_value(inst.arg(0));
    let b = ctx.resolve_value(inst.arg(1));
    let id = ctx.builder.f_mul(ctx.f32_type, None, a, b).unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

pub fn emit_fp_div_32(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let a = ctx.resolve_value(inst.arg(0));
    let b = ctx.resolve_value(inst.arg(1));
    let id = ctx.builder.f_div(ctx.f32_type, None, a, b).unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

pub fn emit_fp_fma_32(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let a = ctx.resolve_value(inst.arg(0));
    let b = ctx.resolve_value(inst.arg(1));
    let c = ctx.resolve_value(inst.arg(2));
    // GLSL.std.450 Fma = opcode 26
    let id = ctx
        .builder
        .ext_inst(
            ctx.f32_type,
            None,
            glsl_ext(ctx),
            26, // Fma
            vec![Operand::IdRef(a), Operand::IdRef(b), Operand::IdRef(c)],
        )
        .unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

// ── Unary arithmetic ──────────────────────────────────────────────────

pub fn emit_fp_neg_32(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let a = ctx.resolve_value(inst.arg(0));
    let id = ctx.builder.f_negate(ctx.f32_type, None, a).unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

pub fn emit_fp_abs_32(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let a = ctx.resolve_value(inst.arg(0));
    // GLSL.std.450 FAbs = opcode 4
    let id = ctx
        .builder
        .ext_inst(
            ctx.f32_type,
            None,
            glsl_ext(ctx),
            4, // FAbs
            vec![Operand::IdRef(a)],
        )
        .unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

pub fn emit_fp_saturate_32(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let a = ctx.resolve_value(inst.arg(0));
    // GLSL.std.450 FClamp = opcode 43
    let id = ctx
        .builder
        .ext_inst(
            ctx.f32_type,
            None,
            glsl_ext(ctx),
            43, // FClamp
            vec![
                Operand::IdRef(a),
                Operand::IdRef(ctx.const_zero_f32),
                Operand::IdRef(ctx.const_one_f32),
            ],
        )
        .unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

// ── Min / Max ─────────────────────────────────────────────────────────

pub fn emit_fp_min_32(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let a = ctx.resolve_value(inst.arg(0));
    let b = ctx.resolve_value(inst.arg(1));
    // GLSL.std.450 FMin = opcode 37
    let id = ctx
        .builder
        .ext_inst(
            ctx.f32_type,
            None,
            glsl_ext(ctx),
            37, // FMin
            vec![Operand::IdRef(a), Operand::IdRef(b)],
        )
        .unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

pub fn emit_fp_max_32(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let a = ctx.resolve_value(inst.arg(0));
    let b = ctx.resolve_value(inst.arg(1));
    // GLSL.std.450 FMax = opcode 40
    let id = ctx
        .builder
        .ext_inst(
            ctx.f32_type,
            None,
            glsl_ext(ctx),
            40, // FMax
            vec![Operand::IdRef(a), Operand::IdRef(b)],
        )
        .unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

// ── Transcendentals ───────────────────────────────────────────────────

pub fn emit_fp_sin(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let a = ctx.resolve_value(inst.arg(0));
    // GLSL.std.450 Sin = opcode 13
    let id = ctx
        .builder
        .ext_inst(
            ctx.f32_type,
            None,
            glsl_ext(ctx),
            13, // Sin
            vec![Operand::IdRef(a)],
        )
        .unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

pub fn emit_fp_cos(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let a = ctx.resolve_value(inst.arg(0));
    // GLSL.std.450 Cos = opcode 14
    let id = ctx
        .builder
        .ext_inst(
            ctx.f32_type,
            None,
            glsl_ext(ctx),
            14, // Cos
            vec![Operand::IdRef(a)],
        )
        .unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

pub fn emit_fp_exp2(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let a = ctx.resolve_value(inst.arg(0));
    // GLSL.std.450 Exp2 = opcode 29
    let id = ctx
        .builder
        .ext_inst(
            ctx.f32_type,
            None,
            glsl_ext(ctx),
            29, // Exp2
            vec![Operand::IdRef(a)],
        )
        .unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

pub fn emit_fp_log2(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let a = ctx.resolve_value(inst.arg(0));
    // GLSL.std.450 Log2 = opcode 30
    let id = ctx
        .builder
        .ext_inst(
            ctx.f32_type,
            None,
            glsl_ext(ctx),
            30, // Log2
            vec![Operand::IdRef(a)],
        )
        .unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

pub fn emit_fp_sqrt_32(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let a = ctx.resolve_value(inst.arg(0));
    // GLSL.std.450 Sqrt = opcode 31
    let id = ctx
        .builder
        .ext_inst(
            ctx.f32_type,
            None,
            glsl_ext(ctx),
            31, // Sqrt
            vec![Operand::IdRef(a)],
        )
        .unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

pub fn emit_fp_recip_32(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let a = ctx.resolve_value(inst.arg(0));
    // 1.0 / a
    let id = ctx
        .builder
        .f_div(ctx.f32_type, None, ctx.const_one_f32, a)
        .unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

pub fn emit_fp_recip_sqrt_32(
    ctx: &mut EmitContext,
    inst: &ir::Inst,
    block_idx: u32,
    inst_idx: u32,
) {
    let a = ctx.resolve_value(inst.arg(0));
    // GLSL.std.450 InverseSqrt = opcode 32
    let id = ctx
        .builder
        .ext_inst(
            ctx.f32_type,
            None,
            glsl_ext(ctx),
            32, // InverseSqrt
            vec![Operand::IdRef(a)],
        )
        .unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

// ── Rounding ──────────────────────────────────────────────────────────

pub fn emit_fp_floor_32(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let a = ctx.resolve_value(inst.arg(0));
    // GLSL.std.450 Floor = opcode 8
    let id = ctx
        .builder
        .ext_inst(
            ctx.f32_type,
            None,
            glsl_ext(ctx),
            8, // Floor
            vec![Operand::IdRef(a)],
        )
        .unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

pub fn emit_fp_ceil_32(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let a = ctx.resolve_value(inst.arg(0));
    // GLSL.std.450 Ceil = opcode 9
    let id = ctx
        .builder
        .ext_inst(
            ctx.f32_type,
            None,
            glsl_ext(ctx),
            9, // Ceil
            vec![Operand::IdRef(a)],
        )
        .unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

pub fn emit_fp_trunc_32(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let a = ctx.resolve_value(inst.arg(0));
    // GLSL.std.450 Trunc = opcode 3
    let id = ctx
        .builder
        .ext_inst(
            ctx.f32_type,
            None,
            glsl_ext(ctx),
            3, // Trunc
            vec![Operand::IdRef(a)],
        )
        .unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

pub fn emit_fp_round_even_32(
    ctx: &mut EmitContext,
    inst: &ir::Inst,
    block_idx: u32,
    inst_idx: u32,
) {
    let a = ctx.resolve_value(inst.arg(0));
    // GLSL.std.450 RoundEven = opcode 2
    let id = ctx
        .builder
        .ext_inst(
            ctx.f32_type,
            None,
            glsl_ext(ctx),
            2, // RoundEven
            vec![Operand::IdRef(a)],
        )
        .unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}

// ── Comparison ────────────────────────────────────────────────────────

pub fn emit_fp_compare(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let a = ctx.resolve_value(inst.arg(0));
    let b = ctx.resolve_value(inst.arg(1));

    let id = match inst.opcode {
        Opcode::FPOrdEqual32 => ctx
            .builder
            .f_ord_equal(ctx.bool_type, None, a, b)
            .unwrap(),
        Opcode::FPOrdNotEqual32 => ctx
            .builder
            .f_ord_not_equal(ctx.bool_type, None, a, b)
            .unwrap(),
        Opcode::FPOrdLessThan32 => ctx
            .builder
            .f_ord_less_than(ctx.bool_type, None, a, b)
            .unwrap(),
        Opcode::FPOrdGreaterThan32 => ctx
            .builder
            .f_ord_greater_than(ctx.bool_type, None, a, b)
            .unwrap(),
        Opcode::FPOrdLessThanEqual32 => ctx
            .builder
            .f_ord_less_than_equal(ctx.bool_type, None, a, b)
            .unwrap(),
        Opcode::FPOrdGreaterThanEqual32 => ctx
            .builder
            .f_ord_greater_than_equal(ctx.bool_type, None, a, b)
            .unwrap(),
        Opcode::FPUnordEqual32 => ctx
            .builder
            .f_unord_equal(ctx.bool_type, None, a, b)
            .unwrap(),
        Opcode::FPUnordNotEqual32 => ctx
            .builder
            .f_unord_not_equal(ctx.bool_type, None, a, b)
            .unwrap(),
        Opcode::FPUnordLessThan32 => ctx
            .builder
            .f_unord_less_than(ctx.bool_type, None, a, b)
            .unwrap(),
        Opcode::FPUnordGreaterThan32 => ctx
            .builder
            .f_unord_greater_than(ctx.bool_type, None, a, b)
            .unwrap(),
        _ => ctx.const_false,
    };

    ctx.set_value(block_idx, inst_idx, id);
}

pub fn emit_fp_is_nan_32(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let a = ctx.resolve_value(inst.arg(0));
    let id = ctx.builder.is_nan(ctx.bool_type, None, a).unwrap();
    ctx.set_value(block_idx, inst_idx, id);
}
