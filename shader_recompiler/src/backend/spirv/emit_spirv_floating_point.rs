// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V floating point emission — maps to zuyu's
//! `backend/spirv/emit_spirv_floating_point.cpp`.

use rspirv::spirv::Word;
use super::spirv_emit_context::SpirvEmitContext;

/// FPAdd32: `OpFAdd` F32.
pub fn emit_fp_add_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder.f_add(ctx.f32_type, None, a, b).unwrap()
}

/// FPSub32: `OpFSub` F32.
pub fn emit_fp_sub_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder.f_sub(ctx.f32_type, None, a, b).unwrap()
}

/// FPMul32: `OpFMul` F32.
pub fn emit_fp_mul_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder.f_mul(ctx.f32_type, None, a, b).unwrap()
}

/// FPDiv32: `OpFDiv` F32.
pub fn emit_fp_div_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder.f_div(ctx.f32_type, None, a, b).unwrap()
}

/// FPFma32: `OpExtInst Fma` F32.
pub fn emit_fp_fma_32(ctx: &mut SpirvEmitContext, a: Word, b: Word, c: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(ctx.f32_type, None, glsl_set, 50 /* Fma */, vec![
            rspirv::dr::Operand::IdRef(a),
            rspirv::dr::Operand::IdRef(b),
            rspirv::dr::Operand::IdRef(c),
        ])
        .unwrap()
}

/// FPNeg32: `OpFNegate` F32.
pub fn emit_fp_neg_32(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    ctx.builder.f_negate(ctx.f32_type, None, value).unwrap()
}

/// FPAbs32: `OpExtInst FAbs` F32.
pub fn emit_fp_abs_32(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(ctx.f32_type, None, glsl_set, 4 /* FAbs */, vec![
            rspirv::dr::Operand::IdRef(value),
        ])
        .unwrap()
}

/// FPMin32: `OpExtInst FMin` F32.
pub fn emit_fp_min_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(ctx.f32_type, None, glsl_set, 37 /* FMin */, vec![
            rspirv::dr::Operand::IdRef(a),
            rspirv::dr::Operand::IdRef(b),
        ])
        .unwrap()
}

/// FPMax32: `OpExtInst FMax` F32.
pub fn emit_fp_max_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(ctx.f32_type, None, glsl_set, 40 /* FMax */, vec![
            rspirv::dr::Operand::IdRef(a),
            rspirv::dr::Operand::IdRef(b),
        ])
        .unwrap()
}

/// FPClamp32: `OpExtInst FClamp` F32.
pub fn emit_fp_clamp_32(
    ctx: &mut SpirvEmitContext,
    value: Word,
    min_val: Word,
    max_val: Word,
) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(ctx.f32_type, None, glsl_set, 43 /* FClamp */, vec![
            rspirv::dr::Operand::IdRef(value),
            rspirv::dr::Operand::IdRef(min_val),
            rspirv::dr::Operand::IdRef(max_val),
        ])
        .unwrap()
}

/// FPSqrt32: `OpExtInst Sqrt` F32.
pub fn emit_fp_sqrt_32(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(ctx.f32_type, None, glsl_set, 31 /* Sqrt */, vec![
            rspirv::dr::Operand::IdRef(value),
        ])
        .unwrap()
}

/// FPRecip32: 1.0 / value.
pub fn emit_fp_recip_32(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    let one = ctx.const_one_f32;
    ctx.builder.f_div(ctx.f32_type, None, one, value).unwrap()
}

/// FPRecipSqrt32: `OpExtInst InverseSqrt` F32.
pub fn emit_fp_recip_sqrt_32(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(
            ctx.f32_type,
            None,
            glsl_set,
            32, /* InverseSqrt */
            vec![rspirv::dr::Operand::IdRef(value)],
        )
        .unwrap()
}

/// FPSin: `OpExtInst Sin` F32.
pub fn emit_fp_sin(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(ctx.f32_type, None, glsl_set, 13 /* Sin */, vec![
            rspirv::dr::Operand::IdRef(value),
        ])
        .unwrap()
}

/// FPCos: `OpExtInst Cos` F32.
pub fn emit_fp_cos(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(ctx.f32_type, None, glsl_set, 14 /* Cos */, vec![
            rspirv::dr::Operand::IdRef(value),
        ])
        .unwrap()
}

/// FPExp2: `OpExtInst Exp2` F32.
pub fn emit_fp_exp2(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(ctx.f32_type, None, glsl_set, 29 /* Exp2 */, vec![
            rspirv::dr::Operand::IdRef(value),
        ])
        .unwrap()
}

/// FPLog2: `OpExtInst Log2` F32.
pub fn emit_fp_log2(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(ctx.f32_type, None, glsl_set, 30 /* Log2 */, vec![
            rspirv::dr::Operand::IdRef(value),
        ])
        .unwrap()
}

/// FPFloor32: `OpExtInst Floor` F32.
pub fn emit_fp_floor_32(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(ctx.f32_type, None, glsl_set, 8 /* Floor */, vec![
            rspirv::dr::Operand::IdRef(value),
        ])
        .unwrap()
}

/// FPCeil32: `OpExtInst Ceil` F32.
pub fn emit_fp_ceil_32(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(ctx.f32_type, None, glsl_set, 9 /* Ceil */, vec![
            rspirv::dr::Operand::IdRef(value),
        ])
        .unwrap()
}

/// FPTrunc32: `OpExtInst Trunc` F32.
pub fn emit_fp_trunc_32(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(ctx.f32_type, None, glsl_set, 3 /* Trunc */, vec![
            rspirv::dr::Operand::IdRef(value),
        ])
        .unwrap()
}

/// FPRoundEven32: `OpExtInst RoundEven` F32.
pub fn emit_fp_round_even_32(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(
            ctx.f32_type,
            None,
            glsl_set,
            2, /* RoundEven */
            vec![rspirv::dr::Operand::IdRef(value)],
        )
        .unwrap()
}

// ── FP Comparison ────────────────────────────────────────────────────────

/// FPOrdEqual32: `OpFOrdEqual`.
pub fn emit_fp_ord_equal_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder
        .f_ord_equal(ctx.bool_type, None, a, b)
        .unwrap()
}

/// FPOrdNotEqual32: `OpFOrdNotEqual`.
pub fn emit_fp_ord_not_equal_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder
        .f_ord_not_equal(ctx.bool_type, None, a, b)
        .unwrap()
}

/// FPOrdLessThan32: `OpFOrdLessThan`.
pub fn emit_fp_ord_less_than_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder
        .f_ord_less_than(ctx.bool_type, None, a, b)
        .unwrap()
}

/// FPOrdGreaterThan32: `OpFOrdGreaterThan`.
pub fn emit_fp_ord_greater_than_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder
        .f_ord_greater_than(ctx.bool_type, None, a, b)
        .unwrap()
}

/// FPOrdLessThanEqual32: `OpFOrdLessThanEqual`.
pub fn emit_fp_ord_less_than_equal_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder
        .f_ord_less_than_equal(ctx.bool_type, None, a, b)
        .unwrap()
}

/// FPOrdGreaterThanEqual32: `OpFOrdGreaterThanEqual`.
pub fn emit_fp_ord_greater_than_equal_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder
        .f_ord_greater_than_equal(ctx.bool_type, None, a, b)
        .unwrap()
}

/// FPUnordEqual32: `OpFUnordEqual`.
pub fn emit_fp_unord_equal_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder
        .f_unord_equal(ctx.bool_type, None, a, b)
        .unwrap()
}

/// FPUnordNotEqual32: `OpFUnordNotEqual`.
pub fn emit_fp_unord_not_equal_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder
        .f_unord_not_equal(ctx.bool_type, None, a, b)
        .unwrap()
}

/// FPIsNan32: `OpIsNan`.
pub fn emit_fp_is_nan_32(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    ctx.builder.is_nan(ctx.bool_type, None, value).unwrap()
}

// ── FP64 arithmetic ──────────────────────────────────────────────────────

/// FPAdd64: `OpFAdd` F64.
pub fn emit_fp_add_64(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder.f_add(ctx.f64_type, None, a, b).unwrap()
}

/// FPSub64: `OpFSub` F64.
pub fn emit_fp_sub_64(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder.f_sub(ctx.f64_type, None, a, b).unwrap()
}

/// FPMul64: `OpFMul` F64.
pub fn emit_fp_mul_64(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder.f_mul(ctx.f64_type, None, a, b).unwrap()
}

/// FPFma64: `OpExtInst Fma` F64.
pub fn emit_fp_fma_64(ctx: &mut SpirvEmitContext, a: Word, b: Word, c: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(ctx.f64_type, None, glsl_set, 50 /* Fma */, vec![
            rspirv::dr::Operand::IdRef(a),
            rspirv::dr::Operand::IdRef(b),
            rspirv::dr::Operand::IdRef(c),
        ])
        .unwrap()
}

/// FPNeg64: `OpFNegate` F64.
pub fn emit_fp_neg_64(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    ctx.builder.f_negate(ctx.f64_type, None, value).unwrap()
}

/// FPAbs64: `OpExtInst FAbs` F64.
pub fn emit_fp_abs_64(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(ctx.f64_type, None, glsl_set, 4 /* FAbs */, vec![
            rspirv::dr::Operand::IdRef(value),
        ])
        .unwrap()
}

/// FPMin64: `OpExtInst FMin` F64.
pub fn emit_fp_min_64(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(ctx.f64_type, None, glsl_set, 37 /* FMin */, vec![
            rspirv::dr::Operand::IdRef(a),
            rspirv::dr::Operand::IdRef(b),
        ])
        .unwrap()
}

/// FPMax64: `OpExtInst FMax` F64.
pub fn emit_fp_max_64(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(ctx.f64_type, None, glsl_set, 40 /* FMax */, vec![
            rspirv::dr::Operand::IdRef(a),
            rspirv::dr::Operand::IdRef(b),
        ])
        .unwrap()
}
