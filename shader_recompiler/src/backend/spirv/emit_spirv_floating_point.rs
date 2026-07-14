// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V floating point emission — maps to zuyu's
//! `backend/spirv/emit_spirv_floating_point.cpp`.

use super::spirv_emit_context::SpirvEmitContext;
use crate::ir::instruction::Inst;
use crate::ir::types::FpControl;
use rspirv::spirv::{Decoration, Word};

/// Port of the local `Decorate` helper in
/// `emit_spirv_floating_point.cpp`.
fn decorate(ctx: &mut SpirvEmitContext, inst: &Inst, result: Word) -> Word {
    if FpControl::from_u32(inst.flags).no_contraction {
        ctx.builder
            .decorate(result, Decoration::NoContraction, vec![]);
    }
    result
}

/// FPAdd32: `OpFAdd` F32.
pub fn emit_fp_add_32(ctx: &mut SpirvEmitContext, inst: &Inst, a: Word, b: Word) -> Word {
    let result = ctx.builder.f_add(ctx.f32_type, None, a, b).unwrap();
    decorate(ctx, inst, result)
}

/// FPSub32: `OpFSub` F32.
pub fn emit_fp_sub_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder.f_sub(ctx.f32_type, None, a, b).unwrap()
}

/// FPMul32: `OpFMul` F32.
pub fn emit_fp_mul_32(ctx: &mut SpirvEmitContext, inst: &Inst, a: Word, b: Word) -> Word {
    let result = ctx.builder.f_mul(ctx.f32_type, None, a, b).unwrap();
    decorate(ctx, inst, result)
}

/// FPDiv32: `OpFDiv` F32.
pub fn emit_fp_div_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder.f_div(ctx.f32_type, None, a, b).unwrap()
}

/// FPFma32: `OpExtInst Fma` F32.
pub fn emit_fp_fma_32(ctx: &mut SpirvEmitContext, inst: &Inst, a: Word, b: Word, c: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    let result = ctx
        .builder
        .ext_inst(
            ctx.f32_type,
            None,
            glsl_set,
            50, /* Fma */
            vec![
                rspirv::dr::Operand::IdRef(a),
                rspirv::dr::Operand::IdRef(b),
                rspirv::dr::Operand::IdRef(c),
            ],
        )
        .unwrap();
    decorate(ctx, inst, result)
}

/// FPNeg32: `OpFNegate` F32.
pub fn emit_fp_neg_32(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    ctx.builder.f_negate(ctx.f32_type, None, value).unwrap()
}

/// FPAbs32: `OpExtInst FAbs` F32.
pub fn emit_fp_abs_32(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(
            ctx.f32_type,
            None,
            glsl_set,
            4, /* FAbs */
            vec![rspirv::dr::Operand::IdRef(value)],
        )
        .unwrap()
}

/// FPMin32: `OpExtInst FMin` F32.
pub fn emit_fp_min_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(
            ctx.f32_type,
            None,
            glsl_set,
            37, /* FMin */
            vec![rspirv::dr::Operand::IdRef(a), rspirv::dr::Operand::IdRef(b)],
        )
        .unwrap()
}

/// FPMax32: `OpExtInst FMax` F32.
pub fn emit_fp_max_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(
            ctx.f32_type,
            None,
            glsl_set,
            40, /* FMax */
            vec![rspirv::dr::Operand::IdRef(a), rspirv::dr::Operand::IdRef(b)],
        )
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
        .ext_inst(
            ctx.f32_type,
            None,
            glsl_set,
            43, /* FClamp */
            vec![
                rspirv::dr::Operand::IdRef(value),
                rspirv::dr::Operand::IdRef(min_val),
                rspirv::dr::Operand::IdRef(max_val),
            ],
        )
        .unwrap()
}

/// FPSqrt32: `OpExtInst Sqrt` F32.
pub fn emit_fp_sqrt_32(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(
            ctx.f32_type,
            None,
            glsl_set,
            31, /* Sqrt */
            vec![rspirv::dr::Operand::IdRef(value)],
        )
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
        .ext_inst(
            ctx.f32_type,
            None,
            glsl_set,
            13, /* Sin */
            vec![rspirv::dr::Operand::IdRef(value)],
        )
        .unwrap()
}

/// FPCos: `OpExtInst Cos` F32.
pub fn emit_fp_cos(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(
            ctx.f32_type,
            None,
            glsl_set,
            14, /* Cos */
            vec![rspirv::dr::Operand::IdRef(value)],
        )
        .unwrap()
}

/// FPExp2: `OpExtInst Exp2` F32.
pub fn emit_fp_exp2(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(
            ctx.f32_type,
            None,
            glsl_set,
            29, /* Exp2 */
            vec![rspirv::dr::Operand::IdRef(value)],
        )
        .unwrap()
}

/// FPLog2: `OpExtInst Log2` F32.
pub fn emit_fp_log2(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(
            ctx.f32_type,
            None,
            glsl_set,
            30, /* Log2 */
            vec![rspirv::dr::Operand::IdRef(value)],
        )
        .unwrap()
}

/// FPFloor32: `OpExtInst Floor` F32.
pub fn emit_fp_floor_32(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(
            ctx.f32_type,
            None,
            glsl_set,
            8, /* Floor */
            vec![rspirv::dr::Operand::IdRef(value)],
        )
        .unwrap()
}

/// FPCeil32: `OpExtInst Ceil` F32.
pub fn emit_fp_ceil_32(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(
            ctx.f32_type,
            None,
            glsl_set,
            9, /* Ceil */
            vec![rspirv::dr::Operand::IdRef(value)],
        )
        .unwrap()
}

/// FPTrunc32: `OpExtInst Trunc` F32.
pub fn emit_fp_trunc_32(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(
            ctx.f32_type,
            None,
            glsl_set,
            3, /* Trunc */
            vec![rspirv::dr::Operand::IdRef(value)],
        )
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

/// FPOrdEqual{16,32,64}: `OpFOrdEqual`.
pub fn emit_fp_ord_equal(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder.f_ord_equal(ctx.bool_type, None, a, b).unwrap()
}

/// FPOrdEqual32: `OpFOrdEqual`.
pub fn emit_fp_ord_equal_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    emit_fp_ord_equal(ctx, a, b)
}

/// FPOrdNotEqual{16,32,64}: `OpFOrdNotEqual`.
pub fn emit_fp_ord_not_equal(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder
        .f_ord_not_equal(ctx.bool_type, None, a, b)
        .unwrap()
}

/// FPOrdNotEqual32: `OpFOrdNotEqual`.
pub fn emit_fp_ord_not_equal_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    emit_fp_ord_not_equal(ctx, a, b)
}

/// FPOrdLessThan{16,32,64}: `OpFOrdLessThan`.
pub fn emit_fp_ord_less_than(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder
        .f_ord_less_than(ctx.bool_type, None, a, b)
        .unwrap()
}

/// FPOrdLessThan32: `OpFOrdLessThan`.
pub fn emit_fp_ord_less_than_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    emit_fp_ord_less_than(ctx, a, b)
}

/// FPOrdGreaterThan{16,32,64}: `OpFOrdGreaterThan`.
pub fn emit_fp_ord_greater_than(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder
        .f_ord_greater_than(ctx.bool_type, None, a, b)
        .unwrap()
}

/// FPOrdGreaterThan32: `OpFOrdGreaterThan`.
pub fn emit_fp_ord_greater_than_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    emit_fp_ord_greater_than(ctx, a, b)
}

/// FPOrdLessThanEqual{16,32,64}: `OpFOrdLessThanEqual`.
pub fn emit_fp_ord_less_than_equal(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder
        .f_ord_less_than_equal(ctx.bool_type, None, a, b)
        .unwrap()
}

/// FPOrdLessThanEqual32: `OpFOrdLessThanEqual`.
pub fn emit_fp_ord_less_than_equal_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    emit_fp_ord_less_than_equal(ctx, a, b)
}

/// FPOrdGreaterThanEqual{16,32,64}: `OpFOrdGreaterThanEqual`.
pub fn emit_fp_ord_greater_than_equal(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder
        .f_ord_greater_than_equal(ctx.bool_type, None, a, b)
        .unwrap()
}

/// FPOrdGreaterThanEqual32: `OpFOrdGreaterThanEqual`.
pub fn emit_fp_ord_greater_than_equal_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    emit_fp_ord_greater_than_equal(ctx, a, b)
}

/// FPUnordEqual{16,32,64}: `OpFUnordEqual`.
pub fn emit_fp_unord_equal(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder
        .f_unord_equal(ctx.bool_type, None, a, b)
        .unwrap()
}

/// FPUnordEqual32: `OpFUnordEqual`.
pub fn emit_fp_unord_equal_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    emit_fp_unord_equal(ctx, a, b)
}

/// FPUnordNotEqual{16,32,64}: `OpFUnordNotEqual`.
pub fn emit_fp_unord_not_equal(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder
        .f_unord_not_equal(ctx.bool_type, None, a, b)
        .unwrap()
}

/// FPUnordNotEqual32: `OpFUnordNotEqual`.
pub fn emit_fp_unord_not_equal_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    emit_fp_unord_not_equal(ctx, a, b)
}

/// FPUnordLessThan{16,32,64}: `OpFUnordLessThan`.
pub fn emit_fp_unord_less_than(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder
        .f_unord_less_than(ctx.bool_type, None, a, b)
        .unwrap()
}

/// FPUnordLessThan32: `OpFUnordLessThan`.
pub fn emit_fp_unord_less_than_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    emit_fp_unord_less_than(ctx, a, b)
}

/// FPUnordGreaterThan{16,32,64}: `OpFUnordGreaterThan`.
pub fn emit_fp_unord_greater_than(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder
        .f_unord_greater_than(ctx.bool_type, None, a, b)
        .unwrap()
}

/// FPUnordGreaterThan32: `OpFUnordGreaterThan`.
pub fn emit_fp_unord_greater_than_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    emit_fp_unord_greater_than(ctx, a, b)
}

/// FPUnordLessThanEqual{16,32,64}: `OpFUnordLessThanEqual`.
pub fn emit_fp_unord_less_than_equal(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder
        .f_unord_less_than_equal(ctx.bool_type, None, a, b)
        .unwrap()
}

/// FPUnordLessThanEqual32: `OpFUnordLessThanEqual`.
pub fn emit_fp_unord_less_than_equal_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    emit_fp_unord_less_than_equal(ctx, a, b)
}

/// FPUnordGreaterThanEqual{16,32,64}: `OpFUnordGreaterThanEqual`.
pub fn emit_fp_unord_greater_than_equal(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder
        .f_unord_greater_than_equal(ctx.bool_type, None, a, b)
        .unwrap()
}

/// FPUnordGreaterThanEqual32: `OpFUnordGreaterThanEqual`.
pub fn emit_fp_unord_greater_than_equal_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    emit_fp_unord_greater_than_equal(ctx, a, b)
}

/// FPIsNan{16,32,64}: `OpIsNan`.
pub fn emit_fp_is_nan(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    ctx.builder.is_nan(ctx.bool_type, None, value).unwrap()
}

/// FPIsNan32: `OpIsNan`.
pub fn emit_fp_is_nan_32(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    emit_fp_is_nan(ctx, value)
}

// ── FP64 arithmetic ──────────────────────────────────────────────────────

/// FPAdd64: `OpFAdd` F64.
pub fn emit_fp_add_64(ctx: &mut SpirvEmitContext, inst: &Inst, a: Word, b: Word) -> Word {
    let result = ctx.builder.f_add(ctx.f64_type, None, a, b).unwrap();
    decorate(ctx, inst, result)
}

/// FPSub64: `OpFSub` F64.
pub fn emit_fp_sub_64(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder.f_sub(ctx.f64_type, None, a, b).unwrap()
}

/// FPMul64: `OpFMul` F64.
pub fn emit_fp_mul_64(ctx: &mut SpirvEmitContext, inst: &Inst, a: Word, b: Word) -> Word {
    let result = ctx.builder.f_mul(ctx.f64_type, None, a, b).unwrap();
    decorate(ctx, inst, result)
}

/// FPFma64: `OpExtInst Fma` F64.
pub fn emit_fp_fma_64(ctx: &mut SpirvEmitContext, inst: &Inst, a: Word, b: Word, c: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    let result = ctx
        .builder
        .ext_inst(
            ctx.f64_type,
            None,
            glsl_set,
            50, /* Fma */
            vec![
                rspirv::dr::Operand::IdRef(a),
                rspirv::dr::Operand::IdRef(b),
                rspirv::dr::Operand::IdRef(c),
            ],
        )
        .unwrap();
    decorate(ctx, inst, result)
}

/// FPNeg64: `OpFNegate` F64.
pub fn emit_fp_neg_64(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    ctx.builder.f_negate(ctx.f64_type, None, value).unwrap()
}

/// FPAbs64: `OpExtInst FAbs` F64.
pub fn emit_fp_abs_64(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(
            ctx.f64_type,
            None,
            glsl_set,
            4, /* FAbs */
            vec![rspirv::dr::Operand::IdRef(value)],
        )
        .unwrap()
}

/// FPMin64: `OpExtInst FMin` F64.
pub fn emit_fp_min_64(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(
            ctx.f64_type,
            None,
            glsl_set,
            37, /* FMin */
            vec![rspirv::dr::Operand::IdRef(a), rspirv::dr::Operand::IdRef(b)],
        )
        .unwrap()
}

/// FPMax64: `OpExtInst FMax` F64.
pub fn emit_fp_max_64(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(
            ctx.f64_type,
            None,
            glsl_set,
            40, /* FMax */
            vec![rspirv::dr::Operand::IdRef(a), rspirv::dr::Operand::IdRef(b)],
        )
        .unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::opcodes::Opcode;
    use crate::ir::program::Program;
    use crate::ir::types::ShaderStage;
    use crate::ir::value::Value;
    use crate::profile::Profile;
    use crate::runtime_info::RuntimeInfo;
    use rspirv::dr::Operand;

    #[test]
    fn fp_no_contraction_flag_decorates_spirv_result() {
        let program = Program::new(ShaderStage::Fragment);
        let profile = Profile::default();
        let runtime_info = RuntimeInfo::default();
        let mut ctx = SpirvEmitContext::new(&program, &profile, &runtime_info);
        ctx.builder
            .begin_function(
                ctx.void_type,
                None,
                rspirv::spirv::FunctionControl::NONE,
                ctx.void_fn_type,
            )
            .unwrap();
        ctx.builder.begin_block(None).unwrap();

        let inst = Inst::with_flags(
            Opcode::FPAdd32,
            vec![Value::ImmF32(0.0), Value::ImmF32(1.0)],
            FpControl {
                no_contraction: true,
                ..FpControl::default()
            }
            .to_u32(),
        );
        let zero = ctx.const_zero_f32;
        let one = ctx.const_one_f32;
        let result = emit_fp_add_32(&mut ctx, &inst, zero, one);

        assert!(ctx
            .builder
            .module_ref()
            .annotations
            .iter()
            .any(|annotation| {
                matches!(
                    annotation.operands.as_slice(),
                    [Operand::IdRef(id), Operand::Decoration(Decoration::NoContraction)]
                        if *id == result
                )
            }));
    }

    #[test]
    fn fp64_no_contraction_flag_decorates_spirv_results() {
        let mut program = Program::new(ShaderStage::Fragment);
        program.info.uses_fp64 = true;
        let profile = Profile::default();
        let runtime_info = RuntimeInfo::default();
        let mut ctx = SpirvEmitContext::new(&program, &profile, &runtime_info);
        ctx.builder
            .begin_function(
                ctx.void_type,
                None,
                rspirv::spirv::FunctionControl::NONE,
                ctx.void_fn_type,
            )
            .unwrap();
        ctx.builder.begin_block(None).unwrap();
        let control = FpControl {
            no_contraction: true,
            ..FpControl::default()
        };
        let inst = Inst::with_flags(
            Opcode::FPAdd64,
            vec![Value::ImmF64(0.0), Value::ImmF64(1.0)],
            control.to_u32(),
        );
        let zero = ctx.builder.constant_bit64(ctx.f64_type, 0.0f64.to_bits());
        let one = ctx.builder.constant_bit64(ctx.f64_type, 1.0f64.to_bits());
        let results = [
            emit_fp_add_64(&mut ctx, &inst, zero, one),
            emit_fp_mul_64(&mut ctx, &inst, zero, one),
            emit_fp_fma_64(&mut ctx, &inst, zero, one, one),
        ];

        for result in results {
            assert!(ctx
                .builder
                .module_ref()
                .annotations
                .iter()
                .any(|annotation| {
                    matches!(
                        annotation.operands.as_slice(),
                        [Operand::IdRef(id), Operand::Decoration(Decoration::NoContraction)]
                            if *id == result
                    )
                }));
        }
    }
}
