// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V integer operation emission — maps to zuyu's
//! `backend/spirv/emit_spirv_integer.cpp`.

use rspirv::spirv::Word;
use super::spirv_emit_context::SpirvEmitContext;

/// IAdd32: `OpIAdd`.
pub fn emit_iadd_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder.i_add(ctx.u32_type, None, a, b).unwrap()
}

/// ISub32: `OpISub`.
pub fn emit_isub_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder.i_sub(ctx.u32_type, None, a, b).unwrap()
}

/// IMul32: `OpIMul`.
pub fn emit_imul_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder.i_mul(ctx.u32_type, None, a, b).unwrap()
}

/// INeg32: `OpSNegate`.
pub fn emit_ineg_32(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    ctx.builder.s_negate(ctx.u32_type, None, value).unwrap()
}

/// IAbs32: `OpExtInst SAbs`.
pub fn emit_iabs_32(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(ctx.u32_type, None, glsl_set, 5 /* SAbs */, vec![
            rspirv::dr::Operand::IdRef(value),
        ])
        .unwrap()
}

/// ShiftLeftLogical32: `OpShiftLeftLogical`.
pub fn emit_shift_left_logical_32(ctx: &mut SpirvEmitContext, base: Word, shift: Word) -> Word {
    ctx.builder
        .shift_left_logical(ctx.u32_type, None, base, shift)
        .unwrap()
}

/// ShiftRightLogical32: `OpShiftRightLogical`.
pub fn emit_shift_right_logical_32(ctx: &mut SpirvEmitContext, base: Word, shift: Word) -> Word {
    ctx.builder
        .shift_right_logical(ctx.u32_type, None, base, shift)
        .unwrap()
}

/// ShiftRightArithmetic32: `OpShiftRightArithmetic`.
pub fn emit_shift_right_arithmetic_32(
    ctx: &mut SpirvEmitContext,
    base: Word,
    shift: Word,
) -> Word {
    ctx.builder
        .shift_right_arithmetic(ctx.u32_type, None, base, shift)
        .unwrap()
}

/// BitwiseAnd32: `OpBitwiseAnd`.
pub fn emit_bitwise_and_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder
        .bitwise_and(ctx.u32_type, None, a, b)
        .unwrap()
}

/// BitwiseOr32: `OpBitwiseOr`.
pub fn emit_bitwise_or_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder.bitwise_or(ctx.u32_type, None, a, b).unwrap()
}

/// BitwiseXor32: `OpBitwiseXor`.
pub fn emit_bitwise_xor_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder
        .bitwise_xor(ctx.u32_type, None, a, b)
        .unwrap()
}

/// BitwiseNot32: `OpNot`.
pub fn emit_bitwise_not_32(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    ctx.builder.not(ctx.u32_type, None, value).unwrap()
}

/// BitFieldInsert: `OpBitFieldInsert`.
pub fn emit_bit_field_insert(
    ctx: &mut SpirvEmitContext,
    base: Word,
    insert: Word,
    offset: Word,
    count: Word,
) -> Word {
    ctx.builder
        .bit_field_insert(ctx.u32_type, None, base, insert, offset, count)
        .unwrap()
}

/// BitFieldSExtract: `OpBitFieldSExtract`.
pub fn emit_bit_field_s_extract(
    ctx: &mut SpirvEmitContext,
    base: Word,
    offset: Word,
    count: Word,
) -> Word {
    ctx.builder
        .bit_field_s_extract(ctx.u32_type, None, base, offset, count)
        .unwrap()
}

/// BitFieldUExtract: `OpBitFieldUExtract`.
pub fn emit_bit_field_u_extract(
    ctx: &mut SpirvEmitContext,
    base: Word,
    offset: Word,
    count: Word,
) -> Word {
    ctx.builder
        .bit_field_u_extract(ctx.u32_type, None, base, offset, count)
        .unwrap()
}

/// BitCount32: `OpBitCount`.
pub fn emit_bit_count_32(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    ctx.builder.bit_count(ctx.u32_type, None, value).unwrap()
}

/// BitReverse32: `OpBitReverse`.
pub fn emit_bit_reverse_32(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    ctx.builder
        .bit_reverse(ctx.u32_type, None, value)
        .unwrap()
}

/// FindSMsb32: `OpExtInst FindSMsb`.
pub fn emit_find_s_msb_32(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(
            ctx.u32_type,
            None,
            glsl_set,
            74, /* FindSMsb */
            vec![rspirv::dr::Operand::IdRef(value)],
        )
        .unwrap()
}

/// FindUMsb32: `OpExtInst FindUMsb`.
pub fn emit_find_u_msb_32(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(
            ctx.u32_type,
            None,
            glsl_set,
            75, /* FindUMsb */
            vec![rspirv::dr::Operand::IdRef(value)],
        )
        .unwrap()
}

/// SMin32: `OpExtInst SMin`.
pub fn emit_s_min_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(ctx.u32_type, None, glsl_set, 39 /* SMin */, vec![
            rspirv::dr::Operand::IdRef(a),
            rspirv::dr::Operand::IdRef(b),
        ])
        .unwrap()
}

/// SMax32: `OpExtInst SMax`.
pub fn emit_s_max_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(ctx.u32_type, None, glsl_set, 42 /* SMax */, vec![
            rspirv::dr::Operand::IdRef(a),
            rspirv::dr::Operand::IdRef(b),
        ])
        .unwrap()
}

/// UMin32: `OpExtInst UMin`.
pub fn emit_u_min_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(ctx.u32_type, None, glsl_set, 38 /* UMin */, vec![
            rspirv::dr::Operand::IdRef(a),
            rspirv::dr::Operand::IdRef(b),
        ])
        .unwrap()
}

/// UMax32: `OpExtInst UMax`.
pub fn emit_u_max_32(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    let glsl_set = ctx.glsl_ext;
    ctx.builder
        .ext_inst(ctx.u32_type, None, glsl_set, 41 /* UMax */, vec![
            rspirv::dr::Operand::IdRef(a),
            rspirv::dr::Operand::IdRef(b),
        ])
        .unwrap()
}

// ── Integer comparison ───────────────────────────────────────────────────

/// IEqual: `OpIEqual`.
pub fn emit_i_equal(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder.i_equal(ctx.bool_type, None, a, b).unwrap()
}

/// INotEqual: `OpINotEqual`.
pub fn emit_i_not_equal(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder
        .i_not_equal(ctx.bool_type, None, a, b)
        .unwrap()
}

/// SLessThan: `OpSLessThan`.
pub fn emit_s_less_than(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder
        .s_less_than(ctx.bool_type, None, a, b)
        .unwrap()
}

/// ULessThan: `OpULessThan`.
pub fn emit_u_less_than(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder
        .u_less_than(ctx.bool_type, None, a, b)
        .unwrap()
}

/// SLessThanEqual: `OpSLessThanEqual`.
pub fn emit_s_less_than_equal(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder
        .s_less_than_equal(ctx.bool_type, None, a, b)
        .unwrap()
}

/// ULessThanEqual: `OpULessThanEqual`.
pub fn emit_u_less_than_equal(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder
        .u_less_than_equal(ctx.bool_type, None, a, b)
        .unwrap()
}

/// SGreaterThan: `OpSGreaterThan`.
pub fn emit_s_greater_than(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder
        .s_greater_than(ctx.bool_type, None, a, b)
        .unwrap()
}

/// UGreaterThan: `OpUGreaterThan`.
pub fn emit_u_greater_than(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder
        .u_greater_than(ctx.bool_type, None, a, b)
        .unwrap()
}

/// SGreaterThanEqual: `OpSGreaterThanEqual`.
pub fn emit_s_greater_than_equal(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder
        .s_greater_than_equal(ctx.bool_type, None, a, b)
        .unwrap()
}

/// UGreaterThanEqual: `OpUGreaterThanEqual`.
pub fn emit_u_greater_than_equal(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder
        .u_greater_than_equal(ctx.bool_type, None, a, b)
        .unwrap()
}

// ── 64-bit integer ───────────────────────────────────────────────────────

/// IAdd64: `OpIAdd` U64.
pub fn emit_iadd_64(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder.i_add(ctx.u64_type, None, a, b).unwrap()
}

/// ISub64: `OpISub` U64.
pub fn emit_isub_64(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder.i_sub(ctx.u64_type, None, a, b).unwrap()
}

/// INeg64: `OpSNegate` U64.
pub fn emit_ineg_64(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    ctx.builder.s_negate(ctx.u64_type, None, value).unwrap()
}
