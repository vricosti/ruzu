// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V select emission — maps to zuyu's `backend/spirv/emit_spirv_select.cpp`.

use super::spirv_emit_context::SpirvEmitContext;
use rspirv::spirv::Word;

/// Emit `OpSelect` for U1 (bool) result.
///
/// Matches upstream `EmitSelectU1(EmitContext&, Id, Id, Id)`.
pub fn emit_select_u1(
    ctx: &mut SpirvEmitContext,
    cond: Word,
    true_val: Word,
    false_val: Word,
) -> Word {
    ctx.builder
        .select(ctx.bool_type, None, cond, true_val, false_val)
        .unwrap()
}

/// Emit `OpSelect` for U32 result.
///
/// Matches upstream `EmitSelectU32(EmitContext&, Id, Id, Id)`.
pub fn emit_select_u32(
    ctx: &mut SpirvEmitContext,
    cond: Word,
    true_val: Word,
    false_val: Word,
) -> Word {
    ctx.builder
        .select(ctx.u32_type, None, cond, true_val, false_val)
        .unwrap()
}

/// Emit `OpSelect` for U64 result.
///
/// Matches upstream `EmitSelectU64(EmitContext&, Id, Id, Id)`.
pub fn emit_select_u64(
    ctx: &mut SpirvEmitContext,
    cond: Word,
    true_val: Word,
    false_val: Word,
) -> Word {
    ctx.builder
        .select(ctx.u64_type, None, cond, true_val, false_val)
        .unwrap()
}

/// Emit `OpSelect` for F32 result.
///
/// Matches upstream `EmitSelectF32(EmitContext&, Id, Id, Id)`.
pub fn emit_select_f32(
    ctx: &mut SpirvEmitContext,
    cond: Word,
    true_val: Word,
    false_val: Word,
) -> Word {
    ctx.builder
        .select(ctx.f32_type, None, cond, true_val, false_val)
        .unwrap()
}

/// Emit `OpSelect` for F64 result.
///
/// Matches upstream `EmitSelectF64(EmitContext&, Id, Id, Id)`.
pub fn emit_select_f64(
    ctx: &mut SpirvEmitContext,
    cond: Word,
    true_val: Word,
    false_val: Word,
) -> Word {
    ctx.builder
        .select(ctx.f64_type, None, cond, true_val, false_val)
        .unwrap()
}
