// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V logical operation emission — maps to zuyu's
//! `backend/spirv/emit_spirv_logical.cpp`.

use super::spirv_emit_context::SpirvEmitContext;
use rspirv::spirv::Word;

/// Emit `OpLogicalOr`.
///
/// Matches upstream `EmitLogicalOr(EmitContext&, Id, Id)`.
pub fn emit_logical_or(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder.logical_or(ctx.bool_type, None, a, b).unwrap()
}

/// Emit `OpLogicalAnd`.
///
/// Matches upstream `EmitLogicalAnd(EmitContext&, Id, Id)`.
pub fn emit_logical_and(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder.logical_and(ctx.bool_type, None, a, b).unwrap()
}

/// Emit `OpLogicalXor` (implemented as `OpLogicalNotEqual`).
///
/// Matches upstream `EmitLogicalXor(EmitContext&, Id, Id)`.
pub fn emit_logical_xor(ctx: &mut SpirvEmitContext, a: Word, b: Word) -> Word {
    ctx.builder
        .logical_not_equal(ctx.bool_type, None, a, b)
        .unwrap()
}

/// Emit `OpLogicalNot`.
///
/// Matches upstream `EmitLogicalNot(EmitContext&, Id)`.
pub fn emit_logical_not(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    ctx.builder.logical_not(ctx.bool_type, None, value).unwrap()
}
