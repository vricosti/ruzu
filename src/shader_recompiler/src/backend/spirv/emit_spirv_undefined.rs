// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V undefined value emission — maps to zuyu's
//! `backend/spirv/emit_spirv_undefined.cpp`.

use super::spirv_emit_context::SpirvEmitContext;
use rspirv::spirv::Word;

/// Emit `OpUndef` for U1 (bool).
///
/// Matches upstream `EmitUndefU1(EmitContext&)`.
pub fn emit_undef_u1(ctx: &mut SpirvEmitContext) -> Word {
    ctx.builder.undef(ctx.bool_type, None)
}

/// Emit `OpUndef` for U32.
///
/// Matches upstream `EmitUndefU32(EmitContext&)`.
pub fn emit_undef_u32(ctx: &mut SpirvEmitContext) -> Word {
    ctx.builder.undef(ctx.u32_type, None)
}

/// Emit `OpUndef` for U64.
///
/// Matches upstream `EmitUndefU64(EmitContext&)` — upstream throws
/// NotImplementedException, but we provide a real undef for completeness.
pub fn emit_undef_u64(ctx: &mut SpirvEmitContext) -> Word {
    ctx.builder.undef(ctx.u64_type, None)
}
