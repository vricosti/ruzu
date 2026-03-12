// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V bitwise conversion emission — maps to zuyu's
//! `backend/spirv/emit_spirv_bitwise_conversion.cpp`.

use rspirv::spirv::Word;
use super::spirv_emit_context::SpirvEmitContext;

/// Emit `OpBitcast` U32 <- F32.
///
/// Matches upstream `EmitBitCastU32F32(EmitContext&, Id)`.
pub fn emit_bit_cast_u32_f32(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    ctx.builder.bitcast(ctx.u32_type, None, value).unwrap()
}

/// Emit `OpBitcast` F32 <- U32.
///
/// Matches upstream `EmitBitCastF32U32(EmitContext&, Id)`.
pub fn emit_bit_cast_f32_u32(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    ctx.builder.bitcast(ctx.f32_type, None, value).unwrap()
}

/// Emit `OpBitcast` U64 <- U32x2 (PackUint2x32).
///
/// Matches upstream `EmitPackUint2x32(EmitContext&, Id)`.
pub fn emit_pack_uint2x32(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    ctx.builder.bitcast(ctx.u64_type, None, value).unwrap()
}

/// Emit `OpBitcast` U32x2 <- U64 (UnpackUint2x32).
///
/// Matches upstream `EmitUnpackUint2x32(EmitContext&, Id)`.
pub fn emit_unpack_uint2x32(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    ctx.builder
        .bitcast(ctx.u32_vec2_type, None, value)
        .unwrap()
}

/// Emit `OpBitcast` U32 <- F16x2 (PackFloat2x16).
///
/// Matches upstream `EmitPackFloat2x16(EmitContext&, Id)`.
pub fn emit_pack_float2x16(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    ctx.builder.bitcast(ctx.u32_type, None, value).unwrap()
}

/// Emit `OpBitcast` F64 <- U32x2 (PackDouble2x32).
///
/// Matches upstream `EmitPackDouble2x32(EmitContext&, Id)`.
pub fn emit_pack_double2x32(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    ctx.builder.bitcast(ctx.f64_type, None, value).unwrap()
}

/// Emit `OpBitcast` U32x2 <- F64 (UnpackDouble2x32).
///
/// Matches upstream `EmitUnpackDouble2x32(EmitContext&, Id)`.
pub fn emit_unpack_double2x32(ctx: &mut SpirvEmitContext, value: Word) -> Word {
    ctx.builder
        .bitcast(ctx.u32_vec2_type, None, value)
        .unwrap()
}
