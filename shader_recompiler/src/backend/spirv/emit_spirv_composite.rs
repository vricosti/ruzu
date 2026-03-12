// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V composite operation emission — maps to zuyu's
//! `backend/spirv/emit_spirv_composite.cpp`.

use rspirv::spirv::Word;
use super::spirv_emit_context::SpirvEmitContext;

// ── U32 composite operations ─────────────────────────────────────────────

/// Emit `OpCompositeConstruct` for U32x2.
pub fn emit_composite_construct_u32x2(ctx: &mut SpirvEmitContext, e1: Word, e2: Word) -> Word {
    ctx.builder
        .composite_construct(ctx.u32_vec2_type, None, vec![e1, e2])
        .unwrap()
}

/// Emit `OpCompositeConstruct` for U32x3.
pub fn emit_composite_construct_u32x3(
    ctx: &mut SpirvEmitContext,
    e1: Word,
    e2: Word,
    e3: Word,
) -> Word {
    ctx.builder
        .composite_construct(ctx.u32_vec3_type, None, vec![e1, e2, e3])
        .unwrap()
}

/// Emit `OpCompositeConstruct` for U32x4.
pub fn emit_composite_construct_u32x4(
    ctx: &mut SpirvEmitContext,
    e1: Word,
    e2: Word,
    e3: Word,
    e4: Word,
) -> Word {
    ctx.builder
        .composite_construct(ctx.u32_vec4_type, None, vec![e1, e2, e3, e4])
        .unwrap()
}

/// Emit `OpCompositeExtract` from U32x2.
pub fn emit_composite_extract_u32x2(
    ctx: &mut SpirvEmitContext,
    composite: Word,
    index: u32,
) -> Word {
    ctx.builder
        .composite_extract(ctx.u32_type, None, composite, vec![index])
        .unwrap()
}

/// Emit `OpCompositeExtract` from U32x3.
pub fn emit_composite_extract_u32x3(
    ctx: &mut SpirvEmitContext,
    composite: Word,
    index: u32,
) -> Word {
    ctx.builder
        .composite_extract(ctx.u32_type, None, composite, vec![index])
        .unwrap()
}

/// Emit `OpCompositeExtract` from U32x4.
pub fn emit_composite_extract_u32x4(
    ctx: &mut SpirvEmitContext,
    composite: Word,
    index: u32,
) -> Word {
    ctx.builder
        .composite_extract(ctx.u32_type, None, composite, vec![index])
        .unwrap()
}

/// Emit `OpCompositeInsert` into U32x2.
pub fn emit_composite_insert_u32x2(
    ctx: &mut SpirvEmitContext,
    composite: Word,
    object: Word,
    index: u32,
) -> Word {
    ctx.builder
        .composite_insert(ctx.u32_vec2_type, None, object, composite, vec![index])
        .unwrap()
}

/// Emit `OpCompositeInsert` into U32x3.
pub fn emit_composite_insert_u32x3(
    ctx: &mut SpirvEmitContext,
    composite: Word,
    object: Word,
    index: u32,
) -> Word {
    ctx.builder
        .composite_insert(ctx.u32_vec3_type, None, object, composite, vec![index])
        .unwrap()
}

/// Emit `OpCompositeInsert` into U32x4.
pub fn emit_composite_insert_u32x4(
    ctx: &mut SpirvEmitContext,
    composite: Word,
    object: Word,
    index: u32,
) -> Word {
    ctx.builder
        .composite_insert(ctx.u32_vec4_type, None, object, composite, vec![index])
        .unwrap()
}

// ── F32 composite operations ─────────────────────────────────────────────

/// Emit `OpCompositeConstruct` for F32x2.
pub fn emit_composite_construct_f32x2(ctx: &mut SpirvEmitContext, e1: Word, e2: Word) -> Word {
    ctx.builder
        .composite_construct(ctx.f32_vec2_type, None, vec![e1, e2])
        .unwrap()
}

/// Emit `OpCompositeConstruct` for F32x3.
pub fn emit_composite_construct_f32x3(
    ctx: &mut SpirvEmitContext,
    e1: Word,
    e2: Word,
    e3: Word,
) -> Word {
    ctx.builder
        .composite_construct(ctx.f32_vec3_type, None, vec![e1, e2, e3])
        .unwrap()
}

/// Emit `OpCompositeConstruct` for F32x4.
pub fn emit_composite_construct_f32x4(
    ctx: &mut SpirvEmitContext,
    e1: Word,
    e2: Word,
    e3: Word,
    e4: Word,
) -> Word {
    ctx.builder
        .composite_construct(ctx.f32_vec4_type, None, vec![e1, e2, e3, e4])
        .unwrap()
}

/// Emit `OpCompositeExtract` from F32x2.
pub fn emit_composite_extract_f32x2(
    ctx: &mut SpirvEmitContext,
    composite: Word,
    index: u32,
) -> Word {
    ctx.builder
        .composite_extract(ctx.f32_type, None, composite, vec![index])
        .unwrap()
}

/// Emit `OpCompositeExtract` from F32x3.
pub fn emit_composite_extract_f32x3(
    ctx: &mut SpirvEmitContext,
    composite: Word,
    index: u32,
) -> Word {
    ctx.builder
        .composite_extract(ctx.f32_type, None, composite, vec![index])
        .unwrap()
}

/// Emit `OpCompositeExtract` from F32x4.
pub fn emit_composite_extract_f32x4(
    ctx: &mut SpirvEmitContext,
    composite: Word,
    index: u32,
) -> Word {
    ctx.builder
        .composite_extract(ctx.f32_type, None, composite, vec![index])
        .unwrap()
}

/// Emit `OpCompositeInsert` into F32x2.
pub fn emit_composite_insert_f32x2(
    ctx: &mut SpirvEmitContext,
    composite: Word,
    object: Word,
    index: u32,
) -> Word {
    ctx.builder
        .composite_insert(ctx.f32_vec2_type, None, object, composite, vec![index])
        .unwrap()
}

/// Emit `OpCompositeInsert` into F32x3.
pub fn emit_composite_insert_f32x3(
    ctx: &mut SpirvEmitContext,
    composite: Word,
    object: Word,
    index: u32,
) -> Word {
    ctx.builder
        .composite_insert(ctx.f32_vec3_type, None, object, composite, vec![index])
        .unwrap()
}

/// Emit `OpCompositeInsert` into F32x4.
pub fn emit_composite_insert_f32x4(
    ctx: &mut SpirvEmitContext,
    composite: Word,
    object: Word,
    index: u32,
) -> Word {
    ctx.builder
        .composite_insert(ctx.f32_vec4_type, None, object, composite, vec![index])
        .unwrap()
}
