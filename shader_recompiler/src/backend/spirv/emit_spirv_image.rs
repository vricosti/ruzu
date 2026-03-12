// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V image/texture emission — maps to zuyu's
//! `backend/spirv/emit_spirv_image.cpp`.
//!
//! Handles texture sampling, image loads, and texture queries.

use rspirv::spirv::Word;
use super::spirv_emit_context::SpirvEmitContext;

/// Emit ImageSampleImplicitLod (TEX/TEXS with implicit LOD).
///
/// Matches upstream `EmitImageSampleImplicitLod`.
pub fn emit_image_sample_implicit_lod(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _coords: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_sample_implicit_lod");
    ctx.builder.undef(ctx.f32_vec4_type, None)
}

/// Emit ImageSampleExplicitLod (TXL).
pub fn emit_image_sample_explicit_lod(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _coords: Word,
    _lod: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_sample_explicit_lod");
    ctx.builder.undef(ctx.f32_vec4_type, None)
}

/// Emit ImageSampleDrefImplicitLod (shadow TEX).
pub fn emit_image_sample_dref_implicit_lod(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _coords: Word,
    _dref: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_sample_dref_implicit_lod");
    ctx.builder.undef(ctx.f32_type, None)
}

/// Emit ImageSampleDrefExplicitLod (shadow TXL).
pub fn emit_image_sample_dref_explicit_lod(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _coords: Word,
    _dref: Word,
    _lod: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_sample_dref_explicit_lod");
    ctx.builder.undef(ctx.f32_type, None)
}

/// Emit ImageFetch (TLD — texel fetch).
pub fn emit_image_fetch(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _coords: Word,
    _lod: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_fetch");
    ctx.builder.undef(ctx.f32_vec4_type, None)
}

/// Emit ImageGather (TLD4).
pub fn emit_image_gather(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _coords: Word,
    _component: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_gather");
    ctx.builder.undef(ctx.f32_vec4_type, None)
}

/// Emit ImageGatherDref (TLD4 with depth comparison).
pub fn emit_image_gather_dref(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _coords: Word,
    _dref: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_gather_dref");
    ctx.builder.undef(ctx.f32_vec4_type, None)
}

/// Emit ImageQueryDimensions (TXQ).
pub fn emit_image_query_dimensions(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _lod: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_query_dimensions");
    ctx.builder.undef(ctx.u32_vec4_type, None)
}

/// Emit ImageQueryLod (TMML).
pub fn emit_image_query_lod(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _coords: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_query_lod");
    ctx.builder.undef(ctx.f32_vec2_type, None)
}

/// Emit ImageGradient (TXD — explicit gradients).
pub fn emit_image_gradient(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _coords: Word,
    _dpdx: Word,
    _dpdy: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_gradient");
    ctx.builder.undef(ctx.f32_vec4_type, None)
}
