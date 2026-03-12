// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V context get/set emission — maps to zuyu's
//! `backend/spirv/emit_spirv_context_get_set.cpp`.
//!
//! Handles constant buffer loads, attribute loads/stores, and other
//! context-related operations.

use rspirv::spirv::Word;
use super::spirv_emit_context::SpirvEmitContext;

/// Emit a constant buffer load (U32).
///
/// Matches upstream `EmitGetCbufU32(EmitContext&, Id, Id)`.
pub fn emit_get_cbuf_u32(
    ctx: &mut SpirvEmitContext,
    _binding: Word,
    _offset: Word,
) -> Word {
    log::trace!("SPIR-V: emit_get_cbuf_u32");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Emit a constant buffer load (F32).
///
/// Matches upstream `EmitGetCbufF32(EmitContext&, Id, Id)`.
pub fn emit_get_cbuf_f32(
    ctx: &mut SpirvEmitContext,
    _binding: Word,
    _offset: Word,
) -> Word {
    log::trace!("SPIR-V: emit_get_cbuf_f32");
    ctx.builder.undef(ctx.f32_type, None)
}

/// Emit a constant buffer load (S32).
pub fn emit_get_cbuf_s32(
    ctx: &mut SpirvEmitContext,
    _binding: Word,
    _offset: Word,
) -> Word {
    log::trace!("SPIR-V: emit_get_cbuf_s32");
    ctx.builder.undef(ctx.i32_type, None)
}

/// Emit a constant buffer load (U16).
pub fn emit_get_cbuf_u16(
    ctx: &mut SpirvEmitContext,
    _binding: Word,
    _offset: Word,
) -> Word {
    log::trace!("SPIR-V: emit_get_cbuf_u16");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Emit a constant buffer load (S16).
pub fn emit_get_cbuf_s16(
    ctx: &mut SpirvEmitContext,
    _binding: Word,
    _offset: Word,
) -> Word {
    log::trace!("SPIR-V: emit_get_cbuf_s16");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Emit a constant buffer load (U8).
pub fn emit_get_cbuf_u8(
    ctx: &mut SpirvEmitContext,
    _binding: Word,
    _offset: Word,
) -> Word {
    log::trace!("SPIR-V: emit_get_cbuf_u8");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Emit a constant buffer load (S8).
pub fn emit_get_cbuf_s8(
    ctx: &mut SpirvEmitContext,
    _binding: Word,
    _offset: Word,
) -> Word {
    log::trace!("SPIR-V: emit_get_cbuf_s8");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Emit a load of a shader input attribute.
///
/// Matches upstream `EmitGetAttribute(EmitContext&, Id, u32)`.
pub fn emit_get_attribute(
    ctx: &mut SpirvEmitContext,
    _attribute: Word,
) -> Word {
    log::trace!("SPIR-V: emit_get_attribute");
    ctx.builder.undef(ctx.f32_type, None)
}

/// Emit a store to a shader output attribute.
///
/// Matches upstream `EmitSetAttribute(EmitContext&, Id, Id, u32)`.
pub fn emit_set_attribute(
    _ctx: &mut SpirvEmitContext,
    _attribute: Word,
    _value: Word,
) {
    log::trace!("SPIR-V: emit_set_attribute");
}

/// Emit a load of a patch attribute.
pub fn emit_get_patch(ctx: &mut SpirvEmitContext, _patch: Word) -> Word {
    log::trace!("SPIR-V: emit_get_patch");
    ctx.builder.undef(ctx.f32_type, None)
}

/// Emit a store to a patch attribute.
pub fn emit_set_patch(_ctx: &mut SpirvEmitContext, _patch: Word, _value: Word) {
    log::trace!("SPIR-V: emit_set_patch");
}

/// Emit SetFragColor.
pub fn emit_set_frag_color(
    _ctx: &mut SpirvEmitContext,
    _render_target: Word,
    _component: Word,
    _value: Word,
) {
    log::trace!("SPIR-V: emit_set_frag_color");
}

/// Emit SetFragDepth.
pub fn emit_set_frag_depth(_ctx: &mut SpirvEmitContext, _value: Word) {
    log::trace!("SPIR-V: emit_set_frag_depth");
}

/// Emit SetSampleMask.
pub fn emit_set_sample_mask(_ctx: &mut SpirvEmitContext, _value: Word) {
    log::trace!("SPIR-V: emit_set_sample_mask");
}
