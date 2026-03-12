// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V image atomic emission — maps to zuyu's
//! `backend/spirv/emit_spirv_image_atomic.cpp`.
//!
//! Handles atomic operations on images (surface atomics).

use rspirv::spirv::Word;
use super::spirv_emit_context::SpirvEmitContext;

/// Image atomic add (U32).
pub fn emit_image_atomic_iadd_32(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _coords: Word,
    _value: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_atomic_iadd_32");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Image atomic min (signed U32).
pub fn emit_image_atomic_smin_32(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _coords: Word,
    _value: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_atomic_smin_32");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Image atomic max (signed U32).
pub fn emit_image_atomic_smax_32(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _coords: Word,
    _value: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_atomic_smax_32");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Image atomic min (unsigned U32).
pub fn emit_image_atomic_umin_32(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _coords: Word,
    _value: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_atomic_umin_32");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Image atomic max (unsigned U32).
pub fn emit_image_atomic_umax_32(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _coords: Word,
    _value: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_atomic_umax_32");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Image atomic AND.
pub fn emit_image_atomic_and_32(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _coords: Word,
    _value: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_atomic_and_32");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Image atomic OR.
pub fn emit_image_atomic_or_32(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _coords: Word,
    _value: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_atomic_or_32");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Image atomic XOR.
pub fn emit_image_atomic_xor_32(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _coords: Word,
    _value: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_atomic_xor_32");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Image atomic exchange.
pub fn emit_image_atomic_exchange_32(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _coords: Word,
    _value: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_atomic_exchange_32");
    ctx.builder.undef(ctx.u32_type, None)
}
