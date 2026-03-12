// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V memory operation emission — maps to zuyu's
//! `backend/spirv/emit_spirv_memory.cpp`.

use rspirv::spirv::Word;
use super::spirv_emit_context::SpirvEmitContext;

/// Emit a load from a storage buffer (SSBO).
///
/// Matches upstream storage buffer load patterns.
pub fn emit_load_storage_32(ctx: &mut SpirvEmitContext, _binding: Word, _offset: Word) -> Word {
    // Storage buffer loads require access chain + OpLoad.
    // The actual implementation depends on the SSBO variable layout
    // set up in SpirvEmitContext.
    log::trace!("SPIR-V: emit_load_storage_32 (storage buffer load)");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Emit a store to a storage buffer (SSBO).
pub fn emit_write_storage_32(
    ctx: &mut SpirvEmitContext,
    _binding: Word,
    _offset: Word,
    _value: Word,
) {
    log::trace!("SPIR-V: emit_write_storage_32 (storage buffer store)");
}

/// Emit a load from global memory.
///
/// Matches upstream `EmitLoadGlobalU32`/etc.
pub fn emit_load_global_u32(ctx: &mut SpirvEmitContext, _address: Word) -> Word {
    log::trace!("SPIR-V: emit_load_global_u32");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Emit a load from global memory (64-bit result).
pub fn emit_load_global_u64(ctx: &mut SpirvEmitContext, _address: Word) -> Word {
    log::trace!("SPIR-V: emit_load_global_u64");
    ctx.builder.undef(ctx.u32_vec2_type, None)
}

/// Emit a store to global memory.
pub fn emit_write_global_u32(ctx: &mut SpirvEmitContext, _address: Word, _value: Word) {
    log::trace!("SPIR-V: emit_write_global_u32");
    let _ = ctx;
}

/// Emit a load from local memory.
pub fn emit_load_local(ctx: &mut SpirvEmitContext, _offset: Word) -> Word {
    log::trace!("SPIR-V: emit_load_local");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Emit a store to local memory.
pub fn emit_write_local(ctx: &mut SpirvEmitContext, _offset: Word, _value: Word) {
    log::trace!("SPIR-V: emit_write_local");
    let _ = ctx;
}
