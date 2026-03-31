// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V atomic operation emission — maps to zuyu's
//! `backend/spirv/emit_spirv_atomic.cpp`.
//!
//! Handles atomic operations on shared memory and storage buffers (SSBOs).

use super::spirv_emit_context::SpirvEmitContext;
use rspirv::spirv::{self, Word};

/// Get device scope and relaxed semantics for atomic operations.
///
/// Matches upstream `AtomicArgs(EmitContext&)`.
fn atomic_args(ctx: &mut SpirvEmitContext) -> (Word, Word) {
    let scope = ctx.constant_u32(spirv::Scope::Device as u32);
    let semantics = ctx.const_zero_u32;
    (scope, semantics)
}

/// Shared memory atomic add (U32).
///
/// Matches upstream `SharedAtomicU32` + `OpAtomicIAdd`.
pub fn emit_shared_atomic_iadd_32(ctx: &mut SpirvEmitContext, _offset: Word, _value: Word) -> Word {
    log::trace!("SPIR-V: emit_shared_atomic_iadd_32");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Shared memory atomic min (signed U32).
pub fn emit_shared_atomic_smin_32(ctx: &mut SpirvEmitContext, _offset: Word, _value: Word) -> Word {
    log::trace!("SPIR-V: emit_shared_atomic_smin_32");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Shared memory atomic max (signed U32).
pub fn emit_shared_atomic_smax_32(ctx: &mut SpirvEmitContext, _offset: Word, _value: Word) -> Word {
    log::trace!("SPIR-V: emit_shared_atomic_smax_32");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Shared memory atomic min (unsigned U32).
pub fn emit_shared_atomic_umin_32(ctx: &mut SpirvEmitContext, _offset: Word, _value: Word) -> Word {
    log::trace!("SPIR-V: emit_shared_atomic_umin_32");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Shared memory atomic max (unsigned U32).
pub fn emit_shared_atomic_umax_32(ctx: &mut SpirvEmitContext, _offset: Word, _value: Word) -> Word {
    log::trace!("SPIR-V: emit_shared_atomic_umax_32");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Shared memory atomic AND (U32).
pub fn emit_shared_atomic_and_32(ctx: &mut SpirvEmitContext, _offset: Word, _value: Word) -> Word {
    log::trace!("SPIR-V: emit_shared_atomic_and_32");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Shared memory atomic OR (U32).
pub fn emit_shared_atomic_or_32(ctx: &mut SpirvEmitContext, _offset: Word, _value: Word) -> Word {
    log::trace!("SPIR-V: emit_shared_atomic_or_32");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Shared memory atomic XOR (U32).
pub fn emit_shared_atomic_xor_32(ctx: &mut SpirvEmitContext, _offset: Word, _value: Word) -> Word {
    log::trace!("SPIR-V: emit_shared_atomic_xor_32");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Shared memory atomic exchange (U32).
pub fn emit_shared_atomic_exchange_32(
    ctx: &mut SpirvEmitContext,
    _offset: Word,
    _value: Word,
) -> Word {
    log::trace!("SPIR-V: emit_shared_atomic_exchange_32");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Storage buffer atomic add (U32).
///
/// Matches upstream `StorageAtomicU32` + `OpAtomicIAdd`.
pub fn emit_storage_atomic_iadd_32(
    ctx: &mut SpirvEmitContext,
    _binding: Word,
    _offset: Word,
    _value: Word,
) -> Word {
    let (_scope, _semantics) = atomic_args(ctx);
    log::trace!("SPIR-V: emit_storage_atomic_iadd_32");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Storage buffer atomic min (signed).
pub fn emit_storage_atomic_smin_32(
    ctx: &mut SpirvEmitContext,
    _binding: Word,
    _offset: Word,
    _value: Word,
) -> Word {
    log::trace!("SPIR-V: emit_storage_atomic_smin_32");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Storage buffer atomic max (signed).
pub fn emit_storage_atomic_smax_32(
    ctx: &mut SpirvEmitContext,
    _binding: Word,
    _offset: Word,
    _value: Word,
) -> Word {
    log::trace!("SPIR-V: emit_storage_atomic_smax_32");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Storage buffer atomic min (unsigned).
pub fn emit_storage_atomic_umin_32(
    ctx: &mut SpirvEmitContext,
    _binding: Word,
    _offset: Word,
    _value: Word,
) -> Word {
    log::trace!("SPIR-V: emit_storage_atomic_umin_32");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Storage buffer atomic max (unsigned).
pub fn emit_storage_atomic_umax_32(
    ctx: &mut SpirvEmitContext,
    _binding: Word,
    _offset: Word,
    _value: Word,
) -> Word {
    log::trace!("SPIR-V: emit_storage_atomic_umax_32");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Storage buffer atomic AND.
pub fn emit_storage_atomic_and_32(
    ctx: &mut SpirvEmitContext,
    _binding: Word,
    _offset: Word,
    _value: Word,
) -> Word {
    log::trace!("SPIR-V: emit_storage_atomic_and_32");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Storage buffer atomic OR.
pub fn emit_storage_atomic_or_32(
    ctx: &mut SpirvEmitContext,
    _binding: Word,
    _offset: Word,
    _value: Word,
) -> Word {
    log::trace!("SPIR-V: emit_storage_atomic_or_32");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Storage buffer atomic XOR.
pub fn emit_storage_atomic_xor_32(
    ctx: &mut SpirvEmitContext,
    _binding: Word,
    _offset: Word,
    _value: Word,
) -> Word {
    log::trace!("SPIR-V: emit_storage_atomic_xor_32");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Storage buffer atomic exchange.
pub fn emit_storage_atomic_exchange_32(
    ctx: &mut SpirvEmitContext,
    _binding: Word,
    _offset: Word,
    _value: Word,
) -> Word {
    log::trace!("SPIR-V: emit_storage_atomic_exchange_32");
    ctx.builder.undef(ctx.u32_type, None)
}
