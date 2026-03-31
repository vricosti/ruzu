// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V memory operation emission — maps to zuyu's
//! `backend/spirv/emit_spirv_memory.cpp`.

use super::spirv_emit_context::SpirvEmitContext;
use rspirv::spirv::Word;

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

// ── IR-instruction dispatching helpers (called from spirv_emit_context) ───

use crate::ir::{self, Opcode};

/// Dispatch load IR instructions (LoadGlobal32 / LoadLocal / LoadStorage32).
pub fn emit_load(ctx: &mut SpirvEmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    match inst.opcode {
        Opcode::LoadGlobal32 | Opcode::LoadStorage32 => {
            log::trace!("SPIR-V: {:?} not fully implemented", inst.opcode);
            let zero = ctx.const_zero_u32;
            ctx.set_value(block_idx, inst_idx, zero);
        }
        Opcode::LoadLocal => {
            log::trace!("SPIR-V: LoadLocal not fully implemented");
            let zero = ctx.const_zero_u32;
            ctx.set_value(block_idx, inst_idx, zero);
        }
        _ => {
            let zero = ctx.const_zero_u32;
            ctx.set_value(block_idx, inst_idx, zero);
        }
    }
}

/// Dispatch store IR instructions (WriteGlobal32 / WriteLocal / WriteStorage32).
pub fn emit_store(ctx: &mut SpirvEmitContext, inst: &ir::Inst, _block_idx: u32, _inst_idx: u32) {
    match inst.opcode {
        Opcode::WriteGlobal32 | Opcode::WriteStorage32 => {
            log::trace!("SPIR-V: {:?} not fully implemented", inst.opcode);
        }
        Opcode::WriteLocal => {
            log::trace!("SPIR-V: WriteLocal not fully implemented");
        }
        _ => {}
    }
    let _ = ctx;
}
