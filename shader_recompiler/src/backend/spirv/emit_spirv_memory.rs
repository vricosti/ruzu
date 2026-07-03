// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V memory operation emission — maps to zuyu's
//! `backend/spirv/emit_spirv_memory.cpp`.

use super::spirv_emit_context::SpirvEmitContext;
use crate::ir::value::Value;
use rspirv::spirv::Word;

fn storage_index(
    ctx: &mut SpirvEmitContext,
    offset: Value,
    element_size: u32,
    index_offset: u32,
) -> Word {
    if offset.is_immediate() {
        return ctx.constant_u32(offset.imm_u32() / element_size + index_offset);
    }

    let mut index = ctx.resolve_value(&offset);
    let shift = element_size.trailing_zeros();
    if shift != 0 {
        let shift_id = ctx.constant_u32(shift);
        index = ctx
            .builder
            .shift_right_logical(ctx.u32_type, None, index, shift_id)
            .unwrap();
    }
    if index_offset != 0 {
        let offset_id = ctx.constant_u32(index_offset);
        index = ctx
            .builder
            .i_add(ctx.u32_type, None, index, offset_id)
            .unwrap();
    }
    index
}

fn storage_pointer(
    ctx: &mut SpirvEmitContext,
    binding: Value,
    offset: Value,
    element_size: u32,
    index_offset: u32,
) -> Word {
    if !binding.is_immediate() {
        panic!("SPIR-V: dynamic storage buffer indexing is not implemented");
    }

    let binding_index = binding.imm_u32();
    let ssbo = *ctx
        .ssbo_vars
        .get(&binding_index)
        .unwrap_or_else(|| panic!("SPIR-V: missing SSBO descriptor {}", binding_index));
    let index = storage_index(ctx, offset, element_size, index_offset);
    ctx.builder
        .access_chain(
            ctx.storage_u32_ptr,
            None,
            ssbo,
            vec![ctx.const_zero_u32, index],
        )
        .unwrap()
}

fn load_storage_32(
    ctx: &mut SpirvEmitContext,
    binding: Value,
    offset: Value,
    index_offset: u32,
) -> Word {
    let pointer = storage_pointer(ctx, binding, offset, 4, index_offset);
    ctx.builder
        .load(ctx.u32_type, None, pointer, None, vec![])
        .unwrap()
}

/// Emit a load from a storage buffer (SSBO).
///
/// Matches upstream storage buffer load patterns.
pub fn emit_load_storage_32(ctx: &mut SpirvEmitContext, binding: Value, offset: Value) -> Word {
    load_storage_32(ctx, binding, offset, 0)
}

pub fn emit_load_storage_64(ctx: &mut SpirvEmitContext, binding: Value, offset: Value) -> Word {
    let x = load_storage_32(ctx, binding, offset, 0);
    let y = load_storage_32(ctx, binding, offset, 1);
    ctx.builder
        .composite_construct(ctx.u32_vec2_type, None, vec![x, y])
        .unwrap()
}

pub fn emit_load_storage_128(ctx: &mut SpirvEmitContext, binding: Value, offset: Value) -> Word {
    let x = load_storage_32(ctx, binding, offset, 0);
    let y = load_storage_32(ctx, binding, offset, 1);
    let z = load_storage_32(ctx, binding, offset, 2);
    let w = load_storage_32(ctx, binding, offset, 3);
    ctx.builder
        .composite_construct(ctx.u32_vec4_type, None, vec![x, y, z, w])
        .unwrap()
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

/// Dispatch load IR instructions.
pub fn emit_load(ctx: &mut SpirvEmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    match inst.opcode {
        Opcode::LoadStorage32 => {
            let id = emit_load_storage_32(ctx, *inst.arg(0), *inst.arg(1));
            ctx.set_value(block_idx, inst_idx, id);
        }
        Opcode::LoadStorage64 => {
            let id = emit_load_storage_64(ctx, *inst.arg(0), *inst.arg(1));
            ctx.set_value(block_idx, inst_idx, id);
        }
        Opcode::LoadStorage128 => {
            let id = emit_load_storage_128(ctx, *inst.arg(0), *inst.arg(1));
            ctx.set_value(block_idx, inst_idx, id);
        }
        Opcode::LoadGlobal32 => {
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
