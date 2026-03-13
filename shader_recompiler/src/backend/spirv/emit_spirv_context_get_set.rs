// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V context get/set emission — maps to zuyu's
//! `backend/spirv/emit_spirv_context_get_set.cpp`.
//!
//! Handles constant buffer loads, attribute loads/stores, and other
//! context-related operations.

use rspirv::spirv::Word;
use super::spirv_emit_context::SpirvEmitContext;
use crate::ir::{self, Opcode};

// ── Low-level per-type emit functions (called from other modules) ─────────

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

// ── IR-instruction dispatching helpers (called from spirv_emit_context) ───

/// Dispatch GetCbufU32 / GetCbufF32 IR instructions.
pub fn emit_get_cbuf(
    ctx: &mut SpirvEmitContext,
    inst: &ir::Inst,
    block_idx: u32,
    inst_idx: u32,
) {
    let cbuf_idx = inst.arg(0).imm_u32();
    let byte_offset = ctx.resolve_value(inst.arg(1));

    let result_type = match inst.opcode {
        Opcode::GetCbufF32 => ctx.f32_type,
        _ => ctx.u32_type,
    };

    if let Some(&cbuf_var) = ctx.cbuf_vars.get(&cbuf_idx) {
        let four = ctx.builder.constant_bit32(ctx.u32_type, 4);
        let index = ctx.builder.u_div(ctx.u32_type, None, byte_offset, four).unwrap();
        let zero = ctx.const_zero_u32;
        let uniform_u32_ptr = ctx.uniform_u32_ptr;
        let ptr = ctx
            .builder
            .access_chain(uniform_u32_ptr, None, cbuf_var, vec![zero, index])
            .unwrap();
        let loaded = ctx.builder.load(ctx.u32_type, None, ptr, None, vec![]).unwrap();

        let id = if inst.opcode == Opcode::GetCbufF32 {
            ctx.builder.bitcast(ctx.f32_type, None, loaded).unwrap()
        } else {
            loaded
        };

        ctx.set_value(block_idx, inst_idx, id);
    } else {
        let id = if inst.opcode == Opcode::GetCbufF32 {
            ctx.const_zero_f32
        } else {
            ctx.const_zero_u32
        };
        ctx.set_value(block_idx, inst_idx, id);
    }
}

/// Dispatch GetAttribute / GetAttributeU32 IR instructions.
pub fn emit_get_attribute_inst(
    ctx: &mut SpirvEmitContext,
    inst: &ir::Inst,
    block_idx: u32,
    inst_idx: u32,
) {
    let attr = inst.arg(0).attribute();

    if attr.is_position() {
        let comp = attr.position_element();
        if let Some(&pos_var) = ctx.output_vars.get(&0xFFFF_0000) {
            let idx_const = ctx.builder.constant_bit32(ctx.u32_type, comp);
            let output_f32_ptr = ctx.output_f32_ptr;
            let ptr = ctx
                .builder
                .access_chain(output_f32_ptr, None, pos_var, vec![idx_const])
                .unwrap();
            let id = ctx.builder.load(ctx.f32_type, None, ptr, None, vec![]).unwrap();
            ctx.set_value(block_idx, inst_idx, id);
        } else {
            let zero = ctx.const_zero_f32;
            ctx.set_value(block_idx, inst_idx, zero);
        }
    } else if attr.is_generic() {
        let generic_idx = attr.generic_index();
        let comp = attr.generic_element();
        if let Some(&input_var) = ctx.input_vars.get(&generic_idx) {
            let idx_const = ctx.builder.constant_bit32(ctx.u32_type, comp);
            let input_f32_ptr = ctx.input_f32_ptr;
            let ptr = ctx
                .builder
                .access_chain(input_f32_ptr, None, input_var, vec![idx_const])
                .unwrap();
            let id = ctx.builder.load(ctx.f32_type, None, ptr, None, vec![]).unwrap();
            ctx.set_value(block_idx, inst_idx, id);
        } else {
            let zero = ctx.const_zero_f32;
            ctx.set_value(block_idx, inst_idx, zero);
        }
    } else {
        log::trace!("SPIR-V: unhandled attribute {:?}", attr);
        let zero = ctx.const_zero_f32;
        ctx.set_value(block_idx, inst_idx, zero);
    }
}

/// Dispatch SetAttribute IR instructions.
pub fn emit_set_attribute_inst(
    ctx: &mut SpirvEmitContext,
    inst: &ir::Inst,
    _block_idx: u32,
    _inst_idx: u32,
) {
    let attr = inst.arg(0).attribute();
    let val = ctx.resolve_value(inst.arg(1));

    if attr.is_position() {
        let comp = attr.position_element();
        if let Some(&pos_var) = ctx.output_vars.get(&0xFFFF_0000) {
            let idx_const = ctx.builder.constant_bit32(ctx.u32_type, comp);
            let output_f32_ptr = ctx.output_f32_ptr;
            let ptr = ctx
                .builder
                .access_chain(output_f32_ptr, None, pos_var, vec![idx_const])
                .unwrap();
            ctx.builder.store(ptr, val, None, vec![]).unwrap();
        }
    } else if attr.is_generic() {
        let generic_idx = attr.generic_index();
        let comp = attr.generic_element();
        if let Some(&output_var) = ctx.output_vars.get(&generic_idx) {
            let idx_const = ctx.builder.constant_bit32(ctx.u32_type, comp);
            let output_f32_ptr = ctx.output_f32_ptr;
            let ptr = ctx
                .builder
                .access_chain(output_f32_ptr, None, output_var, vec![idx_const])
                .unwrap();
            ctx.builder.store(ptr, val, None, vec![]).unwrap();
        }
    } else {
        log::trace!("SPIR-V: unhandled set_attribute {:?}", attr);
    }
}

/// Dispatch SetFragColor IR instructions.
pub fn emit_set_frag_color_inst(
    ctx: &mut SpirvEmitContext,
    inst: &ir::Inst,
    _block_idx: u32,
    _inst_idx: u32,
) {
    let rt = inst.arg(0).imm_u32();
    let comp = inst.arg(1).imm_u32();
    let val = ctx.resolve_value(inst.arg(2));

    if let Some(&output_var) = ctx.output_vars.get(&rt) {
        let idx_const = ctx.builder.constant_bit32(ctx.u32_type, comp);
        let output_f32_ptr = ctx.output_f32_ptr;
        let ptr = ctx
            .builder
            .access_chain(output_f32_ptr, None, output_var, vec![idx_const])
            .unwrap();
        ctx.builder.store(ptr, val, None, vec![]).unwrap();
    }
}

/// Dispatch SetFragDepth IR instructions.
pub fn emit_set_frag_depth_inst(
    ctx: &mut SpirvEmitContext,
    inst: &ir::Inst,
    _block_idx: u32,
    _inst_idx: u32,
) {
    let _val = ctx.resolve_value(inst.arg(0));
    log::trace!("SPIR-V: SetFragDepth not fully implemented (need depth output builtin)");
}
