// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V emission for context access: constant buffers, attributes, and outputs.

use super::spirv_context::EmitContext;
use crate::ir::{self, Opcode};

pub fn emit_get_cbuf(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    // GetCbufU32(cbuf_index, byte_offset) or GetCbufF32(cbuf_index, byte_offset)
    let cbuf_idx = inst.arg(0).imm_u32();
    let byte_offset = ctx.resolve_value(inst.arg(1));

    let result_type = match inst.opcode {
        Opcode::GetCbufF32 => ctx.f32_type,
        _ => ctx.u32_type,
    };

    if let Some(&cbuf_var) = ctx.cbuf_vars.get(&cbuf_idx) {
        // Convert byte offset to u32 index: offset / 4
        let four = ctx.builder.constant_bit32(ctx.u32_type, 4);
        let index = ctx
            .builder
            .u_div(ctx.u32_type, None, byte_offset, four)
            .unwrap();

        // Access chain: cbuf_var -> member 0 (the array) -> index
        let zero = ctx.const_zero_u32;
        let ptr = ctx
            .builder
            .access_chain(ctx.uniform_u32_ptr, None, cbuf_var, vec![zero, index])
            .unwrap();
        let loaded = ctx
            .builder
            .load(ctx.u32_type, None, ptr, None, vec![])
            .unwrap();

        let id = if inst.opcode == Opcode::GetCbufF32 {
            ctx.builder.bitcast(ctx.f32_type, None, loaded).unwrap()
        } else {
            loaded
        };

        ctx.set_value(block_idx, inst_idx, id);
    } else {
        // CB not bound — return zero
        let id = if inst.opcode == Opcode::GetCbufF32 {
            ctx.const_zero_f32
        } else {
            ctx.const_zero_u32
        };
        ctx.set_value(block_idx, inst_idx, id);
    }
}

pub fn emit_get_attribute(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    // GetAttribute(attribute_id)
    let attr = inst.arg(0).attribute();

    if attr.is_position() {
        // Load from gl_Position or equivalent input builtin
        let comp = attr.position_element();
        if let Some(&pos_var) = ctx.output_vars.get(&0xFFFF_0000) {
            // For vertex shader reading back position — uncommon but handle it
            let idx_const = ctx.builder.constant_bit32(ctx.u32_type, comp);
            let ptr = ctx
                .builder
                .access_chain(ctx.output_f32_ptr, None, pos_var, vec![idx_const])
                .unwrap();
            let id = ctx
                .builder
                .load(ctx.f32_type, None, ptr, None, vec![])
                .unwrap();
            ctx.set_value(block_idx, inst_idx, id);
        } else {
            ctx.set_value(block_idx, inst_idx, ctx.const_zero_f32);
        }
    } else if attr.is_generic() {
        let generic_idx = attr.generic_index();
        let comp = attr.generic_element();
        if let Some(&input_var) = ctx.input_vars.get(&generic_idx) {
            let idx_const = ctx.builder.constant_bit32(ctx.u32_type, comp);
            let ptr = ctx
                .builder
                .access_chain(ctx.input_f32_ptr, None, input_var, vec![idx_const])
                .unwrap();
            let id = ctx
                .builder
                .load(ctx.f32_type, None, ptr, None, vec![])
                .unwrap();
            ctx.set_value(block_idx, inst_idx, id);
        } else {
            ctx.set_value(block_idx, inst_idx, ctx.const_zero_f32);
        }
    } else {
        // Unhandled attribute — return zero
        log::trace!("SPIR-V: unhandled attribute {:?}", attr);
        ctx.set_value(block_idx, inst_idx, ctx.const_zero_f32);
    }
}

pub fn emit_set_attribute(ctx: &mut EmitContext, inst: &ir::Inst, _block_idx: u32, _inst_idx: u32) {
    // SetAttribute(attribute_id, value)
    let attr = inst.arg(0).attribute();
    let val = ctx.resolve_value(inst.arg(1));

    if attr.is_position() {
        let comp = attr.position_element();
        if let Some(&pos_var) = ctx.output_vars.get(&0xFFFF_0000) {
            let idx_const = ctx.builder.constant_bit32(ctx.u32_type, comp);
            let ptr = ctx
                .builder
                .access_chain(ctx.output_f32_ptr, None, pos_var, vec![idx_const])
                .unwrap();
            ctx.builder.store(ptr, val, None, vec![]).unwrap();
        }
    } else if attr.is_generic() {
        let generic_idx = attr.generic_index();
        let comp = attr.generic_element();
        if let Some(&output_var) = ctx.output_vars.get(&generic_idx) {
            let idx_const = ctx.builder.constant_bit32(ctx.u32_type, comp);
            let ptr = ctx
                .builder
                .access_chain(ctx.output_f32_ptr, None, output_var, vec![idx_const])
                .unwrap();
            ctx.builder.store(ptr, val, None, vec![]).unwrap();
        }
    } else {
        log::trace!("SPIR-V: unhandled set_attribute {:?}", attr);
    }
}

pub fn emit_set_frag_color(
    ctx: &mut EmitContext,
    inst: &ir::Inst,
    _block_idx: u32,
    _inst_idx: u32,
) {
    // SetFragColor(render_target, component, value)
    let rt = inst.arg(0).imm_u32();
    let comp = inst.arg(1).imm_u32();
    let val = ctx.resolve_value(inst.arg(2));

    if let Some(&output_var) = ctx.output_vars.get(&rt) {
        let idx_const = ctx.builder.constant_bit32(ctx.u32_type, comp);
        let ptr = ctx
            .builder
            .access_chain(ctx.output_f32_ptr, None, output_var, vec![idx_const])
            .unwrap();
        ctx.builder.store(ptr, val, None, vec![]).unwrap();
    }
}

pub fn emit_set_frag_depth(
    ctx: &mut EmitContext,
    inst: &ir::Inst,
    _block_idx: u32,
    _inst_idx: u32,
) {
    // SetFragDepth(value)
    // Requires a depth output builtin variable — would need to be created in define_global_variables
    let _val = ctx.resolve_value(inst.arg(0));
    log::trace!("SPIR-V: SetFragDepth not fully implemented (need depth output builtin)");
}
