// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V context get/set emission — maps to zuyu's
//! `backend/spirv/emit_spirv_context_get_set.cpp`.
//!
//! Handles constant buffer loads, attribute loads/stores, and other
//! context-related operations.

use super::spirv_emit_context::SpirvEmitContext;
use crate::ir::types::ShaderStage;
use crate::ir::{self, Opcode};
use crate::runtime_info::AttributeType;
use rspirv::spirv::Word;

// ── Low-level per-type emit functions (called from other modules) ─────────

/// Emit a constant buffer load (U32).
///
/// Matches upstream `EmitGetCbufU32(EmitContext&, Id, Id)`.
pub fn emit_get_cbuf_u32(ctx: &mut SpirvEmitContext, _binding: Word, _offset: Word) -> Word {
    log::trace!("SPIR-V: emit_get_cbuf_u32");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Emit a constant buffer load (F32).
///
/// Matches upstream `EmitGetCbufF32(EmitContext&, Id, Id)`.
pub fn emit_get_cbuf_f32(ctx: &mut SpirvEmitContext, _binding: Word, _offset: Word) -> Word {
    log::trace!("SPIR-V: emit_get_cbuf_f32");
    ctx.builder.undef(ctx.f32_type, None)
}

/// Emit a constant buffer load (S32).
pub fn emit_get_cbuf_s32(ctx: &mut SpirvEmitContext, _binding: Word, _offset: Word) -> Word {
    log::trace!("SPIR-V: emit_get_cbuf_s32");
    ctx.builder.undef(ctx.i32_type, None)
}

/// Emit a constant buffer load (U16).
pub fn emit_get_cbuf_u16(ctx: &mut SpirvEmitContext, _binding: Word, _offset: Word) -> Word {
    log::trace!("SPIR-V: emit_get_cbuf_u16");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Emit a constant buffer load (S16).
pub fn emit_get_cbuf_s16(ctx: &mut SpirvEmitContext, _binding: Word, _offset: Word) -> Word {
    log::trace!("SPIR-V: emit_get_cbuf_s16");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Emit a constant buffer load (U8).
pub fn emit_get_cbuf_u8(ctx: &mut SpirvEmitContext, _binding: Word, _offset: Word) -> Word {
    log::trace!("SPIR-V: emit_get_cbuf_u8");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Emit a constant buffer load (S8).
pub fn emit_get_cbuf_s8(ctx: &mut SpirvEmitContext, _binding: Word, _offset: Word) -> Word {
    log::trace!("SPIR-V: emit_get_cbuf_s8");
    ctx.builder.undef(ctx.u32_type, None)
}

/// Emit a load of a shader input attribute.
///
/// Matches upstream `EmitGetAttribute(EmitContext&, Id, u32)`.
pub fn emit_get_attribute(ctx: &mut SpirvEmitContext, _attribute: Word) -> Word {
    log::trace!("SPIR-V: emit_get_attribute");
    ctx.builder.undef(ctx.f32_type, None)
}

/// Emit a store to a shader output attribute.
///
/// Matches upstream `EmitSetAttribute(EmitContext&, Id, Id, u32)`.
pub fn emit_set_attribute(_ctx: &mut SpirvEmitContext, _attribute: Word, _value: Word) {
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
pub fn emit_set_frag_depth(ctx: &mut SpirvEmitContext, value: Word) {
    let value = if ctx.runtime_info.convert_depth_mode && !ctx.profile.support_native_ndc {
        let half = ctx.constant_f32(0.5);
        ctx.builder
            .ext_inst(
                ctx.f32_type,
                None,
                ctx.glsl_ext,
                50, /* Fma */
                vec![
                    rspirv::dr::Operand::IdRef(value),
                    rspirv::dr::Operand::IdRef(half),
                    rspirv::dr::Operand::IdRef(half),
                ],
            )
            .unwrap()
    } else {
        value
    };
    ctx.builder
        .store(ctx.frag_depth, value, None, vec![])
        .unwrap();
}

/// Emit SetSampleMask.
pub fn emit_set_sample_mask(_ctx: &mut SpirvEmitContext, _value: Word) {
    log::trace!("SPIR-V: emit_set_sample_mask");
}

// ── IR-instruction dispatching helpers (called from spirv_emit_context) ───

fn cbuf_element_index(
    ctx: &mut SpirvEmitContext,
    offset: ir::Value,
    resolved_offset: Word,
    element_size: u32,
) -> Word {
    if let ir::Value::ImmU32(offset) = offset {
        return ctx.constant_u32(offset / element_size);
    }
    let shift = ctx.constant_u32(element_size.trailing_zeros());
    ctx.builder
        .shift_right_logical(ctx.u32_type, None, resolved_offset, shift)
        .unwrap()
}

fn load_cbuf_view(
    ctx: &mut SpirvEmitContext,
    var: Word,
    result_type: Word,
    pointer_type: Word,
    offset: ir::Value,
    resolved_offset: Word,
    element_size: u32,
) -> Word {
    let index = cbuf_element_index(ctx, offset, resolved_offset, element_size);
    let zero = ctx.const_zero_u32;
    let pointer = ctx
        .builder
        .access_chain(pointer_type, None, var, vec![zero, index])
        .unwrap();
    ctx.builder
        .load(result_type, None, pointer, None, vec![])
        .unwrap()
}

fn load_cbuf_u32x4_element(
    ctx: &mut SpirvEmitContext,
    var: Word,
    offset: ir::Value,
    resolved_offset: Word,
    index_offset: u32,
) -> Word {
    let pointer_type = ctx.uniform_u32_vec4_ptr;
    let vector_type = ctx.u32_vec4_type;
    let vector = load_cbuf_view(
        ctx,
        var,
        vector_type,
        pointer_type,
        offset,
        resolved_offset,
        16,
    );
    if let ir::Value::ImmU32(offset) = offset {
        return ctx
            .builder
            .composite_extract(
                ctx.u32_type,
                None,
                vector,
                vec![(offset / 4) % 4 + index_offset],
            )
            .unwrap();
    }
    let two = ctx.constant_u32(2);
    let word = ctx
        .builder
        .shift_right_logical(ctx.u32_type, None, resolved_offset, two)
        .unwrap();
    let three = ctx.constant_u32(3);
    let mut component = ctx
        .builder
        .bitwise_and(ctx.u32_type, None, word, three)
        .unwrap();
    if index_offset != 0 {
        let offset = ctx.constant_u32(index_offset);
        component = ctx
            .builder
            .i_add(ctx.u32_type, None, component, offset)
            .unwrap();
    }
    ctx.builder
        .vector_extract_dynamic(ctx.u32_type, None, vector, component)
        .unwrap()
}

/// Dispatch constant-buffer load IR instructions.
pub fn emit_get_cbuf(ctx: &mut SpirvEmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    let cbuf_idx = inst.arg(0).imm_u32();
    let offset = *inst.arg(1);
    let resolved_offset = ctx.resolve_value(&offset);
    let definitions = ctx.cbufs.get(&cbuf_idx).copied().unwrap_or_default();

    let id = match inst.opcode {
        Opcode::GetCbufF32 if ctx.profile.support_descriptor_aliasing => {
            if definitions.f32_scalar == 0 {
                ctx.const_zero_f32
            } else {
                load_cbuf_view(
                    ctx,
                    definitions.f32_scalar,
                    ctx.f32_type,
                    ctx.uniform_f32_ptr,
                    offset,
                    resolved_offset,
                    4,
                )
            }
        }
        Opcode::GetCbufU32 if ctx.profile.support_descriptor_aliasing => {
            if definitions.u32_scalar == 0 {
                ctx.const_zero_u32
            } else {
                load_cbuf_view(
                    ctx,
                    definitions.u32_scalar,
                    ctx.u32_type,
                    ctx.uniform_u32_ptr,
                    offset,
                    resolved_offset,
                    4,
                )
            }
        }
        Opcode::GetCbufU32x2 if ctx.profile.support_descriptor_aliasing => {
            if definitions.u32x2 == 0 {
                ctx.builder.undef(ctx.u32_vec2_type, None)
            } else {
                load_cbuf_view(
                    ctx,
                    definitions.u32x2,
                    ctx.u32_vec2_type,
                    ctx.uniform_u32_vec2_ptr,
                    offset,
                    resolved_offset,
                    8,
                )
            }
        }
        Opcode::GetCbufU8 | Opcode::GetCbufS8 | Opcode::GetCbufU16 | Opcode::GetCbufS16
            if ctx.profile.support_descriptor_aliasing =>
        {
            let word = if definitions.u32_scalar == 0 {
                ctx.const_zero_u32
            } else {
                load_cbuf_view(
                    ctx,
                    definitions.u32_scalar,
                    ctx.u32_type,
                    ctx.uniform_u32_ptr,
                    offset,
                    resolved_offset,
                    4,
                )
            };
            let (mask, width, signed) = match inst.opcode {
                Opcode::GetCbufU8 => (3, 8, false),
                Opcode::GetCbufS8 => (3, 8, true),
                Opcode::GetCbufU16 => (2, 16, false),
                Opcode::GetCbufS16 => (2, 16, true),
                _ => unreachable!(),
            };
            let bit_offset = if let ir::Value::ImmU32(offset) = offset {
                ctx.constant_u32((offset & mask) * 8)
            } else {
                let mask = ctx.constant_u32(mask);
                let byte = ctx
                    .builder
                    .bitwise_and(ctx.u32_type, None, resolved_offset, mask)
                    .unwrap();
                let three = ctx.constant_u32(3);
                ctx.builder
                    .shift_left_logical(ctx.u32_type, None, byte, three)
                    .unwrap()
            };
            let width = ctx.constant_u32(width);
            if signed {
                ctx.builder
                    .bit_field_s_extract(ctx.u32_type, None, word, bit_offset, width)
                    .unwrap()
            } else {
                ctx.builder
                    .bit_field_u_extract(ctx.u32_type, None, word, bit_offset, width)
                    .unwrap()
            }
        }
        _ => {
            if definitions.u32x4 == 0 {
                if inst.opcode == Opcode::GetCbufF32 {
                    ctx.const_zero_f32
                } else if inst.opcode == Opcode::GetCbufU32x2 {
                    ctx.builder.undef(ctx.u32_vec2_type, None)
                } else {
                    ctx.const_zero_u32
                }
            } else if inst.opcode == Opcode::GetCbufU32x2 {
                let first =
                    load_cbuf_u32x4_element(ctx, definitions.u32x4, offset, resolved_offset, 0);
                let second =
                    load_cbuf_u32x4_element(ctx, definitions.u32x4, offset, resolved_offset, 1);
                ctx.builder
                    .composite_construct(ctx.u32_vec2_type, None, vec![first, second])
                    .unwrap()
            } else {
                let word =
                    load_cbuf_u32x4_element(ctx, definitions.u32x4, offset, resolved_offset, 0);
                if inst.opcode == Opcode::GetCbufF32 {
                    ctx.builder.bitcast(ctx.f32_type, None, word).unwrap()
                } else {
                    word
                }
            }
        }
    };
    ctx.set_value(block_idx, inst_idx, id);
}

/// Dispatch GetAttribute / GetAttributeU32 IR instructions.
pub fn emit_get_attribute_inst(
    ctx: &mut SpirvEmitContext,
    inst: &ir::Inst,
    block_idx: u32,
    inst_idx: u32,
) {
    let attr = inst.arg(0).attribute();
    let wants_u32 = inst.opcode == Opcode::GetAttributeU32;

    if wants_u32 {
        let id = emit_get_attribute_u32_value(ctx, attr);
        ctx.set_value(block_idx, inst_idx, id);
    } else if attr.is_position() {
        let comp = attr.position_element();
        if ctx.input_position != 0 {
            let idx_const = ctx.builder.constant_bit32(ctx.u32_type, comp);
            let input_f32_ptr = ctx.input_f32_ptr;
            let ptr = ctx
                .builder
                .access_chain(input_f32_ptr, None, ctx.input_position, vec![idx_const])
                .unwrap();
            let id = ctx
                .builder
                .load(ctx.f32_type, None, ptr, None, vec![])
                .unwrap();
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
            let id = load_generic_input_attribute(ctx, generic_idx, input_var, idx_const);
            ctx.set_value(block_idx, inst_idx, id);
        } else {
            let default_value = if comp == 3 {
                ctx.const_one_f32
            } else {
                ctx.const_zero_f32
            };
            ctx.set_value(block_idx, inst_idx, default_value);
        }
    } else {
        let id = emit_get_attribute_f32_value(ctx, attr);
        ctx.set_value(block_idx, inst_idx, id);
    }
}

fn load_generic_input_attribute(
    ctx: &mut SpirvEmitContext,
    generic_idx: u32,
    input_var: Word,
    idx_const: Word,
) -> Word {
    match ctx.runtime_info.generic_input_types[generic_idx as usize] {
        AttributeType::Float => {
            let ptr = ctx
                .builder
                .access_chain(ctx.input_f32_ptr, None, input_var, vec![idx_const])
                .unwrap();
            ctx.builder
                .load(ctx.f32_type, None, ptr, None, vec![])
                .unwrap()
        }
        AttributeType::UnsignedInt => {
            let ptr = ctx
                .builder
                .access_chain(ctx.input_u32_ptr, None, input_var, vec![idx_const])
                .unwrap();
            let value = ctx
                .builder
                .load(ctx.u32_type, None, ptr, None, vec![])
                .unwrap();
            ctx.builder.bitcast(ctx.f32_type, None, value).unwrap()
        }
        AttributeType::SignedInt => {
            let ptr = ctx
                .builder
                .access_chain(ctx.input_i32_ptr, None, input_var, vec![idx_const])
                .unwrap();
            let value = ctx
                .builder
                .load(ctx.i32_type, None, ptr, None, vec![])
                .unwrap();
            ctx.builder.bitcast(ctx.f32_type, None, value).unwrap()
        }
        AttributeType::SignedScaled => {
            if ctx.profile.support_scaled_attributes {
                let ptr = ctx
                    .builder
                    .access_chain(ctx.input_f32_ptr, None, input_var, vec![idx_const])
                    .unwrap();
                ctx.builder
                    .load(ctx.f32_type, None, ptr, None, vec![])
                    .unwrap()
            } else {
                let ptr = ctx
                    .builder
                    .access_chain(ctx.input_i32_ptr, None, input_var, vec![idx_const])
                    .unwrap();
                let value = ctx
                    .builder
                    .load(ctx.i32_type, None, ptr, None, vec![])
                    .unwrap();
                ctx.builder
                    .convert_s_to_f(ctx.f32_type, None, value)
                    .unwrap()
            }
        }
        AttributeType::UnsignedScaled => {
            if ctx.profile.support_scaled_attributes {
                let ptr = ctx
                    .builder
                    .access_chain(ctx.input_f32_ptr, None, input_var, vec![idx_const])
                    .unwrap();
                ctx.builder
                    .load(ctx.f32_type, None, ptr, None, vec![])
                    .unwrap()
            } else {
                let ptr = ctx
                    .builder
                    .access_chain(ctx.input_u32_ptr, None, input_var, vec![idx_const])
                    .unwrap();
                let value = ctx
                    .builder
                    .load(ctx.u32_type, None, ptr, None, vec![])
                    .unwrap();
                ctx.builder
                    .convert_u_to_f(ctx.f32_type, None, value)
                    .unwrap()
            }
        }
        AttributeType::Disabled => ctx.const_zero_f32,
    }
}

fn emit_get_attribute_f32_value(
    ctx: &mut SpirvEmitContext,
    attr: crate::ir::value::Attribute,
) -> Word {
    use crate::ir::value::Attribute;

    match attr {
        Attribute::PRIMITIVE_ID => bitcast_u32_builtin_to_f32(ctx, ctx.primitive_id),
        Attribute::LAYER => bitcast_u32_builtin_to_f32(ctx, ctx.layer),
        Attribute::INSTANCE_ID => {
            if ctx.profile.support_vertex_instance_id {
                bitcast_u32_builtin_to_f32(ctx, ctx.instance_id)
            } else {
                let instance = load_u32_builtin(ctx, ctx.instance_index);
                let base = load_u32_builtin(ctx, ctx.base_instance);
                let value = ctx
                    .builder
                    .i_sub(ctx.u32_type, None, instance, base)
                    .unwrap();
                ctx.builder.bitcast(ctx.f32_type, None, value).unwrap()
            }
        }
        Attribute::VERTEX_ID => {
            if ctx.profile.support_vertex_instance_id {
                bitcast_u32_builtin_to_f32(ctx, ctx.vertex_id)
            } else {
                bitcast_u32_builtin_to_f32(ctx, ctx.vertex_index)
            }
        }
        Attribute::BASE_INSTANCE => bitcast_u32_builtin_to_f32(ctx, ctx.base_instance),
        Attribute::BASE_VERTEX => bitcast_u32_builtin_to_f32(ctx, ctx.base_vertex),
        Attribute::DRAW_ID => bitcast_u32_builtin_to_f32(ctx, ctx.draw_index),
        Attribute::FRONT_FACE => {
            let front = ctx
                .builder
                .load(ctx.bool_type, None, ctx.front_face, None, vec![])
                .unwrap();
            let true_value = ctx.builder.constant_bit32(ctx.u32_type, u32::MAX);
            let true_value = ctx.builder.bitcast(ctx.f32_type, None, true_value).unwrap();
            ctx.builder
                .select(ctx.f32_type, None, front, true_value, ctx.const_zero_f32)
                .unwrap()
        }
        Attribute::POINT_SPRITE_S => load_f32_vec_component(ctx, ctx.point_coord, 0, 2),
        Attribute::POINT_SPRITE_T => load_f32_vec_component(ctx, ctx.point_coord, 1, 2),
        Attribute::TESSELLATION_EVALUATION_POINT_U => {
            load_f32_vec_component(ctx, ctx.tess_coord, 0, 3)
        }
        Attribute::TESSELLATION_EVALUATION_POINT_V => {
            load_f32_vec_component(ctx, ctx.tess_coord, 1, 3)
        }
        _ => {
            log::trace!("SPIR-V: unhandled attribute {:?}", attr);
            ctx.const_zero_f32
        }
    }
}

fn emit_get_attribute_u32_value(
    ctx: &mut SpirvEmitContext,
    attr: crate::ir::value::Attribute,
) -> Word {
    use crate::ir::value::Attribute;

    match attr {
        Attribute::PRIMITIVE_ID => load_u32_builtin(ctx, ctx.primitive_id),
        Attribute::INSTANCE_ID => {
            if ctx.profile.support_vertex_instance_id {
                load_u32_builtin(ctx, ctx.instance_id)
            } else {
                let instance = load_u32_builtin(ctx, ctx.instance_index);
                let base = load_u32_builtin(ctx, ctx.base_instance);
                ctx.builder
                    .i_sub(ctx.u32_type, None, instance, base)
                    .unwrap()
            }
        }
        Attribute::VERTEX_ID => {
            if ctx.profile.support_vertex_instance_id {
                load_u32_builtin(ctx, ctx.vertex_id)
            } else {
                load_u32_builtin(ctx, ctx.vertex_index)
            }
        }
        Attribute::BASE_INSTANCE => load_u32_builtin(ctx, ctx.base_instance),
        Attribute::BASE_VERTEX => load_u32_builtin(ctx, ctx.base_vertex),
        Attribute::DRAW_ID => load_u32_builtin(ctx, ctx.draw_index),
        _ => {
            log::trace!("SPIR-V: unhandled u32 attribute {:?}", attr);
            ctx.const_zero_u32
        }
    }
}

fn load_u32_builtin(ctx: &mut SpirvEmitContext, var: Word) -> Word {
    if var == 0 {
        return ctx.const_zero_u32;
    }
    ctx.builder
        .load(ctx.u32_type, None, var, None, vec![])
        .unwrap()
}

fn bitcast_u32_builtin_to_f32(ctx: &mut SpirvEmitContext, var: Word) -> Word {
    let value = load_u32_builtin(ctx, var);
    ctx.builder.bitcast(ctx.f32_type, None, value).unwrap()
}

fn load_f32_vec_component(
    ctx: &mut SpirvEmitContext,
    var: Word,
    component: u32,
    component_count: u32,
) -> Word {
    if var == 0 {
        return ctx.const_zero_f32;
    }
    let pointer_type =
        ctx.builder
            .type_pointer(None, rspirv::spirv::StorageClass::Input, ctx.f32_type);
    let index = ctx.builder.constant_bit32(ctx.u32_type, component);
    let ptr = ctx
        .builder
        .access_chain(pointer_type, None, var, vec![index])
        .unwrap();
    debug_assert!(component < component_count);
    ctx.builder
        .load(ctx.f32_type, None, ptr, None, vec![])
        .unwrap()
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
    } else if attr == ir::value::Attribute::POINT_SIZE {
        if ctx.output_point_size != 0 {
            ctx.builder
                .store(ctx.output_point_size, val, None, vec![])
                .unwrap();
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
        match ctx.runtime_info.frag_color_types[rt as usize] {
            AttributeType::UnsignedInt => {
                let ptr = ctx
                    .builder
                    .access_chain(ctx.output_u32_ptr, None, output_var, vec![idx_const])
                    .unwrap();
                let value = ctx.builder.bitcast(ctx.u32_type, None, val).unwrap();
                ctx.builder.store(ptr, value, None, vec![]).unwrap();
            }
            AttributeType::SignedInt => {
                let ptr = ctx
                    .builder
                    .access_chain(ctx.output_i32_ptr, None, output_var, vec![idx_const])
                    .unwrap();
                let value = ctx.builder.bitcast(ctx.i32_type, None, val).unwrap();
                ctx.builder.store(ptr, value, None, vec![]).unwrap();
            }
            _ => {
                let ptr = ctx
                    .builder
                    .access_chain(ctx.output_f32_ptr, None, output_var, vec![idx_const])
                    .unwrap();
                ctx.builder.store(ptr, val, None, vec![]).unwrap();
            }
        }
    }
}

/// Dispatch SetFragDepth IR instructions.
pub fn emit_set_frag_depth_inst(
    ctx: &mut SpirvEmitContext,
    inst: &ir::Inst,
    _block_idx: u32,
    _inst_idx: u32,
) {
    let value = ctx.resolve_value(inst.arg(0));
    emit_set_frag_depth(ctx, value);
}

// ── System value emission (matches upstream Emit* functions) ──────────────

/// Matches upstream `EmitWorkgroupId`.
pub fn emit_workgroup_id(ctx: &mut SpirvEmitContext) -> Word {
    ctx.builder
        .load(ctx.u32_vec3_type, None, ctx.workgroup_id, None, vec![])
        .unwrap()
}

/// Matches upstream `EmitLocalInvocationId`.
pub fn emit_local_invocation_id(ctx: &mut SpirvEmitContext) -> Word {
    ctx.builder
        .load(
            ctx.u32_vec3_type,
            None,
            ctx.local_invocation_id,
            None,
            vec![],
        )
        .unwrap()
}

/// Matches upstream `EmitInvocationId`.
pub fn emit_invocation_id(ctx: &mut SpirvEmitContext) -> Word {
    ctx.builder
        .load(ctx.u32_type, None, ctx.invocation_id, None, vec![])
        .unwrap()
}

/// Matches upstream `EmitInvocationInfo`.
pub fn emit_invocation_info(ctx: &mut SpirvEmitContext) -> Word {
    match ctx.stage {
        ShaderStage::TessellationControl | ShaderStage::TessellationEval => {
            let loaded = ctx
                .builder
                .load(ctx.u32_type, None, ctx.patch_vertices_in, None, vec![])
                .unwrap();
            let shift = ctx.builder.constant_bit32(ctx.u32_type, 16);
            ctx.builder
                .shift_left_logical(ctx.u32_type, None, loaded, shift)
                .unwrap()
        }
        _ => {
            log::warn!("(STUBBED) EmitInvocationInfo called for non-tessellation stage");
            ctx.builder.constant_bit32(ctx.u32_type, 0x00ff0000u32)
        }
    }
}

/// Matches upstream `EmitSampleId`.
pub fn emit_sample_id(ctx: &mut SpirvEmitContext) -> Word {
    ctx.builder
        .load(ctx.u32_type, None, ctx.sample_id, None, vec![])
        .unwrap()
}

/// Matches upstream `EmitIsHelperInvocation`.
pub fn emit_is_helper_invocation(ctx: &mut SpirvEmitContext) -> Word {
    ctx.builder
        .load(ctx.bool_type, None, ctx.is_helper_invocation, None, vec![])
        .unwrap()
}

/// Matches upstream `EmitYDirection`.
pub fn emit_y_direction(ctx: &mut SpirvEmitContext) -> Word {
    let value = if ctx.runtime_info.y_negate {
        -1.0f32
    } else {
        1.0f32
    };
    ctx.constant_f32(value)
}

/// Matches upstream `EmitResolutionDownFactor`.
pub fn emit_resolution_down_factor(ctx: &mut SpirvEmitContext) -> Word {
    if ctx.profile.unified_descriptor_binding {
        let pointer_type = ctx.builder.type_pointer(
            None,
            rspirv::spirv::StorageClass::PushConstant,
            ctx.f32_type,
        );
        let index = ctx
            .builder
            .constant_bit32(ctx.u32_type, ctx.rescaling_downfactor_member_index);
        let pointer = ctx
            .builder
            .access_chain(
                pointer_type,
                None,
                ctx.rescaling_push_constants,
                vec![index],
            )
            .unwrap();
        ctx.builder
            .load(ctx.f32_type, None, pointer, None, vec![])
            .unwrap()
    } else {
        let composite = ctx
            .builder
            .load(
                ctx.f32_vec4_type,
                None,
                ctx.rescaling_uniform_constant,
                None,
                vec![],
            )
            .unwrap();
        ctx.builder
            .composite_extract(ctx.f32_type, None, composite, vec![2])
            .unwrap()
    }
}

/// Matches upstream `EmitRenderArea`.
pub fn emit_render_area(ctx: &mut SpirvEmitContext) -> Word {
    if ctx.profile.unified_descriptor_binding {
        let pointer_type = ctx.builder.type_pointer(
            None,
            rspirv::spirv::StorageClass::PushConstant,
            ctx.f32_vec4_type,
        );
        let index = ctx
            .builder
            .constant_bit32(ctx.u32_type, ctx.render_are_member_index);
        let pointer = ctx
            .builder
            .access_chain(
                pointer_type,
                None,
                ctx.render_area_push_constant,
                vec![index],
            )
            .unwrap();
        ctx.builder
            .load(ctx.f32_vec4_type, None, pointer, None, vec![])
            .unwrap()
    } else {
        panic!("EmitRenderArea: non-unified descriptor binding not implemented");
    }
}
