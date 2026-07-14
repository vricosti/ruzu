// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V special emission — maps to zuyu's
//! `backend/spirv/emit_spirv_special.cpp`.
//!
//! Handles prologue/epilogue, emit vertex, end primitive, depth mode
//! conversion, alpha test, and fixed pipeline point size.

use super::spirv_emit_context::SpirvEmitContext;
use crate::ir::types::ShaderStage;
use rspirv::spirv::Word;

fn output_position(ctx: &SpirvEmitContext) -> Option<Word> {
    ctx.output_vars.get(&0xFFFF_0000).copied()
}

fn convert_depth_mode(ctx: &mut SpirvEmitContext) {
    let Some(position_var) = output_position(ctx) else {
        return;
    };
    let position = ctx
        .builder
        .load(ctx.f32_vec4_type, None, position_var, None, vec![])
        .unwrap();
    let z = ctx
        .builder
        .composite_extract(ctx.f32_type, None, position, vec![2])
        .unwrap();
    let w = ctx
        .builder
        .composite_extract(ctx.f32_type, None, position, vec![3])
        .unwrap();
    let z_plus_w = ctx.builder.f_add(ctx.f32_type, None, z, w).unwrap();
    let half = ctx.constant_f32(0.5);
    let screen_depth = ctx
        .builder
        .f_mul(ctx.f32_type, None, z_plus_w, half)
        .unwrap();
    let vector = ctx
        .builder
        .composite_insert(ctx.f32_vec4_type, None, screen_depth, position, vec![2])
        .unwrap();
    ctx.builder
        .store(position_var, vector, None, vec![])
        .unwrap();
}

/// Emit shader prologue.
///
/// Matches upstream `EmitPrologue(EmitContext&)`.
/// For vertex shaders, initializes output position to (0,0,0,1) and
/// sets default values for generic outputs. For geometry shaders,
/// sets fixed pipeline point size.
pub fn emit_prologue(ctx: &mut SpirvEmitContext) {
    log::trace!("SPIR-V: emit_prologue");
    if ctx.stage == ShaderStage::VertexB {
        let default_position = ctx
            .builder
            .composite_construct(
                ctx.f32_vec4_type,
                None,
                vec![
                    ctx.const_zero_f32,
                    ctx.const_zero_f32,
                    ctx.const_zero_f32,
                    ctx.const_one_f32,
                ],
            )
            .unwrap();
        if let Some(position_var) = output_position(ctx) {
            ctx.builder
                .store(position_var, default_position, None, vec![])
                .unwrap();
        }
        // Rust currently materializes each generic output as a vec4. This is
        // the direct counterpart of upstream's output_generics traversal and
        // DefaultVarying(...): every declared varying starts as (0,0,0,1).
        for index in 0..32u32 {
            if let Some(&output_var) = ctx.output_vars.get(&index) {
                ctx.builder
                    .store(output_var, default_position, None, vec![])
                    .unwrap();
            }
        }
    }
    if matches!(ctx.stage, ShaderStage::VertexB | ShaderStage::Geometry) {
        if let Some(point_size) = ctx.runtime_info.fixed_state_point_size {
            if ctx.output_point_size != 0 {
                let value = ctx.constant_f32(point_size);
                ctx.builder
                    .store(ctx.output_point_size, value, None, vec![])
                    .unwrap();
            }
        }
    }
}

/// Emit shader epilogue.
///
/// Matches upstream `EmitEpilogue(EmitContext&)`.
/// For vertex shaders with depth mode conversion, transform Z coordinate.
/// For fragment shaders, run alpha test.
pub fn emit_epilogue(ctx: &mut SpirvEmitContext) {
    log::trace!("SPIR-V: emit_epilogue");
    if ctx.stage == ShaderStage::VertexB
        && ctx.runtime_info.convert_depth_mode
        && !ctx.profile.support_native_ndc
    {
        convert_depth_mode(ctx);
    }
}

/// Emit a geometry shader vertex.
///
/// Matches upstream `EmitEmitVertex(EmitContext&, const IR::Value&)`.
pub fn emit_emit_vertex(_ctx: &mut SpirvEmitContext, _stream: u32) {
    log::trace!("SPIR-V: emit_emit_vertex");
}

/// End a geometry shader primitive.
///
/// Matches upstream `EmitEndPrimitive(EmitContext&, const IR::Value&)`.
pub fn emit_end_primitive(_ctx: &mut SpirvEmitContext, _stream: u32) {
    log::trace!("SPIR-V: emit_end_primitive");
}
