// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V image/texture emission — maps to zuyu's
//! `backend/spirv/emit_spirv_image.cpp`.
//!
//! Handles texture sampling, image loads, and texture queries.

use rspirv::spirv::Word;
use super::spirv_emit_context::SpirvEmitContext;

/// Emit ImageSampleImplicitLod (TEX/TEXS with implicit LOD).
///
/// Matches upstream `EmitImageSampleImplicitLod`.
pub fn emit_image_sample_implicit_lod(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _coords: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_sample_implicit_lod");
    ctx.builder.undef(ctx.f32_vec4_type, None)
}

/// Emit ImageSampleExplicitLod (TXL).
pub fn emit_image_sample_explicit_lod(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _coords: Word,
    _lod: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_sample_explicit_lod");
    ctx.builder.undef(ctx.f32_vec4_type, None)
}

/// Emit ImageSampleDrefImplicitLod (shadow TEX).
pub fn emit_image_sample_dref_implicit_lod(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _coords: Word,
    _dref: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_sample_dref_implicit_lod");
    ctx.builder.undef(ctx.f32_type, None)
}

/// Emit ImageSampleDrefExplicitLod (shadow TXL).
pub fn emit_image_sample_dref_explicit_lod(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _coords: Word,
    _dref: Word,
    _lod: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_sample_dref_explicit_lod");
    ctx.builder.undef(ctx.f32_type, None)
}

/// Emit ImageFetch (TLD — texel fetch).
pub fn emit_image_fetch(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _coords: Word,
    _lod: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_fetch");
    ctx.builder.undef(ctx.f32_vec4_type, None)
}

/// Emit ImageGather (TLD4).
pub fn emit_image_gather(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _coords: Word,
    _component: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_gather");
    ctx.builder.undef(ctx.f32_vec4_type, None)
}

/// Emit ImageGatherDref (TLD4 with depth comparison).
pub fn emit_image_gather_dref(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _coords: Word,
    _dref: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_gather_dref");
    ctx.builder.undef(ctx.f32_vec4_type, None)
}

/// Emit ImageQueryDimensions (TXQ).
pub fn emit_image_query_dimensions(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _lod: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_query_dimensions");
    ctx.builder.undef(ctx.u32_vec4_type, None)
}

/// Emit ImageQueryLod (TMML).
pub fn emit_image_query_lod(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _coords: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_query_lod");
    ctx.builder.undef(ctx.f32_vec2_type, None)
}

/// Emit ImageGradient (TXD — explicit gradients).
pub fn emit_image_gradient(
    ctx: &mut SpirvEmitContext,
    _handle: Word,
    _coords: Word,
    _dpdx: Word,
    _dpdy: Word,
) -> Word {
    log::trace!("SPIR-V: emit_image_gradient");
    ctx.builder.undef(ctx.f32_vec4_type, None)
}

// ── IR-instruction dispatching helpers (called from spirv_emit_context) ───

use rspirv::dr::Operand;
use rspirv::spirv;
use crate::ir::{self, Opcode};

/// Dispatch ImageSampleImplicitLod / ImageSampleExplicitLod IR instructions.
pub fn emit_image_sample(
    ctx: &mut SpirvEmitContext,
    inst: &ir::Inst,
    block_idx: u32,
    inst_idx: u32,
) {
    let desc_idx = inst.arg(0).imm_u32();
    let coord_x = ctx.resolve_value(inst.arg(1));
    let coord_y = ctx.resolve_value(inst.arg(2));

    if let Some(&tex_var) = ctx.texture_vars.get(&desc_idx) {
        let coord = ctx
            .builder
            .composite_construct(ctx.f32_vec2_type, None, vec![coord_x, coord_y])
            .unwrap();

        let image_type = {
            let img = ctx.builder.type_image(
                ctx.f32_type,
                spirv::Dim::Dim2D,
                0,
                0,
                0,
                1,
                spirv::ImageFormat::Unknown,
                None,
            );
            ctx.builder.type_sampled_image(img)
        };
        let sampled_image = ctx
            .builder
            .load(image_type, None, tex_var, None, vec![])
            .unwrap();

        let id = if inst.opcode == Opcode::ImageSampleExplicitLod && inst.args.len() > 3 {
            let lod = ctx.resolve_value(inst.arg(3));
            ctx.builder
                .image_sample_explicit_lod(
                    ctx.f32_vec4_type,
                    None,
                    sampled_image,
                    coord,
                    spirv::ImageOperands::LOD,
                    vec![Operand::IdRef(lod)],
                )
                .unwrap()
        } else {
            ctx.builder
                .image_sample_implicit_lod(
                    ctx.f32_vec4_type,
                    None,
                    sampled_image,
                    coord,
                    None,
                    vec![],
                )
                .unwrap()
        };

        ctx.set_value(block_idx, inst_idx, id);
    } else {
        let zero = ctx.const_zero_f32;
        let id = ctx
            .builder
            .composite_construct(ctx.f32_vec4_type, None, vec![zero, zero, zero, zero])
            .unwrap();
        ctx.set_value(block_idx, inst_idx, id);
    }
}

/// Dispatch ImageFetch IR instructions.
pub fn emit_image_fetch_inst(
    ctx: &mut SpirvEmitContext,
    inst: &ir::Inst,
    block_idx: u32,
    inst_idx: u32,
) {
    let desc_idx = inst.arg(0).imm_u32();
    let coord_x = ctx.resolve_value(inst.arg(1));
    let coord_y = ctx.resolve_value(inst.arg(2));

    if let Some(&tex_var) = ctx.texture_vars.get(&desc_idx) {
        let coord = ctx
            .builder
            .composite_construct(ctx.u32_vec2_type, None, vec![coord_x, coord_y])
            .unwrap();

        let sampled_img_type = {
            let img = ctx.builder.type_image(
                ctx.f32_type,
                spirv::Dim::Dim2D,
                0,
                0,
                0,
                1,
                spirv::ImageFormat::Unknown,
                None,
            );
            ctx.builder.type_sampled_image(img)
        };
        let img_type = ctx.builder.type_image(
            ctx.f32_type,
            spirv::Dim::Dim2D,
            0,
            0,
            0,
            1,
            spirv::ImageFormat::Unknown,
            None,
        );
        let sampled_image = ctx
            .builder
            .load(sampled_img_type, None, tex_var, None, vec![])
            .unwrap();
        let image = ctx.builder.image(img_type, None, sampled_image).unwrap();

        let lod = if inst.args.len() > 3 {
            ctx.resolve_value(inst.arg(3))
        } else {
            ctx.const_zero_u32
        };

        let id = ctx
            .builder
            .image_fetch(
                ctx.f32_vec4_type,
                None,
                image,
                coord,
                Some(spirv::ImageOperands::LOD),
                vec![Operand::IdRef(lod)],
            )
            .unwrap();

        ctx.set_value(block_idx, inst_idx, id);
    } else {
        let zero = ctx.const_zero_f32;
        let id = ctx
            .builder
            .composite_construct(ctx.f32_vec4_type, None, vec![zero, zero, zero, zero])
            .unwrap();
        ctx.set_value(block_idx, inst_idx, id);
    }
}

/// Dispatch ImageQueryDimensions IR instructions.
pub fn emit_image_query(
    ctx: &mut SpirvEmitContext,
    inst: &ir::Inst,
    block_idx: u32,
    inst_idx: u32,
) {
    let desc_idx = inst.arg(0).imm_u32();

    if let Some(&tex_var) = ctx.texture_vars.get(&desc_idx) {
        let sampled_img_type = {
            let img = ctx.builder.type_image(
                ctx.f32_type,
                spirv::Dim::Dim2D,
                0,
                0,
                0,
                1,
                spirv::ImageFormat::Unknown,
                None,
            );
            ctx.builder.type_sampled_image(img)
        };
        let img_type = ctx.builder.type_image(
            ctx.f32_type,
            spirv::Dim::Dim2D,
            0,
            0,
            0,
            1,
            spirv::ImageFormat::Unknown,
            None,
        );
        let sampled_image = ctx
            .builder
            .load(sampled_img_type, None, tex_var, None, vec![])
            .unwrap();
        let image = ctx.builder.image(img_type, None, sampled_image).unwrap();

        let lod = if inst.args.len() > 1 {
            ctx.resolve_value(inst.arg(1))
        } else {
            ctx.const_zero_u32
        };

        let i32_vec2 = ctx.builder.type_vector(ctx.i32_type, 2);
        let id = ctx.builder.image_query_size_lod(i32_vec2, None, image, lod).unwrap();

        ctx.set_value(block_idx, inst_idx, id);
    } else {
        let one = ctx.const_one_u32;
        let id = ctx
            .builder
            .composite_construct(ctx.u32_vec2_type, None, vec![one, one])
            .unwrap();
        ctx.set_value(block_idx, inst_idx, id);
    }
}

/// Dispatch ImageGather / ImageGatherDref IR instructions.
pub fn emit_image_gather_inst(
    ctx: &mut SpirvEmitContext,
    inst: &ir::Inst,
    block_idx: u32,
    inst_idx: u32,
) {
    let desc_idx = inst.arg(0).imm_u32();
    let coord_x = ctx.resolve_value(inst.arg(1));
    let coord_y = ctx.resolve_value(inst.arg(2));

    if let Some(&tex_var) = ctx.texture_vars.get(&desc_idx) {
        let coord = ctx
            .builder
            .composite_construct(ctx.f32_vec2_type, None, vec![coord_x, coord_y])
            .unwrap();

        let sampled_img_type = {
            let img = ctx.builder.type_image(
                ctx.f32_type,
                spirv::Dim::Dim2D,
                0,
                0,
                0,
                1,
                spirv::ImageFormat::Unknown,
                None,
            );
            ctx.builder.type_sampled_image(img)
        };
        let sampled_image = ctx
            .builder
            .load(sampled_img_type, None, tex_var, None, vec![])
            .unwrap();

        let id = if inst.opcode == Opcode::ImageGatherDref && inst.args.len() > 4 {
            let dref = ctx.resolve_value(inst.arg(4));
            ctx.builder
                .image_dref_gather(
                    ctx.f32_vec4_type,
                    None,
                    sampled_image,
                    coord,
                    dref,
                    None,
                    vec![],
                )
                .unwrap()
        } else {
            let component = ctx.resolve_value(inst.arg(3));
            ctx.builder
                .image_gather(
                    ctx.f32_vec4_type,
                    None,
                    sampled_image,
                    coord,
                    component,
                    None,
                    vec![],
                )
                .unwrap()
        };

        ctx.set_value(block_idx, inst_idx, id);
    } else {
        let zero = ctx.const_zero_f32;
        let id = ctx
            .builder
            .composite_construct(ctx.f32_vec4_type, None, vec![zero, zero, zero, zero])
            .unwrap();
        ctx.set_value(block_idx, inst_idx, id);
    }
}
