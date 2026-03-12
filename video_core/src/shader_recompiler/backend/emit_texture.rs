// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! SPIR-V emission for texture sampling, fetch, query, and gather opcodes.

use rspirv::dr::Operand;
use rspirv::spirv;

use super::spirv_context::EmitContext;
use crate::shader_recompiler::ir::{self, Opcode};

pub fn emit_image_sample(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    // ImageSampleImplicitLod / ImageSampleExplicitLod
    // args: descriptor_index, coord_x, coord_y [, lod]
    let desc_idx = inst.arg(0).imm_u32();
    let coord_x = ctx.resolve_value(inst.arg(1));
    let coord_y = ctx.resolve_value(inst.arg(2));

    if let Some(&tex_var) = ctx.texture_vars.get(&desc_idx) {
        // Build 2D coordinate vector
        let coord = ctx
            .builder
            .composite_construct(ctx.f32_vec2_type, None, vec![coord_x, coord_y])
            .unwrap();

        // Load the sampled image
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
            // Explicit LOD
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
            // Implicit LOD
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
        // Texture not bound — return zero vector
        let zero = ctx.const_zero_f32;
        let id = ctx
            .builder
            .composite_construct(ctx.f32_vec4_type, None, vec![zero, zero, zero, zero])
            .unwrap();
        ctx.set_value(block_idx, inst_idx, id);
    }
}

pub fn emit_image_fetch(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    // ImageFetch(descriptor_index, coord_x, coord_y, lod)
    let desc_idx = inst.arg(0).imm_u32();
    let coord_x = ctx.resolve_value(inst.arg(1));
    let coord_y = ctx.resolve_value(inst.arg(2));

    if let Some(&tex_var) = ctx.texture_vars.get(&desc_idx) {
        // Build integer coordinate vector
        let coord = ctx
            .builder
            .composite_construct(ctx.u32_vec2_type, None, vec![coord_x, coord_y])
            .unwrap();

        // Load the sampled image, then extract the image from it
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
        let image = ctx
            .builder
            .image(img_type, None, sampled_image)
            .unwrap();

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

pub fn emit_image_query(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    // ImageQueryDimensions(descriptor_index, lod)
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
        let image = ctx
            .builder
            .image(img_type, None, sampled_image)
            .unwrap();

        let lod = if inst.args.len() > 1 {
            ctx.resolve_value(inst.arg(1))
        } else {
            ctx.const_zero_u32
        };

        // OpImageQuerySizeLod returns ivec2 for 2D
        let i32_vec2 = ctx.builder.type_vector(ctx.i32_type, 2);
        let id = ctx
            .builder
            .image_query_size_lod(i32_vec2, None, image, lod)
            .unwrap();

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

pub fn emit_image_gather(ctx: &mut EmitContext, inst: &ir::Inst, block_idx: u32, inst_idx: u32) {
    // ImageGather / ImageGatherDref
    // args: descriptor_index, coord_x, coord_y, component [, dref]
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
