// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Texture operation translation: TEX, TEXS, TLD, TLDS, TLD4, TXQ, TMML, TXD.

use super::{bit, field, TranslatorVisitor};
use crate::shader_recompiler::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::shader_recompiler::ir::types::TextureInstInfo;
use crate::shader_recompiler::ir::value::Value;

pub fn tex(tv: &mut TranslatorVisitor, insn: u64, _opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_reg = tv.src_a_reg(insn);
    let tex_index = field(insn, 36, 13);
    let dim = field(insn, 28, 3);
    let lod_mode = field(insn, 55, 2);

    let info = TextureInstInfo {
        descriptor_index: tex_index as u16,
        texture_type: dim as u8,
        is_depth: false,
        has_bias: lod_mode == 2,
        has_lod_clamp: false,
        gather_component: 0,
        num_derivatives: 0,
    };

    tv.ir.program.info.register_texture(tex_index, dim as u8, false);

    // Build coordinate vector from source registers
    let coord_x = tv.f(src_reg);
    let coord_y = tv.f(src_reg + 1);
    let coords = tv.ir.composite_construct_f32x2(coord_x, coord_y);
    let handle = Value::ImmU32(tex_index);

    let result = match lod_mode {
        0 => {
            // Implicit LOD (fragment shader)
            tv.ir.image_sample_implicit_lod(handle, coords, info.to_u32())
        }
        1 => {
            // Explicit LOD
            let lod = tv.f(src_reg + 2);
            tv.ir.image_sample_explicit_lod(handle, coords, lod, info.to_u32())
        }
        _ => {
            tv.ir.image_sample_implicit_lod(handle, coords, info.to_u32())
        }
    };

    // Extract result components to destination registers
    let r = tv.ir.composite_extract_f32x4(result, Value::ImmU32(0));
    let g = tv.ir.composite_extract_f32x4(result, Value::ImmU32(1));
    let b = tv.ir.composite_extract_f32x4(result, Value::ImmU32(2));
    let a = tv.ir.composite_extract_f32x4(result, Value::ImmU32(3));

    tv.set_f(dst, r);
    tv.set_f(dst + 1, g);
    tv.set_f(dst + 2, b);
    tv.set_f(dst + 3, a);
}

pub fn texs(tv: &mut TranslatorVisitor, insn: u64) {
    let dst1 = tv.dst_reg(insn);
    let dst2 = field(insn, 28, 8);
    let src_reg = tv.src_a_reg(insn);
    let tex_index = field(insn, 36, 5);

    let info = TextureInstInfo {
        descriptor_index: tex_index as u16,
        texture_type: 1, // 2D
        ..Default::default()
    };

    tv.ir.program.info.register_texture(tex_index, 1, false);

    let coord_x = tv.f(src_reg);
    let coord_y = tv.f(src_reg + 1);
    let coords = tv.ir.composite_construct_f32x2(coord_x, coord_y);
    let handle = Value::ImmU32(tex_index);

    let result = tv.ir.image_sample_implicit_lod(handle, coords, info.to_u32());

    let r = tv.ir.composite_extract_f32x4(result, Value::ImmU32(0));
    let g = tv.ir.composite_extract_f32x4(result, Value::ImmU32(1));
    let b = tv.ir.composite_extract_f32x4(result, Value::ImmU32(2));
    let a = tv.ir.composite_extract_f32x4(result, Value::ImmU32(3));

    // TEXS writes to two separate register ranges
    tv.set_f(dst1, r);
    tv.set_f(dst1 + 1, g);
    tv.set_f(dst2, b);
    tv.set_f(dst2 + 1, a);
}

pub fn tld(tv: &mut TranslatorVisitor, insn: u64, _opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_reg = tv.src_a_reg(insn);
    let tex_index = field(insn, 36, 13);

    let info = TextureInstInfo {
        descriptor_index: tex_index as u16,
        texture_type: 1,
        ..Default::default()
    };

    tv.ir.program.info.register_texture(tex_index, 1, false);

    let coord_x = tv.x(src_reg);
    let coord_y = tv.x(src_reg + 1);
    let coords = tv.ir.composite_construct_u32x2(coord_x, coord_y);
    let lod = Value::ImmU32(0);
    let handle = Value::ImmU32(tex_index);

    let result = tv.ir.image_fetch(handle, coords, lod, info.to_u32());

    let r = tv.ir.composite_extract_f32x4(result, Value::ImmU32(0));
    tv.set_f(dst, r);
}

pub fn tlds(tv: &mut TranslatorVisitor, insn: u64) {
    // TLDS is similar to TLD but with different register packing
    tld(tv, insn, MaxwellOpcode::TLD);
}

pub fn tld4(tv: &mut TranslatorVisitor, insn: u64, _opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_reg = tv.src_a_reg(insn);
    let tex_index = field(insn, 36, 13);
    let component = field(insn, 52, 2);

    let info = TextureInstInfo {
        descriptor_index: tex_index as u16,
        texture_type: 1,
        gather_component: component as u8,
        ..Default::default()
    };

    tv.ir.program.info.register_texture(tex_index, 1, false);

    let coord_x = tv.f(src_reg);
    let coord_y = tv.f(src_reg + 1);
    let coords = tv.ir.composite_construct_f32x2(coord_x, coord_y);
    let handle = Value::ImmU32(tex_index);

    let result = tv.ir.image_gather(handle, coords, info.to_u32());

    let r = tv.ir.composite_extract_f32x4(result, Value::ImmU32(0));
    let g = tv.ir.composite_extract_f32x4(result, Value::ImmU32(1));
    let b = tv.ir.composite_extract_f32x4(result, Value::ImmU32(2));
    let a = tv.ir.composite_extract_f32x4(result, Value::ImmU32(3));

    tv.set_f(dst, r);
    tv.set_f(dst + 1, g);
    tv.set_f(dst + 2, b);
    tv.set_f(dst + 3, a);
}

pub fn txq(tv: &mut TranslatorVisitor, insn: u64, _opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_reg = tv.src_a_reg(insn);
    let tex_index = field(insn, 36, 13);

    let info = TextureInstInfo {
        descriptor_index: tex_index as u16,
        ..Default::default()
    };

    tv.ir.program.info.register_texture(tex_index, 1, false);

    let lod = tv.x(src_reg);
    let handle = Value::ImmU32(tex_index);

    let result = tv.ir.image_query_dimensions(handle, lod, info.to_u32());

    let width = tv.ir.composite_extract_u32x4(result, Value::ImmU32(0));
    let height = tv.ir.composite_extract_u32x4(result, Value::ImmU32(1));

    tv.set_x(dst, width);
    tv.set_x(dst + 1, height);
}
