// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/texture_fetch.cpp
//!
//! Implements TEX and TEX_b (texture fetch with implicit/explicit LOD).

use crate::ir::program::ShaderInfoExt;
use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::types::TextureInstInfo;
use crate::ir::value::Value;

/// TEX — Texture Fetch (bound form, cbuf_offset in instruction).
///
/// Upstream: `TranslatorVisitor::TEX(u64 insn)`
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

    tv.ir
        .program
        .info
        .register_texture(tex_index, crate::shader_info::TextureType::Color2D, false);

    // Build coordinate vector from source registers
    let coord_x = tv.f(src_reg);
    let coord_y = tv.f(src_reg + 1);
    let coords = tv.ir.composite_construct_f32x2(coord_x, coord_y);
    let handle = Value::ImmU32(tex_index);

    let result = match lod_mode {
        0 => {
            // Implicit LOD (fragment shader)
            tv.ir
                .image_sample_implicit_lod(handle, coords, info.to_u32())
        }
        1 => {
            // Explicit LOD
            let lod = tv.f(src_reg + 2);
            tv.ir
                .image_sample_explicit_lod(handle, coords, lod, info.to_u32())
        }
        _ => tv
            .ir
            .image_sample_implicit_lod(handle, coords, info.to_u32()),
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

/// TEX_b — Texture Fetch (bindless form, handle from register).
///
/// Upstream: `TranslatorVisitor::TEX_b(u64 insn)`
pub fn tex_b(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    // Bindless variant: handle comes from meta_reg rather than cbuf_offset.
    // For now delegate to the bound form with the same encoding.
    tex(tv, insn, opcode);
}
