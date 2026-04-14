// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/texture_load_swizzled.cpp
//!
//! Implements TLDS (texture load swizzled, compact dual-destination encoding).

use super::{field, TranslatorVisitor};
use crate::ir::program::ShaderInfoExt;
use crate::ir::types::TextureInstInfo;
use crate::ir::value::Value;

/// TLDS — Texture Load Swizzled.
///
/// Like TLD but with the TEXS-style compact encoding that packs two
/// destination registers and a swizzle mask into one instruction word.
/// Uses integer coordinates and an explicit LOD.
///
/// Upstream: `TranslatorVisitor::TLDS(u64 insn)`
pub fn tlds(tv: &mut TranslatorVisitor, insn: u64) {
    let dst = tv.dst_reg(insn);
    let src_reg = tv.src_a_reg(insn);
    let tex_index = field(insn, 36, 13);

    let info = TextureInstInfo {
        descriptor_index: tex_index as u16,
        texture_type: 1,
        ..Default::default()
    };

    tv.ir.program.info.register_texture(
        tex_index,
        crate::shader_info::TextureType::ColorArray1D,
        false,
    );

    let coord_x = tv.x(src_reg);
    let coord_y = tv.x(src_reg + 1);
    let coords = tv.ir.composite_construct_u32x2(coord_x, coord_y);
    let lod = Value::ImmU32(0);
    let handle = Value::ImmU32(tex_index);

    let result = tv.ir.image_fetch(handle, coords, lod, info.to_u32());

    let r = tv.ir.composite_extract_f32x4(result, Value::ImmU32(0));
    tv.set_f(dst, r);
}
