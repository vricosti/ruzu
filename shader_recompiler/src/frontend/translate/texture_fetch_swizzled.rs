// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/texture_fetch_swizzled.cpp
//!
//! Implements TEXS (texture fetch swizzled, compact dual-destination encoding).

use super::{field, TranslatorVisitor};
use crate::ir::types::TextureInstInfo;
use crate::ir::value::Value;

/// TEXS — Texture Fetch Swizzled.
///
/// The TEXS encoding packs two destination registers (dest_reg_a, dest_reg_b)
/// and two source registers (src_reg_a, src_reg_b) into one instruction word,
/// along with a compact cbuf_offset and a swizzle mask that selects which
/// components are written and how they map to the two destination registers.
///
/// Upstream: `TranslatorVisitor::TEXS(u64 insn)`
pub fn texs(tv: &mut TranslatorVisitor, insn: u64) {
    let dst1 = tv.dst_reg(insn);
    let dst2 = field(insn, 28, 8);
    let src_reg = tv.src_a_reg(insn);
    let tex_index = field(insn, 36, 13);

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

    let result = tv
        .ir
        .image_sample_implicit_lod(handle, coords, info.to_u32());

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
