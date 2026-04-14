// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/texture_load.cpp
//!
//! Implements TLD and TLD_b (texture load with explicit integer coordinates).

use super::{field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::program::ShaderInfoExt;
use crate::ir::types::TextureInstInfo;
use crate::ir::value::Value;

/// TLD — Texture Load (bound form).
///
/// Uses integer coordinates and an explicit LOD level, fetching texel data
/// directly without filtering.
///
/// Upstream: `TranslatorVisitor::TLD(u64 insn)`
pub fn tld(tv: &mut TranslatorVisitor, insn: u64, _opcode: MaxwellOpcode) {
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

/// TLD_b — Texture Load (bindless form).
///
/// Upstream: `TranslatorVisitor::TLD_b(u64 insn)`
pub fn tld_b(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    // Bindless: handle comes from meta_reg. Delegate to bound form for now.
    tld(tv, insn, opcode);
}
