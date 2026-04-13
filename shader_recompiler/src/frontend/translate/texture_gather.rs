// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/texture_gather.cpp
//!
//! Implements TLD4 and TLD4_b (texture gather — returns one component from
//! a 2x2 footprint of texels).

use crate::ir::program::ShaderInfoExt;
use super::{field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::types::TextureInstInfo;
use crate::ir::value::Value;

/// TLD4 — Texture Gather (bound form).
///
/// Gathers one component (R/G/B/A) from a 2x2 footprint.
///
/// Upstream: `TranslatorVisitor::TLD4(u64 insn)`
pub fn tld4(tv: &mut TranslatorVisitor, insn: u64, _opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_reg = tv.src_a_reg(insn);
    let tex_index = field(insn, 36, 13);
    let component = field(insn, 56, 2);

    let info = TextureInstInfo {
        descriptor_index: tex_index as u16,
        texture_type: 1,
        gather_component: component as u8,
        ..Default::default()
    };

    tv.ir.program.info.register_texture(tex_index, crate::shader_info::TextureType::ColorArray1D, false);

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

/// TLD4_b — Texture Gather (bindless form).
///
/// Upstream: `TranslatorVisitor::TLD4_b(u64 insn)`
pub fn tld4_b(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    // Bindless: component field at bits [38:36], handle from meta_reg.
    // Delegate to bound form for now.
    tld4(tv, insn, opcode);
}
