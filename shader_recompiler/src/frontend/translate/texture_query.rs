// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/texture_query.cpp
//!
//! Implements TXQ and TXQ_b (texture query — dimensions, texture type, etc.).

use super::{field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::program::ShaderInfoExt;
use crate::ir::types::TextureInstInfo;
use crate::ir::value::Value;

/// TXQ — Texture Query (bound form).
///
/// Queries texture metadata (dimensions, mip count, etc.) using a
/// cbuf-bound texture handle.
///
/// Upstream: `TranslatorVisitor::TXQ(u64 insn)`
pub fn txq(tv: &mut TranslatorVisitor, insn: u64, _opcode: MaxwellOpcode) {
    let dst = tv.dst_reg(insn);
    let src_reg = tv.src_a_reg(insn);
    let tex_index = field(insn, 36, 13);

    let info = TextureInstInfo {
        descriptor_index: tex_index as u16,
        ..Default::default()
    };

    tv.ir.program.info.register_texture(
        tex_index,
        crate::shader_info::TextureType::ColorArray1D,
        false,
    );

    let lod = tv.x(src_reg);
    let handle = Value::ImmU32(tex_index);

    let result = tv.ir.image_query_dimensions(handle, lod, info.to_u32());

    let width = tv.ir.composite_extract_u32x4(result, Value::ImmU32(0));
    let height = tv.ir.composite_extract_u32x4(result, Value::ImmU32(1));

    tv.set_x(dst, width);
    tv.set_x(dst + 1, height);
}

/// TXQ_b — Texture Query (bindless form).
///
/// Upstream: `TranslatorVisitor::TXQ_b(u64 insn)`
pub fn txq_b(tv: &mut TranslatorVisitor, insn: u64, opcode: MaxwellOpcode) {
    // Bindless: handle comes from src_reg; the real src follows.
    // Delegate to bound form for now.
    txq(tv, insn, opcode);
}
