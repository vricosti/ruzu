// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/texture_mipmap_level.cpp
//!
//! TMML / TMML_b — Texture Mipmap Level query.

use super::{field, TranslatorVisitor};
use crate::ir::types::TextureInstInfo;
use crate::ir::value::Value;
use crate::shader_info::TextureType;

/// Maxwell `TextureType` field encoding (3 bits at [30:28]). Mirrors
/// upstream anonymous-namespace enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MaxwellTexType {
    _1D = 0,
    Array1D = 1,
    _2D = 2,
    Array2D = 3,
    _3D = 4,
    Array3D = 5,
    Cube = 6,
    ArrayCube = 7,
}

impl MaxwellTexType {
    fn from_bits(v: u32) -> Self {
        match v & 7 {
            0 => MaxwellTexType::_1D,
            1 => MaxwellTexType::Array1D,
            2 => MaxwellTexType::_2D,
            3 => MaxwellTexType::Array2D,
            4 => MaxwellTexType::_3D,
            5 => MaxwellTexType::Array3D,
            6 => MaxwellTexType::Cube,
            7 => MaxwellTexType::ArrayCube,
            _ => unreachable!(),
        }
    }
}

fn get_shader_texture_type(t: MaxwellTexType) -> TextureType {
    match t {
        MaxwellTexType::_1D => TextureType::Color1D,
        MaxwellTexType::Array1D => TextureType::ColorArray1D,
        MaxwellTexType::_2D => TextureType::Color2D,
        MaxwellTexType::Array2D => TextureType::ColorArray2D,
        MaxwellTexType::_3D => TextureType::Color3D,
        MaxwellTexType::Array3D => panic!("3D array texture type"),
        MaxwellTexType::Cube => TextureType::ColorCube,
        MaxwellTexType::ArrayCube => TextureType::ColorArrayCube,
    }
}

/// Build the coordinate operand. Port of upstream `MakeCoords`.
fn make_coords(tv: &mut TranslatorVisitor, reg: u32, t: MaxwellTexType) -> Value {
    match t {
        MaxwellTexType::_1D => tv.f(reg),
        MaxwellTexType::Array1D => tv.f(reg + 1),
        MaxwellTexType::_2D => {
            let a = tv.f(reg);
            let b = tv.f(reg + 1);
            tv.ir.composite_construct_f32x2(a, b)
        }
        MaxwellTexType::Array2D => {
            let a = tv.f(reg + 1);
            let b = tv.f(reg + 2);
            tv.ir.composite_construct_f32x2(a, b)
        }
        MaxwellTexType::_3D => {
            let a = tv.f(reg);
            let b = tv.f(reg + 1);
            let c = tv.f(reg + 2);
            tv.ir.composite_construct_f32x4(a, b, c, Value::ImmF32(0.0))
        }
        MaxwellTexType::Array3D => panic!("3D array texture type"),
        MaxwellTexType::Cube => {
            let a = tv.f(reg);
            let b = tv.f(reg + 1);
            let c = tv.f(reg + 2);
            tv.ir.composite_construct_f32x4(a, b, c, Value::ImmF32(0.0))
        }
        MaxwellTexType::ArrayCube => {
            let a = tv.f(reg + 1);
            let b = tv.f(reg + 2);
            let c = tv.f(reg + 3);
            tv.ir.composite_construct_f32x4(a, b, c, Value::ImmF32(0.0))
        }
    }
}

/// Shared TMML impl for both bound and bindless forms. Port of upstream
/// anonymous-namespace `Impl`.
fn impl_tmml(tv: &mut TranslatorVisitor, insn: u64, is_bindless: bool) {
    let dest_reg = field(insn, 0, 8);
    let coord_reg = field(insn, 8, 8);
    let mut meta_reg = field(insn, 20, 8);
    let t = MaxwellTexType::from_bits(field(insn, 28, 3));
    let mask = field(insn, 31, 4);
    let cbuf_offset = field(insn, 36, 13);

    if (mask & 0b1100) != 0 {
        panic!("TMML BA results are not implemented");
    }
    let coords = make_coords(tv, coord_reg, t);

    let handle = if is_bindless {
        let h = tv.x(meta_reg);
        meta_reg += 1;
        h
    } else {
        Value::ImmU32(cbuf_offset * 4)
    };

    let mut info = TextureInstInfo::default();
    info.texture_type = get_shader_texture_type(t) as u8;
    let sample = tv.ir.image_query_lod(handle, coords, info.to_u32());

    // Extract masked components from the 4-component sample; first 2 get
    // shifted-left by 8 (upstream's `ShiftLeftLogical(casted, 8)`); last
    // 2 are float-written via `set_f`.
    let mut dest = dest_reg;
    for element in 0u32..4 {
        if ((mask >> element) & 1) == 0 {
            continue;
        }
        let value = tv
            .ir
            .composite_extract_f32x4(sample.clone(), Value::ImmU32(element));
        if element < 2 {
            let casted = tv.ir.convert_u32_from_f32(value);
            let shifted = tv.ir.shift_left_logical_32(casted, Value::ImmU32(8));
            tv.set_x(dest, shifted);
        } else {
            tv.set_f(dest, value);
        }
        dest += 1;
    }
    let _ = meta_reg;
}

/// TMML — Texture Mipmap Level query (bound).
pub fn tmml(tv: &mut TranslatorVisitor, insn: u64) {
    impl_tmml(tv, insn, false);
}

/// TMML_b — Texture Mipmap Level query (bindless).
pub fn tmml_b(tv: &mut TranslatorVisitor, insn: u64) {
    impl_tmml(tv, insn, true);
}
