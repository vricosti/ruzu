// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/texture_fetch.cpp
//!
//! Implements TEX and TEX_b (texture fetch with implicit/explicit LOD).

use super::{bit, field, TranslatorVisitor};
use crate::frontend::maxwell_opcodes::MaxwellOpcode;
use crate::ir::types::TextureInstInfo;
use crate::ir::value::{Pred, Value};
use crate::shader_info::TextureType as ShaderTextureType;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Blod {
    None,
    Lz,
    Lb,
    Ll,
    Invalid4,
    Invalid5,
    Lba,
    Lla,
}

impl Blod {
    fn from_u32(value: u32) -> Self {
        match value {
            0 => Self::None,
            1 => Self::Lz,
            2 => Self::Lb,
            3 => Self::Ll,
            4 => Self::Invalid4,
            5 => Self::Invalid5,
            6 => Self::Lba,
            7 => Self::Lla,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TextureType {
    D1,
    Array1D,
    D2,
    Array2D,
    D3,
    Array3D,
    Cube,
    ArrayCube,
}

impl TextureType {
    fn from_u32(value: u32) -> Self {
        match value {
            0 => Self::D1,
            1 => Self::Array1D,
            2 => Self::D2,
            3 => Self::Array2D,
            4 => Self::D3,
            5 => Self::Array3D,
            6 => Self::Cube,
            7 => Self::ArrayCube,
            _ => unreachable!(),
        }
    }
}

fn get_type(type_: TextureType) -> ShaderTextureType {
    match type_ {
        TextureType::D1 => ShaderTextureType::Color1D,
        TextureType::Array1D => ShaderTextureType::ColorArray1D,
        TextureType::D2 => ShaderTextureType::Color2D,
        TextureType::Array2D => ShaderTextureType::ColorArray2D,
        TextureType::D3 => ShaderTextureType::Color3D,
        TextureType::Array3D => panic!("3D array texture type"),
        TextureType::Cube => ShaderTextureType::ColorCube,
        TextureType::ArrayCube => ShaderTextureType::ColorArrayCube,
    }
}

fn read_array(v: &mut TranslatorVisitor, reg: u32) -> Value {
    let raw = v.x(reg);
    let extracted = v
        .ir
        .bit_field_u_extract(raw, Value::ImmU32(0), Value::ImmU32(16));
    v.ir.convert_f32_from_u32(extracted)
}

fn make_coords(v: &mut TranslatorVisitor, reg: u32, type_: TextureType) -> Value {
    match type_ {
        TextureType::D1 => v.f(reg),
        TextureType::Array1D => {
            let x = v.f(reg + 1);
            let array = read_array(v, reg);
            v.ir.composite_construct_f32x2(x, array)
        }
        TextureType::D2 => {
            let x = v.f(reg);
            let y = v.f(reg + 1);
            v.ir.composite_construct_f32x2(x, y)
        }
        TextureType::Array2D => {
            let x = v.f(reg + 1);
            let y = v.f(reg + 2);
            let array = read_array(v, reg);
            v.ir.composite_construct_f32x3(x, y, array)
        }
        TextureType::D3 | TextureType::Cube => {
            let x = v.f(reg);
            let y = v.f(reg + 1);
            let z = v.f(reg + 2);
            v.ir.composite_construct_f32x3(x, y, z)
        }
        TextureType::Array3D => panic!("3D array texture type"),
        TextureType::ArrayCube => {
            let x = v.f(reg + 1);
            let y = v.f(reg + 2);
            let z = v.f(reg + 3);
            let array = read_array(v, reg);
            v.ir.composite_construct_f32x4(x, y, z, array)
        }
    }
}

fn make_lod(v: &mut TranslatorVisitor, reg: &mut u32, blod: Blod) -> Value {
    match blod {
        Blod::None | Blod::Lz => Value::ImmF32(0.0),
        Blod::Lb | Blod::Ll | Blod::Lba | Blod::Lla => {
            let value = v.f(*reg);
            *reg += 1;
            value
        }
        Blod::Invalid4 | Blod::Invalid5 => panic!("Invalid blod {:?}", blod),
    }
}

fn make_offset(v: &mut TranslatorVisitor, reg: &mut u32, type_: TextureType) -> Value {
    let value = v.x(*reg);
    *reg += 1;
    match type_ {
        TextureType::D1 | TextureType::Array1D => {
            v.ir
                .bit_field_s_extract(value, Value::ImmU32(0), Value::ImmU32(4))
        }
        TextureType::D2 | TextureType::Array2D => {
            let x = v
                .ir
                .bit_field_s_extract(value, Value::ImmU32(0), Value::ImmU32(4));
            let y = v
                .ir
                .bit_field_s_extract(value, Value::ImmU32(4), Value::ImmU32(4));
            v.ir.composite_construct_u32x2(x, y)
        }
        TextureType::D3 | TextureType::Array3D => {
            let x = v
                .ir
                .bit_field_s_extract(value, Value::ImmU32(0), Value::ImmU32(4));
            let y = v
                .ir
                .bit_field_s_extract(value, Value::ImmU32(4), Value::ImmU32(4));
            let z = v
                .ir
                .bit_field_s_extract(value, Value::ImmU32(8), Value::ImmU32(4));
            v.ir.composite_construct_u32x3(x, y, z)
        }
        TextureType::Cube | TextureType::ArrayCube => panic!("Illegal offset on CUBE sample"),
    }
}

fn has_explicit_lod(blod: Blod) -> bool {
    matches!(blod, Blod::Ll | Blod::Lla | Blod::Lz)
}

fn impl_tex(
    v: &mut TranslatorVisitor,
    insn: u64,
    aoffi: bool,
    blod: Blod,
    lc: bool,
    cbuf_offset: Option<u32>,
) {
    if lc {
        panic!("LC");
    }

    let ndv = bit(insn, 35);
    let dc = bit(insn, 50);
    let sparse_pred = Pred(field(insn, 51, 3) as u8);
    let dest_reg = field(insn, 0, 8);
    let coord_reg = field(insn, 8, 8);
    let mut meta_reg = field(insn, 20, 8);
    let type_ = TextureType::from_u32(field(insn, 28, 3));
    let mask = field(insn, 31, 4);

    let coords = make_coords(v, coord_reg, type_);
    let handle = if let Some(offset) = cbuf_offset {
        Value::ImmU32(offset)
    } else {
        let handle = v.x(meta_reg);
        meta_reg += 1;
        handle
    };
    let lod = make_lod(v, &mut meta_reg, blod);
    let offset = if aoffi {
        make_offset(v, &mut meta_reg, type_)
    } else {
        Value::Void
    };
    let dref = if dc {
        v.f(meta_reg)
    } else {
        Value::Void
    };

    let mut info = TextureInstInfo::default();
    info.texture_type = get_type(type_) as u8;
    info.is_depth = dc;
    info.has_bias = matches!(blod, Blod::Lb | Blod::Lba);
    info.has_lod_clamp = lc;

    // TODO: ruzu's TextureInstInfo still lacks upstream `ndv_is_active`.
    let _ = ndv;

    let sample = if !dc {
        if has_explicit_lod(blod) {
            v.ir
                .image_sample_explicit_lod_full(handle, coords, lod, offset, info.to_u32())
        } else {
            v.ir
                .image_sample_implicit_lod_full(handle, coords, lod, offset, info.to_u32())
        }
    } else if has_explicit_lod(blod) {
        v.ir
            .image_sample_dref_explicit_lod_full(handle, coords, dref, lod, offset, info.to_u32())
    } else {
        v.ir.image_sample_dref_implicit_lod_full(
            handle,
            coords,
            dref,
            lod,
            offset,
            info.to_u32(),
        )
    };

    let mut reg = dest_reg;
    for element in 0..4 {
        if ((mask >> element) & 1) == 0 {
            continue;
        }
        let value = if dc {
            if element < 3 {
                sample
            } else {
                Value::ImmF32(1.0)
            }
        } else {
            v.ir
                .composite_extract_f32x4(sample, Value::ImmU32(element))
        };
        v.set_f(reg, value);
        reg += 1;
    }

    if sparse_pred != Pred::PT {
        // ruzu does not yet wire `GetSparseFromOp` for texture samples.
        v.ir.set_pred(sparse_pred, Value::ImmU1(true));
    }
}

/// TEX — Texture Fetch (bound form, cbuf_offset in instruction).
///
/// Upstream: `TranslatorVisitor::TEX(u64 insn)`
pub fn tex(tv: &mut TranslatorVisitor, insn: u64, _opcode: MaxwellOpcode) {
    let aoffi = bit(insn, 54);
    let blod = Blod::from_u32(field(insn, 55, 3));
    let lc = bit(insn, 58);
    let cbuf_offset = field(insn, 36, 13) * 4;
    impl_tex(tv, insn, aoffi, blod, lc, Some(cbuf_offset));
}

/// TEX_b — Texture Fetch (bindless form, handle from register).
///
/// Upstream: `TranslatorVisitor::TEX_b(u64 insn)`
pub fn tex_b(tv: &mut TranslatorVisitor, insn: u64, _opcode: MaxwellOpcode) {
    let aoffi = bit(insn, 36);
    let blod = Blod::from_u32(field(insn, 37, 3));
    let lc = bit(insn, 40);
    impl_tex(tv, insn, aoffi, blod, lc, None);
}
