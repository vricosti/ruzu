// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/texture_fetch_swizzled.cpp
//!
//! Implements TEXS (texture fetch swizzled, compact dual-destination encoding).

use super::{field, TranslatorVisitor};
use crate::ir::types::TextureInstInfo;
use crate::ir::value::Value;
use crate::shader_info::TextureType;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Precision {
    F16,
    F32,
}

#[derive(Debug, Clone, Copy)]
struct Encoding {
    precision: Precision,
    encoding: u32,
    dest_reg_b: u32,
    dest_reg_a: u32,
    src_reg_a: u32,
    src_reg_b: u32,
    cbuf_offset: u32,
    swizzle: u32,
}

impl Encoding {
    fn new(insn: u64) -> Self {
        Self {
            precision: if field(insn, 59, 1) == 0 {
                Precision::F16
            } else {
                Precision::F32
            },
            encoding: field(insn, 53, 4),
            dest_reg_b: field(insn, 28, 8),
            dest_reg_a: field(insn, 0, 8),
            src_reg_a: field(insn, 8, 8),
            src_reg_b: field(insn, 20, 8),
            cbuf_offset: field(insn, 36, 13),
            swizzle: field(insn, 50, 3),
        }
    }
}

const R: u32 = 1;
const G: u32 = 2;
const B: u32 = 4;
const A: u32 = 8;
const RG_LUT: [u32; 8] = [R, G, B, A, R | G, R | A, G | A, B | A];
const RGBA_LUT: [u32; 5] = [R | G | B, R | G | A, R | B | A, G | B | A, R | G | B | A];

fn is_aligned(reg: u32, alignment: u32) -> bool {
    reg == 255 || reg % alignment == 0
}

fn check_alignment(reg: u32, alignment: u32) {
    assert!(
        is_aligned(reg, alignment),
        "Unaligned source register {}",
        reg
    );
}

fn add_reg(reg: u32, offset: u32) -> u32 {
    if reg == 255 {
        255
    } else {
        reg + offset
    }
}

fn composite2(v: &mut TranslatorVisitor, reg_a: u32, reg_b: u32) -> Value {
    let a = v.f(reg_a);
    let b = v.f(reg_b);
    v.ir.composite_construct_f32x2(a, b)
}

fn composite3(v: &mut TranslatorVisitor, reg_a: u32, reg_b: u32, reg_c: u32) -> Value {
    let a = v.f(reg_a);
    let b = v.f(reg_b);
    let c = v.f(reg_c);
    v.ir.composite_construct_f32x3(a, b, c)
}

fn read_array(v: &mut TranslatorVisitor, value: Value) -> Value {
    let extracted = v
        .ir
        .bit_field_u_extract(value, Value::ImmU32(0), Value::ImmU32(16));
    v.ir.convert_f32_from_u32(extracted)
}

struct Sample {
    value: Value,
    is_shadow: bool,
}

fn sample(v: &mut TranslatorVisitor, insn: u64) -> Sample {
    let texs = Encoding::new(insn);
    let handle = Value::ImmU32(texs.cbuf_offset * 4);
    let zero = Value::ImmF32(0.0);
    let reg_a = texs.src_reg_a;
    let reg_b = texs.src_reg_b;
    let mut info = TextureInstInfo::default();

    let (value, is_shadow) = match texs.encoding {
        0 => {
            info.texture_type = TextureType::Color1D as u8;
            let coords = v.f(reg_a);
            (
                v.ir.image_sample_explicit_lod(handle, coords, zero, info.to_u32()),
                false,
            )
        }
        1 => {
            info.texture_type = TextureType::Color2D as u8;
            let coords = composite2(v, reg_a, reg_b);
            (v.ir.image_sample_implicit_lod(handle, coords, info.to_u32()), false)
        }
        2 => {
            info.texture_type = TextureType::Color2D as u8;
            let coords = composite2(v, reg_a, reg_b);
            (
                v.ir.image_sample_explicit_lod(handle, coords, zero, info.to_u32()),
                false,
            )
        }
        3 => {
            check_alignment(reg_a, 2);
            info.texture_type = TextureType::Color2D as u8;
            let coords = composite2(v, reg_a, add_reg(reg_a, 1));
            let lod = v.f(reg_b);
            (
                v.ir.image_sample_explicit_lod(handle, coords, lod, info.to_u32()),
                false,
            )
        }
        4 => {
            check_alignment(reg_a, 2);
            info.texture_type = TextureType::Color2D as u8;
            info.is_depth = true;
            let coords = composite2(v, reg_a, add_reg(reg_a, 1));
            let dref = v.f(reg_b);
            (
                v.ir.image_sample_dref_implicit_lod(handle, coords, dref, info.to_u32()),
                true,
            )
        }
        5 => {
            check_alignment(reg_a, 2);
            check_alignment(reg_b, 2);
            info.texture_type = TextureType::Color2D as u8;
            info.is_depth = true;
            let coords = composite2(v, reg_a, add_reg(reg_a, 1));
            let dref = v.f(add_reg(reg_b, 1));
            let lod = v.f(reg_b);
            (
                v.ir
                    .image_sample_dref_explicit_lod(handle, coords, dref, lod, info.to_u32()),
                true,
            )
        }
        6 => {
            check_alignment(reg_a, 2);
            info.texture_type = TextureType::Color2D as u8;
            info.is_depth = true;
            let coords = composite2(v, reg_a, add_reg(reg_a, 1));
            let dref = v.f(reg_b);
            (
                v.ir
                    .image_sample_dref_explicit_lod(handle, coords, dref, zero, info.to_u32()),
                true,
            )
        }
        7 => {
            check_alignment(reg_a, 2);
            info.texture_type = TextureType::ColorArray2D as u8;
            let y = v.f(add_reg(reg_a, 1));
            let z = v.f(reg_b);
            let raw_array = v.x(reg_a);
            let arr = read_array(v, raw_array);
            let coords = v.ir.composite_construct_f32x3(y, z, arr);
            (v.ir.image_sample_implicit_lod(handle, coords, info.to_u32()), false)
        }
        8 => {
            check_alignment(reg_a, 2);
            info.texture_type = TextureType::ColorArray2D as u8;
            let y = v.f(add_reg(reg_a, 1));
            let z = v.f(reg_b);
            let raw_array = v.x(reg_a);
            let arr = read_array(v, raw_array);
            let coords = v.ir.composite_construct_f32x3(y, z, arr);
            (
                v.ir.image_sample_explicit_lod(handle, coords, zero, info.to_u32()),
                false,
            )
        }
        9 => {
            check_alignment(reg_a, 2);
            check_alignment(reg_b, 2);
            info.texture_type = TextureType::ColorArray2D as u8;
            info.is_depth = true;
            let y = v.f(add_reg(reg_a, 1));
            let z = v.f(reg_b);
            let raw_array = v.x(reg_a);
            let arr = read_array(v, raw_array);
            let coords = v.ir.composite_construct_f32x3(y, z, arr);
            let dref = v.f(add_reg(reg_b, 1));
            (
                v.ir.image_sample_dref_explicit_lod(
                    handle,
                    coords,
                    dref,
                    zero,
                    info.to_u32(),
                ),
                true,
            )
        }
        10 => {
            check_alignment(reg_a, 2);
            info.texture_type = TextureType::Color3D as u8;
            let coords = composite3(v, reg_a, add_reg(reg_a, 1), reg_b);
            (v.ir.image_sample_implicit_lod(handle, coords, info.to_u32()), false)
        }
        11 => {
            check_alignment(reg_a, 2);
            info.texture_type = TextureType::Color3D as u8;
            let coords = composite3(v, reg_a, add_reg(reg_a, 1), reg_b);
            (
                v.ir.image_sample_explicit_lod(handle, coords, zero, info.to_u32()),
                false,
            )
        }
        12 => {
            check_alignment(reg_a, 2);
            info.texture_type = TextureType::ColorCube as u8;
            let coords = composite3(v, reg_a, add_reg(reg_a, 1), reg_b);
            (v.ir.image_sample_implicit_lod(handle, coords, info.to_u32()), false)
        }
        13 => {
            check_alignment(reg_a, 2);
            check_alignment(reg_b, 2);
            info.texture_type = TextureType::ColorCube as u8;
            let coords = composite3(v, reg_a, add_reg(reg_a, 1), reg_b);
            let lod = v.f(add_reg(reg_b, 1));
            (
                v.ir.image_sample_explicit_lod(handle, coords, lod, info.to_u32()),
                false,
            )
        }
        _ => panic!("Illegal TEXS encoding {}", texs.encoding),
    };
    Sample { value, is_shadow }
}

fn swizzle(insn: u64) -> u32 {
    let texs = Encoding::new(insn);
    let encoding = texs.swizzle as usize;
    if texs.dest_reg_b == 255 {
        *RG_LUT
            .get(encoding)
            .unwrap_or_else(|| panic!("Illegal TEXS RG encoding {}", encoding))
    } else {
        *RGBA_LUT
            .get(encoding)
            .unwrap_or_else(|| panic!("Illegal TEXS RGBA encoding {}", encoding))
    }
}

fn extract(v: &mut TranslatorVisitor, sample: &Sample, component: u32) -> Value {
    if sample.is_shadow {
        if component == 3 {
            Value::ImmF32(1.0)
        } else {
            sample.value
        }
    } else {
        v.ir.composite_extract_f32x4(sample.value, Value::ImmU32(component))
    }
}

fn reg_store_component32(insn: u64, index: u32) -> u32 {
    let texs = Encoding::new(insn);
    match index {
        0 => texs.dest_reg_a,
        1 => {
            check_alignment(texs.dest_reg_a, 2);
            add_reg(texs.dest_reg_a, 1)
        }
        2 => texs.dest_reg_b,
        3 => {
            check_alignment(texs.dest_reg_b, 2);
            add_reg(texs.dest_reg_b, 1)
        }
        _ => panic!("Invalid TEXS store index {}", index),
    }
}

fn store32(v: &mut TranslatorVisitor, insn: u64, sample: &Sample) {
    let swizzle = swizzle(insn);
    let mut store_index = 0;
    for component in 0..4 {
        if ((swizzle >> component) & 1) == 0 {
            continue;
        }
        let dest = reg_store_component32(insn, store_index);
        let value = extract(v, sample, component);
        v.set_f(dest, value);
        store_index += 1;
    }
}

fn pack(v: &mut TranslatorVisitor, lhs: Value, rhs: Value) -> Value {
    let pair = v.ir.composite_construct_f32x2(lhs, rhs);
    v.ir.pack_half_2x16(pair)
}

fn store16(v: &mut TranslatorVisitor, insn: u64, sample: &Sample) {
    let swizzle = swizzle(insn);
    let mut store_index = 0usize;
    let mut swizzled = [Value::Void; 4];
    for component in 0..4 {
        if ((swizzle >> component) & 1) == 0 {
            continue;
        }
        swizzled[store_index] = extract(v, sample, component);
        store_index += 1;
    }
    let zero = Value::ImmF32(0.0);
    let texs = Encoding::new(insn);
    match store_index {
        1 => {
            let packed = pack(v, swizzled[0], zero);
            v.set_x(texs.dest_reg_a, packed);
        }
        2 | 3 | 4 => {
            let packed_a = pack(v, swizzled[0], swizzled[1]);
            v.set_x(texs.dest_reg_a, packed_a);
            if store_index == 3 {
                let packed_b = pack(v, swizzled[2], zero);
                v.set_x(texs.dest_reg_b, packed_b);
            } else if store_index == 4 {
                let packed_b = pack(v, swizzled[2], swizzled[3]);
                v.set_x(texs.dest_reg_b, packed_b);
            }
        }
        _ => {}
    }
}

/// TEXS — Texture Fetch Swizzled.
///
/// Upstream: `TranslatorVisitor::TEXS(u64 insn)`
pub fn texs(tv: &mut TranslatorVisitor, insn: u64) {
    let sample = sample(tv, insn);
    if Encoding::new(insn).precision == Precision::F32 {
        store32(tv, insn, &sample);
    } else {
        store16(tv, insn, &sample);
    }
}
