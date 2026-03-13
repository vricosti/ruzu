// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/shader_recompiler/frontend/maxwell/translate/impl/half_floating_point_helper.cpp
//!
//! Half-precision floating point helper functions: FmzMode conversion,
//! swizzle extraction, and merge result operations.

use super::TranslatorVisitor;
use crate::ir::value::Value;

/// Half-precision mode for FP operations.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HalfPrecision {
    None = 0,
    Ftz = 1,
    Fmz = 2,
}

impl HalfPrecision {
    pub fn from_u32(v: u32) -> Self {
        match v & 3 {
            1 => HalfPrecision::Ftz,
            2 => HalfPrecision::Fmz,
            _ => HalfPrecision::None,
        }
    }
}

/// FP16 swizzle modes.
///
/// Note: upstream enum values differ from field encoding:
/// H1_H0 = 0, F32 = 1, H0_H0 = 2, H1_H1 = 3 (from the bitfield order in .h).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Swizzle {
    /// H1_H0: low half → lhs, high half → rhs
    H1H0 = 0,
    /// F32: treat packed U32 as F32 (both lhs and rhs are same F32 value)
    F32 = 1,
    /// H0_H0: low half duplicated
    H0H0 = 2,
    /// H1_H1: high half duplicated
    H1H1 = 3,
}

impl Swizzle {
    pub fn from_u32(v: u32) -> Self {
        match v & 3 {
            1 => Swizzle::F32,
            2 => Swizzle::H0H0,
            3 => Swizzle::H1H1,
            _ => Swizzle::H1H0,
        }
    }
}

/// Merge modes for half-precision results.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Merge {
    H1H0 = 0,
    F32 = 1,
    MrgH0 = 2,
    MrgH1 = 3,
}

impl Merge {
    pub fn from_u32(v: u32) -> Self {
        match v & 3 {
            1 => Merge::F32,
            2 => Merge::MrgH0,
            3 => Merge::MrgH1,
            _ => Merge::H1H0,
        }
    }
}

/// Convert HalfPrecision to FmzMode (as u32 flags value).
///
/// Corresponds to `HalfPrecision2FmzMode` in upstream.
pub fn half_precision_to_fmz_mode(precision: HalfPrecision) -> u32 {
    match precision {
        HalfPrecision::None => 0, // FmzMode::None
        HalfPrecision::Ftz => 1,  // FmzMode::FTZ
        HalfPrecision::Fmz => 2,  // FmzMode::FMZ
    }
}

/// Extract two F16 (or F32) values from a packed U32 based on swizzle mode.
///
/// Returns (lhs, rhs). When Swizzle::F32 is selected, both values are F32.
/// Otherwise both are F16.
///
/// Corresponds to `Extract` in upstream.
pub fn extract(tv: &mut TranslatorVisitor, value: Value, swizzle: Swizzle) -> (Value, Value) {
    match swizzle {
        Swizzle::H1H0 => {
            // UnpackFloat2x16(value) → F16x2; element 0 = low half, element 1 = high half
            let vec = tv.ir.unpack_float_2x16(value);
            let lo = tv.ir.composite_extract_f16x2(vec.clone(), 0);
            let hi = tv.ir.composite_extract_f16x2(vec, 1);
            (lo, hi)
        }
        Swizzle::H0H0 => {
            let vec = tv.ir.unpack_float_2x16(value);
            let lo = tv.ir.composite_extract_f16x2(vec, 0);
            (lo.clone(), lo)
        }
        Swizzle::H1H1 => {
            let vec = tv.ir.unpack_float_2x16(value);
            let hi = tv.ir.composite_extract_f16x2(vec, 1);
            (hi.clone(), hi)
        }
        Swizzle::F32 => {
            // Treat packed bits as F32 — both lhs and rhs get the same F32
            let f32val = tv.ir.bit_cast_f32_u32(value);
            (f32val.clone(), f32val)
        }
    }
}

/// Apply FP16 abs/neg modifiers.
pub fn fp_abs_neg_16(tv: &mut TranslatorVisitor, value: Value, abs: bool, neg: bool) -> Value {
    tv.ir.fp_abs_neg_16(value, abs, neg)
}

/// Apply FP32 abs/neg modifiers.
pub fn fp_abs_neg_32(tv: &mut TranslatorVisitor, value: Value, abs: bool, neg: bool) -> Value {
    tv.ir.fp_abs_neg_32(value, abs, neg)
}

/// Merge two F16 (or F32 — already promoted) results back into a U32 register value.
///
/// When `lhs`/`rhs` are F32 (from a Swizzle::F32 extraction), convert back to F16 before packing.
///
/// Corresponds to `MergeResult` in upstream.
pub fn merge_result(
    tv: &mut TranslatorVisitor,
    dest_reg: u32,
    lhs: Value,
    rhs: Value,
    merge: Merge,
    lhs_is_f32: bool,
) -> Value {
    // If the values were promoted to F32 during computation, convert them back to F16 first.
    let (lhs16, rhs16) = if lhs_is_f32 {
        let l = tv.ir.convert_f16_from_f32(lhs);
        let r = tv.ir.convert_f16_from_f32(rhs);
        (l, r)
    } else {
        (lhs, rhs)
    };

    match merge {
        Merge::H1H0 => {
            // PackFloat2x16(CompositeConstruct(lhs16, rhs16))
            let vec = tv.ir.composite_construct_f16x2(lhs16, rhs16);
            tv.ir.pack_float_2x16(vec)
        }
        Merge::F32 => {
            // BitCast<U32>(FPConvert(32, lhs16))
            let f32val = tv.ir.convert_f32_from_f16(lhs16);
            tv.ir.bit_cast_u32_f32(f32val)
        }
        Merge::MrgH0 => {
            // Replace low half (element 0) of dest register with lhs16
            let dest_val = tv.x(dest_reg);
            let vec = tv.ir.unpack_float_2x16(dest_val);
            let new_vec = tv.ir.composite_insert_f16x2(vec, lhs16, 0);
            tv.ir.pack_float_2x16(new_vec)
        }
        Merge::MrgH1 => {
            // Replace high half (element 1) of dest register with rhs16
            let dest_val = tv.x(dest_reg);
            let vec = tv.ir.unpack_float_2x16(dest_val);
            let new_vec = tv.ir.composite_insert_f16x2(vec, rhs16, 1);
            tv.ir.pack_float_2x16(new_vec)
        }
    }
}
