// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `frontend/maxwell/translate/impl/common_funcs.h` and `.cpp`
//!
//! Common helper functions used by multiple instruction translators.

use super::TranslatorVisitor;
use crate::ir::value::Value;

/// Apply FP saturation (clamp to [0.0, 1.0]).
pub fn apply_fp_saturate(v: &mut TranslatorVisitor<'_>, value: Value, saturate: bool) -> Value {
    if saturate {
        v.ir.fp_saturate_32(value)
    } else {
        value
    }
}

/// Apply sign based on a negate flag.
pub fn apply_fp_negate(v: &mut TranslatorVisitor<'_>, value: Value, negate: bool) -> Value {
    if negate {
        v.ir.fp_neg_32(value)
    } else {
        value
    }
}

/// Apply absolute value flag.
pub fn apply_fp_abs(v: &mut TranslatorVisitor<'_>, value: Value, abs: bool) -> Value {
    if abs {
        v.ir.fp_abs_32(value)
    } else {
        value
    }
}

/// Apply absolute value then negate.
pub fn apply_fp_abs_neg(
    v: &mut TranslatorVisitor<'_>,
    value: Value,
    abs: bool,
    neg: bool,
) -> Value {
    let value = apply_fp_abs(v, value, abs);
    apply_fp_negate(v, value, neg)
}

/// Apply integer negate.
pub fn apply_int_negate(v: &mut TranslatorVisitor<'_>, value: Value, negate: bool) -> Value {
    if negate {
        v.ir.ineg_32(value)
    } else {
        value
    }
}

/// Apply bitwise NOT.
pub fn apply_bitwise_not(v: &mut TranslatorVisitor<'_>, value: Value, invert: bool) -> Value {
    if invert {
        v.ir.bitwise_not_32(value)
    } else {
        value
    }
}
