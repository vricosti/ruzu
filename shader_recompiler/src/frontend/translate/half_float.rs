// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of upstream half-precision FP translate files:
//! - `impl/half_floating_point_add.cpp`
//! - `impl/half_floating_point_fused_multiply_add.cpp`
//! - `impl/half_floating_point_helper.h` and `.cpp`
//! - `impl/half_floating_point_multiply.cpp`
//! - `impl/half_floating_point_set.cpp`
//! - `impl/half_floating_point_set_predicate.cpp`

use super::TranslatorVisitor;

/// HADD2 - Half-precision floating-point add.
pub fn hadd2(_v: &mut TranslatorVisitor<'_>, _insn: u64) {
    todo!("HADD2: half-precision floating-point add")
}

/// HFMA2 - Half-precision floating-point fused multiply-add.
pub fn hfma2(_v: &mut TranslatorVisitor<'_>, _insn: u64) {
    todo!("HFMA2: half-precision floating-point fused multiply-add")
}

/// HMUL2 - Half-precision floating-point multiply.
pub fn hmul2(_v: &mut TranslatorVisitor<'_>, _insn: u64) {
    todo!("HMUL2: half-precision floating-point multiply")
}

/// HSET2 - Half-precision floating-point set.
pub fn hset2(_v: &mut TranslatorVisitor<'_>, _insn: u64) {
    todo!("HSET2: half-precision floating-point set")
}

/// HSETP2 - Half-precision floating-point set predicate.
pub fn hsetp2(_v: &mut TranslatorVisitor<'_>, _insn: u64) {
    todo!("HSETP2: half-precision floating-point set predicate")
}
