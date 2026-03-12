// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of upstream double-precision FP translate files:
//! - `impl/double_add.cpp`
//! - `impl/double_compare_and_set.cpp`
//! - `impl/double_fused_multiply_add.cpp`
//! - `impl/double_min_max.cpp`
//! - `impl/double_multiply.cpp`
//! - `impl/double_set_predicate.cpp`

use super::TranslatorVisitor;

/// DADD - Double-precision floating-point add.
pub fn dadd(_v: &mut TranslatorVisitor<'_>, _insn: u64) {
    todo!("DADD: double-precision floating-point add")
}

/// DSET - Double-precision floating-point compare and set.
pub fn dset(_v: &mut TranslatorVisitor<'_>, _insn: u64) {
    todo!("DSET: double-precision floating-point compare and set")
}

/// DFMA - Double-precision floating-point fused multiply-add.
pub fn dfma(_v: &mut TranslatorVisitor<'_>, _insn: u64) {
    todo!("DFMA: double-precision floating-point fused multiply-add")
}

/// DMNMX - Double-precision floating-point min/max.
pub fn dmnmx(_v: &mut TranslatorVisitor<'_>, _insn: u64) {
    todo!("DMNMX: double-precision floating-point min/max")
}

/// DMUL - Double-precision floating-point multiply.
pub fn dmul(_v: &mut TranslatorVisitor<'_>, _insn: u64) {
    todo!("DMUL: double-precision floating-point multiply")
}

/// DSETP - Double-precision floating-point set predicate.
pub fn dsetp(_v: &mut TranslatorVisitor<'_>, _insn: u64) {
    todo!("DSETP: double-precision floating-point set predicate")
}
