// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of upstream video instruction translate files:
//! - `impl/video_helper.h` and `.cpp`
//! - `impl/video_minimum_maximum.cpp`
//! - `impl/video_multiply_add.cpp`
//! - `impl/video_set_predicate.cpp`

use super::TranslatorVisitor;

/// VMNMX - Video minimum/maximum.
pub fn vmnmx(_v: &mut TranslatorVisitor<'_>, _insn: u64) {
    todo!("VMNMX: video minimum/maximum")
}

/// VMAD - Video multiply-add.
pub fn vmad(_v: &mut TranslatorVisitor<'_>, _insn: u64) {
    todo!("VMAD: video multiply-add")
}

/// VSETP - Video set predicate.
pub fn vsetp(_v: &mut TranslatorVisitor<'_>, _insn: u64) {
    todo!("VSETP: video set predicate")
}
