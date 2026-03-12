// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of miscellaneous upstream translate files:
//! - `impl/not_implemented.cpp`
//! - `impl/output_geometry.cpp`
//! - `impl/pixel_load.cpp`
//! - `impl/attribute_memory_to_physical.cpp`
//! - `impl/internal_stage_buffer_entry_read.cpp`
//! - `impl/load_effective_address.cpp`

use super::TranslatorVisitor;

/// OUT - Geometry shader output vertex / primitive.
pub fn out(_v: &mut TranslatorVisitor<'_>, _insn: u64) {
    todo!("OUT: output geometry vertex/primitive")
}

/// PIXLD - Pixel load (fragment shader helper invocation info).
pub fn pixld(_v: &mut TranslatorVisitor<'_>, _insn: u64) {
    todo!("PIXLD: pixel load")
}

/// AL2P - Attribute memory to physical address.
pub fn al2p(_v: &mut TranslatorVisitor<'_>, _insn: u64) {
    todo!("AL2P: attribute to physical address")
}

/// ISBERD - Internal stage buffer entry read.
pub fn isberd(_v: &mut TranslatorVisitor<'_>, _insn: u64) {
    todo!("ISBERD: internal stage buffer entry read")
}

/// LEA - Load effective address.
pub fn lea(_v: &mut TranslatorVisitor<'_>, _insn: u64) {
    todo!("LEA: load effective address")
}
