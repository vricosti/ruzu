// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `frontend/ir/patch.h` and `frontend/ir/patch.cpp`
//!
//! Tessellation patch attributes.

use std::fmt;

/// Tessellation patch attribute.
///
/// Components 0-119 map to generic patch outputs, while the first 6
/// are tessellation LOD factors.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u64)]
pub enum IrPatch {
    TessellationLodLeft = 0,
    TessellationLodTop = 1,
    TessellationLodRight = 2,
    TessellationLodBottom = 3,
    TessellationLodInteriorU = 4,
    TessellationLodInteriorV = 5,
    Component0 = 6,
    // Components 1-119 follow sequentially
}

/// First generic component index.
pub const PATCH_COMPONENT0: u64 = 6;
/// Last generic component index.
pub const PATCH_COMPONENT119: u64 = 125;

/// Construct a patch from a raw u64 value.
pub fn patch_from_u64(val: u64) -> u64 {
    val
}

/// Check if a patch value is a generic component.
pub fn is_generic(patch: u64) -> bool {
    patch >= PATCH_COMPONENT0 && patch <= PATCH_COMPONENT119
}

/// Get the generic patch index (0-29) for a generic patch component.
///
/// Panics if the patch is not generic.
pub fn generic_patch_index(patch: u64) -> u32 {
    assert!(is_generic(patch), "Patch {} is not generic", patch);
    ((patch - PATCH_COMPONENT0) / 4) as u32
}

/// Get the generic patch element (0-3) for a generic patch component.
///
/// Panics if the patch is not generic.
pub fn generic_patch_element(patch: u64) -> u32 {
    assert!(is_generic(patch), "Patch {} is not generic", patch);
    ((patch - PATCH_COMPONENT0) % 4) as u32
}

/// Display a patch value.
pub fn patch_name(patch: u64) -> String {
    match patch {
        0 => "TessLodLeft".to_string(),
        1 => "TessLodTop".to_string(),
        2 => "TessLodRight".to_string(),
        3 => "TessLodBottom".to_string(),
        4 => "TessLodInteriorU".to_string(),
        5 => "TessLodInteriorV".to_string(),
        v if is_generic(v) => {
            format!("Component{}", v - PATCH_COMPONENT0)
        }
        v => format!("Patch({})", v),
    }
}

impl fmt::Display for IrPatch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", patch_name(*self as u64))
    }
}
