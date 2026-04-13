// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `shader_recompiler/stage.h`
//!
//! Shader pipeline stages. Note: the existing `ShaderStage` in `ir/types.rs`
//! is a simplified version. This module provides the upstream-faithful `Stage`
//! enum that includes `VertexA` and `VertexB` as separate stages.

use std::fmt;

/// Shader stage matching upstream `Stage` enum exactly.
///
/// Upstream distinguishes VertexA and VertexB (dual vertex shaders),
/// while `ir::types::ShaderStage` merges them into a single `Vertex`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u32)]
pub enum Stage {
    VertexB = 0,
    TessellationControl = 1,
    TessellationEval = 2,
    Geometry = 3,
    Fragment = 4,
    Compute = 5,
    VertexA = 6,
}

/// Maximum number of stage types (excluding VertexA).
pub const MAX_STAGE_TYPES: u32 = 6;

impl Stage {
    /// Create a stage from a numeric index (0 = VertexB, ..., 4 = Fragment).
    pub fn from_index(index: usize) -> Self {
        match index {
            0 => Stage::VertexB,
            1 => Stage::TessellationControl,
            2 => Stage::TessellationEval,
            3 => Stage::Geometry,
            4 => Stage::Fragment,
            5 => Stage::Compute,
            _ => panic!("Invalid stage index: {}", index),
        }
    }
}

// Note: there used to be a `From<ir::types::ShaderStage> for Stage` impl
// here that translated the simplified 6-variant `ShaderStage` into the
// upstream 7-variant `Stage`. Both are now the same type
// (`ir::types::ShaderStage` is a re-export of `Stage`), so the conversion
// is the identity and the impl was removed.

impl fmt::Display for Stage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stage::VertexB => write!(f, "VertexB"),
            Stage::TessellationControl => write!(f, "TessellationControl"),
            Stage::TessellationEval => write!(f, "TessellationEval"),
            Stage::Geometry => write!(f, "Geometry"),
            Stage::Fragment => write!(f, "Fragment"),
            Stage::Compute => write!(f, "Compute"),
            Stage::VertexA => write!(f, "VertexA"),
        }
    }
}
