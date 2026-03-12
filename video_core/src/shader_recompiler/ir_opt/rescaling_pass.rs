// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `ir_opt/rescaling_pass.cpp`
//!
//! Applies resolution rescaling to texture coordinates when the game
//! renders at a different resolution than native. Identifies texture
//! sampling instructions and scales their coordinates by the rescaling
//! factor.

use crate::shader_recompiler::ir::program::Program;

/// Apply resolution rescaling to texture operations.
pub fn rescaling_pass(_program: &mut Program) {
    todo!("RescalingPass: insert rescaling factors for texture coordinates")
}
