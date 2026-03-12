// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `ir_opt/position_pass.cpp`
//!
//! Transforms position outputs when viewport transform state indicates
//! that the shader needs to apply render area scaling. Rewrites
//! PositionX/Y attribute stores to apply FMA with render area dimensions.

use crate::shader_recompiler::ir::program::Program;

/// Apply render area position transformation to position outputs.
pub fn position_pass(_program: &mut Program) {
    todo!("PositionPass: apply render area scaling to position outputs")
}
