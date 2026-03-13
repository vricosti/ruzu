// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `ir_opt/position_pass.cpp`
//!
//! Transforms position outputs when viewport transform state indicates
//! that the shader needs to apply render area scaling. Rewrites
//! PositionX/Y attribute stores to apply FMA with render area dimensions.

use crate::ir::program::Program;

/// Apply render area position transformation to position outputs.
///
/// Not yet implemented: requires identifying position attribute stores and
/// inserting FMA with render area dimensions.
pub fn position_pass(_program: &mut Program) {
    log::warn!("PositionPass not yet implemented — position outputs left unscaled");
}
