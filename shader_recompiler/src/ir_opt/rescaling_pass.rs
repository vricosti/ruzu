// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `ir_opt/rescaling_pass.cpp`
//!
//! Applies resolution rescaling to texture coordinates when the game
//! renders at a different resolution than native. Identifies texture
//! sampling instructions and scales their coordinates by the rescaling factor.

use crate::ir::program::Program;

/// Apply resolution rescaling to texture operations.
///
/// Not yet implemented: requires identifying texture sampling instructions
/// and inserting FMA rescaling for their coordinates.
pub fn rescaling_pass(_program: &mut Program) {
    log::warn!("RescalingPass not yet implemented — texture coordinates left unscaled");
}
