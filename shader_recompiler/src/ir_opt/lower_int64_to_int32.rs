// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `ir_opt/lower_int64_to_int32.cpp`
//!
//! Lowers 64-bit integer operations to pairs of 32-bit operations for
//! GPUs that do not support native 64-bit integers.

use crate::ir::program::Program;

/// Lower all 64-bit integer operations to 32-bit hi/lo pairs.
///
/// Not yet implemented: requires walking all IR instructions and splitting
/// 64-bit integer opcodes into 32-bit pairs.
pub fn lower_int64_to_int32(_program: &mut Program) {
    log::warn!("LowerInt64ToInt32 pass not yet implemented — 64-bit integer ops left as-is");
}
