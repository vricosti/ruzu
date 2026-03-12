// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `ir_opt/lower_int64_to_int32.cpp`
//!
//! Lowers 64-bit integer operations to pairs of 32-bit operations for
//! GPUs that do not support native 64-bit integers.

use crate::shader_recompiler::ir::program::Program;

/// Lower all 64-bit integer operations to 32-bit pairs.
pub fn lower_int64_to_int32(_program: &mut Program) {
    todo!("LowerInt64ToInt32: split 64-bit integer ops into 32-bit hi/lo pairs")
}
