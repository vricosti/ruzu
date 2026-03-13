// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `ir_opt/lower_fp64_to_fp32.cpp`
//!
//! Lowers FP64 operations to FP32 equivalents for GPUs that do not
//! support native 64-bit floating-point operations.

use crate::ir::program::Program;

/// Lower all FP64 operations to FP32 equivalents.
///
/// Not yet implemented: requires walking all IR instructions and replacing
/// FP64 opcodes with FP32 approximations via bit manipulation.
pub fn lower_fp64_to_fp32(_program: &mut Program) {
    log::warn!("LowerFp64ToFp32 pass not yet implemented — FP64 ops left as-is");
}
