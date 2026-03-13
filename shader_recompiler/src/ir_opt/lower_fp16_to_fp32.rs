// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `ir_opt/lower_fp16_to_fp32.cpp`
//!
//! Lowers FP16 operations to FP32 equivalents for GPUs that do not
//! support native 16-bit floating-point operations.

use crate::ir::program::Program;

/// Lower all FP16 operations to FP32 equivalents.
///
/// Not yet implemented: requires walking all IR instructions and replacing
/// FP16 opcodes with FP32 equivalents plus convert instructions.
pub fn lower_fp16_to_fp32(_program: &mut Program) {
    log::warn!("LowerFp16ToFp32 pass not yet implemented — FP16 ops left as-is");
}
