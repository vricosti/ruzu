// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `ir_opt/lower_fp16_to_fp32.cpp`
//!
//! Lowers FP16 operations to FP32 equivalents for GPUs that do not
//! support native 16-bit floating-point operations.

use crate::shader_recompiler::ir::program::Program;

/// Lower all FP16 operations to FP32 equivalents.
pub fn lower_fp16_to_fp32(_program: &mut Program) {
    todo!("LowerFp16ToFp32: replace FP16 opcodes with FP32 equivalents")
}
