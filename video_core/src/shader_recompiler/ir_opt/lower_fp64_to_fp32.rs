// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `ir_opt/lower_fp64_to_fp32.cpp`
//!
//! Lowers FP64 operations to FP32 equivalents for GPUs that do not
//! support native 64-bit floating-point operations. Uses manual bit
//! manipulation to emulate FP64 precision where needed.

use crate::shader_recompiler::ir::program::Program;

/// Lower all FP64 operations to FP32 equivalents.
pub fn lower_fp64_to_fp32(_program: &mut Program) {
    todo!("LowerFp64ToFp32: emulate FP64 ops with FP32 bit manipulation")
}
