// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `ir_opt/dual_vertex_pass.cpp`
//!
//! Dual vertex shader passes for merging VertexA and VertexB programs.
//! VertexA transform removes the Epilogue instruction.
//! VertexB transform removes the Prologue instruction.
//!
//! Note: `JoinTextureInfo` lives in `texture_pass.rs` and `JoinStorageInfo` lives in
//! `global_memory_to_storage_buffer_pass.rs` — matching upstream file ownership.

use crate::ir::opcodes::Opcode;
use crate::ir::program::Program;

/// Transform pass for VertexA: removes the Epilogue instruction.
///
/// Upstream: `VertexATransformPass` in `dual_vertex_pass.cpp`.
pub fn vertex_a_transform_pass(program: &mut Program) {
    for block in &mut program.blocks {
        for inst in &mut block.instructions {
            if inst.opcode == Opcode::Epilogue {
                inst.opcode = Opcode::Identity;
                inst.args.clear();
                return;
            }
        }
    }
}

/// Transform pass for VertexB: removes the Prologue instruction.
///
/// Upstream: `VertexBTransformPass` in `dual_vertex_pass.cpp`.
pub fn vertex_b_transform_pass(program: &mut Program) {
    for block in &mut program.blocks {
        for inst in &mut block.instructions {
            if inst.opcode == Opcode::Prologue {
                inst.opcode = Opcode::Identity;
                inst.args.clear();
                return;
            }
        }
    }
}
