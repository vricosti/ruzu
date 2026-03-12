// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `ir_opt/dual_vertex_pass.cpp`
//!
//! Dual vertex shader passes for merging VertexA and VertexB programs.
//! VertexA transform removes the Epilogue instruction.
//! VertexB transform removes the Prologue instruction.

use crate::shader_recompiler::ir::opcodes::Opcode;
use crate::shader_recompiler::ir::program::Program;

/// Transform pass for VertexA: removes the Epilogue instruction.
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

/// Join texture info from a source program into a base program.
pub fn join_texture_info(
    _base: &mut crate::shader_recompiler::ir::program::ShaderInfo,
    _source: &mut crate::shader_recompiler::ir::program::ShaderInfo,
) {
    todo!("JoinTextureInfo: merge texture descriptors from dual vertex programs")
}

/// Join storage buffer info from a source program into a base program.
pub fn join_storage_info(
    _base: &mut crate::shader_recompiler::ir::program::ShaderInfo,
    _source: &mut crate::shader_recompiler::ir::program::ShaderInfo,
) {
    todo!("JoinStorageInfo: merge storage descriptors from dual vertex programs")
}
