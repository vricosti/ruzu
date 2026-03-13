// SPDX-FileCopyrightText: Copyright 2021 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `frontend/maxwell/translate_program.cpp`
//!
//! Top-level shader translation: takes a CFG and environment, translates
//! each block using the TranslatorVisitor, builds the structured control
//! flow AST, and returns an IR::Program.

use crate::ir::program::Program;

/// Translate a Maxwell shader program from CFG to IR.
///
/// Not yet implemented: requires full CFG construction, block translation loop,
/// and structured control-flow AST building.
pub fn translate_program(
    _instructions: &[u64],
    stage: crate::ir::types::ShaderStage,
) -> Program {
    log::warn!("TranslateProgram not yet implemented — returning empty program");
    Program::new(stage)
}

/// Merge dual vertex programs (VertexA + VertexB) into a single program.
///
/// Not yet implemented: requires dual-vertex-specific CFG merging and
/// `JoinTextureInfo`/`JoinStorageInfo` pass invocation.
pub fn merge_dual_vertex_programs(
    _vertex_a: &mut Program,
    _vertex_b: &mut Program,
) -> Program {
    log::warn!("MergeDualVertexPrograms not yet implemented — returning empty program");
    Program::new(crate::ir::types::ShaderStage::Vertex)
}

/// Convert legacy (fixed-function) varyings to generic attributes.
///
/// Not yet implemented: requires attribute mapping tables and instruction mutation.
pub fn convert_legacy_to_generic(
    _program: &mut Program,
    _runtime_info: &crate::runtime_info::RuntimeInfo,
) {
    log::warn!("ConvertLegacyToGeneric not yet implemented — attributes left unmapped");
}

/// Generate a passthrough geometry shader for layer emulation.
///
/// Not yet implemented: requires generating a full passthrough GS IR program.
pub fn generate_geometry_passthrough(
    _source_program: &mut Program,
) -> Program {
    log::warn!("GenerateGeometryPassthrough not yet implemented — returning empty program");
    Program::new(crate::ir::types::ShaderStage::Vertex)
}
