// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `frontend/maxwell/translate_program.h` and `translate_program.cpp`
//!
//! Top-level shader translation: takes a CFG and environment, translates
//! each block using the TranslatorVisitor, builds the structured control
//! flow AST, and returns an IR::Program.

use crate::shader_recompiler::ir::program::Program;

/// Translate a Maxwell shader program from CFG to IR.
///
/// This is the main entry point for shader translation. It:
/// 1. Creates basic blocks from the CFG
/// 2. Translates each Maxwell instruction to IR via TranslatorVisitor
/// 3. Builds the structured control flow AST
/// 4. Returns the complete IR::Program
pub fn translate_program(
    _instructions: &[u64],
    _stage: crate::shader_recompiler::ir::types::ShaderStage,
) -> Program {
    todo!("TranslateProgram: full CFG-based shader translation")
}

/// Merge dual vertex programs (VertexA + VertexB) into a single program.
pub fn merge_dual_vertex_programs(
    _vertex_a: &mut Program,
    _vertex_b: &mut Program,
) -> Program {
    todo!("MergeDualVertexPrograms: combine VertexA and VertexB")
}

/// Convert legacy (fixed-function) varyings to generic attributes.
pub fn convert_legacy_to_generic(
    _program: &mut Program,
    _runtime_info: &crate::shader_recompiler::runtime_info::RuntimeInfo,
) {
    todo!("ConvertLegacyToGeneric: remap fixed-function attributes to generics")
}

/// Generate a passthrough geometry shader for layer emulation.
pub fn generate_geometry_passthrough(
    _source_program: &mut Program,
) -> Program {
    todo!("GenerateGeometryPassthrough: create passthrough GS for layer emulation")
}
