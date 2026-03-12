// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLSL not-implemented stubs.
//!
//! Maps to upstream `backend/glsl/emit_glsl_not_implemented.cpp`.

use super::glsl_emit_context::EmitContext;

pub fn emit_get_register(_ctx: &mut EmitContext) { panic!("GLSL instruction GetRegister not implemented"); }
pub fn emit_set_register(_ctx: &mut EmitContext) { panic!("GLSL instruction SetRegister not implemented"); }
pub fn emit_get_pred(_ctx: &mut EmitContext) { panic!("GLSL instruction GetPred not implemented"); }
pub fn emit_set_pred(_ctx: &mut EmitContext) { panic!("GLSL instruction SetPred not implemented"); }
pub fn emit_set_goto_variable(_ctx: &mut EmitContext) { panic!("GLSL instruction SetGotoVariable not implemented"); }
pub fn emit_get_goto_variable(_ctx: &mut EmitContext) { panic!("GLSL instruction GetGotoVariable not implemented"); }
pub fn emit_set_indirect_branch_variable(_ctx: &mut EmitContext) { panic!("GLSL instruction SetIndirectBranchVariable not implemented"); }
pub fn emit_get_indirect_branch_variable(_ctx: &mut EmitContext) { panic!("GLSL instruction GetIndirectBranchVariable not implemented"); }
