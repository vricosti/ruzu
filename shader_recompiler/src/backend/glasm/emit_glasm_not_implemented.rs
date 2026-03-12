// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLASM not-implemented stubs.
//!
//! Maps to upstream `backend/glasm/emit_glasm_not_implemented.cpp`.
//!
//! These functions all panic matching upstream's `NotImplemented()` macro.

use super::glasm_emit_context::EmitContext;

pub fn emit_get_register(_ctx: &mut EmitContext) { panic!("GLASM instruction GetRegister not implemented"); }
pub fn emit_set_register(_ctx: &mut EmitContext) { panic!("GLASM instruction SetRegister not implemented"); }
pub fn emit_get_pred(_ctx: &mut EmitContext) { panic!("GLASM instruction GetPred not implemented"); }
pub fn emit_set_pred(_ctx: &mut EmitContext) { panic!("GLASM instruction SetPred not implemented"); }
pub fn emit_set_goto_variable(_ctx: &mut EmitContext) { panic!("GLASM instruction SetGotoVariable not implemented"); }
pub fn emit_get_goto_variable(_ctx: &mut EmitContext) { panic!("GLASM instruction GetGotoVariable not implemented"); }
pub fn emit_set_indirect_branch_variable(_ctx: &mut EmitContext) { panic!("GLASM instruction SetIndirectBranchVariable not implemented"); }
pub fn emit_get_indirect_branch_variable(_ctx: &mut EmitContext) { panic!("GLASM instruction GetIndirectBranchVariable not implemented"); }
pub fn emit_get_z_flag(_ctx: &mut EmitContext) { panic!("GLASM instruction GetZFlag not implemented"); }
pub fn emit_get_s_flag(_ctx: &mut EmitContext) { panic!("GLASM instruction GetSFlag not implemented"); }
pub fn emit_get_c_flag(_ctx: &mut EmitContext) { panic!("GLASM instruction GetCFlag not implemented"); }
pub fn emit_get_o_flag(_ctx: &mut EmitContext) { panic!("GLASM instruction GetOFlag not implemented"); }
pub fn emit_set_z_flag(_ctx: &mut EmitContext) { panic!("GLASM instruction SetZFlag not implemented"); }
pub fn emit_set_s_flag(_ctx: &mut EmitContext) { panic!("GLASM instruction SetSFlag not implemented"); }
pub fn emit_set_c_flag(_ctx: &mut EmitContext) { panic!("GLASM instruction SetCFlag not implemented"); }
pub fn emit_set_o_flag(_ctx: &mut EmitContext) { panic!("GLASM instruction SetOFlag not implemented"); }
pub fn emit_get_zero_from_op(_ctx: &mut EmitContext) { panic!("GLASM instruction GetZeroFromOp not implemented"); }
pub fn emit_get_sign_from_op(_ctx: &mut EmitContext) { panic!("GLASM instruction GetSignFromOp not implemented"); }
pub fn emit_get_carry_from_op(_ctx: &mut EmitContext) { panic!("GLASM instruction GetCarryFromOp not implemented"); }
pub fn emit_get_overflow_from_op(_ctx: &mut EmitContext) { panic!("GLASM instruction GetOverflowFromOp not implemented"); }
pub fn emit_get_sparse_from_op(_ctx: &mut EmitContext) { panic!("GLASM instruction GetSparseFromOp not implemented"); }
pub fn emit_get_in_bounds_from_op(_ctx: &mut EmitContext) { panic!("GLASM instruction GetInBoundsFromOp not implemented"); }
