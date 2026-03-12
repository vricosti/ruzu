// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLSL undefined value emission.
//!
//! Maps to upstream `backend/glsl/emit_glsl_undefined.cpp`.

use super::glsl_emit_context::EmitContext;

pub fn emit_undef_u1(ctx: &mut EmitContext) { ctx.add_line("b_0=false;"); }
pub fn emit_undef_u8(ctx: &mut EmitContext) { ctx.add_line("u_0=0u;"); }
pub fn emit_undef_u16(ctx: &mut EmitContext) { ctx.add_line("u_0=0u;"); }
pub fn emit_undef_u32(ctx: &mut EmitContext) { ctx.add_line("u_0=0u;"); }
pub fn emit_undef_u64(ctx: &mut EmitContext) { ctx.add_line("u64_0=0ul;"); }
