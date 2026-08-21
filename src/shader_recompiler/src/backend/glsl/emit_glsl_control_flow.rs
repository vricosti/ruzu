// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLSL control flow emission.
//!
//! Maps to upstream `backend/glsl/emit_glsl_control_flow.cpp`.

use super::glsl_emit_context::EmitContext;

pub fn emit_join(_ctx: &mut EmitContext) {
    panic!("Join shouldn't be emitted");
}
pub fn emit_demote_to_helper_invocation(ctx: &mut EmitContext) {
    ctx.add_line("discard;");
}
