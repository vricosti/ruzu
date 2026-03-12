// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLASM control flow emission.
//!
//! Maps to upstream `backend/glasm/emit_glasm_control_flow.cpp`.

use super::glasm_emit_context::EmitContext;

/// Join should never be emitted in GLASM.
pub fn emit_join(_ctx: &mut EmitContext) {
    panic!("Join shouldn't be emitted");
}

/// Demote to helper invocation (discard).
pub fn emit_demote_to_helper_invocation(ctx: &mut EmitContext) {
    ctx.add_line("KIL TR.x;");
}
