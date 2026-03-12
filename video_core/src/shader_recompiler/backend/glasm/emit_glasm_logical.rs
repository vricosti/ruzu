// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLASM logical operation emission.
//!
//! Maps to upstream `backend/glasm/emit_glasm_logical.cpp`.

use super::glasm_emit_context::EmitContext;
use super::reg_alloc::ScalarS32;

/// Logical OR of two boolean values.
pub fn emit_logical_or(ctx: &mut EmitContext, _a: ScalarS32, _b: ScalarS32) {
    ctx.add_line("OR.S RC.x,RC.x,RC.y;");
}

/// Logical AND of two boolean values.
pub fn emit_logical_and(ctx: &mut EmitContext, _a: ScalarS32, _b: ScalarS32) {
    ctx.add_line("AND.S RC.x,RC.x,RC.y;");
}

/// Logical XOR of two boolean values.
pub fn emit_logical_xor(ctx: &mut EmitContext, _a: ScalarS32, _b: ScalarS32) {
    ctx.add_line("XOR.S RC.x,RC.x,RC.y;");
}

/// Logical NOT of a boolean value.
pub fn emit_logical_not(ctx: &mut EmitContext, _value: ScalarS32) {
    ctx.add_line("SEQ.S RC.x,RC.x,0;");
}
