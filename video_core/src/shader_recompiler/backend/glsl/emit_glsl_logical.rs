// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLSL logical operation emission.
//!
//! Maps to upstream `backend/glsl/emit_glsl_logical.cpp`.

use super::glsl_emit_context::EmitContext;

pub fn emit_logical_or(ctx: &mut EmitContext, a: &str, b: &str) {
    ctx.add_fmt(format!("b_0={}||{};", a, b));
}
pub fn emit_logical_and(ctx: &mut EmitContext, a: &str, b: &str) {
    ctx.add_fmt(format!("b_0={}&&{};", a, b));
}
pub fn emit_logical_xor(ctx: &mut EmitContext, a: &str, b: &str) {
    ctx.add_fmt(format!("b_0={}^^{};", a, b));
}
pub fn emit_logical_not(ctx: &mut EmitContext, value: &str) {
    ctx.add_fmt(format!("b_0=!{};", value));
}
