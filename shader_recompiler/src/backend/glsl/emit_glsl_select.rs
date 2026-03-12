// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLSL select (ternary) emission.
//!
//! Maps to upstream `backend/glsl/emit_glsl_select.cpp`.

use super::glsl_emit_context::EmitContext;

pub fn emit_select_u1(ctx: &mut EmitContext, cond: &str, true_val: &str, false_val: &str) {
    ctx.add_fmt(format!("b_0={}?{}:{};", cond, true_val, false_val));
}
pub fn emit_select_u8(_ctx: &mut EmitContext, _cond: &str, _true_val: &str, _false_val: &str) {
    panic!("GLSL SelectU8 not implemented");
}
pub fn emit_select_u16(_ctx: &mut EmitContext, _cond: &str, _true_val: &str, _false_val: &str) {
    panic!("GLSL SelectU16 not implemented");
}
pub fn emit_select_u32(ctx: &mut EmitContext, cond: &str, true_val: &str, false_val: &str) {
    ctx.add_fmt(format!("u_0={}?{}:{};", cond, true_val, false_val));
}
pub fn emit_select_u64(ctx: &mut EmitContext, cond: &str, true_val: &str, false_val: &str) {
    ctx.add_fmt(format!("u64_0={}?{}:{};", cond, true_val, false_val));
}
pub fn emit_select_f16(_ctx: &mut EmitContext, _cond: &str, _true_val: &str, _false_val: &str) {
    panic!("GLSL SelectF16 not implemented");
}
pub fn emit_select_f32(ctx: &mut EmitContext, cond: &str, true_val: &str, false_val: &str) {
    ctx.add_fmt(format!("f_0={}?{}:{};", cond, true_val, false_val));
}
pub fn emit_select_f64(ctx: &mut EmitContext, cond: &str, true_val: &str, false_val: &str) {
    ctx.add_fmt(format!("d_0={}?{}:{};", cond, true_val, false_val));
}
