// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLSL composite (vector construct/extract/insert) emission.
//!
//! Maps to upstream `backend/glsl/emit_glsl_composite.cpp`.

use super::glsl_emit_context::EmitContext;

pub fn emit_composite_construct_u32x2(ctx: &mut EmitContext, e1: &str, e2: &str) {
    ctx.add_fmt(format!("u2_0=uvec2({},{});", e1, e2));
}
pub fn emit_composite_construct_u32x3(ctx: &mut EmitContext, e1: &str, e2: &str, e3: &str) {
    ctx.add_fmt(format!("u3_0=uvec3({},{},{});", e1, e2, e3));
}
pub fn emit_composite_construct_u32x4(
    ctx: &mut EmitContext,
    e1: &str,
    e2: &str,
    e3: &str,
    e4: &str,
) {
    ctx.add_fmt(format!("u4_0=uvec4({},{},{},{});", e1, e2, e3, e4));
}
pub fn emit_composite_extract_u32x2(ctx: &mut EmitContext, composite: &str, index: u32) {
    let swz = ["x", "y"][index as usize];
    ctx.add_fmt(format!("u_0={}.{};", composite, swz));
}
pub fn emit_composite_extract_u32x3(ctx: &mut EmitContext, composite: &str, index: u32) {
    let swz = ["x", "y", "z"][index as usize];
    ctx.add_fmt(format!("u_0={}.{};", composite, swz));
}
pub fn emit_composite_extract_u32x4(ctx: &mut EmitContext, composite: &str, index: u32) {
    let swz = ["x", "y", "z", "w"][index as usize];
    ctx.add_fmt(format!("u_0={}.{};", composite, swz));
}
pub fn emit_composite_construct_f32x2(ctx: &mut EmitContext, e1: &str, e2: &str) {
    ctx.add_fmt(format!("f2_0=vec2({},{});", e1, e2));
}
pub fn emit_composite_construct_f32x3(ctx: &mut EmitContext, e1: &str, e2: &str, e3: &str) {
    ctx.add_fmt(format!("f3_0=vec3({},{},{});", e1, e2, e3));
}
pub fn emit_composite_construct_f32x4(
    ctx: &mut EmitContext,
    e1: &str,
    e2: &str,
    e3: &str,
    e4: &str,
) {
    ctx.add_fmt(format!("f4_0=vec4({},{},{},{});", e1, e2, e3, e4));
}
pub fn emit_composite_extract_f32x2(ctx: &mut EmitContext, composite: &str, index: u32) {
    let swz = ["x", "y"][index as usize];
    ctx.add_fmt(format!("f_0={}.{};", composite, swz));
}
pub fn emit_composite_extract_f32x3(ctx: &mut EmitContext, composite: &str, index: u32) {
    let swz = ["x", "y", "z"][index as usize];
    ctx.add_fmt(format!("f_0={}.{};", composite, swz));
}
pub fn emit_composite_extract_f32x4(ctx: &mut EmitContext, composite: &str, index: u32) {
    let swz = ["x", "y", "z", "w"][index as usize];
    ctx.add_fmt(format!("f_0={}.{};", composite, swz));
}
pub fn emit_composite_insert_f32x2(
    ctx: &mut EmitContext,
    composite: &str,
    object: &str,
    index: u32,
) {
    let swz = ["x", "y"][index as usize];
    ctx.add_fmt(format!("f2_0={};f2_0.{}={};", composite, swz, object));
}
pub fn emit_composite_insert_f32x3(
    ctx: &mut EmitContext,
    composite: &str,
    object: &str,
    index: u32,
) {
    let swz = ["x", "y", "z"][index as usize];
    ctx.add_fmt(format!("f3_0={};f3_0.{}={};", composite, swz, object));
}
pub fn emit_composite_insert_f32x4(
    ctx: &mut EmitContext,
    composite: &str,
    object: &str,
    index: u32,
) {
    let swz = ["x", "y", "z", "w"][index as usize];
    ctx.add_fmt(format!("f4_0={};f4_0.{}={};", composite, swz, object));
}
