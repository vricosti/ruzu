// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLSL type conversion emission.
//!
//! Maps to upstream `backend/glsl/emit_glsl_convert.cpp`.

use super::glsl_emit_context::EmitContext;

pub fn emit_convert_s16_f16(ctx: &mut EmitContext, v: &str) {
    ctx.add_fmt(format!("u_0=uint(int({}));", v));
}
pub fn emit_convert_s16_f32(ctx: &mut EmitContext, v: &str) {
    ctx.add_fmt(format!("u_0=uint(int({}))&0xFFFFu;", v));
}
pub fn emit_convert_s16_f64(ctx: &mut EmitContext, v: &str) {
    ctx.add_fmt(format!("u_0=uint(int({}))&0xFFFFu;", v));
}
pub fn emit_convert_s32_f16(ctx: &mut EmitContext, v: &str) {
    ctx.add_fmt(format!("u_0=uint(int({}));", v));
}
pub fn emit_convert_s32_f32(ctx: &mut EmitContext, v: &str) {
    ctx.add_fmt(format!("u_0=uint(int({}));", v));
}
pub fn emit_convert_s32_f64(ctx: &mut EmitContext, v: &str) {
    ctx.add_fmt(format!("u_0=uint(int({}));", v));
}
pub fn emit_convert_s64_f16(ctx: &mut EmitContext, v: &str) {
    ctx.add_fmt(format!("u64_0=uint64_t(int64_t({}));", v));
}
pub fn emit_convert_s64_f32(ctx: &mut EmitContext, v: &str) {
    ctx.add_fmt(format!("u64_0=uint64_t(int64_t({}));", v));
}
pub fn emit_convert_s64_f64(ctx: &mut EmitContext, v: &str) {
    ctx.add_fmt(format!("u64_0=uint64_t(int64_t({}));", v));
}
pub fn emit_convert_u16_f16(ctx: &mut EmitContext, v: &str) {
    ctx.add_fmt(format!("u_0=uint({})&0xFFFFu;", v));
}
pub fn emit_convert_u16_f32(ctx: &mut EmitContext, v: &str) {
    ctx.add_fmt(format!("u_0=uint({})&0xFFFFu;", v));
}
pub fn emit_convert_u16_f64(ctx: &mut EmitContext, v: &str) {
    ctx.add_fmt(format!("u_0=uint({})&0xFFFFu;", v));
}
pub fn emit_convert_u32_f16(ctx: &mut EmitContext, v: &str) {
    ctx.add_fmt(format!("u_0=uint({});", v));
}
pub fn emit_convert_u32_f32(ctx: &mut EmitContext, v: &str) {
    ctx.add_fmt(format!("u_0=uint({});", v));
}
pub fn emit_convert_u32_f64(ctx: &mut EmitContext, v: &str) {
    ctx.add_fmt(format!("u_0=uint({});", v));
}
pub fn emit_convert_u64_f16(ctx: &mut EmitContext, v: &str) {
    ctx.add_fmt(format!("u64_0=uint64_t({});", v));
}
pub fn emit_convert_u64_f32(ctx: &mut EmitContext, v: &str) {
    ctx.add_fmt(format!("u64_0=uint64_t({});", v));
}
pub fn emit_convert_u64_f64(ctx: &mut EmitContext, v: &str) {
    ctx.add_fmt(format!("u64_0=uint64_t({});", v));
}
pub fn emit_convert_u64_u32(ctx: &mut EmitContext, v: &str) {
    ctx.add_fmt(format!("u64_0=uint64_t({});", v));
}
pub fn emit_convert_u32_u64(ctx: &mut EmitContext, v: &str) {
    ctx.add_fmt(format!("u_0=uint({});", v));
}
pub fn emit_convert_f16_f32(ctx: &mut EmitContext, v: &str) {
    ctx.add_fmt(format!("// F16<-F32 {}", v));
}
pub fn emit_convert_f32_f16(ctx: &mut EmitContext, v: &str) {
    ctx.add_fmt(format!("f_0=float({});", v));
}
pub fn emit_convert_f32_f64(ctx: &mut EmitContext, v: &str) {
    ctx.add_fmt(format!("f_0=float({});", v));
}
pub fn emit_convert_f64_f32(ctx: &mut EmitContext, v: &str) {
    ctx.add_fmt(format!("d_0=double({});", v));
}
pub fn emit_convert_f32_s32(ctx: &mut EmitContext, v: &str) {
    ctx.add_fmt(format!("f_0=float(int({}));", v));
}
pub fn emit_convert_f32_u32(ctx: &mut EmitContext, v: &str) {
    ctx.add_fmt(format!("f_0=float({});", v));
}
pub fn emit_convert_f64_s32(ctx: &mut EmitContext, v: &str) {
    ctx.add_fmt(format!("d_0=double(int({}));", v));
}
pub fn emit_convert_f64_u32(ctx: &mut EmitContext, v: &str) {
    ctx.add_fmt(format!("d_0=double({});", v));
}
