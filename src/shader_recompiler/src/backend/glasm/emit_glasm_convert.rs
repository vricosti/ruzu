// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLASM type conversion emission.
//!
//! Maps to upstream `backend/glasm/emit_glasm_convert.cpp`.

use super::glasm_emit_context::EmitContext;

/// Helper to emit CVT instruction.
fn convert(ctx: &mut EmitContext, dest: &str, src: &str) {
    ctx.add_fmt(format!("CVT.{}.{} RC.x,RC.x;", dest, src));
}

pub fn emit_convert_s16_f16(ctx: &mut EmitContext) {
    convert(ctx, "S16", "F16");
}
pub fn emit_convert_s16_f32(ctx: &mut EmitContext) {
    convert(ctx, "S16", "F32");
}
pub fn emit_convert_s16_f64(ctx: &mut EmitContext) {
    convert(ctx, "S16", "F64");
}
pub fn emit_convert_s32_f16(ctx: &mut EmitContext) {
    convert(ctx, "S32", "F16");
}
pub fn emit_convert_s32_f32(ctx: &mut EmitContext) {
    convert(ctx, "S32", "F32");
}
pub fn emit_convert_s32_f64(ctx: &mut EmitContext) {
    convert(ctx, "S32", "F64");
}
pub fn emit_convert_s64_f16(ctx: &mut EmitContext) {
    convert(ctx, "S64", "F16");
}
pub fn emit_convert_s64_f32(ctx: &mut EmitContext) {
    convert(ctx, "S64", "F32");
}
pub fn emit_convert_s64_f64(ctx: &mut EmitContext) {
    convert(ctx, "S64", "F64");
}
pub fn emit_convert_u16_f16(ctx: &mut EmitContext) {
    convert(ctx, "U16", "F16");
}
pub fn emit_convert_u16_f32(ctx: &mut EmitContext) {
    convert(ctx, "U16", "F32");
}
pub fn emit_convert_u16_f64(ctx: &mut EmitContext) {
    convert(ctx, "U16", "F64");
}
pub fn emit_convert_u32_f16(ctx: &mut EmitContext) {
    convert(ctx, "U32", "F16");
}
pub fn emit_convert_u32_f32(ctx: &mut EmitContext) {
    convert(ctx, "U32", "F32");
}
pub fn emit_convert_u32_f64(ctx: &mut EmitContext) {
    convert(ctx, "U32", "F64");
}
pub fn emit_convert_u64_f16(ctx: &mut EmitContext) {
    convert(ctx, "U64", "F16");
}
pub fn emit_convert_u64_f32(ctx: &mut EmitContext) {
    convert(ctx, "U64", "F32");
}
pub fn emit_convert_u64_f64(ctx: &mut EmitContext) {
    convert(ctx, "U64", "F64");
}
pub fn emit_convert_u64_u32(ctx: &mut EmitContext) {
    convert(ctx, "U64", "U32");
}
pub fn emit_convert_u32_u64(ctx: &mut EmitContext) {
    convert(ctx, "U32", "U64");
}
pub fn emit_convert_f16_f32(ctx: &mut EmitContext) {
    convert(ctx, "F16", "F32");
}
pub fn emit_convert_f32_f16(ctx: &mut EmitContext) {
    convert(ctx, "F32", "F16");
}
pub fn emit_convert_f32_f64(ctx: &mut EmitContext) {
    convert(ctx, "F32", "F64");
}
pub fn emit_convert_f64_f32(ctx: &mut EmitContext) {
    convert(ctx, "F64", "F32");
}
pub fn emit_convert_f16_s8(ctx: &mut EmitContext) {
    convert(ctx, "F16", "S8");
}
pub fn emit_convert_f16_s16(ctx: &mut EmitContext) {
    convert(ctx, "F16", "S16");
}
pub fn emit_convert_f16_s32(ctx: &mut EmitContext) {
    convert(ctx, "F16", "S32");
}
pub fn emit_convert_f16_s64(ctx: &mut EmitContext) {
    convert(ctx, "F16", "S64");
}
pub fn emit_convert_f16_u8(ctx: &mut EmitContext) {
    convert(ctx, "F16", "U8");
}
pub fn emit_convert_f16_u16(ctx: &mut EmitContext) {
    convert(ctx, "F16", "U16");
}
pub fn emit_convert_f16_u32(ctx: &mut EmitContext) {
    convert(ctx, "F16", "U32");
}
pub fn emit_convert_f16_u64(ctx: &mut EmitContext) {
    convert(ctx, "F16", "U64");
}
pub fn emit_convert_f32_s8(ctx: &mut EmitContext) {
    convert(ctx, "F32", "S8");
}
pub fn emit_convert_f32_s16(ctx: &mut EmitContext) {
    convert(ctx, "F32", "S16");
}
pub fn emit_convert_f32_s32(ctx: &mut EmitContext) {
    convert(ctx, "F32", "S32");
}
pub fn emit_convert_f32_s64(ctx: &mut EmitContext) {
    convert(ctx, "F32", "S64");
}
pub fn emit_convert_f32_u8(ctx: &mut EmitContext) {
    convert(ctx, "F32", "U8");
}
pub fn emit_convert_f32_u16(ctx: &mut EmitContext) {
    convert(ctx, "F32", "U16");
}
pub fn emit_convert_f32_u32(ctx: &mut EmitContext) {
    convert(ctx, "F32", "U32");
}
pub fn emit_convert_f32_u64(ctx: &mut EmitContext) {
    convert(ctx, "F32", "U64");
}
pub fn emit_convert_f64_s8(ctx: &mut EmitContext) {
    convert(ctx, "F64", "S8");
}
pub fn emit_convert_f64_s16(ctx: &mut EmitContext) {
    convert(ctx, "F64", "S16");
}
pub fn emit_convert_f64_s32(ctx: &mut EmitContext) {
    convert(ctx, "F64", "S32");
}
pub fn emit_convert_f64_s64(ctx: &mut EmitContext) {
    convert(ctx, "F64", "S64");
}
pub fn emit_convert_f64_u8(ctx: &mut EmitContext) {
    convert(ctx, "F64", "U8");
}
pub fn emit_convert_f64_u16(ctx: &mut EmitContext) {
    convert(ctx, "F64", "U16");
}
pub fn emit_convert_f64_u32(ctx: &mut EmitContext) {
    convert(ctx, "F64", "U32");
}
pub fn emit_convert_f64_u64(ctx: &mut EmitContext) {
    convert(ctx, "F64", "U64");
}
