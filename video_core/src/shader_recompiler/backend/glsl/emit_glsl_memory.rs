// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLSL memory operation emission (global, storage buffer).
//!
//! Maps to upstream `backend/glsl/emit_glsl_memory.cpp`.

use super::glsl_emit_context::EmitContext;

pub fn emit_load_storage_u8(ctx: &mut EmitContext, binding: &str, offset: &str) {
    ctx.add_fmt(format!("u_0=bitfieldExtract(ssbo{}[{}/4],int(({})%4)*8,8);", binding, offset, offset));
}
pub fn emit_load_storage_s8(ctx: &mut EmitContext, binding: &str, offset: &str) {
    ctx.add_fmt(format!("u_0=uint(bitfieldExtract(int(ssbo{}[{}/4]),int(({})%4)*8,8));", binding, offset, offset));
}
pub fn emit_load_storage_u16(ctx: &mut EmitContext, binding: &str, offset: &str) {
    ctx.add_fmt(format!("u_0=bitfieldExtract(ssbo{}[{}/4],int(({}/2)%2)*16,16);", binding, offset, offset));
}
pub fn emit_load_storage_s16(ctx: &mut EmitContext, binding: &str, offset: &str) {
    ctx.add_fmt(format!("u_0=uint(bitfieldExtract(int(ssbo{}[{}/4]),int(({}/2)%2)*16,16));", binding, offset, offset));
}
pub fn emit_load_storage_u32(ctx: &mut EmitContext, binding: &str, offset: &str) {
    ctx.add_fmt(format!("u_0=ssbo{}[{}/4];", binding, offset));
}
pub fn emit_load_storage_u64(ctx: &mut EmitContext, binding: &str, offset: &str) {
    ctx.add_fmt(format!("u2_0=uvec2(ssbo{}[{}/4],ssbo{}[{}/4+1]);", binding, offset, binding, offset));
}
pub fn emit_load_storage_u128(ctx: &mut EmitContext, binding: &str, offset: &str) {
    ctx.add_fmt(format!("u4_0=uvec4(ssbo{}[{}/4],ssbo{}[{}/4+1],ssbo{}[{}/4+2],ssbo{}[{}/4+3]);",
        binding, offset, binding, offset, binding, offset, binding, offset));
}
pub fn emit_write_storage_u8(ctx: &mut EmitContext, binding: &str, offset: &str) {
    ctx.add_fmt(format!("ssbo{}[{}/4]=bitfieldInsert(ssbo{}[{}/4],u_0,int(({})%4)*8,8);",
        binding, offset, binding, offset, offset));
}
pub fn emit_write_storage_u32(ctx: &mut EmitContext, binding: &str, offset: &str) {
    ctx.add_fmt(format!("ssbo{}[{}/4]=u_0;", binding, offset));
}
