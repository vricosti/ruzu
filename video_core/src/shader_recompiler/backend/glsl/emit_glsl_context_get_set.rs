// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLSL context get/set emission (constant buffers, attributes).
//!
//! Maps to upstream `backend/glsl/emit_glsl_context_get_set.cpp`.

use super::glsl_emit_context::EmitContext;

pub fn emit_get_cbuf_u8(ctx: &mut EmitContext, binding: u32, offset: &str) {
    ctx.add_fmt(format!("u_0=bitfieldExtract(cbuf{}[{}/4],int(({})%4)*8,8);", binding, offset, offset));
}
pub fn emit_get_cbuf_s8(ctx: &mut EmitContext, binding: u32, offset: &str) {
    ctx.add_fmt(format!("u_0=uint(bitfieldExtract(int(cbuf{}[{}/4]),int(({})%4)*8,8));", binding, offset, offset));
}
pub fn emit_get_cbuf_u16(ctx: &mut EmitContext, binding: u32, offset: &str) {
    ctx.add_fmt(format!("u_0=bitfieldExtract(cbuf{}[{}/4],int(({}/2)%2)*16,16);", binding, offset, offset));
}
pub fn emit_get_cbuf_s16(ctx: &mut EmitContext, binding: u32, offset: &str) {
    ctx.add_fmt(format!("u_0=uint(bitfieldExtract(int(cbuf{}[{}/4]),int(({}/2)%2)*16,16));", binding, offset, offset));
}
pub fn emit_get_cbuf_u32(ctx: &mut EmitContext, binding: u32, offset: &str) {
    ctx.add_fmt(format!("u_0=cbuf{}[{}/4];", binding, offset));
}
pub fn emit_get_cbuf_f32(ctx: &mut EmitContext, binding: u32, offset: &str) {
    ctx.add_fmt(format!("f_0=uintBitsToFloat(cbuf{}[{}/4]);", binding, offset));
}

pub fn emit_get_attribute(ctx: &mut EmitContext, attr_raw: u32, _vertex: &str) {
    let element = attr_raw % 4;
    let swizzle = ["x", "y", "z", "w"][element as usize];
    if attr_raw >= 32 && attr_raw < 160 {
        let index = (attr_raw - 32) / 4;
        ctx.add_fmt(format!("f_0=in_attr{}.{};", index, swizzle));
        return;
    }
    if attr_raw >= 28 && attr_raw < 32 {
        ctx.add_fmt(format!("f_0=gl_Position.{};", swizzle));
        return;
    }
    ctx.add_fmt(format!("// GetAttribute({}) not fully implemented", attr_raw));
}

pub fn emit_set_attribute(ctx: &mut EmitContext, attr_raw: u32, _value: &str) {
    let element = attr_raw % 4;
    let swizzle = ["x", "y", "z", "w"][element as usize];
    if attr_raw >= 32 && attr_raw < 160 {
        let index = (attr_raw - 32) / 4;
        ctx.add_fmt(format!("out_attr{}.{}=f_0;", index, swizzle));
        return;
    }
    if attr_raw >= 28 && attr_raw < 32 {
        ctx.add_fmt(format!("gl_Position.{}=f_0;", swizzle));
        return;
    }
    ctx.add_fmt(format!("// SetAttribute({}) not fully implemented", attr_raw));
}
