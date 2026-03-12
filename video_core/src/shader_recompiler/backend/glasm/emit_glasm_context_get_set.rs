// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLASM context get/set emission (constant buffers, attributes).
//!
//! Maps to upstream `backend/glasm/emit_glasm_context_get_set.cpp`.

use super::glasm_emit_context::EmitContext;

/// Load from constant buffer with size suffix.
fn get_cbuf(ctx: &mut EmitContext, binding: u32, offset: u32, size: &str) {
    if offset >= 0x10000 {
        ctx.add_line("MOV.S RC,0;");
        return;
    }
    ctx.add_fmt(format!("LDC.{} RC,c{}[{}];", size, binding, offset));
}

pub fn emit_get_cbuf_u8(ctx: &mut EmitContext, binding: u32, offset: u32) {
    get_cbuf(ctx, binding, offset, "U8");
}

pub fn emit_get_cbuf_s8(ctx: &mut EmitContext, binding: u32, offset: u32) {
    get_cbuf(ctx, binding, offset, "S8");
}

pub fn emit_get_cbuf_u16(ctx: &mut EmitContext, binding: u32, offset: u32) {
    get_cbuf(ctx, binding, offset, "U16");
}

pub fn emit_get_cbuf_s16(ctx: &mut EmitContext, binding: u32, offset: u32) {
    get_cbuf(ctx, binding, offset, "S16");
}

pub fn emit_get_cbuf_u32(ctx: &mut EmitContext, binding: u32, offset: u32) {
    get_cbuf(ctx, binding, offset, "U32");
}

pub fn emit_get_cbuf_f32(ctx: &mut EmitContext, binding: u32, offset: u32) {
    get_cbuf(ctx, binding, offset, "F32");
}

pub fn emit_get_cbuf_u32x2(ctx: &mut EmitContext, binding: u32, offset: u32) {
    get_cbuf(ctx, binding, offset, "U32X2");
}

/// Read a shader input attribute.
pub fn emit_get_attribute(ctx: &mut EmitContext, attr_raw: u32, _vertex: u32) {
    let element = attr_raw % 4;
    let swizzle = ['x', 'y', 'z', 'w'][element as usize];
    // Generic attributes
    if attr_raw >= 32 && attr_raw < 160 {
        let index = (attr_raw - 32) / 4;
        ctx.add_fmt(format!("MOV.F RC.x,in_attr{}[0].{};", index, swizzle));
        return;
    }
    // Position
    if attr_raw >= 28 && attr_raw < 32 {
        ctx.add_fmt(format!("MOV.F RC.x,vertex.position.{};", swizzle));
        return;
    }
    ctx.add_fmt(format!("; GetAttribute({}) not fully implemented", attr_raw));
}

/// Write a shader output attribute.
pub fn emit_set_attribute(ctx: &mut EmitContext, attr_raw: u32, _value: u32) {
    let element = attr_raw % 4;
    let swizzle = ['x', 'y', 'z', 'w'][element as usize];
    if attr_raw >= 32 && attr_raw < 160 {
        let index = (attr_raw - 32) / 4;
        ctx.add_fmt(format!("MOV.F out_attr{}[0].{},RC.x;", index, swizzle));
        return;
    }
    if attr_raw >= 28 && attr_raw < 32 {
        ctx.add_fmt(format!("MOV.F result.position.{},RC.x;", swizzle));
        return;
    }
    ctx.add_fmt(format!("; SetAttribute({}) not fully implemented", attr_raw));
}
