// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLASM composite (vector construct/extract/insert) emission.
//!
//! Maps to upstream `backend/glasm/emit_glasm_composite.cpp`.

use super::glasm_emit_context::EmitContext;

const SWIZZLE: [char; 4] = ['x', 'y', 'z', 'w'];

/// Extract a component from a composite.
fn composite_extract(ctx: &mut EmitContext, index: u32, type_char: char) {
    let swz = SWIZZLE[index as usize];
    ctx.add_fmt(format!("MOV.{} RC.x,RC.{};", type_char, swz));
}

/// Insert a component into a composite.
fn composite_insert(ctx: &mut EmitContext, index: u32, type_char: char) {
    let swz = SWIZZLE[index as usize];
    ctx.add_fmt(format!("MOV.{} RC.{},RC.x;", type_char, swz));
}

// U32 composites
pub fn emit_composite_construct_u32x2(ctx: &mut EmitContext) {
    ctx.add_line("MOV.U RC,{0,0,0,0};");
}
pub fn emit_composite_construct_u32x3(ctx: &mut EmitContext) {
    ctx.add_line("MOV.U RC,{0,0,0,0};");
}
pub fn emit_composite_construct_u32x4(ctx: &mut EmitContext) {
    ctx.add_line("MOV.U RC,{0,0,0,0};");
}
pub fn emit_composite_extract_u32x2(ctx: &mut EmitContext, index: u32) {
    composite_extract(ctx, index, 'U');
}
pub fn emit_composite_extract_u32x3(ctx: &mut EmitContext, index: u32) {
    composite_extract(ctx, index, 'U');
}
pub fn emit_composite_extract_u32x4(ctx: &mut EmitContext, index: u32) {
    composite_extract(ctx, index, 'U');
}
pub fn emit_composite_insert_u32x2(_ctx: &mut EmitContext) {
    panic!("GLASM CompositeInsertU32x2 not implemented");
}
pub fn emit_composite_insert_u32x3(_ctx: &mut EmitContext) {
    panic!("GLASM CompositeInsertU32x3 not implemented");
}
pub fn emit_composite_insert_u32x4(_ctx: &mut EmitContext) {
    panic!("GLASM CompositeInsertU32x4 not implemented");
}

// F16 composites (not implemented in upstream)
pub fn emit_composite_construct_f16x2(_ctx: &mut EmitContext) {
    panic!("GLASM CompositeConstructF16x2 not implemented");
}
pub fn emit_composite_construct_f16x3(_ctx: &mut EmitContext) {
    panic!("GLASM CompositeConstructF16x3 not implemented");
}
pub fn emit_composite_construct_f16x4(_ctx: &mut EmitContext) {
    panic!("GLASM CompositeConstructF16x4 not implemented");
}
pub fn emit_composite_extract_f16x2(_ctx: &mut EmitContext) {
    panic!("GLASM CompositeExtractF16x2 not implemented");
}
pub fn emit_composite_extract_f16x3(_ctx: &mut EmitContext) {
    panic!("GLASM CompositeExtractF16x3 not implemented");
}
pub fn emit_composite_extract_f16x4(_ctx: &mut EmitContext) {
    panic!("GLASM CompositeExtractF16x4 not implemented");
}
pub fn emit_composite_insert_f16x2(_ctx: &mut EmitContext) {
    panic!("GLASM CompositeInsertF16x2 not implemented");
}
pub fn emit_composite_insert_f16x3(_ctx: &mut EmitContext) {
    panic!("GLASM CompositeInsertF16x3 not implemented");
}
pub fn emit_composite_insert_f16x4(_ctx: &mut EmitContext) {
    panic!("GLASM CompositeInsertF16x4 not implemented");
}

// F32 composites
pub fn emit_composite_construct_f32x2(ctx: &mut EmitContext) {
    ctx.add_line("MOV.F RC,{0,0,0,0};");
}
pub fn emit_composite_construct_f32x3(ctx: &mut EmitContext) {
    ctx.add_line("MOV.F RC,{0,0,0,0};");
}
pub fn emit_composite_construct_f32x4(ctx: &mut EmitContext) {
    ctx.add_line("MOV.F RC,{0,0,0,0};");
}
pub fn emit_composite_extract_f32x2(ctx: &mut EmitContext, index: u32) {
    composite_extract(ctx, index, 'F');
}
pub fn emit_composite_extract_f32x3(ctx: &mut EmitContext, index: u32) {
    composite_extract(ctx, index, 'F');
}
pub fn emit_composite_extract_f32x4(ctx: &mut EmitContext, index: u32) {
    composite_extract(ctx, index, 'F');
}
pub fn emit_composite_insert_f32x2(ctx: &mut EmitContext, index: u32) {
    composite_insert(ctx, index, 'F');
}
pub fn emit_composite_insert_f32x3(ctx: &mut EmitContext, index: u32) {
    composite_insert(ctx, index, 'F');
}
pub fn emit_composite_insert_f32x4(ctx: &mut EmitContext, index: u32) {
    composite_insert(ctx, index, 'F');
}

// F64 composites (not implemented in upstream)
pub fn emit_composite_construct_f64x2(_ctx: &mut EmitContext) {
    panic!("GLASM CompositeConstructF64x2 not implemented");
}
pub fn emit_composite_construct_f64x3(_ctx: &mut EmitContext) {
    panic!("GLASM CompositeConstructF64x3 not implemented");
}
pub fn emit_composite_construct_f64x4(_ctx: &mut EmitContext) {
    panic!("GLASM CompositeConstructF64x4 not implemented");
}
pub fn emit_composite_extract_f64x2(_ctx: &mut EmitContext) {
    panic!("GLASM CompositeExtractF64x2 not implemented");
}
pub fn emit_composite_extract_f64x3(_ctx: &mut EmitContext) {
    panic!("GLASM CompositeExtractF64x3 not implemented");
}
pub fn emit_composite_extract_f64x4(_ctx: &mut EmitContext) {
    panic!("GLASM CompositeExtractF64x4 not implemented");
}
pub fn emit_composite_insert_f64x2(_ctx: &mut EmitContext) {
    panic!("GLASM CompositeInsertF64x2 not implemented");
}
pub fn emit_composite_insert_f64x3(_ctx: &mut EmitContext) {
    panic!("GLASM CompositeInsertF64x3 not implemented");
}
pub fn emit_composite_insert_f64x4(_ctx: &mut EmitContext) {
    panic!("GLASM CompositeInsertF64x4 not implemented");
}
