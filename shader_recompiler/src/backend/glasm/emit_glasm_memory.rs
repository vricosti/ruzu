// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLASM memory operation emission (global, storage buffer).
//!
//! Maps to upstream `backend/glasm/emit_glasm_memory.cpp`.

use super::glasm_emit_context::EmitContext;

/// Load global memory operations.
pub fn emit_load_global_u8(ctx: &mut EmitContext) {
    ctx.add_line("; LoadGlobalU8 (complex, requires SSBO binding)");
}
pub fn emit_load_global_s8(ctx: &mut EmitContext) {
    ctx.add_line("; LoadGlobalS8 (complex, requires SSBO binding)");
}
pub fn emit_load_global_u16(ctx: &mut EmitContext) {
    ctx.add_line("; LoadGlobalU16 (complex, requires SSBO binding)");
}
pub fn emit_load_global_s16(ctx: &mut EmitContext) {
    ctx.add_line("; LoadGlobalS16 (complex, requires SSBO binding)");
}
pub fn emit_load_global_u32(ctx: &mut EmitContext) {
    ctx.add_line("; LoadGlobalU32 (complex, requires SSBO binding)");
}
pub fn emit_load_global_u64(ctx: &mut EmitContext) {
    ctx.add_line("; LoadGlobalU64 (complex, requires SSBO binding)");
}
pub fn emit_load_global_u128(ctx: &mut EmitContext) {
    ctx.add_line("; LoadGlobalU128 (complex, requires SSBO binding)");
}

/// Write global memory operations.
pub fn emit_write_global_u8(ctx: &mut EmitContext) {
    ctx.add_line("; WriteGlobalU8 (complex, requires SSBO binding)");
}
pub fn emit_write_global_s8(ctx: &mut EmitContext) {
    ctx.add_line("; WriteGlobalS8 (complex, requires SSBO binding)");
}
pub fn emit_write_global_u16(ctx: &mut EmitContext) {
    ctx.add_line("; WriteGlobalU16 (complex, requires SSBO binding)");
}
pub fn emit_write_global_s16(ctx: &mut EmitContext) {
    ctx.add_line("; WriteGlobalS16 (complex, requires SSBO binding)");
}
pub fn emit_write_global_u32(ctx: &mut EmitContext) {
    ctx.add_line("; WriteGlobalU32 (complex, requires SSBO binding)");
}
pub fn emit_write_global_u64(ctx: &mut EmitContext) {
    ctx.add_line("; WriteGlobalU64 (complex, requires SSBO binding)");
}
pub fn emit_write_global_u128(ctx: &mut EmitContext) {
    ctx.add_line("; WriteGlobalU128 (complex, requires SSBO binding)");
}

/// Load storage buffer operations.
pub fn emit_load_storage_u8(ctx: &mut EmitContext, binding: u32, offset: u32) {
    ctx.add_fmt(format!("BUFFER.LOAD.U8 RC,ssbo{}[{}];", binding, offset));
}
pub fn emit_load_storage_s8(ctx: &mut EmitContext, binding: u32, offset: u32) {
    ctx.add_fmt(format!("BUFFER.LOAD.S8 RC,ssbo{}[{}];", binding, offset));
}
pub fn emit_load_storage_u16(ctx: &mut EmitContext, binding: u32, offset: u32) {
    ctx.add_fmt(format!("BUFFER.LOAD.U16 RC,ssbo{}[{}];", binding, offset));
}
pub fn emit_load_storage_s16(ctx: &mut EmitContext, binding: u32, offset: u32) {
    ctx.add_fmt(format!("BUFFER.LOAD.S16 RC,ssbo{}[{}];", binding, offset));
}
pub fn emit_load_storage_u32(ctx: &mut EmitContext, binding: u32, offset: u32) {
    ctx.add_fmt(format!("BUFFER.LOAD.U32 RC,ssbo{}[{}];", binding, offset));
}
pub fn emit_load_storage_u64(ctx: &mut EmitContext, binding: u32, offset: u32) {
    ctx.add_fmt(format!("BUFFER.LOAD.U32X2 RC,ssbo{}[{}];", binding, offset));
}
pub fn emit_load_storage_u128(ctx: &mut EmitContext, binding: u32, offset: u32) {
    ctx.add_fmt(format!("BUFFER.LOAD.U32X4 RC,ssbo{}[{}];", binding, offset));
}

/// Write storage buffer operations.
pub fn emit_write_storage_u8(ctx: &mut EmitContext, binding: u32, offset: u32) {
    ctx.add_fmt(format!("BUFFER.STORE.U8 RC,ssbo{}[{}];", binding, offset));
}
pub fn emit_write_storage_s8(ctx: &mut EmitContext, binding: u32, offset: u32) {
    ctx.add_fmt(format!("BUFFER.STORE.S8 RC,ssbo{}[{}];", binding, offset));
}
pub fn emit_write_storage_u16(ctx: &mut EmitContext, binding: u32, offset: u32) {
    ctx.add_fmt(format!("BUFFER.STORE.U16 RC,ssbo{}[{}];", binding, offset));
}
pub fn emit_write_storage_s16(ctx: &mut EmitContext, binding: u32, offset: u32) {
    ctx.add_fmt(format!("BUFFER.STORE.S16 RC,ssbo{}[{}];", binding, offset));
}
pub fn emit_write_storage_u32(ctx: &mut EmitContext, binding: u32, offset: u32) {
    ctx.add_fmt(format!("BUFFER.STORE.U32 RC,ssbo{}[{}];", binding, offset));
}
pub fn emit_write_storage_u64(ctx: &mut EmitContext, binding: u32, offset: u32) {
    ctx.add_fmt(format!(
        "BUFFER.STORE.U32X2 RC,ssbo{}[{}];",
        binding, offset
    ));
}
pub fn emit_write_storage_u128(ctx: &mut EmitContext, binding: u32, offset: u32) {
    ctx.add_fmt(format!(
        "BUFFER.STORE.U32X4 RC,ssbo{}[{}];",
        binding, offset
    ));
}
