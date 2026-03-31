// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLASM shared memory operation emission.
//!
//! Maps to upstream `backend/glasm/emit_glasm_shared_memory.cpp`.

use super::glasm_emit_context::EmitContext;

pub fn emit_load_shared_u8(ctx: &mut EmitContext) {
    ctx.add_line("LDS.U8 RC,shared_mem[RC.x];");
}
pub fn emit_load_shared_s8(ctx: &mut EmitContext) {
    ctx.add_line("LDS.S8 RC,shared_mem[RC.x];");
}
pub fn emit_load_shared_u16(ctx: &mut EmitContext) {
    ctx.add_line("LDS.U16 RC,shared_mem[RC.x];");
}
pub fn emit_load_shared_s16(ctx: &mut EmitContext) {
    ctx.add_line("LDS.S16 RC,shared_mem[RC.x];");
}
pub fn emit_load_shared_u32(ctx: &mut EmitContext) {
    ctx.add_line("LDS.U32 RC,shared_mem[RC.x];");
}
pub fn emit_load_shared_u64(ctx: &mut EmitContext) {
    ctx.add_line("LDS.U32X2 RC,shared_mem[RC.x];");
}
pub fn emit_load_shared_u128(ctx: &mut EmitContext) {
    ctx.add_line("LDS.U32X4 RC,shared_mem[RC.x];");
}
pub fn emit_write_shared_u8(ctx: &mut EmitContext) {
    ctx.add_line("STS.U8 RC.y,shared_mem[RC.x];");
}
pub fn emit_write_shared_u16(ctx: &mut EmitContext) {
    ctx.add_line("STS.U16 RC.y,shared_mem[RC.x];");
}
pub fn emit_write_shared_u32(ctx: &mut EmitContext) {
    ctx.add_line("STS.U32 RC.y,shared_mem[RC.x];");
}
pub fn emit_write_shared_u64(ctx: &mut EmitContext) {
    ctx.add_line("STS.U32X2 RC,shared_mem[RC.x];");
}
pub fn emit_write_shared_u128(ctx: &mut EmitContext) {
    ctx.add_line("STS.U32X4 RC,shared_mem[RC.x];");
}
