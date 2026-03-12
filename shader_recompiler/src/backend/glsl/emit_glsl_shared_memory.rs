// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLSL shared memory operation emission.
//!
//! Maps to upstream `backend/glsl/emit_glsl_shared_memory.cpp`.

use super::glsl_emit_context::EmitContext;

pub fn emit_load_shared_u8(ctx: &mut EmitContext, offset: &str) { ctx.add_fmt(format!("u_0=bitfieldExtract(smem[{}/4],int(({})%4)*8,8);", offset, offset)); }
pub fn emit_load_shared_s8(ctx: &mut EmitContext, offset: &str) { ctx.add_fmt(format!("u_0=uint(bitfieldExtract(int(smem[{}/4]),int(({})%4)*8,8));", offset, offset)); }
pub fn emit_load_shared_u16(ctx: &mut EmitContext, offset: &str) { ctx.add_fmt(format!("u_0=bitfieldExtract(smem[{}/4],int(({}/2)%2)*16,16);", offset, offset)); }
pub fn emit_load_shared_s16(ctx: &mut EmitContext, offset: &str) { ctx.add_fmt(format!("u_0=uint(bitfieldExtract(int(smem[{}/4]),int(({}/2)%2)*16,16));", offset, offset)); }
pub fn emit_load_shared_u32(ctx: &mut EmitContext, offset: &str) { ctx.add_fmt(format!("u_0=smem[{}/4];", offset)); }
pub fn emit_load_shared_u64(ctx: &mut EmitContext, offset: &str) { ctx.add_fmt(format!("u2_0=uvec2(smem[{}/4],smem[{}/4+1]);", offset, offset)); }
pub fn emit_load_shared_u128(ctx: &mut EmitContext, offset: &str) { ctx.add_fmt(format!("u4_0=uvec4(smem[{}/4],smem[{}/4+1],smem[{}/4+2],smem[{}/4+3]);", offset, offset, offset, offset)); }
pub fn emit_write_shared_u8(ctx: &mut EmitContext, offset: &str) { ctx.add_fmt(format!("smem[{}/4]=bitfieldInsert(smem[{}/4],u_0,int(({})%4)*8,8);", offset, offset, offset)); }
pub fn emit_write_shared_u16(ctx: &mut EmitContext, offset: &str) { ctx.add_fmt(format!("smem[{}/4]=bitfieldInsert(smem[{}/4],u_0,int(({}/2)%2)*16,16);", offset, offset, offset)); }
pub fn emit_write_shared_u32(ctx: &mut EmitContext, offset: &str) { ctx.add_fmt(format!("smem[{}/4]=u_0;", offset)); }
pub fn emit_write_shared_u64(ctx: &mut EmitContext, offset: &str) { ctx.add_fmt(format!("smem[{}/4]=u_0;smem[{}/4+1]=u_1;", offset, offset)); }
pub fn emit_write_shared_u128(ctx: &mut EmitContext, offset: &str) { ctx.add_fmt(format!("smem[{}/4]=u_0;smem[{}/4+1]=u_1;smem[{}/4+2]=u_2;smem[{}/4+3]=u_3;", offset, offset, offset, offset)); }
