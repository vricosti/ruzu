// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLSL atomic operation emission.
//!
//! Maps to upstream `backend/glsl/emit_glsl_atomic.cpp`.

use super::glsl_emit_context::EmitContext;

pub fn emit_storage_atomic_iadd32(ctx: &mut EmitContext, binding: &str, offset: &str) {
    ctx.add_fmt(format!("u_0=atomicAdd(ssbo{}[{}/4],u_1);", binding, offset));
}
pub fn emit_storage_atomic_smin32(ctx: &mut EmitContext, binding: &str, offset: &str) {
    ctx.add_fmt(format!("u_0=uint(atomicMin(ssbo_s{}[{}/4],int(u_1)));", binding, offset));
}
pub fn emit_storage_atomic_umin32(ctx: &mut EmitContext, binding: &str, offset: &str) {
    ctx.add_fmt(format!("u_0=atomicMin(ssbo{}[{}/4],u_1);", binding, offset));
}
pub fn emit_storage_atomic_smax32(ctx: &mut EmitContext, binding: &str, offset: &str) {
    ctx.add_fmt(format!("u_0=uint(atomicMax(ssbo_s{}[{}/4],int(u_1)));", binding, offset));
}
pub fn emit_storage_atomic_umax32(ctx: &mut EmitContext, binding: &str, offset: &str) {
    ctx.add_fmt(format!("u_0=atomicMax(ssbo{}[{}/4],u_1);", binding, offset));
}
pub fn emit_storage_atomic_and32(ctx: &mut EmitContext, binding: &str, offset: &str) {
    ctx.add_fmt(format!("u_0=atomicAnd(ssbo{}[{}/4],u_1);", binding, offset));
}
pub fn emit_storage_atomic_or32(ctx: &mut EmitContext, binding: &str, offset: &str) {
    ctx.add_fmt(format!("u_0=atomicOr(ssbo{}[{}/4],u_1);", binding, offset));
}
pub fn emit_storage_atomic_xor32(ctx: &mut EmitContext, binding: &str, offset: &str) {
    ctx.add_fmt(format!("u_0=atomicXor(ssbo{}[{}/4],u_1);", binding, offset));
}
pub fn emit_storage_atomic_exchange32(ctx: &mut EmitContext, binding: &str, offset: &str) {
    ctx.add_fmt(format!("u_0=atomicExchange(ssbo{}[{}/4],u_1);", binding, offset));
}

// Shared memory atomics
pub fn emit_shared_atomic_iadd32(ctx: &mut EmitContext, offset: &str) {
    ctx.add_fmt(format!("u_0=atomicAdd(smem[{}/4],u_1);", offset));
}
pub fn emit_shared_atomic_smin32(ctx: &mut EmitContext, offset: &str) {
    ctx.add_fmt(format!("u_0=uint(atomicMin(smem_s[{}/4],int(u_1)));", offset));
}
pub fn emit_shared_atomic_umin32(ctx: &mut EmitContext, offset: &str) {
    ctx.add_fmt(format!("u_0=atomicMin(smem[{}/4],u_1);", offset));
}
pub fn emit_shared_atomic_smax32(ctx: &mut EmitContext, offset: &str) {
    ctx.add_fmt(format!("u_0=uint(atomicMax(smem_s[{}/4],int(u_1)));", offset));
}
pub fn emit_shared_atomic_umax32(ctx: &mut EmitContext, offset: &str) {
    ctx.add_fmt(format!("u_0=atomicMax(smem[{}/4],u_1);", offset));
}
pub fn emit_shared_atomic_and32(ctx: &mut EmitContext, offset: &str) {
    ctx.add_fmt(format!("u_0=atomicAnd(smem[{}/4],u_1);", offset));
}
pub fn emit_shared_atomic_or32(ctx: &mut EmitContext, offset: &str) {
    ctx.add_fmt(format!("u_0=atomicOr(smem[{}/4],u_1);", offset));
}
pub fn emit_shared_atomic_xor32(ctx: &mut EmitContext, offset: &str) {
    ctx.add_fmt(format!("u_0=atomicXor(smem[{}/4],u_1);", offset));
}
pub fn emit_shared_atomic_exchange32(ctx: &mut EmitContext, offset: &str) {
    ctx.add_fmt(format!("u_0=atomicExchange(smem[{}/4],u_1);", offset));
}
