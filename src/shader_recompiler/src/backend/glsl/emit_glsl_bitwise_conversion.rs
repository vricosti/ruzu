// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLSL bitwise conversion emission.
//!
//! Maps to upstream `backend/glsl/emit_glsl_bitwise_conversion.cpp`.

use super::glsl_emit_context::EmitContext;

pub fn emit_identity(_ctx: &mut EmitContext) {}
pub fn emit_bit_cast_u16_f16(_ctx: &mut EmitContext) {}
pub fn emit_bit_cast_u32_f32(ctx: &mut EmitContext) {
    ctx.add_line("u_0=floatBitsToUint(f_1);");
}
pub fn emit_bit_cast_u64_f64(ctx: &mut EmitContext) {
    ctx.add_line("u64_0=doubleBitsToUint64(d_1);");
}
pub fn emit_bit_cast_f16_u16(_ctx: &mut EmitContext) {}
pub fn emit_bit_cast_f32_u32(ctx: &mut EmitContext) {
    ctx.add_line("f_0=uintBitsToFloat(u_1);");
}
pub fn emit_bit_cast_f64_u64(ctx: &mut EmitContext) {
    ctx.add_line("d_0=uint64BitsToDouble(u64_1);");
}
pub fn emit_pack_uint2x32(ctx: &mut EmitContext) {
    ctx.add_line("u64_0=packUint2x32(u2_1);");
}
pub fn emit_unpack_uint2x32(ctx: &mut EmitContext) {
    ctx.add_line("u2_0=unpackUint2x32(u64_1);");
}
pub fn emit_pack_float2x16(_ctx: &mut EmitContext) {
    panic!("GLSL PackFloat2x16 not implemented");
}
pub fn emit_unpack_float2x16(_ctx: &mut EmitContext) {
    panic!("GLSL UnpackFloat2x16 not implemented");
}
pub fn emit_pack_half2x16(ctx: &mut EmitContext) {
    ctx.add_line("u_0=packHalf2x16(f2_1);");
}
pub fn emit_unpack_half2x16(ctx: &mut EmitContext) {
    ctx.add_line("f2_0=unpackHalf2x16(u_1);");
}
pub fn emit_pack_double2x32(ctx: &mut EmitContext) {
    ctx.add_line("d_0=packDouble2x32(u2_1);");
}
pub fn emit_unpack_double2x32(ctx: &mut EmitContext) {
    ctx.add_line("u2_0=unpackDouble2x32(d_1);");
}
