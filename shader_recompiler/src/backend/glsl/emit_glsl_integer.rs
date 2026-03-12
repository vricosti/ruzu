// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLSL integer operation emission.
//!
//! Maps to upstream `backend/glsl/emit_glsl_integer.cpp`.

use super::glsl_emit_context::EmitContext;

pub fn emit_iadd32(ctx: &mut EmitContext, a: &str, b: &str) { ctx.add_fmt(format!("u_0={}+{};", a, b)); }
pub fn emit_iadd64(ctx: &mut EmitContext, a: &str, b: &str) { ctx.add_fmt(format!("u64_0={}+{};", a, b)); }
pub fn emit_isub32(ctx: &mut EmitContext, a: &str, b: &str) { ctx.add_fmt(format!("u_0={}-{};", a, b)); }
pub fn emit_isub64(ctx: &mut EmitContext, a: &str, b: &str) { ctx.add_fmt(format!("u64_0={}-{};", a, b)); }
pub fn emit_imul32(ctx: &mut EmitContext, a: &str, b: &str) { ctx.add_fmt(format!("u_0={}*{};", a, b)); }
pub fn emit_sdiv32(ctx: &mut EmitContext, a: &str, b: &str) { ctx.add_fmt(format!("u_0=uint(int({})/int({}));", a, b)); }
pub fn emit_udiv32(ctx: &mut EmitContext, a: &str, b: &str) { ctx.add_fmt(format!("u_0={}/{};", a, b)); }
pub fn emit_ineg32(ctx: &mut EmitContext, v: &str) { ctx.add_fmt(format!("u_0=uint(-int({}));", v)); }
pub fn emit_ineg64(ctx: &mut EmitContext, v: &str) { ctx.add_fmt(format!("u64_0=uint64_t(-int64_t({}));", v)); }
pub fn emit_iabs32(ctx: &mut EmitContext, v: &str) { ctx.add_fmt(format!("u_0=uint(abs(int({})));", v)); }
pub fn emit_shift_left_logical32(ctx: &mut EmitContext, a: &str, b: &str) { ctx.add_fmt(format!("u_0={}<<{};", a, b)); }
pub fn emit_shift_left_logical64(ctx: &mut EmitContext, a: &str, b: &str) { ctx.add_fmt(format!("u64_0={}<<{};", a, b)); }
pub fn emit_shift_right_logical32(ctx: &mut EmitContext, a: &str, b: &str) { ctx.add_fmt(format!("u_0={}>>{};", a, b)); }
pub fn emit_shift_right_logical64(ctx: &mut EmitContext, a: &str, b: &str) { ctx.add_fmt(format!("u64_0={}>>{};", a, b)); }
pub fn emit_shift_right_arithmetic32(ctx: &mut EmitContext, a: &str, b: &str) { ctx.add_fmt(format!("u_0=uint(int({})>>int({}));", a, b)); }
pub fn emit_shift_right_arithmetic64(ctx: &mut EmitContext, a: &str, b: &str) { ctx.add_fmt(format!("u64_0=uint64_t(int64_t({})>>int64_t({}));", a, b)); }
pub fn emit_bitwise_and32(ctx: &mut EmitContext, a: &str, b: &str) { ctx.add_fmt(format!("u_0={}&{};", a, b)); }
pub fn emit_bitwise_or32(ctx: &mut EmitContext, a: &str, b: &str) { ctx.add_fmt(format!("u_0={}|{};", a, b)); }
pub fn emit_bitwise_xor32(ctx: &mut EmitContext, a: &str, b: &str) { ctx.add_fmt(format!("u_0={}^{};", a, b)); }
pub fn emit_bitwise_not32(ctx: &mut EmitContext, v: &str) { ctx.add_fmt(format!("u_0=~{};", v)); }
pub fn emit_bit_field_insert(ctx: &mut EmitContext, base: &str, insert: &str, offset: &str, count: &str) {
    ctx.add_fmt(format!("u_0=bitfieldInsert({},{},int({}),int({}));", base, insert, offset, count));
}
pub fn emit_bit_field_s_extract(ctx: &mut EmitContext, base: &str, offset: &str, count: &str) {
    ctx.add_fmt(format!("u_0=uint(bitfieldExtract(int({}),int({}),int({})));", base, offset, count));
}
pub fn emit_bit_field_u_extract(ctx: &mut EmitContext, base: &str, offset: &str, count: &str) {
    ctx.add_fmt(format!("u_0=bitfieldExtract({},int({}),int({}));", base, offset, count));
}
pub fn emit_bit_reverse32(ctx: &mut EmitContext, v: &str) { ctx.add_fmt(format!("u_0=bitfieldReverse({});", v)); }
pub fn emit_bit_count32(ctx: &mut EmitContext, v: &str) { ctx.add_fmt(format!("u_0=uint(bitCount({}));", v)); }
pub fn emit_find_s_msb32(ctx: &mut EmitContext, v: &str) { ctx.add_fmt(format!("u_0=uint(findMSB(int({})));", v)); }
pub fn emit_find_u_msb32(ctx: &mut EmitContext, v: &str) { ctx.add_fmt(format!("u_0=uint(findMSB({}));", v)); }
pub fn emit_s_min32(ctx: &mut EmitContext, a: &str, b: &str) { ctx.add_fmt(format!("u_0=uint(min(int({}),int({})));", a, b)); }
pub fn emit_u_min32(ctx: &mut EmitContext, a: &str, b: &str) { ctx.add_fmt(format!("u_0=min({},{});", a, b)); }
pub fn emit_s_max32(ctx: &mut EmitContext, a: &str, b: &str) { ctx.add_fmt(format!("u_0=uint(max(int({}),int({})));", a, b)); }
pub fn emit_u_max32(ctx: &mut EmitContext, a: &str, b: &str) { ctx.add_fmt(format!("u_0=max({},{});", a, b)); }
pub fn emit_s_clamp32(ctx: &mut EmitContext, v: &str, lo: &str, hi: &str) {
    ctx.add_fmt(format!("u_0=uint(clamp(int({}),int({}),int({})));", v, lo, hi));
}
pub fn emit_u_clamp32(ctx: &mut EmitContext, v: &str, lo: &str, hi: &str) {
    ctx.add_fmt(format!("u_0=clamp({},{},{});", v, lo, hi));
}
pub fn emit_s_less_than(ctx: &mut EmitContext, a: &str, b: &str) { ctx.add_fmt(format!("b_0=int({})<int({});", a, b)); }
pub fn emit_u_less_than(ctx: &mut EmitContext, a: &str, b: &str) { ctx.add_fmt(format!("b_0={}<{};", a, b)); }
pub fn emit_i_equal(ctx: &mut EmitContext, a: &str, b: &str) { ctx.add_fmt(format!("b_0={}=={};", a, b)); }
pub fn emit_s_less_than_equal(ctx: &mut EmitContext, a: &str, b: &str) { ctx.add_fmt(format!("b_0=int({})<=int({});", a, b)); }
pub fn emit_u_less_than_equal(ctx: &mut EmitContext, a: &str, b: &str) { ctx.add_fmt(format!("b_0={}<={};", a, b)); }
pub fn emit_s_greater_than(ctx: &mut EmitContext, a: &str, b: &str) { ctx.add_fmt(format!("b_0=int({})>int({});", a, b)); }
pub fn emit_u_greater_than(ctx: &mut EmitContext, a: &str, b: &str) { ctx.add_fmt(format!("b_0={}>{};", a, b)); }
pub fn emit_i_not_equal(ctx: &mut EmitContext, a: &str, b: &str) { ctx.add_fmt(format!("b_0={}!={};", a, b)); }
pub fn emit_s_greater_than_equal(ctx: &mut EmitContext, a: &str, b: &str) { ctx.add_fmt(format!("b_0=int({})>=int({});", a, b)); }
pub fn emit_u_greater_than_equal(ctx: &mut EmitContext, a: &str, b: &str) { ctx.add_fmt(format!("b_0={}>={};", a, b)); }
