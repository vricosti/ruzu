// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLASM integer operation emission.
//!
//! Maps to upstream `backend/glasm/emit_glasm_integer.cpp`.

use super::glasm_emit_context::EmitContext;

pub fn emit_iadd32(ctx: &mut EmitContext) {
    ctx.add_line("ADD.S RC.x,RC.x,RC.y;");
}
pub fn emit_iadd64(ctx: &mut EmitContext) {
    ctx.add_line("ADD.S64 DC.x,DC.x,DC.y;");
}
pub fn emit_isub32(ctx: &mut EmitContext) {
    ctx.add_line("SUB.S RC.x,RC.x,RC.y;");
}
pub fn emit_isub64(ctx: &mut EmitContext) {
    ctx.add_line("SUB.S64 DC.x,DC.x,DC.y;");
}
pub fn emit_imul32(ctx: &mut EmitContext) {
    ctx.add_line("MUL.S RC.x,RC.x,RC.y;");
}
pub fn emit_sdiv32(ctx: &mut EmitContext) {
    ctx.add_line("DIV.S RC.x,RC.x,RC.y;");
}
pub fn emit_udiv32(ctx: &mut EmitContext) {
    ctx.add_line("DIV.U RC.x,RC.x,RC.y;");
}
pub fn emit_ineg32(ctx: &mut EmitContext) {
    ctx.add_line("MOV.S RC.x,-RC.x;");
}
pub fn emit_ineg64(ctx: &mut EmitContext) {
    ctx.add_line("MOV.S64 DC.x,-DC.x;");
}
pub fn emit_iabs32(ctx: &mut EmitContext) {
    ctx.add_line("ABS.S RC.x,RC.x;");
}
pub fn emit_shift_left_logical32(ctx: &mut EmitContext) {
    ctx.add_line("SHL.U RC.x,RC.x,RC.y;");
}
pub fn emit_shift_left_logical64(ctx: &mut EmitContext) {
    ctx.add_line("SHL.U64 DC.x,DC.x,RC.y;");
}
pub fn emit_shift_right_logical32(ctx: &mut EmitContext) {
    ctx.add_line("SHR.U RC.x,RC.x,RC.y;");
}
pub fn emit_shift_right_logical64(ctx: &mut EmitContext) {
    ctx.add_line("SHR.U64 DC.x,DC.x,RC.y;");
}
pub fn emit_shift_right_arithmetic32(ctx: &mut EmitContext) {
    ctx.add_line("SHR.S RC.x,RC.x,RC.y;");
}
pub fn emit_shift_right_arithmetic64(ctx: &mut EmitContext) {
    ctx.add_line("SHR.S64 DC.x,DC.x,RC.y;");
}
pub fn emit_bitwise_and32(ctx: &mut EmitContext) {
    ctx.add_line("AND.S RC.x,RC.x,RC.y;");
}
pub fn emit_bitwise_or32(ctx: &mut EmitContext) {
    ctx.add_line("OR.S RC.x,RC.x,RC.y;");
}
pub fn emit_bitwise_xor32(ctx: &mut EmitContext) {
    ctx.add_line("XOR.S RC.x,RC.x,RC.y;");
}
pub fn emit_bitwise_not32(ctx: &mut EmitContext) {
    ctx.add_line("NOT.S RC.x,RC.x;");
}
pub fn emit_bit_field_insert(ctx: &mut EmitContext) {
    ctx.add_line("BFI.S RC.x,RC,RC.y,RC.z;");
}
pub fn emit_bit_field_s_extract(ctx: &mut EmitContext) {
    ctx.add_line("BFE.S RC.x,RC,RC.y;");
}
pub fn emit_bit_field_u_extract(ctx: &mut EmitContext) {
    ctx.add_line("BFE.U RC.x,RC,RC.y;");
}
pub fn emit_bit_reverse32(ctx: &mut EmitContext) {
    ctx.add_line("BFR RC.x,RC.x;");
}
pub fn emit_bit_count32(ctx: &mut EmitContext) {
    ctx.add_line("BTC RC.x,RC.x;");
}
pub fn emit_find_s_msb32(ctx: &mut EmitContext) {
    ctx.add_line("BTFM.S RC.x,RC.x;");
}
pub fn emit_find_u_msb32(ctx: &mut EmitContext) {
    ctx.add_line("BTFM.U RC.x,RC.x;");
}
pub fn emit_s_min32(ctx: &mut EmitContext) {
    ctx.add_line("MIN.S RC.x,RC.x,RC.y;");
}
pub fn emit_u_min32(ctx: &mut EmitContext) {
    ctx.add_line("MIN.U RC.x,RC.x,RC.y;");
}
pub fn emit_s_max32(ctx: &mut EmitContext) {
    ctx.add_line("MAX.S RC.x,RC.x,RC.y;");
}
pub fn emit_u_max32(ctx: &mut EmitContext) {
    ctx.add_line("MAX.U RC.x,RC.x,RC.y;");
}
pub fn emit_s_less_than(ctx: &mut EmitContext) {
    ctx.add_line("SLT.S RC.x,RC.x,RC.y;");
}
pub fn emit_u_less_than(ctx: &mut EmitContext) {
    ctx.add_line("SLT.U RC.x,RC.x,RC.y;");
}
pub fn emit_i_equal(ctx: &mut EmitContext) {
    ctx.add_line("SEQ.S RC.x,RC.x,RC.y;");
}
pub fn emit_s_less_than_equal(ctx: &mut EmitContext) {
    ctx.add_line("SLE.S RC.x,RC.x,RC.y;");
}
pub fn emit_u_less_than_equal(ctx: &mut EmitContext) {
    ctx.add_line("SLE.U RC.x,RC.x,RC.y;");
}
pub fn emit_s_greater_than(ctx: &mut EmitContext) {
    ctx.add_line("SGT.S RC.x,RC.x,RC.y;");
}
pub fn emit_u_greater_than(ctx: &mut EmitContext) {
    ctx.add_line("SGT.U RC.x,RC.x,RC.y;");
}
pub fn emit_i_not_equal(ctx: &mut EmitContext) {
    ctx.add_line("SNE.U RC.x,RC.x,RC.y;");
}
pub fn emit_s_greater_than_equal(ctx: &mut EmitContext) {
    ctx.add_line("SGE.S RC.x,RC.x,RC.y;");
}
pub fn emit_u_greater_than_equal(ctx: &mut EmitContext) {
    ctx.add_line("SGE.U RC.x,RC.x,RC.y;");
}
pub fn emit_s_clamp32(ctx: &mut EmitContext) {
    ctx.add_line("MIN.S RC.x,RC.z,RC.x;");
    ctx.add_line("MAX.S RC.x,RC.x,RC.y;");
}
pub fn emit_u_clamp32(ctx: &mut EmitContext) {
    ctx.add_line("MIN.U RC.x,RC.z,RC.x;");
    ctx.add_line("MAX.U RC.x,RC.x,RC.y;");
}
