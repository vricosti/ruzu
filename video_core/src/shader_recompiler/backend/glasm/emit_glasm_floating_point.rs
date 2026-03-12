// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLASM floating-point operation emission.
//!
//! Maps to upstream `backend/glasm/emit_glasm_floating_point.cpp`.

use super::glasm_emit_context::EmitContext;

pub fn emit_fp_abs32(ctx: &mut EmitContext) { ctx.add_line("MOV.F RC.x,|RC.x|;"); }
pub fn emit_fp_abs64(ctx: &mut EmitContext) { ctx.add_line("MOV.F64 DC.x,|DC.x|;"); }
pub fn emit_fp_add32(ctx: &mut EmitContext) { ctx.add_line("ADD.F RC.x,RC.x,RC.y;"); }
pub fn emit_fp_add64(ctx: &mut EmitContext) { ctx.add_line("ADD.F64 DC.x,DC.x,DC.y;"); }
pub fn emit_fp_fma32(ctx: &mut EmitContext) { ctx.add_line("MAD.F RC.x,RC.x,RC.y,RC.z;"); }
pub fn emit_fp_fma64(ctx: &mut EmitContext) { ctx.add_line("MAD.F64 DC.x,DC.x,DC.y,DC.z;"); }
pub fn emit_fp_max32(ctx: &mut EmitContext) { ctx.add_line("MAX.F RC.x,RC.x,RC.y;"); }
pub fn emit_fp_max64(ctx: &mut EmitContext) { ctx.add_line("MAX.F64 DC.x,DC.x,DC.y;"); }
pub fn emit_fp_min32(ctx: &mut EmitContext) { ctx.add_line("MIN.F RC.x,RC.x,RC.y;"); }
pub fn emit_fp_min64(ctx: &mut EmitContext) { ctx.add_line("MIN.F64 DC.x,DC.x,DC.y;"); }
pub fn emit_fp_mul32(ctx: &mut EmitContext) { ctx.add_line("MUL.F RC.x,RC.x,RC.y;"); }
pub fn emit_fp_mul64(ctx: &mut EmitContext) { ctx.add_line("MUL.F64 DC.x,DC.x,DC.y;"); }
pub fn emit_fp_neg32(ctx: &mut EmitContext) { ctx.add_line("MOV.F RC.x,-RC.x;"); }
pub fn emit_fp_neg64(ctx: &mut EmitContext) { ctx.add_line("MOV.F64 DC.x,-DC.x;"); }
pub fn emit_fp_sin(ctx: &mut EmitContext) { ctx.add_line("SIN RC.x,RC.x;"); }
pub fn emit_fp_cos(ctx: &mut EmitContext) { ctx.add_line("COS RC.x,RC.x;"); }
pub fn emit_fp_exp2(ctx: &mut EmitContext) { ctx.add_line("EX2 RC.x,RC.x;"); }
pub fn emit_fp_log2(ctx: &mut EmitContext) { ctx.add_line("LG2 RC.x,RC.x;"); }
pub fn emit_fp_recip32(ctx: &mut EmitContext) { ctx.add_line("RCP RC.x,RC.x;"); }
pub fn emit_fp_recip_sqrt32(ctx: &mut EmitContext) { ctx.add_line("RSQ RC.x,RC.x;"); }
pub fn emit_fp_sqrt(ctx: &mut EmitContext) {
    ctx.add_line("RSQ RC.x,RC.x;");
    ctx.add_line("RCP RC.x,RC.x;");
}
pub fn emit_fp_saturate32(ctx: &mut EmitContext) { ctx.add_line("MOV.F.SAT RC.x,RC.x;"); }
pub fn emit_fp_clamp32(ctx: &mut EmitContext) {
    ctx.add_line("MAX.F RC.x,RC.y,RC.x;");
    ctx.add_line("MIN.F RC.x,RC.x,RC.z;");
}
pub fn emit_fp_clamp64(ctx: &mut EmitContext) {
    ctx.add_line("MAX.F64 DC.x,DC.y,DC.x;");
    ctx.add_line("MIN.F64 DC.x,DC.x,DC.z;");
}
pub fn emit_fp_round_even32(ctx: &mut EmitContext) { ctx.add_line("ROUND.F RC.x,RC.x;"); }
pub fn emit_fp_round_even64(ctx: &mut EmitContext) { ctx.add_line("ROUND.F64 DC.x,DC.x;"); }
pub fn emit_fp_floor32(ctx: &mut EmitContext) { ctx.add_line("FLR.F RC.x,RC.x;"); }
pub fn emit_fp_floor64(ctx: &mut EmitContext) { ctx.add_line("FLR.F64 DC.x,DC.x;"); }
pub fn emit_fp_ceil32(ctx: &mut EmitContext) { ctx.add_line("CEIL.F RC.x,RC.x;"); }
pub fn emit_fp_ceil64(ctx: &mut EmitContext) { ctx.add_line("CEIL.F64 DC.x,DC.x;"); }
pub fn emit_fp_trunc32(ctx: &mut EmitContext) { ctx.add_line("TRUNC.F RC.x,RC.x;"); }
pub fn emit_fp_trunc64(ctx: &mut EmitContext) { ctx.add_line("TRUNC.F64 DC.x,DC.x;"); }

// Comparisons
pub fn emit_fp_ord_equal32(ctx: &mut EmitContext) { ctx.add_line("SEQ.F RC.x,RC.x,RC.y;"); }
pub fn emit_fp_ord_equal64(ctx: &mut EmitContext) { ctx.add_line("SEQ.F64 RC.x,DC.x,DC.y;"); }
pub fn emit_fp_unord_equal32(ctx: &mut EmitContext) { ctx.add_line("SEQ.F RC.x,RC.x,RC.y;"); }
pub fn emit_fp_unord_equal64(ctx: &mut EmitContext) { ctx.add_line("SEQ.F64 RC.x,DC.x,DC.y;"); }
pub fn emit_fp_ord_not_equal32(ctx: &mut EmitContext) { ctx.add_line("SNE.F RC.x,RC.x,RC.y;"); }
pub fn emit_fp_ord_not_equal64(ctx: &mut EmitContext) { ctx.add_line("SNE.F64 RC.x,DC.x,DC.y;"); }
pub fn emit_fp_unord_not_equal32(ctx: &mut EmitContext) { ctx.add_line("SNE.F RC.x,RC.x,RC.y;"); }
pub fn emit_fp_unord_not_equal64(ctx: &mut EmitContext) { ctx.add_line("SNE.F64 RC.x,DC.x,DC.y;"); }
pub fn emit_fp_ord_less_than32(ctx: &mut EmitContext) { ctx.add_line("SLT.F RC.x,RC.x,RC.y;"); }
pub fn emit_fp_ord_less_than64(ctx: &mut EmitContext) { ctx.add_line("SLT.F64 RC.x,DC.x,DC.y;"); }
pub fn emit_fp_unord_less_than32(ctx: &mut EmitContext) { ctx.add_line("SLT.F RC.x,RC.x,RC.y;"); }
pub fn emit_fp_unord_less_than64(ctx: &mut EmitContext) { ctx.add_line("SLT.F64 RC.x,DC.x,DC.y;"); }
pub fn emit_fp_ord_greater_than32(ctx: &mut EmitContext) { ctx.add_line("SGT.F RC.x,RC.x,RC.y;"); }
pub fn emit_fp_ord_greater_than64(ctx: &mut EmitContext) { ctx.add_line("SGT.F64 RC.x,DC.x,DC.y;"); }
pub fn emit_fp_unord_greater_than32(ctx: &mut EmitContext) { ctx.add_line("SGT.F RC.x,RC.x,RC.y;"); }
pub fn emit_fp_unord_greater_than64(ctx: &mut EmitContext) { ctx.add_line("SGT.F64 RC.x,DC.x,DC.y;"); }
pub fn emit_fp_ord_less_than_equal32(ctx: &mut EmitContext) { ctx.add_line("SLE.F RC.x,RC.x,RC.y;"); }
pub fn emit_fp_ord_less_than_equal64(ctx: &mut EmitContext) { ctx.add_line("SLE.F64 RC.x,DC.x,DC.y;"); }
pub fn emit_fp_unord_less_than_equal32(ctx: &mut EmitContext) { ctx.add_line("SLE.F RC.x,RC.x,RC.y;"); }
pub fn emit_fp_unord_less_than_equal64(ctx: &mut EmitContext) { ctx.add_line("SLE.F64 RC.x,DC.x,DC.y;"); }
pub fn emit_fp_ord_greater_than_equal32(ctx: &mut EmitContext) { ctx.add_line("SGE.F RC.x,RC.x,RC.y;"); }
pub fn emit_fp_ord_greater_than_equal64(ctx: &mut EmitContext) { ctx.add_line("SGE.F64 RC.x,DC.x,DC.y;"); }
pub fn emit_fp_unord_greater_than_equal32(ctx: &mut EmitContext) { ctx.add_line("SGE.F RC.x,RC.x,RC.y;"); }
pub fn emit_fp_unord_greater_than_equal64(ctx: &mut EmitContext) { ctx.add_line("SGE.F64 RC.x,DC.x,DC.y;"); }
pub fn emit_fp_is_nan32(ctx: &mut EmitContext) { ctx.add_line("SNE.F RC.x,RC.x,RC.x;"); }
pub fn emit_fp_is_nan64(ctx: &mut EmitContext) { ctx.add_line("SNE.F64 RC.x,DC.x,DC.x;"); }
