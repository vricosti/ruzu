// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLASM select (conditional move) emission.
//!
//! Maps to upstream `backend/glasm/emit_glasm_select.cpp`.

use super::glasm_emit_context::EmitContext;
use super::reg_alloc::ScalarS32;

/// Select U1 based on condition.
pub fn emit_select_u1(
    ctx: &mut EmitContext,
    _cond: ScalarS32,
    _true_val: ScalarS32,
    _false_val: ScalarS32,
) {
    ctx.add_line("CMP.S RC.x,RC.x,RC.y,RC.z;");
}

/// Select U8 - not implemented in upstream.
pub fn emit_select_u8(
    _ctx: &mut EmitContext,
    _cond: ScalarS32,
    _true_val: ScalarS32,
    _false_val: ScalarS32,
) {
    panic!("GLASM instruction SelectU8 not implemented");
}

/// Select U16 - not implemented in upstream.
pub fn emit_select_u16(
    _ctx: &mut EmitContext,
    _cond: ScalarS32,
    _true_val: ScalarS32,
    _false_val: ScalarS32,
) {
    panic!("GLASM instruction SelectU16 not implemented");
}

/// Select U32 based on condition.
pub fn emit_select_u32(
    ctx: &mut EmitContext,
    _cond: ScalarS32,
    _true_val: ScalarS32,
    _false_val: ScalarS32,
) {
    ctx.add_line("CMP.S RC.x,RC.x,RC.y,RC.z;");
}

/// Select F32 based on condition.
pub fn emit_select_f32(
    ctx: &mut EmitContext,
    _cond: ScalarS32,
    _true_val: ScalarS32,
    _false_val: ScalarS32,
) {
    ctx.add_line("CMP.S RC.x,RC.x,RC.y,RC.z;");
}

/// Select F16 - not implemented in upstream.
pub fn emit_select_f16(_ctx: &mut EmitContext) {
    panic!("GLASM instruction SelectF16 not implemented");
}

/// Select F64 - not implemented in upstream.
pub fn emit_select_f64(_ctx: &mut EmitContext) {
    panic!("GLASM instruction SelectF64 not implemented");
}
