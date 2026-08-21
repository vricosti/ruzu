// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GLASM bitwise conversion emission.
//!
//! Maps to upstream `backend/glasm/emit_glasm_bitwise_conversion.cpp`.
//!
//! In GLASM, registers are untyped, so bitcasts are identity operations.
//! Only pack/unpack operations emit actual instructions.

use super::glasm_emit_context::EmitContext;
use super::reg_alloc::Register;

/// Identity operation - no code emitted in GLASM (registers are untyped).
pub fn emit_identity(_ctx: &mut EmitContext) {
    // No-op: aliased in register allocator
}

/// BitCast U16 <-> F16 is a no-op in GLASM.
pub fn emit_bit_cast_u16_f16(_ctx: &mut EmitContext) {}

/// BitCast U32 <-> F32 is a no-op in GLASM.
pub fn emit_bit_cast_u32_f32(_ctx: &mut EmitContext) {}

/// BitCast U64 <-> F64 is a no-op in GLASM.
pub fn emit_bit_cast_u64_f64(_ctx: &mut EmitContext) {}

/// BitCast F16 <-> U16 is a no-op in GLASM.
pub fn emit_bit_cast_f16_u16(_ctx: &mut EmitContext) {}

/// BitCast F32 <-> U32 is a no-op in GLASM.
pub fn emit_bit_cast_f32_u32(_ctx: &mut EmitContext) {}

/// BitCast F64 <-> U64 is a no-op in GLASM.
pub fn emit_bit_cast_f64_u64(_ctx: &mut EmitContext) {}

/// Pack two u32 into one u64.
pub fn emit_pack_uint2x32(ctx: &mut EmitContext, _value: Register) {
    ctx.add_line("PK64.U RC.x,RC;");
}

/// Unpack one u64 into two u32.
pub fn emit_unpack_uint2x32(ctx: &mut EmitContext, _value: Register) {
    ctx.add_line("UP64.U RC.xy,RC.x;");
}

/// Pack two f16 into one u32.
pub fn emit_pack_half2x16(ctx: &mut EmitContext, _value: Register) {
    ctx.add_line("PK2H RC.x,RC;");
}

/// Unpack one u32 into two f16.
pub fn emit_unpack_half2x16(ctx: &mut EmitContext, _value: Register) {
    ctx.add_line("UP2H RC.xy,RC.x;");
}

/// Pack two f32 into one f64.
pub fn emit_pack_double2x32(ctx: &mut EmitContext, _value: Register) {
    ctx.add_line("PK64 RC.x,RC;");
}

/// Unpack one f64 into two f32.
pub fn emit_unpack_double2x32(ctx: &mut EmitContext, _value: Register) {
    ctx.add_line("UP64 RC.xy,RC.x;");
}

/// Pack float2x16 - not implemented in upstream.
pub fn emit_pack_float2x16(_ctx: &mut EmitContext, _value: Register) {
    panic!("GLASM instruction PackFloat2x16 not implemented");
}

/// Unpack float2x16 - not implemented in upstream.
pub fn emit_unpack_float2x16(_ctx: &mut EmitContext, _value: Register) {
    panic!("GLASM instruction UnpackFloat2x16 not implemented");
}
