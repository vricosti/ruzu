// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `backend/spirv/emit_spirv_bitwise_conversion.cpp`
//!
//! SPIR-V emission for bitwise type conversions (bitcast).

use super::spirv_context::EmitContext;

/// Emit SPIR-V bitcast from U16 to F16.
pub fn emit_bit_cast_u16_f16(_ctx: &mut EmitContext) {
    todo!("EmitBitCastU16F16")
}

/// Emit SPIR-V bitcast from F16 to U16.
pub fn emit_bit_cast_f16_u16(_ctx: &mut EmitContext) {
    todo!("EmitBitCastF16U16")
}

/// Emit SPIR-V bitcast from U32 to F32.
pub fn emit_bit_cast_u32_f32(_ctx: &mut EmitContext) {
    todo!("EmitBitCastU32F32")
}

/// Emit SPIR-V bitcast from F32 to U32.
pub fn emit_bit_cast_f32_u32(_ctx: &mut EmitContext) {
    todo!("EmitBitCastF32U32")
}

/// Emit SPIR-V bitcast from U64 to F64.
pub fn emit_bit_cast_u64_f64(_ctx: &mut EmitContext) {
    todo!("EmitBitCastU64F64")
}

/// Emit SPIR-V bitcast from F64 to U64.
pub fn emit_bit_cast_f64_u64(_ctx: &mut EmitContext) {
    todo!("EmitBitCastF64U64")
}

/// Emit SPIR-V pack u32x2 to u64.
pub fn emit_pack_uint2x32(_ctx: &mut EmitContext) {
    todo!("EmitPackUint2x32")
}

/// Emit SPIR-V unpack u64 to u32x2.
pub fn emit_unpack_uint2x32(_ctx: &mut EmitContext) {
    todo!("EmitUnpackUint2x32")
}

/// Emit SPIR-V pack f16x2 to u32.
pub fn emit_pack_float2x16(_ctx: &mut EmitContext) {
    todo!("EmitPackFloat2x16")
}

/// Emit SPIR-V unpack u32 to f16x2.
pub fn emit_unpack_float2x16(_ctx: &mut EmitContext) {
    todo!("EmitUnpackFloat2x16")
}

/// Emit SPIR-V pack half2x16.
pub fn emit_pack_half2x16(_ctx: &mut EmitContext) {
    todo!("EmitPackHalf2x16")
}

/// Emit SPIR-V unpack half2x16.
pub fn emit_unpack_half2x16(_ctx: &mut EmitContext) {
    todo!("EmitUnpackHalf2x16")
}

/// Emit SPIR-V pack double2x32.
pub fn emit_pack_double2x32(_ctx: &mut EmitContext) {
    todo!("EmitPackDouble2x32")
}

/// Emit SPIR-V unpack double2x32.
pub fn emit_unpack_double2x32(_ctx: &mut EmitContext) {
    todo!("EmitUnpackDouble2x32")
}
