// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `backend/spirv/emit_spirv_context_get_set.cpp`
//!
//! SPIR-V emission for attribute get/set, constant buffer loads,
//! and other context-dependent operations.

use super::spirv_context::EmitContext;

/// Emit SPIR-V for getting a shader attribute.
pub fn emit_get_attribute(_ctx: &mut EmitContext) {
    todo!("EmitGetAttribute")
}

/// Emit SPIR-V for setting a shader attribute.
pub fn emit_set_attribute(_ctx: &mut EmitContext) {
    todo!("EmitSetAttribute")
}

/// Emit SPIR-V for getting a patch attribute.
pub fn emit_get_patch(_ctx: &mut EmitContext) {
    todo!("EmitGetPatch")
}

/// Emit SPIR-V for setting a patch attribute.
pub fn emit_set_patch(_ctx: &mut EmitContext) {
    todo!("EmitSetPatch")
}

/// Emit SPIR-V for loading from a constant buffer (U32).
pub fn emit_get_cbuf_u32(_ctx: &mut EmitContext) {
    todo!("EmitGetCbufU32")
}

/// Emit SPIR-V for loading from a constant buffer (F32).
pub fn emit_get_cbuf_f32(_ctx: &mut EmitContext) {
    todo!("EmitGetCbufF32")
}

/// Emit SPIR-V for loading from a constant buffer (U8).
pub fn emit_get_cbuf_u8(_ctx: &mut EmitContext) {
    todo!("EmitGetCbufU8")
}

/// Emit SPIR-V for loading from a constant buffer (S8).
pub fn emit_get_cbuf_s8(_ctx: &mut EmitContext) {
    todo!("EmitGetCbufS8")
}

/// Emit SPIR-V for loading from a constant buffer (U16).
pub fn emit_get_cbuf_u16(_ctx: &mut EmitContext) {
    todo!("EmitGetCbufU16")
}

/// Emit SPIR-V for loading from a constant buffer (S16).
pub fn emit_get_cbuf_s16(_ctx: &mut EmitContext) {
    todo!("EmitGetCbufS16")
}
