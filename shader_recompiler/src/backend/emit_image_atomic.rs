// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `backend/spirv/emit_spirv_image_atomic.cpp`
//!
//! SPIR-V emission for atomic operations on images.

use super::spirv_context::EmitContext;

/// Emit SPIR-V for image atomic add.
pub fn emit_image_atomic_iadd32(_ctx: &mut EmitContext) {
    todo!("EmitImageAtomicIAdd32")
}

/// Emit SPIR-V for image atomic signed min.
pub fn emit_image_atomic_smin32(_ctx: &mut EmitContext) {
    todo!("EmitImageAtomicSMin32")
}

/// Emit SPIR-V for image atomic unsigned min.
pub fn emit_image_atomic_umin32(_ctx: &mut EmitContext) {
    todo!("EmitImageAtomicUMin32")
}

/// Emit SPIR-V for image atomic signed max.
pub fn emit_image_atomic_smax32(_ctx: &mut EmitContext) {
    todo!("EmitImageAtomicSMax32")
}

/// Emit SPIR-V for image atomic unsigned max.
pub fn emit_image_atomic_umax32(_ctx: &mut EmitContext) {
    todo!("EmitImageAtomicUMax32")
}

/// Emit SPIR-V for image atomic AND.
pub fn emit_image_atomic_and32(_ctx: &mut EmitContext) {
    todo!("EmitImageAtomicAnd32")
}

/// Emit SPIR-V for image atomic OR.
pub fn emit_image_atomic_or32(_ctx: &mut EmitContext) {
    todo!("EmitImageAtomicOr32")
}

/// Emit SPIR-V for image atomic XOR.
pub fn emit_image_atomic_xor32(_ctx: &mut EmitContext) {
    todo!("EmitImageAtomicXor32")
}

/// Emit SPIR-V for image atomic exchange.
pub fn emit_image_atomic_exchange32(_ctx: &mut EmitContext) {
    todo!("EmitImageAtomicExchange32")
}
