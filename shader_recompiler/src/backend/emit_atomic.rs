// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `backend/spirv/emit_spirv_atomic.cpp`
//!
//! SPIR-V emission for atomic operations on storage buffers and global memory.

use super::spirv_context::EmitContext;

/// Emit SPIR-V for storage buffer atomic add.
pub fn emit_storage_atomic_iadd32(_ctx: &mut EmitContext) {
    todo!("EmitStorageAtomicIAdd32")
}

/// Emit SPIR-V for storage buffer atomic signed min.
pub fn emit_storage_atomic_smin32(_ctx: &mut EmitContext) {
    todo!("EmitStorageAtomicSMin32")
}

/// Emit SPIR-V for storage buffer atomic unsigned min.
pub fn emit_storage_atomic_umin32(_ctx: &mut EmitContext) {
    todo!("EmitStorageAtomicUMin32")
}

/// Emit SPIR-V for storage buffer atomic signed max.
pub fn emit_storage_atomic_smax32(_ctx: &mut EmitContext) {
    todo!("EmitStorageAtomicSMax32")
}

/// Emit SPIR-V for storage buffer atomic unsigned max.
pub fn emit_storage_atomic_umax32(_ctx: &mut EmitContext) {
    todo!("EmitStorageAtomicUMax32")
}

/// Emit SPIR-V for storage buffer atomic AND.
pub fn emit_storage_atomic_and32(_ctx: &mut EmitContext) {
    todo!("EmitStorageAtomicAnd32")
}

/// Emit SPIR-V for storage buffer atomic OR.
pub fn emit_storage_atomic_or32(_ctx: &mut EmitContext) {
    todo!("EmitStorageAtomicOr32")
}

/// Emit SPIR-V for storage buffer atomic XOR.
pub fn emit_storage_atomic_xor32(_ctx: &mut EmitContext) {
    todo!("EmitStorageAtomicXor32")
}

/// Emit SPIR-V for storage buffer atomic exchange.
pub fn emit_storage_atomic_exchange32(_ctx: &mut EmitContext) {
    todo!("EmitStorageAtomicExchange32")
}

/// Emit SPIR-V for global memory atomic add.
pub fn emit_global_atomic_iadd32(_ctx: &mut EmitContext) {
    todo!("EmitGlobalAtomicIAdd32")
}

/// Emit SPIR-V for global memory atomic exchange.
pub fn emit_global_atomic_exchange32(_ctx: &mut EmitContext) {
    todo!("EmitGlobalAtomicExchange32")
}
