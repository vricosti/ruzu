// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `backend/spirv/emit_spirv_image.cpp`
//!
//! SPIR-V emission for texture sampling and image operations.

use super::spirv_context::EmitContext;

/// Emit SPIR-V for implicit LOD texture sample.
pub fn emit_image_sample_implicit_lod(_ctx: &mut EmitContext) {
    todo!("EmitImageSampleImplicitLod")
}

/// Emit SPIR-V for explicit LOD texture sample.
pub fn emit_image_sample_explicit_lod(_ctx: &mut EmitContext) {
    todo!("EmitImageSampleExplicitLod")
}

/// Emit SPIR-V for texture sample with depth comparison.
pub fn emit_image_sample_dref_implicit_lod(_ctx: &mut EmitContext) {
    todo!("EmitImageSampleDrefImplicitLod")
}

/// Emit SPIR-V for texture sample with depth comparison and explicit LOD.
pub fn emit_image_sample_dref_explicit_lod(_ctx: &mut EmitContext) {
    todo!("EmitImageSampleDrefExplicitLod")
}

/// Emit SPIR-V for texture gather.
pub fn emit_image_gather(_ctx: &mut EmitContext) {
    todo!("EmitImageGather")
}

/// Emit SPIR-V for texture gather with depth comparison.
pub fn emit_image_gather_dref(_ctx: &mut EmitContext) {
    todo!("EmitImageGatherDref")
}

/// Emit SPIR-V for texture fetch (texelFetch).
pub fn emit_image_fetch(_ctx: &mut EmitContext) {
    todo!("EmitImageFetch")
}

/// Emit SPIR-V for texture query dimensions.
pub fn emit_image_query_dimensions(_ctx: &mut EmitContext) {
    todo!("EmitImageQueryDimensions")
}

/// Emit SPIR-V for texture query LOD.
pub fn emit_image_query_lod(_ctx: &mut EmitContext) {
    todo!("EmitImageQueryLod")
}

/// Emit SPIR-V for image gradient sample.
pub fn emit_image_gradient(_ctx: &mut EmitContext) {
    todo!("EmitImageGradient")
}

/// Emit SPIR-V for image read.
pub fn emit_image_read(_ctx: &mut EmitContext) {
    todo!("EmitImageRead")
}

/// Emit SPIR-V for image write.
pub fn emit_image_write(_ctx: &mut EmitContext) {
    todo!("EmitImageWrite")
}
