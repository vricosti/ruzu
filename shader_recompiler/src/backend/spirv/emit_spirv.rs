// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Top-level SPIR-V emission — maps to zuyu's `backend/spirv/emit_spirv.h` and
//! `emit_spirv.cpp`.
//!
//! Contains the `EmitSPIRV` entry point and constants for rescaling/render area
//! push constant layouts.

use crate::ir;
use super::spirv_emit_context::SpirvEmitContext;

/// Number of u32 words for texture rescaling data.
pub const NUM_TEXTURE_SCALING_WORDS: u32 = 4;

/// Number of u32 words for image rescaling data.
pub const NUM_IMAGE_SCALING_WORDS: u32 = 2;

/// Combined texture + image rescaling word count.
pub const NUM_TEXTURE_AND_IMAGE_SCALING_WORDS: u32 =
    NUM_TEXTURE_SCALING_WORDS + NUM_IMAGE_SCALING_WORDS;

/// Rescaling push constant layout.
///
/// Matches upstream `RescalingLayout`:
/// ```c
/// struct RescalingLayout {
///     alignas(16) std::array<u32, 4> rescaling_textures;
///     alignas(16) std::array<u32, 2> rescaling_images;
///     u32 down_factor;
/// };
/// ```
#[repr(C, align(16))]
pub struct RescalingLayout {
    pub rescaling_textures: [u32; NUM_TEXTURE_SCALING_WORDS as usize],
    pub rescaling_images: [u32; NUM_IMAGE_SCALING_WORDS as usize],
    pub down_factor: u32,
}

/// Render area push constant layout.
#[repr(C)]
pub struct RenderAreaLayout {
    pub render_area: [f32; 4],
}

/// Byte offset of `rescaling_textures` within `RescalingLayout`.
pub const RESCALING_LAYOUT_WORDS_OFFSET: u32 = 0;

/// Byte offset of `down_factor` within `RescalingLayout`.
pub const RESCALING_LAYOUT_DOWN_FACTOR_OFFSET: u32 = {
    // rescaling_textures: 16 bytes (align(16))
    // rescaling_images: 8 bytes padded to 16
    // down_factor at offset 24
    // But with repr(C, align(16)) the actual offset depends on padding.
    // Upstream: offsetof(RescalingLayout, down_factor)
    // textures = 4 * 4 = 16 bytes at offset 0
    // images = 2 * 4 = 8 bytes at offset 16
    // down_factor at offset 24
    24
};

/// Byte offset of `render_area` within `RenderAreaLayout`.
pub const RENDERAREA_LAYOUT_OFFSET: u32 = 0;

/// Emit SPIR-V binary from an IR program.
///
/// This is the main entry point matching upstream `EmitSPIRV()`.
/// Returns SPIR-V word vector ready for VkShaderModule creation.
pub fn emit_spirv(
    program: &ir::Program,
    profile: &super::super::Profile,
) -> Vec<u32> {
    let mut ctx = SpirvEmitContext::new(program, profile);
    ctx.emit_program(program);
    ctx.finalize()
}
