// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of `shader_recompiler/environment.h`
//!
//! Abstract environment interface providing access to the shader binary
//! and GPU state needed during shader translation.

use super::program_header::ProgramHeader;
use super::shader_info::{ReplaceConstant, TexturePixelFormat, TextureType};
use super::stage::Stage;

/// Abstract environment for shader recompilation.
///
/// Provides access to shader instructions, constant buffer values,
/// texture types, and other GPU state needed during translation.
pub trait Environment {
    /// Read a 64-bit instruction at the given byte address.
    fn read_instruction(&self, address: u32) -> u64;

    /// Read a 32-bit value from a constant buffer.
    fn read_cbuf_value(&self, cbuf_index: u32, cbuf_offset: u32) -> u32;

    /// Read the texture type for a raw texture handle.
    fn read_texture_type(&self, raw_handle: u32) -> TextureType;

    /// Read the texture pixel format for a raw texture handle.
    fn read_texture_pixel_format(&self, raw_handle: u32) -> TexturePixelFormat;

    /// Check if a texture pixel format is integer.
    fn is_texture_pixel_format_integer(&self, raw_handle: u32) -> bool;

    /// Read the viewport transform state register.
    fn read_viewport_transform_state(&self) -> u32;

    /// Get the texture bound buffer index.
    fn texture_bound_buffer(&self) -> u32;

    /// Get the local memory size.
    fn local_memory_size(&self) -> u32;

    /// Get the shared memory size.
    fn shared_memory_size(&self) -> u32;

    /// Get the workgroup size [x, y, z].
    fn workgroup_size(&self) -> [u32; 3];

    /// Whether the environment has HLE macro state.
    fn has_hle_macro_state(&self) -> bool;

    /// Get a replacement constant for a cbuf access (HLE macro support).
    fn get_replace_const_buffer(&self, bank: u32, offset: u32) -> Option<ReplaceConstant>;

    /// Dump shader for debugging.
    fn dump(&self, pipeline_hash: u64, shader_hash: u64);

    /// Get the shader program header.
    fn sph(&self) -> &ProgramHeader;

    /// Get the geometry passthrough mask.
    fn gp_passthrough_mask(&self) -> &[u32; 8];

    /// Get the shader stage.
    fn shader_stage(&self) -> Stage;

    /// Get the start address of the shader.
    fn start_address(&self) -> u32;

    /// Whether this is a proprietary driver.
    fn is_proprietary_driver(&self) -> bool;
}
