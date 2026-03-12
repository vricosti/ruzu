// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/util_shaders.h and util_shaders.cpp
//!
//! Utility compute shaders for texture swizzling, ASTC decoding, format conversion, and MSAA.

/// Utility shaders collection.
///
/// Corresponds to `OpenGL::UtilShaders`.
pub struct UtilShaders {
    swizzle_table_buffer: u32,
    astc_decoder_program: u32,
    block_linear_unswizzle_2d_program: u32,
    block_linear_unswizzle_3d_program: u32,
    pitch_unswizzle_program: u32,
    copy_bc4_program: u32,
    convert_s8d24_program: u32,
    convert_ms_to_nonms_program: u32,
    convert_nonms_to_ms_program: u32,
}

impl UtilShaders {
    /// Create utility shaders.
    ///
    /// Corresponds to `UtilShaders::UtilShaders()`.
    pub fn new() -> Self {
        // TODO: Compile compute shaders from host shader sources
        // TODO: Create swizzle table buffer
        Self {
            swizzle_table_buffer: 0,
            astc_decoder_program: 0,
            block_linear_unswizzle_2d_program: 0,
            block_linear_unswizzle_3d_program: 0,
            pitch_unswizzle_program: 0,
            copy_bc4_program: 0,
            convert_s8d24_program: 0,
            convert_ms_to_nonms_program: 0,
            convert_nonms_to_ms_program: 0,
        }
    }

    /// Decode ASTC textures using compute shader.
    pub fn astc_decode(&self) {
        todo!("UtilShaders::ASTCDecode")
    }

    /// Upload a 2D block-linear image using compute shader.
    pub fn block_linear_upload_2d(&self) {
        todo!("UtilShaders::BlockLinearUpload2D")
    }

    /// Upload a 3D block-linear image using compute shader.
    pub fn block_linear_upload_3d(&self) {
        todo!("UtilShaders::BlockLinearUpload3D")
    }

    /// Upload a pitch-linear image using compute shader.
    pub fn pitch_upload(&self) {
        todo!("UtilShaders::PitchUpload")
    }

    /// Copy BC4 texture data.
    pub fn copy_bc4(&self) {
        todo!("UtilShaders::CopyBC4")
    }

    /// Convert S8D24 depth-stencil format.
    pub fn convert_s8d24(&self) {
        todo!("UtilShaders::ConvertS8D24")
    }

    /// Copy between MSAA and non-MSAA textures.
    pub fn copy_msaa(&self) {
        todo!("UtilShaders::CopyMSAA")
    }
}

impl Drop for UtilShaders {
    fn drop(&mut self) {
        unsafe {
            if self.swizzle_table_buffer != 0 {
                gl::DeleteBuffers(1, &self.swizzle_table_buffer);
            }
            for &prog in &[
                self.astc_decoder_program,
                self.block_linear_unswizzle_2d_program,
                self.block_linear_unswizzle_3d_program,
                self.pitch_unswizzle_program,
                self.copy_bc4_program,
                self.convert_s8d24_program,
                self.convert_ms_to_nonms_program,
                self.convert_nonms_to_ms_program,
            ] {
                if prog != 0 {
                    gl::DeleteProgram(prog);
                }
            }
        }
    }
}

/// Map bytes-per-block to the appropriate GL store format for compute shader image access.
///
/// Corresponds to `OpenGL::StoreFormat()`.
pub fn store_format(bytes_per_block: u32) -> u32 {
    match bytes_per_block {
        1 => gl::R8UI,
        2 => gl::R16UI,
        4 => gl::R32UI,
        8 => gl::RG32UI,
        16 => gl::RGBA32UI,
        _ => {
            debug_assert!(false, "Invalid bytes_per_block: {}", bytes_per_block);
            gl::R8UI
        }
    }
}
