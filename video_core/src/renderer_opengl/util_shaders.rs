// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/util_shaders.h and util_shaders.cpp
//!
//! Utility compute shaders for texture swizzling, ASTC decoding, format conversion, and MSAA.

/// Swizzle table dimensions (64x8 = 512 bytes).
const SWIZZLE_TABLE_SIZE: usize = 512;

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
    /// Port of `UtilShaders::UtilShaders()`.
    /// In the full implementation, this compiles compute shaders from host shader
    /// sources and creates the swizzle table buffer.
    pub fn new() -> Self {
        let mut swizzle_table_buffer: u32 = 0;

        // Create and fill the swizzle table buffer
        // The swizzle table maps (x, y) positions to linear offsets within a GOB
        let swizzle_table = Self::build_swizzle_table();
        unsafe {
            gl::CreateBuffers(1, &mut swizzle_table_buffer);
            gl::NamedBufferStorage(
                swizzle_table_buffer,
                SWIZZLE_TABLE_SIZE as isize,
                swizzle_table.as_ptr() as *const _,
                0,
            );
        }

        Self {
            swizzle_table_buffer,
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

    /// Build the GOB swizzle table matching the hardware layout.
    /// The table maps (x, y) within a 64x8 GOB to a linear byte offset.
    fn build_swizzle_table() -> [u8; SWIZZLE_TABLE_SIZE] {
        let mut table = [0u8; SWIZZLE_TABLE_SIZE];
        for y in 0u32..8 {
            for x in 0u32..64 {
                // NV block linear swizzle: interleave bits of x and y
                let offset = ((x & 0x3F) | ((y & 0x01) << 6) | ((y & 0x06) << 1)) as u8;
                table[(y * 64 + x) as usize] = offset;
            }
        }
        table
    }

    /// Decode ASTC textures using compute shader.
    ///
    /// Port of `UtilShaders::ASTCDecode`.
    /// Dispatches the ASTC decoder compute shader to decompress ASTC-encoded
    /// texture data stored in a buffer into an uncompressed image.
    pub fn astc_decode(
        &self,
        dst_image: u32,
        src_buffer: u32,
        block_width: u32,
        block_height: u32,
        layer_stride: u32,
        width: u32,
        height: u32,
        depth: u32,
        layer: u32,
        level: u32,
    ) {
        if self.astc_decoder_program == 0 {
            log::warn!("ASTC decoder program not compiled");
            return;
        }
        unsafe {
            gl::UseProgram(self.astc_decoder_program);

            // Bind output image
            gl::BindImageTexture(
                0,
                dst_image,
                level as i32,
                gl::TRUE,
                layer as i32,
                gl::WRITE_ONLY,
                gl::RGBA8,
            );

            // Bind source buffer
            gl::BindBufferBase(gl::SHADER_STORAGE_BUFFER, 0, src_buffer);

            // Set uniforms
            gl::Uniform1ui(0, block_width);
            gl::Uniform1ui(1, block_height);
            gl::Uniform1ui(2, layer_stride);
            gl::Uniform1ui(3, width);
            gl::Uniform1ui(4, height);
            gl::Uniform1ui(5, depth);
            gl::Uniform1ui(6, layer);

            // Dispatch
            let groups_x = (width + block_width - 1) / block_width;
            let groups_y = (height + block_height - 1) / block_height;
            gl::DispatchCompute(groups_x, groups_y, depth);

            gl::MemoryBarrier(gl::TEXTURE_FETCH_BARRIER_BIT | gl::SHADER_IMAGE_ACCESS_BARRIER_BIT);
        }
    }

    /// Upload a 2D block-linear image using compute shader.
    ///
    /// Port of `UtilShaders::BlockLinearUpload2D`.
    pub fn block_linear_upload_2d(
        &self,
        dst_image: u32,
        src_buffer: u32,
        width: u32,
        height: u32,
        depth: u32,
        level: u32,
        block_height: u32,
        bytes_per_block: u32,
    ) {
        if self.block_linear_unswizzle_2d_program == 0 {
            log::warn!("Block linear unswizzle 2D program not compiled");
            return;
        }
        let store_fmt = store_format(bytes_per_block);
        unsafe {
            gl::UseProgram(self.block_linear_unswizzle_2d_program);
            gl::BindImageTexture(
                0,
                dst_image,
                level as i32,
                gl::TRUE,
                0,
                gl::WRITE_ONLY,
                store_fmt,
            );
            gl::BindBufferBase(gl::SHADER_STORAGE_BUFFER, 0, src_buffer);
            gl::BindBufferBase(gl::SHADER_STORAGE_BUFFER, 1, self.swizzle_table_buffer);

            gl::Uniform1ui(0, width);
            gl::Uniform1ui(1, height);
            gl::Uniform1ui(2, depth);
            gl::Uniform1ui(3, block_height);
            gl::Uniform1ui(4, bytes_per_block);

            gl::DispatchCompute((width + 15) / 16, (height + 15) / 16, depth);
            gl::MemoryBarrier(gl::TEXTURE_FETCH_BARRIER_BIT | gl::SHADER_IMAGE_ACCESS_BARRIER_BIT);
        }
    }

    /// Upload a 3D block-linear image using compute shader.
    ///
    /// Port of `UtilShaders::BlockLinearUpload3D`.
    pub fn block_linear_upload_3d(
        &self,
        dst_image: u32,
        src_buffer: u32,
        width: u32,
        height: u32,
        depth: u32,
        level: u32,
        block_height: u32,
        block_depth: u32,
        bytes_per_block: u32,
    ) {
        if self.block_linear_unswizzle_3d_program == 0 {
            log::warn!("Block linear unswizzle 3D program not compiled");
            return;
        }
        let store_fmt = store_format(bytes_per_block);
        unsafe {
            gl::UseProgram(self.block_linear_unswizzle_3d_program);
            gl::BindImageTexture(
                0,
                dst_image,
                level as i32,
                gl::TRUE,
                0,
                gl::WRITE_ONLY,
                store_fmt,
            );
            gl::BindBufferBase(gl::SHADER_STORAGE_BUFFER, 0, src_buffer);
            gl::BindBufferBase(gl::SHADER_STORAGE_BUFFER, 1, self.swizzle_table_buffer);

            gl::Uniform1ui(0, width);
            gl::Uniform1ui(1, height);
            gl::Uniform1ui(2, depth);
            gl::Uniform1ui(3, block_height);
            gl::Uniform1ui(4, block_depth);
            gl::Uniform1ui(5, bytes_per_block);

            gl::DispatchCompute((width + 15) / 16, (height + 15) / 16, (depth + 3) / 4);
            gl::MemoryBarrier(gl::TEXTURE_FETCH_BARRIER_BIT | gl::SHADER_IMAGE_ACCESS_BARRIER_BIT);
        }
    }

    /// Upload a pitch-linear image using compute shader.
    ///
    /// Port of `UtilShaders::PitchUpload`.
    pub fn pitch_upload(
        &self,
        dst_image: u32,
        src_buffer: u32,
        width: u32,
        height: u32,
        depth: u32,
        level: u32,
        bytes_per_block: u32,
        origin_x: u32,
        origin_y: u32,
    ) {
        if self.pitch_unswizzle_program == 0 {
            log::warn!("Pitch unswizzle program not compiled");
            return;
        }
        let store_fmt = store_format(bytes_per_block);
        unsafe {
            gl::UseProgram(self.pitch_unswizzle_program);
            gl::BindImageTexture(
                0,
                dst_image,
                level as i32,
                gl::TRUE,
                0,
                gl::WRITE_ONLY,
                store_fmt,
            );
            gl::BindBufferBase(gl::SHADER_STORAGE_BUFFER, 0, src_buffer);

            gl::Uniform1ui(0, width);
            gl::Uniform1ui(1, height);
            gl::Uniform1ui(2, depth);
            gl::Uniform1ui(3, bytes_per_block);
            gl::Uniform1ui(4, origin_x);
            gl::Uniform1ui(5, origin_y);

            gl::DispatchCompute((width + 15) / 16, (height + 15) / 16, depth);
            gl::MemoryBarrier(gl::TEXTURE_FETCH_BARRIER_BIT | gl::SHADER_IMAGE_ACCESS_BARRIER_BIT);
        }
    }

    /// Copy BC4 texture data.
    ///
    /// Port of `UtilShaders::CopyBC4`.
    pub fn copy_bc4(
        &self,
        dst_image: u32,
        src_buffer: u32,
        width: u32,
        height: u32,
        depth: u32,
        level: u32,
    ) {
        if self.copy_bc4_program == 0 {
            log::warn!("Copy BC4 program not compiled");
            return;
        }
        unsafe {
            gl::UseProgram(self.copy_bc4_program);
            gl::BindImageTexture(
                0,
                dst_image,
                level as i32,
                gl::TRUE,
                0,
                gl::WRITE_ONLY,
                gl::RGBA8,
            );
            gl::BindBufferBase(gl::SHADER_STORAGE_BUFFER, 0, src_buffer);

            gl::Uniform1ui(0, width);
            gl::Uniform1ui(1, height);
            gl::Uniform1ui(2, depth);

            let block_w = (width + 3) / 4;
            let block_h = (height + 3) / 4;
            gl::DispatchCompute(block_w, block_h, depth);
            gl::MemoryBarrier(gl::TEXTURE_FETCH_BARRIER_BIT | gl::SHADER_IMAGE_ACCESS_BARRIER_BIT);
        }
    }

    /// Convert S8D24 depth-stencil format.
    ///
    /// Port of `UtilShaders::ConvertS8D24`.
    pub fn convert_s8d24(&self, dst_image: u32, src_buffer: u32, width: u32, height: u32) {
        if self.convert_s8d24_program == 0 {
            log::warn!("Convert S8D24 program not compiled");
            return;
        }
        unsafe {
            gl::UseProgram(self.convert_s8d24_program);
            gl::BindImageTexture(0, dst_image, 0, gl::FALSE, 0, gl::WRITE_ONLY, gl::R32UI);
            gl::BindBufferBase(gl::SHADER_STORAGE_BUFFER, 0, src_buffer);

            gl::Uniform1ui(0, width);
            gl::Uniform1ui(1, height);

            gl::DispatchCompute((width + 15) / 16, (height + 15) / 16, 1);
            gl::MemoryBarrier(gl::TEXTURE_FETCH_BARRIER_BIT | gl::SHADER_IMAGE_ACCESS_BARRIER_BIT);
        }
    }

    /// Copy between MSAA and non-MSAA textures.
    ///
    /// Port of `UtilShaders::CopyMSAA`.
    pub fn copy_msaa(
        &self,
        dst_image: u32,
        src_image: u32,
        width: u32,
        height: u32,
        samples: u32,
        ms_to_nonms: bool,
    ) {
        let program = if ms_to_nonms {
            self.convert_ms_to_nonms_program
        } else {
            self.convert_nonms_to_ms_program
        };
        if program == 0 {
            log::warn!("MSAA copy program not compiled");
            return;
        }
        unsafe {
            gl::UseProgram(program);
            gl::BindImageTexture(0, dst_image, 0, gl::TRUE, 0, gl::WRITE_ONLY, gl::RGBA8);
            gl::BindImageTexture(1, src_image, 0, gl::TRUE, 0, gl::READ_ONLY, gl::RGBA8);

            gl::Uniform1ui(0, width);
            gl::Uniform1ui(1, height);
            gl::Uniform1ui(2, samples);

            gl::DispatchCompute((width + 15) / 16, (height + 15) / 16, 1);
            gl::MemoryBarrier(gl::TEXTURE_FETCH_BARRIER_BIT | gl::SHADER_IMAGE_ACCESS_BARRIER_BIT);
        }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn store_format_mapping() {
        assert_eq!(store_format(1), gl::R8UI);
        assert_eq!(store_format(2), gl::R16UI);
        assert_eq!(store_format(4), gl::R32UI);
        assert_eq!(store_format(8), gl::RG32UI);
        assert_eq!(store_format(16), gl::RGBA32UI);
    }

    #[test]
    fn swizzle_table_size() {
        assert_eq!(SWIZZLE_TABLE_SIZE, 512);
    }
}
