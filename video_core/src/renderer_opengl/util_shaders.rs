// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/util_shaders.h and util_shaders.cpp
//!
//! Utility compute shaders for texture swizzling, ASTC decoding, format conversion, and MSAA.

use crate::host_shaders::compute_shaders::{
    ASTC_DECODER_COMP, BLOCK_LINEAR_UNSWIZZLE_2D_COMP, BLOCK_LINEAR_UNSWIZZLE_3D_COMP,
    CONVERT_MSAA_TO_NON_MSAA_COMP, CONVERT_NON_MSAA_TO_MSAA_COMP, OPENGL_CONVERT_S8D24_COMP,
    OPENGL_COPY_BC4_COMP, PITCH_UNSWIZZLE_COMP,
};
use crate::renderer_opengl::gl_shader_manager::ProgramManagerHandle;
use crate::renderer_opengl::gl_shader_util::create_program_from_source;
use crate::surface::{bytes_per_block, default_block_height, default_block_width};
use crate::texture_cache::accelerated_swizzle::{
    make_block_linear_swizzle_2d_params, make_block_linear_swizzle_3d_params,
};
use crate::texture_cache::image_info::{ImageInfo, TilingMode};
use crate::texture_cache::types::{ImageCopy, SwizzleParameters};

/// Swizzle table dimensions (64x8 = 512 bytes).
const SWIZZLE_TABLE_SIZE: usize = 512;

/// Utility shaders collection.
///
/// Corresponds to `OpenGL::UtilShaders`.
pub struct UtilShaders {
    program_manager: ProgramManagerHandle,
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
    pub fn new(program_manager: ProgramManagerHandle) -> Self {
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
            program_manager,
            swizzle_table_buffer,
            astc_decoder_program: create_program_from_source(ASTC_DECODER_COMP, gl::COMPUTE_SHADER),
            block_linear_unswizzle_2d_program: create_program_from_source(
                BLOCK_LINEAR_UNSWIZZLE_2D_COMP,
                gl::COMPUTE_SHADER,
            ),
            block_linear_unswizzle_3d_program: create_program_from_source(
                BLOCK_LINEAR_UNSWIZZLE_3D_COMP,
                gl::COMPUTE_SHADER,
            ),
            pitch_unswizzle_program: create_program_from_source(
                PITCH_UNSWIZZLE_COMP,
                gl::COMPUTE_SHADER,
            ),
            copy_bc4_program: create_program_from_source(OPENGL_COPY_BC4_COMP, gl::COMPUTE_SHADER),
            convert_s8d24_program: create_program_from_source(
                OPENGL_CONVERT_S8D24_COMP,
                gl::COMPUTE_SHADER,
            ),
            convert_ms_to_nonms_program: create_program_from_source(
                CONVERT_MSAA_TO_NON_MSAA_COMP,
                gl::COMPUTE_SHADER,
            ),
            convert_nonms_to_ms_program: create_program_from_source(
                CONVERT_NON_MSAA_TO_MSAA_COMP,
                gl::COMPUTE_SHADER,
            ),
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

    /// Port of `UtilShaders::ASTCDecode`.
    pub fn astc_decode(
        &mut self,
        dst_image: u32,
        src_buffer: u32,
        buffer_offset: usize,
        guest_size_bytes: usize,
        info: &ImageInfo,
        swizzles: &[SwizzleParameters],
    ) {
        if self.astc_decoder_program == 0 {
            log::warn!("ASTC decoder program not compiled");
            return;
        }
        let tile_width = default_block_width(info.format);
        let tile_height = default_block_height(info.format);
        let mut program_manager = self.program_manager.lock();
        program_manager.local_memory_warmup();
        program_manager.bind_compute_program(self.astc_decoder_program);
        unsafe {
            gl::FlushMappedNamedBufferRange(
                src_buffer,
                buffer_offset as isize,
                guest_size_bytes as isize,
            );
            gl::Uniform2ui(1, tile_width, tile_height);
            for swizzle in swizzles {
                let input_offset = buffer_offset + swizzle.buffer_offset;
                let range_size = guest_size_bytes.saturating_sub(swizzle.buffer_offset);
                let params = make_block_linear_swizzle_2d_params(swizzle, info);
                if params.origin != [0, 0, 0] || params.destination != [0, 0, 0] {
                    log::warn!(
                        "UtilShaders::astc_decode: unsupported swizzle origin/destination {:?}/{:?}",
                        params.origin,
                        params.destination
                    );
                    continue;
                }
                if params.bytes_per_block_log2 != 4 {
                    log::warn!(
                        "UtilShaders::astc_decode: unexpected bytes_per_block_log2={}",
                        params.bytes_per_block_log2
                    );
                    continue;
                }
                gl::Uniform1ui(2, params.layer_stride);
                gl::Uniform1ui(3, params.block_size);
                gl::Uniform1ui(4, params.x_shift);
                gl::Uniform1ui(5, params.block_height);
                gl::Uniform1ui(6, params.block_height_mask);
                gl::BindBufferRange(
                    gl::SHADER_STORAGE_BUFFER,
                    0,
                    src_buffer,
                    input_offset as isize,
                    range_size as isize,
                );
                gl::BindImageTexture(
                    0,
                    dst_image,
                    swizzle.level,
                    gl::TRUE,
                    0,
                    gl::WRITE_ONLY,
                    gl::RGBA8,
                );
                gl::DispatchCompute(
                    swizzle.num_tiles.width.div_ceil(8),
                    swizzle.num_tiles.height.div_ceil(8),
                    info.resources.layers as u32,
                );
            }
            gl::MemoryBarrier(
                gl::UNIFORM_BARRIER_BIT
                    | gl::COMMAND_BARRIER_BIT
                    | gl::PIXEL_BUFFER_BARRIER_BIT
                    | gl::TEXTURE_UPDATE_BARRIER_BIT
                    | gl::BUFFER_UPDATE_BARRIER_BIT
                    | gl::SHADER_STORAGE_BARRIER_BIT
                    | gl::CLIENT_MAPPED_BUFFER_BARRIER_BIT,
            );
        }
        program_manager.restore_guest_compute();
    }

    /// Port of `UtilShaders::BlockLinearUpload2D`.
    pub fn block_linear_upload_2d(
        &mut self,
        dst_image: u32,
        src_buffer: u32,
        buffer_offset: usize,
        guest_size_bytes: usize,
        info: &ImageInfo,
        swizzles: &[SwizzleParameters],
    ) {
        if self.block_linear_unswizzle_2d_program == 0 {
            log::warn!("Block linear unswizzle 2D program not compiled");
            return;
        }
        let store_fmt = store_format(bytes_per_block(info.format));
        let mut program_manager = self.program_manager.lock();
        program_manager.bind_compute_program(self.block_linear_unswizzle_2d_program);
        unsafe {
            gl::FlushMappedNamedBufferRange(
                src_buffer,
                buffer_offset as isize,
                guest_size_bytes as isize,
            );
            gl::BindBufferBase(gl::SHADER_STORAGE_BUFFER, 0, self.swizzle_table_buffer);
            for swizzle in swizzles {
                let input_offset = buffer_offset + swizzle.buffer_offset;
                let range_size = guest_size_bytes.saturating_sub(swizzle.buffer_offset);
                let params = make_block_linear_swizzle_2d_params(swizzle, info);
                gl::Uniform3uiv(0, 1, params.origin.as_ptr());
                gl::Uniform3iv(1, 1, params.destination.as_ptr());
                gl::Uniform1ui(2, params.bytes_per_block_log2);
                gl::Uniform1ui(3, params.layer_stride);
                gl::Uniform1ui(4, params.block_size);
                gl::Uniform1ui(5, params.x_shift);
                gl::Uniform1ui(6, params.block_height);
                gl::Uniform1ui(7, params.block_height_mask);
                gl::BindBufferRange(
                    gl::SHADER_STORAGE_BUFFER,
                    1,
                    src_buffer,
                    input_offset as isize,
                    range_size as isize,
                );
                gl::BindImageTexture(
                    0,
                    dst_image,
                    swizzle.level,
                    gl::TRUE,
                    0,
                    gl::WRITE_ONLY,
                    store_fmt,
                );
                gl::DispatchCompute(
                    swizzle.num_tiles.width.div_ceil(32),
                    swizzle.num_tiles.height.div_ceil(32),
                    info.resources.layers as u32,
                );
            }
        }
        program_manager.restore_guest_compute();
    }

    /// Port of `UtilShaders::BlockLinearUpload3D`.
    pub fn block_linear_upload_3d(
        &mut self,
        dst_image: u32,
        src_buffer: u32,
        buffer_offset: usize,
        guest_size_bytes: usize,
        info: &ImageInfo,
        swizzles: &[SwizzleParameters],
    ) {
        if self.block_linear_unswizzle_3d_program == 0 {
            log::warn!("Block linear unswizzle 3D program not compiled");
            return;
        }
        let store_fmt = store_format(bytes_per_block(info.format));
        let mut program_manager = self.program_manager.lock();
        program_manager.bind_compute_program(self.block_linear_unswizzle_3d_program);
        unsafe {
            gl::FlushMappedNamedBufferRange(
                src_buffer,
                buffer_offset as isize,
                guest_size_bytes as isize,
            );
            gl::BindBufferBase(gl::SHADER_STORAGE_BUFFER, 0, self.swizzle_table_buffer);
            for swizzle in swizzles {
                let input_offset = buffer_offset + swizzle.buffer_offset;
                let range_size = guest_size_bytes.saturating_sub(swizzle.buffer_offset);
                let params = make_block_linear_swizzle_3d_params(swizzle, info);
                gl::Uniform3uiv(0, 1, params.origin.as_ptr());
                gl::Uniform3iv(1, 1, params.destination.as_ptr());
                gl::Uniform1ui(2, params.bytes_per_block_log2);
                gl::Uniform1ui(3, params.slice_size);
                gl::Uniform1ui(4, params.block_size);
                gl::Uniform1ui(5, params.x_shift);
                gl::Uniform1ui(6, params.block_height);
                gl::Uniform1ui(7, params.block_height_mask);
                gl::Uniform1ui(8, params.block_depth);
                gl::Uniform1ui(9, params.block_depth_mask);
                gl::BindBufferRange(
                    gl::SHADER_STORAGE_BUFFER,
                    1,
                    src_buffer,
                    input_offset as isize,
                    range_size as isize,
                );
                gl::BindImageTexture(
                    0,
                    dst_image,
                    swizzle.level,
                    gl::TRUE,
                    0,
                    gl::WRITE_ONLY,
                    store_fmt,
                );
                gl::DispatchCompute(
                    swizzle.num_tiles.width.div_ceil(16),
                    swizzle.num_tiles.height.div_ceil(8),
                    swizzle.num_tiles.depth.div_ceil(8),
                );
            }
        }
        program_manager.restore_guest_compute();
    }

    /// Port of `UtilShaders::PitchUpload`.
    pub fn pitch_upload(
        &mut self,
        dst_image: u32,
        src_buffer: u32,
        buffer_offset: usize,
        guest_size_bytes: usize,
        info: &ImageInfo,
        swizzles: &[SwizzleParameters],
    ) {
        if self.pitch_unswizzle_program == 0 {
            log::warn!("Pitch unswizzle program not compiled");
            return;
        }
        let bytes_per_block = bytes_per_block(info.format);
        let store_fmt = store_format(bytes_per_block);
        let pitch = match info.tiling {
            TilingMode::PitchLinear(pitch) => pitch,
            TilingMode::BlockLinear(_) => {
                log::warn!("UtilShaders::pitch_upload called for block-linear image");
                return;
            }
        };
        let mut program_manager = self.program_manager.lock();
        program_manager.bind_compute_program(self.pitch_unswizzle_program);
        unsafe {
            gl::FlushMappedNamedBufferRange(
                src_buffer,
                buffer_offset as isize,
                guest_size_bytes as isize,
            );
            gl::Uniform2ui(0, 0, 0);
            gl::Uniform2i(1, 0, 0);
            gl::Uniform1ui(2, bytes_per_block);
            gl::Uniform1ui(3, pitch);
            gl::BindImageTexture(0, dst_image, 0, gl::FALSE, 0, gl::WRITE_ONLY, store_fmt);
            for swizzle in swizzles {
                let input_offset = buffer_offset + swizzle.buffer_offset;
                let range_size = guest_size_bytes.saturating_sub(swizzle.buffer_offset);
                gl::BindBufferRange(
                    gl::SHADER_STORAGE_BUFFER,
                    0,
                    src_buffer,
                    input_offset as isize,
                    range_size as isize,
                );
                gl::DispatchCompute(
                    swizzle.num_tiles.width.div_ceil(32),
                    swizzle.num_tiles.height.div_ceil(32),
                    1,
                );
            }
        }
        program_manager.restore_guest_compute();
    }

    /// Port of `UtilShaders::CopyBC4`.
    pub fn copy_bc4(&mut self, dst_image: u32, src_image: u32, copies: &[ImageCopy]) {
        if self.copy_bc4_program == 0 {
            log::warn!("Copy BC4 program not compiled");
            return;
        }
        let mut program_manager = self.program_manager.lock();
        program_manager.bind_compute_program(self.copy_bc4_program);
        unsafe {
            for copy in copies {
                if copy.src_subresource.base_layer != 0
                    || copy.src_subresource.num_layers != 1
                    || copy.dst_subresource.base_layer != 0
                    || copy.dst_subresource.num_layers != 1
                {
                    log::warn!(
                        "UtilShaders::copy_bc4: unsupported layer copy src_base={} src_layers={} dst_base={} dst_layers={}",
                        copy.src_subresource.base_layer,
                        copy.src_subresource.num_layers,
                        copy.dst_subresource.base_layer,
                        copy.dst_subresource.num_layers
                    );
                    continue;
                }

                gl::Uniform3ui(
                    0,
                    copy.src_offset.x as u32,
                    copy.src_offset.y as u32,
                    copy.src_offset.z as u32,
                );
                gl::Uniform3ui(
                    1,
                    copy.dst_offset.x as u32,
                    copy.dst_offset.y as u32,
                    copy.dst_offset.z as u32,
                );
                gl::BindImageTexture(
                    0,
                    src_image,
                    copy.src_subresource.base_level as i32,
                    gl::TRUE,
                    0,
                    gl::READ_ONLY,
                    gl::RG32UI,
                );
                gl::BindImageTexture(
                    1,
                    dst_image,
                    copy.dst_subresource.base_level as i32,
                    gl::TRUE,
                    0,
                    gl::WRITE_ONLY,
                    gl::RGBA8UI,
                );
                gl::DispatchCompute(copy.extent.width, copy.extent.height, copy.extent.depth);
            }
            gl::MemoryBarrier(gl::TEXTURE_FETCH_BARRIER_BIT | gl::SHADER_IMAGE_ACCESS_BARRIER_BIT);
        }
        program_manager.restore_guest_compute();
    }

    /// Port of `UtilShaders::ConvertS8D24`.
    pub fn convert_s8d24(&mut self, dst_image: u32, copies: &[ImageCopy]) {
        if self.convert_s8d24_program == 0 {
            log::warn!("Convert S8D24 program not compiled");
            return;
        }
        let mut program_manager = self.program_manager.lock();
        program_manager.bind_compute_program(self.convert_s8d24_program);
        unsafe {
            for copy in copies {
                if copy.src_subresource.base_layer != 0
                    || copy.src_subresource.num_layers != 1
                    || copy.dst_subresource.base_layer != 0
                    || copy.dst_subresource.num_layers != 1
                {
                    log::warn!(
                        "UtilShaders::convert_s8d24: unsupported layer copy src_base={} src_layers={} dst_base={} dst_layers={}",
                        copy.src_subresource.base_layer,
                        copy.src_subresource.num_layers,
                        copy.dst_subresource.base_layer,
                        copy.dst_subresource.num_layers
                    );
                    continue;
                }

                gl::Uniform3ui(0, copy.extent.width, copy.extent.height, copy.extent.depth);
                gl::BindImageTexture(
                    0,
                    dst_image,
                    copy.dst_subresource.base_level as i32,
                    gl::TRUE,
                    0,
                    gl::READ_WRITE,
                    gl::RGBA8UI,
                );
                gl::DispatchCompute(
                    copy.extent.width.div_ceil(16),
                    copy.extent.height.div_ceil(8),
                    copy.extent.depth,
                );
            }
            gl::MemoryBarrier(gl::TEXTURE_FETCH_BARRIER_BIT | gl::SHADER_IMAGE_ACCESS_BARRIER_BIT);
        }
        program_manager.restore_guest_compute();
    }

    /// Copy between MSAA and non-MSAA textures.
    ///
    /// Port of `UtilShaders::CopyMSAA`.
    pub fn copy_msaa(
        &mut self,
        dst_image: u32,
        src_image: u32,
        copies: &[ImageCopy],
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
        let mut program_manager = self.program_manager.lock();
        program_manager.bind_compute_program(program);
        unsafe {
            for copy in copies {
                if copy.src_subresource.base_layer != 0
                    || copy.src_subresource.num_layers != 1
                    || copy.dst_subresource.base_layer != 0
                    || copy.dst_subresource.num_layers != 1
                {
                    log::warn!(
                        "UtilShaders::copy_msaa: unsupported layer copy src_base={} src_layers={} dst_base={} dst_layers={}",
                        copy.src_subresource.base_layer,
                        copy.src_subresource.num_layers,
                        copy.dst_subresource.base_layer,
                        copy.dst_subresource.num_layers
                    );
                    continue;
                }

                gl::BindImageTexture(
                    0,
                    src_image,
                    copy.src_subresource.base_level as i32,
                    gl::TRUE,
                    0,
                    gl::READ_ONLY,
                    gl::RGBA8,
                );
                gl::BindImageTexture(
                    1,
                    dst_image,
                    copy.dst_subresource.base_level as i32,
                    gl::TRUE,
                    0,
                    gl::WRITE_ONLY,
                    gl::RGBA8,
                );

                let groups_x = copy.extent.width.div_ceil(8);
                let groups_y = copy.extent.height.div_ceil(8);
                gl::DispatchCompute(groups_x, groups_y, copy.extent.depth);
            }
            gl::MemoryBarrier(gl::TEXTURE_FETCH_BARRIER_BIT | gl::SHADER_IMAGE_ACCESS_BARRIER_BIT);
        }
        program_manager.restore_guest_compute();
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
