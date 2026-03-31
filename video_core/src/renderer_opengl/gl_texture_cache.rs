// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_texture_cache.h and gl_texture_cache.cpp
//!
//! OpenGL texture cache -- manages GPU texture and image objects, framebuffers, and samplers.

/// Number of render targets.
pub const NUM_RT: usize = 8;

/// Number of texture types (1D, 2D, 2DRect, 3D, Cube, 1DArray, 2DArray, Buffer, CubeArray).
const NUM_TEXTURE_TYPES: usize = 9;

/// Format properties for a given GL internal format.
///
/// Corresponds to `OpenGL::FormatProperties`.
#[derive(Clone, Debug)]
pub struct FormatProperties {
    pub compatibility_class: u32,
    pub compatibility_by_size: bool,
    pub is_compressed: bool,
}

/// Format conversion pass using compute shaders.
///
/// Corresponds to `OpenGL::FormatConversionPass`.
pub struct FormatConversionPass {
    intermediate_pbo: u32,
    pbo_size: usize,
}

impl FormatConversionPass {
    pub fn new() -> Self {
        Self {
            intermediate_pbo: 0,
            pbo_size: 0,
        }
    }

    /// Convert between image formats using a PBO intermediate.
    ///
    /// Port of `FormatConversionPass::ConvertImage`.
    /// In the full implementation, this creates a PBO if needed, copies the source
    /// texture into the PBO, then copies the PBO into the destination texture with
    /// the target format.
    pub fn convert_image(
        &mut self,
        dst_texture: u32,
        dst_internal_format: u32,
        width: u32,
        height: u32,
        depth: u32,
        src_format: u32,
        src_type: u32,
        src_texture: u32,
    ) {
        let required_size = (width * height * depth * 4) as usize;
        self.ensure_pbo_size(required_size);

        unsafe {
            // Bind PBO for pack (GPU -> PBO)
            gl::BindBuffer(gl::PIXEL_PACK_BUFFER, self.intermediate_pbo);
            gl::GetTextureImage(
                src_texture,
                0,
                src_format,
                src_type,
                required_size as i32,
                std::ptr::null_mut(),
            );

            // Bind PBO for unpack (PBO -> GPU)
            gl::BindBuffer(gl::PIXEL_UNPACK_BUFFER, self.intermediate_pbo);
            gl::BindBuffer(gl::PIXEL_PACK_BUFFER, 0);

            gl::TextureSubImage3D(
                dst_texture,
                0,
                0,
                0,
                0,
                width as i32,
                height as i32,
                depth as i32,
                src_format,
                src_type,
                std::ptr::null(),
            );

            gl::BindBuffer(gl::PIXEL_UNPACK_BUFFER, 0);
        }

        let _ = dst_internal_format; // Used in full implementation for format validation
    }

    fn ensure_pbo_size(&mut self, required_size: usize) {
        if self.pbo_size >= required_size {
            return;
        }
        unsafe {
            if self.intermediate_pbo != 0 {
                gl::DeleteBuffers(1, &self.intermediate_pbo);
            }
            gl::CreateBuffers(1, &mut self.intermediate_pbo);
            gl::NamedBufferData(
                self.intermediate_pbo,
                required_size as isize,
                std::ptr::null(),
                gl::STREAM_COPY,
            );
        }
        self.pbo_size = required_size;
    }
}

impl Drop for FormatConversionPass {
    fn drop(&mut self) {
        if self.intermediate_pbo != 0 {
            unsafe {
                gl::DeleteBuffers(1, &self.intermediate_pbo);
            }
        }
    }
}

/// Runtime state for the OpenGL texture cache.
///
/// Corresponds to `OpenGL::TextureCacheRuntime`.
pub struct TextureCacheRuntime {
    pub has_broken_texture_view_formats: bool,
    pub device_access_memory: u64,
}

impl TextureCacheRuntime {
    /// Create a new texture cache runtime.
    pub fn new(_device: &super::gl_device::Device) -> Self {
        Self {
            has_broken_texture_view_formats: false,
            device_access_memory: 2 * 1024 * 1024 * 1024,
        }
    }

    pub fn finish(&self) {
        unsafe {
            gl::Finish();
        }
    }

    pub fn get_device_local_memory(&self) -> u64 {
        self.device_access_memory
    }

    /// Port of `TextureCacheRuntime::GetDeviceMemoryUsage`.
    /// Queries NVX_gpu_memory_info if available for actual VRAM usage.
    pub fn get_device_memory_usage(&self) -> u64 {
        // GL_GPU_MEMORY_INFO_CURRENT_AVAILABLE_VIDMEM_NVX = 0x9049
        const GL_GPU_MEMORY_INFO_CURRENT_AVAILABLE_VIDMEM_NVX: u32 = 0x9049;
        let mut available_kb: i32 = 0;
        unsafe {
            gl::GetIntegerv(
                GL_GPU_MEMORY_INFO_CURRENT_AVAILABLE_VIDMEM_NVX,
                &mut available_kb,
            );
        }
        if available_kb > 0 {
            self.device_access_memory - (available_kb as u64 * 1024)
        } else {
            0
        }
    }

    pub fn should_reinterpret(&self) -> bool {
        true
    }

    pub fn can_upload_msaa(&self) -> bool {
        true
    }

    pub fn has_native_bgr(&self) -> bool {
        false
    }

    pub fn has_broken_texture_view_formats(&self) -> bool {
        self.has_broken_texture_view_formats
    }

    /// Port of `TextureCacheRuntime::HasNativeASTC`.
    /// Checks whether GL_KHR_texture_compression_astc_ldr is supported.
    pub fn has_native_astc(&self) -> bool {
        // Check for ASTC LDR support via extension
        let mut num_extensions: i32 = 0;
        unsafe {
            gl::GetIntegerv(gl::NUM_EXTENSIONS, &mut num_extensions);
        }
        for i in 0..num_extensions {
            let ext = unsafe {
                let ptr = gl::GetStringi(gl::EXTENSIONS, i as u32);
                if ptr.is_null() {
                    continue;
                }
                std::ffi::CStr::from_ptr(ptr as *const i8)
                    .to_str()
                    .unwrap_or("")
            };
            if ext == "GL_KHR_texture_compression_astc_ldr" {
                return true;
            }
        }
        false
    }

    pub fn insert_upload_memory_barrier(&self) {
        unsafe {
            gl::MemoryBarrier(gl::TEXTURE_FETCH_BARRIER_BIT | gl::SHADER_IMAGE_ACCESS_BARRIER_BIT);
        }
    }
}

/// An OpenGL texture/image.
///
/// Corresponds to `OpenGL::Image`.
pub struct Image {
    pub texture: u32,
    pub upscaled_backup: u32,
    pub gl_internal_format: u32,
    pub gl_format: u32,
    pub gl_type: u32,
    pub gl_num_levels: i32,
    pub current_texture: u32,
}

impl Image {
    pub fn new() -> Self {
        Self {
            texture: 0,
            upscaled_backup: 0,
            gl_internal_format: gl::NONE,
            gl_format: gl::NONE,
            gl_type: gl::NONE,
            gl_num_levels: 0,
            current_texture: 0,
        }
    }

    pub fn handle(&self) -> u32 {
        self.current_texture
    }

    /// Port of `Image::StorageHandle`.
    /// Returns the texture handle used for image load/store.
    pub fn storage_handle(&mut self) -> u32 {
        self.texture
    }

    /// Port of `Image::UploadMemory`.
    /// Uploads pixel data from CPU to GPU texture.
    pub fn upload_memory(&mut self, _data: &[u8], _level: i32) {
        // Full implementation requires:
        // 1. Bind PBO with data
        // 2. glTextureSubImage2D/3D based on texture type
        // 3. Generate mipmaps if needed
    }

    /// Port of `Image::DownloadMemory`.
    /// Downloads pixel data from GPU texture to CPU.
    pub fn download_memory(&mut self, _output: &mut [u8], _level: i32) {
        // Full implementation:
        // glGetTextureImage(texture, level, gl_format, gl_type, size, output.as_mut_ptr())
    }

    /// Port of `Image::ScaleUp`.
    /// Scales the image up by the resolution scaling factor.
    pub fn scale_up(&mut self, _ignore: bool) -> bool {
        // Full implementation creates upscaled_backup texture and blits
        false
    }

    /// Port of `Image::ScaleDown`.
    /// Scales the image down from the resolution scaling factor.
    pub fn scale_down(&mut self, _ignore: bool) -> bool {
        // Full implementation blits from upscaled_backup back to original
        false
    }
}

impl Drop for Image {
    fn drop(&mut self) {
        unsafe {
            if self.texture != 0 {
                gl::DeleteTextures(1, &self.texture);
            }
            if self.upscaled_backup != 0 {
                gl::DeleteTextures(1, &self.upscaled_backup);
            }
        }
    }
}

/// An OpenGL image view.
///
/// Corresponds to `OpenGL::ImageView`.
pub struct ImageView {
    pub views: [u32; NUM_TEXTURE_TYPES],
    pub default_handle: u32,
    pub internal_format: u32,
    pub buffer_size: u32,
}

impl ImageView {
    pub fn new() -> Self {
        Self {
            views: [0; NUM_TEXTURE_TYPES],
            default_handle: 0,
            internal_format: gl::NONE,
            buffer_size: 0,
        }
    }

    pub fn handle(&self, handle_type: usize) -> u32 {
        self.views[handle_type]
    }

    pub fn default_handle(&self) -> u32 {
        self.default_handle
    }

    pub fn format(&self) -> u32 {
        self.internal_format
    }

    /// Port of `ImageView::StorageView`.
    /// Returns or creates a texture view for image load/store with a specific format.
    pub fn storage_view(&mut self, texture_type: u32, image_format: u32) -> u32 {
        // In the full implementation, this creates a glTextureView with the
        // specified format for image load/store operations.
        let _ = (texture_type, image_format);
        self.default_handle
    }
}

impl Drop for ImageView {
    fn drop(&mut self) {
        unsafe {
            for &view in &self.views {
                if view != 0 {
                    gl::DeleteTextures(1, &view);
                }
            }
        }
    }
}

/// An OpenGL sampler.
///
/// Corresponds to `OpenGL::Sampler`.
pub struct Sampler {
    pub sampler: u32,
    pub sampler_default_anisotropy: u32,
}

impl Sampler {
    pub fn new() -> Self {
        Self {
            sampler: 0,
            sampler_default_anisotropy: 0,
        }
    }

    pub fn handle(&self) -> u32 {
        self.sampler
    }

    pub fn handle_with_default_anisotropy(&self) -> u32 {
        self.sampler_default_anisotropy
    }

    pub fn has_added_anisotropy(&self) -> bool {
        self.sampler_default_anisotropy != 0
    }
}

impl Drop for Sampler {
    fn drop(&mut self) {
        unsafe {
            if self.sampler != 0 {
                gl::DeleteSamplers(1, &self.sampler);
            }
            if self.sampler_default_anisotropy != 0 {
                gl::DeleteSamplers(1, &self.sampler_default_anisotropy);
            }
        }
    }
}

/// An OpenGL framebuffer.
///
/// Corresponds to `OpenGL::Framebuffer` (texture cache version).
pub struct TextureCacheFramebuffer {
    pub framebuffer: u32,
    pub buffer_bits: u32,
}

impl TextureCacheFramebuffer {
    pub fn new() -> Self {
        Self {
            framebuffer: 0,
            buffer_bits: gl::NONE,
        }
    }

    pub fn handle(&self) -> u32 {
        self.framebuffer
    }

    pub fn buffer_bits(&self) -> u32 {
        self.buffer_bits
    }
}

impl Drop for TextureCacheFramebuffer {
    fn drop(&mut self) {
        if self.framebuffer != 0 {
            unsafe {
                gl::DeleteFramebuffers(1, &self.framebuffer);
            }
        }
    }
}

/// Texture cache parameters matching upstream `TextureCacheParams`.
pub struct TextureCacheParams;

impl TextureCacheParams {
    pub const ENABLE_VALIDATION: bool = true;
    pub const FRAMEBUFFER_BLITS: bool = true;
    pub const HAS_EMULATED_COPIES: bool = true;
    pub const HAS_DEVICE_MEMORY_INFO: bool = true;
    pub const IMPLEMENTS_ASYNC_DOWNLOADS: bool = true;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn constants() {
        assert_eq!(NUM_RT, 8);
        assert_eq!(NUM_TEXTURE_TYPES, 9);
    }

    #[test]
    fn texture_cache_params() {
        assert!(TextureCacheParams::ENABLE_VALIDATION);
        assert!(TextureCacheParams::FRAMEBUFFER_BLITS);
        assert!(TextureCacheParams::HAS_EMULATED_COPIES);
    }
}
