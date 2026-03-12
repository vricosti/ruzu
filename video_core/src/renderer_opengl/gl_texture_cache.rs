// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_texture_cache.h and gl_texture_cache.cpp
//!
//! OpenGL texture cache — manages GPU texture and image objects, framebuffers, and samplers.

/// Number of render targets.
pub const NUM_RT: usize = 8;

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
    pub fn convert_image(&mut self) {
        todo!("FormatConversionPass::ConvertImage")
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

    pub fn get_device_memory_usage(&self) -> u64 {
        todo!("TextureCacheRuntime::GetDeviceMemoryUsage")
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

    pub fn has_native_astc(&self) -> bool {
        todo!("TextureCacheRuntime::HasNativeASTC")
    }

    pub fn insert_upload_memory_barrier(&self) {
        unsafe {
            gl::MemoryBarrier(
                gl::TEXTURE_FETCH_BARRIER_BIT | gl::SHADER_IMAGE_ACCESS_BARRIER_BIT,
            );
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

    pub fn storage_handle(&mut self) -> u32 {
        todo!("Image::StorageHandle")
    }

    pub fn upload_memory(&mut self) {
        todo!("Image::UploadMemory")
    }

    pub fn download_memory(&mut self) {
        todo!("Image::DownloadMemory")
    }

    pub fn scale_up(&mut self, _ignore: bool) -> bool {
        todo!("Image::ScaleUp")
    }

    pub fn scale_down(&mut self, _ignore: bool) -> bool {
        todo!("Image::ScaleDown")
    }
}

/// An OpenGL image view.
///
/// Corresponds to `OpenGL::ImageView`.
pub struct ImageView {
    pub views: [u32; 7], // NUM_TEXTURE_TYPES
    pub default_handle: u32,
    pub internal_format: u32,
    pub buffer_size: u32,
}

impl ImageView {
    pub fn new() -> Self {
        Self {
            views: [0; 7],
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

    pub fn storage_view(&mut self, _texture_type: u32, _image_format: u32) -> u32 {
        todo!("ImageView::StorageView")
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

/// Texture cache parameters matching upstream `TextureCacheParams`.
pub struct TextureCacheParams;

impl TextureCacheParams {
    pub const ENABLE_VALIDATION: bool = true;
    pub const FRAMEBUFFER_BLITS: bool = true;
    pub const HAS_EMULATED_COPIES: bool = true;
    pub const HAS_DEVICE_MEMORY_INFO: bool = true;
    pub const IMPLEMENTS_ASYNC_DOWNLOADS: bool = true;
}
