// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_texture_cache.h and gl_texture_cache.cpp
//!
//! OpenGL texture cache -- manages GPU texture and image objects, framebuffers, and samplers.

use std::collections::HashMap;

use crate::engines::maxwell_3d::RenderTargetInfo;
use crate::framebuffer_config::FramebufferConfig;
use crate::renderer_base::GuestMemoryWriter;
use crate::shader_environment::TextureType;
use crate::surface::PixelFormat;
use crate::texture_cache::image_base::ImageBase;
use crate::texture_cache::image_view_base::ImageViewBase;
use crate::texture_cache::image_base::ImageFlagBits;
use crate::texture_cache::texture_cache_base::{
    FramebufferImageView, TextureCacheBase as CommonTextureCache,
};
use crate::texture_cache::types::{ImageId, ImageType, ImageViewId, SubresourceRange};
use crate::texture_cache::util::{full_download_copies, swizzle_image};

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
    pub num_samples: i32,
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
            num_samples: 1,
        }
    }

    pub fn handle(&self) -> u32 {
        self.current_texture
    }

    /// Port of
    /// `Image::Image(TextureCacheRuntime&, const VideoCommon::ImageInfo&, GPUVAddr, VAddr)`
    /// for the currently ported render-target path.
    pub fn from_base(base: &ImageBase) -> Self {
        let tuple = super::maxwell_to_gl::get_format_tuple(base.info.format as usize);
        let width = base.info.size.width.max(1) as i32;
        let height = base.info.size.height.max(1) as i32;
        let depth = base.info.size.depth.max(1) as i32;
        let max_host_mip_levels = 32 - base.info.size.width.max(1).leading_zeros();
        let gl_num_levels = base
            .info
            .resources
            .levels
            .min(max_host_mip_levels as i32)
            .max(1);

        let mut texture = 0;
        unsafe {
            match base.info.image_type {
                ImageType::E1D => {
                    gl::CreateTextures(gl::TEXTURE_1D, 1, &mut texture);
                    if texture != 0 {
                        gl::TextureStorage1D(texture, gl_num_levels, tuple.internal_format, width);
                    }
                }
                ImageType::E3D => {
                    gl::CreateTextures(gl::TEXTURE_3D, 1, &mut texture);
                    if texture != 0 {
                        gl::TextureStorage3D(
                            texture,
                            gl_num_levels,
                            tuple.internal_format,
                            width,
                            height,
                            depth,
                        );
                    }
                }
                ImageType::Buffer => {}
                ImageType::E2D | ImageType::Linear => {
                    gl::CreateTextures(gl::TEXTURE_2D, 1, &mut texture);
                    if texture != 0 {
                        gl::TextureStorage2D(
                            texture,
                            gl_num_levels,
                            tuple.internal_format,
                            width,
                            height,
                        );
                    }
                }
            }
        }

        Self {
            texture,
            upscaled_backup: 0,
            gl_internal_format: tuple.internal_format,
            gl_format: tuple.format,
            gl_type: tuple.gl_type,
            gl_num_levels,
            current_texture: texture,
            num_samples: base.info.num_samples as i32,
        }
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
    ///
    /// Upstream calls `glGetTextureImage(texture, level, gl_format, gl_type,
    /// size, buffer)` to read the mip level into `output`. Without this, the
    /// `TextureCache::download_memory` path has nothing to write back to
    /// guest memory and rendered framebuffers stay zero — visible to user
    /// as a black SDL window even though MK8D submitted layers correctly.
    pub fn download_memory(&mut self, output: &mut [u8], level: i32) {
        if self.current_texture == 0
            || self.gl_format == gl::NONE
            || self.gl_type == gl::NONE
            || output.is_empty()
        {
            return;
        }
        // Safety: glGetTextureImage on a valid texture; `output` length
        // must be at least `buf_size` (caller's responsibility — upstream
        // sizes via `image.unswizzled_size_bytes`). Pass output.len() as
        // bufSize so the driver clips if needed.
        unsafe {
            gl::PixelStorei(gl::PACK_ALIGNMENT, 1);
            gl::GetTextureImage(
                self.current_texture,
                level,
                self.gl_format,
                self.gl_type,
                output.len() as i32,
                output.as_mut_ptr() as *mut _,
            );
        }
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
    original_texture: u32,
    num_samples: i32,
    flat_range: SubresourceRange,
    full_range: SubresourceRange,
    is_render_target: bool,
}

impl ImageView {
    pub fn new() -> Self {
        Self {
            views: [0; NUM_TEXTURE_TYPES],
            default_handle: 0,
            internal_format: gl::NONE,
            buffer_size: 0,
            original_texture: 0,
            num_samples: 0,
            flat_range: SubresourceRange::default(),
            full_range: SubresourceRange::default(),
            is_render_target: false,
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

    /// Port of
    /// `ImageView::ImageView(TextureCacheRuntime&, const ImageViewInfo&, ImageId, Image&, ...)`
    /// for the currently ported Color2D render-target path.
    pub fn new_color_2d(base: &ImageViewBase, image: &Image) -> Self {
        let mut view = Self::new();
        view.internal_format = present_internal_format(base.format);
        view.original_texture = image.handle();
        view.num_samples = image.num_samples;
        view.flat_range = base.range;
        view.full_range = base.range;
        view.is_render_target = true;

        view.setup_view(TextureType::Color2D);
        view.default_handle = view.handle_for_texture_type(TextureType::Color2D);
        view
    }

    pub fn handle_for_texture_type(&self, handle_type: TextureType) -> u32 {
        self.handle(handle_type as usize)
    }

    fn setup_view(&mut self, view_type: TextureType) {
        let view = self.make_view(view_type, self.internal_format);
        self.views[view_type as usize] = view;
    }

    fn make_view(&self, view_type: TextureType, view_format: u32) -> u32 {
        if self.original_texture == 0 {
            return 0;
        }
        let view_range = match view_type {
            TextureType::Color1D
            | TextureType::Color2D
            | TextureType::ColorCube
            | TextureType::Color2DRect => self.flat_range,
            TextureType::ColorArray1D
            | TextureType::ColorArray2D
            | TextureType::Color3D
            | TextureType::ColorArrayCube => self.full_range,
            _ => return 0,
        };
        let target = match view_type {
            TextureType::Color1D => gl::TEXTURE_1D,
            TextureType::ColorArray1D => gl::TEXTURE_1D_ARRAY,
            TextureType::Color2D | TextureType::Color2DRect => gl::TEXTURE_2D,
            TextureType::ColorArray2D => gl::TEXTURE_2D_ARRAY,
            TextureType::Color3D => gl::TEXTURE_3D,
            TextureType::ColorCube => gl::TEXTURE_CUBE_MAP,
            TextureType::ColorArrayCube => gl::TEXTURE_CUBE_MAP_ARRAY,
            _ => return 0,
        };
        let mut view = 0;
        unsafe {
            gl::GenTextures(1, &mut view);
            if view != 0 {
                gl::TextureView(
                    view,
                    target,
                    self.original_texture,
                    view_format,
                    view_range.base.level as u32,
                    view_range.extent.levels as u32,
                    view_range.base.layer as u32,
                    view_range.extent.layers as u32,
                );
                gl::TextureParameteri(view, gl::TEXTURE_MIN_FILTER, gl::NEAREST as i32);
                gl::TextureParameteri(view, gl::TEXTURE_MAG_FILTER, gl::NEAREST as i32);
                gl::TextureParameteri(view, gl::TEXTURE_WRAP_S, gl::CLAMP_TO_EDGE as i32);
                gl::TextureParameteri(view, gl::TEXTURE_WRAP_T, gl::CLAMP_TO_EDGE as i32);
            }
        }
        view
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

/// Returns the OpenGL internal format for an image view.
///
/// Port of the `MaxwellToGL::GetFormatTuple(format).internal_format` branch
/// used by upstream `ImageView::ImageView(...)`.
pub fn present_internal_format(format: PixelFormat) -> u32 {
    super::maxwell_to_gl::get_format_tuple(format as usize).internal_format
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

/// OpenGL texture cache policy instance.
///
/// Corresponds to upstream
/// `using TextureCache = VideoCommon::TextureCache<TextureCacheParams>`.
/// The Rust base cache still owns backend-independent `ImageBase` /
/// `ImageViewBase` slots, while this OpenGL wrapper owns the backend
/// `OpenGL::ImageView` slots keyed by the same `ImageViewId`.
pub struct TextureCache {
    pub base: CommonTextureCache,
    images: HashMap<ImageId, Image>,
    image_views: HashMap<ImageViewId, ImageView>,
    framebuffers: HashMap<ImageViewId, TextureCacheFramebuffer>,
}

/// OpenGL backend result of `TextureCache<P>::TryFindFramebufferImageView`.
pub struct FramebufferImageViewOpenGL {
    pub view_id: ImageViewId,
    pub display_texture: u32,
    pub width: u32,
    pub height: u32,
    pub scaled: bool,
}

impl TextureCache {
    pub fn new() -> Self {
        Self {
            base: CommonTextureCache::new(),
            images: HashMap::new(),
            image_views: HashMap::new(),
            framebuffers: HashMap::new(),
        }
    }

    pub fn set_guest_memory_writer(&mut self, writer: GuestMemoryWriter) {
        self.base.set_guest_memory_writer(writer);
    }

    /// Port of `TextureCache<P>::DownloadMemory` for `TextureCacheParams =
    /// OpenGL`. The base implementation uses an `image_downloader` closure
    /// callback so the borrow-checker doesn't let it reach into the backend
    /// image table; ruzu inverts the call here so the OpenGL wrapper does
    /// the per-image loop in user code with direct access to
    /// `self.images: HashMap<ImageId, Image>`.
    ///
    /// Without this, `gl_rasterizer::flush_region` → base
    /// `download_memory` early-returns at the missing-downloader guard,
    /// guest framebuffer pages stay zero, and the compositor reads zeros
    /// → black SDL window.
    pub fn download_memory(&mut self, cpu_addr: u64, size: usize) {
        let Some(writer) = self.base.guest_memory_writer.as_ref().cloned() else {
            return;
        };

        // Snapshot the images we want to download, filtered + sorted exactly
        // as the base path used to do internally.
        let mut images = self.base.collect_images_in_region(cpu_addr, size);
        images.retain(|&id| self.base.slot_images[id].is_safe_download());
        if images.is_empty() {
            return;
        }
        for &id in &images {
            self.base.slot_images[id]
                .flags
                .remove(ImageFlagBits::GPU_MODIFIED);
        }
        images.sort_by_key(|&id| self.base.slot_images[id].modification_tick);

        for image_id in images {
            // Make sure the backend image slot exists for this base image —
            // `update_render_targets_from_draw_state` typically pre-populates
            // it, but defensive insertion here keeps offscreen downloads
            // working even before any draw has touched the slot.
            let base_image = self.base.slot_images[image_id].clone();
            let backend_image = self
                .images
                .entry(image_id)
                .or_insert_with(|| Image::from_base(&base_image));

            // Drain the GPU texture into a staging buffer sized by the
            // upstream `unswizzled_size_bytes` (the in-memory layout the
            // guest expects).
            let buffer_size = base_image.unswizzled_size_bytes as usize;
            if buffer_size == 0 {
                continue;
            }
            let mut staging = vec![0u8; buffer_size];
            backend_image.download_memory(&mut staging, 0);

            // Swizzle (or pitch-copy) the staging buffer back into guest
            // memory at `image.cpu_addr`. For linear pitch surfaces (the
            // common framebuffer case) this is a row-by-row block copy;
            // for block-linear surfaces it walks `swizzle_texture`.
            let copies = full_download_copies(&base_image.info);
            swizzle_image(
                writer.as_ref(),
                base_image.cpu_addr,
                &base_image.info,
                &copies,
                &staging,
                &mut self.base.swizzle_data_buffer,
            );
        }
    }

    pub fn write_memory(&mut self, cpu_addr: u64, size: usize) {
        self.base.write_memory(cpu_addr, size);
    }

    pub fn unmap_memory(&mut self, cpu_addr: u64, size: usize) {
        self.framebuffers.clear();
        self.image_views.clear();
        self.images.clear();
        self.base.unmap_memory(cpu_addr, size);
    }

    pub fn tick_frame(&mut self) {
        self.base.tick_frame();
    }

    pub fn should_wait_async_flushes(&self) -> bool {
        self.base.should_wait_async_flushes()
    }

    pub fn has_uncommitted_flushes(&self) -> bool {
        self.base.has_uncommitted_flushes()
    }

    pub fn pop_async_flushes(&mut self) {
        self.base.pop_async_flushes();
    }

    pub fn commit_async_flushes(&mut self) {
        self.base.commit_async_flushes();
    }

    pub fn update_render_targets_from_draw_state(
        &mut self,
        draw_state: &crate::engines::draw_manager::DrawState,
        gpu_to_cpu: impl FnMut(crate::texture_cache::image_base::GPUVAddr) -> Option<u64>,
    ) {
        self.base
            .update_render_targets_from_draw_state(draw_state, gpu_to_cpu);
        for (image_id, image) in self.base.slot_images.iter() {
            self.images
                .entry(image_id)
                .or_insert_with(|| Image::from_base(image));
        }
    }

    /// Port of `TextureCache<P>::TryFindFramebufferImageView` for
    /// `TextureCacheParams = OpenGL`.
    pub fn try_find_framebuffer_image_view(
        &mut self,
        config: &FramebufferConfig,
        cpu_addr: u64,
    ) -> Option<FramebufferImageViewOpenGL> {
        let framebuffer_view = self
            .base
            .try_find_framebuffer_image_view(config, cpu_addr)?;
        let FramebufferImageView {
            view_id,
            view,
            scaled,
        } = framebuffer_view;
        let image_id = view.image_id;
        let backend_image = self.images.entry(image_id).or_insert_with(|| {
            let image = &self.base.slot_images[image_id];
            Image::from_base(image)
        });
        let backend_view = self
            .image_views
            .entry(view_id)
            .or_insert_with(|| ImageView::new_color_2d(&view, backend_image));
        let display_texture = backend_view.handle_for_texture_type(TextureType::Color2D);
        if display_texture == 0 {
            return None;
        }
        Some(FramebufferImageViewOpenGL {
            view_id,
            display_texture,
            width: view.size.width,
            height: view.size.height,
            scaled,
        })
    }

    /// OpenGL counterpart of upstream `TextureCache<P>::GetFramebuffer()` for
    /// the currently ported single-color-target clear/present path.
    pub fn framebuffer_for_render_target(
        &mut self,
        rt: &RenderTargetInfo,
    ) -> Option<(u32, u32, u32)> {
        if rt.address == 0 || rt.width == 0 || rt.height == 0 {
            return None;
        }

        let (image_id, image_base) = self.base.slot_images.iter().find(|(_, image)| {
            image.gpu_addr == rt.address
                && image.info.size.width == rt.width
                && image.info.size.height == rt.height
                && !image.image_view_ids.is_empty()
        })?;
        let view_id = image_base.image_view_ids[0];
        let view_base = self.base.slot_image_views[view_id].clone();

        let backend_image = self
            .images
            .entry(image_id)
            .or_insert_with(|| Image::from_base(image_base));
        let backend_view = self
            .image_views
            .entry(view_id)
            .or_insert_with(|| ImageView::new_color_2d(&view_base, backend_image));
        let texture = backend_view.handle_for_texture_type(TextureType::Color2D);
        if texture == 0 {
            return None;
        }

        let framebuffer = self.framebuffers.entry(view_id).or_insert_with(|| {
            let mut framebuffer = 0;
            unsafe {
                gl::CreateFramebuffers(1, &mut framebuffer);
                if framebuffer != 0 {
                    gl::NamedFramebufferTexture(framebuffer, gl::COLOR_ATTACHMENT0, texture, 0);
                    gl::BindFramebuffer(gl::DRAW_FRAMEBUFFER, framebuffer);
                    gl::DrawBuffer(gl::COLOR_ATTACHMENT0);
                }
            }
            TextureCacheFramebuffer {
                framebuffer,
                buffer_bits: gl::COLOR_BUFFER_BIT,
            }
        });
        let handle = framebuffer.handle();
        (handle != 0).then_some((handle, view_base.size.width, view_base.size.height))
    }
}

impl Default for TextureCache {
    fn default() -> Self {
        Self::new()
    }
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

    #[test]
    fn present_internal_format_matches_basic_surface_formats() {
        assert_eq!(
            present_internal_format(PixelFormat::A8B8G8R8Unorm),
            gl::RGBA8
        );
        assert_eq!(
            present_internal_format(PixelFormat::B8G8R8A8Unorm),
            gl::RGBA8
        );
        assert_eq!(
            present_internal_format(PixelFormat::A8B8G8R8Srgb),
            gl::SRGB8_ALPHA8
        );
        assert_eq!(
            present_internal_format(PixelFormat::R5G6B5Unorm),
            gl::RGB565
        );
    }
}
