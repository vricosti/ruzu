// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_texture_cache.h and gl_texture_cache.cpp
//!
//! OpenGL texture cache -- manages GPU texture and image objects, framebuffers, and samplers.

use std::collections::HashMap;

use crate::engines::draw_manager::Maxwell3DRenderTargets;
use crate::engines::maxwell_3d::RenderTargetInfo;
use crate::framebuffer_config::FramebufferConfig;
use crate::renderer_base::GuestMemoryWriter;
use crate::shader_environment::TextureType;
use crate::surface::PixelFormat;
use crate::texture_cache::image_base::{ImageBase, ImageFlagBits, ImageMapView};
use crate::texture_cache::image_info::ImageInfo;
use crate::texture_cache::image_view_base::{ImageViewBase, ImageViewFlagBits};
use crate::texture_cache::image_view_info::ImageViewInfo;
use crate::texture_cache::texture_cache_base::{
    FramebufferImageView, TextureCacheBase as CommonTextureCache,
};
use crate::texture_cache::types::{
    ImageId, ImageType, ImageViewId, ImageViewType, Offset2D, Region2D, SubresourceRange,
};
use crate::texture_cache::util::{
    full_download_copies, map_size_bytes, swizzle_image, unswizzle_image,
};

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
    ///
    /// `device_access_memory` mirrors the upstream
    /// `TextureCacheRuntime::TextureCacheRuntime` budget: NVX total +
    /// 512 MiB when the extension is present, else a 2 GiB minimum. Kept
    /// in sync with the buffer-cache runtime (gl_buffer_cache.cpp:139).
    pub fn new(device: &super::gl_device::Device) -> Self {
        const HALF_GIB: u64 = 512 * 1024 * 1024;
        let device_access_memory = if device.can_report_memory() {
            device.get_current_dedicated_video_memory() + HALF_GIB
        } else {
            2 * 1024 * 1024 * 1024
        };
        Self {
            has_broken_texture_view_formats: false,
            device_access_memory,
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

    /// Port of `TextureCacheRuntime::GetDeviceMemoryUsage`. Mirrors the
    /// buffer-cache port: queries `TOTAL_AVAILABLE_MEMORY_NVX` (0x9048),
    /// the same constant upstream uses; subtraction stays non-negative
    /// because the ctor sized `device_access_memory` to `total + 512MiB`.
    pub fn get_device_memory_usage(&self) -> u64 {
        const GL_GPU_MEMORY_INFO_TOTAL_AVAILABLE_MEMORY_NVX: u32 = 0x9048;
        let mut total_kb: i32 = 0;
        unsafe {
            gl::GetIntegerv(GL_GPU_MEMORY_INFO_TOTAL_AVAILABLE_MEMORY_NVX, &mut total_kb);
        }
        if total_kb > 0 {
            self.device_access_memory
                .saturating_sub((total_kb as u64) * 1024)
        } else {
            2 * 1024 * 1024 * 1024
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

/// Port of upstream `OpenGL::ImageTarget(const VideoCommon::ImageInfo& info)`
/// (gl_texture_cache.cpp:70-88). Maps ruzu `ImageType` to the GL target
/// that the corresponding texture object lives under. Note that 1D and 2D
/// non-multisampled images map to the *_ARRAY targets — upstream uses
/// `GL_TEXTURE_1D_ARRAY` / `GL_TEXTURE_2D_ARRAY` even for layer-count=1
/// images so the same target works for arrays without re-allocation.
fn image_target(info: &ImageInfo) -> u32 {
    match info.image_type {
        ImageType::E1D => gl::TEXTURE_1D_ARRAY,
        ImageType::E2D => {
            if info.num_samples > 1 {
                gl::TEXTURE_2D_MULTISAMPLE_ARRAY
            } else {
                gl::TEXTURE_2D_ARRAY
            }
        }
        ImageType::E3D => gl::TEXTURE_3D,
        // Upstream: `case ImageType::Linear: return GL_TEXTURE_2D_ARRAY;`
        ImageType::Linear => gl::TEXTURE_2D_ARRAY,
        ImageType::Buffer => gl::TEXTURE_BUFFER,
    }
}

/// Port of upstream `OpenGL::ImageTarget(Shader::TextureType, int)`
/// (gl_texture_cache.cpp:90-113). Maps a shader texture view type to the
/// GL target used for `glTextureView`, including multisample targets.
fn image_view_target(view_type: TextureType, num_samples: i32) -> u32 {
    let is_multisampled = num_samples > 1;
    match view_type {
        TextureType::Color1D => gl::TEXTURE_1D,
        TextureType::Color2D | TextureType::Color2DRect => {
            if is_multisampled {
                gl::TEXTURE_2D_MULTISAMPLE
            } else {
                gl::TEXTURE_2D
            }
        }
        TextureType::ColorCube => gl::TEXTURE_CUBE_MAP,
        TextureType::Color3D => gl::TEXTURE_3D,
        TextureType::ColorArray1D => gl::TEXTURE_1D_ARRAY,
        TextureType::ColorArray2D => {
            if is_multisampled {
                gl::TEXTURE_2D_MULTISAMPLE_ARRAY
            } else {
                gl::TEXTURE_2D_ARRAY
            }
        }
        TextureType::ColorArrayCube => gl::TEXTURE_CUBE_MAP_ARRAY,
        TextureType::Buffer => gl::TEXTURE_BUFFER,
        _ => gl::NONE,
    }
}

/// Port of upstream `OpenGL::MakeImage` (gl_texture_cache.cpp:363-406).
/// Allocates a GL texture sized to `info` via `glCreateTextures(target)` +
/// `glTextureStorage{2,3}D(...)`. Returns the GL handle (0 if `target ==
/// GL_TEXTURE_BUFFER`, since buffer textures don't allocate storage here).
///
/// MULTISAMPLE_ARRAY uses `glTextureStorage3DMultisample` with the
/// effective per-sample width/height (`width >> samples_x` etc.) via the
/// shared `texture_cache::samples_helper::samples_log2` port.
fn make_image(info: &ImageInfo, gl_internal_format: u32, gl_num_levels: i32, target: u32) -> u32 {
    // Upstream uses GLsizei (i32) here; ruzu stores extents as u32. Casts
    // are lossless for realistic texture sizes (<= 16384). No defensive
    // `.max(1)` — upstream lets the GL driver reject invalid extents.
    let width = info.size.width as i32;
    let height = info.size.height as i32;
    let depth = info.size.depth as i32;
    let num_layers = info.resources.layers;
    let num_samples = info.num_samples as i32;

    if target == gl::TEXTURE_BUFFER {
        // Upstream allocates the texture on Buffer too (skipping
        // glCreateTextures), then leaves `handle = 0`. The buffer-storage
        // binding happens in `Image::UploadMemory`. Match by returning 0.
        return 0;
    }

    let mut handle = 0u32;
    unsafe {
        gl::CreateTextures(target, 1, &mut handle);
        if handle == 0 {
            return 0;
        }
        match target {
            gl::TEXTURE_1D_ARRAY => {
                gl::TextureStorage2D(handle, gl_num_levels, gl_internal_format, width, num_layers);
            }
            gl::TEXTURE_2D_ARRAY => {
                gl::TextureStorage3D(
                    handle,
                    gl_num_levels,
                    gl_internal_format,
                    width,
                    height,
                    num_layers,
                );
            }
            gl::TEXTURE_2D_MULTISAMPLE_ARRAY => {
                // Upstream calls `SamplesLog2(info.num_samples)` from
                // video_core/texture_cache/samples_helper.h. The Rust port
                // lives at `texture_cache::samples_helper::samples_log2`.
                let (samples_x, samples_y) =
                    crate::texture_cache::samples_helper::samples_log2(num_samples);
                gl::TextureStorage3DMultisample(
                    handle,
                    num_samples,
                    gl_internal_format,
                    width >> samples_x,
                    height >> samples_y,
                    num_layers,
                    gl::FALSE,
                );
            }
            gl::TEXTURE_RECTANGLE => {
                gl::TextureStorage2D(handle, gl_num_levels, gl_internal_format, width, height);
            }
            gl::TEXTURE_3D => {
                gl::TextureStorage3D(
                    handle,
                    gl_num_levels,
                    gl_internal_format,
                    width,
                    height,
                    depth,
                );
            }
            _ => {
                log::warn!(
                    "gl_texture_cache::make_image: unhandled target=0x{:x}",
                    target
                );
            }
        }
    }
    handle
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
    /// Snapshot of `ImageInfo::image_type` taken at materialisation.
    /// Used by `upload_memory` / `download_memory` to pick the right
    /// `glTextureSubImage{2,3}D` variant per upstream's switch on
    /// `info.type`. Upstream `Image` inherits from `ImageBase` so the
    /// field is `info.type` directly; ruzu carries an explicit snapshot.
    pub image_type: ImageType,
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
            image_type: ImageType::E2D,
        }
    }

    pub fn handle(&self) -> u32 {
        self.current_texture
    }

    /// Port of `OpenGL::Image::Image(TextureCacheRuntime&, const VideoCommon::ImageInfo&,
    /// GPUVAddr, VAddr)` (gl_texture_cache.cpp:692-728).
    ///
    /// Allocates the backing GL texture via `MakeImage` (`image_target` +
    /// `glTextureStorage*D` per dimensionality). The async-decode / accelerated-
    /// upload / converted-format branches upstream gate on `runtime->device`
    /// queries (HasNativeBgr, ASTC support, etc.) — they're not wired yet, so
    /// the converted-format / SRGB / ASTC re-targeting upstream applies at
    /// lines 700-718 is skipped here. Once the device-cap port lands those
    /// branches plug in at the same point.
    pub fn from_base(base: &ImageBase) -> Self {
        let tuple = super::maxwell_to_gl::get_format_tuple(base.info.format as usize);
        // Upstream: `gl_num_levels = std::min(info.resources.levels,
        //                                     std::bit_width(info.size.width));`
        // bit_width(x) = floor(log2(x)) + 1 = 32 - x.leading_zeros() for u32.
        let max_host_mip_levels = if base.info.size.width == 0 {
            0
        } else {
            32 - base.info.size.width.leading_zeros() as i32
        };
        let gl_num_levels = base.info.resources.levels.min(max_host_mip_levels).max(1);

        let target = image_target(&base.info);
        let texture = make_image(&base.info, tuple.internal_format, gl_num_levels, target);

        if std::env::var_os("RUZU_FORCE_MAX_LEVEL_0").is_some() && texture != 0 {
            unsafe {
                let actual_levels = (gl_num_levels - 1).max(0);
                gl::TextureParameteri(texture, gl::TEXTURE_BASE_LEVEL, 0);
                gl::TextureParameteri(texture, gl::TEXTURE_MAX_LEVEL, actual_levels);
            }
        }

        if std::env::var_os("RUZU_TRACE_IMAGE_LIFECYCLE").is_some() {
            let mut sample = [0u8; 16];
            if texture != 0 {
                unsafe {
                    gl::GetTextureSubImage(
                        texture,
                        0,
                        0,
                        0,
                        0,
                        2,
                        2,
                        1,
                        gl::RGBA,
                        gl::UNSIGNED_BYTE,
                        sample.len() as i32,
                        sample.as_mut_ptr() as *mut _,
                    );
                }
            }
            // Optional storage-functionality test:
            // - Force the storage to a known value via glClearTexImage, then
            //   re-read it. If the texture's storage is functional, the read
            //   should return the clear value. If broken, the read returns 0.
            let mut after_clear = [0u8; 16];
            let mut clear_err: u32 = 0;
            if std::env::var_os("RUZU_PROBE_IMAGE_STORAGE").is_some() && texture != 0 {
                let red: [u8; 4] = [0xAB, 0xCD, 0xEF, 0x42];
                unsafe {
                    while gl::GetError() != gl::NO_ERROR {}
                    gl::ClearTexImage(
                        texture,
                        0,
                        gl::RGBA,
                        gl::UNSIGNED_BYTE,
                        red.as_ptr() as *const _,
                    );
                    clear_err = gl::GetError();
                    gl::GetTextureSubImage(
                        texture,
                        0,
                        0,
                        0,
                        0,
                        2,
                        2,
                        1,
                        gl::RGBA,
                        gl::UNSIGNED_BYTE,
                        after_clear.len() as i32,
                        after_clear.as_mut_ptr() as *mut _,
                    );
                }
            }
            log::warn!(
                "[IMAGE_NEW] gpu=0x{:X} cpu=0x{:X} {}x{} fmt={:?} texture={} levels={} target=0x{:X} initial_bytes={:02X?} after_red_clear={:02X?} clear_err=0x{:X}",
                base.gpu_addr,
                base.cpu_addr,
                base.info.size.width,
                base.info.size.height,
                base.info.format,
                texture,
                gl_num_levels,
                target,
                sample,
                after_clear,
                clear_err,
            );
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
            image_type: base.info.image_type,
        }
    }

    /// Port of `Image::StorageHandle`.
    /// Returns the texture handle used for image load/store.
    pub fn storage_handle(&mut self) -> u32 {
        self.texture
    }

    /// Port of `Image::UploadMemory(GLuint buffer_handle, size_t
    /// buffer_offset, std::span<const BufferImageCopy>)`
    /// (gl_texture_cache.cpp:734-765).
    ///
    /// Binds the supplied PBO as the unpack source, flushes the mapped
    /// range so the driver sees the staging writes, then walks `copies`
    /// applying per-slice `glTextureSubImage{2,3}D` (or compressed
    /// variant when `gl_format == GL_NONE`). The pixel-store state for
    /// `GL_UNPACK_ROW_LENGTH` / `GL_UNPACK_IMAGE_HEIGHT` is updated
    /// only when it changes, mirroring upstream's cached comparison.
    ///
    /// The rescale path (`ScaleDown` / `ScaleUp`) upstream wraps the
    /// upload is omitted — ruzu hasn't ported rescaling yet.
    pub fn upload_memory(
        &mut self,
        buffer_handle: u32,
        buffer_offset: usize,
        copies: &[crate::texture_cache::types::BufferImageCopy],
    ) {
        if self.texture == 0 {
            return;
        }
        unsafe {
            gl::BindBuffer(gl::PIXEL_UNPACK_BUFFER, buffer_handle);
            gl::PixelStorei(gl::UNPACK_ALIGNMENT, 1);
        }
        let mut current_row_length: u32 = u32::MAX;
        let mut current_image_height: u32 = u32::MAX;
        for copy in copies {
            if copy.image_subresource.base_level >= self.gl_num_levels {
                continue;
            }
            if current_row_length != copy.buffer_row_length {
                current_row_length = copy.buffer_row_length;
                unsafe {
                    gl::PixelStorei(gl::UNPACK_ROW_LENGTH, current_row_length as i32);
                }
            }
            if current_image_height != copy.buffer_image_height {
                current_image_height = copy.buffer_image_height;
                unsafe {
                    gl::PixelStorei(gl::UNPACK_IMAGE_HEIGHT, current_image_height as i32);
                }
            }
            self.copy_buffer_to_image(copy, buffer_offset);
        }
        unsafe {
            gl::BindBuffer(gl::PIXEL_UNPACK_BUFFER, 0);
        }
    }

    /// Port of `Image::CopyBufferToImage` (gl_texture_cache.cpp:853-907).
    /// Per `image_type` dispatch to the right `glTextureSubImage*` call.
    /// Compressed formats (`gl_format == GL_NONE`) use the
    /// `glCompressedTextureSubImage*` variants. 1D and 2D/Linear share
    /// the 2D/3D APIs respectively (upstream uses 2D-array semantics for
    /// 1D and 2D-array for 2D).
    fn copy_buffer_to_image(
        &self,
        copy: &crate::texture_cache::types::BufferImageCopy,
        buffer_offset: usize,
    ) {
        let is_compressed = self.gl_format == gl::NONE;
        let offset = (copy.buffer_offset + buffer_offset) as *const std::ffi::c_void;
        let level = copy.image_subresource.base_level;
        let base_layer = copy.image_subresource.base_layer;
        let num_layers = copy.image_subresource.num_layers;
        let width = copy.image_extent.width as i32;
        let height = copy.image_extent.height as i32;
        let depth = copy.image_extent.depth as i32;
        let buf_size = copy.buffer_size as i32;
        let ox = copy.image_offset.x;
        let oy = copy.image_offset.y;
        let oz = copy.image_offset.z;
        unsafe {
            match self.image_type {
                ImageType::E1D => {
                    if is_compressed {
                        gl::CompressedTextureSubImage2D(
                            self.texture,
                            level,
                            ox,
                            base_layer,
                            width,
                            num_layers,
                            self.gl_internal_format,
                            buf_size,
                            offset,
                        );
                    } else {
                        gl::TextureSubImage2D(
                            self.texture,
                            level,
                            ox,
                            base_layer,
                            width,
                            num_layers,
                            self.gl_format,
                            self.gl_type,
                            offset,
                        );
                    }
                }
                ImageType::E2D | ImageType::Linear => {
                    if is_compressed {
                        gl::CompressedTextureSubImage3D(
                            self.texture,
                            level,
                            ox,
                            oy,
                            base_layer,
                            width,
                            height,
                            num_layers,
                            self.gl_internal_format,
                            buf_size,
                            offset,
                        );
                    } else {
                        gl::TextureSubImage3D(
                            self.texture,
                            level,
                            ox,
                            oy,
                            base_layer,
                            width,
                            height,
                            num_layers,
                            self.gl_format,
                            self.gl_type,
                            offset,
                        );
                    }
                }
                ImageType::E3D => {
                    if is_compressed {
                        gl::CompressedTextureSubImage3D(
                            self.texture,
                            level,
                            ox,
                            oy,
                            oz,
                            width,
                            height,
                            depth,
                            self.gl_internal_format,
                            buf_size,
                            offset,
                        );
                    } else {
                        gl::TextureSubImage3D(
                            self.texture,
                            level,
                            ox,
                            oy,
                            oz,
                            width,
                            height,
                            depth,
                            self.gl_format,
                            self.gl_type,
                            offset,
                        );
                    }
                }
                ImageType::Buffer => {
                    // Upstream `ASSERT(false)` — buffer images don't go
                    // through this path; they bind via the buffer cache.
                    log::warn!(
                        "Image::copy_buffer_to_image: called on Buffer-type image — should never happen"
                    );
                }
            }
        }
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
        if std::env::var_os("RUZU_TRACE_IMAGE_LIFECYCLE").is_some() {
            log::warn!(
                "[IMAGE_DROP] texture={} upscaled_backup={} current_texture={}",
                self.texture,
                self.upscaled_backup,
                self.current_texture,
            );
        }
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

/// Port of upstream `ImageView::StorageViews` (gl_texture_cache.h ~~line 130).
/// Per-texture-type cache for storage views, split by signed/unsigned
/// channel interpretation (signed views need different GL internal
/// formats than unsigned, even at the same bit width).
#[derive(Default)]
struct StorageViews {
    signeds: [u32; NUM_TEXTURE_TYPES],
    unsigneds: [u32; NUM_TEXTURE_TYPES],
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
    /// Snapshot of `ImageViewBase::supports_anisotropy()` captured at
    /// materialisation. Used by the rasterizer's bind loop to pick the
    /// fallback-anisotropy sampler when the format can't carry the
    /// descriptor's configured anisotropy (upstream
    /// gl_graphics_pipeline.cpp:490-493).
    supports_anisotropy: bool,
    /// Lazily-allocated storage-view cache. Upstream uses
    /// `std::unique_ptr<StorageViews>` — same lazy-alloc pattern.
    storage_views: Option<StorageViews>,
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
            supports_anisotropy: false,
            storage_views: None,
        }
    }

    /// Returns the snapshot of `ImageViewBase::supports_anisotropy()`
    /// captured at materialisation time. Used by the bind-time
    /// anisotropy-fallback gate (Slice 15).
    pub fn supports_anisotropy(&self) -> bool {
        self.supports_anisotropy
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
        view.supports_anisotropy = base.supports_anisotropy();

        if base.flags.contains(ImageViewFlagBits::SLICE) {
            view.full_range = SubresourceRange {
                base: crate::texture_cache::types::SubresourceBase {
                    level: base.range.base.level,
                    layer: 0,
                },
                extent: crate::texture_cache::types::SubresourceExtent {
                    levels: 1,
                    layers: 1,
                },
            };
            view.setup_view(TextureType::Color3D);
        } else {
            if base.view_type == ImageViewType::E2DArray {
                view.flat_range.extent.layers = 1;
            }
            view.setup_view(TextureType::Color2D);
            view.setup_view(TextureType::ColorArray2D);
        }
        view.default_handle = match base.view_type {
            ImageViewType::E2DArray => view.handle_for_texture_type(TextureType::ColorArray2D),
            _ => view.handle_for_texture_type(TextureType::Color2D),
        };
        view
    }

    pub fn handle_for_texture_type(&self, handle_type: TextureType) -> u32 {
        self.handle(handle_type as usize)
    }

    /// Port of upstream `ImageView::ImageView(TextureCacheRuntime& runtime,
    /// const ImageViewInfo& info, ImageId image_id_, Image& image,
    /// const SlotVector<Image>&)` (gl_texture_cache.cpp:1101-1196).
    ///
    /// Selects the per-`ImageViewType` `SetupView` calls and `default_handle`
    /// mapping. Buffer views are constructed via a separate path upstream
    /// (`ImageView::ImageView(..., const ImageInfo&, const ImageViewInfo&,
    /// GPUVAddr)`) and are not handled here — those are produced from the
    /// buffer cache, not the descriptor-fill path.
    ///
    /// The upstream `Converted` / ASTC / SRGB re-targeting branches gate on
    /// `image.flags & ImageFlagBits::Converted` and a device-cap query
    /// (`IsAstcRecompressionEnabled()`). The cache-side conversion path
    /// isn't ported yet, so the simple `MaxwellToGL::GetFormatTuple` branch
    /// is the only one wired up. That matches the path taken when the GL
    /// driver natively supports the format — which covers all of MK8D.
    pub fn from_image_view_info(base: &ImageViewBase, image: &Image) -> Self {
        use crate::texture_cache::types::ImageViewType;

        let mut view = Self::new();
        view.internal_format = present_internal_format(base.format);
        view.original_texture = image.handle();
        view.num_samples = image.num_samples;
        view.full_range = base.range;
        view.flat_range = base.range;
        view.supports_anisotropy = base.supports_anisotropy();
        // Upstream sets `is_render_target = info.IsRenderTarget()`; that
        // helper checks `info.range.base.layer != 0 || info.range.extent.layers != 1`.
        // Ruzu's `ImageViewInfo` doesn't carry the same helper yet — the
        // descriptor-fill path always produces sampled views, so default
        // to `false`. Render-target views go through `new_color_2d`.
        view.is_render_target = false;

        // First switch: per-type SetupView calls.
        match base.view_type {
            ImageViewType::E1DArray => {
                view.flat_range.extent.layers = 1;
                view.setup_view(TextureType::Color1D);
                view.setup_view(TextureType::ColorArray1D);
            }
            ImageViewType::E1D => {
                view.setup_view(TextureType::Color1D);
                view.setup_view(TextureType::ColorArray1D);
            }
            ImageViewType::E2DArray => {
                view.flat_range.extent.layers = 1;
                view.setup_view(TextureType::Color2D);
                view.setup_view(TextureType::ColorArray2D);
            }
            ImageViewType::E2D | ImageViewType::Rect => {
                // Upstream also handles the `ImageViewFlagBits::Slice`
                // branch (2D view of a 3D texture, used exclusively for
                // render targets). Sampled-texture views never carry the
                // SLICE flag, so we don't need it here.
                view.setup_view(TextureType::Color2D);
                view.setup_view(TextureType::ColorArray2D);
            }
            ImageViewType::E3D => {
                view.setup_view(TextureType::Color3D);
            }
            ImageViewType::CubeArray => {
                view.flat_range.extent.layers = 6;
                view.setup_view(TextureType::ColorCube);
                view.setup_view(TextureType::ColorArrayCube);
            }
            ImageViewType::Cube => {
                view.setup_view(TextureType::ColorCube);
                view.setup_view(TextureType::ColorArrayCube);
            }
            ImageViewType::Buffer => {
                // Upstream ASSERT(false): buffer views go through a
                // different ctor. Leave handles zero — caller treats this
                // as "view not yet materialised".
            }
        }

        // Second switch: default_handle selection.
        view.default_handle = match base.view_type {
            ImageViewType::E1D => view.handle_for_texture_type(TextureType::Color1D),
            ImageViewType::E1DArray => view.handle_for_texture_type(TextureType::ColorArray1D),
            ImageViewType::E2D | ImageViewType::Rect => {
                view.handle_for_texture_type(TextureType::Color2D)
            }
            ImageViewType::E2DArray => view.handle_for_texture_type(TextureType::ColorArray2D),
            ImageViewType::E3D => view.handle_for_texture_type(TextureType::Color3D),
            ImageViewType::Cube => view.handle_for_texture_type(TextureType::ColorCube),
            ImageViewType::CubeArray => view.handle_for_texture_type(TextureType::ColorArrayCube),
            ImageViewType::Buffer => 0,
        };
        view
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
        let target = image_view_target(view_type, self.num_samples);
        if target == gl::NONE {
            return 0;
        }
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
                let view_err = gl::GetError();
                gl::TextureParameteri(view, gl::TEXTURE_MIN_FILTER, gl::NEAREST as i32);
                gl::TextureParameteri(view, gl::TEXTURE_MAG_FILTER, gl::NEAREST as i32);
                gl::TextureParameteri(view, gl::TEXTURE_WRAP_S, gl::CLAMP_TO_EDGE as i32);
                gl::TextureParameteri(view, gl::TEXTURE_WRAP_T, gl::CLAMP_TO_EDGE as i32);
                if std::env::var_os("RUZU_PROBE_VIEW_STORAGE").is_some() {
                    while gl::GetError() != gl::NO_ERROR {}
                    let red: [u8; 4] = [0xAB, 0xCD, 0xEF, 0x42];
                    gl::ClearTexImage(
                        view,
                        0,
                        gl::RGBA,
                        gl::UNSIGNED_BYTE,
                        red.as_ptr() as *const _,
                    );
                    let clear_err = gl::GetError();
                    let mut via_view = [0u8; 16];
                    gl::GetTextureSubImage(
                        view, 0, 0, 0, 0, 2, 2, 1,
                        gl::RGBA, gl::UNSIGNED_BYTE,
                        via_view.len() as i32,
                        via_view.as_mut_ptr() as *mut _,
                    );
                    let view_read_err = gl::GetError();
                    let mut via_parent = [0u8; 16];
                    gl::GetTextureSubImage(
                        self.original_texture, 0, 0, 0, 0, 2, 2, 1,
                        gl::RGBA, gl::UNSIGNED_BYTE,
                        via_parent.len() as i32,
                        via_parent.as_mut_ptr() as *mut _,
                    );
                    let parent_read_err = gl::GetError();
                    log::warn!(
                        "[VIEW_PROBE] view={} parent={} target=0x{:X} view_format=0x{:X} range=lvl{}+{}/lyr{}+{} view_err=0x{:X} clear_err=0x{:X} via_view={:02X?} via_parent={:02X?} v_read_err=0x{:X} p_read_err=0x{:X}",
                        view, self.original_texture, target, view_format,
                        view_range.base.level, view_range.extent.levels,
                        view_range.base.layer, view_range.extent.layers,
                        view_err, clear_err, via_view, via_parent,
                        view_read_err, parent_read_err,
                    );
                }
            }
        }
        view
    }

    /// Port of `ImageView::StorageView`
    /// (gl_texture_cache.cpp:1212-1227). Returns a GL texture handle
    /// usable as an image-load/store target with the requested
    /// `image_format`. A `Typeless` request just returns the sampling
    /// handle for the texture type. Otherwise the result is a separate
    /// `glTextureView` cached by (signed/unsigned, texture_type) so
    /// signed/unsigned formats with the same width get distinct names.
    ///
    /// Ruzu's port keeps the cache structure shape but inlines the
    /// signed/unsigned split into two `[u32; NUM_TEXTURE_TYPES]` arrays
    /// instead of upstream's `std::unique_ptr<StorageViews>` indirection
    /// — the storage cost is 9*4*2 = 72 bytes per view, negligible.
    pub fn storage_view(
        &mut self,
        texture_type: TextureType,
        image_format: shader_recompiler::shader_info::ImageFormat,
    ) -> u32 {
        use shader_recompiler::shader_info::ImageFormat;
        if image_format == ImageFormat::Typeless {
            return self.handle(texture_type as usize);
        }
        let is_signed = matches!(image_format, ImageFormat::R8Sint | ImageFormat::R16Sint);
        let idx = texture_type as usize;
        if idx >= NUM_TEXTURE_TYPES {
            return 0;
        }
        // Probe the cache first (immutable read), then create + insert
        // if missing. The make_view borrow + storage_views borrow can't
        // overlap, so split into two stages.
        let cached = self.storage_views.as_ref().map(|sv| {
            if is_signed {
                sv.signeds[idx]
            } else {
                sv.unsigneds[idx]
            }
        });
        if let Some(h) = cached {
            if h != 0 {
                return h;
            }
        }
        let new_view = self.make_view(texture_type, shader_format(image_format));
        let storage = self.storage_views.get_or_insert_with(StorageViews::default);
        if is_signed {
            storage.signeds[idx] = new_view;
        } else {
            storage.unsigneds[idx] = new_view;
        }
        new_view
    }
}

/// Port of upstream `OpenGL::ShaderFormat(Shader::ImageFormat)`
/// (gl_texture_cache.cpp:419-440). Maps a single-channel /
/// integer-channel storage-image format to its GL internal format.
/// `Typeless` is not legal here — caller short-circuits before this.
pub fn shader_format(format: shader_recompiler::shader_info::ImageFormat) -> u32 {
    use shader_recompiler::shader_info::ImageFormat;
    match format {
        ImageFormat::Typeless => {
            log::warn!("gl_texture_cache::shader_format: called with Typeless");
            gl::R32UI
        }
        ImageFormat::R8Sint => gl::R8I,
        ImageFormat::R8Uint => gl::R8UI,
        ImageFormat::R16Uint => gl::R16UI,
        ImageFormat::R16Sint => gl::R16I,
        ImageFormat::R32Uint => gl::R32UI,
        ImageFormat::R32G32Uint => gl::RG32UI,
        ImageFormat::R32G32B32A32Uint => gl::RGBA32UI,
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
            if let Some(sv) = self.storage_views.as_ref() {
                for &view in sv.signeds.iter().chain(sv.unsigneds.iter()) {
                    if view != 0 {
                        gl::DeleteTextures(1, &view);
                    }
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

    /// Port of `OpenGL::Sampler::Sampler(TextureCacheRuntime&, const TSCEntry&)`
    /// (gl_texture_cache.cpp:1271-1324).
    ///
    /// Builds a `glCreateSamplers`-allocated sampler object configured per
    /// the TSC descriptor: wrap modes, compare op, min/mag filters, LOD
    /// range/bias, border colour, and (when extensions are present)
    /// anisotropy + reduction-filter mode + seamless-cubemap toggle.
    ///
    /// The dual-sampler "fallback anisotropy" path that upstream uses
    /// when `MaxAnisotropy() > (1 << config.max_anisotropy)` is wired
    /// up identically — `sampler_default_anisotropy` is allocated only
    /// when needed. Extension probe gates (`GLAD_GL_ARB_texture_filter
    /// _anisotropic` / `_minmax`, `GLAD_GL_ARB_seamless_cubemap_per_texture`)
    /// aren't yet exposed by ruzu's `Device`; the parameters are set
    /// unconditionally — `glSamplerParameter*` on an unsupported pname
    /// generates a silent `GL_INVALID_ENUM` which matches the upstream
    /// warning-on-missing-extension behaviour.
    pub fn from_tsc_entry(config: &crate::textures::texture::TscEntry) -> Self {
        let compare_mode = if config.depth_compare_enabled() != 0 {
            gl::COMPARE_REF_TO_TEXTURE as i32
        } else {
            gl::NONE as i32
        };
        let compare_func =
            super::maxwell_to_gl::depth_compare_func(config.depth_compare_func()) as i32;
        // Upstream calls `TextureFilterMode(config.mag_filter, TextureMipmapFilter::None)`
        // for mag; ruzu's helper takes a u32 mipmap_filter so we pass 0
        // (TextureMipmapFilter::None == 0).
        let mag = super::maxwell_to_gl::texture_filter_mode(config.mag_filter(), 1) as i32;
        let min_mipmap_filter = if std::env::var_os("RUZU_FORCE_NO_MIP_SAMPLERS").is_some() {
            1
        } else {
            config.mipmap_filter()
        };
        let min = super::maxwell_to_gl::texture_filter_mode(config.min_filter(), min_mipmap_filter)
            as i32;
        let reduction = super::maxwell_to_gl::reduction_filter(config.reduction_filter()) as i32;
        let seamless = if config.cubemap_interface_filtering() != 0 {
            gl::TRUE as i32
        } else {
            gl::FALSE as i32
        };

        let max_anisotropy = config.computed_max_anisotropy().clamp(1.0, 16.0);
        let border = config.border_color();

        let create_one = |anisotropy: f32| -> u32 {
            let mut handle = 0u32;
            unsafe {
                gl::CreateSamplers(1, &mut handle);
                if handle == 0 {
                    return 0;
                }
                gl::SamplerParameteri(
                    handle,
                    gl::TEXTURE_WRAP_S,
                    super::maxwell_to_gl::wrap_mode(config.wrap_u()) as i32,
                );
                gl::SamplerParameteri(
                    handle,
                    gl::TEXTURE_WRAP_T,
                    super::maxwell_to_gl::wrap_mode(config.wrap_v()) as i32,
                );
                gl::SamplerParameteri(
                    handle,
                    gl::TEXTURE_WRAP_R,
                    super::maxwell_to_gl::wrap_mode(config.wrap_p()) as i32,
                );
                gl::SamplerParameteri(handle, gl::TEXTURE_COMPARE_MODE, compare_mode);
                gl::SamplerParameteri(handle, gl::TEXTURE_COMPARE_FUNC, compare_func);
                gl::SamplerParameteri(handle, gl::TEXTURE_MAG_FILTER, mag);
                gl::SamplerParameteri(handle, gl::TEXTURE_MIN_FILTER, min);
                gl::SamplerParameterf(handle, gl::TEXTURE_LOD_BIAS, config.lod_bias());
                gl::SamplerParameterf(handle, gl::TEXTURE_MIN_LOD, config.min_lod());
                gl::SamplerParameterf(handle, gl::TEXTURE_MAX_LOD, config.max_lod());
                gl::SamplerParameterfv(handle, gl::TEXTURE_BORDER_COLOR, border.as_ptr());
                // The `gl` crate doesn't expose either pname symbolically.
                // GL_TEXTURE_MAX_ANISOTROPY = 0x84FE (ARB-promoted from EXT).
                // GL_TEXTURE_REDUCTION_MODE_ARB = 0x9366.
                const GL_TEXTURE_MAX_ANISOTROPY: u32 = 0x84FE;
                const GL_TEXTURE_REDUCTION_MODE_ARB: u32 = 0x9366;
                gl::SamplerParameterf(handle, GL_TEXTURE_MAX_ANISOTROPY, anisotropy);
                gl::SamplerParameteri(handle, GL_TEXTURE_REDUCTION_MODE_ARB, reduction);
                gl::SamplerParameteri(handle, gl::TEXTURE_CUBE_MAP_SEAMLESS, seamless);
            }
            handle
        };

        let sampler = create_one(max_anisotropy);
        // Upstream's dual-sampler trick: if the requested anisotropy
        // exceeds the descriptor's `max_anisotropy` bit field (rare —
        // happens when Settings forces higher than the game asked for),
        // build a second sampler with the descriptor's value so render
        // passes that fall outside the override can use the original.
        let max_anisotropy_default = (1u32 << config.max_anisotropy_raw()) as f32;
        let sampler_default_anisotropy = if max_anisotropy > max_anisotropy_default {
            create_one(max_anisotropy_default)
        } else {
            0
        };

        Self {
            sampler,
            sampler_default_anisotropy,
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
    samplers: HashMap<crate::texture_cache::types::SamplerId, Sampler>,
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
    /// Port of `OpenGL::TextureCache::TextureCache(Runtime&, MaxwellDeviceMemoryManager&)`.
    /// `device_memory` is the shared `Arc` from `Host1x::memory_manager()`.
    pub fn new(
        device_memory: std::sync::Arc<
            crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager,
        >,
    ) -> Self {
        Self {
            base: CommonTextureCache::new(device_memory),
            images: HashMap::new(),
            image_views: HashMap::new(),
            samplers: HashMap::new(),
            framebuffers: HashMap::new(),
        }
    }

    pub fn set_guest_memory_writer(&mut self, writer: GuestMemoryWriter) {
        self.base.set_guest_memory_writer(writer);
    }

    /// OpenGL-backed port of `TextureCache<P>::PrepareImage`.
    ///
    /// The common Rust base owns the upstream method name, but it cannot reach
    /// backend `Image::UploadMemory`. This wrapper keeps the upstream ordering
    /// for the OpenGL specialization: refresh CPU-modified contents unless the
    /// caller invalidates, then mark GPU modification when requested.
    pub fn prepare_image_with_gpu_reader(
        &mut self,
        image_id: ImageId,
        is_modification: bool,
        invalidate: bool,
        read_gpu: &mut dyn FnMut(u64, &mut [u8]) -> bool,
    ) {
        if !image_id.is_valid() {
            return;
        }
        if std::env::var_os("RUZU_TRACE_TEXTURE_UPLOAD").is_some() {
            let image = &self.base.slot_images[image_id];
            log::warn!(
                "[TEXTURE_UPLOAD] prepare id={} gpu=0x{:X} flags={:?} guest_size={} invalidate={} modification={}",
                image_id.index,
                image.gpu_addr,
                image.flags,
                image.guest_size_bytes,
                invalidate,
                is_modification,
            );
        }
        if invalidate {
            let image = &mut self.base.slot_images[image_id];
            image
                .flags
                .remove(ImageFlagBits::CPU_MODIFIED | ImageFlagBits::GPU_MODIFIED);
        } else {
            self.refresh_contents_with_gpu_reader(image_id, read_gpu);
            // TODO: SynchronizeAliases(image_id) once alias tracking reaches
            // upstream parity.
        }
        if is_modification {
            self.base.mark_modification_by_id(image_id);
        }
    }

    /// OpenGL-backed port of `TextureCache<P>::RefreshContents`.
    ///
    /// If an image is CPU-modified, read its guest swizzled bytes through the
    /// channel GPU-VA path, unswizzle into a staging buffer, and upload through
    /// `Image::UploadMemory`.
    pub fn refresh_contents_with_gpu_reader(
        &mut self,
        image_id: ImageId,
        read_gpu: &mut dyn FnMut(u64, &mut [u8]) -> bool,
    ) {
        if !image_id.is_valid() {
            return;
        }
        let base_image = self.base.slot_images[image_id].clone();
        if !base_image.flags.contains(ImageFlagBits::CPU_MODIFIED) {
            return;
        }
        if base_image.guest_size_bytes == 0 {
            self.base.slot_images[image_id]
                .flags
                .remove(ImageFlagBits::CPU_MODIFIED);
            return;
        }

        let mut guest = vec![0u8; base_image.guest_size_bytes as usize];
        if !read_gpu(base_image.gpu_addr, &mut guest) {
            if std::env::var_os("RUZU_TRACE_TEXTURE_UPLOAD").is_some() {
                log::warn!(
                    "[TEXTURE_UPLOAD] read_miss id={} gpu=0x{:X} size={}",
                    image_id.index,
                    base_image.gpu_addr,
                    guest.len(),
                );
            }
            return;
        }

        let staging_size = map_size_bytes(&base_image) as usize;
        if staging_size == 0 {
            return;
        }
        let mut staging = vec![0u8; staging_size];
        let copies = unswizzle_image(
            &(),
            base_image.gpu_addr,
            &base_image.info,
            &guest,
            &mut staging,
        );
        if copies.is_empty() {
            return;
        }
        if std::env::var_os("RUZU_TRACE_TEXTURE_UPLOAD").is_some() {
            let guest_nonzero = guest.iter().filter(|&&b| b != 0).take(1).count() != 0;
            let staging_nonzero = staging.iter().filter(|&&b| b != 0).take(1).count() != 0;
            let guest_checksum = guest.iter().take(4096).fold(0u64, |acc, &b| {
                acc.wrapping_mul(16777619).wrapping_add(b as u64)
            });
            let staging_checksum = staging.iter().take(4096).fold(0u64, |acc, &b| {
                acc.wrapping_mul(16777619).wrapping_add(b as u64)
            });
            log::warn!(
                "[TEXTURE_UPLOAD] staging id={} guest_nonzero={} staging_nonzero={} guest_crc=0x{:X} staging_crc=0x{:X}",
                image_id.index,
                guest_nonzero,
                staging_nonzero,
                guest_checksum,
                staging_checksum,
            );
        }

        let backend_image = self
            .images
            .entry(image_id)
            .or_insert_with(|| Image::from_base(&base_image));
        if backend_image.handle() == 0 {
            return;
        }

        unsafe {
            let mut pbo = 0u32;
            gl::CreateBuffers(1, &mut pbo);
            if pbo == 0 {
                return;
            }
            gl::NamedBufferData(
                pbo,
                staging.len() as isize,
                staging.as_ptr() as *const _,
                gl::STREAM_DRAW,
            );
            backend_image.upload_memory(pbo, 0, &copies);
            gl::DeleteBuffers(1, &pbo);
            gl::MemoryBarrier(gl::TEXTURE_FETCH_BARRIER_BIT | gl::SHADER_IMAGE_ACCESS_BARRIER_BIT);
        }
        self.base.slot_images[image_id]
            .flags
            .remove(ImageFlagBits::CPU_MODIFIED);

        if std::env::var_os("RUZU_TRACE_TEXTURE_UPLOAD").is_some() {
            log::warn!(
                "[TEXTURE_UPLOAD] uploaded id={} gpu=0x{:X} guest={} staging={} copies={}",
                image_id.index,
                base_image.gpu_addr,
                guest.len(),
                staging.len(),
                copies.len(),
            );
        }
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
            // `update_render_targets_from_snapshot` typically pre-populates
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

    /// Materialise GL-side `Image` and `ImageView` objects for every filled
    /// view returned by `TextureCacheBase::fill_graphics_image_views`.
    ///
    /// This is the bridge from the backend-independent slot pools
    /// (`base.slot_images` / `base.slot_image_views`, populated by
    /// `create_image_view`) to the backend-specific HashMaps
    /// (`self.images` / `self.image_views`) that hold the GL texture
    /// handles needed at draw time. Without this step, the cache fills
    /// `view.id` from the descriptor cbuf but no GL handle exists to bind.
    ///
    /// Upstream lumps this with the `ImageView` constructor inside
    /// `SlotVector<ImageView>::insert(runtime, info, image_id, image,
    /// slot_images)` because the upstream cache stores `P::Image` directly
    /// in the slot. Ruzu separates the two: `create_image_view` only
    /// touches `ImageBase`/`ImageViewBase`; the backend wrapper does the
    /// lazy materialisation here on demand.
    ///
    /// Invalid view IDs (`SlotId::invalid()` and `NULL_IMAGE_VIEW_ID`) are
    /// skipped — upstream's blacklist path also yields these and the
    /// caller is expected to never bind them.
    pub fn materialize_views(
        &mut self,
        views: &[crate::texture_cache::texture_cache_base::ImageViewInOut],
    ) {
        use crate::texture_cache::types::{NULL_IMAGE_ID, NULL_IMAGE_VIEW_ID};
        for view in views {
            let view_id = view.id;
            if !view_id.is_valid() || view_id == NULL_IMAGE_VIEW_ID {
                continue;
            }
            // Resolve parent image_id from the base view slot. Skip null
            // sentinels — those happen for buffer views that haven't been
            // wired through this path yet.
            let view_base = self.base.slot_image_views.get(view_id).clone();
            let image_id = view_base.image_id;
            if !image_id.is_valid() || image_id == NULL_IMAGE_ID {
                continue;
            }

            // Lazy-insert backend Image, then backend ImageView using a
            // borrow to the just-materialised image. The two HashMaps are
            // disjoint so we don't need a split-borrow dance.
            let backend_image_handle: u32 = {
                let base_image = self.base.slot_images[image_id].clone();
                let img = self
                    .images
                    .entry(image_id)
                    .or_insert_with(|| Image::from_base(&base_image));
                img.handle()
            };
            if backend_image_handle == 0 {
                // Image::from_base couldn't allocate (TEXTURE_BUFFER or
                // glCreateTextures failure). Skip so a subsequent draw can
                // retry without leaving a stale ImageView handle behind.
                continue;
            }
            if self.image_views.contains_key(&view_id) {
                continue;
            }
            let image_ref = self
                .images
                .get(&image_id)
                .expect("image inserted above must be present");
            let backend_view = ImageView::from_image_view_info(&view_base, image_ref);
            self.image_views.insert(view_id, backend_view);
        }
    }

    /// Materialise GL-side `Sampler` objects for the given sampler ids.
    ///
    /// Mirrors `materialize_views` but for the sampler slot pool. Lazy
    /// because the TSC descriptors at the same id may be re-read across
    /// frames; `base.find_sampler` dedupes via the TscEntry hashmap so
    /// the same id stays stable while the descriptor is unchanged.
    ///
    /// `NULL_SAMPLER_ID` and invalid ids are skipped — `glBindSamplers`
    /// handles a zero entry as "use texture's default sampler state".
    pub fn materialize_samplers(&mut self, sampler_ids: &[crate::texture_cache::types::SamplerId]) {
        use crate::texture_cache::types::NULL_SAMPLER_ID;
        for &id in sampler_ids {
            if !id.is_valid() || id == NULL_SAMPLER_ID {
                continue;
            }
            if self.samplers.contains_key(&id) {
                continue;
            }
            let tsc = *self.base.slot_samplers.get(id);
            self.samplers.insert(id, Sampler::from_tsc_entry(&tsc));
        }
    }

    /// Look up a materialised GL `Sampler` by its base slot id.
    ///
    /// Returns `None` for `NULL_SAMPLER_ID`, invalid ids, or ids that
    /// haven't been materialised yet.
    pub fn get_sampler(&self, id: crate::texture_cache::types::SamplerId) -> Option<&Sampler> {
        if !id.is_valid() {
            return None;
        }
        self.samplers.get(&id)
    }

    /// Look up a materialised GL `ImageView` by its base slot id.
    ///
    /// Returns `None` when `materialize_views` hasn't been called for
    /// this id yet, or when the id is the null sentinel. Used by the
    /// rasterizer's `glBindTextures` loop (Slice 12) to resolve view ids
    /// produced by `fill_graphics_image_views` into real GL texture
    /// handles.
    pub fn get_image_view(&self, view_id: ImageViewId) -> Option<&ImageView> {
        if !view_id.is_valid() {
            return None;
        }
        self.image_views.get(&view_id)
    }

    /// Mutable variant of `get_image_view` — used by the storage-image
    /// binding path (Slice 14) since `ImageView::storage_view` caches
    /// per-format views via `glTextureView` and needs `&mut self`.
    pub fn get_image_view_mut(&mut self, view_id: ImageViewId) -> Option<&mut ImageView> {
        if !view_id.is_valid() {
            return None;
        }
        self.image_views.get_mut(&view_id)
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

    pub fn update_render_targets_from_snapshot(
        &mut self,
        render_targets: &Maxwell3DRenderTargets,
        gpu_to_cpu: impl FnMut(crate::texture_cache::image_base::GPUVAddr) -> Option<u64>,
    ) {
        self.base
            .update_render_targets_from_snapshot(render_targets, gpu_to_cpu);
        for (image_id, image) in self.base.slot_images.iter() {
            if image_id == crate::texture_cache::types::NULL_IMAGE_ID {
                continue;
            }
            self.images
                .entry(image_id)
                .or_insert_with(|| Image::from_base(image));
        }
    }

    pub fn prepare_render_targets_from_snapshot(
        &mut self,
        render_targets: &Maxwell3DRenderTargets,
        mut read_gpu: Option<&mut dyn FnMut(u64, &mut [u8]) -> bool>,
    ) {
        let mut image_ids = Vec::new();
        for &target in render_targets
            .rt_control
            .map
            .iter()
            .take(render_targets.rt_control.count.min(8) as usize)
        {
            let Some(rt) = render_targets.render_targets.get(target as usize) else {
                continue;
            };
            if rt.address == 0 || rt.width == 0 || rt.height == 0 {
                continue;
            }
            if let Some((image_id, _)) = self.base.slot_images.iter().find(|(_, image)| {
                image.gpu_addr == rt.address
                    && image.info.size.width == rt.width
                    && image.info.size.height == rt.height
                    && !image.image_view_ids.is_empty()
            }) {
                image_ids.push(image_id);
            }
        }

        image_ids.sort_by_key(|id| id.index);
        image_ids.dedup_by_key(|id| id.index);
        for image_id in image_ids {
            if let Some(reader) = read_gpu.as_deref_mut() {
                self.prepare_image_with_gpu_reader(image_id, true, false, reader);
            } else {
                self.base.mark_modification_by_id(image_id);
            }
        }
    }

    /// OpenGL-backed port of `TextureCache<P>::BlitImage` for the currently
    /// implemented framebuffer-blit path (`TextureCacheParams::FRAMEBUFFER_BLITS`).
    pub fn blit_image(
        &mut self,
        dst: &crate::engines::fermi_2d::Surface,
        src: &crate::engines::fermi_2d::Surface,
        copy: &crate::engines::fermi_2d::Config,
        mut gpu_to_cpu: impl FnMut(u64) -> Option<u64>,
    ) -> bool {
        let dst_addr = dst.address();
        let src_addr = src.address();
        let mut dst_info = ImageInfo::from_fermi2d_surface(dst);
        let mut src_info = ImageInfo::from_fermi2d_surface(src);
        let can_be_depth_blit = dst_info.format == src_info.format
            && copy.filter == crate::engines::fermi_2d::Filter::Point;

        let src_id = self.base.find_image(src_addr);
        let dst_id = self.base.find_image(dst_addr);
        if !copy.must_accelerate {
            let src_gpu_modified = src_id
                .map(|id| {
                    self.base.slot_images[id]
                        .flags
                        .contains(ImageFlagBits::GPU_MODIFIED)
                })
                .unwrap_or(false);
            let dst_gpu_modified = dst_id
                .map(|id| {
                    self.base.slot_images[id]
                        .flags
                        .contains(ImageFlagBits::GPU_MODIFIED)
                })
                .unwrap_or(false);
            if !src_gpu_modified && !dst_gpu_modified {
                return false;
            }
        }

        if can_be_depth_blit {
            let src_image = src_id.map(|id| &self.base.slot_images[id]);
            let dst_image = dst_id.map(|id| &self.base.slot_images[id]);
            crate::texture_cache::util::deduce_blit_images(
                &mut dst_info,
                &mut src_info,
                dst_image,
                src_image,
            );
            if crate::surface::get_format_type(dst_info.format)
                != crate::surface::get_format_type(src_info.format)
            {
                return false;
            }
        }

        let Some(src_cpu_addr) = gpu_to_cpu(src_addr) else {
            if std::env::var_os("RUZU_TRACE_FERMI2D_BLIT").is_some() {
                log::info!("[FERMI2D_BLIT] miss src_translate gpu=0x{:X}", src_addr);
            }
            return false;
        };
        let Some(dst_cpu_addr) = gpu_to_cpu(dst_addr) else {
            if std::env::var_os("RUZU_TRACE_FERMI2D_BLIT").is_some() {
                log::info!("[FERMI2D_BLIT] miss dst_translate gpu=0x{:X}", dst_addr);
            }
            return false;
        };

        let src_id = self.find_or_insert_mapped_image(&src_info, src_addr, src_cpu_addr);
        let dst_id = self.find_or_insert_mapped_image(&dst_info, dst_addr, dst_cpu_addr);
        let src_view_id = self.ensure_color_view(src_id, src_info.format);
        let dst_view_id = self.ensure_color_view(dst_id, dst_info.format);
        let Some((src_fbo, _, _)) = self.framebuffer_for_image_view(src_view_id) else {
            return false;
        };
        let Some((dst_fbo, _, _)) = self.framebuffer_for_image_view(dst_view_id) else {
            return false;
        };

        let src_region = Region2D {
            start: Offset2D {
                x: copy.src_x0,
                y: copy.src_y0,
            },
            end: Offset2D {
                x: copy.src_x1,
                y: copy.src_y1,
            },
        };
        let dst_region = Region2D {
            start: Offset2D {
                x: copy.dst_x0,
                y: copy.dst_y0,
            },
            end: Offset2D {
                x: copy.dst_x1,
                y: copy.dst_y1,
            },
        };
        self.blit_framebuffer(dst_fbo, src_fbo, dst_region, src_region, copy.filter);
        self.base.mark_modification_by_id(dst_id);

        if std::env::var_os("RUZU_TRACE_FERMI2D_BLIT").is_some() {
            log::info!(
                "[FERMI2D_BLIT] accelerated src=0x{:X}/0x{:X} dst=0x{:X}/0x{:X} {}x{} -> {}x{}",
                src_addr,
                src_cpu_addr,
                dst_addr,
                dst_cpu_addr,
                src_info.size.width,
                src_info.size.height,
                dst_info.size.width,
                dst_info.size.height
            );
        }
        true
    }

    fn find_or_insert_mapped_image(
        &mut self,
        info: &ImageInfo,
        gpu_addr: u64,
        cpu_addr: u64,
    ) -> ImageId {
        if let Some((id, _)) = self.base.slot_images.iter().find(|(_, image)| {
            image.gpu_addr == gpu_addr
                && image.cpu_addr == cpu_addr
                && image.info.size == info.size
                && image.info.format == info.format
        }) {
            return id;
        }
        let image_id =
            self.base
                .slot_images
                .insert(ImageBase::new(info.clone(), gpu_addr, cpu_addr));
        let image_size = self.base.slot_images[image_id].guest_size_bytes as usize;
        let map_id = self
            .base
            .slot_map_views
            .insert(ImageMapView::new(gpu_addr, cpu_addr, image_size, image_id));
        self.base.slot_images[image_id].map_view_id = map_id;
        self.base.register_image(image_id);
        image_id
    }

    fn ensure_color_view(
        &mut self,
        image_id: ImageId,
        view_format: crate::surface::PixelFormat,
    ) -> ImageViewId {
        let view_info = ImageViewInfo::for_render_target(
            ImageViewType::E2D,
            view_format,
            SubresourceRange::default(),
        );
        let existing = self.base.slot_images[image_id].find_view(&view_info);
        if existing.is_valid() {
            return existing;
        }
        let image_info = self.base.slot_images[image_id].info.clone();
        let gpu_addr = self.base.slot_images[image_id].gpu_addr;
        let view = ImageViewBase::new(&view_info, &image_info, image_id, gpu_addr);
        let view_id = self.base.slot_image_views.insert(view);
        self.base.slot_images[image_id].insert_view(view_info, view_id);
        view_id
    }

    fn framebuffer_for_image_view(&mut self, view_id: ImageViewId) -> Option<(u32, u32, u32)> {
        let view_base = self.base.slot_image_views.get(view_id).clone();
        let image_id = view_base.image_id;
        if !image_id.is_valid() {
            return None;
        }
        let backend_image = self
            .images
            .entry(image_id)
            .or_insert_with(|| Image::from_base(&self.base.slot_images[image_id]));
        let backend_view = self
            .image_views
            .entry(view_id)
            .or_insert_with(|| ImageView::new_color_2d(&view_base, backend_image));
        let attachment_texture = if view_base.flags.contains(ImageViewFlagBits::SLICE) {
            backend_view.handle_for_texture_type(TextureType::Color3D)
        } else {
            backend_view.default_handle()
        };
        if attachment_texture == 0 {
            return None;
        }
        let framebuffer = self.framebuffers.entry(view_id).or_insert_with(|| {
            let mut framebuffer = 0;
            unsafe {
                gl::CreateFramebuffers(1, &mut framebuffer);
                if framebuffer != 0 {
                    gl::NamedFramebufferTexture(
                        framebuffer,
                        gl::COLOR_ATTACHMENT0,
                        attachment_texture,
                        0,
                    );
                    gl::NamedFramebufferDrawBuffer(framebuffer, gl::COLOR_ATTACHMENT0);
                    gl::NamedFramebufferReadBuffer(framebuffer, gl::COLOR_ATTACHMENT0);
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

    fn blit_framebuffer(
        &self,
        dst_framebuffer: u32,
        src_framebuffer: u32,
        dst_region: Region2D,
        src_region: Region2D,
        filter: crate::engines::fermi_2d::Filter,
    ) {
        let filter = match filter {
            crate::engines::fermi_2d::Filter::Point => gl::NEAREST,
            crate::engines::fermi_2d::Filter::Bilinear => gl::LINEAR,
        };
        unsafe {
            gl::BlitNamedFramebuffer(
                src_framebuffer,
                dst_framebuffer,
                src_region.start.x,
                src_region.start.y,
                src_region.end.x,
                src_region.end.y,
                dst_region.start.x,
                dst_region.start.y,
                dst_region.end.x,
                dst_region.end.y,
                gl::COLOR_BUFFER_BIT,
                filter,
            );
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
        if std::env::var_os("RUZU_TRACE_PRESENT_ALIASES").is_some() {
            let image = &self.base.slot_images[image_id];
            log::info!(
                "[PRESENT_ALIAS] selected image={} view={} cpu=0x{:X} gpu=0x{:X} fmt={:?} size={}x{} flags={:?} tick={} aliases={} overlaps={}",
                image_id.index,
                view_id.index,
                image.cpu_addr,
                image.gpu_addr,
                image.info.format,
                image.info.size.width,
                image.info.size.height,
                image.flags,
                image.modification_tick,
                image.aliased_images.len(),
                image.overlapping_images.len(),
            );
            for alias in &image.aliased_images {
                let alias_image = &self.base.slot_images[alias.id];
                log::info!(
                    "[PRESENT_ALIAS]   alias image={} cpu=0x{:X} gpu=0x{:X} fmt={:?} size={}x{} flags={:?} tick={} copies={}",
                    alias.id.index,
                    alias_image.cpu_addr,
                    alias_image.gpu_addr,
                    alias_image.info.format,
                    alias_image.info.size.width,
                    alias_image.info.size.height,
                    alias_image.flags,
                    alias_image.modification_tick,
                    alias.copies.len(),
                );
            }
            for &overlap_id in &image.overlapping_images {
                let overlap = &self.base.slot_images[overlap_id];
                log::info!(
                    "[PRESENT_ALIAS]   overlap image={} cpu=0x{:X} gpu=0x{:X} fmt={:?} size={}x{} flags={:?} tick={}",
                    overlap_id.index,
                    overlap.cpu_addr,
                    overlap.gpu_addr,
                    overlap.info.format,
                    overlap.info.size.width,
                    overlap.info.size.height,
                    overlap.flags,
                    overlap.modification_tick,
                );
            }
        }
        let backend_image = self.images.entry(image_id).or_insert_with(|| {
            let image = &self.base.slot_images[image_id];
            Image::from_base(image)
        });
        let original_texture = backend_image.handle();
        let current_texture = backend_image.current_texture;
        let backend_view = self
            .image_views
            .entry(view_id)
            .or_insert_with(|| ImageView::from_image_view_info(&view, backend_image));
        let display_texture = backend_view.handle_for_texture_type(TextureType::Color2D);
        if display_texture == 0 {
            return None;
        }
        if std::env::var_os("RUZU_TRACE_PRESENT_HANDLES").is_some() {
            let mut fbo_handle: u32 = 0;
            let mut fbo_attachment: i32 = 0;
            if let Some(fb) = self.framebuffers.get(&view_id) {
                fbo_handle = fb.framebuffer;
                if fbo_handle != 0 {
                    unsafe {
                        gl::GetNamedFramebufferAttachmentParameteriv(
                            fbo_handle,
                            gl::COLOR_ATTACHMENT0,
                            gl::FRAMEBUFFER_ATTACHMENT_OBJECT_NAME,
                            &mut fbo_attachment,
                        );
                    }
                }
            }
            // Read first 16 RGBA8 bytes from BOTH the present display_texture
            // view AND the underlying image's current_texture handle. If both
            // alias the same backing storage they must read identical bytes.
            let mut display_bytes = [0u8; 16];
            let mut original_bytes = [0u8; 16];
            unsafe {
                gl::PixelStorei(gl::PACK_ALIGNMENT, 1);
                gl::GetTextureSubImage(
                    display_texture,
                    0,
                    0,
                    0,
                    0,
                    2,
                    2,
                    1,
                    gl::RGBA,
                    gl::UNSIGNED_BYTE,
                    display_bytes.len() as i32,
                    display_bytes.as_mut_ptr() as *mut _,
                );
                if current_texture != 0 {
                    gl::GetTextureSubImage(
                        current_texture,
                        0,
                        0,
                        0,
                        0,
                        2,
                        2,
                        1,
                        gl::RGBA,
                        gl::UNSIGNED_BYTE,
                        original_bytes.len() as i32,
                        original_bytes.as_mut_ptr() as *mut _,
                    );
                }
            }
            log::warn!(
                "[PRESENT_HANDLES] image={} view={} cpu=0x{:X} original_texture={} current_texture={} display_texture={} fbo={} fbo_attached_texture={} display_bytes={:02X?} original_bytes={:02X?}",
                image_id.index,
                view_id.index,
                cpu_addr,
                original_texture,
                current_texture,
                display_texture,
                fbo_handle,
                fbo_attachment,
                display_bytes,
                original_bytes,
            );
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

        // EXPERIMENT: when MK8D wants to draw to flinger buffer #2 or #3
        // (0x502570000 / 0x502DE0000), redirect to the FIRST flinger buffer
        // (0x501D00000) to test the "late parent textures reject FS writes"
        // hypothesis from [[mk8d-black-screen-2026-05-21-evening]]. If MK8D
        // ends up rendering visible content when we redirect, the bug is in
        // the late-created parent textures.
        let force_first_flinger = std::env::var_os("RUZU_FBO_FORCE_FIRST_FLINGER").is_some();
        let lookup_addr = if force_first_flinger
            && (rt.address == 0x502570000 || rt.address == 0x502DE0000)
        {
            log::warn!(
                "[FBO_FORCE_FIRST] redirecting RT gpu=0x{:X} → 0x501D00000",
                rt.address
            );
            0x501D00000
        } else {
            rt.address
        };
        let (image_id, image_base) = self.base.slot_images.iter().find(|(_, image)| {
            image.gpu_addr == lookup_addr
                && image.info.size.width == rt.width
                && image.info.size.height == rt.height
                && !image.image_view_ids.is_empty()
        })?;
        let image_id = image_id;
        let image_base = image_base.clone();
        let view_id = self
            .base
            .find_render_target_view_from_image(image_id, rt, lookup_addr);
        if !view_id.is_valid() {
            return None;
        }
        if std::env::var_os("RUZU_TRACE_RT_FBO").is_some() {
            log::warn!(
                "[RT_FBO] image={} view={} cpu=0x{:X} gpu=0x{:X} {}x{} num_views={}",
                image_id.index,
                view_id.index,
                image_base.cpu_addr,
                rt.address,
                rt.width,
                rt.height,
                image_base.image_view_ids.len(),
            );
        }
        let view_base = self.base.slot_image_views[view_id].clone();

        let backend_image = self
            .images
            .entry(image_id)
            .or_insert_with(|| Image::from_base(&image_base));
        let backend_view = self
            .image_views
            .entry(view_id)
            .or_insert_with(|| ImageView::new_color_2d(&view_base, backend_image));
        let attachment_texture = if view_base.flags.contains(ImageViewFlagBits::SLICE) {
            backend_view.handle_for_texture_type(TextureType::Color3D)
        } else {
            backend_view.default_handle()
        };
        if attachment_texture == 0 {
            return None;
        }

        let view_w = view_base.size.width as i32;
        let view_h = view_base.size.height as i32;
        let parent_handle = backend_image.handle();
        let framebuffer = self.framebuffers.entry(view_id).or_insert_with(|| {
            let mut framebuffer = 0;
            unsafe {
                gl::CreateFramebuffers(1, &mut framebuffer);
                if framebuffer != 0 {
                    if std::env::var_os("RUZU_FBO_ATTACH_PARENT").is_some() && parent_handle != 0 {
                        // Attach the parent TEXTURE_2D_ARRAY directly at layer 0
                        gl::NamedFramebufferTextureLayer(framebuffer, gl::COLOR_ATTACHMENT0, parent_handle, 0, 0);
                        log::warn!("[FBO_ATTACH_PARENT] fbo={} parent={} layer=0", framebuffer, parent_handle);
                    } else {
                        if view_base.flags.contains(ImageViewFlagBits::SLICE) {
                            if view_base.range.extent.layers > 1 {
                                gl::NamedFramebufferTexture(
                                    framebuffer,
                                    gl::COLOR_ATTACHMENT0,
                                    attachment_texture,
                                    0,
                                );
                            } else {
                                gl::NamedFramebufferTextureLayer(
                                    framebuffer,
                                    gl::COLOR_ATTACHMENT0,
                                    attachment_texture,
                                    0,
                                    view_base.range.base.layer,
                                );
                            }
                        } else {
                            gl::NamedFramebufferTexture(
                                framebuffer,
                                gl::COLOR_ATTACHMENT0,
                                attachment_texture,
                                0,
                            );
                        }
                    }
                    // Upstream `Framebuffer::Framebuffer` (gl_texture_cache.cpp:1326)
                    // uses `glNamedFramebufferDrawBuffer` (direct state access)
                    // and sets GL_FRAMEBUFFER_DEFAULT_WIDTH/HEIGHT.
                    gl::NamedFramebufferDrawBuffer(framebuffer, gl::COLOR_ATTACHMENT0);
                    gl::NamedFramebufferReadBuffer(framebuffer, gl::COLOR_ATTACHMENT0);
                    gl::NamedFramebufferParameteri(
                        framebuffer,
                        gl::FRAMEBUFFER_DEFAULT_WIDTH,
                        view_w,
                    );
                    gl::NamedFramebufferParameteri(
                        framebuffer,
                        gl::FRAMEBUFFER_DEFAULT_HEIGHT,
                        view_h,
                    );
                    if std::env::var_os("RUZU_FBO_INIT_CLEAR").is_some() {
                        let init_color = [0.0f32, 0.0, 0.0, 1.0];
                        gl::ClearNamedFramebufferfv(framebuffer, gl::COLOR, 0, init_color.as_ptr());
                        gl::Finish();
                        log::warn!("[FBO_INIT_CLEAR] fbo={} cleared to black", framebuffer);
                    }
                }
            }
            TextureCacheFramebuffer {
                framebuffer,
                buffer_bits: gl::COLOR_BUFFER_BIT,
            }
        });
        if std::env::var_os("RUZU_TRACE_RT_FBO").is_some() {
            let fbo = framebuffer.framebuffer;
            let mut attached: i32 = 0;
            let mut atype: i32 = 0;
            let mut alevel: i32 = 0;
            let mut alayer: i32 = 0;
            let mut alayered: i32 = 0;
            let mut acubeface: i32 = 0;
            let mut afmt: i32 = 0;
            let mut acolor_encoding: i32 = 0;
            let mut acomp_type: i32 = 0;
            let mut tex_internal_format: i32 = 0;
            let mut tex_width: i32 = 0;
            let mut tex_height: i32 = 0;
            let mut tex_depth: i32 = 0;
            let mut tex_immutable: i32 = 0;
            let mut tex_view_min_level: i32 = 0;
            let mut tex_view_num_levels: i32 = 0;
            let mut tex_view_min_layer: i32 = 0;
            let mut tex_view_num_layers: i32 = 0;
            const GL_TEXTURE_VIEW_MIN_LEVEL: u32 = 0x82DB;
            const GL_TEXTURE_VIEW_NUM_LEVELS: u32 = 0x82DC;
            const GL_TEXTURE_VIEW_MIN_LAYER: u32 = 0x82DD;
            const GL_TEXTURE_VIEW_NUM_LAYERS: u32 = 0x82DE;
            unsafe {
                if fbo != 0 {
                    gl::GetNamedFramebufferAttachmentParameteriv(
                        fbo,
                        gl::COLOR_ATTACHMENT0,
                        gl::FRAMEBUFFER_ATTACHMENT_OBJECT_NAME,
                        &mut attached,
                    );
                    gl::GetNamedFramebufferAttachmentParameteriv(
                        fbo,
                        gl::COLOR_ATTACHMENT0,
                        gl::FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE,
                        &mut atype,
                    );
                    gl::GetNamedFramebufferAttachmentParameteriv(
                        fbo,
                        gl::COLOR_ATTACHMENT0,
                        gl::FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL,
                        &mut alevel,
                    );
                    gl::GetNamedFramebufferAttachmentParameteriv(
                        fbo,
                        gl::COLOR_ATTACHMENT0,
                        gl::FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER,
                        &mut alayer,
                    );
                    gl::GetNamedFramebufferAttachmentParameteriv(
                        fbo,
                        gl::COLOR_ATTACHMENT0,
                        gl::FRAMEBUFFER_ATTACHMENT_LAYERED,
                        &mut alayered,
                    );
                    gl::GetNamedFramebufferAttachmentParameteriv(
                        fbo,
                        gl::COLOR_ATTACHMENT0,
                        gl::FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE,
                        &mut acubeface,
                    );
                    gl::GetNamedFramebufferAttachmentParameteriv(
                        fbo,
                        gl::COLOR_ATTACHMENT0,
                        gl::FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING,
                        &mut acolor_encoding,
                    );
                    gl::GetNamedFramebufferAttachmentParameteriv(
                        fbo,
                        gl::COLOR_ATTACHMENT0,
                        gl::FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE,
                        &mut acomp_type,
                    );
                    if attached > 0 {
                        gl::GetTextureLevelParameteriv(
                            attached as u32,
                            alevel,
                            gl::TEXTURE_INTERNAL_FORMAT,
                            &mut tex_internal_format,
                        );
                        gl::GetTextureLevelParameteriv(
                            attached as u32,
                            alevel,
                            gl::TEXTURE_WIDTH,
                            &mut tex_width,
                        );
                        gl::GetTextureLevelParameteriv(
                            attached as u32,
                            alevel,
                            gl::TEXTURE_HEIGHT,
                            &mut tex_height,
                        );
                        gl::GetTextureLevelParameteriv(
                            attached as u32,
                            alevel,
                            gl::TEXTURE_DEPTH,
                            &mut tex_depth,
                        );
                        gl::GetTextureParameteriv(
                            attached as u32,
                            gl::TEXTURE_IMMUTABLE_FORMAT,
                            &mut tex_immutable,
                        );
                        gl::GetTextureParameteriv(
                            attached as u32,
                            GL_TEXTURE_VIEW_MIN_LEVEL,
                            &mut tex_view_min_level,
                        );
                        gl::GetTextureParameteriv(
                            attached as u32,
                            GL_TEXTURE_VIEW_NUM_LEVELS,
                            &mut tex_view_num_levels,
                        );
                        gl::GetTextureParameteriv(
                            attached as u32,
                            GL_TEXTURE_VIEW_MIN_LAYER,
                            &mut tex_view_min_layer,
                        );
                        gl::GetTextureParameteriv(
                            attached as u32,
                            GL_TEXTURE_VIEW_NUM_LAYERS,
                            &mut tex_view_num_layers,
                        );
                    }
                }
            }
            log::warn!(
                "[RT_FBO_ATTACH] view={} fbo={} attached={} expected={} backend_original={} atype=0x{:X} alevel={} alayer={} alayered={} cubeface=0x{:X} fmt=0x{:X} comp=0x{:X} enc=0x{:X} tex_size={}x{}x{} immutable={} view_lvl={}+{} view_layer={}+{}",
                view_id.index,
                fbo,
                attached,
                attachment_texture,
                backend_image.handle(),
                atype,
                alevel,
                alayer,
                alayered,
                acubeface,
                tex_internal_format,
                acomp_type,
                acolor_encoding,
                tex_width,
                tex_height,
                tex_depth,
                tex_immutable,
                tex_view_min_level,
                tex_view_num_levels,
                tex_view_min_layer,
                tex_view_num_layers,
            );
        }
        let handle = framebuffer.handle();
        (handle != 0).then_some((handle, view_base.size.width, view_base.size.height))
    }
}

impl Default for TextureCache {
    /// Standalone default for tests / fallback paths. Production
    /// construction goes through `RasterizerOpenGL::new`, which threads
    /// the shared `Arc` from `Host1x::memory_manager()`.
    fn default() -> Self {
        Self::new(std::sync::Arc::new(
            crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager::default(),
        ))
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
