// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_texture_cache.h and gl_texture_cache.cpp
//!
//! OpenGL texture cache -- manages GPU texture and image objects, framebuffers, and samplers.

use std::collections::HashMap;
use std::ptr::NonNull;
use std::sync::atomic::Ordering;
use std::sync::{Arc, OnceLock};

use common::settings;
use common::slot_vector::SlotVector;

use super::gl_shader_manager::{ProgramManager, ProgramManagerHandle};
use super::gl_staging_buffer_pool::{StagingBufferMap, StagingBufferPool};
use super::gl_state_tracker::StateTracker;
use super::util_shaders::UtilShaders;
use crate::delayed_destruction_ring::DelayedDestructionRing;
use crate::engines::draw_manager::Maxwell3DRenderTargets;
use crate::engines::maxwell_3d::{RenderTargetInfo, ScissorInfo};
use crate::framebuffer_config::FramebufferConfig;
use crate::renderer_base::GuestMemoryWriter;
use crate::shader_environment::TextureType;
use crate::surface::{PixelFormat, SurfaceType};
use crate::texture_cache::image_base::{ImageBase, ImageFlagBits, ImageMapView};
use crate::texture_cache::image_info::ImageInfo;
use crate::texture_cache::image_view_base::{ImageViewBase, ImageViewFlagBits};
use crate::texture_cache::image_view_info::{ImageViewInfo, SwizzleSource};
use crate::texture_cache::render_targets::RenderTargets;
use crate::texture_cache::texture_cache_base::{
    AsyncDecodeContext, FramebufferImageView, JoinCopy, TextureCacheBase as CommonTextureCache,
    TICKS_TO_DESTROY,
};
use crate::texture_cache::types::{BufferImageCopy, ImageCopy};
use crate::texture_cache::types::{
    Extent2D, Extent3D, FramebufferId, ImageId, ImageType, ImageViewId, ImageViewType, Offset2D,
    Region2D, RelaxedOptions, SubresourceRange, NULL_IMAGE_ID, NULL_IMAGE_VIEW_ID,
};
use crate::texture_cache::util::{
    full_download_copies, full_upload_swizzles, make_shrink_image_copies, map_size_bytes,
    unswizzle_image,
};
use crate::textures::workers::ThreadWorker;

/// Number of render targets.
pub const NUM_RT: usize = 8;

fn parse_u64_env_list(name: &str) -> Option<Vec<u64>> {
    let spec = std::env::var(name).ok()?;
    let spec = spec.trim();
    if spec.is_empty() {
        return None;
    }
    if spec == "*" {
        return Some(Vec::new());
    }
    let targets = spec
        .split(',')
        .filter_map(|raw| {
            let value = raw.trim();
            if value.is_empty() {
                return None;
            }
            if let Some(hex) = value
                .strip_prefix("0x")
                .or_else(|| value.strip_prefix("0X"))
            {
                u64::from_str_radix(hex, 16).ok()
            } else {
                value.parse::<u64>().ok()
            }
        })
        .collect::<Vec<_>>();
    (!targets.is_empty()).then_some(targets)
}

fn scale_up_image_copies(copies: &[ImageCopy], both_2d: bool) -> Vec<ImageCopy> {
    let resolution = settings::values().resolution_info.clone();
    copies
        .iter()
        .copied()
        .map(|mut copy| {
            copy.src_offset.x = resolution.scale_up_i32(copy.src_offset.x);
            copy.dst_offset.x = resolution.scale_up_i32(copy.dst_offset.x);
            copy.extent.width = resolution.scale_up_u32(copy.extent.width);
            if both_2d {
                copy.src_offset.y = resolution.scale_up_i32(copy.src_offset.y);
                copy.dst_offset.y = resolution.scale_up_i32(copy.dst_offset.y);
                copy.extent.height = resolution.scale_up_u32(copy.extent.height);
            }
            copy
        })
        .collect()
}

fn is_pixel_format_bgr(format: PixelFormat) -> bool {
    matches!(
        format,
        PixelFormat::B5G6R5Unorm | PixelFormat::B8G8R8A8Unorm | PixelFormat::B8G8R8A8Srgb
    )
}

fn texture_upload_trace_targets() -> Option<&'static [u64]> {
    static TARGETS: OnceLock<Option<Vec<u64>>> = OnceLock::new();
    TARGETS
        .get_or_init(|| parse_u64_env_list("RUZU_TRACE_TEXTURE_UPLOAD_ADDRS"))
        .as_deref()
}

fn should_trace_texture_upload(gpu_addr: u64) -> bool {
    if std::env::var_os("RUZU_TRACE_TEXTURE_UPLOAD").is_none() {
        return false;
    }
    let Some(targets) = texture_upload_trace_targets() else {
        return true;
    };
    targets.is_empty() || targets.contains(&gpu_addr)
}

fn should_dump_texture_upload(gpu_addr: u64) -> bool {
    if std::env::var_os("RUZU_DUMP_TEXTURE_UPLOAD_DIR").is_none() {
        return false;
    }
    let Some(targets) = texture_upload_trace_targets() else {
        return false;
    };
    targets.is_empty() || targets.contains(&gpu_addr)
}

fn texture_cache_trace_targets() -> Option<&'static [u64]> {
    static TARGETS: OnceLock<Option<Vec<u64>>> = OnceLock::new();
    TARGETS
        .get_or_init(|| parse_u64_env_list("RUZU_TRACE_TEXTURE_CACHE_ADDRS"))
        .as_deref()
}

fn should_trace_texture_cache_address(gpu_addr: u64) -> bool {
    let Some(targets) = texture_cache_trace_targets() else {
        return false;
    };
    targets.is_empty() || targets.contains(&gpu_addr)
}

fn texture_cache_cpu_trace_targets() -> Option<&'static [u64]> {
    static TARGETS: OnceLock<Option<Vec<u64>>> = OnceLock::new();
    TARGETS
        .get_or_init(|| parse_u64_env_list("RUZU_TRACE_TEXTURE_CACHE_CPU_ADDRS"))
        .as_deref()
}

fn should_trace_texture_cache_cpu_region(cpu_addr: u64, size: usize) -> bool {
    let Some(targets) = texture_cache_cpu_trace_targets() else {
        return false;
    };
    if targets.is_empty() {
        return true;
    }
    let end = cpu_addr.saturating_add(size as u64);
    targets
        .iter()
        .any(|&target| cpu_addr <= target && target < end)
}

fn should_trace_invalid_copy_image() -> bool {
    std::env::var_os("RUZU_TRACE_COPY_IMAGE_INVALID").is_some()
}

fn image_view_trace_targets() -> Option<&'static [u64]> {
    static TARGETS: OnceLock<Option<Vec<u64>>> = OnceLock::new();
    TARGETS
        .get_or_init(|| parse_u64_env_list("RUZU_TRACE_IMAGE_VIEW_ADDRS"))
        .as_deref()
}

fn should_trace_image_view_address(gpu_addr: u64) -> bool {
    let Some(targets) = image_view_trace_targets() else {
        return true;
    };
    targets.is_empty() || targets.contains(&gpu_addr)
}

fn framebuffer_attachment_type(format: PixelFormat) -> u32 {
    match crate::surface::get_format_type(format) {
        SurfaceType::Depth => gl::DEPTH_ATTACHMENT,
        SurfaceType::Stencil => gl::STENCIL_ATTACHMENT,
        SurfaceType::DepthStencil => gl::DEPTH_STENCIL_ATTACHMENT,
        _ => gl::DEPTH_ATTACHMENT,
    }
}

fn rescale_attachment_type(format_type: SurfaceType) -> u32 {
    match format_type {
        SurfaceType::ColorTexture => gl::COLOR_ATTACHMENT0,
        SurfaceType::Depth => gl::DEPTH_ATTACHMENT,
        SurfaceType::Stencil => gl::STENCIL_ATTACHMENT,
        SurfaceType::DepthStencil => gl::DEPTH_STENCIL_ATTACHMENT,
        _ => gl::COLOR_ATTACHMENT0,
    }
}

fn rescale_buffer_mask(format_type: SurfaceType) -> u32 {
    match format_type {
        SurfaceType::ColorTexture => gl::COLOR_BUFFER_BIT,
        SurfaceType::Depth => gl::DEPTH_BUFFER_BIT,
        SurfaceType::Stencil => gl::STENCIL_BUFFER_BIT,
        SurfaceType::DepthStencil => gl::DEPTH_BUFFER_BIT | gl::STENCIL_BUFFER_BIT,
        _ => gl::COLOR_BUFFER_BIT,
    }
}

fn rescale_fbo_index(format_type: SurfaceType) -> usize {
    match format_type {
        SurfaceType::ColorTexture => 0,
        SurfaceType::Depth => 1,
        SurfaceType::Stencil => 2,
        SurfaceType::DepthStencil => 3,
        _ => 0,
    }
}

/// Port of upstream `IsConverted(const Device&, PixelFormat, ImageType)`.
fn is_converted_image(has_native_astc: bool, format: PixelFormat, image_type: ImageType) -> bool {
    if !has_native_astc && crate::surface::is_pixel_format_astc(format) {
        return true;
    }
    matches!(format, PixelFormat::Bc4Unorm | PixelFormat::Bc5Unorm) && image_type == ImageType::E3D
}

/// Port of upstream `CanBeAccelerated(const TextureCacheRuntime&, const ImageInfo&)`.
fn can_be_accelerated(has_native_astc: bool, info: &ImageInfo) -> bool {
    if crate::surface::is_pixel_format_astc(info.format) && info.size.depth == 1 && !has_native_astc
    {
        return *common::settings::values().accelerate_astc.get_value()
            == common::settings_enums::AstcDecodeMode::Gpu
            && *common::settings::values().astc_recompression.get_value()
                == common::settings_enums::AstcRecompression::Uncompressed;
    }
    false
}

/// Port of upstream `CanBeDecodedAsync(const TextureCacheRuntime&, const ImageInfo&)`.
fn can_be_decoded_async(has_native_astc: bool, info: &ImageInfo) -> bool {
    crate::surface::is_pixel_format_astc(info.format)
        && !has_native_astc
        && *common::settings::values().accelerate_astc.get_value()
            == common::settings_enums::AstcDecodeMode::CpuAsynchronous
}

const GL_COMPRESSED_RGBA_S3TC_DXT1_EXT: u32 = 0x83F1;
const GL_COMPRESSED_RGBA_S3TC_DXT5_EXT: u32 = 0x83F3;
const GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT: u32 = 0x8C4D;
const GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT: u32 = 0x8C4F;

/// Port of upstream `IsAstcRecompressionEnabled()`.
fn is_astc_recompression_enabled() -> bool {
    *common::settings::values().astc_recompression.get_value()
        != common::settings_enums::AstcRecompression::Uncompressed
}

/// Port of upstream `SelectAstcFormat(PixelFormat format, bool is_srgb)`.
fn select_astc_format(_format: PixelFormat, is_srgb: bool) -> u32 {
    match *common::settings::values().astc_recompression.get_value() {
        common::settings_enums::AstcRecompression::Bc1 => {
            if is_srgb {
                GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT
            } else {
                GL_COMPRESSED_RGBA_S3TC_DXT1_EXT
            }
        }
        common::settings_enums::AstcRecompression::Bc3 => {
            if is_srgb {
                GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT
            } else {
                GL_COMPRESSED_RGBA_S3TC_DXT5_EXT
            }
        }
        common::settings_enums::AstcRecompression::Uncompressed => {
            if is_srgb {
                gl::SRGB8_ALPHA8
            } else {
                gl::RGBA8
            }
        }
    }
}

/// Number of texture types (1D, 2D, 2DRect, 3D, Cube, 1DArray, 2DArray, Buffer, CubeArray).
const NUM_TEXTURE_TYPES: usize = 9;

/// Format properties for a given GL internal format.
///
/// Corresponds to `OpenGL::FormatProperties`.
#[derive(Clone, Debug, Default)]
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
        dst_target: u32,
        dst_format: u32,
        dst_type: u32,
        src_texture: u32,
        src_target: u32,
        src_format: u32,
        src_type: u32,
        src_pixel_format: crate::surface::PixelFormat,
        copies: &[ImageCopy],
    ) {
        unsafe {
            let img_bpp = crate::surface::bytes_per_block(src_pixel_format);
            for copy in copies {
                let src_origin =
                    make_copy_origin(copy.src_offset, copy.src_subresource, src_target);
                let dst_origin =
                    make_copy_origin(copy.dst_offset, copy.dst_subresource, dst_target);
                let region = make_copy_region(copy.extent, copy.dst_subresource, dst_target);
                if region.width == 0 || region.height == 0 || region.depth == 0 || img_bpp == 0 {
                    continue;
                }
                let copy_size = region.width as usize
                    * region.height as usize
                    * region.depth as usize
                    * img_bpp as usize;
                self.ensure_pbo_size(copy_size);

                gl::PixelStorei(gl::PACK_ALIGNMENT, 1);
                gl::PixelStorei(gl::PACK_ROW_LENGTH, copy.extent.width as i32);
                gl::BindBuffer(gl::PIXEL_PACK_BUFFER, self.intermediate_pbo);
                gl::GetTextureSubImage(
                    src_texture,
                    src_origin.level,
                    src_origin.x,
                    src_origin.y,
                    src_origin.z,
                    region.width,
                    region.height,
                    region.depth,
                    src_format,
                    src_type,
                    self.pbo_size as i32,
                    std::ptr::null_mut(),
                );

                gl::PixelStorei(gl::UNPACK_ALIGNMENT, 1);
                gl::PixelStorei(gl::UNPACK_ROW_LENGTH, copy.extent.width as i32);
                gl::BindBuffer(gl::PIXEL_UNPACK_BUFFER, self.intermediate_pbo);
                gl::TextureSubImage3D(
                    dst_texture,
                    dst_origin.level,
                    dst_origin.x,
                    dst_origin.y,
                    dst_origin.z,
                    region.width,
                    region.height,
                    region.depth,
                    dst_format,
                    dst_type,
                    std::ptr::null(),
                );
            }

            gl::BindBuffer(gl::PIXEL_PACK_BUFFER, 0);
            gl::BindBuffer(gl::PIXEL_UNPACK_BUFFER, 0);
        }
    }

    fn ensure_pbo_size(&mut self, required_size: usize) {
        if self.pbo_size >= required_size {
            return;
        }
        let allocation_size = required_size.max(1).next_power_of_two();
        unsafe {
            if self.intermediate_pbo != 0 {
                gl::DeleteBuffers(1, &self.intermediate_pbo);
            }
            gl::CreateBuffers(1, &mut self.intermediate_pbo);
            gl::NamedBufferData(
                self.intermediate_pbo,
                allocation_size as isize,
                std::ptr::null(),
                gl::STREAM_COPY,
            );
        }
        self.pbo_size = allocation_size;
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
    can_report_memory_usage: bool,
    has_native_astc: bool,
    // Upstream stores `StateTracker& state_tracker`. `RasterizerOpenGL` owns
    // the tracker allocation in Rust, and this non-null pointer has the same
    // lifetime as the texture cache runtime field.
    state_tracker: NonNull<StateTracker>,
    staging_buffer_pool: StagingBufferPool,
    util_shaders: UtilShaders,
    format_conversion_pass: FormatConversionPass,
    format_properties: [HashMap<u32, FormatProperties>; 3],
    null_image_handles: [u32; 7],
    null_image_views: [u32; NUM_TEXTURE_TYPES],
    rescale_draw_fbos: [u32; 4],
    rescale_read_fbos: [u32; 4],
}

impl TextureCacheRuntime {
    /// Create a new texture cache runtime.
    ///
    /// `device_access_memory` mirrors the upstream
    /// `TextureCacheRuntime::TextureCacheRuntime` budget: NVX total +
    /// 512 MiB when the extension is present, else a 2 GiB minimum. Kept
    /// in sync with the buffer-cache runtime (gl_buffer_cache.cpp:139).
    pub fn new(
        device: &super::gl_device::Device,
        program_manager: ProgramManagerHandle,
        state_tracker: &mut StateTracker,
    ) -> Self {
        const HALF_GIB: u64 = 512 * 1024 * 1024;
        let device_access_memory = if device.can_report_memory() {
            device.get_current_dedicated_video_memory() + HALF_GIB
        } else {
            2 * 1024 * 1024 * 1024
        };
        Self::new_with_caps(
            device.has_broken_texture_view_formats(),
            device_access_memory,
            device.can_report_memory(),
            device.has_astc(),
            device.has_debugging_tool_attached(),
            program_manager,
            state_tracker,
        )
    }

    pub fn new_with_caps(
        has_broken_texture_view_formats: bool,
        device_access_memory: u64,
        can_report_memory_usage: bool,
        has_native_astc: bool,
        has_debugging_tool_attached: bool,
        program_manager: ProgramManagerHandle,
        state_tracker: &mut StateTracker,
    ) -> Self {
        let (null_image_handles, null_image_views) =
            create_null_image_views(has_debugging_tool_attached);
        let mut runtime = Self {
            has_broken_texture_view_formats,
            device_access_memory,
            can_report_memory_usage,
            has_native_astc,
            state_tracker: NonNull::from(state_tracker),
            staging_buffer_pool: StagingBufferPool::new(),
            util_shaders: UtilShaders::new(program_manager),
            format_conversion_pass: FormatConversionPass::new(),
            format_properties: create_format_properties(),
            null_image_handles,
            null_image_views,
            rescale_draw_fbos: [0; 4],
            rescale_read_fbos: [0; 4],
        };
        if settings::values().resolution_info.active {
            unsafe {
                gl::CreateFramebuffers(4, runtime.rescale_draw_fbos.as_mut_ptr());
                gl::CreateFramebuffers(4, runtime.rescale_read_fbos.as_mut_ptr());
            }
        }
        runtime
    }

    pub fn finish(&self) {
        unsafe {
            gl::Finish();
        }
    }

    pub fn upload_staging_buffer(&mut self, size: usize) -> StagingBufferMap {
        self.staging_buffer_pool.request_upload_buffer(size)
    }

    pub fn download_staging_buffer(&mut self, size: usize, deferred: bool) -> StagingBufferMap {
        self.staging_buffer_pool
            .request_download_buffer(size, deferred)
    }

    pub fn free_deferred_staging_buffer(&mut self, buffer: &StagingBufferMap) {
        self.staging_buffer_pool
            .free_deferred_staging_buffer(buffer);
    }

    pub fn blit_framebuffer(
        &mut self,
        dst_framebuffer: u32,
        src_framebuffer: u32,
        dst_buffer_bits: u32,
        src_buffer_bits: u32,
        dst_region: Region2D,
        src_region: Region2D,
        filter: crate::engines::fermi_2d::Filter,
        _operation: crate::engines::fermi_2d::Operation,
    ) {
        let state_tracker = unsafe { self.state_tracker.as_mut() };
        state_tracker.notify_scissor0();
        state_tracker.notify_rasterize_enable();
        state_tracker.notify_framebuffer_srgb();

        debug_assert_eq!(dst_buffer_bits, src_buffer_bits);
        let buffer_bits = dst_buffer_bits;
        let has_depth = (buffer_bits & !gl::COLOR_BUFFER_BIT) != 0;
        let is_linear = !has_depth && filter == crate::engines::fermi_2d::Filter::Bilinear;
        let gl_filter = if is_linear { gl::LINEAR } else { gl::NEAREST };
        unsafe {
            gl::Enable(gl::FRAMEBUFFER_SRGB);
            gl::Disable(gl::RASTERIZER_DISCARD);
            gl::Disablei(gl::SCISSOR_TEST, 0);
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
                buffer_bits,
                gl_filter,
            );
        }
    }

    pub fn notify_rescale_blit_state_changed(&mut self) {
        let state_tracker = unsafe { self.state_tracker.as_mut() };
        state_tracker.notify_viewport0();
        state_tracker.notify_scissor0();
    }

    pub fn can_image_be_copied(&self, dst: &ImageBase, src: &ImageBase) -> bool {
        if dst.info.image_type == ImageType::E3D
            && dst.info.format == crate::surface::PixelFormat::Bc4Unorm
        {
            return false;
        }
        if is_pixel_format_bgr(dst.info.format) != is_pixel_format_bgr(src.info.format) {
            return false;
        }
        true
    }

    #[allow(clippy::too_many_arguments)]
    pub fn reinterpret_image(
        &mut self,
        dst_handle: u32,
        dst_target: u32,
        dst_format: u32,
        dst_type: u32,
        dst_info: &ImageInfo,
        src_handle: u32,
        src_target: u32,
        src_format: u32,
        src_type: u32,
        src_info: &ImageInfo,
        copies: &[ImageCopy],
    ) {
        self.format_conversion_pass.convert_image(
            dst_handle,
            dst_target,
            dst_format,
            dst_type,
            src_handle,
            src_target,
            src_format,
            src_type,
            src_info.format,
            copies,
        );
        if src_info.format == crate::surface::PixelFormat::D24UnormS8Uint
            && dst_info.format == crate::surface::PixelFormat::A8B8G8R8Unorm
        {
            self.util_shaders.convert_s8d24(dst_handle, copies);
        }
        unsafe {
            gl::MemoryBarrier(gl::TEXTURE_FETCH_BARRIER_BIT | gl::SHADER_IMAGE_ACCESS_BARRIER_BIT);
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub fn emulate_copy_image(
        &mut self,
        dst_handle: u32,
        dst_target: u32,
        dst_format: u32,
        dst_type: u32,
        dst_info: &ImageInfo,
        src_handle: u32,
        src_target: u32,
        src_format: u32,
        src_type: u32,
        src_info: &ImageInfo,
        copies: &[ImageCopy],
    ) -> bool {
        if dst_info.image_type == ImageType::E3D
            && dst_info.format == crate::surface::PixelFormat::Bc4Unorm
        {
            debug_assert_eq!(src_info.image_type, ImageType::E3D);
            self.util_shaders.copy_bc4(dst_handle, src_handle, copies);
            return true;
        }
        if is_pixel_format_bgr(dst_info.format) || is_pixel_format_bgr(src_info.format) {
            self.reinterpret_image(
                dst_handle, dst_target, dst_format, dst_type, dst_info, src_handle, src_target,
                src_format, src_type, src_info, copies,
            );
            return true;
        }
        false
    }

    pub fn get_device_local_memory(&self) -> u64 {
        self.device_access_memory
    }

    /// Port of `TextureCacheRuntime::GetDeviceMemoryUsage`. Mirrors the
    /// buffer-cache port: queries `TOTAL_AVAILABLE_MEMORY_NVX` (0x9048),
    /// the same constant upstream uses; subtraction stays non-negative
    /// because the ctor sized `device_access_memory` to `total + 512MiB`.
    pub fn get_device_memory_usage(&self) -> u64 {
        if !self.can_report_memory_usage {
            return 2 * 1024 * 1024 * 1024;
        }
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

    pub fn can_report_memory_usage(&self) -> bool {
        self.can_report_memory_usage
    }

    pub fn should_reinterpret(&self) -> bool {
        true
    }

    pub fn can_upload_msaa(&self) -> bool {
        true
    }

    pub fn format_info(&self, image_type: ImageType, internal_format: u32) -> FormatProperties {
        let table_index = match image_type {
            ImageType::E1D => 0,
            ImageType::E2D | ImageType::Linear => 1,
            ImageType::E3D => 2,
            _ => {
                debug_assert!(false, "unsupported image type {:?}", image_type);
                return FormatProperties::default();
            }
        };
        self.format_properties[table_index]
            .get(&internal_format)
            .cloned()
            .unwrap_or_default()
    }

    pub fn has_native_bgr(&self) -> bool {
        false
    }

    pub fn has_broken_texture_view_formats(&self) -> bool {
        self.has_broken_texture_view_formats
    }

    /// Port of `TextureCacheRuntime::HasNativeASTC`.
    pub fn has_native_astc(&self) -> bool {
        self.has_native_astc
    }

    pub fn insert_upload_memory_barrier(&self) {
        unsafe {
            gl::MemoryBarrier(gl::TEXTURE_FETCH_BARRIER_BIT | gl::SHADER_IMAGE_ACCESS_BARRIER_BIT);
        }
    }

    pub fn transition_image_layout(&self, _image: &ImageBase) {}

    pub fn tick_frame(&self) {}

    pub fn barrier_feedback_loop(&self) {}

    pub fn accelerate_image_upload(
        &mut self,
        image_handle: u32,
        info: &ImageInfo,
        map: &StagingBufferMap,
        swizzles: &[crate::texture_cache::types::SwizzleParameters],
    ) {
        match info.image_type {
            ImageType::E2D => {
                if crate::surface::is_pixel_format_astc(info.format) {
                    self.util_shaders.astc_decode(
                        image_handle,
                        map.buffer,
                        map.offset,
                        map.mapped_size,
                        info,
                        swizzles,
                    );
                } else {
                    self.util_shaders.block_linear_upload_2d(
                        image_handle,
                        map.buffer,
                        map.offset,
                        map.mapped_size,
                        info,
                        swizzles,
                    );
                }
            }
            ImageType::E3D => self.util_shaders.block_linear_upload_3d(
                image_handle,
                map.buffer,
                map.offset,
                map.mapped_size,
                info,
                swizzles,
            ),
            ImageType::Linear => self.util_shaders.pitch_upload(
                image_handle,
                map.buffer,
                map.offset,
                map.mapped_size,
                info,
                swizzles,
            ),
            _ => log::warn!(
                "TextureCacheRuntime::accelerate_image_upload unsupported image type {:?}",
                info.image_type
            ),
        }
    }
}

impl Drop for TextureCacheRuntime {
    fn drop(&mut self) {
        unsafe {
            for &handle in &self.null_image_handles {
                if handle != 0 {
                    gl::DeleteTextures(1, &handle);
                }
            }
            gl::DeleteFramebuffers(4, self.rescale_draw_fbos.as_ptr());
            gl::DeleteFramebuffers(4, self.rescale_read_fbos.as_ptr());
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct CopyOrigin {
    level: i32,
    x: i32,
    y: i32,
    z: i32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct CopyRegion {
    width: i32,
    height: i32,
    depth: i32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct CompressedBlockCopyRect {
    read_width: i32,
    read_height: i32,
    write_width: i32,
    write_height: i32,
    buffer_size: i32,
}

fn compressed_block_copy_rect(
    src_size: (i32, i32, i32),
    dst_size: (i32, i32, i32),
    src_origin: CopyOrigin,
    dst_origin: CopyOrigin,
    region: CopyRegion,
    block_width: u32,
    block_height: u32,
    bytes_per_block: u32,
) -> Option<CompressedBlockCopyRect> {
    if block_width <= 1 && block_height <= 1
        || bytes_per_block == 0
        || region.width <= 0
        || region.height <= 0
        || region.depth <= 0
        || src_origin.x < 0
        || src_origin.y < 0
        || dst_origin.x < 0
        || dst_origin.y < 0
    {
        return None;
    }

    let block_width = block_width as i32;
    let block_height = block_height as i32;
    if src_origin.x % block_width != 0
        || src_origin.y % block_height != 0
        || dst_origin.x % block_width != 0
        || dst_origin.y % block_height != 0
    {
        return None;
    }

    let src_remaining_width = src_size.0 - src_origin.x;
    let src_remaining_height = src_size.1 - src_origin.y;
    let dst_remaining_width = dst_size.0 - dst_origin.x;
    let dst_remaining_height = dst_size.1 - dst_origin.y;
    let src_remaining_depth = src_size.2 - src_origin.z;
    let dst_remaining_depth = dst_size.2 - dst_origin.z;
    if src_remaining_width <= 0
        || src_remaining_height <= 0
        || dst_remaining_width <= 0
        || dst_remaining_height <= 0
        || src_remaining_depth < region.depth
        || dst_remaining_depth < region.depth
    {
        return None;
    }

    let blocks_x = (region.width + block_width - 1) / block_width;
    let blocks_y = (region.height + block_height - 1) / block_height;
    let block_width_pixels = blocks_x * block_width;
    let block_height_pixels = blocks_y * block_height;
    let read_width = block_width_pixels.min(src_remaining_width);
    let read_height = block_height_pixels.min(src_remaining_height);
    let write_width = block_width_pixels.min(dst_remaining_width);
    let write_height = block_height_pixels.min(dst_remaining_height);
    if read_width <= 0 || read_height <= 0 || write_width <= 0 || write_height <= 0 {
        return None;
    }

    let buffer_size = blocks_x
        .checked_mul(blocks_y)?
        .checked_mul(region.depth)?
        .checked_mul(bytes_per_block as i32)?;
    Some(CompressedBlockCopyRect {
        read_width,
        read_height,
        write_width,
        write_height,
        buffer_size,
    })
}

fn compressed_copy_fallback_format(
    dst_format: PixelFormat,
    src_format: PixelFormat,
) -> Option<(u32, u32, u32)> {
    if compressed_copy_layout_class(dst_format)? != compressed_copy_layout_class(src_format)? {
        return None;
    }
    let block_width = crate::surface::default_block_width(dst_format);
    let block_height = crate::surface::default_block_height(dst_format);
    if block_width != crate::surface::default_block_width(src_format)
        || block_height != crate::surface::default_block_height(src_format)
    {
        return None;
    }
    if block_width <= 1 && block_height <= 1 {
        return None;
    }
    let bytes_per_block = crate::surface::bytes_per_block(dst_format);
    if bytes_per_block != crate::surface::bytes_per_block(src_format) {
        return None;
    }
    (bytes_per_block != 0).then_some((block_width, block_height, bytes_per_block))
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum RawCompressedCopyDirection {
    RawToCompressed,
    CompressedToRaw,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct RawCompressedCopyFormat {
    direction: RawCompressedCopyDirection,
    compressed_format: PixelFormat,
    raw_format: PixelFormat,
    block_width: u32,
    block_height: u32,
    bytes_per_block: u32,
}

fn raw_compressed_copy_fallback_format(
    dst_format: PixelFormat,
    src_format: PixelFormat,
) -> Option<RawCompressedCopyFormat> {
    let dst_compressed = compressed_copy_layout_class(dst_format).is_some();
    let src_compressed = compressed_copy_layout_class(src_format).is_some();
    let (direction, compressed_format, raw_format) = match (dst_compressed, src_compressed) {
        (true, false) => (
            RawCompressedCopyDirection::RawToCompressed,
            dst_format,
            src_format,
        ),
        (false, true) => (
            RawCompressedCopyDirection::CompressedToRaw,
            src_format,
            dst_format,
        ),
        _ => return None,
    };
    let block_width = crate::surface::default_block_width(compressed_format);
    let block_height = crate::surface::default_block_height(compressed_format);
    let bytes_per_block = crate::surface::bytes_per_block(compressed_format);
    let raw_bytes_per_texel = crate::surface::bytes_per_block(raw_format);
    let raw_tuple = super::maxwell_to_gl::get_format_tuple(raw_format as usize);
    if block_width <= 1 && block_height <= 1
        || bytes_per_block == 0
        || raw_bytes_per_texel == 0
        || bytes_per_block != raw_bytes_per_texel
        || raw_tuple.format == gl::NONE
        || raw_tuple.gl_type == gl::NONE
    {
        return None;
    }
    Some(RawCompressedCopyFormat {
        direction,
        compressed_format,
        raw_format,
        block_width,
        block_height,
        bytes_per_block,
    })
}

fn compressed_copy_layout_class(format: PixelFormat) -> Option<u8> {
    match format {
        PixelFormat::Bc1RgbaUnorm | PixelFormat::Bc1RgbaSrgb => Some(1),
        PixelFormat::Bc2Unorm | PixelFormat::Bc2Srgb => Some(2),
        PixelFormat::Bc3Unorm | PixelFormat::Bc3Srgb => Some(3),
        PixelFormat::Bc4Unorm | PixelFormat::Bc4Snorm => Some(4),
        PixelFormat::Bc5Unorm | PixelFormat::Bc5Snorm => Some(5),
        PixelFormat::Bc6hUfloat | PixelFormat::Bc6hSfloat => Some(6),
        PixelFormat::Bc7Unorm | PixelFormat::Bc7Srgb => Some(7),
        PixelFormat::Astc2d4x4Unorm | PixelFormat::Astc2d4x4Srgb => Some(20),
        PixelFormat::Astc2d5x4Unorm | PixelFormat::Astc2d5x4Srgb => Some(21),
        PixelFormat::Astc2d5x5Unorm | PixelFormat::Astc2d5x5Srgb => Some(22),
        PixelFormat::Astc2d6x5Unorm | PixelFormat::Astc2d6x5Srgb => Some(23),
        PixelFormat::Astc2d6x6Unorm | PixelFormat::Astc2d6x6Srgb => Some(24),
        PixelFormat::Astc2d8x5Unorm | PixelFormat::Astc2d8x5Srgb => Some(25),
        PixelFormat::Astc2d8x6Unorm | PixelFormat::Astc2d8x6Srgb => Some(26),
        PixelFormat::Astc2d8x8Unorm | PixelFormat::Astc2d8x8Srgb => Some(27),
        PixelFormat::Astc2d10x5Unorm | PixelFormat::Astc2d10x5Srgb => Some(28),
        PixelFormat::Astc2d10x6Unorm | PixelFormat::Astc2d10x6Srgb => Some(29),
        PixelFormat::Astc2d10x8Unorm | PixelFormat::Astc2d10x8Srgb => Some(30),
        PixelFormat::Astc2d10x10Unorm | PixelFormat::Astc2d10x10Srgb => Some(31),
        PixelFormat::Astc2d12x10Unorm | PixelFormat::Astc2d12x10Srgb => Some(32),
        PixelFormat::Astc2d12x12Unorm | PixelFormat::Astc2d12x12Srgb => Some(33),
        _ => None,
    }
}

/// Port of upstream `MakeCopyOrigin` (gl_texture_cache.cpp:278).
fn make_copy_origin(
    offset: crate::texture_cache::types::Offset3D,
    subresource: crate::texture_cache::types::SubresourceLayers,
    target: u32,
) -> CopyOrigin {
    match target {
        gl::TEXTURE_1D => CopyOrigin {
            level: subresource.base_level,
            x: offset.x,
            y: 0,
            z: 0,
        },
        gl::TEXTURE_1D_ARRAY => CopyOrigin {
            level: subresource.base_level,
            x: offset.x,
            y: 0,
            z: subresource.base_layer,
        },
        gl::TEXTURE_2D_ARRAY | gl::TEXTURE_2D_MULTISAMPLE_ARRAY => CopyOrigin {
            level: subresource.base_level,
            x: offset.x,
            y: offset.y,
            z: subresource.base_layer,
        },
        gl::TEXTURE_3D => CopyOrigin {
            level: subresource.base_level,
            x: offset.x,
            y: offset.y,
            z: offset.z,
        },
        _ => {
            log::warn!("gl_texture_cache::make_copy_origin: unhandled target=0x{target:x}");
            CopyOrigin {
                level: 0,
                x: 0,
                y: 0,
                z: 0,
            }
        }
    }
}

/// Port of upstream `MakeCopyRegion` (gl_texture_cache.cpp:313).
fn make_copy_region(
    extent: crate::texture_cache::types::Extent3D,
    dst_subresource: crate::texture_cache::types::SubresourceLayers,
    target: u32,
) -> CopyRegion {
    match target {
        gl::TEXTURE_1D => CopyRegion {
            width: extent.width as i32,
            height: 1,
            depth: 1,
        },
        gl::TEXTURE_1D_ARRAY => CopyRegion {
            width: extent.width as i32,
            height: 1,
            depth: dst_subresource.num_layers,
        },
        gl::TEXTURE_2D_ARRAY | gl::TEXTURE_2D_MULTISAMPLE_ARRAY => CopyRegion {
            width: extent.width as i32,
            height: extent.height as i32,
            depth: dst_subresource.num_layers,
        },
        gl::TEXTURE_3D => CopyRegion {
            width: extent.width as i32,
            height: extent.height as i32,
            depth: extent.depth as i32,
        },
        _ => {
            log::warn!("gl_texture_cache::make_copy_region: unhandled target=0x{target:x}");
            CopyRegion {
                width: 0,
                height: 0,
                depth: 0,
            }
        }
    }
}

fn texture_level_extent(texture: u32, level: i32) -> (i32, i32, i32) {
    let mut width = 0;
    let mut height = 0;
    let mut depth = 0;
    unsafe {
        gl::GetTextureLevelParameteriv(texture, level, gl::TEXTURE_WIDTH, &mut width);
        gl::GetTextureLevelParameteriv(texture, level, gl::TEXTURE_HEIGHT, &mut height);
        gl::GetTextureLevelParameteriv(texture, level, gl::TEXTURE_DEPTH, &mut depth);
    }
    (width, height, depth)
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

fn swizzle(source: SwizzleSource) -> i32 {
    match source {
        SwizzleSource::Zero => gl::ZERO as i32,
        SwizzleSource::R => gl::RED as i32,
        SwizzleSource::G => gl::GREEN as i32,
        SwizzleSource::B => gl::BLUE as i32,
        SwizzleSource::A => gl::ALPHA as i32,
        SwizzleSource::OneInt | SwizzleSource::OneFloat => gl::ONE as i32,
    }
}

fn pack_u8x4(values: [u8; 4]) -> u64 {
    values[0] as u64
        | ((values[1] as u64) << 8)
        | ((values[2] as u64) << 16)
        | ((values[3] as u64) << 24)
}

fn pack_i32x4(values: [i32; 4]) -> u64 {
    (values[0] as u32 as u64)
        | ((values[1] as u32 as u64) << 16)
        | ((values[2] as u32 as u64) << 32)
        | ((values[3] as u32 as u64) << 48)
}

fn convert_green_red(value: SwizzleSource) -> SwizzleSource {
    match value {
        SwizzleSource::G => SwizzleSource::R,
        _ => value,
    }
}

fn convert_a5b5g5r1_unorm(source: SwizzleSource) -> i32 {
    match source {
        SwizzleSource::Zero => gl::ZERO as i32,
        SwizzleSource::R => gl::ALPHA as i32,
        SwizzleSource::G => gl::BLUE as i32,
        SwizzleSource::B => gl::GREEN as i32,
        SwizzleSource::A => gl::RED as i32,
        SwizzleSource::OneInt | SwizzleSource::OneFloat => gl::ONE as i32,
    }
}

fn texture_mode(format: PixelFormat, swizzle: [SwizzleSource; 4]) -> u32 {
    let any_r = swizzle.iter().any(|source| *source == SwizzleSource::R);
    match format {
        PixelFormat::D24UnormS8Uint | PixelFormat::D32FloatS8Uint => {
            if any_r {
                gl::DEPTH_COMPONENT
            } else {
                gl::STENCIL_INDEX
            }
        }
        PixelFormat::S8UintD24Unorm => {
            if any_r {
                gl::STENCIL_INDEX
            } else {
                gl::DEPTH_COMPONENT
            }
        }
        _ => gl::DEPTH_COMPONENT,
    }
}

fn apply_swizzle(handle: u32, format: PixelFormat, mut source_swizzle: [SwizzleSource; 4]) {
    unsafe {
        match format {
            PixelFormat::D24UnormS8Uint
            | PixelFormat::D32FloatS8Uint
            | PixelFormat::S8UintD24Unorm => {
                gl::TextureParameteri(
                    handle,
                    gl::DEPTH_STENCIL_TEXTURE_MODE,
                    texture_mode(format, source_swizzle) as i32,
                );
                source_swizzle = source_swizzle.map(convert_green_red);
            }
            PixelFormat::A5B5G5R1Unorm => {
                let gl_swizzle = source_swizzle.map(convert_a5b5g5r1_unorm);
                gl::TextureParameteriv(handle, gl::TEXTURE_SWIZZLE_RGBA, gl_swizzle.as_ptr());
                return;
            }
            _ => {}
        }
        let gl_swizzle = source_swizzle.map(swizzle);
        gl::TextureParameteriv(handle, gl::TEXTURE_SWIZZLE_RGBA, gl_swizzle.as_ptr());
    }
}

fn gl_swizzle_for_trace(format: PixelFormat, mut source_swizzle: [SwizzleSource; 4]) -> [i32; 4] {
    match format {
        PixelFormat::D24UnormS8Uint | PixelFormat::D32FloatS8Uint | PixelFormat::S8UintD24Unorm => {
            source_swizzle = source_swizzle.map(convert_green_red);
        }
        PixelFormat::A5B5G5R1Unorm => {
            return source_swizzle.map(convert_a5b5g5r1_unorm);
        }
        _ => {}
    }
    source_swizzle.map(swizzle)
}

fn decode_swizzle(raw: [u8; 4]) -> Option<[SwizzleSource; 4]> {
    fn decode(value: u8) -> Option<SwizzleSource> {
        match value {
            0 => Some(SwizzleSource::Zero),
            2 => Some(SwizzleSource::R),
            3 => Some(SwizzleSource::G),
            4 => Some(SwizzleSource::B),
            5 => Some(SwizzleSource::A),
            6 => Some(SwizzleSource::OneInt),
            7 => Some(SwizzleSource::OneFloat),
            _ => None,
        }
    }
    Some([
        decode(raw[0])?,
        decode(raw[1])?,
        decode(raw[2])?,
        decode(raw[3])?,
    ])
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
    pub converted: bool,
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
            converted: false,
        }
    }

    pub fn handle(&self) -> u32 {
        self.current_texture
    }

    /// Port of `OpenGL::Image::Image(TextureCacheRuntime&, const VideoCommon::ImageInfo&,
    /// GPUVAddr, VAddr)` (gl_texture_cache.cpp:692-728).
    ///
    /// Allocates the backing GL texture via `MakeImage` (`image_target` +
    /// `glTextureStorage*D` per dimensionality), including upstream's
    /// converted-format retargeting for ASTC-without-native-support and
    /// BC4/BC5 3D images.
    pub fn from_base(base: &ImageBase, has_native_astc: bool) -> Self {
        let converted = is_converted_image(has_native_astc, base.info.format, base.info.image_type);
        let (gl_internal_format, gl_format, gl_type) = if converted {
            let is_srgb = crate::surface::is_pixel_format_srgb(base.info.format);
            let mut internal_format = if is_srgb { gl::SRGB8_ALPHA8 } else { gl::RGBA8 };
            let mut format = gl::RGBA;
            if crate::surface::is_pixel_format_astc(base.info.format)
                && is_astc_recompression_enabled()
            {
                internal_format = select_astc_format(base.info.format, is_srgb);
                format = gl::NONE;
            }
            (internal_format, format, gl::UNSIGNED_INT_8_8_8_8_REV)
        } else {
            let tuple = super::maxwell_to_gl::get_format_tuple(base.info.format as usize);
            (tuple.internal_format, tuple.format, tuple.gl_type)
        };
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
        let texture = make_image(&base.info, gl_internal_format, gl_num_levels, target);

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
            gl_internal_format,
            gl_format,
            gl_type,
            gl_num_levels,
            current_texture: texture,
            num_samples: base.info.num_samples as i32,
            image_type: base.info.image_type,
            converted,
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
            // RUZU_TRACE_GL_UPLOAD_ERR=1 — when a copy raises a GL error
            // (e.g. "out of bounds PBO access"), log the full copy + PBO
            // geometry so the offending texture/staging size mismatch can be
            // identified. Gated: glGetError stalls the pipeline.
            if std::env::var_os("RUZU_TRACE_GL_UPLOAD_ERR").is_some() {
                let err = unsafe { gl::GetError() };
                if err != gl::NO_ERROR {
                    let mut pbo_size: i64 = -1;
                    unsafe {
                        gl::GetNamedBufferParameteri64v(
                            buffer_handle,
                            gl::BUFFER_SIZE,
                            &mut pbo_size,
                        );
                    }
                    log::warn!(
                        "[GL_UPLOAD_ERR] err=0x{:X} tex={} type={:?} ifmt=0x{:X} fmt=0x{:X} ty=0x{:X} \
                         level={} layer={}+{} off=({},{},{}) extent={}x{}x{} row_len={} img_h={} \
                         buf_off=0x{:X}+0x{:X} buf_size=0x{:X} pbo_size=0x{:X}",
                        err,
                        self.texture,
                        self.image_type,
                        self.gl_internal_format,
                        self.gl_format,
                        self.gl_type,
                        copy.image_subresource.base_level,
                        copy.image_subresource.base_layer,
                        copy.image_subresource.num_layers,
                        copy.image_offset.x,
                        copy.image_offset.y,
                        copy.image_offset.z,
                        copy.image_extent.width,
                        copy.image_extent.height,
                        copy.image_extent.depth,
                        copy.buffer_row_length,
                        copy.buffer_image_height,
                        buffer_offset,
                        copy.buffer_offset,
                        copy.buffer_size,
                        pbo_size,
                    );
                }
            }
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

    /// Port of `Image::DownloadMemory(GLuint, size_t, span<BufferImageCopy>)`.
    pub fn download_memory_to_buffer(
        &mut self,
        buffer_handle: u32,
        buffer_offset: usize,
        copies: &[BufferImageCopy],
    ) {
        if self.current_texture == 0 || buffer_handle == 0 {
            return;
        }
        unsafe {
            gl::MemoryBarrier(gl::PIXEL_BUFFER_BARRIER_BIT);
            gl::BindBuffer(gl::PIXEL_PACK_BUFFER, buffer_handle);
            gl::PixelStorei(gl::PACK_ALIGNMENT, 1);
        }
        let mut current_row_length = u32::MAX;
        let mut current_image_height = u32::MAX;
        for copy in copies {
            if copy.image_subresource.base_level >= self.gl_num_levels {
                continue;
            }
            if current_row_length != copy.buffer_row_length {
                current_row_length = copy.buffer_row_length;
                unsafe {
                    gl::PixelStorei(gl::PACK_ROW_LENGTH, current_row_length as i32);
                }
            }
            if current_image_height != copy.buffer_image_height {
                current_image_height = copy.buffer_image_height;
                unsafe {
                    gl::PixelStorei(gl::PACK_IMAGE_HEIGHT, current_image_height as i32);
                }
            }
            self.copy_image_to_buffer(copy, buffer_offset);
        }
        unsafe {
            gl::BindBuffer(gl::PIXEL_PACK_BUFFER, 0);
        }
    }

    /// Port of `Image::DownloadMemory(StagingBufferMap&, span<BufferImageCopy>)`.
    pub fn download_memory_to_staging(
        &mut self,
        map: &mut StagingBufferMap,
        copies: &[BufferImageCopy],
    ) {
        self.download_memory_to_buffer(map.buffer, map.offset, copies);
    }

    /// Backward-compatible direct host read helper for diagnostics/tests.
    pub fn download_memory(&mut self, output: &mut [u8], level: i32) {
        if self.current_texture == 0 || output.is_empty() {
            return;
        }
        unsafe {
            gl::BindBuffer(gl::PIXEL_PACK_BUFFER, 0);
            gl::PixelStorei(gl::PACK_ALIGNMENT, 1);
            if self.gl_format == gl::NONE {
                gl::GetCompressedTextureImage(
                    self.current_texture,
                    level,
                    output.len() as i32,
                    output.as_mut_ptr() as *mut _,
                );
            } else {
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
    }

    /// Port of `Image::CopyImageToBuffer`.
    fn copy_image_to_buffer(&self, copy: &BufferImageCopy, buffer_offset: usize) {
        let level = copy.image_subresource.base_level;
        let base_layer = copy.image_subresource.base_layer;
        let num_layers = copy.image_subresource.num_layers;
        let width = copy.image_extent.width as i32;
        let height = copy.image_extent.height as i32;
        let depth = copy.image_extent.depth as i32;
        let ox = copy.image_offset.x;
        let oy = copy.image_offset.y;
        let oz = copy.image_offset.z;
        let offset = (copy.buffer_offset + buffer_offset) as *mut std::ffi::c_void;
        let buf_size = copy.buffer_size as i32;
        unsafe {
            match self.image_type {
                ImageType::E1D => {
                    if self.gl_format == gl::NONE {
                        gl::GetCompressedTextureSubImage(
                            self.current_texture,
                            level,
                            ox,
                            base_layer,
                            0,
                            width,
                            num_layers,
                            1,
                            buf_size,
                            offset,
                        );
                    } else {
                        gl::GetTextureSubImage(
                            self.current_texture,
                            level,
                            ox,
                            base_layer,
                            0,
                            width,
                            num_layers,
                            1,
                            self.gl_format,
                            self.gl_type,
                            buf_size,
                            offset,
                        );
                    }
                }
                ImageType::E2D | ImageType::Linear => {
                    if self.gl_format == gl::NONE {
                        gl::GetCompressedTextureSubImage(
                            self.current_texture,
                            level,
                            ox,
                            oy,
                            base_layer,
                            width,
                            height,
                            num_layers,
                            buf_size,
                            offset,
                        );
                    } else {
                        gl::GetTextureSubImage(
                            self.current_texture,
                            level,
                            ox,
                            oy,
                            base_layer,
                            width,
                            height,
                            num_layers,
                            self.gl_format,
                            self.gl_type,
                            buf_size,
                            offset,
                        );
                    }
                }
                ImageType::E3D => {
                    if self.gl_format == gl::NONE {
                        gl::GetCompressedTextureSubImage(
                            self.current_texture,
                            level,
                            ox,
                            oy,
                            oz,
                            width,
                            height,
                            depth,
                            buf_size,
                            offset,
                        );
                    } else {
                        gl::GetTextureSubImage(
                            self.current_texture,
                            level,
                            ox,
                            oy,
                            oz,
                            width,
                            height,
                            depth,
                            self.gl_format,
                            self.gl_type,
                            buf_size,
                            offset,
                        );
                    }
                }
                ImageType::Buffer => {
                    log::warn!(
                        "Image::copy_image_to_buffer: called on Buffer-type image — should never happen"
                    );
                }
            }
        }
    }

    /// Port of `Image::ScaleUp`.
    /// Scales the image up by the resolution scaling factor.
    pub fn scale_up(
        &mut self,
        base: &mut ImageBase,
        rescale_read_fbos: &[u32; 4],
        rescale_draw_fbos: &[u32; 4],
        ignore: bool,
    ) -> bool {
        let resolution = settings::values().resolution_info.clone();
        if !resolution.active {
            return false;
        }
        if base.flags.contains(ImageFlagBits::RESCALED) {
            return false;
        }
        if self.gl_format == gl::NONE && self.gl_type == gl::NONE {
            return false;
        }
        if base.info.image_type == ImageType::Linear {
            debug_assert!(false, "Image::scale_up called for linear image");
            return false;
        }
        base.flags.insert(ImageFlagBits::RESCALED);
        base.has_scaled = true;
        if ignore {
            self.current_texture = self.upscaled_backup;
            return true;
        }
        self.scale(base, rescale_read_fbos, rescale_draw_fbos, true);
        true
    }

    /// Port of `Image::ScaleDown`.
    /// Scales the image down from the resolution scaling factor.
    pub fn scale_down(
        &mut self,
        base: &mut ImageBase,
        rescale_read_fbos: &[u32; 4],
        rescale_draw_fbos: &[u32; 4],
        ignore: bool,
    ) -> bool {
        let resolution = settings::values().resolution_info.clone();
        if !resolution.active {
            return false;
        }
        if !base.flags.contains(ImageFlagBits::RESCALED) {
            return false;
        }
        base.flags.remove(ImageFlagBits::RESCALED);
        if ignore {
            self.current_texture = self.texture;
            return true;
        }
        self.scale(base, rescale_read_fbos, rescale_draw_fbos, false);
        true
    }

    fn scale(
        &mut self,
        base: &ImageBase,
        rescale_read_fbos: &[u32; 4],
        rescale_draw_fbos: &[u32; 4],
        up_scale: bool,
    ) {
        let format_type = crate::surface::get_format_type(base.info.format);
        let attachment = rescale_attachment_type(format_type);
        let mask = rescale_buffer_mask(format_type);
        let fbo_index = rescale_fbo_index(format_type);
        let is_2d = base.info.image_type == ImageType::E2D;
        let is_color = (mask & gl::COLOR_BUFFER_BIT) != 0;
        let linear_color_format =
            is_color && !crate::surface::is_pixel_format_integer(base.info.format);
        let filter = if linear_color_format {
            gl::LINEAR
        } else {
            gl::NEAREST
        };
        let resolution = settings::values().resolution_info.clone();
        let scaled_width = resolution.scale_up_u32(base.info.size.width);
        let scaled_height = if is_2d {
            resolution.scale_up_u32(base.info.size.height)
        } else {
            base.info.size.height
        };
        let original_width = base.info.size.width;
        let original_height = base.info.size.height;

        if self.upscaled_backup == 0 {
            let mut dst_info = base.info.clone();
            dst_info.size.width = scaled_width;
            dst_info.size.height = scaled_height;
            self.upscaled_backup = make_image(
                &dst_info,
                self.gl_internal_format,
                self.gl_num_levels,
                image_target(&dst_info),
            );
        }
        if self.upscaled_backup == 0 || self.texture == 0 {
            return;
        }

        let src_width = if up_scale {
            original_width
        } else {
            scaled_width
        };
        let src_height = if up_scale {
            original_height
        } else {
            scaled_height
        };
        let dst_width = if up_scale {
            scaled_width
        } else {
            original_width
        };
        let dst_height = if up_scale {
            scaled_height
        } else {
            original_height
        };
        let src_handle = if up_scale {
            self.texture
        } else {
            self.upscaled_backup
        };
        let dst_handle = if up_scale {
            self.upscaled_backup
        } else {
            self.texture
        };
        let read_fbo = rescale_read_fbos[fbo_index];
        let draw_fbo = rescale_draw_fbos[fbo_index];
        if read_fbo == 0 || draw_fbo == 0 {
            return;
        }

        unsafe {
            gl::Disablei(gl::SCISSOR_TEST, 0);
            gl::ViewportIndexedf(0, 0.0, 0.0, dst_width as f32, dst_height as f32);
            for layer in 0..base.info.resources.layers {
                for level in 0..base.info.resources.levels.min(self.gl_num_levels) {
                    let src_level_width = std::cmp::max(1, src_width >> level);
                    let src_level_height = std::cmp::max(1, src_height >> level);
                    let dst_level_width = std::cmp::max(1, dst_width >> level);
                    let dst_level_height = std::cmp::max(1, dst_height >> level);
                    gl::NamedFramebufferTextureLayer(
                        read_fbo, attachment, src_handle, level, layer,
                    );
                    gl::NamedFramebufferTextureLayer(
                        draw_fbo, attachment, dst_handle, level, layer,
                    );
                    gl::BlitNamedFramebuffer(
                        read_fbo,
                        draw_fbo,
                        0,
                        0,
                        src_level_width as i32,
                        src_level_height as i32,
                        0,
                        0,
                        dst_level_width as i32,
                        dst_level_height as i32,
                        mask,
                        filter,
                    );
                }
            }
        }
        self.current_texture = dst_handle;
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
    image_id: ImageId,
    format: PixelFormat,
    view_type: ImageViewType,
    swizzle: [u8; 4],
    original_texture: u32,
    num_samples: i32,
    flat_range: SubresourceRange,
    full_range: SubresourceRange,
    size: Extent3D,
    flags: ImageViewFlagBits,
    is_render_target: bool,
    /// Snapshot of `ImageViewBase::supports_anisotropy()` captured at
    /// materialisation. Used by the rasterizer's bind loop to pick the
    /// fallback-anisotropy sampler when the format can't carry the
    /// descriptor's configured anisotropy (upstream
    /// gl_graphics_pipeline.cpp:490-493).
    supports_anisotropy: bool,
    owned_views: Vec<u32>,
    /// Lazily-allocated storage-view cache. Upstream uses
    /// `std::unique_ptr<StorageViews>` — same lazy-alloc pattern.
    storage_views: Option<StorageViews>,
}

impl ImageView {
    pub fn new() -> Self {
        Self::with_null_views([0; NUM_TEXTURE_TYPES])
    }

    fn with_null_views(null_image_views: [u32; NUM_TEXTURE_TYPES]) -> Self {
        Self {
            views: null_image_views,
            default_handle: 0,
            internal_format: gl::NONE,
            buffer_size: 0,
            image_id: NULL_IMAGE_ID,
            format: PixelFormat::Invalid,
            view_type: ImageViewType::Buffer,
            swizzle: [
                SwizzleSource::R as u8,
                SwizzleSource::G as u8,
                SwizzleSource::B as u8,
                SwizzleSource::A as u8,
            ],
            original_texture: 0,
            num_samples: 0,
            flat_range: SubresourceRange::default(),
            full_range: SubresourceRange::default(),
            size: Extent3D::default(),
            flags: ImageViewFlagBits::empty(),
            is_render_target: false,
            supports_anisotropy: false,
            owned_views: Vec::new(),
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
    pub fn new_color_2d(
        base: &ImageViewBase,
        image: &Image,
        null_image_views: [u32; NUM_TEXTURE_TYPES],
    ) -> Self {
        let mut view = Self::with_null_views(null_image_views);
        view.image_id = base.image_id;
        view.internal_format = present_internal_format(base.format);
        view.format = base.format;
        view.view_type = base.view_type;
        view.swizzle = base.swizzle;
        view.original_texture = image.handle();
        view.num_samples = image.num_samples;
        view.flat_range = base.range;
        view.full_range = base.range;
        view.size = base.size;
        view.flags = base.flags;
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

    fn matches_base_image(&self, base: &ImageViewBase, image: &Image) -> bool {
        self.image_id == base.image_id
            && self.original_texture == image.handle()
            && self.format == base.format
            && self.view_type == base.view_type
            && self.swizzle == base.swizzle
            && self.full_range == base.range
            && self.size == base.size
            && self.flags == base.flags
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
    /// The upstream `Converted` / ASTC / SRGB re-targeting branch keys off
    /// `image.flags & ImageFlagBits::Converted`. Ruzu mirrors that through
    /// the backend `Image::converted` snapshot because `ImageBase` and
    /// backend `Image` are stored separately.
    pub fn from_image_view_info(
        base: &ImageViewBase,
        image: &Image,
        null_image_views: [u32; NUM_TEXTURE_TYPES],
    ) -> Self {
        use crate::texture_cache::types::ImageViewType;

        let mut view = Self::with_null_views(null_image_views);
        view.image_id = base.image_id;
        view.internal_format = if image.converted {
            let is_srgb = crate::surface::is_pixel_format_srgb(base.format);
            let mut internal_format = if is_srgb { gl::SRGB8_ALPHA8 } else { gl::RGBA8 };
            if crate::surface::is_pixel_format_astc(base.format) && is_astc_recompression_enabled()
            {
                internal_format = select_astc_format(base.format, is_srgb);
            }
            internal_format
        } else {
            present_internal_format(base.format)
        };
        view.format = base.format;
        view.view_type = base.view_type;
        view.swizzle = base.swizzle;
        view.original_texture = image.handle();
        view.num_samples = image.num_samples;
        view.full_range = base.range;
        view.flat_range = base.range;
        view.size = base.size;
        view.flags = base.flags;
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

    fn make_view(&mut self, view_type: TextureType, view_format: u32) -> u32 {
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
                if view_err != gl::NO_ERROR
                    || std::env::var_os("RUZU_TRACE_TEXTURE_VIEW_CREATE").is_some()
                {
                    let mut original_width = 0;
                    let mut original_height = 0;
                    let mut original_depth = 0;
                    gl::GetTextureLevelParameteriv(
                        self.original_texture,
                        view_range.base.level,
                        gl::TEXTURE_WIDTH,
                        &mut original_width,
                    );
                    gl::GetTextureLevelParameteriv(
                        self.original_texture,
                        view_range.base.level,
                        gl::TEXTURE_HEIGHT,
                        &mut original_height,
                    );
                    gl::GetTextureLevelParameteriv(
                        self.original_texture,
                        view_range.base.level,
                        gl::TEXTURE_DEPTH,
                        &mut original_depth,
                    );
                    log::warn!(
                        "[TEXTURE_VIEW_CREATE] err=0x{:X} view={} view_type={:?} target=0x{:X} original={} fmt=0x{:X} level={}+{} layer={}+{} original_size={}x{}x{}",
                        view_err,
                        view,
                        view_type,
                        target,
                        self.original_texture,
                        view_format,
                        view_range.base.level,
                        view_range.extent.levels,
                        view_range.base.layer,
                        view_range.extent.layers,
                        original_width,
                        original_height,
                        original_depth,
                    );
                }
                gl::TextureParameteri(view, gl::TEXTURE_MIN_FILTER, gl::NEAREST as i32);
                gl::TextureParameteri(view, gl::TEXTURE_MAG_FILTER, gl::NEAREST as i32);
                gl::TextureParameteri(view, gl::TEXTURE_WRAP_S, gl::CLAMP_TO_EDGE as i32);
                gl::TextureParameteri(view, gl::TEXTURE_WRAP_T, gl::CLAMP_TO_EDGE as i32);
                if !self.is_render_target {
                    if let Some(swizzle) = decode_swizzle(self.swizzle) {
                        apply_swizzle(view, self.format, swizzle);
                    }
                }
            }
        }
        if view != 0 {
            self.owned_views.push(view);
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
            for &view in &self.owned_views {
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

fn trace_image_view_event(
    stage: u64,
    view_id: ImageViewId,
    view_base: &ImageViewBase,
    image: &Image,
    view: Option<&ImageView>,
) {
    if !common::trace::is_enabled(common::trace::cat::IMAGE_VIEW) {
        return;
    }
    if !should_trace_image_view_address(view_base.gpu_addr) {
        return;
    }
    let range = view_base.range;
    let pack_size = view_base.size.width as u64 | ((view_base.size.height as u64) << 32);
    let pack_range = range.base.level as u64
        | ((range.base.layer as u64) << 16)
        | ((range.extent.levels as u64) << 32)
        | ((range.extent.layers as u64) << 48);
    let default_handle = view.map_or(0, ImageView::default_handle);
    let color2d = view.map_or(0, |view| view.handle_for_texture_type(TextureType::Color2D));
    let color_array2d = view.map_or(0, |view| {
        view.handle_for_texture_type(TextureType::ColorArray2D)
    });
    common::trace::emit_raw(
        common::trace::cat::IMAGE_VIEW,
        &[
            stage,
            view_id.index as u64,
            view_base.image_id.index as u64,
            image.texture as u64,
            image.current_texture as u64,
            default_handle as u64,
            color2d as u64,
            color_array2d as u64,
            view_base.view_type as u64,
            view_base.format as u64,
            pack_size,
            view_base.gpu_addr,
            pack_range,
            view_base.flags.bits() as u64,
        ],
    );
    let source_swizzle_pack = pack_u8x4(view_base.swizzle);
    let gl_swizzle_pack = decode_swizzle(view_base.swizzle)
        .map(|swizzle| pack_i32x4(gl_swizzle_for_trace(view_base.format, swizzle)))
        .unwrap_or(u64::MAX);
    common::trace::emit_raw(
        common::trace::cat::IMAGE_VIEW,
        &[
            9,
            view_id.index as u64,
            view_base.image_id.index as u64,
            image.texture as u64,
            view_base.gpu_addr,
            view_base.format as u64,
            view_base.view_type as u64,
            source_swizzle_pack,
            gl_swizzle_pack,
            default_handle as u64,
            color2d as u64,
            color_array2d as u64,
        ],
    );
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
        let min_mipmap_filter = config.mipmap_filter();
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
    runtime: TextureCacheRuntime,
    standalone_state_tracker: Option<Box<StateTracker>>,
    images: HashMap<ImageId, Image>,
    image_views: HashMap<ImageViewId, ImageView>,
    samplers: HashMap<crate::texture_cache::types::SamplerId, Sampler>,
    slot_framebuffers: SlotVector<TextureCacheFramebuffer>,
    sentenced_framebuffers: DelayedDestructionRing<TextureCacheFramebuffer, TICKS_TO_DESTROY>,
    has_native_astc: bool,
    has_broken_texture_view_formats: bool,
    has_native_bgr: bool,
    texture_decode_worker: ThreadWorker,
}

/// OpenGL backend result of `TextureCache<P>::TryFindFramebufferImageView`.
pub struct FramebufferImageViewOpenGL {
    pub view_id: ImageViewId,
    pub display_texture: u32,
    pub width: u32,
    pub height: u32,
    pub scaled: bool,
}

fn create_null_image_views(
    has_debugging_tool_attached: bool,
) -> ([u32; 7], [u32; NUM_TEXTURE_TYPES]) {
    let mut handles = [0u32; 7];
    let mut views = [0u32; NUM_TEXTURE_TYPES];
    unsafe {
        gl::CreateTextures(gl::TEXTURE_1D_ARRAY, 1, &mut handles[0]);
        gl::CreateTextures(gl::TEXTURE_CUBE_MAP_ARRAY, 1, &mut handles[1]);
        gl::CreateTextures(gl::TEXTURE_3D, 1, &mut handles[2]);
        if handles[0] != 0 {
            gl::TextureStorage2D(handles[0], 1, gl::R8, 1, 1);
        }
        if handles[1] != 0 {
            gl::TextureStorage3D(handles[1], 1, gl::R8, 1, 1, 6);
        }
        if handles[2] != 0 {
            gl::TextureStorage3D(handles[2], 1, gl::R8, 1, 1, 1);
        }

        gl::GenTextures(4, handles[3..].as_mut_ptr());
        if handles[3] != 0 && handles[0] != 0 {
            gl::TextureView(handles[3], gl::TEXTURE_1D, handles[0], gl::R8, 0, 1, 0, 1);
        }
        if handles[4] != 0 && handles[1] != 0 {
            gl::TextureView(handles[4], gl::TEXTURE_2D, handles[1], gl::R8, 0, 1, 0, 1);
        }
        if handles[5] != 0 && handles[1] != 0 {
            gl::TextureView(
                handles[5],
                gl::TEXTURE_2D_ARRAY,
                handles[1],
                gl::R8,
                0,
                1,
                0,
                1,
            );
        }
        if handles[6] != 0 && handles[1] != 0 {
            gl::TextureView(
                handles[6],
                gl::TEXTURE_CUBE_MAP,
                handles[1],
                gl::R8,
                0,
                1,
                0,
                6,
            );
        }

        let zero_swizzle = [gl::ZERO as i32; 4];
        for &handle in &handles {
            if handle != 0 {
                gl::TextureParameteriv(handle, gl::TEXTURE_SWIZZLE_RGBA, zero_swizzle.as_ptr());
            }
        }
    }

    let mut set_view = |texture_type: TextureType, handle: u32| {
        if has_debugging_tool_attached && handle != 0 {
            let name = format!("NullImage {texture_type:?}");
            unsafe {
                gl::ObjectLabel(gl::TEXTURE, handle, name.len() as i32, name.as_ptr().cast());
            }
        }
        views[texture_type as usize] = handle;
    };

    set_view(TextureType::Color1D, handles[3]);
    set_view(TextureType::Color2D, handles[4]);
    set_view(TextureType::ColorCube, handles[6]);
    set_view(TextureType::Color3D, handles[2]);
    set_view(TextureType::ColorArray1D, handles[0]);
    set_view(TextureType::ColorArray2D, handles[5]);
    set_view(TextureType::ColorArrayCube, handles[1]);
    set_view(TextureType::Color2DRect, handles[4]);
    (handles, views)
}

fn create_format_properties() -> [HashMap<u32, FormatProperties>; 3] {
    const GL_IMAGE_COMPATIBILITY_CLASS: u32 = 0x82A8;
    const GL_IMAGE_FORMAT_COMPATIBILITY_TYPE: u32 = 0x90C7;
    const GL_IMAGE_FORMAT_COMPATIBILITY_BY_SIZE: i32 = 0x90C8;
    const GL_TEXTURE_COMPRESSED: u32 = 0x86A1;

    let targets = [gl::TEXTURE_1D_ARRAY, gl::TEXTURE_2D_ARRAY, gl::TEXTURE_3D];
    let mut format_properties: [HashMap<u32, FormatProperties>; 3] =
        std::array::from_fn(|_| HashMap::new());
    for (index, target) in targets.into_iter().enumerate() {
        for tuple in super::maxwell_to_gl::FORMAT_TABLE {
            let format = tuple.internal_format;
            let mut compatibility_class = 0;
            let mut compatibility_type = 0;
            let mut is_compressed = 0;
            unsafe {
                gl::GetInternalformativ(
                    target,
                    format,
                    GL_IMAGE_COMPATIBILITY_CLASS,
                    1,
                    &mut compatibility_class,
                );
                gl::GetInternalformativ(
                    target,
                    format,
                    GL_IMAGE_FORMAT_COMPATIBILITY_TYPE,
                    1,
                    &mut compatibility_type,
                );
                gl::GetInternalformativ(
                    target,
                    format,
                    GL_TEXTURE_COMPRESSED,
                    1,
                    &mut is_compressed,
                );
            }
            format_properties[index].insert(
                format,
                FormatProperties {
                    compatibility_class: compatibility_class as u32,
                    compatibility_by_size: compatibility_type
                        == GL_IMAGE_FORMAT_COMPATIBILITY_BY_SIZE,
                    is_compressed: is_compressed == gl::TRUE as i32,
                },
            );
        }
    }
    format_properties
}

unsafe fn trace_present_image_grid(
    present_index: u64,
    gpu_addr: u64,
    image_id: u64,
    image: &Image,
    image_base: &ImageBase,
) {
    let texture = image.handle();
    let width = image_base.info.size.width;
    let height = image_base.info.size.height;
    if texture == 0 || width == 0 || height == 0 {
        common::trace::emit_raw(
            common::trace::cat::PRESENT_TEXTURE,
            &[
                present_index,
                gpu_addr,
                image_id,
                texture as u64,
                width as u64,
                height as u64,
                image.gl_internal_format as u64,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
            ],
        );
        return;
    }

    let mut old_pack_buffer = 0;
    let mut old_pack_alignment = 0;
    let mut old_pack_row_length = 0;
    gl::GetIntegerv(gl::PIXEL_PACK_BUFFER_BINDING, &mut old_pack_buffer);
    gl::GetIntegerv(gl::PACK_ALIGNMENT, &mut old_pack_alignment);
    gl::GetIntegerv(gl::PACK_ROW_LENGTH, &mut old_pack_row_length);
    gl::BindBuffer(gl::PIXEL_PACK_BUFFER, 0);
    gl::PixelStorei(gl::PACK_ALIGNMENT, 1);
    gl::PixelStorei(gl::PACK_ROW_LENGTH, 0);

    while gl::GetError() != gl::NO_ERROR {}
    let mut gl_error = gl::NO_ERROR;
    let mut samples = 0u64;
    let mut rgb_nonzero = 0u64;
    let mut alpha_nonzero = 0u64;
    let mut checksum = 0u64;
    let mut first_rgba = 0u64;
    let mut last_rgba = 0u64;
    for y_idx in 0..8 {
        let y = ((height - 1) * y_idx / 7) as i32;
        for x_idx in 0..8 {
            let x = ((width - 1) * x_idx / 7) as i32;
            let mut px = [0u8; 4];
            gl::GetTextureSubImage(
                texture,
                0,
                x,
                y,
                0,
                1,
                1,
                1,
                gl::RGBA,
                gl::UNSIGNED_BYTE,
                px.len() as i32,
                px.as_mut_ptr().cast(),
            );
            let err = gl::GetError();
            if err != gl::NO_ERROR {
                gl_error = err;
                break;
            }
            let rgba = u32::from_le_bytes(px) as u64;
            if samples == 0 {
                first_rgba = rgba;
            }
            last_rgba = rgba;
            if px[0] != 0 || px[1] != 0 || px[2] != 0 {
                rgb_nonzero += 1;
            }
            if px[3] != 0 {
                alpha_nonzero += 1;
            }
            checksum = checksum.wrapping_mul(16777619).wrapping_add(rgba);
            samples += 1;
        }
        if gl_error != gl::NO_ERROR {
            break;
        }
    }

    gl::BindBuffer(gl::PIXEL_PACK_BUFFER, old_pack_buffer as u32);
    gl::PixelStorei(gl::PACK_ALIGNMENT, old_pack_alignment);
    gl::PixelStorei(gl::PACK_ROW_LENGTH, old_pack_row_length);

    common::trace::emit_raw(
        common::trace::cat::PRESENT_TEXTURE,
        &[
            present_index,
            gpu_addr,
            image_id,
            texture as u64,
            width as u64,
            height as u64,
            image.gl_internal_format as u64,
            gl_error as u64,
            samples,
            rgb_nonzero,
            alpha_nonzero,
            checksum,
            first_rgba,
            last_rgba,
        ],
    );
}

unsafe fn dump_present_image_ppm(
    present_index: u64,
    gpu_addr: u64,
    image_id: u64,
    image: &Image,
    image_base: &ImageBase,
) {
    let Some(output_dir) = std::env::var_os("RUZU_DUMP_PRESENT_EXTRA_PPM_DIR") else {
        return;
    };
    static EVERY: std::sync::OnceLock<Option<u64>> = std::sync::OnceLock::new();
    let every = *EVERY.get_or_init(|| {
        std::env::var("RUZU_DUMP_PRESENT_EXTRA_PPM_EVERY")
            .ok()
            .and_then(|value| value.parse::<u64>().ok())
            .filter(|&value| value != 0)
    });
    if every.is_some_and(|every| present_index % every != 0) {
        return;
    }

    let texture = image.handle();
    let width = image_base.info.size.width as usize;
    let height = image_base.info.size.height as usize;
    if texture == 0 || width == 0 || height == 0 {
        return;
    }

    let byte_count = width.saturating_mul(height).saturating_mul(4);
    if byte_count == 0 || byte_count > 256 * 1024 * 1024 {
        log::warn!(
            "[PRESENT_EXTRA_PPM] skipped present={} gpu=0x{:X} image={} size={}x{} bytes={}",
            present_index,
            gpu_addr,
            image_id,
            width,
            height,
            byte_count
        );
        return;
    }

    let mut old_pack_buffer = 0;
    let mut old_pack_alignment = 0;
    let mut old_pack_row_length = 0;
    gl::GetIntegerv(gl::PIXEL_PACK_BUFFER_BINDING, &mut old_pack_buffer);
    gl::GetIntegerv(gl::PACK_ALIGNMENT, &mut old_pack_alignment);
    gl::GetIntegerv(gl::PACK_ROW_LENGTH, &mut old_pack_row_length);
    gl::BindBuffer(gl::PIXEL_PACK_BUFFER, 0);
    gl::PixelStorei(gl::PACK_ALIGNMENT, 1);
    gl::PixelStorei(gl::PACK_ROW_LENGTH, 0);

    while gl::GetError() != gl::NO_ERROR {}
    let mut rgba = vec![0u8; byte_count];
    gl::GetTextureImage(
        texture,
        0,
        gl::RGBA,
        gl::UNSIGNED_BYTE,
        rgba.len() as i32,
        rgba.as_mut_ptr().cast(),
    );
    let gl_error = gl::GetError();

    gl::BindBuffer(gl::PIXEL_PACK_BUFFER, old_pack_buffer as u32);
    gl::PixelStorei(gl::PACK_ALIGNMENT, old_pack_alignment);
    gl::PixelStorei(gl::PACK_ROW_LENGTH, old_pack_row_length);

    let mut ppm = Vec::with_capacity(width * height * 3 + 64);
    ppm.extend_from_slice(format!("P6\n{} {}\n255\n", width, height).as_bytes());
    let mut rgb_nonzero = 0usize;
    let mut alpha_nonzero = 0usize;
    let mut checksum = 0u64;
    let mut first_rgba = 0u32;
    let mut last_rgba = 0u32;
    let mut samples = 0usize;
    for row in (0..height).rev() {
        let start = row * width * 4;
        let end = start + width * 4;
        for px in rgba[start..end].chunks_exact(4) {
            let rgba = u32::from_le_bytes([px[0], px[1], px[2], px[3]]);
            if samples == 0 {
                first_rgba = rgba;
            }
            last_rgba = rgba;
            if px[0] != 0 || px[1] != 0 || px[2] != 0 {
                rgb_nonzero += 1;
            }
            if px[3] != 0 {
                alpha_nonzero += 1;
            }
            checksum = checksum.wrapping_mul(16777619).wrapping_add(rgba as u64);
            samples += 1;
            ppm.extend_from_slice(&px[..3]);
        }
    }

    let mut path = std::path::PathBuf::from(output_dir);
    if let Err(err) = std::fs::create_dir_all(&path) {
        log::warn!(
            "[PRESENT_EXTRA_PPM] failed to create {}: {}",
            path.display(),
            err
        );
        return;
    }
    path.push(format!(
        "present_{present_index}_gpu_{gpu_addr:016X}_image_{image_id}.ppm"
    ));
    match std::fs::write(&path, ppm) {
        Ok(()) => log::info!(
            "[PRESENT_EXTRA_PPM] wrote {} present={} gpu=0x{:X} image={} gl_error=0x{:X} samples={} rgb_nonzero={} alpha_nonzero={} checksum=0x{:X} first_rgba=0x{:08X} last_rgba=0x{:08X}",
            path.display(),
            present_index,
            gpu_addr,
            image_id,
            gl_error,
            samples,
            rgb_nonzero,
            alpha_nonzero,
            checksum,
            first_rgba,
            last_rgba,
        ),
        Err(err) => log::warn!(
            "[PRESENT_EXTRA_PPM] failed to write {}: {}",
            path.display(),
            err
        ),
    }
}

fn dump_texture_upload_staging(
    image_id: ImageId,
    base_image: &ImageBase,
    staging: &[u8],
    copies: &[BufferImageCopy],
) {
    let Some(output_dir) = std::env::var_os("RUZU_DUMP_TEXTURE_UPLOAD_DIR") else {
        return;
    };
    let mut path = std::path::PathBuf::from(output_dir);
    if let Err(err) = std::fs::create_dir_all(&path) {
        log::warn!(
            "[TEXTURE_UPLOAD_DUMP] failed to create {}: {}",
            path.display(),
            err
        );
        return;
    }

    let stem = format!(
        "upload_gpu_{:016X}_image_{}_fmt_{:?}_{}x{}",
        base_image.gpu_addr,
        image_id.index,
        base_image.info.format,
        base_image.info.size.width,
        base_image.info.size.height
    );

    path.push(format!("{stem}.bin"));
    match std::fs::write(&path, staging) {
        Ok(()) => log::warn!(
            "[TEXTURE_UPLOAD_DUMP] wrote {} bytes={} gpu=0x{:X} image={}",
            path.display(),
            staging.len(),
            base_image.gpu_addr,
            image_id.index,
        ),
        Err(err) => log::warn!(
            "[TEXTURE_UPLOAD_DUMP] failed to write {}: {}",
            path.display(),
            err
        ),
    }
    path.pop();

    if base_image.info.format != PixelFormat::Bc4Unorm {
        return;
    }
    let Some(mut copy) = copies.first().cloned() else {
        return;
    };
    let width = base_image.info.size.width as usize;
    let height = base_image.info.size.height as usize;
    let output_size = width.saturating_mul(height);
    if output_size == 0 || output_size > 256 * 1024 * 1024 {
        return;
    }

    let mut decoded = vec![0u8; output_size];
    copy.image_extent.width = base_image.info.size.width;
    copy.image_extent.height = base_image.info.size.height;
    copy.image_extent.depth = 1;
    copy.image_subresource.num_layers = 1;
    crate::texture_cache::decode_bc::decompress_bcn(
        staging,
        &mut decoded,
        &mut copy,
        PixelFormat::Bc4Unorm,
    );

    let mut pgm = Vec::with_capacity(decoded.len() + 64);
    pgm.extend_from_slice(format!("P5\n{} {}\n255\n", width, height).as_bytes());
    pgm.extend_from_slice(&decoded);
    path.push(format!("{stem}.pgm"));
    match std::fs::write(&path, pgm) {
        Ok(()) => log::warn!(
            "[TEXTURE_UPLOAD_DUMP] wrote {} decoded_bc4_bytes={} gpu=0x{:X} image={}",
            path.display(),
            decoded.len(),
            base_image.gpu_addr,
            image_id.index,
        ),
        Err(err) => log::warn!(
            "[TEXTURE_UPLOAD_DUMP] failed to write {}: {}",
            path.display(),
            err
        ),
    }
}

unsafe fn sample_present_texture(texture: u32, width: u32, height: u32) -> (u64, u64, u64, u64) {
    if texture == 0 || width == 0 || height == 0 {
        return (0, 0, 0, 0);
    }

    let mut old_pack_buffer = 0;
    let mut old_pack_alignment = 0;
    let mut old_pack_row_length = 0;
    gl::GetIntegerv(gl::PIXEL_PACK_BUFFER_BINDING, &mut old_pack_buffer);
    gl::GetIntegerv(gl::PACK_ALIGNMENT, &mut old_pack_alignment);
    gl::GetIntegerv(gl::PACK_ROW_LENGTH, &mut old_pack_row_length);
    gl::BindBuffer(gl::PIXEL_PACK_BUFFER, 0);
    gl::PixelStorei(gl::PACK_ALIGNMENT, 1);
    gl::PixelStorei(gl::PACK_ROW_LENGTH, 0);

    while gl::GetError() != gl::NO_ERROR {}
    let mut checksum = 0u64;
    let mut rgb_nonzero = 0u64;
    let mut first_rgba = 0u64;
    let mut samples = 0u64;
    for y_idx in 0..8 {
        let y = ((height - 1) * y_idx / 7) as i32;
        for x_idx in 0..8 {
            let x = ((width - 1) * x_idx / 7) as i32;
            let mut px = [0u8; 4];
            gl::GetTextureSubImage(
                texture,
                0,
                x,
                y,
                0,
                1,
                1,
                1,
                gl::RGBA,
                gl::UNSIGNED_BYTE,
                px.len() as i32,
                px.as_mut_ptr().cast(),
            );
            if gl::GetError() != gl::NO_ERROR {
                gl::BindBuffer(gl::PIXEL_PACK_BUFFER, old_pack_buffer as u32);
                gl::PixelStorei(gl::PACK_ALIGNMENT, old_pack_alignment);
                gl::PixelStorei(gl::PACK_ROW_LENGTH, old_pack_row_length);
                return (checksum, rgb_nonzero, first_rgba, samples);
            }
            let rgba = u32::from_le_bytes(px) as u64;
            if samples == 0 {
                first_rgba = rgba;
            }
            if px[0] != 0 || px[1] != 0 || px[2] != 0 {
                rgb_nonzero += 1;
            }
            checksum = checksum.wrapping_mul(16777619).wrapping_add(rgba);
            samples += 1;
        }
    }

    gl::BindBuffer(gl::PIXEL_PACK_BUFFER, old_pack_buffer as u32);
    gl::PixelStorei(gl::PACK_ALIGNMENT, old_pack_alignment);
    gl::PixelStorei(gl::PACK_ROW_LENGTH, old_pack_row_length);
    (checksum, rgb_nonzero, first_rgba, samples)
}

impl TextureCache {
    /// Port of `OpenGL::TextureCache::TextureCache(Runtime&, MaxwellDeviceMemoryManager&)`.
    /// `device_memory` is the shared `Arc` from `Host1x::memory_manager()`.
    pub fn new(
        device_memory: std::sync::Arc<
            crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager,
        >,
        device: &super::gl_device::Device,
        program_manager: ProgramManagerHandle,
        state_tracker: &mut StateTracker,
    ) -> Self {
        Self::new_with_runtime(
            device_memory,
            device.has_astc(),
            device.has_broken_texture_view_formats(),
            false,
            TextureCacheRuntime::new(device, program_manager, state_tracker),
        )
    }

    pub(crate) fn new_with_caps(
        device_memory: std::sync::Arc<
            crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager,
        >,
        has_native_astc: bool,
        has_broken_texture_view_formats: bool,
        has_native_bgr: bool,
        has_debugging_tool_attached: bool,
        program_manager: ProgramManagerHandle,
        state_tracker: &mut StateTracker,
    ) -> Self {
        Self::new_with_runtime(
            device_memory,
            has_native_astc,
            has_broken_texture_view_formats,
            has_native_bgr,
            TextureCacheRuntime::new_with_caps(
                has_broken_texture_view_formats,
                2 * 1024 * 1024 * 1024,
                false,
                has_native_astc,
                has_debugging_tool_attached,
                program_manager,
                state_tracker,
            ),
        )
    }

    fn new_with_runtime(
        device_memory: std::sync::Arc<
            crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager,
        >,
        has_native_astc: bool,
        has_broken_texture_view_formats: bool,
        has_native_bgr: bool,
        runtime: TextureCacheRuntime,
    ) -> Self {
        let mut base = CommonTextureCache::new_with_caps(
            device_memory,
            has_broken_texture_view_formats,
            has_native_bgr,
        );
        base.configure_device_memory_budget(runtime.get_device_local_memory());
        Self {
            base,
            runtime,
            standalone_state_tracker: None,
            images: HashMap::new(),
            image_views: HashMap::new(),
            samplers: HashMap::new(),
            slot_framebuffers: SlotVector::new(),
            sentenced_framebuffers: DelayedDestructionRing::new(),
            has_native_astc,
            has_broken_texture_view_formats,
            has_native_bgr,
            texture_decode_worker: ThreadWorker::new_named(1, "TextureDecoder"),
        }
    }

    /// Port of `TextureCache<P>::IsRescaling`.
    pub fn is_rescaling_active(&self) -> bool {
        self.base.is_rescaling_active()
    }

    fn ensure_backend_image(&mut self, image_id: ImageId) -> bool {
        if !image_id.is_valid() || image_id == NULL_IMAGE_ID {
            return false;
        }
        let base_image = self.base.slot_images[image_id].clone();
        let image = self
            .images
            .entry(image_id)
            .or_insert_with(|| Image::from_base(&base_image, self.has_native_astc));
        image.handle() != 0
    }

    fn image_can_rescale(&mut self, image_id: ImageId) -> bool {
        if !image_id.is_valid() {
            return false;
        }
        let image = &mut self.base.slot_images[image_id];
        if !image.info.rescaleable {
            return false;
        }
        let resolution = settings::values().resolution_info.clone();
        if resolution.downscale && !image.info.downscaleable {
            return false;
        }
        if image
            .flags
            .intersects(ImageFlagBits::RESCALED | ImageFlagBits::CHECKING_RESCALABLE)
        {
            return true;
        }
        if image.flags.contains(ImageFlagBits::IS_RESCALABLE) {
            return true;
        }
        image.flags.insert(ImageFlagBits::CHECKING_RESCALABLE);
        let aliases: Vec<ImageId> = image.aliased_images.iter().map(|alias| alias.id).collect();
        for alias_id in aliases {
            if !self.image_can_rescale(alias_id) {
                self.base.slot_images[image_id]
                    .flags
                    .remove(ImageFlagBits::CHECKING_RESCALABLE);
                return false;
            }
        }
        let image = &mut self.base.slot_images[image_id];
        image.flags.remove(ImageFlagBits::CHECKING_RESCALABLE);
        image.flags.insert(ImageFlagBits::IS_RESCALABLE);
        true
    }

    fn scale_up_image(&mut self, image_id: ImageId) -> bool {
        let Some(image) = self.images.get_mut(&image_id) else {
            return false;
        };
        let rescale_read_fbos = self.runtime.rescale_read_fbos;
        let rescale_draw_fbos = self.runtime.rescale_draw_fbos;
        let has_copy = self.base.slot_images[image_id].has_scaled;
        let rescaled = image.scale_up(
            &mut self.base.slot_images[image_id],
            &rescale_read_fbos,
            &rescale_draw_fbos,
            false,
        );
        if rescaled {
            self.runtime.notify_rescale_blit_state_changed();
            if !has_copy {
                let resolution = settings::values().resolution_info.clone();
                let scale_up = (resolution.up_scale * resolution.up_scale) as u64;
                let down_shift = (resolution.down_shift + resolution.down_shift) as u64;
                let image = &self.base.slot_images[image_id];
                let image_size_bytes =
                    u64::from(image.guest_size_bytes.max(image.unswizzled_size_bytes));
                let tentative_size = (image_size_bytes * scale_up) >> down_shift;
                self.base.total_used_memory += common::alignment::align_up(tentative_size, 1024);
            }
            self.invalidate_scale(image_id);
        }
        rescaled
    }

    fn scale_down_image(&mut self, image_id: ImageId) -> bool {
        let Some(image) = self.images.get_mut(&image_id) else {
            return false;
        };
        let rescale_read_fbos = self.runtime.rescale_read_fbos;
        let rescale_draw_fbos = self.runtime.rescale_draw_fbos;
        let rescaled = image.scale_down(
            &mut self.base.slot_images[image_id],
            &rescale_read_fbos,
            &rescale_draw_fbos,
            false,
        );
        if rescaled {
            self.runtime.notify_rescale_blit_state_changed();
            self.invalidate_scale(image_id);
        }
        rescaled
    }

    fn invalidate_scale(&mut self, image_id: ImageId) {
        if self.base.slot_images[image_id].scale_tick <= self.base.frame_tick {
            self.base.slot_images[image_id].scale_tick = self.base.frame_tick + 1;
        }
        let image_view_ids = self.base.slot_images[image_id].image_view_ids.clone();
        for view_id in &image_view_ids {
            for color_buffer_id in &mut self.base.render_targets.color_buffer_ids {
                if *color_buffer_id == *view_id {
                    *color_buffer_id = ImageViewId::default();
                }
            }
            if self.base.render_targets.depth_buffer_id == *view_id {
                self.base.render_targets.depth_buffer_id = ImageViewId::default();
            }
            self.image_views.remove(view_id);
            self.remove_framebuffers_for_view(*view_id);
            if *view_id != NULL_IMAGE_VIEW_ID && view_id.is_valid() {
                let image_view = self.base.slot_image_views.take(*view_id);
                self.base.sentenced_image_view.push(image_view);
            }
        }
        self.base
            .channel_state
            .image_views
            .retain(|_, id| !image_view_ids.contains(id));
        self.base.channel_state.graphics_image_table.invalidate();
        self.base.channel_state.compute_image_table.invalidate();
        self.base
            .channel_state
            .graphics_image_view_ids
            .fill(crate::texture_cache::types::CORRUPT_ID);
        self.base
            .channel_state
            .compute_image_view_ids
            .fill(crate::texture_cache::types::CORRUPT_ID);
        self.base.slot_images[image_id].image_view_ids.clear();
        self.base.slot_images[image_id].image_view_infos.clear();
        self.base.has_deleted_images = true;
    }

    /// Port of upstream `TextureCache<P>::RescaleRenderTargets` for the
    /// current Rust render-target snapshot bridge.
    fn rescale_current_render_targets(
        &mut self,
    ) -> (bool, u32, [Option<ImageId>; NUM_RT], Option<ImageId>) {
        let mut scale_rating = 0u32;
        let mut any_rescaled = false;
        let mut can_rescale = true;
        let mut color_images = [None; NUM_RT];
        let mut depth_image = None;

        let mut check_rescale =
            |cache: &mut Self, view_id: ImageViewId, save: &mut Option<ImageId>| {
                if view_id.is_valid() && view_id != NULL_IMAGE_VIEW_ID {
                    let image_id = cache.base.slot_image_views[view_id].image_id;
                    *save = Some(image_id);
                    cache.ensure_backend_image(image_id);
                    can_rescale &= cache.image_can_rescale(image_id);
                    let image = &cache.base.slot_images[image_id];
                    any_rescaled |= image.flags.contains(ImageFlagBits::RESCALED)
                        || crate::surface::get_format_type(image.info.format)
                            != SurfaceType::ColorTexture;
                    scale_rating = scale_rating.max(if image.scale_tick <= cache.base.frame_tick {
                        image.scale_rating + 1
                    } else {
                        image.scale_rating
                    });
                } else {
                    *save = None;
                }
            };

        for index in 0..NUM_RT {
            let view_id = self.base.render_targets.color_buffer_ids[index];
            check_rescale(self, view_id, &mut color_images[index]);
        }
        check_rescale(
            self,
            self.base.render_targets.depth_buffer_id,
            &mut depth_image,
        );

        let rescaled = if can_rescale {
            let rescaled = any_rescaled || scale_rating >= 2;
            if rescaled {
                for image_id in color_images.into_iter().flatten() {
                    self.scale_up_image(image_id);
                }
                if let Some(image_id) = depth_image {
                    self.scale_up_image(image_id);
                }
                scale_rating = 2;
            }
            rescaled
        } else {
            for image_id in color_images.into_iter().flatten() {
                self.scale_down_image(image_id);
            }
            if let Some(image_id) = depth_image {
                self.scale_down_image(image_id);
            }
            scale_rating = 1;
            false
        };

        (rescaled, scale_rating, color_images, depth_image)
    }

    fn set_render_target_scale_rating(
        &mut self,
        scale_rating: u32,
        color_images: [Option<ImageId>; NUM_RT],
        depth_image: Option<ImageId>,
    ) {
        for image_id in color_images.into_iter().flatten().chain(depth_image) {
            let image = &mut self.base.slot_images[image_id];
            image.scale_rating = scale_rating;
            if image.scale_tick <= self.base.frame_tick {
                image.scale_tick = self.base.frame_tick + 1;
            }
        }
    }

    fn update_rescaled_render_target_size(&mut self, render_targets: &Maxwell3DRenderTargets) {
        let resolution = settings::values().resolution_info.clone();
        let (up_scale, down_shift) = if self.base.is_rescaling {
            (resolution.up_scale, resolution.down_shift)
        } else {
            (1, 0)
        };
        self.base.render_targets.size = Extent2D {
            width: (render_targets.surface_clip.width.saturating_mul(up_scale)) >> down_shift,
            height: (render_targets.surface_clip.height.saturating_mul(up_scale)) >> down_shift,
        };
        self.base.render_targets.is_rescaled = self.base.is_rescaling;
    }

    fn ensure_backend_image_flags(&mut self, image_id: ImageId) {
        if !image_id.is_valid() {
            return;
        }
        let image = &mut self.base.slot_images[image_id];
        Self::apply_backend_image_flags(image, self.has_native_astc);
    }

    fn apply_backend_image_flags(image: &mut ImageBase, has_native_astc: bool) {
        // Upstream sets ASYNCHRONOUS_DECODE / ACCELERATED_UPLOAD in
        // OpenGL::Image::Image with this priority.
        if can_be_decoded_async(has_native_astc, &image.info) {
            image.flags.insert(ImageFlagBits::ASYNCHRONOUS_DECODE);
        } else if can_be_accelerated(has_native_astc, &image.info) {
            image.flags.insert(ImageFlagBits::ACCELERATED_UPLOAD);
        }
        if is_converted_image(has_native_astc, image.info.format, image.info.image_type) {
            image
                .flags
                .insert(ImageFlagBits::CONVERTED | ImageFlagBits::COSTLY_LOAD);
        }
    }

    #[cfg(test)]
    fn apply_backend_image_flags_for_test(image: &mut ImageBase, has_native_astc: bool) {
        Self::apply_backend_image_flags(image, has_native_astc);
    }

    fn materialize_image(&mut self, image_id: ImageId) -> Option<&mut Image> {
        if !image_id.is_valid() {
            return None;
        }
        self.ensure_backend_image_flags(image_id);
        if !self.images.contains_key(&image_id) {
            let base_image = self.base.slot_images[image_id].clone();
            let image = Image::from_base(&base_image, self.has_native_astc);
            self.images.insert(image_id, image);
        }
        self.images.get_mut(&image_id)
    }

    fn upload_staging_to_image(
        &mut self,
        image_id: ImageId,
        base_image: &ImageBase,
        staging: &StagingBufferMap,
        copies: &[BufferImageCopy],
    ) -> bool {
        if copies.is_empty() || staging.mapped_size == 0 {
            return false;
        }
        let backend_image = self
            .images
            .entry(image_id)
            .or_insert_with(|| Image::from_base(base_image, self.has_native_astc));
        if backend_image.handle() == 0 {
            return false;
        }

        staging.flush();
        backend_image.upload_memory(staging.buffer, staging.offset, copies);
        true
    }

    /// OpenGL-backed port of `TextureCache<P>::QueueAsyncDecode`.
    fn queue_async_decode(
        &mut self,
        image_id: ImageId,
        base_image: &ImageBase,
        guest: &[u8],
    ) -> bool {
        if !base_image.flags.contains(ImageFlagBits::CONVERTED) {
            log::warn!(
                "TextureCache::queue_async_decode called for non-converted image id={}",
                image_id.index
            );
            return false;
        }
        log::info!("Queuing async texture decode");
        self.base.slot_images[image_id]
            .flags
            .insert(ImageFlagBits::IS_DECODING);

        let decode = Arc::new(AsyncDecodeContext::new(image_id));
        self.base.async_decodes.push(Arc::clone(&decode));

        let mut unswizzled = vec![0u8; base_image.unswizzled_size_bytes as usize];
        let mut copies = unswizzle_image(
            &(),
            base_image.gpu_addr,
            &base_image.info,
            guest,
            &mut unswizzled,
        );
        let out_size = map_size_bytes(base_image) as usize;
        let info = base_image.info.clone();
        self.texture_decode_worker.queue_work(move || {
            let mut decoded_data = vec![0u8; out_size];
            crate::texture_cache::util::convert_image(
                &unswizzled,
                &info,
                &mut decoded_data,
                &mut copies,
            );
            {
                let mut output = decode.output.lock().unwrap();
                output.decoded_data = decoded_data;
                output.copies = copies;
            }
            decode.complete.store(true, Ordering::Release);
        });
        true
    }

    /// OpenGL-backed port of `TextureCache<P>::TickAsyncDecode`.
    fn tick_async_decode(&mut self) {
        let mut has_uploads = false;
        let mut index = 0;
        while index < self.base.async_decodes.len() {
            let decode = Arc::clone(&self.base.async_decodes[index]);
            if !decode.complete.load(Ordering::Acquire) {
                index += 1;
                continue;
            }

            let image_id = decode.image_id;
            let mut output = decode.output.lock().unwrap();
            let decoded_data = std::mem::take(&mut output.decoded_data);
            let copies = std::mem::take(&mut output.copies);
            drop(output);

            if image_id.is_valid()
                && self
                    .base
                    .slot_images
                    .iter()
                    .any(|(candidate_id, _)| candidate_id == image_id)
            {
                let base_image = self.base.slot_images[image_id].clone();
                let mut staging = self.runtime.upload_staging_buffer(decoded_data.len());
                staging.mapped_span_mut().copy_from_slice(&decoded_data);
                if self.upload_staging_to_image(image_id, &base_image, &staging, &copies) {
                    self.base.slot_images[image_id]
                        .flags
                        .remove(ImageFlagBits::IS_DECODING);
                    has_uploads = true;
                }
            }
            self.base.async_decodes.remove(index);
        }
        if has_uploads {
            self.runtime.insert_upload_memory_barrier();
        }
    }

    /// OpenGL-backed port of `TextureCache<P>::UploadImageContents`.
    fn upload_image_contents_with_gpu_reader(
        &mut self,
        image_id: ImageId,
        base_image: &ImageBase,
        read_gpu: &mut dyn FnMut(u64, &mut [u8]) -> bool,
    ) -> bool {
        let mut guest = vec![0u8; base_image.guest_size_bytes as usize];
        let trace_upload = should_trace_texture_upload(base_image.gpu_addr);
        if !read_gpu(base_image.gpu_addr, &mut guest) {
            if trace_upload {
                log::warn!(
                    "[TEXTURE_UPLOAD] read_miss id={} gpu=0x{:X} size={}",
                    image_id.index,
                    base_image.gpu_addr,
                    guest.len(),
                );
            }
            return false;
        }

        let staging_size = map_size_bytes(base_image) as usize;
        if staging_size == 0 {
            return false;
        }

        if base_image.flags.contains(ImageFlagBits::ACCELERATED_UPLOAD) {
            let backend_image = self
                .images
                .entry(image_id)
                .or_insert_with(|| Image::from_base(base_image, self.has_native_astc));
            let handle = backend_image.handle();
            if handle == 0 {
                return false;
            }
            let swizzles = full_upload_swizzles(&base_image.info);
            let mut staging = self.runtime.upload_staging_buffer(guest.len());
            staging.mapped_span_mut().copy_from_slice(&guest);
            staging.flush();
            self.runtime
                .accelerate_image_upload(handle, &base_image.info, &staging, &swizzles);
            if trace_upload {
                log::warn!(
                    "[TEXTURE_UPLOAD] accelerated id={} gpu=0x{:X} cpu=0x{:X} guest={} swizzles={}",
                    image_id.index,
                    base_image.gpu_addr,
                    base_image.cpu_addr,
                    guest.len(),
                    swizzles.len(),
                );
            }
            return true;
        }

        let mut staging = self.runtime.upload_staging_buffer(staging_size);
        // Upstream `UploadImageContents`: converted images (ASTC without
        // native support, BC4/BC5 3D) are unswizzled into a scratch buffer
        // first, then `ConvertImage` decodes into the staging map and
        // rewrites the copies to the converted layout (RGBA8 offsets/sizes,
        // tight row_length/image_height). Uploading the raw unswizzled bytes
        // with unconverted copies leaves the GL_RGBA8 texture empty
        // (GL_INVALID_OPERATION: out-of-bounds PBO access -- MK8D splash text
        // and demo assets).
        let copies = if base_image.flags.contains(ImageFlagBits::CONVERTED) {
            let mut unswizzled = vec![0u8; base_image.unswizzled_size_bytes as usize];
            let mut copies = unswizzle_image(
                &(),
                base_image.gpu_addr,
                &base_image.info,
                &guest,
                &mut unswizzled,
            );
            crate::texture_cache::util::convert_image(
                &unswizzled,
                &base_image.info,
                staging.mapped_span_mut(),
                &mut copies,
            );
            copies
        } else {
            unswizzle_image(
                &(),
                base_image.gpu_addr,
                &base_image.info,
                &guest,
                staging.mapped_span_mut(),
            )
        };
        if copies.is_empty() {
            return false;
        }
        if should_dump_texture_upload(base_image.gpu_addr) {
            dump_texture_upload_staging(image_id, base_image, staging.mapped_span(), &copies);
        }
        if trace_upload {
            let guest_nonzero = guest.iter().filter(|&&b| b != 0).take(1).count() != 0;
            let staging_nonzero = staging
                .mapped_span()
                .iter()
                .filter(|&&b| b != 0)
                .take(1)
                .count()
                != 0;
            let guest_checksum = guest.iter().take(4096).fold(0u64, |acc, &b| {
                acc.wrapping_mul(16777619).wrapping_add(b as u64)
            });
            let staging_checksum = staging
                .mapped_span()
                .iter()
                .take(4096)
                .fold(0u64, |acc, &b| {
                    acc.wrapping_mul(16777619).wrapping_add(b as u64)
                });
            log::warn!(
                "[TEXTURE_UPLOAD] staging id={} gpu=0x{:X} cpu=0x{:X} guest_nonzero={} staging_nonzero={} guest_crc=0x{:X} staging_crc=0x{:X}",
                image_id.index,
                base_image.gpu_addr,
                base_image.cpu_addr,
                guest_nonzero,
                staging_nonzero,
                guest_checksum,
                staging_checksum,
            );
        }

        let uploaded = self.upload_staging_to_image(image_id, base_image, &staging, &copies);
        if trace_upload {
            log::warn!(
                "[TEXTURE_UPLOAD] uploaded id={} gpu=0x{:X} cpu=0x{:X} guest={} staging={} copies={}",
                image_id.index,
                base_image.gpu_addr,
                base_image.cpu_addr,
                guest.len(),
                staging.mapped_size,
                copies.len(),
            );
        }
        uploaded
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
    pub fn prepare_image_without_gpu_reader(
        &mut self,
        image_id: ImageId,
        is_modification: bool,
        invalidate: bool,
    ) {
        if !image_id.is_valid() {
            return;
        }
        if self.prepare_image_with_bound_gpu_reader(image_id, is_modification, invalidate) {
            return;
        }
        self.ensure_backend_image_flags(image_id);
        if invalidate {
            let image = &mut self.base.slot_images[image_id];
            image
                .flags
                .remove(ImageFlagBits::CPU_MODIFIED | ImageFlagBits::GPU_MODIFIED);
            if !self.base.slot_images[image_id]
                .flags
                .contains(ImageFlagBits::TRACKED)
            {
                self.base.track_image(image_id);
            }
        } else {
            // Upstream `PrepareImage` always runs `SynchronizeAliases` after
            // `RefreshContents`. The reader-less Rust path cannot refresh
            // guest CPU writes; only run the backend-local alias copy when
            // `RefreshContents` would be a no-op, otherwise we could propagate
            // stale backend texture contents.
            if self.can_synchronize_aliases_without_refresh(image_id) {
                self.synchronize_aliases(image_id);
            }
        }
        if is_modification {
            self.base.mark_modification_by_id(image_id);
        }
        self.base.touch_image(image_id);
    }

    fn prepare_image_with_bound_gpu_reader(
        &mut self,
        image_id: ImageId,
        is_modification: bool,
        invalidate: bool,
    ) -> bool {
        let Some(gpu_memory) = self.base.channel_gpu_memory.as_ref().cloned() else {
            return false;
        };
        self.prepare_image_with_gpu_reader(
            image_id,
            is_modification,
            invalidate,
            &mut |addr, out| gpu_memory.lock().read_block(addr, out),
        );
        true
    }

    fn refresh_contents_with_bound_gpu_reader(&mut self, image_id: ImageId) -> bool {
        let Some(gpu_memory) = self.base.channel_gpu_memory.as_ref().cloned() else {
            return false;
        };
        self.refresh_contents_with_gpu_reader(image_id, &mut |addr, out| {
            gpu_memory.lock().read_block(addr, out)
        });
        true
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
        self.ensure_backend_image_flags(image_id);
        let image = &self.base.slot_images[image_id];
        let trace_upload = should_trace_texture_upload(image.gpu_addr);
        if trace_upload {
            log::warn!(
                "[TEXTURE_UPLOAD] prepare id={} gpu=0x{:X} cpu=0x{:X} flags={:?} guest_size={} invalidate={} modification={}",
                image_id.index,
                image.gpu_addr,
                image.cpu_addr,
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
            if !self.base.slot_images[image_id]
                .flags
                .contains(ImageFlagBits::TRACKED)
            {
                self.base.track_image(image_id);
            }
        } else {
            self.refresh_contents_with_gpu_reader(image_id, read_gpu);
            self.synchronize_aliases(image_id);
        }
        if is_modification {
            self.base.mark_modification_by_id(image_id);
        }
        self.base.touch_image(image_id);
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
        self.ensure_backend_image_flags(image_id);
        let base_image = self.base.slot_images[image_id].clone();
        if !base_image.flags.contains(ImageFlagBits::CPU_MODIFIED) {
            return;
        }
        if base_image.guest_size_bytes == 0 {
            self.base.slot_images[image_id]
                .flags
                .remove(ImageFlagBits::CPU_MODIFIED);
            if !self.base.slot_images[image_id]
                .flags
                .contains(ImageFlagBits::TRACKED)
            {
                self.base.track_image(image_id);
            }
            return;
        }

        self.base.slot_images[image_id]
            .flags
            .remove(ImageFlagBits::CPU_MODIFIED);
        debug_assert!(
            !self.base.slot_images[image_id]
                .flags
                .contains(ImageFlagBits::TRACKED),
            "TextureCache::refresh_contents: CPU-modified image should have been untracked"
        );
        if !self.base.slot_images[image_id]
            .flags
            .contains(ImageFlagBits::TRACKED)
        {
            self.base.track_image(image_id);
        }

        if base_image.info.num_samples > 1 && !self.runtime.can_upload_msaa() {
            log::warn!("MSAA image uploads are not implemented");
            self.runtime.transition_image_layout(&base_image);
            return;
        }

        if base_image
            .flags
            .contains(ImageFlagBits::ASYNCHRONOUS_DECODE)
        {
            let mut guest = vec![0u8; base_image.guest_size_bytes as usize];
            let trace_upload = should_trace_texture_upload(base_image.gpu_addr);
            if !read_gpu(base_image.gpu_addr, &mut guest) {
                if trace_upload {
                    log::warn!(
                        "[TEXTURE_UPLOAD] read_miss id={} gpu=0x{:X} size={}",
                        image_id.index,
                        base_image.gpu_addr,
                        guest.len(),
                    );
                }
                return;
            }
            if self.queue_async_decode(image_id, &base_image, &guest) {
                return;
            }
        }

        if self.upload_image_contents_with_gpu_reader(image_id, &base_image, read_gpu) {
            self.runtime.insert_upload_memory_barrier();
            return;
        }
    }

    fn can_synchronize_aliases_without_refresh(&self, image_id: ImageId) -> bool {
        image_id.is_valid()
            && !self.base.slot_images[image_id]
                .flags
                .contains(ImageFlagBits::CPU_MODIFIED)
    }

    /// Port of upstream `TextureCache<P>::SynchronizeAliases`.
    ///
    /// This Rust OpenGL specialization implements the direct-copy path used
    /// when aliased images share the same surface type. The remaining upstream
    /// rescale/reinterpret/convert branches stay documented in `DIFF.md`
    /// because ruzu has not ported the corresponding runtime passes yet.
    fn synchronize_aliases(&mut self, image_id: ImageId) {
        if !image_id.is_valid() {
            return;
        }

        let image_snapshot = self.base.slot_images[image_id].clone();
        let mut aliases: Vec<_> = image_snapshot
            .aliased_images
            .iter()
            .filter_map(|alias| {
                let alias_image = &self.base.slot_images[alias.id];
                (image_snapshot.modification_tick < alias_image.modification_tick)
                    .then(|| alias.clone())
            })
            .collect();
        if aliases.is_empty() {
            return;
        }

        let mut most_recent_tick = image_snapshot.modification_tick;
        let mut any_modified = image_snapshot.flags.contains(ImageFlagBits::GPU_MODIFIED);
        for alias in &aliases {
            let alias_image = &self.base.slot_images[alias.id];
            most_recent_tick = most_recent_tick.max(alias_image.modification_tick);
            any_modified |= alias_image.flags.contains(ImageFlagBits::GPU_MODIFIED);
        }

        {
            let image = &mut self.base.slot_images[image_id];
            image.modification_tick = most_recent_tick;
            if any_modified {
                image.flags.insert(ImageFlagBits::GPU_MODIFIED);
            }
        }

        aliases.sort_by_key(|alias| self.base.slot_images[alias.id].modification_tick);
        for alias in aliases {
            self.copy_image(image_id, alias.id, &alias.copies);
        }
    }

    fn base_image_exists(&self, image_id: ImageId) -> bool {
        image_id.is_valid() && self.base.slot_images.iter().any(|(id, _)| id == image_id)
    }

    fn finish_pending_join_copies(&mut self) {
        self.finish_pending_join_copies_impl(None);
    }

    fn finish_pending_join_copies_with_gpu_reader(
        &mut self,
        read_gpu: &mut dyn FnMut(u64, &mut [u8]) -> bool,
    ) {
        self.finish_pending_join_copies_impl(Some(read_gpu));
    }

    fn finish_pending_join_copies_impl(
        &mut self,
        mut read_gpu: Option<&mut dyn FnMut(u64, &mut [u8]) -> bool>,
    ) {
        let pending = std::mem::take(&mut self.base.pending_join_copies);
        let mut deferred = Vec::new();
        let mut blocked_on_reader = false;
        let trace_join = std::env::var_os("RUZU_TRACE_JOIN_IMAGES").is_some();
        for join in pending {
            if blocked_on_reader {
                deferred.push(join);
                continue;
            }
            if !self.base_image_exists(join.new_image_id) {
                continue;
            }
            if trace_join {
                let image = &self.base.slot_images[join.new_image_id];
                log::warn!(
                    "[JOIN_IMAGES] drain new={} gpu=0x{:X} cpu=0x{:X} fmt={:?} {}x{} copies={}",
                    join.new_image_id.index,
                    image.gpu_addr,
                    image.cpu_addr,
                    image.info.format,
                    image.info.size.width,
                    image.info.size.height,
                    join.copies.len()
                );
            }
            match read_gpu.as_deref_mut() {
                Some(reader) => {
                    self.refresh_contents_with_gpu_reader(join.new_image_id, reader);
                }
                None if self.base.slot_images[join.new_image_id]
                    .flags
                    .contains(ImageFlagBits::CPU_MODIFIED) =>
                {
                    if !self.refresh_contents_with_bound_gpu_reader(join.new_image_id) {
                        if trace_join {
                            log::warn!(
                                "[JOIN_IMAGES] defer new={} because RefreshContents needs a GPU reader",
                                join.new_image_id.index
                            );
                        }
                        deferred.push(join);
                        blocked_on_reader = true;
                        continue;
                    }
                }
                None => {}
            }
            let can_rescale =
                self.prepare_pending_join_rescale(join.new_image_id, &join.copies, trace_join);
            let resolution = settings::values().resolution_info.clone();
            let (copy_up_scale, copy_down_shift) = if can_rescale {
                (resolution.up_scale, resolution.down_shift)
            } else {
                (1, 0)
            };
            for copy_object in join.copies {
                if !self.base_image_exists(copy_object.id) {
                    continue;
                }
                if trace_join {
                    let overlap = &self.base.slot_images[copy_object.id];
                    log::warn!(
                        "[JOIN_IMAGES] copy new={} overlap={} alias={} gpu_modified_at_join={} gpu=0x{:X} cpu=0x{:X} fmt={:?} {}x{} flags=0x{:X}",
                        join.new_image_id.index,
                        copy_object.id.index,
                        copy_object.is_alias,
                        copy_object.gpu_modified_at_join,
                        overlap.gpu_addr,
                        overlap.cpu_addr,
                        overlap.info.format,
                        overlap.info.size.width,
                        overlap.info.size.height,
                        overlap.flags.bits(),
                    );
                }

                if copy_object.is_alias {
                    if !self.base.slot_images[copy_object.id].is_safe_download() {
                        continue;
                    }
                    let Some(&alias_index) = join.alias_indices.get(&copy_object.id) else {
                        continue;
                    };
                    let Some(alias) = self.base.slot_images[join.new_image_id]
                        .aliased_images
                        .get(alias_index)
                        .cloned()
                    else {
                        continue;
                    };
                    self.copy_image(join.new_image_id, alias.id, &alias.copies);
                    let tick = self.base.slot_images[copy_object.id].modification_tick;
                    self.base.slot_images[join.new_image_id].modification_tick = tick;
                    continue;
                }

                let overlap_snapshot = self.base.slot_images[copy_object.id].clone();
                if copy_object.gpu_modified_at_join
                    || overlap_snapshot.flags.contains(ImageFlagBits::GPU_MODIFIED)
                {
                    self.base.slot_images[join.new_image_id]
                        .flags
                        .insert(ImageFlagBits::GPU_MODIFIED);
                    if let Some(base) = self.base.slot_images[join.new_image_id]
                        .try_find_base(overlap_snapshot.gpu_addr)
                    {
                        let new_info = self.base.slot_images[join.new_image_id].info.clone();
                        let copies = make_shrink_image_copies(
                            &new_info,
                            &overlap_snapshot.info,
                            base,
                            copy_up_scale,
                            copy_down_shift,
                        );
                        if trace_join {
                            log::warn!(
                                "[JOIN_IMAGES] gl_copy new={} overlap={} copies={} base_level={} base_layer={} can_rescale={}",
                                join.new_image_id.index,
                                copy_object.id.index,
                                copies.len(),
                                base.level,
                                base.layer,
                                can_rescale
                            );
                        }
                        self.copy_image(join.new_image_id, copy_object.id, &copies);
                        self.base.slot_images[join.new_image_id].modification_tick =
                            overlap_snapshot.modification_tick;
                    }
                }

                self.delete_join_overlap_image(copy_object.id);
            }
            if self.base_image_exists(join.new_image_id)
                && !self.base.slot_images[join.new_image_id]
                    .flags
                    .contains(ImageFlagBits::REGISTERED)
            {
                self.base.register_image(join.new_image_id);
            }
        }
        if !deferred.is_empty() {
            let mut newly_queued = std::mem::take(&mut self.base.pending_join_copies);
            deferred.append(&mut newly_queued);
            self.base.pending_join_copies = deferred;
        }
    }

    fn prepare_pending_join_rescale(
        &mut self,
        new_image_id: ImageId,
        copies: &[JoinCopy],
        trace_join: bool,
    ) -> bool {
        if !self.base_image_exists(new_image_id) {
            return false;
        }

        let mut can_rescale = self.base.slot_images[new_image_id].info.rescaleable;
        let mut any_rescaled = false;
        for copy in copies {
            if !can_rescale {
                break;
            }
            if !self.base_image_exists(copy.id) {
                can_rescale = false;
                break;
            }
            self.ensure_backend_image_flags(copy.id);
            can_rescale &= self.image_can_rescale(copy.id);
            any_rescaled |= self.base.slot_images[copy.id]
                .flags
                .contains(ImageFlagBits::RESCALED);
        }
        can_rescale &= any_rescaled;

        for copy in copies {
            if !self.base_image_exists(copy.id) {
                continue;
            }
            self.ensure_backend_image_flags(copy.id);
            if can_rescale {
                self.scale_up_image(copy.id);
            } else {
                self.scale_down_image(copy.id);
            }
        }

        self.ensure_backend_image_flags(new_image_id);
        if can_rescale {
            self.scale_up_image(new_image_id);
        } else {
            self.scale_down_image(new_image_id);
        }

        if trace_join {
            let image = &self.base.slot_images[new_image_id];
            log::warn!(
                "[JOIN_IMAGES] rescale new={} gpu=0x{:X} can_rescale={} any_rescaled={} flags=0x{:X}",
                new_image_id.index,
                image.gpu_addr,
                can_rescale,
                any_rescaled,
                image.flags.bits()
            );
        }
        can_rescale
    }

    fn delete_join_overlap_image(&mut self, image_id: ImageId) {
        if !self.base_image_exists(image_id) {
            return;
        }
        if self.base.slot_images[image_id]
            .flags
            .contains(ImageFlagBits::TRACKED)
        {
            self.base.untrack_image(image_id);
        }
        if self.base.slot_images[image_id]
            .flags
            .contains(ImageFlagBits::REGISTERED)
        {
            self.base.unregister_image(image_id);
        }
        let view_ids = self.base.slot_images[image_id].image_view_ids.clone();
        self.images.remove(&image_id);
        for view_id in view_ids {
            self.image_views.remove(&view_id);
            self.remove_framebuffers_for_view(view_id);
        }
        self.base.delete_image(image_id, false);
    }

    /// Port of upstream `TextureCache<P>::CopyImage` for OpenGL.
    fn copy_image(&mut self, dst_id: ImageId, src_id: ImageId, copies: &[ImageCopy]) {
        if !dst_id.is_valid() || !src_id.is_valid() || copies.is_empty() {
            return;
        }

        let dst_base = self.base.slot_images[dst_id].clone();
        let src_base = self.base.slot_images[src_id].clone();
        let trace_copy = should_trace_texture_cache_address(dst_base.gpu_addr)
            || should_trace_texture_cache_address(src_base.gpu_addr);
        let src_rescaled = src_base.flags.contains(ImageFlagBits::RESCALED);
        let dst_rescaled = dst_base.flags.contains(ImageFlagBits::RESCALED);
        if src_rescaled != dst_rescaled {
            log::warn!(
                "TextureCache::copy_image_direct: mismatched rescaled alias copy src={} dst={}",
                src_rescaled,
                dst_rescaled
            );
            return;
        }
        if src_base.info.num_samples != dst_base.info.num_samples {
            if trace_copy {
                log::warn!(
                    "[COPY_IMAGE] path=msaa src={} gpu=0x{:X} samples={} dst={} gpu=0x{:X} samples={} copies={}",
                    src_id.index,
                    src_base.gpu_addr,
                    src_base.info.num_samples,
                    dst_id.index,
                    dst_base.gpu_addr,
                    dst_base.info.num_samples,
                    copies.len()
                );
            }
            self.copy_image_msaa(dst_id, src_id, copies);
            return;
        }
        let copies = if src_rescaled {
            let both_2d = src_base.info.image_type == ImageType::E2D
                && dst_base.info.image_type == ImageType::E2D;
            scale_up_image_copies(copies, both_2d)
        } else {
            copies.to_vec()
        };

        let dst_format_type = crate::surface::get_format_type(dst_base.info.format);
        let src_format_type = crate::surface::get_format_type(src_base.info.format);

        self.images
            .entry(dst_id)
            .or_insert_with(|| Image::from_base(&dst_base, self.has_native_astc));
        self.images
            .entry(src_id)
            .or_insert_with(|| Image::from_base(&src_base, self.has_native_astc));

        let dst_handle = self.images.get(&dst_id).map(Image::handle).unwrap_or(0);
        let src_handle = self.images.get(&src_id).map(Image::handle).unwrap_or(0);
        let dst_gl_internal_format = self
            .images
            .get(&dst_id)
            .map(|image| image.gl_internal_format)
            .unwrap_or(gl::NONE);
        if dst_handle == 0 || src_handle == 0 {
            return;
        }

        let dst_target = image_target(&dst_base.info);
        let src_target = image_target(&src_base.info);
        if src_format_type == dst_format_type
            && self.runtime.can_image_be_copied(&dst_base, &src_base)
        {
            if trace_copy {
                log::warn!(
                    "[COPY_IMAGE] path=direct src={} gpu=0x{:X} fmt={:?} dst={} gpu=0x{:X} fmt={:?} copies={}",
                    src_id.index,
                    src_base.gpu_addr,
                    src_base.info.format,
                    dst_id.index,
                    dst_base.gpu_addr,
                    dst_base.info.format,
                    copies.len()
                );
            }
            self.copy_image_direct_raw(
                dst_handle,
                dst_target,
                dst_gl_internal_format,
                dst_base.info.format,
                src_handle,
                src_target,
                src_base.info.format,
                &copies,
            );
            return;
        }

        if src_format_type == dst_format_type {
            if trace_copy {
                log::warn!(
                    "[COPY_IMAGE] path=emulate src={} gpu=0x{:X} fmt={:?} dst={} gpu=0x{:X} fmt={:?} copies={}",
                    src_id.index,
                    src_base.gpu_addr,
                    src_base.info.format,
                    dst_id.index,
                    dst_base.gpu_addr,
                    dst_base.info.format,
                    copies.len()
                );
            }
            self.emulate_copy_image(dst_id, src_id, &copies);
            return;
        }

        if dst_base.info.image_type != ImageType::E2D || src_base.info.image_type != ImageType::E2D
        {
            log::warn!(
                "TextureCache::copy_image: reinterpret path only implemented for 2D images dst={:?} src={:?}",
                dst_base.info.image_type,
                src_base.info.image_type
            );
            return;
        }
        if trace_copy {
            log::warn!(
                "[COPY_IMAGE] path=reinterpret src={} gpu=0x{:X} fmt={:?}/{:?} dst={} gpu=0x{:X} fmt={:?}/{:?} copies={}",
                src_id.index,
                src_base.gpu_addr,
                src_base.info.format,
                src_format_type,
                dst_id.index,
                dst_base.gpu_addr,
                dst_base.info.format,
                dst_format_type,
                copies.len()
            );
        }
        self.reinterpret_image(dst_id, src_id, &copies);
    }

    fn copy_image_msaa(&mut self, dst_id: ImageId, src_id: ImageId, copies: &[ImageCopy]) {
        let dst_base = self.base.slot_images[dst_id].clone();
        let src_base = self.base.slot_images[src_id].clone();

        self.images
            .entry(dst_id)
            .or_insert_with(|| Image::from_base(&dst_base, self.has_native_astc));
        self.images
            .entry(src_id)
            .or_insert_with(|| Image::from_base(&src_base, self.has_native_astc));

        let Some(dst_image) = self.images.get(&dst_id) else {
            return;
        };
        let Some(src_image) = self.images.get(&src_id) else {
            return;
        };
        let dst_handle = dst_image.handle();
        let src_handle = src_image.handle();
        if dst_handle == 0 || src_handle == 0 {
            return;
        }

        let src_msaa = src_base.info.num_samples > 1;
        let dst_msaa = dst_base.info.num_samples > 1;
        if src_msaa == dst_msaa {
            log::warn!(
                "TextureCache::copy_image_msaa: unsupported sample transition src_samples={} dst_samples={}",
                src_base.info.num_samples,
                dst_base.info.num_samples
            );
            return;
        }
        self.runtime
            .util_shaders
            .copy_msaa(dst_handle, src_handle, copies, src_msaa && !dst_msaa);
    }

    fn copy_image_direct_raw(
        &self,
        dst_handle: u32,
        dst_target: u32,
        dst_gl_internal_format: u32,
        dst_format: PixelFormat,
        src_handle: u32,
        src_target: u32,
        src_format: PixelFormat,
        copies: &[ImageCopy],
    ) {
        unsafe {
            let trace_invalid = should_trace_invalid_copy_image();
            let compressed_copy_fallback =
                compressed_copy_fallback_format(dst_format, src_format).is_some();
            let raw_compressed_copy_fallback =
                raw_compressed_copy_fallback_format(dst_format, src_format);
            for copy in copies {
                let src_origin =
                    make_copy_origin(copy.src_offset, copy.src_subresource, src_target);
                let dst_origin =
                    make_copy_origin(copy.dst_offset, copy.dst_subresource, dst_target);
                let region = make_copy_region(copy.extent, copy.dst_subresource, dst_target);
                if region.width == 0 || region.height == 0 || region.depth == 0 {
                    continue;
                }
                if trace_invalid
                    || compressed_copy_fallback
                    || raw_compressed_copy_fallback.is_some()
                {
                    while gl::GetError() != gl::NO_ERROR {}
                }
                gl::CopyImageSubData(
                    src_handle,
                    src_target,
                    src_origin.level,
                    src_origin.x,
                    src_origin.y,
                    src_origin.z,
                    dst_handle,
                    dst_target,
                    dst_origin.level,
                    dst_origin.x,
                    dst_origin.y,
                    dst_origin.z,
                    region.width,
                    region.height,
                    region.depth,
                );
                if trace_invalid
                    || compressed_copy_fallback
                    || raw_compressed_copy_fallback.is_some()
                {
                    let err = gl::GetError();
                    if err != gl::NO_ERROR {
                        let src_size = texture_level_extent(src_handle, src_origin.level);
                        let dst_size = texture_level_extent(dst_handle, dst_origin.level);
                        let mut fallback_name = "none";
                        let mut fallback_ok = compressed_copy_fallback
                            && self.copy_compressed_block_fallback(
                                dst_handle,
                                dst_gl_internal_format,
                                dst_format,
                                src_handle,
                                src_origin,
                                dst_origin,
                                region,
                                src_size,
                                dst_size,
                            );
                        if fallback_ok {
                            fallback_name = "compressed_block";
                        }
                        if !fallback_ok {
                            if let Some(format) = raw_compressed_copy_fallback {
                                fallback_ok = self.copy_raw_compressed_block_fallback(
                                    dst_handle,
                                    dst_gl_internal_format,
                                    src_handle,
                                    format,
                                    src_origin,
                                    dst_origin,
                                    region,
                                    src_size,
                                    dst_size,
                                );
                                if fallback_ok {
                                    fallback_name = "raw_compressed_block";
                                }
                            }
                        }
                        if trace_invalid {
                            log::warn!(
                                "[COPY_IMAGE_INVALID] err=0x{:X} fallback={} src={} fmt={:?} target=0x{:X} level={} origin=({}, {}, {}) size={:?} dst={} fmt={:?} target=0x{:X} level={} origin=({}, {}, {}) size={:?} region={}x{}x{} copy={:?}",
                                err,
                                fallback_name,
                                src_handle,
                                src_format,
                                src_target,
                                src_origin.level,
                                src_origin.x,
                                src_origin.y,
                                src_origin.z,
                                src_size,
                                dst_handle,
                                dst_format,
                                dst_target,
                                dst_origin.level,
                                dst_origin.x,
                                dst_origin.y,
                                dst_origin.z,
                                dst_size,
                                region.width,
                                region.height,
                                region.depth,
                                copy,
                            );
                        } else if !fallback_ok {
                            log::warn!(
                                "TextureCache::copy_image_direct_raw: glCopyImageSubData failed err=0x{:X} without fallback dst_fmt={:?} src_fmt={:?}",
                                err,
                                dst_format,
                                src_format
                            );
                        }
                    }
                }
            }
            gl::MemoryBarrier(gl::TEXTURE_FETCH_BARRIER_BIT | gl::SHADER_IMAGE_ACCESS_BARRIER_BIT);
        }
    }

    fn copy_compressed_block_fallback(
        &self,
        dst_handle: u32,
        dst_gl_internal_format: u32,
        dst_format: PixelFormat,
        src_handle: u32,
        src_origin: CopyOrigin,
        dst_origin: CopyOrigin,
        region: CopyRegion,
        src_size: (i32, i32, i32),
        dst_size: (i32, i32, i32),
    ) -> bool {
        let Some((block_width, block_height, bytes_per_block)) =
            compressed_copy_fallback_format(dst_format, dst_format)
        else {
            return false;
        };
        let Some(rect) = compressed_block_copy_rect(
            src_size,
            dst_size,
            src_origin,
            dst_origin,
            region,
            block_width,
            block_height,
            bytes_per_block,
        ) else {
            return false;
        };
        let mut buffer = vec![0u8; rect.buffer_size as usize];
        unsafe {
            while gl::GetError() != gl::NO_ERROR {}
            gl::GetCompressedTextureSubImage(
                src_handle,
                src_origin.level,
                src_origin.x,
                src_origin.y,
                src_origin.z,
                rect.read_width,
                rect.read_height,
                region.depth,
                rect.buffer_size,
                buffer.as_mut_ptr() as *mut _,
            );
            let read_error = gl::GetError();
            if read_error != gl::NO_ERROR {
                log::warn!(
                    "TextureCache::copy_compressed_block_fallback: read failed err=0x{:X}",
                    read_error
                );
                return false;
            }
            gl::CompressedTextureSubImage3D(
                dst_handle,
                dst_origin.level,
                dst_origin.x,
                dst_origin.y,
                dst_origin.z,
                rect.write_width,
                rect.write_height,
                region.depth,
                dst_gl_internal_format,
                rect.buffer_size,
                buffer.as_ptr() as *const _,
            );
            let write_error = gl::GetError();
            if write_error != gl::NO_ERROR {
                log::warn!(
                    "TextureCache::copy_compressed_block_fallback: write failed err=0x{:X}",
                    write_error
                );
                return false;
            }
        }
        true
    }

    fn copy_raw_compressed_block_fallback(
        &self,
        dst_handle: u32,
        dst_gl_internal_format: u32,
        src_handle: u32,
        format: RawCompressedCopyFormat,
        src_origin: CopyOrigin,
        dst_origin: CopyOrigin,
        region: CopyRegion,
        src_size: (i32, i32, i32),
        dst_size: (i32, i32, i32),
    ) -> bool {
        if region.width <= 0
            || region.height <= 0
            || region.depth <= 0
            || src_origin.x < 0
            || src_origin.y < 0
            || dst_origin.x < 0
            || dst_origin.y < 0
        {
            return false;
        }
        let compressed_size = match format.direction {
            RawCompressedCopyDirection::RawToCompressed => dst_size,
            RawCompressedCopyDirection::CompressedToRaw => src_size,
        };
        let compressed_origin = match format.direction {
            RawCompressedCopyDirection::RawToCompressed => dst_origin,
            RawCompressedCopyDirection::CompressedToRaw => src_origin,
        };
        let raw_size = match format.direction {
            RawCompressedCopyDirection::RawToCompressed => src_size,
            RawCompressedCopyDirection::CompressedToRaw => dst_size,
        };
        let raw_origin = match format.direction {
            RawCompressedCopyDirection::RawToCompressed => src_origin,
            RawCompressedCopyDirection::CompressedToRaw => dst_origin,
        };
        if raw_origin.x + region.width > raw_size.0
            || raw_origin.y + region.height > raw_size.1
            || raw_origin.z + region.depth > raw_size.2
            || compressed_origin.z + region.depth > compressed_size.2
        {
            return false;
        }

        let block_width = format.block_width as i32;
        let block_height = format.block_height as i32;
        let compressed_remaining_width = compressed_size.0 - compressed_origin.x;
        let compressed_remaining_height = compressed_size.1 - compressed_origin.y;
        if compressed_remaining_width <= 0 || compressed_remaining_height <= 0 {
            return false;
        }
        let compressed_width = (region.width * block_width).min(compressed_remaining_width);
        let compressed_height = (region.height * block_height).min(compressed_remaining_height);
        if compressed_width <= 0 || compressed_height <= 0 {
            return false;
        }

        let buffer_size = region
            .width
            .checked_mul(region.height)
            .and_then(|size| size.checked_mul(region.depth))
            .and_then(|size| size.checked_mul(format.bytes_per_block as i32));
        let Some(buffer_size) = buffer_size else {
            return false;
        };
        let raw_tuple = super::maxwell_to_gl::get_format_tuple(format.raw_format as usize);
        let mut buffer = vec![0u8; buffer_size as usize];
        unsafe {
            while gl::GetError() != gl::NO_ERROR {}
            match format.direction {
                RawCompressedCopyDirection::RawToCompressed => {
                    gl::GetTextureSubImage(
                        src_handle,
                        src_origin.level,
                        src_origin.x,
                        src_origin.y,
                        src_origin.z,
                        region.width,
                        region.height,
                        region.depth,
                        raw_tuple.format,
                        raw_tuple.gl_type,
                        buffer_size,
                        buffer.as_mut_ptr() as *mut _,
                    );
                }
                RawCompressedCopyDirection::CompressedToRaw => {
                    gl::GetCompressedTextureSubImage(
                        src_handle,
                        src_origin.level,
                        src_origin.x,
                        src_origin.y,
                        src_origin.z,
                        compressed_width,
                        compressed_height,
                        region.depth,
                        buffer_size,
                        buffer.as_mut_ptr() as *mut _,
                    );
                }
            }
            let read_error = gl::GetError();
            if read_error != gl::NO_ERROR {
                log::warn!(
                    "TextureCache::copy_raw_compressed_block_fallback: read failed err=0x{:X} direction={:?} compressed_fmt={:?} raw_fmt={:?}",
                    read_error,
                    format.direction,
                    format.compressed_format,
                    format.raw_format
                );
                return false;
            }

            match format.direction {
                RawCompressedCopyDirection::RawToCompressed => {
                    gl::CompressedTextureSubImage3D(
                        dst_handle,
                        dst_origin.level,
                        dst_origin.x,
                        dst_origin.y,
                        dst_origin.z,
                        compressed_width,
                        compressed_height,
                        region.depth,
                        dst_gl_internal_format,
                        buffer_size,
                        buffer.as_ptr() as *const _,
                    );
                }
                RawCompressedCopyDirection::CompressedToRaw => {
                    gl::TextureSubImage3D(
                        dst_handle,
                        dst_origin.level,
                        dst_origin.x,
                        dst_origin.y,
                        dst_origin.z,
                        region.width,
                        region.height,
                        region.depth,
                        raw_tuple.format,
                        raw_tuple.gl_type,
                        buffer.as_ptr() as *const _,
                    );
                }
            }
            let write_error = gl::GetError();
            if write_error != gl::NO_ERROR {
                log::warn!(
                    "TextureCache::copy_raw_compressed_block_fallback: write failed err=0x{:X} direction={:?} compressed_fmt={:?} raw_fmt={:?}",
                    write_error,
                    format.direction,
                    format.compressed_format,
                    format.raw_format
                );
                return false;
            }
        }
        true
    }

    fn emulate_copy_image(&mut self, dst_id: ImageId, src_id: ImageId, copies: &[ImageCopy]) {
        let dst_base = self.base.slot_images[dst_id].clone();
        let src_base = self.base.slot_images[src_id].clone();
        let Some(dst_image) = self.images.get(&dst_id) else {
            return;
        };
        let Some(src_image) = self.images.get(&src_id) else {
            return;
        };
        let dst_handle = dst_image.handle();
        let src_handle = src_image.handle();
        if dst_handle == 0 || src_handle == 0 {
            return;
        }
        let handled = self.runtime.emulate_copy_image(
            dst_handle,
            image_target(&dst_base.info),
            dst_image.gl_format,
            dst_image.gl_type,
            &dst_base.info,
            src_handle,
            image_target(&src_base.info),
            src_image.gl_format,
            src_image.gl_type,
            &src_base.info,
            copies,
        );
        if !handled {
            log::warn!(
                "TextureCache::emulate_copy_image: unsupported emulated copy dst={:?} src={:?}",
                dst_base.info.format,
                src_base.info.format
            );
        }
    }

    fn reinterpret_image(&mut self, dst_id: ImageId, src_id: ImageId, copies: &[ImageCopy]) {
        let dst_base = self.base.slot_images[dst_id].clone();
        let src_base = self.base.slot_images[src_id].clone();
        let Some(dst_image) = self.images.get(&dst_id) else {
            return;
        };
        let Some(src_image) = self.images.get(&src_id) else {
            return;
        };
        let dst_handle = dst_image.handle();
        let src_handle = src_image.handle();
        if dst_handle == 0 || src_handle == 0 {
            return;
        }
        let dst_target = image_target(&dst_base.info);
        let src_target = image_target(&src_base.info);
        self.runtime.reinterpret_image(
            dst_handle,
            dst_target,
            dst_image.gl_format,
            dst_image.gl_type,
            &dst_base.info,
            src_handle,
            src_target,
            src_image.gl_format,
            src_image.gl_type,
            &src_base.info,
            copies,
        );
    }

    /// Port of `TextureCache<P>::DownloadMemory` for `TextureCacheParams =
    /// OpenGL`.
    ///
    /// The OpenGL wrapper owns the backend image table, so it performs the
    /// upstream runtime/image download sequence directly, then uses the common
    /// swizzle/writeback helper for guest memory.
    pub fn download_memory(&mut self, cpu_addr: u64, size: usize) {
        let trace_cpu = should_trace_texture_cache_cpu_region(cpu_addr, size);
        if self.base.channel_gpu_memory.is_none() && self.base.guest_memory_writer.is_none() {
            if trace_cpu {
                log::warn!(
                    "[TEXTURE_DOWNLOAD] miss writeback cpu=0x{:X} size={}",
                    cpu_addr,
                    size
                );
            }
            return;
        }

        // Snapshot the images we want to download, filtered + sorted exactly
        // as the base path used to do internally.
        let candidates = self.base.collect_images_in_region(cpu_addr, size);
        if trace_cpu {
            log::warn!(
                "[TEXTURE_DOWNLOAD] begin cpu=0x{:X} size={} candidates={}",
                cpu_addr,
                size,
                candidates.len()
            );
            for &id in &candidates {
                let image = &self.base.slot_images[id];
                log::warn!(
                    "[TEXTURE_DOWNLOAD] candidate id={} gpu=0x{:X} cpu=0x{:X} end=0x{:X} fmt={:?} {}x{} flags=0x{:X} safe={} tick={} overlaps={}",
                    id.index,
                    image.gpu_addr,
                    image.cpu_addr,
                    image.cpu_addr_end,
                    image.info.format,
                    image.info.size.width,
                    image.info.size.height,
                    image.flags.bits(),
                    image.is_safe_download(),
                    image.modification_tick,
                    image.overlapping_images.len()
                );
            }
        }
        let mut images = candidates;
        images.retain(|&id| self.base.slot_images[id].is_safe_download());
        if images.is_empty() {
            if trace_cpu {
                log::warn!(
                    "[TEXTURE_DOWNLOAD] skip no_safe_images cpu=0x{:X} size={}",
                    cpu_addr,
                    size
                );
            }
            return;
        }
        for &id in &images {
            self.base.slot_images[id]
                .flags
                .remove(ImageFlagBits::GPU_MODIFIED);
        }
        images.sort_by_key(|&id| self.base.slot_images[id].modification_tick);

        for image_id in images {
            let Some((base_image, staging)) = self.download_image_to_host_staging(image_id) else {
                continue;
            };
            if trace_cpu {
                let nonzero = staging.iter().filter(|&&byte| byte != 0).take(1024).count();
                let first = staging
                    .get(..16)
                    .map(|bytes| {
                        bytes
                            .iter()
                            .map(|byte| format!("{byte:02X}"))
                            .collect::<Vec<_>>()
                            .join("")
                    })
                    .unwrap_or_default();
                log::warn!(
                    "[TEXTURE_DOWNLOAD] downloaded id={} gpu=0x{:X} cpu=0x{:X} bytes={} first16={} nonzero_first1k={}",
                    image_id.index,
                    base_image.gpu_addr,
                    base_image.cpu_addr,
                    staging.len(),
                    first,
                    nonzero
                );
            }

            let copies = full_download_copies(&base_image.info);
            let _ = self
                .base
                .write_downloaded_image(&base_image, &copies, &staging);
        }
    }

    fn download_image_to_host_staging(
        &mut self,
        image_id: ImageId,
    ) -> Option<(ImageBase, Vec<u8>)> {
        let base_image = self.base.slot_images[image_id].clone();
        let buffer_size = base_image.unswizzled_size_bytes as usize;
        if buffer_size == 0 {
            return None;
        }
        let copies = full_download_copies(&base_image.info);
        let backend_image = self
            .images
            .entry(image_id)
            .or_insert_with(|| Image::from_base(&base_image, self.has_native_astc));
        let mut map = self.runtime.download_staging_buffer(buffer_size, false);
        backend_image.download_memory_to_staging(&mut map, &copies);
        self.runtime.finish();
        Some((base_image, map.mapped_span().to_vec()))
    }

    /// Port of `TextureCache<P>::GetFlushArea`.
    pub fn get_flush_area(
        &mut self,
        cpu_addr: u64,
        size: u64,
    ) -> Option<crate::rasterizer_interface::RasterizerDownloadArea> {
        self.base.get_flush_area(cpu_addr, size as usize)
    }

    /// OpenGL-backed wrapper for `TextureCache<P>::FillGraphicsImageViews`.
    ///
    /// `TextureCacheBase` resolves TIC descriptors and base image-view slots.
    /// The upstream blacklist branch also calls `ScaleDown(image)`, which is
    /// backend-specific, so the OpenGL wrapper owns that part.
    pub fn fill_graphics_image_views(
        &mut self,
        views: &mut [crate::texture_cache::texture_cache_base::ImageViewInOut],
        has_blacklists: bool,
    ) {
        loop {
            self.base.fill_graphics_image_views(views, has_blacklists);
            if !has_blacklists {
                break;
            }

            let mut has_blacklisted = false;
            for view in views.iter() {
                if !view.blacklist || !view.id.is_valid() || view.id == NULL_IMAGE_VIEW_ID {
                    continue;
                }
                let image_id = self.base.slot_image_views[view.id].image_id;
                if !image_id.is_valid() || image_id == NULL_IMAGE_ID {
                    continue;
                }
                self.ensure_backend_image(image_id);
                has_blacklisted |= self.scale_down_image(image_id);
                self.base.slot_images[image_id].scale_rating = 0;
            }
            if !has_blacklisted {
                break;
            }
        }
    }

    /// OpenGL-backed wrapper for `TextureCache<P>::FillComputeImageViews`.
    ///
    /// Upstream always instantiates compute image-view filling with
    /// `has_blacklists=true`; the blacklist branch scales down written images
    /// through the backend `P::Image` owner.
    pub fn fill_compute_image_views(
        &mut self,
        views: &mut [crate::texture_cache::texture_cache_base::ImageViewInOut],
    ) {
        loop {
            self.base.fill_compute_image_views(views);

            let mut has_blacklisted = false;
            for view in views.iter() {
                if !view.blacklist || !view.id.is_valid() || view.id == NULL_IMAGE_VIEW_ID {
                    continue;
                }
                let image_id = self.base.slot_image_views[view.id].image_id;
                if !image_id.is_valid() || image_id == NULL_IMAGE_ID {
                    continue;
                }
                self.ensure_backend_image(image_id);
                has_blacklisted |= self.scale_down_image(image_id);
                self.base.slot_images[image_id].scale_rating = 0;
            }
            if !has_blacklisted {
                break;
            }
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
        self.materialize_views_impl(views, None);
    }

    pub fn materialize_views_with_gpu_reader(
        &mut self,
        views: &[crate::texture_cache::texture_cache_base::ImageViewInOut],
        read_gpu: &mut dyn FnMut(u64, &mut [u8]) -> bool,
    ) {
        self.materialize_views_impl(views, Some(read_gpu));
    }

    fn materialize_views_impl(
        &mut self,
        views: &[crate::texture_cache::texture_cache_base::ImageViewInOut],
        mut read_gpu: Option<&mut dyn FnMut(u64, &mut [u8]) -> bool>,
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
                    .or_insert_with(|| Image::from_base(&base_image, self.has_native_astc));
                img.handle()
            };
            if backend_image_handle == 0 {
                // Image::from_base couldn't allocate (TEXTURE_BUFFER or
                // glCreateTextures failure). Skip so a subsequent draw can
                // retry without leaving a stale ImageView handle behind.
                continue;
            }
            let image_ref = self
                .images
                .get(&image_id)
                .expect("image inserted above must be present");
            let view_matches = self
                .image_views
                .get(&view_id)
                .is_some_and(|view| view.matches_base_image(&view_base, image_ref));
            if view_matches {
                continue;
            }
            let old_view = self.image_views.get(&view_id);
            if old_view.is_some() {
                trace_image_view_event(3, view_id, &view_base, image_ref, old_view);
            }
            self.image_views.remove(&view_id);
            self.remove_framebuffers_for_view(view_id);
            let image_ref = self
                .images
                .get(&image_id)
                .expect("image inserted above must be present");
            let backend_view = ImageView::from_image_view_info(
                &view_base,
                image_ref,
                self.runtime.null_image_views,
            );
            trace_image_view_event(2, view_id, &view_base, image_ref, Some(&backend_view));
            self.image_views.insert(view_id, backend_view);
        }
        // Upstream performs the JoinImages copy/delete tail synchronously before
        // returning the newly-created image id. Ruzu's base cache queues that
        // tail until backend GL objects exist, so drain it at the texture-view
        // materialisation boundary as well as at render-target setup. Without
        // this, a sampled view can bind an image before its overlapping source
        // content has been copied into it.
        if let Some(reader) = read_gpu.as_deref_mut() {
            self.finish_pending_join_copies_with_gpu_reader(reader);
        } else {
            self.finish_pending_join_copies();
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
        if should_trace_texture_cache_cpu_region(cpu_addr, size) {
            let candidates = self.base.collect_images_in_region(cpu_addr, size);
            log::warn!(
                "[TEXTURE_WRITE] cpu=0x{:X} size={} candidates={}",
                cpu_addr,
                size,
                candidates.len()
            );
            for id in candidates {
                let image = &self.base.slot_images[id];
                log::warn!(
                    "[TEXTURE_WRITE] candidate id={} gpu=0x{:X} cpu=0x{:X} end=0x{:X} fmt={:?} {}x{} flags_before=0x{:X} tick={} overlaps={}",
                    id.index,
                    image.gpu_addr,
                    image.cpu_addr,
                    image.cpu_addr_end,
                    image.info.format,
                    image.info.size.width,
                    image.info.size.height,
                    image.flags.bits(),
                    image.modification_tick,
                    image.overlapping_images.len()
                );
            }
        }
        self.base.write_memory(cpu_addr, size);
    }

    pub fn unmap_memory(&mut self, cpu_addr: u64, size: usize) {
        let end = cpu_addr.saturating_add(size as u64);
        let deleted_images: Vec<(ImageId, Vec<ImageViewId>)> = self
            .base
            .slot_images
            .iter()
            .filter_map(|(image_id, image)| {
                if image_id == crate::texture_cache::types::NULL_IMAGE_ID {
                    return None;
                }
                let image_end = image
                    .cpu_addr_end
                    .max(image.cpu_addr.saturating_add(map_size_bytes(image) as u64));
                (image.cpu_addr < end && cpu_addr < image_end)
                    .then(|| (image_id, image.image_view_ids.clone()))
            })
            .collect();
        if common::trace::is_enabled(common::trace::cat::IMAGE_VIEW) {
            common::trace::emit_raw(
                common::trace::cat::IMAGE_VIEW,
                &[
                    8,
                    u64::MAX,
                    u64::MAX,
                    self.images.len() as u64,
                    self.image_views.len() as u64,
                    self.base.framebuffers.len() as u64,
                    self.slot_framebuffers.size() as u64,
                    deleted_images.len() as u64,
                    0,
                    0,
                    size as u64,
                    cpu_addr,
                    0,
                    0,
                ],
            );
        }
        // RUZU_TRACE_UNMAP_VICTIMS=1 — name every image evicted by this unmap
        // walk (correlates scene-RT evictions with composite draws).
        if std::env::var_os("RUZU_TRACE_UNMAP_VICTIMS").is_some() {
            for (image_id, _) in &deleted_images {
                let image = &self.base.slot_images[*image_id];
                log::warn!(
                    "[UNMAP_VICTIM] unmap=0x{:X}+0x{:X} image_id={} gpu=0x{:X} cpu=0x{:X} size=0x{:X} {}x{} flags=0x{:X}",
                    cpu_addr,
                    size,
                    image_id.index,
                    image.gpu_addr,
                    image.cpu_addr,
                    image.guest_size_bytes,
                    image.info.size.width,
                    image.info.size.height,
                    image.flags.bits(),
                );
            }
        }
        for (image_id, view_ids) in deleted_images {
            self.images.remove(&image_id);
            for view_id in view_ids {
                self.image_views.remove(&view_id);
                self.remove_framebuffers_for_view(view_id);
            }
        }
        self.base.unmap_memory(cpu_addr, size);
    }

    pub fn tick_frame(&mut self) {
        if self.runtime.can_report_memory_usage() {
            self.base
                .update_total_used_memory_from_runtime(self.runtime.get_device_memory_usage());
        }
        if self.base.total_used_memory > self.base.minimum_memory {
            let framebuffers_before_gc = self.base.framebuffers.clone();
            let runtime = &mut self.runtime;
            let images = &mut self.images;
            let has_native_astc = self.has_native_astc;
            self.base
                .run_garbage_collector_with_downloader(|image_id, base_image, staging| {
                    if staging.is_empty() {
                        return false;
                    }
                    let copies = full_download_copies(&base_image.info);
                    let backend_image = images
                        .entry(image_id)
                        .or_insert_with(|| Image::from_base(base_image, has_native_astc));
                    let mut map = runtime.download_staging_buffer(staging.len(), false);
                    backend_image.download_memory_to_staging(&mut map, &copies);
                    runtime.finish();
                    staging.copy_from_slice(map.mapped_span());
                    true
                });
            self.sentence_framebuffers_removed_by_base(&framebuffers_before_gc);
        }
        self.base.tick_delayed_destruction_rings();
        self.sentenced_framebuffers.tick();
        self.tick_async_decode();
        self.runtime.tick_frame();
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

    fn remove_framebuffers_for_view(&mut self, view_id: ImageViewId) {
        let removed_ids = [view_id];
        self.base.framebuffers.retain(|key, framebuffer_id| {
            if key.contains(&removed_ids) {
                let framebuffer = self.slot_framebuffers.take(*framebuffer_id);
                self.sentenced_framebuffers.push(framebuffer);
                false
            } else {
                true
            }
        });
    }

    fn sentence_framebuffers_removed_by_base(
        &mut self,
        old_framebuffers: &HashMap<RenderTargets, FramebufferId>,
    ) {
        for (key, &framebuffer_id) in old_framebuffers {
            if self.base.framebuffers.contains_key(key) {
                continue;
            }
            if framebuffer_id.is_valid() {
                let framebuffer = self.slot_framebuffers.take(framebuffer_id);
                self.sentenced_framebuffers.push(framebuffer);
            }
        }
    }

    pub fn update_render_targets_from_snapshot(
        &mut self,
        render_targets: &Maxwell3DRenderTargets,
        mut gpu_to_cpu: impl FnMut(crate::texture_cache::image_base::GPUVAddr) -> Option<u64>,
    ) {
        let (rescaled, scale_rating, color_images, depth_image) = loop {
            self.base.has_deleted_images = false;
            self.base
                .update_render_targets_from_snapshot(render_targets, &mut gpu_to_cpu);
            let result = self.rescale_current_render_targets();
            if !self.base.has_deleted_images {
                break result;
            }
        };
        self.set_render_target_scale_rating(scale_rating, color_images, depth_image);
        self.base.is_rescaling = rescaled;
        self.update_rescaled_render_target_size(render_targets);
    }

    pub fn prepare_render_targets_from_snapshot(
        &mut self,
        _render_targets: &Maxwell3DRenderTargets,
        mut read_gpu: Option<&mut dyn FnMut(u64, &mut [u8]) -> bool>,
        is_clear: bool,
        clear_scissor: Option<ScissorInfo>,
    ) {
        let mut image_views = Vec::new();
        for &view_id in &self.base.render_targets.color_buffer_ids {
            if view_id.is_valid() {
                image_views.push(view_id);
            }
        }
        let depth_view_id = self.base.render_targets.depth_buffer_id;
        if depth_view_id.is_valid() {
            image_views.push(depth_view_id);
        }

        image_views.sort_by_key(|id| id.index);
        image_views.dedup_by_key(|id| id.index);
        for view_id in image_views {
            let image_id = self.base.slot_image_views[view_id].image_id;
            let invalidate = is_clear && self.is_full_clear_for_view(view_id, clear_scissor);
            if let Some(reader) = read_gpu.as_deref_mut() {
                self.prepare_image_with_gpu_reader(image_id, true, invalidate, reader);
            } else {
                if self.prepare_image_with_bound_gpu_reader(image_id, true, invalidate) {
                    continue;
                }
                if invalidate {
                    let image = &mut self.base.slot_images[image_id];
                    image
                        .flags
                        .remove(ImageFlagBits::CPU_MODIFIED | ImageFlagBits::GPU_MODIFIED);
                    if !self.base.slot_images[image_id]
                        .flags
                        .contains(ImageFlagBits::TRACKED)
                    {
                        self.base.track_image(image_id);
                    }
                } else {
                    // Upstream `PrepareImage(image_id, true, false)` always
                    // synchronizes aliases before marking the render target as
                    // modified. The reader-less Rust path cannot refresh CPU
                    // writes, so only run alias copies when RefreshContents
                    // would be a no-op.
                    if self.can_synchronize_aliases_without_refresh(image_id) {
                        self.synchronize_aliases(image_id);
                    }
                }
                self.base.mark_modification_by_id(image_id);
            }
        }
        // Upstream performs the JoinImages copy/delete tail after
        // RefreshContents and before RegisterImage returns. The Rust cache
        // splits UpdateRenderTargets and PrepareImageView, so drain after the
        // prepare pass rather than immediately after target discovery.
        if let Some(reader) = read_gpu.as_deref_mut() {
            self.finish_pending_join_copies_with_gpu_reader(reader);
        } else {
            self.finish_pending_join_copies();
        }
    }

    fn is_full_clear_for_view(&self, view_id: ImageViewId, scissor: Option<ScissorInfo>) -> bool {
        if !view_id.is_valid() {
            return true;
        }
        let image_view = &self.base.slot_image_views[view_id];
        let image = &self.base.slot_images[image_view.image_id];
        if image.info.resources.levels > 1 || image.info.resources.layers > 1 {
            return false;
        }
        let Some(scissor) = scissor else {
            return true;
        };
        scissor.min_x == 0
            && scissor.min_y == 0
            && scissor.max_x >= image_view.size.width
            && scissor.max_y >= image_view.size.height
    }

    /// OpenGL counterpart of upstream `TextureCache<P>::GetFramebuffer()`.
    ///
    /// Upstream keys framebuffers by the complete `RenderTargets` object and
    /// attaches every color target before selecting draw buffers. The previous
    /// Rust path bound only the first mapped render target, which is not
    /// equivalent for MK8D's multi-pass composition.
    pub fn framebuffer_for_render_targets_from_snapshot(
        &mut self,
        _render_targets: &Maxwell3DRenderTargets,
        size: Extent2D,
    ) -> Option<(u32, u32, u32)> {
        let mut key = self.base.render_targets;
        if key.size.width == 0 {
            key.size.width = size.width;
        }
        if key.size.height == 0 {
            key.size.height = size.height;
        }
        let mut attachment_textures = [0u32; NUM_RT];
        let mut depth_attachment_texture = 0u32;
        let mut depth_attachment_format = PixelFormat::Invalid;
        let mut depth_attachment_view_id = ImageViewId::default();
        let mut depth_attachment_image_id = ImageId::default();
        let mut depth_attachment_gpu_addr = 0u64;
        let mut depth_attachment_gl = gl::NONE;
        let mut max_width = 0u32;
        let mut max_height = 0u32;

        for index in 0..NUM_RT {
            let view_id = key.color_buffer_ids[index];
            if !view_id.is_valid() {
                continue;
            }

            let view_base = self.base.slot_image_views[view_id].clone();
            let image_id = view_base.image_id;
            let image_base = self.base.slot_images[image_id].clone();
            self.images
                .entry(image_id)
                .or_insert_with(|| Image::from_base(&image_base, self.has_native_astc));
            let view_mismatch = {
                let backend_image = self
                    .images
                    .get(&image_id)
                    .expect("image inserted above must be present");
                self.image_views
                    .get(&view_id)
                    .is_some_and(|view| !view.matches_base_image(&view_base, backend_image))
            };
            if view_mismatch {
                let backend_image = self
                    .images
                    .get(&image_id)
                    .expect("image inserted above must be present");
                let old_view = self.image_views.get(&view_id);
                trace_image_view_event(7, view_id, &view_base, backend_image, old_view);
                self.image_views.remove(&view_id);
                self.remove_framebuffers_for_view(view_id);
            }
            let backend_image = self
                .images
                .get(&image_id)
                .expect("image inserted above must be present");
            let backend_view = self.image_views.entry(view_id).or_insert_with(|| {
                ImageView::new_color_2d(&view_base, backend_image, self.runtime.null_image_views)
            });
            trace_image_view_event(6, view_id, &view_base, backend_image, Some(backend_view));
            let attachment_texture = if view_base.flags.contains(ImageViewFlagBits::SLICE) {
                backend_view.handle_for_texture_type(TextureType::Color3D)
            } else {
                backend_view.default_handle()
            };
            if attachment_texture == 0 {
                key.color_buffer_ids[index] = ImageViewId::default();
                continue;
            }

            attachment_textures[index] = attachment_texture;
            max_width = max_width.max(view_base.size.width);
            max_height = max_height.max(view_base.size.height);
        }

        let view_id = key.depth_buffer_id;
        if view_id.is_valid() {
            let view_base = self.base.slot_image_views[view_id].clone();
            let image_id = view_base.image_id;
            let image_base = self.base.slot_images[image_id].clone();
            self.images
                .entry(image_id)
                .or_insert_with(|| Image::from_base(&image_base, self.has_native_astc));
            let backend_image = self
                .images
                .get(&image_id)
                .expect("depth image inserted above must be present");
            let backend_view = self.image_views.entry(view_id).or_insert_with(|| {
                ImageView::new_color_2d(&view_base, backend_image, self.runtime.null_image_views)
            });
            depth_attachment_texture = backend_view.default_handle();
            depth_attachment_format = self.base.slot_images[image_id].info.format;
            depth_attachment_view_id = view_id;
            depth_attachment_image_id = image_id;
            depth_attachment_gpu_addr = self.base.slot_images[image_id].gpu_addr;
            max_width = max_width.max(view_base.size.width);
            max_height = max_height.max(view_base.size.height);
            trace_image_view_event(6, view_id, &view_base, backend_image, Some(backend_view));
        }

        if attachment_textures.iter().all(|&texture| texture == 0) && depth_attachment_texture == 0
        {
            return None;
        }
        if key.size.width == 0 {
            key.size.width = max_width;
        }
        if key.size.height == 0 {
            key.size.height = max_height;
        }

        let framebuffer_id = if let Some(&framebuffer_id) = self.base.framebuffers.get(&key) {
            framebuffer_id
        } else {
            let mut framebuffer = 0;
            let mut buffer_bits = gl::NONE;
            unsafe {
                gl::CreateFramebuffers(1, &mut framebuffer);
                if framebuffer != 0 {
                    let mut num_buffers = 0i32;
                    let mut gl_draw_buffers = [gl::NONE; NUM_RT];
                    for index in 0..NUM_RT {
                        let texture = attachment_textures[index];
                        if texture == 0 {
                            continue;
                        }
                        buffer_bits |= gl::COLOR_BUFFER_BIT;
                        gl_draw_buffers[index] =
                            gl::COLOR_ATTACHMENT0 + key.draw_buffers[index] as u32;
                        num_buffers = index as i32 + 1;

                        let view_id = key.color_buffer_ids[index];
                        let view_base = self.base.slot_image_views[view_id].clone();
                        let attachment = gl::COLOR_ATTACHMENT0 + index as u32;
                        if view_base.flags.contains(ImageViewFlagBits::SLICE)
                            && view_base.range.extent.layers == 1
                        {
                            gl::NamedFramebufferTextureLayer(
                                framebuffer,
                                attachment,
                                texture,
                                0,
                                view_base.range.base.layer,
                            );
                        } else {
                            gl::NamedFramebufferTexture(framebuffer, attachment, texture, 0);
                        }
                    }
                    if depth_attachment_texture != 0 {
                        let attachment = framebuffer_attachment_type(depth_attachment_format);
                        depth_attachment_gl = attachment;
                        buffer_bits |=
                            match crate::surface::get_format_type(depth_attachment_format) {
                                SurfaceType::Depth => gl::DEPTH_BUFFER_BIT,
                                SurfaceType::Stencil => gl::STENCIL_BUFFER_BIT,
                                SurfaceType::DepthStencil => {
                                    gl::DEPTH_BUFFER_BIT | gl::STENCIL_BUFFER_BIT
                                }
                                _ => gl::DEPTH_BUFFER_BIT,
                            };
                        gl::NamedFramebufferTexture(
                            framebuffer,
                            attachment,
                            depth_attachment_texture,
                            0,
                        );
                    }

                    if num_buffers > 1 {
                        gl::NamedFramebufferDrawBuffers(
                            framebuffer,
                            num_buffers,
                            gl_draw_buffers.as_ptr(),
                        );
                    } else if num_buffers > 0 {
                        gl::NamedFramebufferDrawBuffer(framebuffer, gl_draw_buffers[0]);
                    } else {
                        gl::NamedFramebufferDrawBuffer(framebuffer, gl::NONE);
                    }
                    gl::NamedFramebufferParameteri(
                        framebuffer,
                        gl::FRAMEBUFFER_DEFAULT_WIDTH,
                        key.size.width as i32,
                    );
                    gl::NamedFramebufferParameteri(
                        framebuffer,
                        gl::FRAMEBUFFER_DEFAULT_HEIGHT,
                        key.size.height as i32,
                    );
                    if common::trace::is_enabled(common::trace::cat::RT_DEPTH_ATTACH)
                        && depth_attachment_texture != 0
                    {
                        let draw_buffers_pack = key
                            .draw_buffers
                            .iter()
                            .take(8)
                            .enumerate()
                            .fold(0u64, |acc, (index, &buffer)| {
                                acc | ((buffer as u64) << (index * 8))
                            });
                        let status =
                            gl::CheckNamedFramebufferStatus(framebuffer, gl::DRAW_FRAMEBUFFER);
                        common::trace::emit_raw(
                            common::trace::cat::RT_DEPTH_ATTACH,
                            &[
                                framebuffer as u64,
                                depth_attachment_view_id.index as u64,
                                depth_attachment_image_id.index as u64,
                                depth_attachment_gpu_addr,
                                depth_attachment_format as u64,
                                ((key.size.width as u64) << 32) | key.size.height as u64,
                                depth_attachment_texture as u64,
                                depth_attachment_gl as u64,
                                buffer_bits as u64,
                                status as u64,
                                key.color_buffer_ids[0].index as u64,
                                key.color_buffer_ids[1].index as u64,
                                draw_buffers_pack,
                            ],
                        );
                    }
                }
            }
            let framebuffer_id = self.slot_framebuffers.insert(TextureCacheFramebuffer {
                framebuffer,
                buffer_bits,
            });
            self.base.framebuffers.insert(key, framebuffer_id);
            framebuffer_id
        };
        let framebuffer = &self.slot_framebuffers[framebuffer_id];
        let handle = framebuffer.handle();
        (handle != 0).then_some((handle, key.size.width, key.size.height))
    }

    /// OpenGL-backed port of `TextureCache<P>::BlitImage` for the currently
    /// implemented framebuffer-blit path (`TextureCacheParams::FRAMEBUFFER_BLITS`).
    pub fn blit_image(
        &mut self,
        dst: &crate::engines::fermi_2d::Surface,
        src: &crate::engines::fermi_2d::Surface,
        copy: &crate::engines::fermi_2d::Config,
        mut gpu_to_cpu: impl FnMut(u64) -> Option<u64>,
        mut read_gpu: impl FnMut(u64, &mut [u8]) -> bool,
    ) -> bool {
        let dst_addr = dst.address();
        let src_addr = src.address();
        let mut dst_info = ImageInfo::from_fermi2d_surface(dst);
        let mut src_info = ImageInfo::from_fermi2d_surface(src);
        let can_be_depth_blit = dst_info.format == src_info.format
            && copy.filter == crate::engines::fermi_2d::Filter::Point;
        let try_options = if can_be_depth_blit {
            RelaxedOptions::SAMPLES | RelaxedOptions::FORMAT
        } else {
            RelaxedOptions::SAMPLES
        };

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

        let mut src_id;
        let mut dst_id;
        loop {
            self.base.has_deleted_images = false;
            src_id = self.base.find_image_in_cpu_region_with_caps(
                &src_info,
                src_addr,
                src_cpu_addr,
                try_options,
                self.has_broken_texture_view_formats,
                self.has_native_bgr,
            );
            dst_id = self.base.find_image_in_cpu_region_with_caps(
                &dst_info,
                dst_addr,
                dst_cpu_addr,
                try_options,
                self.has_broken_texture_view_formats,
                self.has_native_bgr,
            );
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
                if src_id.is_none() && dst_id.is_none() {
                    return false;
                }
                if !src_gpu_modified && !dst_gpu_modified {
                    return false;
                }
            }

            let src_image = src_id.map(|id| &self.base.slot_images[id]);
            if src_image.is_some_and(|image| image.info.num_samples > 1) {
                let msaa_options = RelaxedOptions::SAMPLES | RelaxedOptions::FORCE_BROKEN_VIEWS;
                src_id = Some(self.base.find_or_insert_image_from_info_with_options(
                    &src_info,
                    src_addr,
                    src_cpu_addr,
                    msaa_options,
                ));
                dst_id = Some(self.base.find_or_insert_image_from_info_with_options(
                    &dst_info,
                    dst_addr,
                    dst_cpu_addr,
                    msaa_options,
                ));
                if self.base.has_deleted_images {
                    continue;
                }
                break;
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
                    continue;
                }
            }

            if src_id.is_none() {
                src_id = Some(self.base.find_or_insert_image_from_info_with_options(
                    &src_info,
                    src_addr,
                    src_cpu_addr,
                    RelaxedOptions::empty(),
                ));
            }
            if dst_id.is_none() {
                dst_id = Some(self.base.find_or_insert_image_from_info_with_options(
                    &dst_info,
                    dst_addr,
                    dst_cpu_addr,
                    RelaxedOptions::empty(),
                ));
            }
            if !self.base.has_deleted_images {
                break;
            }
        }

        let mut src_id = src_id.unwrap_or(NULL_IMAGE_ID);
        let mut dst_id = dst_id.unwrap_or(NULL_IMAGE_ID);
        if !src_id.is_valid() || !dst_id.is_valid() {
            return false;
        }

        self.finish_pending_join_copies_with_gpu_reader(&mut read_gpu);
        if !self.base_image_exists(src_id) || !self.base_image_exists(dst_id) {
            return false;
        }

        let native_bgr = self.has_native_bgr;
        if crate::surface::get_format_type(dst_info.format)
            != crate::surface::get_format_type(self.base.slot_images[dst_id].info.format)
            || crate::surface::get_format_type(src_info.format)
                != crate::surface::get_format_type(self.base.slot_images[src_id].info.format)
            || !crate::surface::is_view_compatible(
                dst_info.format,
                self.base.slot_images[dst_id].info.format,
                false,
                native_bgr,
            )
            || !crate::surface::is_view_compatible(
                src_info.format,
                self.base.slot_images[src_id].info.format,
                false,
                native_bgr,
            )
        {
            loop {
                self.base.has_deleted_images = false;
                src_id = self.base.find_or_insert_image_from_info_with_options(
                    &src_info,
                    src_addr,
                    src_cpu_addr,
                    RelaxedOptions::empty(),
                );
                dst_id = self.base.find_or_insert_image_from_info_with_options(
                    &dst_info,
                    dst_addr,
                    dst_cpu_addr,
                    RelaxedOptions::empty(),
                );
                if !self.base.has_deleted_images {
                    break;
                }
            }
            self.finish_pending_join_copies_with_gpu_reader(&mut read_gpu);
            if !self.base_image_exists(src_id) || !self.base_image_exists(dst_id) {
                return false;
            }
        }

        self.prepare_image_with_gpu_reader(src_id, false, false, &mut read_gpu);
        self.prepare_image_with_gpu_reader(dst_id, true, false, &mut read_gpu);

        self.images.entry(dst_id).or_insert_with(|| {
            Image::from_base(&self.base.slot_images[dst_id], self.has_native_astc)
        });
        self.images.entry(src_id).or_insert_with(|| {
            Image::from_base(&self.base.slot_images[src_id], self.has_native_astc)
        });

        let mut is_src_rescaled = self.base.slot_images[src_id]
            .flags
            .contains(ImageFlagBits::RESCALED);
        let mut is_dst_rescaled = self.base.slot_images[dst_id]
            .flags
            .contains(ImageFlagBits::RESCALED);
        let is_resolve = self.base.slot_images[src_id].info.num_samples != 1
            && self.base.slot_images[dst_id].info.num_samples == 1;
        if is_src_rescaled != is_dst_rescaled {
            if self.image_can_rescale(src_id) {
                self.scale_up_image(src_id);
                is_src_rescaled = self.base.slot_images[src_id]
                    .flags
                    .contains(ImageFlagBits::RESCALED);
                if is_resolve {
                    self.base.slot_images[dst_id].info.rescaleable = true;
                    let aliases = self.base.slot_images[dst_id].aliased_images.clone();
                    for alias in aliases {
                        self.base.slot_images[alias.id].info.rescaleable = true;
                    }
                }
            }
            if self.image_can_rescale(dst_id) {
                self.scale_up_image(dst_id);
                is_dst_rescaled = self.base.slot_images[dst_id]
                    .flags
                    .contains(ImageFlagBits::RESCALED);
            }
        }
        if is_resolve && is_src_rescaled != is_dst_rescaled {
            self.scale_down_image(src_id);
            self.scale_down_image(dst_id);
            is_src_rescaled = self.base.slot_images[src_id]
                .flags
                .contains(ImageFlagBits::RESCALED);
            is_dst_rescaled = self.base.slot_images[dst_id]
                .flags
                .contains(ImageFlagBits::RESCALED);
        }
        let resolution = settings::values().resolution_info.clone();
        let scale_region = |region: &mut Region2D| {
            region.start.x = resolution.scale_up_i32(region.start.x);
            region.start.y = resolution.scale_up_i32(region.start.y);
            region.end.x = resolution.scale_up_i32(region.end.x);
            region.end.y = resolution.scale_up_i32(region.end.y);
        };

        let Some(src_base) = self.base.slot_images[src_id].try_find_base(src_addr) else {
            return false;
        };
        let Some(dst_base) = self.base.slot_images[dst_id].try_find_base(dst_addr) else {
            return false;
        };
        let src_range = SubresourceRange {
            base: src_base,
            ..Default::default()
        };
        let dst_range = SubresourceRange {
            base: dst_base,
            ..Default::default()
        };
        let src_view_id = self.ensure_color_view_for_range(src_id, src_info.format, src_range);
        let dst_view_id = self.ensure_color_view_for_range(dst_id, dst_info.format, dst_range);
        let Some((src_fbo, _, _)) = self.framebuffer_for_image_view(src_view_id) else {
            return false;
        };
        let Some((dst_fbo, _, _)) = self.framebuffer_for_image_view(dst_view_id) else {
            return false;
        };

        let (src_samples_x, src_samples_y) = crate::texture_cache::samples_helper::samples_log2(
            self.base.slot_images[src_id].info.num_samples as i32,
        );
        let (dst_samples_x, dst_samples_y) = crate::texture_cache::samples_helper::samples_log2(
            self.base.slot_images[dst_id].info.num_samples as i32,
        );

        let mut src_region = Region2D {
            start: Offset2D {
                x: copy.src_x0 >> src_samples_x,
                y: copy.src_y0 >> src_samples_y,
            },
            end: Offset2D {
                x: copy.src_x1 >> src_samples_x,
                y: copy.src_y1 >> src_samples_y,
            },
        };
        if is_src_rescaled {
            scale_region(&mut src_region);
        }
        let mut dst_region = Region2D {
            start: Offset2D {
                x: copy.dst_x0 >> dst_samples_x,
                y: copy.dst_y0 >> dst_samples_y,
            },
            end: Offset2D {
                x: copy.dst_x1 >> dst_samples_x,
                y: copy.dst_y1 >> dst_samples_y,
            },
        };
        if is_dst_rescaled {
            scale_region(&mut dst_region);
        }
        self.runtime.blit_framebuffer(
            dst_fbo,
            src_fbo,
            gl::COLOR_BUFFER_BIT,
            gl::COLOR_BUFFER_BIT,
            dst_region,
            src_region,
            copy.filter,
            copy.operation,
        );
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

    fn find_mapped_image_with_options(
        &self,
        info: &ImageInfo,
        gpu_addr: u64,
        options: RelaxedOptions,
    ) -> Option<ImageId> {
        let broken_views = self.has_broken_texture_view_formats
            || options.contains(RelaxedOptions::FORCE_BROKEN_VIEWS);
        let native_bgr = self.has_native_bgr;
        self.base
            .slot_images
            .iter()
            .filter_map(|(id, image)| {
                if image.flags.contains(ImageFlagBits::REMAPPED) {
                    return None;
                }
                crate::texture_cache::util::is_subresource(
                    info,
                    image,
                    gpu_addr,
                    options,
                    broken_views,
                    native_bgr,
                )
                .then_some((id, image.modification_tick))
            })
            .max_by_key(|(_, tick)| *tick)
            .map(|(id, _)| id)
    }

    fn find_or_insert_mapped_image_with_options(
        &mut self,
        info: &ImageInfo,
        gpu_addr: u64,
        cpu_addr: u64,
        options: RelaxedOptions,
    ) -> ImageId {
        self.find_mapped_image_with_options(info, gpu_addr, options)
            .unwrap_or_else(|| self.find_or_insert_mapped_image(info, gpu_addr, cpu_addr))
    }

    fn ensure_color_view_for_range(
        &mut self,
        image_id: ImageId,
        view_format: crate::surface::PixelFormat,
        range: SubresourceRange,
    ) -> ImageViewId {
        let view_info = ImageViewInfo::for_render_target(ImageViewType::E2D, view_format, range);
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
        self.images.entry(image_id).or_insert_with(|| {
            Image::from_base(&self.base.slot_images[image_id], self.has_native_astc)
        });
        let view_mismatch = {
            let backend_image = self
                .images
                .get(&image_id)
                .expect("image inserted above must be present");
            self.image_views
                .get(&view_id)
                .is_some_and(|view| !view.matches_base_image(&view_base, backend_image))
        };
        if view_mismatch {
            let backend_image = self
                .images
                .get(&image_id)
                .expect("image inserted above must be present");
            let old_view = self.image_views.get(&view_id);
            trace_image_view_event(5, view_id, &view_base, backend_image, old_view);
            self.image_views.remove(&view_id);
            self.remove_framebuffers_for_view(view_id);
        }
        let backend_image = self
            .images
            .get(&image_id)
            .expect("image inserted above must be present");
        let backend_view = self.image_views.entry(view_id).or_insert_with(|| {
            ImageView::new_color_2d(&view_base, backend_image, self.runtime.null_image_views)
        });
        trace_image_view_event(4, view_id, &view_base, backend_image, Some(backend_view));
        let attachment_texture = if view_base.flags.contains(ImageViewFlagBits::SLICE) {
            backend_view.handle_for_texture_type(TextureType::Color3D)
        } else {
            backend_view.default_handle()
        };
        if attachment_texture == 0 {
            return None;
        }
        let key = self.render_targets_key_for_image_view(view_id, &view_base);
        let framebuffer_id = if let Some(&framebuffer_id) = self.base.framebuffers.get(&key) {
            framebuffer_id
        } else {
            let framebuffer =
                self.create_color_framebuffer_for_view(key, &view_base, attachment_texture);
            let framebuffer_id = self.slot_framebuffers.insert(framebuffer);
            self.base.framebuffers.insert(key, framebuffer_id);
            framebuffer_id
        };
        let framebuffer = &self.slot_framebuffers[framebuffer_id];
        let handle = framebuffer.handle();
        (handle != 0).then_some((handle, key.size.width, key.size.height))
    }

    fn render_targets_key_for_image_view(
        &self,
        view_id: ImageViewId,
        view_base: &ImageViewBase,
    ) -> RenderTargets {
        let image = &self.base.slot_images[view_base.image_id];
        let is_rescaled = image.flags.contains(ImageFlagBits::RESCALED);
        let resolution = settings::values().resolution_info.clone();
        let mut width = view_base.size.width;
        let mut height = view_base.size.height;
        if is_rescaled {
            width = resolution.scale_up_u32(width);
            if image.info.image_type == ImageType::E2D {
                height = resolution.scale_up_u32(height);
            }
        }
        let (samples_x, samples_y) =
            crate::texture_cache::samples_helper::samples_log2(image.info.num_samples as i32);
        let mut color_buffer_ids = [ImageViewId::default(); NUM_RT];
        color_buffer_ids[0] = view_id;
        RenderTargets {
            color_buffer_ids,
            depth_buffer_id: ImageViewId::default(),
            draw_buffers: [0; NUM_RT],
            size: Extent2D {
                width: (width >> samples_x).max(1),
                height: (height >> samples_y).max(1),
            },
            is_rescaled,
        }
    }

    fn create_color_framebuffer_for_view(
        &self,
        key: RenderTargets,
        view_base: &ImageViewBase,
        attachment_texture: u32,
    ) -> TextureCacheFramebuffer {
        let mut framebuffer = 0;
        unsafe {
            gl::CreateFramebuffers(1, &mut framebuffer);
            if framebuffer != 0 {
                if view_base.flags.contains(ImageViewFlagBits::SLICE)
                    && view_base.range.extent.layers == 1
                {
                    gl::NamedFramebufferTextureLayer(
                        framebuffer,
                        gl::COLOR_ATTACHMENT0,
                        attachment_texture,
                        0,
                        view_base.range.base.layer,
                    );
                } else {
                    gl::NamedFramebufferTexture(
                        framebuffer,
                        gl::COLOR_ATTACHMENT0,
                        attachment_texture,
                        0,
                    );
                }
                gl::NamedFramebufferDrawBuffer(framebuffer, gl::COLOR_ATTACHMENT0);
                gl::NamedFramebufferParameteri(
                    framebuffer,
                    gl::FRAMEBUFFER_DEFAULT_WIDTH,
                    key.size.width as i32,
                );
                gl::NamedFramebufferParameteri(
                    framebuffer,
                    gl::FRAMEBUFFER_DEFAULT_HEIGHT,
                    key.size.height as i32,
                );
            }
        }
        TextureCacheFramebuffer {
            framebuffer,
            buffer_bits: gl::COLOR_BUFFER_BIT,
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
        self.images.entry(image_id).or_insert_with(|| {
            let image = &self.base.slot_images[image_id];
            Image::from_base(image, self.has_native_astc)
        });
        let backend_image = self
            .images
            .get(&image_id)
            .expect("image inserted above must be present");
        let original_texture = backend_image.handle();
        let current_texture = backend_image.current_texture;
        let view_mismatch = self
            .image_views
            .get(&view_id)
            .is_some_and(|backend_view| !backend_view.matches_base_image(&view, backend_image));
        if view_mismatch {
            let old_view = self.image_views.get(&view_id);
            trace_image_view_event(5, view_id, &view, backend_image, old_view);
            self.image_views.remove(&view_id);
            self.remove_framebuffers_for_view(view_id);
        }
        let backend_image = self
            .images
            .get(&image_id)
            .expect("image inserted above must be present");
        let backend_view = self.image_views.entry(view_id).or_insert_with(|| {
            ImageView::from_image_view_info(&view, backend_image, self.runtime.null_image_views)
        });
        trace_image_view_event(4, view_id, &view, backend_image, Some(backend_view));
        let display_texture = backend_view.handle_for_texture_type(TextureType::Color2D);
        if display_texture == 0 {
            return None;
        }
        if std::env::var_os("RUZU_TRACE_PRESENT_HANDLES").is_some() {
            let mut fbo_handle: u32 = 0;
            let mut fbo_attachment: i32 = 0;
            let view_ids = [view_id];
            if let Some((_, &framebuffer_id)) = self
                .base
                .framebuffers
                .iter()
                .find(|(key, _)| key.contains(&view_ids))
            {
                if framebuffer_id.is_valid() {
                    let fb = &self.slot_framebuffers[framebuffer_id];
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

    /// Diagnostic-only present-time readback of already-materialised GL
    /// images by guest GPU address.
    ///
    /// Upstream has no equivalent production path; this mirrors the local
    /// `AccelerateDisplay` debug hooks and is gated by
    /// `RUZU_DUMP_PRESENT_EXTRA_GPU_ADDRS`; trace-ring emission still requires
    /// `opengl.present_texture`, while PPM dumping only requires
    /// `RUZU_DUMP_PRESENT_EXTRA_PPM_DIR`.
    pub fn trace_present_images_by_gpu_addr(&self, present_index: u64, gpu_addrs: &[u64]) {
        let trace_enabled = common::trace::is_enabled(common::trace::cat::PRESENT_TEXTURE);
        let ppm_enabled = std::env::var_os("RUZU_DUMP_PRESENT_EXTRA_PPM_DIR").is_some();
        if !trace_enabled && !ppm_enabled {
            return;
        }

        let _texture_lock = self.base.mutex.lock();
        for &gpu_addr in gpu_addrs {
            let Some((image_id, image_base)) = self
                .base
                .slot_images
                .iter()
                .find(|(_, image)| image.gpu_addr == gpu_addr)
            else {
                if trace_enabled {
                    common::trace::emit_raw(
                        common::trace::cat::PRESENT_TEXTURE,
                        &[
                            present_index,
                            gpu_addr,
                            u64::MAX,
                            0,
                            0,
                            0,
                            0,
                            0,
                            0,
                            0,
                            0,
                            0,
                            0,
                            0,
                        ],
                    );
                }
                continue;
            };

            let Some(image) = self.images.get(&image_id) else {
                if trace_enabled {
                    common::trace::emit_raw(
                        common::trace::cat::PRESENT_TEXTURE,
                        &[
                            present_index,
                            gpu_addr,
                            image_id.index as u64,
                            0,
                            image_base.info.size.width as u64,
                            image_base.info.size.height as u64,
                            0,
                            0,
                            0,
                            0,
                            0,
                            0,
                            0,
                            0,
                        ],
                    );
                }
                continue;
            };

            unsafe {
                if trace_enabled {
                    trace_present_image_grid(
                        present_index,
                        gpu_addr,
                        image_id.index as u64,
                        image,
                        image_base,
                    );
                }
                dump_present_image_ppm(
                    present_index,
                    gpu_addr,
                    image_id.index as u64,
                    image,
                    image_base,
                );
                if common::trace::is_enabled(common::trace::cat::PRESENT_ALIAS) {
                    let width = image_base.info.size.width;
                    let height = image_base.info.size.height;
                    let current_texture = image.current_texture;
                    let (current_checksum, current_rgb_nonzero, current_first_rgba, _) =
                        sample_present_texture(current_texture, width, height);
                    let mut emitted = false;
                    for view_id in &image_base.image_view_ids {
                        let Some(view) = self.image_views.get(view_id) else {
                            continue;
                        };
                        let view_texture = view.default_handle();
                        let (view_checksum, view_rgb_nonzero, view_first_rgba, _) =
                            sample_present_texture(view_texture, width, height);
                        common::trace::emit_raw(
                            common::trace::cat::PRESENT_ALIAS,
                            &[
                                present_index,
                                gpu_addr,
                                image_id.index as u64,
                                view_id.index as u64,
                                current_texture as u64,
                                view_texture as u64,
                                width as u64,
                                height as u64,
                                current_checksum,
                                view_checksum,
                                current_rgb_nonzero,
                                view_rgb_nonzero,
                                current_first_rgba,
                                view_first_rgba,
                            ],
                        );
                        emitted = true;
                    }
                    if !emitted {
                        common::trace::emit_raw(
                            common::trace::cat::PRESENT_ALIAS,
                            &[
                                present_index,
                                gpu_addr,
                                image_id.index as u64,
                                u64::MAX,
                                current_texture as u64,
                                0,
                                width as u64,
                                height as u64,
                                current_checksum,
                                0,
                                current_rgb_nonzero,
                                0,
                                current_first_rgba,
                                0,
                            ],
                        );
                    }
                }
            }
        }
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
        let lookup_addr =
            if force_first_flinger && (rt.address == 0x502570000 || rt.address == 0x502DE0000) {
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
            .find_render_target_view_from_image(image_id, rt, 0, lookup_addr);
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

        self.images
            .entry(image_id)
            .or_insert_with(|| Image::from_base(&image_base, self.has_native_astc));
        let view_mismatch = {
            let backend_image = self
                .images
                .get(&image_id)
                .expect("image inserted above must be present");
            self.image_views
                .get(&view_id)
                .is_some_and(|view| !view.matches_base_image(&view_base, backend_image))
        };
        if view_mismatch {
            let backend_image = self
                .images
                .get(&image_id)
                .expect("image inserted above must be present");
            let old_view = self.image_views.get(&view_id);
            trace_image_view_event(7, view_id, &view_base, backend_image, old_view);
            self.image_views.remove(&view_id);
            self.remove_framebuffers_for_view(view_id);
        }
        let backend_image = self
            .images
            .get(&image_id)
            .expect("image inserted above must be present");
        let backend_view = self.image_views.entry(view_id).or_insert_with(|| {
            ImageView::new_color_2d(&view_base, backend_image, self.runtime.null_image_views)
        });
        trace_image_view_event(6, view_id, &view_base, backend_image, Some(backend_view));
        let attachment_texture = if view_base.flags.contains(ImageViewFlagBits::SLICE) {
            backend_view.handle_for_texture_type(TextureType::Color3D)
        } else {
            backend_view.default_handle()
        };
        if attachment_texture == 0 {
            return None;
        }

        let key = self.render_targets_key_for_image_view(view_id, &view_base);
        let framebuffer_id = if let Some(&framebuffer_id) = self.base.framebuffers.get(&key) {
            framebuffer_id
        } else {
            let framebuffer =
                self.create_color_framebuffer_for_view(key, &view_base, attachment_texture);
            let framebuffer_id = self.slot_framebuffers.insert(framebuffer);
            self.base.framebuffers.insert(key, framebuffer_id);
            framebuffer_id
        };
        let framebuffer = &self.slot_framebuffers[framebuffer_id];
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
        (handle != 0).then_some((handle, key.size.width, key.size.height))
    }
}

impl Default for TextureCache {
    /// Standalone default for tests / fallback paths. Production
    /// construction goes through `RasterizerOpenGL::new`, which threads
    /// the shared `Arc` from `Host1x::memory_manager()`.
    fn default() -> Self {
        let mut state_tracker = Box::new(StateTracker::new());
        let mut cache = Self::new_with_caps(
            std::sync::Arc::new(
                crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager::default(),
            ),
            true,
            false,
            false,
            false,
            ProgramManager::new_shared_with_caps(false, false),
            state_tracker.as_mut(),
        );
        cache.standalone_state_tracker = Some(state_tracker);
        cache
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::texture_cache::image_view_info::SwizzleSource;
    use std::sync::{Mutex, MutexGuard};

    static ASTC_SETTINGS_LOCK: Mutex<()> = Mutex::new(());

    fn lock_astc_settings() -> MutexGuard<'static, ()> {
        ASTC_SETTINGS_LOCK.lock().unwrap()
    }

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
    fn compressed_block_copy_rect_clamps_source_and_destination_mip_edges() {
        let rect = compressed_block_copy_rect(
            (4, 1, 48),
            (2, 2, 48),
            CopyOrigin {
                level: 0,
                x: 0,
                y: 0,
                z: 0,
            },
            CopyOrigin {
                level: 6,
                x: 0,
                y: 0,
                z: 0,
            },
            CopyRegion {
                width: 1,
                height: 1,
                depth: 1,
            },
            4,
            4,
            8,
        );
        assert_eq!(
            rect,
            Some(CompressedBlockCopyRect {
                read_width: 4,
                read_height: 1,
                write_width: 2,
                write_height: 2,
                buffer_size: 8,
            })
        );
    }

    #[test]
    fn compressed_block_copy_rect_uses_one_block_for_small_region() {
        let rect = compressed_block_copy_rect(
            (8, 8, 48),
            (4, 2, 1),
            CopyOrigin {
                level: 4,
                x: 0,
                y: 0,
                z: 6,
            },
            CopyOrigin {
                level: 0,
                x: 0,
                y: 0,
                z: 0,
            },
            CopyRegion {
                width: 2,
                height: 2,
                depth: 1,
            },
            4,
            4,
            8,
        );
        assert_eq!(
            rect,
            Some(CompressedBlockCopyRect {
                read_width: 4,
                read_height: 4,
                write_width: 4,
                write_height: 2,
                buffer_size: 8,
            })
        );
    }

    #[test]
    fn compressed_block_copy_rect_rejects_unaligned_origins() {
        assert_eq!(
            compressed_block_copy_rect(
                (8, 8, 1),
                (8, 8, 1),
                CopyOrigin {
                    level: 0,
                    x: 1,
                    y: 0,
                    z: 0,
                },
                CopyOrigin {
                    level: 0,
                    x: 0,
                    y: 0,
                    z: 0,
                },
                CopyRegion {
                    width: 2,
                    height: 2,
                    depth: 1,
                },
                4,
                4,
                8,
            ),
            None
        );
    }

    #[test]
    fn compressed_copy_fallback_accepts_same_layout_srgb_pairs_only() {
        assert_eq!(
            compressed_copy_fallback_format(PixelFormat::Bc1RgbaUnorm, PixelFormat::Bc1RgbaSrgb),
            Some((4, 4, 8))
        );
        assert_eq!(
            compressed_copy_fallback_format(PixelFormat::Bc3Unorm, PixelFormat::Bc3Srgb),
            Some((4, 4, 16))
        );
        assert_eq!(
            compressed_copy_fallback_format(PixelFormat::Bc3Unorm, PixelFormat::Bc5Unorm),
            None
        );
        assert_eq!(
            compressed_copy_fallback_format(
                PixelFormat::Astc2d8x5Unorm,
                PixelFormat::Astc2d8x5Srgb
            ),
            Some((8, 5, 16))
        );
        assert_eq!(
            compressed_copy_fallback_format(
                PixelFormat::Astc2d8x5Unorm,
                PixelFormat::Astc2d8x8Unorm
            ),
            None
        );
    }

    #[test]
    fn raw_compressed_copy_fallback_accepts_matching_block_bytes() {
        assert_eq!(
            raw_compressed_copy_fallback_format(
                PixelFormat::Bc3Unorm,
                PixelFormat::R32G32B32A32Uint
            ),
            Some(RawCompressedCopyFormat {
                direction: RawCompressedCopyDirection::RawToCompressed,
                compressed_format: PixelFormat::Bc3Unorm,
                raw_format: PixelFormat::R32G32B32A32Uint,
                block_width: 4,
                block_height: 4,
                bytes_per_block: 16,
            })
        );
        assert_eq!(
            raw_compressed_copy_fallback_format(
                PixelFormat::R32G32B32A32Uint,
                PixelFormat::Bc3Unorm
            ),
            Some(RawCompressedCopyFormat {
                direction: RawCompressedCopyDirection::CompressedToRaw,
                compressed_format: PixelFormat::Bc3Unorm,
                raw_format: PixelFormat::R32G32B32A32Uint,
                block_width: 4,
                block_height: 4,
                bytes_per_block: 16,
            })
        );
    }

    #[test]
    fn raw_compressed_copy_fallback_rejects_mismatched_block_bytes() {
        assert_eq!(
            raw_compressed_copy_fallback_format(
                PixelFormat::Bc1RgbaUnorm,
                PixelFormat::R32G32B32A32Uint
            ),
            None
        );
        assert_eq!(
            raw_compressed_copy_fallback_format(PixelFormat::Bc3Unorm, PixelFormat::A8B8G8R8Unorm),
            None
        );
        assert_eq!(
            raw_compressed_copy_fallback_format(PixelFormat::Bc3Unorm, PixelFormat::Bc3Srgb),
            None
        );
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

    #[test]
    fn select_astc_format_follows_recompression_setting() {
        use common::settings_enums::AstcRecompression;

        let _lock = lock_astc_settings();

        struct AstcRecompressionRestore(AstcRecompression);

        impl Drop for AstcRecompressionRestore {
            fn drop(&mut self) {
                common::settings::values_mut()
                    .astc_recompression
                    .set_value(self.0);
            }
        }

        let previous = *common::settings::values().astc_recompression.get_value();
        let _restore = AstcRecompressionRestore(previous);

        common::settings::values_mut()
            .astc_recompression
            .set_value(AstcRecompression::Uncompressed);
        assert_eq!(
            select_astc_format(PixelFormat::Astc2d4x4Unorm, false),
            gl::RGBA8
        );
        assert_eq!(
            select_astc_format(PixelFormat::Astc2d4x4Srgb, true),
            gl::SRGB8_ALPHA8
        );

        common::settings::values_mut()
            .astc_recompression
            .set_value(AstcRecompression::Bc1);
        assert_eq!(
            select_astc_format(PixelFormat::Astc2d4x4Unorm, false),
            GL_COMPRESSED_RGBA_S3TC_DXT1_EXT
        );
        assert_eq!(
            select_astc_format(PixelFormat::Astc2d4x4Srgb, true),
            GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT
        );

        common::settings::values_mut()
            .astc_recompression
            .set_value(AstcRecompression::Bc3);
        assert_eq!(
            select_astc_format(PixelFormat::Astc2d4x4Unorm, false),
            GL_COMPRESSED_RGBA_S3TC_DXT5_EXT
        );
        assert_eq!(
            select_astc_format(PixelFormat::Astc2d4x4Srgb, true),
            GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT
        );
    }

    #[test]
    fn astc_upload_flags_follow_upstream_policy() {
        use common::settings_enums::{AstcDecodeMode, AstcRecompression};

        let _lock = lock_astc_settings();

        struct AstcSettingsRestore {
            decode: AstcDecodeMode,
            recompression: AstcRecompression,
        }

        impl Drop for AstcSettingsRestore {
            fn drop(&mut self) {
                let mut values = common::settings::values_mut();
                values.accelerate_astc.set_value(self.decode);
                values.astc_recompression.set_value(self.recompression);
            }
        }

        let _restore = AstcSettingsRestore {
            decode: *common::settings::values().accelerate_astc.get_value(),
            recompression: *common::settings::values().astc_recompression.get_value(),
        };
        let info = ImageInfo {
            format: PixelFormat::Astc2d4x4Unorm,
            image_type: ImageType::E2D,
            size: crate::texture_cache::types::Extent3D {
                width: 64,
                height: 64,
                depth: 1,
            },
            ..ImageInfo::default()
        };

        {
            let mut values = common::settings::values_mut();
            values
                .accelerate_astc
                .set_value(AstcDecodeMode::CpuAsynchronous);
            values
                .astc_recompression
                .set_value(AstcRecompression::Uncompressed);
        }
        assert!(can_be_decoded_async(false, &info));
        assert!(!can_be_accelerated(false, &info));
        let mut async_image = ImageBase::new(info.clone(), 0, 0);
        TextureCache::apply_backend_image_flags_for_test(&mut async_image, false);
        assert!(async_image
            .flags
            .contains(ImageFlagBits::ASYNCHRONOUS_DECODE));
        assert!(!async_image
            .flags
            .contains(ImageFlagBits::ACCELERATED_UPLOAD));

        {
            let mut values = common::settings::values_mut();
            values.accelerate_astc.set_value(AstcDecodeMode::Gpu);
            values
                .astc_recompression
                .set_value(AstcRecompression::Uncompressed);
        }
        assert!(!can_be_decoded_async(false, &info));
        assert!(can_be_accelerated(false, &info));
        let mut accelerated_image = ImageBase::new(info.clone(), 0, 0);
        TextureCache::apply_backend_image_flags_for_test(&mut accelerated_image, false);
        assert!(!accelerated_image
            .flags
            .contains(ImageFlagBits::ASYNCHRONOUS_DECODE));
        assert!(accelerated_image
            .flags
            .contains(ImageFlagBits::ACCELERATED_UPLOAD));

        common::settings::values_mut()
            .astc_recompression
            .set_value(AstcRecompression::Bc1);
        assert!(!can_be_accelerated(false, &info));
        assert!(!can_be_decoded_async(true, &info));
        assert!(!can_be_accelerated(true, &info));
        let mut native_image = ImageBase::new(info, 0, 0);
        TextureCache::apply_backend_image_flags_for_test(&mut native_image, true);
        assert!(!native_image
            .flags
            .contains(ImageFlagBits::ASYNCHRONOUS_DECODE));
        assert!(!native_image
            .flags
            .contains(ImageFlagBits::ACCELERATED_UPLOAD));
    }

    #[test]
    fn decode_swizzle_matches_upstream_sources() {
        assert_eq!(
            decode_swizzle([
                SwizzleSource::R as u8,
                SwizzleSource::R as u8,
                SwizzleSource::R as u8,
                SwizzleSource::OneFloat as u8,
            ]),
            Some([
                SwizzleSource::R,
                SwizzleSource::R,
                SwizzleSource::R,
                SwizzleSource::OneFloat,
            ])
        );
        assert_eq!(decode_swizzle([u8::MAX; 4]), None);
    }

    #[test]
    fn image_view_parent_guard_rejects_stale_backend_view() {
        use crate::texture_cache::format_lookup_table::PixelFormat;
        use crate::texture_cache::image_info::ImageInfo;
        use crate::texture_cache::image_view_base::ImageViewBase;
        use crate::texture_cache::image_view_info::ImageViewInfo;
        use crate::texture_cache::types::ImageType;
        use common::slot_vector::SlotId;

        let image_id = SlotId { index: 42 };
        let image_info = ImageInfo {
            format: PixelFormat::A8B8G8R8Unorm,
            image_type: ImageType::E2D,
            ..ImageInfo::default()
        };
        let view_info = ImageViewInfo {
            format: PixelFormat::A8B8G8R8Unorm,
            ..ImageViewInfo::default()
        };
        let base = ImageViewBase::new(&view_info, &image_info, image_id, 0x1000);
        let mut image = Image::new();
        image.current_texture = 7;

        let mut view = ImageView::new();
        assert!(!view.matches_base_image(&base, &image));

        view.image_id = image_id;
        view.original_texture = 7;
        view.format = base.format;
        view.view_type = base.view_type;
        view.swizzle = base.swizzle;
        view.full_range = base.range;
        view.size = base.size;
        view.flags = base.flags;
        assert!(view.matches_base_image(&base, &image));

        let mut different_view_info = view_info;
        different_view_info.range.base.level = 1;
        let different_base =
            ImageViewBase::new(&different_view_info, &image_info, image_id, 0x1000);
        assert!(!view.matches_base_image(&different_base, &image));

        let mut different_view_info = view_info;
        different_view_info.x_source = SwizzleSource::B as u8;
        let different_base =
            ImageViewBase::new(&different_view_info, &image_info, image_id, 0x1000);
        assert!(!view.matches_base_image(&different_base, &image));

        image.current_texture = 8;
        assert!(!view.matches_base_image(&base, &image));
    }
}
