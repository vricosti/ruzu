// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/texture_cache/util.h and util.cpp
//!
//! Utility functions for the texture cache: size/offset calculations,
//! swizzle/unswizzle, copy generation, subresource lookup, and more.
//!
//! util.cpp is ~1 500 lines of dense GPU-texture math.  Method signatures
//! and constant definitions are ported in full; complex bodies are stubbed
//! with `todo!()` and will be filled as dependent types are completed.

use super::format_lookup_table::PixelFormat;
use super::image_base::{GPUVAddr, ImageBase, VAddr};
use super::image_info::ImageInfo;
use super::types::*;

// ── Type aliases matching upstream ─────────────────────────────────────

pub type LevelArray = [u32; MAX_MIP_LEVELS];

// ── OverlapResult ──────────────────────────────────────────────────────

/// Result of resolving an overlap between two images.
///
/// Port of `VideoCommon::OverlapResult`.
#[derive(Debug, Clone, Copy)]
pub struct OverlapResult {
    pub gpu_addr: GPUVAddr,
    pub cpu_addr: VAddr,
    pub resources: SubresourceExtent,
}

// ── Size / offset calculation stubs ────────────────────────────────────

/// Port of `CalculateGuestSizeInBytes`.
pub fn calculate_guest_size_in_bytes(_info: &ImageInfo) -> u32 {
    todo!("calculate_guest_size_in_bytes")
}

/// Port of `CalculateUnswizzledSizeBytes`.
pub fn calculate_unswizzled_size_bytes(_info: &ImageInfo) -> u32 {
    todo!("calculate_unswizzled_size_bytes")
}

/// Port of `CalculateConvertedSizeBytes`.
pub fn calculate_converted_size_bytes(_info: &ImageInfo) -> u32 {
    todo!("calculate_converted_size_bytes")
}

/// Port of `CalculateLayerStride`.
pub fn calculate_layer_stride(_info: &ImageInfo) -> u32 {
    todo!("calculate_layer_stride")
}

/// Port of `CalculateLayerSize`.
pub fn calculate_layer_size(_info: &ImageInfo) -> u32 {
    todo!("calculate_layer_size")
}

/// Port of `CalculateMipLevelOffsets`.
pub fn calculate_mip_level_offsets(_info: &ImageInfo) -> LevelArray {
    todo!("calculate_mip_level_offsets")
}

/// Port of `CalculateMipLevelSizes`.
pub fn calculate_mip_level_sizes(_info: &ImageInfo) -> LevelArray {
    todo!("calculate_mip_level_sizes")
}

/// Port of `CalculateSliceOffsets`.
pub fn calculate_slice_offsets(_info: &ImageInfo) -> Vec<u32> {
    todo!("calculate_slice_offsets")
}

/// Port of `CalculateSliceSubresources`.
pub fn calculate_slice_subresources(_info: &ImageInfo) -> Vec<SubresourceBase> {
    todo!("calculate_slice_subresources")
}

/// Port of `CalculateLevelStrideAlignment`.
pub fn calculate_level_stride_alignment(_info: &ImageInfo, _level: u32) -> u32 {
    todo!("calculate_level_stride_alignment")
}

// ── Format helpers ─────────────────────────────────────────────────────

/// Port of `PixelFormatFromTIC`.
pub fn pixel_format_from_tic(_config: &()) -> PixelFormat {
    todo!("pixel_format_from_tic — needs TICEntry port")
}

/// Port of `RenderTargetImageViewType`.
pub fn render_target_image_view_type(_info: &ImageInfo) -> ImageViewType {
    todo!("render_target_image_view_type")
}

// ── Copy generation ────────────────────────────────────────────────────

/// Port of `MakeShrinkImageCopies`.
pub fn make_shrink_image_copies(
    _dst: &ImageInfo,
    _src: &ImageInfo,
    _base: SubresourceBase,
    _up_scale: u32,
    _down_shift: u32,
) -> Vec<ImageCopy> {
    todo!("make_shrink_image_copies")
}

/// Port of `MakeReinterpretImageCopies`.
pub fn make_reinterpret_image_copies(
    _src: &ImageInfo,
    _up_scale: u32,
    _down_shift: u32,
) -> Vec<ImageCopy> {
    todo!("make_reinterpret_image_copies")
}

// ── Validation ─────────────────────────────────────────────────────────

/// Port of `IsValidEntry`.
pub fn is_valid_entry(_gpu_memory: &(), _config: &()) -> bool {
    todo!("is_valid_entry — needs MemoryManager & TICEntry")
}

// ── Swizzle / unswizzle ────────────────────────────────────────────────

/// Port of `UnswizzleImage`.
pub fn unswizzle_image(
    _gpu_memory: &(),
    _gpu_addr: GPUVAddr,
    _info: &ImageInfo,
    _input: &[u8],
    _output: &mut [u8],
) -> Vec<BufferImageCopy> {
    todo!("unswizzle_image")
}

/// Port of `ConvertImage`.
pub fn convert_image(
    _input: &[u8],
    _info: &ImageInfo,
    _output: &mut [u8],
    _copies: &mut [BufferImageCopy],
) {
    todo!("convert_image")
}

/// Port of `FullDownloadCopies`.
pub fn full_download_copies(_info: &ImageInfo) -> Vec<BufferImageCopy> {
    todo!("full_download_copies")
}

/// Port of `FullUploadSwizzles`.
pub fn full_upload_swizzles(_info: &ImageInfo) -> Vec<SwizzleParameters> {
    todo!("full_upload_swizzles")
}

/// Port of `SwizzleImage`.
pub fn swizzle_image(
    _gpu_memory: &(),
    _gpu_addr: GPUVAddr,
    _info: &ImageInfo,
    _copies: &[BufferImageCopy],
    _memory: &[u8],
    _tmp_buffer: &mut Vec<u8>,
) {
    todo!("swizzle_image")
}

// ── Mip helpers ────────────────────────────────────────────────────────

/// Compute the size at a given mip level.
///
/// Port of `MipSize`.
pub fn mip_size(size: Extent3D, level: u32) -> Extent3D {
    Extent3D {
        width: (size.width >> level).max(1),
        height: (size.height >> level).max(1),
        depth: (size.depth >> level).max(1),
    }
}

/// Compute the block size at a given mip level.
///
/// Port of `MipBlockSize`.
pub fn mip_block_size(_info: &ImageInfo, _level: u32) -> Extent3D {
    todo!("mip_block_size")
}

// ── Compatibility checks ───────────────────────────────────────────────

/// Port of `IsBlockLinearSizeCompatible`.
pub fn is_block_linear_size_compatible(
    _new_info: &ImageInfo,
    _overlap_info: &ImageInfo,
    _new_level: u32,
    _overlap_level: u32,
    _strict_size: bool,
) -> bool {
    todo!("is_block_linear_size_compatible")
}

/// Port of `IsPitchLinearSameSize`.
pub fn is_pitch_linear_same_size(_lhs: &ImageInfo, _rhs: &ImageInfo, _strict_size: bool) -> bool {
    todo!("is_pitch_linear_same_size")
}

/// Port of `IsBlockLinearSizeCompatibleBPPRelaxed`.
pub fn is_block_linear_size_compatible_bpp_relaxed(
    _lhs: &ImageInfo,
    _rhs: &ImageInfo,
    _lhs_level: u32,
    _rhs_level: u32,
) -> bool {
    todo!("is_block_linear_size_compatible_bpp_relaxed")
}

/// Port of `ResolveOverlap`.
pub fn resolve_overlap(
    _new_info: &ImageInfo,
    _gpu_addr: GPUVAddr,
    _cpu_addr: VAddr,
    _overlap: &ImageBase,
    _strict_size: bool,
    _broken_views: bool,
    _native_bgr: bool,
) -> Option<OverlapResult> {
    todo!("resolve_overlap")
}

/// Port of `IsLayerStrideCompatible`.
pub fn is_layer_stride_compatible(_lhs: &ImageInfo, _rhs: &ImageInfo) -> bool {
    todo!("is_layer_stride_compatible")
}

/// Port of `FindSubresource`.
pub fn find_subresource(
    _candidate: &ImageInfo,
    _image: &ImageBase,
    _candidate_addr: GPUVAddr,
    _options: RelaxedOptions,
    _broken_views: bool,
    _native_bgr: bool,
) -> Option<SubresourceBase> {
    todo!("find_subresource")
}

/// Port of `IsSubresource`.
pub fn is_subresource(
    _candidate: &ImageInfo,
    _image: &ImageBase,
    _candidate_addr: GPUVAddr,
    _options: RelaxedOptions,
    _broken_views: bool,
    _native_bgr: bool,
) -> bool {
    todo!("is_subresource")
}

/// Port of `IsSubCopy`.
pub fn is_sub_copy(_candidate: &ImageInfo, _image: &ImageBase, _candidate_addr: GPUVAddr) -> bool {
    todo!("is_sub_copy")
}

/// Port of `DeduceBlitImages`.
pub fn deduce_blit_images(
    _dst_info: &mut ImageInfo,
    _src_info: &mut ImageInfo,
    _dst: Option<&ImageBase>,
    _src: Option<&ImageBase>,
) {
    todo!("deduce_blit_images")
}

/// Port of `MapSizeBytes`.
pub fn map_size_bytes(_image: &ImageBase) -> u32 {
    todo!("map_size_bytes")
}
