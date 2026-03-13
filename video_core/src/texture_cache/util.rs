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

use crate::surface;
use crate::textures::decoders::{
    GOB_SIZE, GOB_SIZE_SHIFT, GOB_SIZE_X, GOB_SIZE_X_SHIFT, GOB_SIZE_Y, GOB_SIZE_Y_SHIFT,
    GOB_SIZE_Z, GOB_SIZE_Z_SHIFT,
};

// ── Alignment helpers ─────────────────────────────────────────────────

fn align_up_log2(value: u32, alignment_log2: u32) -> u32 {
    let mask = (1u32 << alignment_log2) - 1;
    (value + mask) & !mask
}

fn div_ceil(a: u32, b: u32) -> u32 {
    (a + b - 1) / b
}

fn div_ceil_log2(value: u32, shift: u32) -> u32 {
    let mask = (1u32 << shift) - 1;
    (value + mask) >> shift
}

fn align_up(value: u32, alignment: u32) -> u32 {
    if alignment == 0 {
        return value;
    }
    let mask = alignment - 1;
    (value + mask) & !mask
}

// ── Type aliases matching upstream ─────────────────────────────────────

pub type LevelArray = [u32; MAX_MIP_LEVELS];

// ── LevelInfo (internal) ──────────────────────────────────────────────

/// Internal level info struct for size calculations.
///
/// Port of anonymous `LevelInfo` from `util.cpp`.
struct LevelInfo {
    size: Extent3D,
    block: Extent3D,
    tile_size: Extent2D,
    bpp_log2: u32,
    tile_width_spacing: u32,
    num_levels: u32,
}

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

// ── Internal helper functions (port of anonymous namespace) ────────────

/// Port of `AdjustTileSize(u32, u32, u32)`.
fn adjust_tile_size_scalar(shift: u32, unit_factor: u32, dimension: u32) -> u32 {
    if shift == 0 {
        return 0;
    }
    let mut s = shift;
    let mut x = unit_factor << (s - 1);
    if x >= dimension {
        while s > 0 {
            s -= 1;
            x >>= 1;
            if x < dimension {
                break;
            }
        }
    }
    s
}

/// Port of `AdjustMipSize(u32, u32)`.
fn adjust_mip_size(size: u32, level: u32) -> u32 {
    (size >> level).max(1)
}

/// Port of `AdjustMipBlockSize<GOB_EXTENT>(num_tiles, block_size, level)`.
fn adjust_mip_block_size_impl(gob_extent: u32, num_tiles: u32, mut block_size: u32, mut level: u32) -> u32 {
    loop {
        while block_size > 0 && num_tiles <= (1u32 << (block_size - 1)) * gob_extent {
            block_size -= 1;
        }
        if level == 0 {
            break;
        }
        level -= 1;
    }
    block_size
}

/// Port of `AdjustMipSize(Extent3D, s32)` — 3D overload.
fn adjust_mip_size_3d(size: Extent3D, level: u32) -> Extent3D {
    Extent3D {
        width: adjust_mip_size(size.width, level),
        height: adjust_mip_size(size.height, level),
        depth: adjust_mip_size(size.depth, level),
    }
}

/// Port of `AdjustSamplesSize`.
fn adjust_samples_size(size: Extent3D, num_samples: u32) -> Extent3D {
    let (samples_x, samples_y) = super::samples_helper::samples_log2(num_samples as i32);
    Extent3D {
        width: size.width >> samples_x as u32,
        height: size.height >> samples_y as u32,
        depth: size.depth,
    }
}

/// Port of `AdjustTileSize(Extent3D, Extent2D)`.
fn adjust_tile_size_3d(size: Extent3D, tile_size: Extent2D) -> Extent3D {
    Extent3D {
        width: div_ceil(size.width, tile_size.width),
        height: div_ceil(size.height, tile_size.height),
        depth: size.depth,
    }
}

/// Port of `AdjustMipBlockSize(Extent3D, Extent3D, u32, u32)`.
fn adjust_mip_block_size_3d(num_tiles: Extent3D, block_size: Extent3D, level: u32, num_levels: u32) -> Extent3D {
    Extent3D {
        width: adjust_mip_block_size_impl(GOB_SIZE_X, num_tiles.width, block_size.width, level),
        height: adjust_mip_block_size_impl(GOB_SIZE_Y, num_tiles.height, block_size.height, level),
        depth: if level == 0 && num_levels == 1 {
            block_size.depth
        } else {
            adjust_mip_block_size_impl(GOB_SIZE_Z, num_tiles.depth, block_size.depth, level)
        },
    }
}

/// Port of `StrideAlignment(Extent3D, Extent3D, Extent2D, u32)`.
fn stride_alignment_gob(num_tiles: Extent3D, block: Extent3D, gob: Extent2D, bpp_log2: u32) -> u32 {
    if is_smaller_than_gob_size(num_tiles, gob, block.depth) {
        GOB_SIZE_X_SHIFT - bpp_log2
    } else {
        gob.width
    }
}

/// Port of `StrideAlignment(Extent3D, Extent3D, u32, u32)`.
fn stride_alignment(num_tiles: Extent3D, block: Extent3D, bpp_log2: u32, tile_width_spacing: u32) -> u32 {
    let g = gob_size(bpp_log2, block.height, tile_width_spacing);
    stride_alignment_gob(num_tiles, block, g, bpp_log2)
}

/// Port of `PitchLinearAlignedSize`.
fn pitch_linear_aligned_size(info: &ImageInfo) -> Extent2D {
    const STRIDE_ALIGNMENT: u32 = 32;
    debug_assert!(info.image_type == ImageType::Linear);
    let num_tiles = Extent2D {
        width: div_ceil(info.size.width, surface::default_block_width(info.format)),
        height: div_ceil(info.size.height, surface::default_block_height(info.format)),
    };
    let width_alignment = STRIDE_ALIGNMENT / surface::bytes_per_block(info.format);
    Extent2D {
        width: align_up(num_tiles.width, width_alignment),
        height: num_tiles.height,
    }
}

/// Port of `BlockLinearAlignedSize`.
fn block_linear_aligned_size(info: &ImageInfo, level: u32) -> Extent3D {
    debug_assert!(info.image_type != ImageType::Linear);
    let size = adjust_mip_size_3d(info.size, level);
    let num_tiles = Extent3D {
        width: div_ceil(size.width, surface::default_block_width(info.format)),
        height: div_ceil(size.height, surface::default_block_height(info.format)),
        depth: size.depth,
    };
    let bpp_log2 = bytes_per_block_log2_format(info.format);
    let alignment = stride_alignment(num_tiles, info.block(), bpp_log2, info.tile_width_spacing);
    let mip_block = adjust_mip_block_size_3d(num_tiles, info.block(), 0, info.resources.levels as u32);
    Extent3D {
        width: align_up_log2(num_tiles.width, alignment),
        height: align_up_log2(num_tiles.height, GOB_SIZE_Y_SHIFT + mip_block.height),
        depth: align_up_log2(num_tiles.depth, GOB_SIZE_Z_SHIFT + mip_block.depth),
    }
}

/// Port of `BytesPerBlockLog2(u32)`.
fn bytes_per_block_log2(bytes_per_block: u32) -> u32 {
    bytes_per_block.leading_zeros() ^ 0x1F
}

/// Port of `BytesPerBlockLog2(PixelFormat)`.
fn bytes_per_block_log2_format(format: PixelFormat) -> u32 {
    bytes_per_block_log2(surface::bytes_per_block(format))
}

/// Port of `DefaultBlockSize(PixelFormat)`.
fn default_block_size(format: PixelFormat) -> Extent2D {
    Extent2D {
        width: surface::default_block_width(format),
        height: surface::default_block_height(format),
    }
}

/// Port of `NumLevelBlocks(info, level)`.
fn num_level_blocks(info: &LevelInfo, level: u32) -> Extent3D {
    Extent3D {
        width: div_ceil(adjust_mip_size(info.size.width, level), info.tile_size.width) << info.bpp_log2,
        height: div_ceil(adjust_mip_size(info.size.height, level), info.tile_size.height),
        depth: adjust_mip_size(info.size.depth, level),
    }
}

/// Port of `TileShift(info, level)`.
fn tile_shift(info: &LevelInfo, level: u32) -> Extent3D {
    if level == 0 && info.num_levels == 1 {
        return info.block;
    }
    let blocks = num_level_blocks(info, level);
    Extent3D {
        width: adjust_tile_size_scalar(info.block.width, GOB_SIZE_X, blocks.width),
        height: adjust_tile_size_scalar(info.block.height, GOB_SIZE_Y, blocks.height),
        depth: adjust_tile_size_scalar(info.block.depth, GOB_SIZE_Z, blocks.depth),
    }
}

/// Port of `GobSize(bpp_log2, block_height, tile_width_spacing)`.
fn gob_size(bpp_log2: u32, block_height: u32, tile_width_spacing: u32) -> Extent2D {
    Extent2D {
        width: GOB_SIZE_X_SHIFT - bpp_log2 + tile_width_spacing,
        height: GOB_SIZE_Y_SHIFT + block_height,
    }
}

/// Port of `IsSmallerThanGobSize`.
fn is_smaller_than_gob_size(num_tiles: Extent3D, gob: Extent2D, block_depth: u32) -> bool {
    num_tiles.width <= (1u32 << gob.width)
        || num_tiles.height <= (1u32 << gob.height)
        || num_tiles.depth < (1u32 << block_depth)
}

/// Port of `NumGobs(info, level)`.
fn num_gobs(info: &LevelInfo, level: u32) -> Extent2D {
    let blocks = num_level_blocks(info, level);
    let gobs = Extent2D {
        width: div_ceil_log2(blocks.width, GOB_SIZE_X_SHIFT),
        height: div_ceil_log2(blocks.height, GOB_SIZE_Y_SHIFT),
    };
    let gob = gob_size(info.bpp_log2, info.block.height, info.tile_width_spacing);
    let is_small = is_smaller_than_gob_size(blocks, gob, info.block.depth);
    let alignment = if is_small { 0 } else { info.tile_width_spacing };
    Extent2D {
        width: align_up_log2(gobs.width, alignment),
        height: gobs.height,
    }
}

/// Port of `LevelTiles(info, level)`.
fn level_tiles(info: &LevelInfo, level: u32) -> Extent3D {
    let blocks = num_level_blocks(info, level);
    let ts = tile_shift(info, level);
    let gobs = num_gobs(info, level);
    Extent3D {
        width: div_ceil_log2(gobs.width, ts.width),
        height: div_ceil_log2(gobs.height, ts.height),
        depth: div_ceil_log2(blocks.depth, ts.depth),
    }
}

/// Port of `CalculateLevelSize(info, level)`.
fn calculate_level_size(info: &LevelInfo, level: u32) -> u32 {
    let ts = tile_shift(info, level);
    let tiles = level_tiles(info, level);
    let num_tiles = tiles.width * tiles.height * tiles.depth;
    let shift = GOB_SIZE_SHIFT + ts.width + ts.height + ts.depth;
    num_tiles << shift
}

/// Port of `CalculateLevelSizes(info, num_levels)`.
fn calculate_level_sizes(info: &LevelInfo, num_levels: u32) -> LevelArray {
    assert!((num_levels as usize) <= MAX_MIP_LEVELS);
    let mut sizes = [0u32; MAX_MIP_LEVELS];
    for level in 0..num_levels {
        sizes[level as usize] = calculate_level_size(info, level);
    }
    sizes
}

/// Port of `CalculateLevelBytes(sizes, num_levels)`.
fn calculate_level_bytes(sizes: &LevelArray, num_levels: u32) -> u32 {
    sizes[..num_levels as usize].iter().sum()
}

/// Port of `MakeLevelInfo(format, size, block, tile_width_spacing, num_levels)`.
fn make_level_info(
    format: PixelFormat,
    size: Extent3D,
    block: Extent3D,
    tile_width_spacing: u32,
    num_levels: u32,
) -> LevelInfo {
    let bpb = surface::bytes_per_block(format);
    LevelInfo {
        size,
        block,
        tile_size: default_block_size(format),
        bpp_log2: bytes_per_block_log2(bpb),
        tile_width_spacing,
        num_levels,
    }
}

/// Port of `MakeLevelInfo(const ImageInfo&)`.
fn make_level_info_from_image(info: &ImageInfo) -> LevelInfo {
    make_level_info(
        info.format,
        info.size,
        info.block(),
        info.tile_width_spacing,
        info.resources.levels as u32 as u32,
    )
}

/// Port of `AlignLayerSize`.
fn align_layer_size(
    size_bytes: u32,
    size: Extent3D,
    mut block: Extent3D,
    tile_size_y: u32,
    tile_width_spacing: u32,
) -> u32 {
    if tile_width_spacing > 0 {
        let alignment_log2 = GOB_SIZE_SHIFT + tile_width_spacing + block.height + block.depth;
        return align_up_log2(size_bytes, alignment_log2);
    }
    let aligned_height = align_up(size.height, tile_size_y);
    while block.height != 0 && aligned_height <= (1u32 << (block.height - 1)) * GOB_SIZE_Y {
        block.height -= 1;
    }
    while block.depth != 0 && size.depth <= (1u32 << (block.depth - 1)) {
        block.depth -= 1;
    }
    let block_shift = GOB_SIZE_SHIFT + block.height + block.depth;
    let num_blocks = size_bytes >> block_shift;
    if size_bytes != num_blocks << block_shift {
        (num_blocks + 1) << block_shift
    } else {
        size_bytes
    }
}

// ── Size / offset calculation ─────────────────────────────────────────

/// Port of `CalculateGuestSizeInBytes`.
pub fn calculate_guest_size_in_bytes(info: &ImageInfo) -> u32 {
    if info.image_type == ImageType::Linear {
        return info.pitch() * info.size.height;
    }
    if info.image_type == ImageType::Buffer {
        return surface::bytes_per_block(info.format) * info.size.width;
    }
    let level_info = make_level_info_from_image(info);
    let sizes = calculate_level_sizes(&level_info, info.resources.levels as u32);
    let level_bytes = calculate_level_bytes(&sizes, info.resources.levels as u32);
    let layer_size = align_layer_size(
        level_bytes,
        info.size,
        info.block(),
        surface::default_block_height(info.format),
        info.tile_width_spacing,
    );
    layer_size * info.resources.layers as u32
}

/// Port of `CalculateUnswizzledSizeBytes`.
pub fn calculate_unswizzled_size_bytes(info: &ImageInfo) -> u32 {
    if info.image_type == ImageType::Linear {
        return info.pitch() * info.size.height;
    }
    if info.image_type == ImageType::Buffer {
        return surface::bytes_per_block(info.format) * info.size.width;
    }
    let bpb = surface::bytes_per_block(info.format);
    let tile = default_block_size(info.format);
    let mut total: u32 = 0;
    for level in 0..info.resources.levels as u32 {
        let w = div_ceil(adjust_mip_size(info.size.width, level), tile.width);
        let h = div_ceil(adjust_mip_size(info.size.height, level), tile.height);
        let d = adjust_mip_size(info.size.depth, level);
        total += w * h * d * bpb;
    }
    total * info.resources.layers as u32
}

/// Port of `CalculateConvertedSizeBytes`.
pub fn calculate_converted_size_bytes(info: &ImageInfo) -> u32 {
    // For non-compressed or non-ASTC formats, same as unswizzled
    if !surface::is_pixel_format_astc(info.format) {
        return calculate_unswizzled_size_bytes(info);
    }
    // ASTC: decompress block -> RGBA8 (4 bytes per pixel)
    let tile = default_block_size(info.format);
    let mut total: u32 = 0;
    for level in 0..info.resources.levels as u32 {
        let w = adjust_mip_size(info.size.width, level);
        let h = adjust_mip_size(info.size.height, level);
        let d = adjust_mip_size(info.size.depth, level);
        // Each block covers tile.width * tile.height pixels
        let blocks_w = div_ceil(w, tile.width);
        let blocks_h = div_ceil(h, tile.height);
        total += blocks_w * tile.width * blocks_h * tile.height * d * 4;
    }
    total * info.resources.layers as u32
}

/// Port of `CalculateLayerStride`.
pub fn calculate_layer_stride(info: &ImageInfo) -> u32 {
    if info.image_type == ImageType::Linear {
        return info.pitch() * info.size.height;
    }
    let level_info = make_level_info_from_image(info);
    let sizes = calculate_level_sizes(&level_info, info.resources.levels as u32);
    let level_bytes = calculate_level_bytes(&sizes, info.resources.levels as u32);
    align_layer_size(
        level_bytes,
        info.size,
        info.block(),
        surface::default_block_height(info.format),
        info.tile_width_spacing,
    )
}

/// Port of `CalculateLayerSize`.
pub fn calculate_layer_size(info: &ImageInfo) -> u32 {
    let level_info = make_level_info_from_image(info);
    let sizes = calculate_level_sizes(&level_info, info.resources.levels as u32);
    calculate_level_bytes(&sizes, info.resources.levels as u32)
}

/// Port of `CalculateMipLevelOffsets`.
pub fn calculate_mip_level_offsets(info: &ImageInfo) -> LevelArray {
    if info.image_type == ImageType::Linear {
        return [0u32; MAX_MIP_LEVELS];
    }
    let level_info = make_level_info_from_image(info);
    let sizes = calculate_level_sizes(&level_info, info.resources.levels as u32);
    let mut offsets = [0u32; MAX_MIP_LEVELS];
    let mut offset = 0u32;
    for level in 0..(info.resources.levels as u32 as usize) {
        offsets[level] = offset;
        offset += sizes[level];
    }
    offsets
}

/// Port of `CalculateMipLevelSizes`.
pub fn calculate_mip_level_sizes(info: &ImageInfo) -> LevelArray {
    if info.image_type == ImageType::Linear {
        let mut sizes = [0u32; MAX_MIP_LEVELS];
        sizes[0] = info.pitch() * info.size.height;
        return sizes;
    }
    let level_info = make_level_info_from_image(info);
    calculate_level_sizes(&level_info, info.resources.levels as u32)
}

/// Port of `CalculateSliceOffsets`.
pub fn calculate_slice_offsets(info: &ImageInfo) -> Vec<u32> {
    debug_assert!(info.image_type == ImageType::E3D);
    let level_info = make_level_info_from_image(info);
    let mut offsets = Vec::new();
    let mut mip_offset = 0u32;
    for level in 0..info.resources.levels as u32 {
        let ts = tile_shift(&level_info, level);
        let tiles = level_tiles(&level_info, level);
        let gob_size_shift = ts.height + GOB_SIZE_SHIFT;
        let slice_size = (tiles.width * tiles.height) << gob_size_shift;
        let z_mask = (1u32 << ts.depth) - 1;
        let depth = adjust_mip_size(info.size.depth, level);
        for slice in 0..depth {
            let z_low = slice & z_mask;
            let z_high = slice & !z_mask;
            offsets.push(mip_offset + (z_low << gob_size_shift) + (z_high * slice_size));
        }
        mip_offset += calculate_level_size(&level_info, level);
    }
    offsets
}

/// Port of `CalculateSliceSubresources`.
pub fn calculate_slice_subresources(info: &ImageInfo) -> Vec<SubresourceBase> {
    debug_assert!(info.image_type == ImageType::E3D);
    let mut subresources = Vec::new();
    for level in 0..info.resources.levels {
        let depth = adjust_mip_size(info.size.depth, level as u32) as i32;
        for slice in 0..depth {
            subresources.push(SubresourceBase {
                level,
                layer: slice,
            });
        }
    }
    subresources
}

/// Port of `CalculateLevelStrideAlignment`.
pub fn calculate_level_stride_alignment(info: &ImageInfo, level: u32) -> u32 {
    if info.image_type == ImageType::Linear {
        return 0;
    }
    let tile_size = default_block_size(info.format);
    let level_size = adjust_mip_size_3d(info.size, level);
    let num_tiles = adjust_tile_size_3d(level_size, tile_size);
    let block = adjust_mip_block_size_3d(
        num_tiles,
        info.block(),
        level,
        info.resources.levels as u32,
    );
    let bpp_log2 = bytes_per_block_log2_format(info.format);
    stride_alignment(num_tiles, block, bpp_log2, info.tile_width_spacing)
}

// ── Format helpers ─────────────────────────────────────────────────────

/// Port of `PixelFormatFromTIC`.
///
/// Full implementation requires `Tegra::Texture::TICEntry`, which is not yet
/// ported.  Returns `PixelFormat::Invalid` and logs a warning.
pub fn pixel_format_from_tic(_config: &()) -> PixelFormat {
    log::warn!("pixel_format_from_tic: TICEntry not yet ported — returning Invalid");
    PixelFormat::Invalid
}

/// Port of `RenderTargetImageViewType`.
pub fn render_target_image_view_type(info: &ImageInfo) -> ImageViewType {
    match info.image_type {
        ImageType::E2D => {
            if info.resources.layers > 1 {
                ImageViewType::E2DArray
            } else {
                ImageViewType::E2D
            }
        }
        ImageType::E3D => ImageViewType::E2DArray,
        ImageType::Linear => ImageViewType::E2D,
        _ => {
            log::error!("Unimplemented image type {:?}", info.image_type);
            ImageViewType::E2D
        }
    }
}

// ── Copy generation ────────────────────────────────────────────────────

/// Port of `MakeShrinkImageCopies`.
pub fn make_shrink_image_copies(
    dst: &ImageInfo,
    src: &ImageInfo,
    base: SubresourceBase,
    up_scale: u32,
    down_shift: u32,
) -> Vec<ImageCopy> {
    debug_assert!(dst.resources.levels >= src.resources.levels);
    let is_dst_3d = dst.image_type == ImageType::E3D;
    if is_dst_3d {
        debug_assert!(src.image_type == ImageType::E3D);
        debug_assert!(src.resources.levels == 1);
    }
    let both_2d = src.image_type == ImageType::E2D && dst.image_type == ImageType::E2D;
    let mut copies = Vec::with_capacity(src.resources.levels as usize);
    for level in 0..src.resources.levels {
        let src_subresource = SubresourceLayers {
            base_level: level,
            base_layer: 0,
            num_layers: src.resources.layers,
        };
        let dst_subresource = SubresourceLayers {
            base_level: base.level + level,
            base_layer: if is_dst_3d { 0 } else { base.layer },
            num_layers: if is_dst_3d { 1 } else { src.resources.layers },
        };
        let src_offset = Offset3D { x: 0, y: 0, z: 0 };
        let dst_offset = Offset3D {
            x: 0,
            y: 0,
            z: if is_dst_3d { base.layer } else { 0 },
        };
        let mip = adjust_mip_size_3d(dst.size, (base.level + level) as u32);
        let mut extent = adjust_samples_size(mip, dst.num_samples);
        if is_dst_3d {
            extent.depth = src.size.depth;
        }
        extent.width = ((extent.width * up_scale) >> down_shift).max(1);
        if both_2d {
            extent.height = ((extent.height * up_scale) >> down_shift).max(1);
        }
        copies.push(ImageCopy {
            src_subresource,
            dst_subresource,
            src_offset,
            dst_offset,
            extent,
        });
    }
    copies
}

/// Port of `MakeReinterpretImageCopies`.
pub fn make_reinterpret_image_copies(
    src: &ImageInfo,
    up_scale: u32,
    down_shift: u32,
) -> Vec<ImageCopy> {
    let is_3d = src.image_type == ImageType::E3D;
    let mut copies = Vec::with_capacity(src.resources.levels as usize);
    for level in 0..src.resources.levels {
        let subresource = SubresourceLayers {
            base_level: level,
            base_layer: 0,
            num_layers: src.resources.layers,
        };
        let offset = Offset3D { x: 0, y: 0, z: 0 };
        let mip = adjust_mip_size_3d(src.size, level as u32);
        let mut extent = adjust_samples_size(mip, src.num_samples);
        if is_3d {
            extent.depth = src.size.depth;
        }
        extent.width = ((extent.width * up_scale) >> down_shift).max(1);
        extent.height = ((extent.height * up_scale) >> down_shift).max(1);
        copies.push(ImageCopy {
            src_subresource: subresource,
            dst_subresource: subresource,
            src_offset: offset,
            dst_offset: offset,
            extent,
        });
    }
    copies
}

// ── Validation ─────────────────────────────────────────────────────────

/// Port of `IsValidEntry`.
///
/// Upstream reads the GPU virtual address from a `TICEntry` and checks whether
/// it maps to a valid physical address via `Tegra::MemoryManager`.  Both types
/// are not yet ported; returns `false` and logs a warning.
pub fn is_valid_entry(_gpu_memory: &(), _config: &()) -> bool {
    log::warn!("is_valid_entry: MemoryManager / TICEntry not yet ported — returning false");
    false
}

// ── Swizzle / unswizzle ────────────────────────────────────────────────

/// Port of `UnswizzleImage`.
///
/// Upstream reads swizzled texture data from GPU memory via `Tegra::MemoryManager`
/// and writes the unswizzled result.  The memory manager is not yet ported;
/// returns an empty copy list and logs a warning.
pub fn unswizzle_image(
    _gpu_memory: &(),
    _gpu_addr: GPUVAddr,
    _info: &ImageInfo,
    _input: &[u8],
    _output: &mut [u8],
) -> Vec<BufferImageCopy> {
    log::warn!("unswizzle_image: MemoryManager not yet ported — returning empty copy list");
    Vec::new()
}

/// Port of `ConvertImage`.
///
/// Decompresses ASTC or BCn compressed images into uncompressed output.
/// Currently implements the ASTC uncompressed decompression path.
/// BCn decompression and ASTC-to-BCn recompression are stubbed pending
/// a BCn codec crate and Settings system integration.
pub fn convert_image(
    input: &[u8],
    info: &ImageInfo,
    output: &mut [u8],
    copies: &mut [BufferImageCopy],
) {
    let mut output_offset = 0u32;
    let tile_size = default_block_size(info.format);

    for copy in copies.iter_mut() {
        let level = copy.image_subresource.base_level;
        let _mip_size = adjust_mip_size_3d(info.size, level as u32);

        let input_offset = copy.buffer_offset;
        copy.buffer_offset = output_offset as usize;

        let astc = surface::is_pixel_format_astc(
            // SAFETY: info.format is a valid PixelFormat discriminant from the texture cache
            unsafe { std::mem::transmute::<u32, surface::PixelFormat>(info.format as u32) },
        );

        if astc {
            // ASTC uncompressed decompression path.
            // Note: The upstream also has ASTC-to-BC1/BC3 recompression paths
            // gated behind Settings::values.astc_recompression. Those paths
            // are not yet implemented (requires Settings system and BCN compressor).
            let input_slice = &input[input_offset..];
            let depth_layers = copy.image_subresource.num_layers as u32
                * copy.image_extent.depth;
            crate::textures::astc::decompress(
                input_slice,
                copy.image_extent.width,
                copy.image_extent.height,
                depth_layers,
                tile_size.width,
                tile_size.height,
                &mut output[output_offset as usize..],
            );

            output_offset += copy.image_extent.width
                * copy.image_extent.height
                * copy.image_subresource.num_layers as u32
                * surface::bytes_per_block(surface::PixelFormat::A8B8G8R8Unorm);
        } else {
            // BCn decompression path
            // Stubbed: requires BCn decoder crate (equivalent of bc_decoder.h)
            log::warn!(
                "ConvertImage: BCn decompression not yet implemented for format {:?}",
                info.format
            );
            let bytes = copy.image_extent.width
                * copy.image_extent.height
                * copy.image_subresource.num_layers as u32
                * crate::texture_cache::decode_bc::converted_bytes_per_block(info.format);
            // Zero-fill the output for unsupported formats
            let end = (output_offset + bytes) as usize;
            if end <= output.len() {
                output[output_offset as usize..end].fill(0);
            }
            output_offset += bytes;
        }

        copy.buffer_row_length = _mip_size.width;
        copy.buffer_image_height = _mip_size.height;
    }
}

/// Port of `FullDownloadCopies`.
pub fn full_download_copies(info: &ImageInfo) -> Vec<BufferImageCopy> {
    let size = info.size;
    let bpb = surface::bytes_per_block(info.format);
    if info.image_type == ImageType::Linear {
        debug_assert!(info.pitch() % bpb == 0);
        return vec![BufferImageCopy {
            buffer_offset: 0,
            buffer_size: (info.pitch() * size.height) as usize,
            buffer_row_length: info.pitch() / bpb,
            buffer_image_height: size.height,
            image_subresource: SubresourceLayers {
                base_level: 0,
                base_layer: 0,
                num_layers: 1,
            },
            image_offset: Offset3D { x: 0, y: 0, z: 0 },
            image_extent: size,
        }];
    }
    if info.tile_width_spacing > 0 {
        log::warn!("FullDownloadCopies: tile_width_spacing > 0 not fully implemented");
    }
    let num_layers = info.resources.layers;
    let num_levels = info.resources.levels;
    let tile_size = default_block_size(info.format);
    let mut host_offset = 0u32;
    let mut copies = Vec::with_capacity(num_levels as usize);
    for level in 0..num_levels {
        let level_size = adjust_mip_size_3d(size, level as u32);
        let adj = adjust_tile_size_3d(level_size, tile_size);
        let num_blocks_per_layer = adj.width * adj.height * adj.depth;
        let host_bytes_per_level = num_blocks_per_layer * bpb * num_layers as u32;
        copies.push(BufferImageCopy {
            buffer_offset: host_offset as usize,
            buffer_size: host_bytes_per_level as usize,
            buffer_row_length: level_size.width,
            buffer_image_height: level_size.height,
            image_subresource: SubresourceLayers {
                base_level: level,
                base_layer: 0,
                num_layers: info.resources.layers,
            },
            image_offset: Offset3D { x: 0, y: 0, z: 0 },
            image_extent: level_size,
        });
        host_offset += host_bytes_per_level;
    }
    copies
}

/// Port of `FullUploadSwizzles`.
pub fn full_upload_swizzles(info: &ImageInfo) -> Vec<SwizzleParameters> {
    let tile_size = default_block_size(info.format);
    if info.image_type == ImageType::Linear {
        return vec![SwizzleParameters {
            num_tiles: adjust_tile_size_3d(info.size, tile_size),
            block: Extent3D {
                width: 0,
                height: 0,
                depth: 0,
            },
            buffer_offset: 0,
            level: 0,
        }];
    }
    let level_info = make_level_info_from_image(info);
    let size = info.size;
    let num_levels = info.resources.levels;
    let mut guest_offset = 0u32;
    let mut params = Vec::with_capacity(num_levels as usize);
    for level in 0..num_levels {
        let level_size = adjust_mip_size_3d(size, level as u32);
        let num_tiles = adjust_tile_size_3d(level_size, tile_size);
        let block = adjust_mip_block_size_3d(
            num_tiles,
            level_info.block,
            level as u32,
            level_info.num_levels,
        );
        params.push(SwizzleParameters {
            num_tiles,
            block,
            buffer_offset: guest_offset as usize,
            level,
        });
        guest_offset += calculate_level_size(&level_info, level as u32);
    }
    params
}

/// Port of `SwizzleImage`.
///
/// Upstream writes swizzled texture data back to GPU memory via
/// `Tegra::MemoryManager`.  The memory manager is not yet ported; logs a
/// warning and returns without writing.
pub fn swizzle_image(
    _gpu_memory: &(),
    _gpu_addr: GPUVAddr,
    _info: &ImageInfo,
    _copies: &[BufferImageCopy],
    _memory: &[u8],
    _tmp_buffer: &mut Vec<u8>,
) {
    log::warn!("swizzle_image: MemoryManager not yet ported — no data written");
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
pub fn mip_block_size(info: &ImageInfo, level: u32) -> Extent3D {
    let level_info = make_level_info_from_image(info);
    let tile_size = default_block_size(info.format);
    let level_size = Extent3D {
        width: adjust_mip_size(info.size.width, level),
        height: adjust_mip_size(info.size.height, level),
        depth: adjust_mip_size(info.size.depth, level),
    };
    let num_tiles = Extent3D {
        width: div_ceil(level_size.width, tile_size.width),
        height: div_ceil(level_size.height, tile_size.height),
        depth: level_size.depth,
    };
    adjust_mip_block_size_3d(num_tiles, level_info.block, level, level_info.num_levels)
}

// ── Compatibility checks ───────────────────────────────────────────────

/// Port of `IsBlockLinearSizeCompatible`.
pub fn is_block_linear_size_compatible(
    lhs: &ImageInfo,
    rhs: &ImageInfo,
    lhs_level: u32,
    rhs_level: u32,
    strict_size: bool,
) -> bool {
    debug_assert!(lhs.image_type != ImageType::Linear);
    debug_assert!(rhs.image_type != ImageType::Linear);
    if strict_size {
        let lhs_size = adjust_mip_size_3d(lhs.size, lhs_level);
        let rhs_size = adjust_mip_size_3d(rhs.size, rhs_level);
        lhs_size.width == rhs_size.width && lhs_size.height == rhs_size.height
    } else {
        let lhs_size = block_linear_aligned_size(lhs, lhs_level);
        let rhs_size = block_linear_aligned_size(rhs, rhs_level);
        lhs_size.width == rhs_size.width && lhs_size.height == rhs_size.height
    }
}

/// Port of `IsPitchLinearSameSize`.
pub fn is_pitch_linear_same_size(lhs: &ImageInfo, rhs: &ImageInfo, strict_size: bool) -> bool {
    debug_assert!(lhs.image_type == ImageType::Linear);
    debug_assert!(rhs.image_type == ImageType::Linear);
    if strict_size {
        lhs.size.width == rhs.size.width && lhs.size.height == rhs.size.height
    } else {
        pitch_linear_aligned_size(lhs) == pitch_linear_aligned_size(rhs)
    }
}

/// Port of `IsBlockLinearSizeCompatibleBPPRelaxed`.
pub fn is_block_linear_size_compatible_bpp_relaxed(
    lhs: &ImageInfo,
    rhs: &ImageInfo,
    lhs_level: u32,
    rhs_level: u32,
) -> bool {
    debug_assert!(lhs.image_type != ImageType::Linear);
    debug_assert!(rhs.image_type != ImageType::Linear);
    let lhs_bpp = surface::bytes_per_block(lhs.format);
    let rhs_bpp = surface::bytes_per_block(rhs.format);
    let lhs_size = adjust_mip_size_3d(lhs.size, lhs_level);
    let rhs_size = adjust_mip_size_3d(rhs.size, rhs_level);
    align_up_log2(lhs_size.width * lhs_bpp, GOB_SIZE_X_SHIFT)
        == align_up_log2(rhs_size.width * rhs_bpp, GOB_SIZE_X_SHIFT)
        && align_up_log2(lhs_size.height, GOB_SIZE_Y_SHIFT)
            == align_up_log2(rhs_size.height, GOB_SIZE_Y_SHIFT)
}

/// Port of `ResolveOverlapEqualAddress`.
fn resolve_overlap_equal_address(
    new_info: &ImageInfo,
    overlap: &ImageBase,
    strict_size: bool,
) -> Option<SubresourceExtent> {
    let info = &overlap.info;
    if !is_block_linear_size_compatible(new_info, info, 0, 0, strict_size) {
        return None;
    }
    if new_info.block() != info.block() {
        return None;
    }
    let resources = new_info.resources;
    Some(SubresourceExtent {
        levels: resources.levels.max(info.resources.levels),
        layers: resources.layers.max(info.resources.layers),
    })
}

/// Port of `ResolveOverlapRightAddress3D`.
fn resolve_overlap_right_address_3d(
    new_info: &ImageInfo,
    gpu_addr: GPUVAddr,
    overlap: &ImageBase,
    strict_size: bool,
) -> Option<SubresourceExtent> {
    let slice_offsets = calculate_slice_offsets(new_info);
    let diff = (overlap.gpu_addr - gpu_addr) as u32;
    let it = slice_offsets.iter().position(|&o| o == diff);
    let idx = it?;
    let subresources = calculate_slice_subresources(new_info);
    let base = subresources[idx];
    let info = &overlap.info;
    if !is_block_linear_size_compatible(new_info, info, base.level as u32, 0, strict_size) {
        return None;
    }
    let mip_depth = 1u32.max(new_info.size.depth >> base.level as u32);
    if mip_depth < info.size.depth + base.layer as u32 {
        return None;
    }
    if mip_block_size(new_info, base.level as u32) != info.block() {
        return None;
    }
    Some(SubresourceExtent {
        levels: new_info.resources.levels.max(info.resources.levels + base.level),
        layers: 1,
    })
}

/// Port of `ResolveOverlapRightAddress2D`.
fn resolve_overlap_right_address_2d(
    new_info: &ImageInfo,
    gpu_addr: GPUVAddr,
    overlap: &ImageBase,
    strict_size: bool,
) -> Option<SubresourceExtent> {
    let layer_stride = new_info.layer_stride as u64;
    let new_size = layer_stride * new_info.resources.layers as u64;
    let diff = overlap.gpu_addr - gpu_addr;
    if diff > new_size {
        return None;
    }
    let base_layer = (diff / layer_stride) as i32;
    let mip_offset = (diff % layer_stride) as u32;
    let offsets = calculate_mip_level_offsets(new_info);
    let levels = new_info.resources.levels as usize;
    let it = offsets[..levels].iter().position(|&o| o == mip_offset);
    let level = it? as i32;
    let base = SubresourceBase {
        level,
        layer: base_layer,
    };
    let info = &overlap.info;
    if !is_block_linear_size_compatible(new_info, info, base.level as u32, 0, strict_size) {
        return None;
    }
    if mip_block_size(new_info, base.level as u32) != info.block() {
        return None;
    }
    Some(SubresourceExtent {
        levels: new_info.resources.levels.max(info.resources.levels + base.level),
        layers: new_info.resources.layers.max(info.resources.layers + base.layer),
    })
}

/// Port of `ResolveOverlapRightAddress`.
fn resolve_overlap_right_address(
    new_info: &ImageInfo,
    gpu_addr: GPUVAddr,
    cpu_addr: VAddr,
    overlap: &ImageBase,
    strict_size: bool,
) -> Option<OverlapResult> {
    let resources = if new_info.image_type != ImageType::E3D {
        resolve_overlap_right_address_2d(new_info, gpu_addr, overlap, strict_size)?
    } else {
        resolve_overlap_right_address_3d(new_info, gpu_addr, overlap, strict_size)?
    };
    Some(OverlapResult {
        gpu_addr,
        cpu_addr,
        resources,
    })
}

/// Port of `ResolveOverlapLeftAddress`.
fn resolve_overlap_left_address(
    new_info: &ImageInfo,
    gpu_addr: GPUVAddr,
    _cpu_addr: VAddr,
    overlap: &ImageBase,
    strict_size: bool,
) -> Option<OverlapResult> {
    let base = overlap.try_find_base(gpu_addr)?;
    let info = &overlap.info;
    if !is_block_linear_size_compatible(new_info, info, base.level as u32, 0, strict_size) {
        return None;
    }
    if new_info.block() != mip_block_size(info, base.level as u32) {
        return None;
    }
    let resources = new_info.resources;
    let layers = if info.image_type != ImageType::E3D {
        resources.layers.max(info.resources.layers + base.layer)
    } else {
        1
    };
    Some(OverlapResult {
        gpu_addr: overlap.gpu_addr,
        cpu_addr: overlap.cpu_addr,
        resources: SubresourceExtent {
            levels: (resources.levels + base.level).max(info.resources.levels),
            layers,
        },
    })
}

/// Port of `ResolveOverlap`.
pub fn resolve_overlap(
    new_info: &ImageInfo,
    gpu_addr: GPUVAddr,
    cpu_addr: VAddr,
    overlap: &ImageBase,
    strict_size: bool,
    broken_views: bool,
    native_bgr: bool,
) -> Option<OverlapResult> {
    debug_assert!(new_info.image_type != ImageType::Linear);
    debug_assert!(overlap.info.image_type != ImageType::Linear);
    if !is_layer_stride_compatible(new_info, &overlap.info) {
        return None;
    }
    if !surface::is_view_compatible(overlap.info.format, new_info.format, broken_views, native_bgr) {
        return None;
    }
    if gpu_addr == overlap.gpu_addr {
        let solution = resolve_overlap_equal_address(new_info, overlap, strict_size)?;
        return Some(OverlapResult {
            gpu_addr,
            cpu_addr,
            resources: solution,
        });
    }
    if overlap.gpu_addr > gpu_addr {
        return resolve_overlap_right_address(new_info, gpu_addr, cpu_addr, overlap, strict_size);
    }
    resolve_overlap_left_address(new_info, gpu_addr, cpu_addr, overlap, strict_size)
}

/// Port of `IsLayerStrideCompatible`.
pub fn is_layer_stride_compatible(lhs: &ImageInfo, rhs: &ImageInfo) -> bool {
    if lhs.layer_stride == 0 {
        return true;
    }
    if rhs.layer_stride == 0 {
        return true;
    }
    if lhs.layer_stride == rhs.layer_stride {
        return true;
    }
    if lhs.maybe_unaligned_layer_stride == rhs.maybe_unaligned_layer_stride {
        return true;
    }
    false
}

/// Port of `FindSubresource`.
pub fn find_subresource(
    candidate: &ImageInfo,
    image: &ImageBase,
    candidate_addr: GPUVAddr,
    options: RelaxedOptions,
    broken_views: bool,
    native_bgr: bool,
) -> Option<SubresourceBase> {
    let base = image.try_find_base(candidate_addr)?;
    let existing = &image.info;

    if options.contains(RelaxedOptions::FORMAT) {
        // Format checking is relaxed, but still check matching bytes per block.
        if surface::bytes_per_block(existing.format) != surface::bytes_per_block(candidate.format) {
            return None;
        }
    } else {
        if !surface::is_view_compatible(existing.format, candidate.format, broken_views, native_bgr)
        {
            return None;
        }
    }
    if !is_layer_stride_compatible(existing, candidate) {
        return None;
    }
    if existing.image_type != candidate.image_type {
        return None;
    }
    if !options.contains(RelaxedOptions::SAMPLES)
        && existing.num_samples != candidate.num_samples
    {
        return None;
    }
    if existing.resources.levels < candidate.resources.levels + base.level {
        return None;
    }
    if existing.image_type == ImageType::E3D {
        let mip_depth = 1u32.max(existing.size.depth >> base.level as u32);
        if mip_depth < candidate.size.depth + base.layer as u32 {
            return None;
        }
    } else if existing.resources.layers < candidate.resources.layers + base.layer {
        return None;
    }
    let strict_size = !options.contains(RelaxedOptions::SIZE);
    if !is_block_linear_size_compatible(existing, candidate, base.level as u32, 0, strict_size) {
        return None;
    }
    Some(base)
}

/// Port of `IsSubresource`.
pub fn is_subresource(
    candidate: &ImageInfo,
    image: &ImageBase,
    candidate_addr: GPUVAddr,
    options: RelaxedOptions,
    broken_views: bool,
    native_bgr: bool,
) -> bool {
    find_subresource(candidate, image, candidate_addr, options, broken_views, native_bgr).is_some()
}

/// Port of `IsSubCopy`.
pub fn is_sub_copy(candidate: &ImageInfo, image: &ImageBase, candidate_addr: GPUVAddr) -> bool {
    let base = match image.try_find_base(candidate_addr) {
        Some(b) => b,
        None => return false,
    };
    let existing = &image.info;
    if existing.resources.levels < candidate.resources.levels + base.level {
        return false;
    }
    if existing.image_type == ImageType::E3D {
        let mip_depth = 1u32.max(existing.size.depth >> base.level as u32);
        if mip_depth < candidate.size.depth + base.layer as u32 {
            return false;
        }
    } else if existing.resources.layers < candidate.resources.layers + base.layer {
        return false;
    }
    if !is_block_linear_size_compatible_bpp_relaxed(existing, candidate, base.level as u32, 0) {
        return false;
    }
    true
}

/// Port of `DeduceBlitImages`.
pub fn deduce_blit_images(
    dst_info: &mut ImageInfo,
    src_info: &mut ImageInfo,
    dst: Option<&ImageBase>,
    src: Option<&ImageBase>,
) {
    let original_dst_format = dst_info.format;
    if let Some(s) = src {
        if surface::get_format_type(s.info.format) != surface::SurfaceType::ColorTexture {
            src_info.format = s.info.format;
        }
    }
    if let Some(d) = dst {
        if surface::get_format_type(d.info.format) != surface::SurfaceType::ColorTexture {
            dst_info.format = d.info.format;
        }
    }
    if let Some(s) = src {
        if surface::get_format_type(s.info.format) != surface::SurfaceType::ColorTexture {
            dst_info.format = s.info.format;
        }
    }
    if let Some(d) = dst {
        if surface::get_format_type(d.info.format) != surface::SurfaceType::ColorTexture {
            if let Some(s) = src {
                if surface::get_format_type(s.info.format) == surface::SurfaceType::ColorTexture {
                    dst_info.format = original_dst_format;
                }
            } else {
                src_info.format = d.info.format;
            }
        }
    }
}

/// Port of `MapSizeBytes`.
pub fn map_size_bytes(image: &ImageBase) -> u32 {
    use super::image_base::ImageFlagBits;
    if image.flags.contains(ImageFlagBits::ACCELERATED_UPLOAD) {
        image.guest_size_bytes
    } else if image.flags.contains(ImageFlagBits::CONVERTED) {
        image.converted_size_bytes
    } else {
        image.unswizzled_size_bytes
    }
}
