// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/texture_cache/accelerated_swizzle.h and accelerated_swizzle.cpp
//!
//! GPU-accelerated block-linear swizzle parameter generation for 2D and 3D
//! textures.

use crate::surface::bytes_per_block;

use super::image_info::ImageInfo;
use super::types::*;

// ── Parameter structs ──────────────────────────────────────────────────

/// Parameters for a 2D block-linear swizzle compute dispatch.
///
/// Port of `VideoCommon::Accelerated::BlockLinearSwizzle2DParams`.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C, align(16))]
pub struct BlockLinearSwizzle2DParams {
    pub origin: [u32; 3],
    pub _pad0: u32,
    pub destination: [i32; 3],
    pub _pad1: i32,
    pub bytes_per_block_log2: u32,
    pub layer_stride: u32,
    pub block_size: u32,
    pub x_shift: u32,
    pub block_height: u32,
    pub block_height_mask: u32,
}

/// Parameters for a 3D block-linear swizzle compute dispatch.
///
/// Port of `VideoCommon::Accelerated::BlockLinearSwizzle3DParams`.
#[derive(Debug, Clone, Copy, Default)]
#[repr(C)]
pub struct BlockLinearSwizzle3DParams {
    pub origin: [u32; 3],
    pub destination: [i32; 3],
    pub bytes_per_block_log2: u32,
    pub slice_size: u32,
    pub block_size: u32,
    pub x_shift: u32,
    pub block_height: u32,
    pub block_height_mask: u32,
    pub block_depth: u32,
    pub block_depth_mask: u32,
}

// ── GOB constants ──────────────────────────────────────────────────────
// Upstream: Tegra::Texture::GOB_SIZE_* in textures/decoders.h

const GOB_SIZE_SHIFT: u32 = 9; // 512 bytes per GOB
const GOB_SIZE_X_SHIFT: u32 = 6; // 64 bytes wide
const GOB_SIZE_Y_SHIFT: u32 = 3; // 8 rows tall

// ── Public functions ───────────────────────────────────────────────────

/// Build parameters for a 2D block-linear swizzle.
///
/// Port of `VideoCommon::Accelerated::MakeBlockLinearSwizzle2DParams`.
pub fn make_block_linear_swizzle_2d_params(
    swizzle: &SwizzleParameters,
    info: &ImageInfo,
) -> BlockLinearSwizzle2DParams {
    let block = swizzle.block;
    let num_tiles = swizzle.num_tiles;
    let bytes_per_block = bytes_per_block_for_format(info.format);
    let stride_alignment =
        super::util::calculate_level_stride_alignment(info, swizzle.level as u32);
    let stride = align_up_log2(num_tiles.width, stride_alignment) * bytes_per_block;
    let gobs_in_x = div_ceil_log2(stride, GOB_SIZE_X_SHIFT);
    BlockLinearSwizzle2DParams {
        origin: [0, 0, 0],
        _pad0: 0,
        destination: [0, 0, 0],
        _pad1: 0,
        bytes_per_block_log2: bytes_per_block.trailing_zeros(),
        layer_stride: info.layer_stride,
        block_size: gobs_in_x << (GOB_SIZE_SHIFT + block.height + block.depth),
        x_shift: GOB_SIZE_SHIFT + block.height + block.depth,
        block_height: block.height,
        block_height_mask: (1u32 << block.height) - 1,
    }
}

/// Build parameters for a 3D block-linear swizzle.
///
/// Port of `VideoCommon::Accelerated::MakeBlockLinearSwizzle3DParams`.
pub fn make_block_linear_swizzle_3d_params(
    swizzle: &SwizzleParameters,
    info: &ImageInfo,
) -> BlockLinearSwizzle3DParams {
    let block = swizzle.block;
    let num_tiles = swizzle.num_tiles;
    let bytes_per_block = bytes_per_block_for_format(info.format);
    let stride_alignment =
        super::util::calculate_level_stride_alignment(info, swizzle.level as u32);
    let stride = align_up_log2(num_tiles.width, stride_alignment) * bytes_per_block;

    let gob_size_x: u32 = 1 << GOB_SIZE_X_SHIFT;
    let gobs_in_x = (stride + gob_size_x - 1) >> GOB_SIZE_X_SHIFT;
    let block_size = gobs_in_x << (GOB_SIZE_SHIFT + block.height + block.depth);
    let slice_size = div_ceil_log2(num_tiles.height, block.height + GOB_SIZE_Y_SHIFT) * block_size;

    BlockLinearSwizzle3DParams {
        origin: [0, 0, 0],
        destination: [0, 0, 0],
        bytes_per_block_log2: bytes_per_block.trailing_zeros(),
        slice_size,
        block_size,
        x_shift: GOB_SIZE_SHIFT + block.height + block.depth,
        block_height: block.height,
        block_height_mask: (1u32 << block.height) - 1,
        block_depth: block.depth,
        block_depth_mask: (1u32 << block.depth) - 1,
    }
}

// ── Internal helpers ───────────────────────────────────────────────────

/// Bytes per block for a given pixel format.
///
/// Delegates to `VideoCore::Surface::BytesPerBlock` (ported in `surface.rs`).
fn bytes_per_block_for_format(format: super::format_lookup_table::PixelFormat) -> u32 {
    bytes_per_block(format)
}

fn align_up_log2(value: u32, alignment_log2: u32) -> u32 {
    let mask = (1u32 << alignment_log2) - 1;
    (value + mask) & !mask
}

fn div_ceil_log2(value: u32, shift: u32) -> u32 {
    (value + (1u32 << shift) - 1) >> shift
}
