// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/texture_cache/decode_bc.h and decode_bc.cpp
//!
//! Software BCn block-compression decompression.
//!
//! Upstream calls into `bc_decoder.h`; ruzu ports the block traversal here
//! and currently implements the BC4_UNORM path used by MK8D alpha masks.

use super::format_lookup_table::PixelFormat;
use super::types::BufferImageCopy;

// ── Constants ──────────────────────────────────────────────────────────

const BLOCK_SIZE: u32 = 4;

// ── Public API ─────────────────────────────────────────────────────────

/// Bytes per output texel after decompression for a given BCn format.
///
/// Port of `VideoCommon::ConvertedBytesPerBlock`.
pub fn converted_bytes_per_block(pixel_format: PixelFormat) -> u32 {
    match pixel_format {
        PixelFormat::Bc4Snorm | PixelFormat::Bc4Unorm => 1,
        PixelFormat::Bc5Snorm | PixelFormat::Bc5Unorm => 2,
        PixelFormat::Bc6hSfloat | PixelFormat::Bc6hUfloat => 8,
        _ => 4,
    }
}

/// Decompress a BCn-compressed image into `output`.
///
/// Port of `VideoCommon::DecompressBCn`.
pub fn decompress_bcn(
    input: &[u8],
    output: &mut [u8],
    copy: &mut BufferImageCopy,
    pixel_format: PixelFormat,
) {
    match pixel_format {
        PixelFormat::Bc4Unorm => {
            decompress_bc4_unorm_blocks(input, output, copy);
        }
        PixelFormat::Bc1RgbaUnorm | PixelFormat::Bc1RgbaSrgb => {
            log::warn!("DecompressBCn: BC1 not yet implemented (no BCn decoder crate)");
            output.fill(0);
        }
        PixelFormat::Bc2Unorm | PixelFormat::Bc2Srgb => {
            log::warn!("DecompressBCn: BC2 not yet implemented (no BCn decoder crate)");
            output.fill(0);
        }
        PixelFormat::Bc3Unorm | PixelFormat::Bc3Srgb => {
            log::warn!("DecompressBCn: BC3 not yet implemented (no BCn decoder crate)");
            output.fill(0);
        }
        PixelFormat::Bc4Snorm => {
            log::warn!("DecompressBCn: BC4_SNORM not yet implemented (no BCn decoder crate)");
            output.fill(0);
        }
        PixelFormat::Bc5Snorm | PixelFormat::Bc5Unorm => {
            log::warn!("DecompressBCn: BC5 not yet implemented (no BCn decoder crate)");
            output.fill(0);
        }
        PixelFormat::Bc6hSfloat | PixelFormat::Bc6hUfloat => {
            log::warn!("DecompressBCn: BC6 not yet implemented (no BCn decoder crate)");
            output.fill(0);
        }
        PixelFormat::Bc7Srgb | PixelFormat::Bc7Unorm => {
            log::warn!("DecompressBCn: BC7 not yet implemented (no BCn decoder crate)");
            output.fill(0);
        }
        _ => {
            log::warn!("DecompressBCn: unimplemented format {:?}", pixel_format);
        }
    }
}

// ── Private helpers ────────────────────────────────────────────────────

/// Whether the format uses a signed decode path.
#[allow(dead_code)]
const fn is_signed(pf: PixelFormat) -> bool {
    matches!(
        pf,
        PixelFormat::Bc4Snorm
            | PixelFormat::Bc4Unorm
            | PixelFormat::Bc5Snorm
            | PixelFormat::Bc5Unorm
            | PixelFormat::Bc6hSfloat
            | PixelFormat::Bc6hUfloat
    )
}

/// Compressed block size in bytes.
const fn block_byte_size(pf: PixelFormat) -> u32 {
    match pf {
        PixelFormat::Bc1RgbaSrgb
        | PixelFormat::Bc1RgbaUnorm
        | PixelFormat::Bc4Snorm
        | PixelFormat::Bc4Unorm => 8,
        _ => 16,
    }
}

fn decompress_bc4_unorm_blocks(input: &[u8], output: &mut [u8], copy: &BufferImageCopy) {
    let out_bpp = converted_bytes_per_block(PixelFormat::Bc4Unorm);
    let block_size = block_byte_size(PixelFormat::Bc4Unorm);
    let width = copy.image_extent.width;
    let height = copy.image_extent.height * copy.image_subresource.num_layers as u32;
    let depth = copy.image_extent.depth;
    if width == 0 || height == 0 || depth == 0 {
        return;
    }
    let block_width = width.min(BLOCK_SIZE);
    let block_height = height.min(BLOCK_SIZE);
    let pitch = width * out_bpp;
    let mut input_offset = 0usize;
    let mut output_offset = 0usize;

    for _slice in 0..depth {
        for y in (0..height).step_by(block_height as usize) {
            let mut src_offset = input_offset;
            let mut dst_offset = output_offset;
            for x in (0..width).step_by(block_width as usize) {
                let Some(block) = input
                    .get(src_offset..src_offset + block_size as usize)
                    .and_then(|bytes| bytes.try_into().ok())
                else {
                    return;
                };
                let values = decode_bc4_unorm_block(block);
                let copy_width = block_width.min(width - x);
                let copy_height = block_height.min(height - y);
                for row in 0..copy_height as usize {
                    let src_row = row * BLOCK_SIZE as usize;
                    let dst_row = dst_offset + row * pitch as usize;
                    let len = copy_width as usize;
                    let Some(dst) = output.get_mut(dst_row..dst_row + len) else {
                        return;
                    };
                    dst.copy_from_slice(&values[src_row..src_row + len]);
                }
                src_offset += block_size as usize;
                dst_offset += (block_width * out_bpp) as usize;
            }
            input_offset +=
                (copy.buffer_row_length * block_size / block_width) as usize;
            output_offset += (block_height * pitch) as usize;
        }
    }
}

fn decode_bc4_unorm_block(block: &[u8; 8]) -> [u8; 16] {
    let r0 = block[0];
    let r1 = block[1];
    let mut palette = [0u8; 8];
    palette[0] = r0;
    palette[1] = r1;
    if r0 > r1 {
        for i in 1..7u16 {
            palette[(i + 1) as usize] = (((7 - i) * r0 as u16 + i * r1 as u16 + 3) / 7) as u8;
        }
    } else {
        for i in 1..5u16 {
            palette[(i + 1) as usize] = (((5 - i) * r0 as u16 + i * r1 as u16 + 2) / 5) as u8;
        }
        palette[6] = 0;
        palette[7] = 255;
    }

    let mut bits = 0u64;
    for i in 0..6 {
        bits |= (block[2 + i] as u64) << (i * 8);
    }
    let mut values = [0u8; 16];
    for (i, value) in values.iter_mut().enumerate() {
        *value = palette[((bits >> (i * 3)) & 0x7) as usize];
    }
    values
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bc4_unorm_decodes_3bit_indices() {
        let block = [10, 20, 0b1000_1000, 0b1100_0110, 0b1111_1010, 0, 0, 0];
        let values = decode_bc4_unorm_block(&block);
        assert_eq!(values[0], 10);
        assert_eq!(values[1], 20);
        assert_eq!(values[2], 12);
        assert_eq!(values[3], 14);
        assert_eq!(values[4], 16);
        assert_eq!(values[5], 18);
        assert_eq!(values[6], 0);
        assert_eq!(values[7], 255);
    }

    #[test]
    fn decompress_bc4_unorm_respects_copy_layout() {
        let block = [0x40, 0x80, 0, 0, 0, 0, 0, 0];
        let mut copy = BufferImageCopy {
            buffer_offset: 0,
            buffer_size: 8,
            buffer_row_length: 4,
            buffer_image_height: 4,
            image_subresource: super::super::types::SubresourceLayers {
                base_level: 0,
                base_layer: 0,
                num_layers: 1,
            },
            image_offset: super::super::types::Offset3D { x: 0, y: 0, z: 0 },
            image_extent: super::super::types::Extent3D {
                width: 4,
                height: 4,
                depth: 1,
            },
        };
        let mut output = [0u8; 16];
        decompress_bcn(&block, &mut output, &mut copy, PixelFormat::Bc4Unorm);
        assert_eq!(output, [0x40; 16]);
    }

    #[test]
    fn decompress_bc4_unorm_clamps_partial_edge_blocks() {
        let input = [
            0x11, 0x80, 0, 0, 0, 0, 0, 0, // x=0..3
            0x22, 0x80, 0, 0, 0, 0, 0, 0, // x=4 edge texel only
        ];
        let mut copy = BufferImageCopy {
            buffer_offset: 0,
            buffer_size: input.len(),
            buffer_row_length: 8,
            buffer_image_height: 4,
            image_subresource: super::super::types::SubresourceLayers {
                base_level: 0,
                base_layer: 0,
                num_layers: 1,
            },
            image_offset: super::super::types::Offset3D { x: 0, y: 0, z: 0 },
            image_extent: super::super::types::Extent3D {
                width: 5,
                height: 3,
                depth: 1,
            },
        };
        let mut output = [0u8; 15];
        decompress_bcn(&input, &mut output, &mut copy, PixelFormat::Bc4Unorm);
        assert_eq!(
            output,
            [
                0x11, 0x11, 0x11, 0x11, 0x22,
                0x11, 0x11, 0x11, 0x11, 0x22,
                0x11, 0x11, 0x11, 0x11, 0x22,
            ]
        );
    }
}
