// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/texture_cache/decode_bc.h and decode_bc.cpp
//!
//! Software BCn block-compression decompression.
//!
//! Upstream calls into `bc_decoder.h`; ruzu ports the block traversal here
//! and currently implements the smaller BC1/BC2/BC3/BC4 paths directly.

use super::format_lookup_table::PixelFormat;
use super::types::BufferImageCopy;

// ── Constants ──────────────────────────────────────────────────────────

const BLOCK_SIZE: u32 = 4;

type BcnDecodeSigned = unsafe extern "C" fn(*const u8, *mut u8, usize, usize, usize, usize, bool);
type BcnDecode = unsafe extern "C" fn(*const u8, *mut u8, usize, usize, usize, usize);

extern "C" {
    fn ruzu_decode_bc6_block(
        src: *const u8,
        dst: *mut u8,
        x: usize,
        y: usize,
        width: usize,
        height: usize,
        is_signed: bool,
    );
    fn ruzu_decode_bc7_block(
        src: *const u8,
        dst: *mut u8,
        x: usize,
        y: usize,
        width: usize,
        height: usize,
    );
}

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
        PixelFormat::Bc4Unorm | PixelFormat::Bc4Snorm => {
            decompress_bc4_blocks(input, output, copy, pixel_format == PixelFormat::Bc4Snorm);
        }
        PixelFormat::Bc1RgbaUnorm | PixelFormat::Bc1RgbaSrgb => {
            decompress_bc1_blocks(input, output, copy);
        }
        PixelFormat::Bc2Unorm | PixelFormat::Bc2Srgb => {
            decompress_bc2_blocks(input, output, copy);
        }
        PixelFormat::Bc3Unorm | PixelFormat::Bc3Srgb => {
            decompress_bc3_blocks(input, output, copy);
        }
        PixelFormat::Bc5Snorm | PixelFormat::Bc5Unorm => {
            decompress_bc5_blocks(input, output, copy, pixel_format == PixelFormat::Bc5Snorm);
        }
        PixelFormat::Bc6hSfloat | PixelFormat::Bc6hUfloat => {
            decompress_bc6_blocks(input, output, copy, pixel_format == PixelFormat::Bc6hSfloat);
        }
        PixelFormat::Bc7Srgb | PixelFormat::Bc7Unorm => {
            decompress_bc7_blocks(input, output, copy);
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

fn decompress_bc4_blocks(input: &[u8], output: &mut [u8], copy: &BufferImageCopy, is_signed: bool) {
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
                let values = decode_bc4_block(block, is_signed);
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
            input_offset += (copy.buffer_row_length * block_size / block_width) as usize;
            output_offset += (block_height * pitch) as usize;
        }
    }
}

fn decompress_bc5_blocks(input: &[u8], output: &mut [u8], copy: &BufferImageCopy, is_signed: bool) {
    let out_bpp = converted_bytes_per_block(PixelFormat::Bc5Unorm);
    let block_size = block_byte_size(PixelFormat::Bc5Unorm);
    let channel_block_size = block_byte_size(PixelFormat::Bc4Unorm) as usize;
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
                let Some(red_block) = input
                    .get(src_offset..src_offset + channel_block_size)
                    .and_then(|bytes| bytes.try_into().ok())
                else {
                    return;
                };
                let Some(green_block) = input
                    .get(src_offset + channel_block_size..src_offset + block_size as usize)
                    .and_then(|bytes| bytes.try_into().ok())
                else {
                    return;
                };
                let values = decode_bc5_block(red_block, green_block, is_signed);
                let copy_width = block_width.min(width - x);
                let copy_height = block_height.min(height - y);
                for row in 0..copy_height as usize {
                    let src_row = row * BLOCK_SIZE as usize * out_bpp as usize;
                    let dst_row = dst_offset + row * pitch as usize;
                    let len = copy_width as usize * out_bpp as usize;
                    let Some(dst) = output.get_mut(dst_row..dst_row + len) else {
                        return;
                    };
                    dst.copy_from_slice(&values[src_row..src_row + len]);
                }
                src_offset += block_size as usize;
                dst_offset += (block_width * out_bpp) as usize;
            }
            input_offset += (copy.buffer_row_length * block_size / block_width) as usize;
            output_offset += (block_height * pitch) as usize;
        }
    }
}

fn decode_bc5_block(red_block: &[u8; 8], green_block: &[u8; 8], is_signed: bool) -> [u8; 32] {
    let red = decode_bc4_block(red_block, is_signed);
    let green = decode_bc4_block(green_block, is_signed);
    let mut out = [0u8; 32];
    for i in 0..16 {
        out[i * 2] = red[i];
        out[i * 2 + 1] = green[i];
    }
    out
}

fn decompress_bc1_blocks(input: &[u8], output: &mut [u8], copy: &BufferImageCopy) {
    decompress_color_blocks(input, output, copy, PixelFormat::Bc1RgbaUnorm);
}

fn decompress_bc2_blocks(input: &[u8], output: &mut [u8], copy: &BufferImageCopy) {
    decompress_color_blocks(input, output, copy, PixelFormat::Bc2Unorm);
}

fn decompress_bc3_blocks(input: &[u8], output: &mut [u8], copy: &BufferImageCopy) {
    decompress_color_blocks(input, output, copy, PixelFormat::Bc3Unorm);
}

fn decompress_bc6_blocks(input: &[u8], output: &mut [u8], copy: &BufferImageCopy, is_signed: bool) {
    decompress_external_signed_blocks(
        input,
        output,
        copy,
        PixelFormat::Bc6hUfloat,
        ruzu_decode_bc6_block,
        is_signed,
    );
}

fn decompress_bc7_blocks(input: &[u8], output: &mut [u8], copy: &BufferImageCopy) {
    decompress_external_blocks(
        input,
        output,
        copy,
        PixelFormat::Bc7Unorm,
        ruzu_decode_bc7_block,
    );
}

fn decompress_external_signed_blocks(
    input: &[u8],
    output: &mut [u8],
    copy: &BufferImageCopy,
    pixel_format: PixelFormat,
    decompress: BcnDecodeSigned,
    is_signed: bool,
) {
    decompress_external_blocks_impl(
        input,
        output,
        copy,
        pixel_format,
        |src, dst, x, y, width, height| unsafe {
            decompress(src, dst, x, y, width, height, is_signed);
        },
    );
}

fn decompress_external_blocks(
    input: &[u8],
    output: &mut [u8],
    copy: &BufferImageCopy,
    pixel_format: PixelFormat,
    decompress: BcnDecode,
) {
    decompress_external_blocks_impl(
        input,
        output,
        copy,
        pixel_format,
        |src, dst, x, y, width, height| unsafe {
            decompress(src, dst, x, y, width, height);
        },
    );
}

fn decompress_external_blocks_impl(
    input: &[u8],
    output: &mut [u8],
    copy: &BufferImageCopy,
    pixel_format: PixelFormat,
    mut decompress: impl FnMut(*const u8, *mut u8, usize, usize, usize, usize),
) {
    let out_bpp = converted_bytes_per_block(pixel_format);
    let block_size = block_byte_size(pixel_format);
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
                let Some(src) = input.get(src_offset..src_offset + block_size as usize) else {
                    return;
                };
                let Some(dst) = output.get_mut(dst_offset..) else {
                    return;
                };
                decompress(
                    src.as_ptr(),
                    dst.as_mut_ptr(),
                    x as usize,
                    y as usize,
                    width as usize,
                    height as usize,
                );
                src_offset += block_size as usize;
                dst_offset += (block_width * out_bpp) as usize;
            }
            input_offset += (copy.buffer_row_length * block_size / block_width) as usize;
            output_offset += (block_height * pitch) as usize;
        }
    }
}

fn decompress_color_blocks(
    input: &[u8],
    output: &mut [u8],
    copy: &BufferImageCopy,
    pixel_format: PixelFormat,
) {
    let out_bpp = converted_bytes_per_block(PixelFormat::Bc1RgbaUnorm);
    let block_size = block_byte_size(pixel_format);
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
                let Some(block) = input.get(src_offset..src_offset + block_size as usize) else {
                    return;
                };
                let values = match pixel_format {
                    PixelFormat::Bc1RgbaUnorm => {
                        let Some(block) = block.try_into().ok() else {
                            return;
                        };
                        decode_bc_color_block(block, true, false)
                    }
                    PixelFormat::Bc2Unorm => {
                        let Some(block) = block.try_into().ok() else {
                            return;
                        };
                        decode_bc2_block(block)
                    }
                    PixelFormat::Bc3Unorm => {
                        let Some(block) = block.try_into().ok() else {
                            return;
                        };
                        decode_bc3_block(block)
                    }
                    _ => return,
                };
                let copy_width = block_width.min(width - x);
                let copy_height = block_height.min(height - y);
                for row in 0..copy_height as usize {
                    let src_row = row * BLOCK_SIZE as usize * out_bpp as usize;
                    let dst_row = dst_offset + row * pitch as usize;
                    let len = copy_width as usize * out_bpp as usize;
                    let Some(dst) = output.get_mut(dst_row..dst_row + len) else {
                        return;
                    };
                    dst.copy_from_slice(&values[src_row..src_row + len]);
                }
                src_offset += block_size as usize;
                dst_offset += (block_width * out_bpp) as usize;
            }
            input_offset += (copy.buffer_row_length * block_size / block_width) as usize;
            output_offset += (block_height * pitch) as usize;
        }
    }
}

fn decode_bc1_block(block: &[u8; 8]) -> [u8; 64] {
    decode_bc_color_block(block, true, false)
}

fn decode_bc_color_block(
    block: &[u8; 8],
    has_alpha_channel: bool,
    has_separate_alpha: bool,
) -> [u8; 64] {
    let c0 = u16::from_le_bytes([block[0], block[1]]);
    let c1 = u16::from_le_bytes([block[2], block[3]]);
    let indices = u32::from_le_bytes([block[4], block[5], block[6], block[7]]);
    let mut palette = [[0u8; 4]; 4];
    palette[0] = rgb565_to_rgba(c0);
    palette[1] = rgb565_to_rgba(c1);
    if has_separate_alpha || c0 > c1 {
        palette[2] = interpolate_rgba(palette[0], 2, palette[1], 1, 3);
        palette[3] = interpolate_rgba(palette[1], 2, palette[0], 1, 3);
    } else {
        palette[2] = average_rgba(palette[0], palette[1]);
        if has_alpha_channel {
            palette[3] = [0, 0, 0, 0];
        }
    }

    let mut out = [0u8; 64];
    for texel in 0..16 {
        let index = ((indices >> (texel * 2)) & 0x3) as usize;
        out[texel * 4..texel * 4 + 4].copy_from_slice(&palette[index]);
    }
    out
}

fn decode_bc2_block(block: &[u8; 16]) -> [u8; 64] {
    let color_block: &[u8; 8] = block[8..16].try_into().expect("BC2 color block size");
    let mut out = decode_bc_color_block(color_block, false, true);
    let alpha = u64::from_le_bytes(block[0..8].try_into().expect("BC2 alpha block size"));
    for texel in 0..16 {
        let value = ((alpha >> (texel * 4)) & 0xf) as u8;
        out[texel * 4 + 3] = value | (value << 4);
    }
    out
}

fn decode_bc3_block(block: &[u8; 16]) -> [u8; 64] {
    let color_block: &[u8; 8] = block[8..16].try_into().expect("BC3 color block size");
    let mut out = decode_bc_color_block(color_block, false, true);
    let alpha_block: &[u8; 8] = block[0..8].try_into().expect("BC3 alpha block size");
    let alpha = decode_bc4_block(alpha_block, false);
    for texel in 0..16 {
        out[texel * 4 + 3] = alpha[texel];
    }
    out
}

fn rgb565_to_rgba(color: u16) -> [u8; 4] {
    let blue = (((color & 0x001f) << 3) | ((color & 0x001c) >> 2)) as u8;
    let green = (((color & 0x07e0) >> 3) | ((color & 0x0600) >> 9)) as u8;
    let red = (((color & 0xf800) >> 8) | ((color & 0xe000) >> 13)) as u8;
    [red, green, blue, 0xff]
}

fn interpolate_rgba(a: [u8; 4], a_factor: u16, b: [u8; 4], b_factor: u16, divisor: u16) -> [u8; 4] {
    [
        (((a[0] as u16 * a_factor) + (b[0] as u16 * b_factor)) / divisor) as u8,
        (((a[1] as u16 * a_factor) + (b[1] as u16 * b_factor)) / divisor) as u8,
        (((a[2] as u16 * a_factor) + (b[2] as u16 * b_factor)) / divisor) as u8,
        (((a[3] as u16 * a_factor) + (b[3] as u16 * b_factor)) / divisor) as u8,
    ]
}

fn average_rgba(a: [u8; 4], b: [u8; 4]) -> [u8; 4] {
    [
        ((a[0] as u16 + b[0] as u16) >> 1) as u8,
        ((a[1] as u16 + b[1] as u16) >> 1) as u8,
        ((a[2] as u16 + b[2] as u16) >> 1) as u8,
        ((a[3] as u16 + b[3] as u16) >> 1) as u8,
    ]
}

fn decode_bc4_block(block: &[u8; 8], is_signed: bool) -> [u8; 16] {
    let r0 = if is_signed {
        block[0] as i8 as i32
    } else {
        block[0] as i32
    };
    let r1 = if is_signed {
        block[1] as i8 as i32
    } else {
        block[1] as i32
    };
    let mut palette = [0i32; 8];
    palette[0] = r0;
    palette[1] = r1;
    if r0 > r1 {
        for i in 2..8i32 {
            palette[i as usize] = ((8 - i) * r0 + (i - 1) * r1) / 7;
        }
    } else {
        for i in 2..6i32 {
            palette[i as usize] = ((6 - i) * r0 + (i - 1) * r1) / 5;
        }
        palette[6] = if is_signed { -128 } else { 0 };
        palette[7] = if is_signed { 127 } else { 255 };
    }

    let mut bits = 0u64;
    for i in 0..6 {
        bits |= (block[2 + i] as u64) << (i * 8);
    }
    let mut values = [0u8; 16];
    for (i, value) in values.iter_mut().enumerate() {
        *value = palette[((bits >> (i * 3)) & 0x7) as usize] as u8;
    }
    values
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bc4_unorm_decodes_3bit_indices() {
        let block = [10, 20, 0b1000_1000, 0b1100_0110, 0b1111_1010, 0, 0, 0];
        let values = decode_bc4_block(&block, false);
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
    fn bc4_snorm_preserves_signed_endpoint_bit_patterns() {
        let block = [0x80, 0x7f, 0b1000_1000, 0b1100_0110, 0b1111_1010, 0, 0, 0];
        let values = decode_bc4_block(&block, true);
        assert_eq!(values[0], 0x80);
        assert_eq!(values[1], 0x7f);
        assert_eq!(values[2], 0xb3);
        assert_eq!(values[3], 0xe6);
        assert_eq!(values[4], 0x19);
        assert_eq!(values[5], 0x4c);
        assert_eq!(values[6], 0x80);
        assert_eq!(values[7], 0x7f);
    }

    #[test]
    fn bc1_decodes_rgb565_and_integer_interpolation() {
        let c0 = 0xf800u16.to_le_bytes(); // red
        let c1 = 0x07e0u16.to_le_bytes(); // green
        let indices = 0b11_10_01_00u32.to_le_bytes();
        let block = [
            c0[0], c0[1], c1[0], c1[1], indices[0], indices[1], indices[2], indices[3],
        ];
        let values = decode_bc1_block(&block);
        assert_eq!(&values[0..4], &[255, 0, 0, 255]);
        assert_eq!(&values[4..8], &[0, 255, 0, 255]);
        assert_eq!(&values[8..12], &[170, 85, 0, 255]);
        assert_eq!(&values[12..16], &[85, 170, 0, 255]);
    }

    #[test]
    fn bc1_decodes_transparent_fourth_color_when_c0_le_c1() {
        let c0 = 0x001fu16.to_le_bytes(); // blue, less than white
        let c1 = 0xffffu16.to_le_bytes(); // white
        let indices = 0b11_10_01_00u32.to_le_bytes();
        let block = [
            c0[0], c0[1], c1[0], c1[1], indices[0], indices[1], indices[2], indices[3],
        ];
        let values = decode_bc1_block(&block);
        assert_eq!(&values[0..4], &[0, 0, 255, 255]);
        assert_eq!(&values[4..8], &[255, 255, 255, 255]);
        assert_eq!(&values[8..12], &[127, 127, 255, 255]);
        assert_eq!(&values[12..16], &[0, 0, 0, 0]);
    }

    #[test]
    fn bc2_decodes_separate_4bit_alpha() {
        let alpha = 0xfedc_ba98_7654_3210u64.to_le_bytes();
        let c0 = 0xf800u16.to_le_bytes();
        let c1 = 0x07e0u16.to_le_bytes();
        let indices = 0u32.to_le_bytes();
        let block = [
            alpha[0], alpha[1], alpha[2], alpha[3], alpha[4], alpha[5], alpha[6], alpha[7], c0[0],
            c0[1], c1[0], c1[1], indices[0], indices[1], indices[2], indices[3],
        ];
        let values = decode_bc2_block(&block);
        assert_eq!(&values[0..4], &[255, 0, 0, 0x00]);
        assert_eq!(&values[4..8], &[255, 0, 0, 0x11]);
        assert_eq!(&values[8..12], &[255, 0, 0, 0x22]);
        assert_eq!(&values[60..64], &[255, 0, 0, 0xff]);
    }

    #[test]
    fn bc3_decodes_interpolated_alpha_channel() {
        let alpha = [10, 20, 0b1000_1000, 0b1100_0110, 0b1111_1010, 0, 0, 0];
        let c0 = 0xf800u16.to_le_bytes();
        let c1 = 0x07e0u16.to_le_bytes();
        let indices = 0u32.to_le_bytes();
        let block = [
            alpha[0], alpha[1], alpha[2], alpha[3], alpha[4], alpha[5], alpha[6], alpha[7], c0[0],
            c0[1], c1[0], c1[1], indices[0], indices[1], indices[2], indices[3],
        ];
        let values = decode_bc3_block(&block);
        assert_eq!(&values[0..4], &[255, 0, 0, 10]);
        assert_eq!(&values[4..8], &[255, 0, 0, 20]);
        assert_eq!(&values[8..12], &[255, 0, 0, 12]);
        assert_eq!(&values[12..16], &[255, 0, 0, 14]);
    }

    #[test]
    fn bc5_unorm_interleaves_red_and_green_channels() {
        let red = [10, 20, 0, 0, 0, 0, 0, 0];
        let green = [30, 40, 0, 0, 0, 0, 0, 0];
        let values = decode_bc5_block(&red, &green, false);
        assert_eq!(&values[0..8], &[10, 30, 10, 30, 10, 30, 10, 30]);
        assert_eq!(values.len(), 32);
    }

    #[test]
    fn bc5_snorm_preserves_signed_channel_bit_patterns() {
        let red = [0x80, 0x7f, 0, 0, 0, 0, 0, 0];
        let green = [0x7f, 0x80, 0, 0, 0, 0, 0, 0];
        let values = decode_bc5_block(&red, &green, true);
        assert_eq!(
            &values[0..8],
            &[0x80, 0x7f, 0x80, 0x7f, 0x80, 0x7f, 0x80, 0x7f]
        );
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
                0x11, 0x11, 0x11, 0x11, 0x22, 0x11, 0x11, 0x11, 0x11, 0x22, 0x11, 0x11, 0x11, 0x11,
                0x22,
            ]
        );
    }

    fn single_block_copy() -> BufferImageCopy {
        BufferImageCopy {
            buffer_offset: 0,
            buffer_size: 16,
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
        }
    }

    #[test]
    fn decompress_bc6_uses_upstream_decoder_instead_of_zero_fill() {
        let input = [0xffu8; 16];
        let mut copy = single_block_copy();
        let mut output = [0u8; 4 * 4 * 8];

        decompress_bcn(&input, &mut output, &mut copy, PixelFormat::Bc6hUfloat);

        assert!(output.iter().any(|&byte| byte != 0));
    }

    #[test]
    fn decompress_bc7_uses_upstream_decoder_instead_of_zero_fill() {
        let input = [0xffu8; 16];
        let mut copy = single_block_copy();
        let mut output = [0u8; 4 * 4 * 4];

        decompress_bcn(&input, &mut output, &mut copy, PixelFormat::Bc7Unorm);

        assert!(output.iter().any(|&byte| byte != 0));
    }
}
