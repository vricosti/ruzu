// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! GOB (Group of Bytes) block-linear detiling for Tegra X1.
//!
//! The Switch GPU stores framebuffers in block-linear (GOB) tiling format.
//! Each GOB is 64 bytes wide × 8 rows = 512 bytes. GOBs are stacked vertically
//! in blocks of `2^block_height` GOBs. This module converts block-linear data
//! to a linear (row-major) format for CPU-side presentation.

/// Width of one GOB in bytes.
const GOB_SIZE_X: usize = 64;

/// Height of one GOB in rows.
const GOB_SIZE_Y: usize = 8;

/// Total bytes per GOB.
const GOB_SIZE: usize = GOB_SIZE_X * GOB_SIZE_Y; // 512

/// Detile a block-linear framebuffer to linear row-major format.
///
/// # Parameters
/// - `input`: Block-linear tiled pixel data
/// - `width`: Width in pixels
/// - `height`: Height in pixels
/// - `bpp`: Bytes per pixel (typically 4 for RGBA8888)
/// - `block_height_log2`: Block height exponent (actual block height = 2^block_height_log2 GOBs)
///
/// # Returns
/// Linear row-major pixel data.
pub fn detile_block_linear(
    input: &[u8],
    width: u32,
    height: u32,
    bpp: u32,
    block_height_log2: u32,
) -> Vec<u8> {
    let width = width as usize;
    let height = height as usize;
    let bpp = bpp as usize;
    let stride_bytes = width * bpp;

    let block_height = 1usize << block_height_log2; // GOBs per block vertically
    let block_height_pixels = block_height * GOB_SIZE_Y;

    // Number of GOBs horizontally.
    let gobs_x = (stride_bytes + GOB_SIZE_X - 1) / GOB_SIZE_X;

    let mut output = vec![0u8; stride_bytes * height];

    // Process each row of the output.
    for y in 0..height {
        // Which block row are we in?
        let block_y = y / block_height_pixels;
        // Which GOB within the block?
        let gob_y_in_block = (y % block_height_pixels) / GOB_SIZE_Y;
        // Which row within the GOB?
        let row_in_gob = y % GOB_SIZE_Y;

        for gob_x in 0..gobs_x {
            // Offset of this GOB in the tiled data.
            let gob_offset = (block_y * gobs_x * block_height + gob_y_in_block * gobs_x + gob_x)
                * GOB_SIZE
                + row_in_gob * GOB_SIZE_X;

            // Destination offset in linear output.
            let dst_offset = y * stride_bytes + gob_x * GOB_SIZE_X;

            // How many bytes to copy (may be partial at the right edge).
            let copy_len = GOB_SIZE_X.min(stride_bytes.saturating_sub(gob_x * GOB_SIZE_X));

            if gob_offset + copy_len <= input.len() && dst_offset + copy_len <= output.len() {
                output[dst_offset..dst_offset + copy_len]
                    .copy_from_slice(&input[gob_offset..gob_offset + copy_len]);
            }
        }
    }

    output
}

/// Try to guess an appropriate block_height_log2 for a given height.
///
/// The block height should be chosen so that the number of GOB rows in a
/// block doesn't exceed the image height. Common values:
/// - 720p/768p → block_height_log2 = 4 (16 GOBs = 128 pixels)
/// - 1080p → block_height_log2 = 4
/// - 240p → block_height_log2 = 2 (4 GOBs = 32 pixels)
pub fn guess_block_height_log2(height: u32) -> u32 {
    let mut block_height_log2 = 4u32; // Start with maximum common value
    while block_height_log2 > 0 {
        let block_height_pixels = (1u32 << block_height_log2) * GOB_SIZE_Y as u32;
        if block_height_pixels <= height {
            return block_height_log2;
        }
        block_height_log2 -= 1;
    }
    0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gob_constants() {
        assert_eq!(GOB_SIZE_X, 64);
        assert_eq!(GOB_SIZE_Y, 8);
        assert_eq!(GOB_SIZE, 512);
    }

    #[test]
    fn test_guess_block_height() {
        assert_eq!(guess_block_height_log2(720), 4); // 128 <= 720
        assert_eq!(guess_block_height_log2(768), 4);
        assert_eq!(guess_block_height_log2(1080), 4);
        assert_eq!(guess_block_height_log2(64), 3);  // 64 <= 64
        assert_eq!(guess_block_height_log2(32), 2);  // 32 <= 32
        assert_eq!(guess_block_height_log2(8), 0);   // 8 <= 8
    }

    #[test]
    fn test_detile_single_gob() {
        // A single GOB: 64 bytes wide, 8 rows.
        // For a 16x8 image with bpp=4: stride = 64 bytes = 1 GOB wide.
        let width = 16u32;
        let height = 8u32;
        let bpp = 4u32;

        // Block linear with block_height_log2 = 0 (1 GOB per block) is just
        // one GOB, which is already linear within the GOB.
        let mut input = vec![0u8; GOB_SIZE];
        // Fill each row with its row number.
        for row in 0..8usize {
            for col in 0..64usize {
                input[row * 64 + col] = row as u8;
            }
        }

        let output = detile_block_linear(&input, width, height, bpp, 0);
        assert_eq!(output.len(), (width * height * bpp) as usize);

        // Verify each row has the correct value.
        for row in 0..8usize {
            assert_eq!(output[row * 64], row as u8);
        }
    }

    #[test]
    fn test_detile_preserves_size() {
        let width = 1280u32;
        let height = 720u32;
        let bpp = 4u32;
        let stride_bytes = width * bpp;
        let gobs_x = (stride_bytes as usize + GOB_SIZE_X - 1) / GOB_SIZE_X;
        let block_height = 1usize << 4; // block_height_log2 = 4
        let block_height_pixels = block_height * GOB_SIZE_Y;
        let block_rows = (height as usize + block_height_pixels - 1) / block_height_pixels;
        let tiled_size = block_rows * gobs_x * block_height * GOB_SIZE;

        let input = vec![0u8; tiled_size];
        let output = detile_block_linear(&input, width, height, bpp, 4);
        assert_eq!(output.len(), (width * height * bpp) as usize);
    }
}
