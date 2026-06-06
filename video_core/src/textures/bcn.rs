// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `video_core/textures/bcn.h` and `bcn.cpp`.
//!
//! BC1 and BC3 block compression for textures. The upstream implementation
//! uses the `stb_dxt` library for the actual block compression; this port
//! provides the same interface and dispatching logic.

// ── Types ────────────────────────────────────────────────────────────────────

/// Type alias for a BCN block compressor function.
///
/// Port of `BCNCompressor` typedef from `bcn.cpp`.
///
/// Parameters: `(block_output, block_input, any_alpha)`
type BcnCompressor = fn(block_output: &mut [u8], block_input: &[u8], any_alpha: bool);

extern "C" {
    fn ruzu_stb_compress_bc1_block(dest: *mut u8, src: *const u8, alpha: i32);
    fn ruzu_stb_compress_bc3_block(dest: *mut u8, src: *const u8);
}

// ── Constants ────────────────────────────────────────────────────────────────

const ALPHA_THRESHOLD: u8 = 128;
const BYTES_PER_PX: u32 = 4;

// ── Internal ─────────────────────────────────────────────────────────────────

/// Generic BCN compression dispatcher.
///
/// Port of the `CompressBCN<BytesPerBlock, ThresholdAlpha>` template from `bcn.cpp`.
///
/// Iterates over 4x4 blocks, gathers RGBA texels, and calls the compressor `f`.
fn compress_bcn<const BYTES_PER_BLOCK: u32, const THRESHOLD_ALPHA: bool>(
    data: &[u8],
    width: u32,
    height: u32,
    depth: u32,
    output: &mut [u8],
    f: BcnCompressor,
) {
    fn divide_up(a: u32, b: u32) -> u32 {
        (a + b - 1) / b
    }

    let plane_dim = width * height;

    for z in 0..depth {
        for y in (0..height).step_by(4) {
            // In upstream, each row is dispatched to the thread pool.
            // We process inline for now; workers integration is a future step.
            for x in (0..width).step_by(4) {
                // Gather 4x4 block of RGBA texels
                let mut input_colors = [[0u8; 4]; 16]; // 4x4 * 4 channels
                let mut any_alpha = false;

                for j in 0..4u32 {
                    for i in 0..4u32 {
                        let coord = (z * plane_dim + (y + j) * width + (x + i)) as usize
                            * BYTES_PER_PX as usize;
                        let dst_idx = (j * 4 + i) as usize;

                        if (x + i < width) && (y + j < height) {
                            if THRESHOLD_ALPHA {
                                if coord + 3 < data.len() && data[coord + 3] >= ALPHA_THRESHOLD {
                                    input_colors[dst_idx][0] = data[coord];
                                    input_colors[dst_idx][1] = data[coord + 1];
                                    input_colors[dst_idx][2] = data[coord + 2];
                                    input_colors[dst_idx][3] = 255;
                                } else {
                                    any_alpha = true;
                                    input_colors[dst_idx] = [0, 0, 0, 0];
                                }
                            } else if coord + BYTES_PER_PX as usize <= data.len() {
                                input_colors[dst_idx][0] = data[coord];
                                input_colors[dst_idx][1] = data[coord + 1];
                                input_colors[dst_idx][2] = data[coord + 2];
                                input_colors[dst_idx][3] = data[coord + 3];
                            }
                        }
                        // Else: already zero-initialized
                    }
                }

                let bytes_per_row = BYTES_PER_BLOCK * divide_up(width, 4);
                let bytes_per_plane = bytes_per_row * divide_up(height, 4);
                let offset = (z * bytes_per_plane
                    + (y / 4) * bytes_per_row
                    + (x / 4) * BYTES_PER_BLOCK) as usize;

                // Flatten input_colors for the compressor
                let flat_input: Vec<u8> = input_colors
                    .iter()
                    .flat_map(|c| c.iter().copied())
                    .collect();

                if offset + BYTES_PER_BLOCK as usize <= output.len() {
                    let out_slice = &mut output[offset..offset + BYTES_PER_BLOCK as usize];
                    f(out_slice, &flat_input, any_alpha);
                }
            }
        }
    }
}

// ── Public API ───────────────────────────────────────────────────────────────

/// Compress RGBA8 data into BC1 format.
///
/// Port of `Tegra::Texture::BCN::CompressBC1`.
pub fn compress_bc1(data: &[u8], width: u32, height: u32, depth: u32, output: &mut [u8]) {
    compress_bcn::<8, true>(
        data,
        width,
        height,
        depth,
        output,
        |block_output, block_input, any_alpha| unsafe {
            ruzu_stb_compress_bc1_block(
                block_output.as_mut_ptr(),
                block_input.as_ptr(),
                any_alpha as i32,
            );
        },
    );
}

/// Compress RGBA8 data into BC3 format.
///
/// Port of `Tegra::Texture::BCN::CompressBC3`.
pub fn compress_bc3(data: &[u8], width: u32, height: u32, depth: u32, output: &mut [u8]) {
    compress_bcn::<16, false>(
        data,
        width,
        height,
        depth,
        output,
        |block_output, block_input, _any_alpha| unsafe {
            ruzu_stb_compress_bc3_block(block_output.as_mut_ptr(), block_input.as_ptr());
        },
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn divide_up_basic() {
        fn divide_up(a: u32, b: u32) -> u32 {
            (a + b - 1) / b
        }
        assert_eq!(divide_up(7, 4), 2);
        assert_eq!(divide_up(8, 4), 2);
        assert_eq!(divide_up(9, 4), 3);
    }

    #[test]
    fn bc1_uses_stb_dxt_output() {
        let mut rgba = vec![0u8; 4 * 4 * 4];
        for pixel in rgba.chunks_exact_mut(4) {
            pixel.copy_from_slice(&[255, 0, 0, 255]);
        }
        let mut output = [0u8; 8];
        compress_bc1(&rgba, 4, 4, 1, &mut output);
        assert_ne!(output, [0u8; 8]);
    }

    #[test]
    fn bc3_uses_stb_dxt_output() {
        let mut rgba = vec![0u8; 4 * 4 * 4];
        for (index, pixel) in rgba.chunks_exact_mut(4).enumerate() {
            pixel.copy_from_slice(&[0, 255, 0, (index * 17) as u8]);
        }
        let mut output = [0u8; 16];
        compress_bc3(&rgba, 4, 4, 1, &mut output);
        assert_ne!(output, [0u8; 16]);
    }
}
