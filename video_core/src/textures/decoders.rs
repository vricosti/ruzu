// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `video_core/textures/decoders.h` and `decoders.cpp`.
//!
//! Tegra block-linear (GOB-based) texture swizzle/unswizzle routines.

// ── GOB Constants ────────────────────────────────────────────────────────────

/// GOB (Graphics Operation Block) X dimension in bytes.
pub const GOB_SIZE_X: u32 = 64;
/// GOB Y dimension in rows.
pub const GOB_SIZE_Y: u32 = 8;
/// GOB Z dimension in slices.
pub const GOB_SIZE_Z: u32 = 1;
/// Total GOB size in bytes.
pub const GOB_SIZE: u32 = GOB_SIZE_X * GOB_SIZE_Y * GOB_SIZE_Z;

pub const GOB_SIZE_X_SHIFT: u32 = 6;
pub const GOB_SIZE_Y_SHIFT: u32 = 3;
pub const GOB_SIZE_Z_SHIFT: u32 = 0;
pub const GOB_SIZE_SHIFT: u32 = GOB_SIZE_X_SHIFT + GOB_SIZE_Y_SHIFT + GOB_SIZE_Z_SHIFT;

/// Internal swizzle bitmask for X coordinate within a GOB.
pub const SWIZZLE_X_BITS: u32 = 0b100101111;
/// Internal swizzle bitmask for Y coordinate within a GOB.
pub const SWIZZLE_Y_BITS: u32 = 0b011010000;

// ── Swizzle Table ────────────────────────────────────────────────────────────

/// Type alias for the GOB internal swizzle table.
pub type SwizzleTable = [[u32; GOB_SIZE_X as usize]; GOB_SIZE_Y as usize];

/// Generates the internal swizzle table for a GOB.
///
/// Port of `MakeSwizzleTable()` from `decoders.h`.
///
/// This table represents the internal swizzle of a gob, in format 16 bytes x 2
/// sector packing. Calculates the offset of an (x, y) position within a
/// swizzled texture. Taken from the Tegra X1 Technical Reference Manual,
/// pages 1187-1188.
pub const fn make_swizzle_table() -> SwizzleTable {
    let mut table = [[0u32; GOB_SIZE_X as usize]; GOB_SIZE_Y as usize];
    let mut y = 0u32;
    while y < GOB_SIZE_Y {
        let mut x = 0u32;
        while x < GOB_SIZE_X {
            table[y as usize][x as usize] = ((x % 64) / 32) * 256
                + ((y % 8) / 2) * 64
                + ((x % 32) / 16) * 32
                + (y % 2) * 16
                + (x % 16);
            x += 1;
        }
        y += 1;
    }
    table
}

// ── Helper: pdep (parallel bit deposit) ──────────────────────────────────────

/// Parallel bit deposit — deposits bits of `value` at positions specified by `mask`.
///
/// Port of the `pdep<mask>(value)` template from `decoders.cpp`.
const fn pdep(mask: u32, value: u32) -> u32 {
    let mut result = 0u32;
    let mut m = mask;
    let mut bit = 1u32;
    while m != 0 {
        if value & bit != 0 {
            result |= m & m.wrapping_neg(); // m & (~m + 1)
        }
        m &= m.wrapping_sub(1);
        bit = bit.wrapping_add(bit);
    }
    result
}

/// Increment a pdep-encoded value by `incr_amount` within the given `mask`.
///
/// Port of `incrpdep<mask, incr_amount>(value)` from `decoders.cpp`.
fn incrpdep(value: &mut u32, mask: u32, swizzled_incr: u32) {
    *value = ((*value | !mask).wrapping_add(swizzled_incr)) & mask;
}

// ── Helper: alignment ────────────────────────────────────────────────────────

fn align_up_log2(value: u32, alignment_log2: u32) -> u32 {
    let mask = (1u32 << alignment_log2) - 1;
    (value + mask) & !mask
}

fn div_ceil_log2(value: u32, shift: u32) -> u32 {
    let mask = (1u32 << shift) - 1;
    (value + mask) >> shift
}

// ── Swizzle implementation ───────────────────────────────────────────────────

/// Core swizzle/unswizzle implementation for a given bytes-per-pixel.
///
/// Port of `SwizzleImpl<TO_LINEAR, BYTES_PER_PIXEL>` from `decoders.cpp`.
fn swizzle_impl(
    output: &mut [u8],
    input: &[u8],
    bytes_per_pixel: u32,
    width: u32,
    height: u32,
    depth: u32,
    block_height: u32,
    block_depth: u32,
    stride: u32,
    to_linear: bool,
) {
    let origin_x: u32 = 0;
    let origin_y: u32 = 0;
    let origin_z: u32 = 0;

    let pitch = width * bytes_per_pixel;
    let gobs_in_x = div_ceil_log2(stride, GOB_SIZE_X_SHIFT);
    let block_size = gobs_in_x << (GOB_SIZE_SHIFT + block_height + block_depth);
    let slice_size = div_ceil_log2(height, block_height + GOB_SIZE_Y_SHIFT) * block_size;

    let block_height_mask = (1u32 << block_height) - 1;
    let block_depth_mask = (1u32 << block_depth) - 1;
    let x_shift = GOB_SIZE_SHIFT + block_height + block_depth;

    let swizzled_incr = pdep(SWIZZLE_X_BITS, bytes_per_pixel);

    for slice in 0..depth {
        let z = slice + origin_z;
        let offset_z = (z >> block_depth) * slice_size
            + ((z & block_depth_mask) << (GOB_SIZE_SHIFT + block_height));

        for line in 0..height {
            let y = line + origin_y;
            let swizzled_y = pdep(SWIZZLE_Y_BITS, y);
            let block_y = y >> GOB_SIZE_Y_SHIFT;
            let offset_y = (block_y >> block_height) * block_size
                + ((block_y & block_height_mask) << GOB_SIZE_SHIFT);

            let mut swizzled_x = pdep(SWIZZLE_X_BITS, origin_x * bytes_per_pixel);

            for column in 0..width {
                let x = (column + origin_x) * bytes_per_pixel;
                let offset_x = (x >> GOB_SIZE_X_SHIFT) << x_shift;

                let base_swizzled_offset = offset_z + offset_y + offset_x;
                let swizzled_offset = (base_swizzled_offset + (swizzled_x | swizzled_y)) as usize;
                let unswizzled_offset =
                    (slice * pitch * height + line * pitch + column * bytes_per_pixel) as usize;

                let bpp = bytes_per_pixel as usize;
                if to_linear {
                    if swizzled_offset + bpp <= output.len()
                        && unswizzled_offset + bpp <= input.len()
                    {
                        output[swizzled_offset..swizzled_offset + bpp]
                            .copy_from_slice(&input[unswizzled_offset..unswizzled_offset + bpp]);
                    }
                } else {
                    if unswizzled_offset + bpp <= output.len()
                        && swizzled_offset + bpp <= input.len()
                    {
                        output[unswizzled_offset..unswizzled_offset + bpp]
                            .copy_from_slice(&input[swizzled_offset..swizzled_offset + bpp]);
                    }
                }

                incrpdep(&mut swizzled_x, SWIZZLE_X_BITS, swizzled_incr);
            }
        }
    }
}

/// BPP dispatch for swizzle operations.
///
/// Port of the `Swizzle<TO_LINEAR>` function from `decoders.cpp`.
fn swizzle_dispatch(
    output: &mut [u8],
    input: &[u8],
    bytes_per_pixel: u32,
    width: u32,
    height: u32,
    depth: u32,
    block_height: u32,
    block_depth: u32,
    stride_alignment: u32,
    to_linear: bool,
) {
    match bytes_per_pixel {
        1 | 2 | 3 | 4 | 6 | 8 | 12 | 16 => {
            swizzle_impl(
                output,
                input,
                bytes_per_pixel,
                width,
                height,
                depth,
                block_height,
                block_depth,
                stride_alignment,
                to_linear,
            );
        }
        _ => panic!("Invalid bytes_per_pixel={}", bytes_per_pixel),
    }
}

// ── Public API ───────────────────────────────────────────────────────────────

/// Unswizzles a block linear texture into linear memory.
///
/// Port of `Tegra::Texture::UnswizzleTexture`.
pub fn unswizzle_texture(
    output: &mut [u8],
    input: &[u8],
    mut bytes_per_pixel: u32,
    mut width: u32,
    height: u32,
    depth: u32,
    block_height: u32,
    block_depth: u32,
    stride_alignment: u32,
) {
    let stride = align_up_log2(width, stride_alignment) * bytes_per_pixel;
    let new_bpp = std::cmp::min(4, (width * bytes_per_pixel).trailing_zeros());
    width = (width * bytes_per_pixel) >> new_bpp;
    bytes_per_pixel = 1u32 << new_bpp;
    swizzle_dispatch(
        output,
        input,
        bytes_per_pixel,
        width,
        height,
        depth,
        block_height,
        block_depth,
        stride,
        false,
    );
}

/// Swizzles linear memory into a block linear texture.
///
/// Port of `Tegra::Texture::SwizzleTexture`.
pub fn swizzle_texture(
    output: &mut [u8],
    input: &[u8],
    mut bytes_per_pixel: u32,
    mut width: u32,
    height: u32,
    depth: u32,
    block_height: u32,
    block_depth: u32,
    stride_alignment: u32,
) {
    let stride = align_up_log2(width, stride_alignment) * bytes_per_pixel;
    let new_bpp = std::cmp::min(4, (width * bytes_per_pixel).trailing_zeros());
    width = (width * bytes_per_pixel) >> new_bpp;
    bytes_per_pixel = 1u32 << new_bpp;
    swizzle_dispatch(
        output,
        input,
        bytes_per_pixel,
        width,
        height,
        depth,
        block_height,
        block_depth,
        stride,
        true,
    );
}

/// Copies an untiled subrectangle into a tiled surface.
///
/// Port of `Tegra::Texture::SwizzleSubrect`.
pub fn swizzle_subrect(
    _output: &mut [u8],
    _input: &[u8],
    _bytes_per_pixel: u32,
    _width: u32,
    _height: u32,
    _depth: u32,
    _origin_x: u32,
    _origin_y: u32,
    _extent_x: u32,
    _extent_y: u32,
    _block_height: u32,
    _block_depth: u32,
    _pitch_linear: u32,
) {
    todo!("SwizzleSubrect not yet implemented")
}

/// Copies a tiled subrectangle into a linear surface.
///
/// Port of `Tegra::Texture::UnswizzleSubrect`.
pub fn unswizzle_subrect(
    _output: &mut [u8],
    _input: &[u8],
    _bytes_per_pixel: u32,
    _width: u32,
    _height: u32,
    _depth: u32,
    _origin_x: u32,
    _origin_y: u32,
    _extent_x: u32,
    _extent_y: u32,
    _block_height: u32,
    _block_depth: u32,
    _pitch_linear: u32,
) {
    todo!("UnswizzleSubrect not yet implemented")
}

/// Calculates the correct size of a texture depending on whether it's tiled or not.
///
/// Port of `Tegra::Texture::CalculateSize`.
pub fn calculate_size(
    tiled: bool,
    bytes_per_pixel: u32,
    width: u32,
    height: u32,
    depth: u32,
    block_height: u32,
    block_depth: u32,
) -> usize {
    if tiled {
        let aligned_width = align_up_log2(width * bytes_per_pixel, GOB_SIZE_X_SHIFT);
        let aligned_height = align_up_log2(height, GOB_SIZE_Y_SHIFT + block_height);
        let aligned_depth = align_up_log2(depth, GOB_SIZE_Z_SHIFT + block_depth);
        (aligned_width * aligned_height * aligned_depth) as usize
    } else {
        (width * height * depth * bytes_per_pixel) as usize
    }
}

/// Obtains the offset of the GOB for positions `dst_x` & `dst_y`.
///
/// Port of `Tegra::Texture::GetGOBOffset`.
pub fn get_gob_offset(
    width: u32,
    _height: u32,
    dst_x: u32,
    dst_y: u32,
    block_height: u32,
    bytes_per_pixel: u32,
) -> u64 {
    let div_ceil = |x: u32, y: u32| (x + y - 1) / y;
    let gobs_in_block = 1u32 << block_height;
    let y_blocks = GOB_SIZE_Y << block_height;
    let x_per_gob = GOB_SIZE_X / bytes_per_pixel;
    let x_blocks = div_ceil(width, x_per_gob);
    let block_size = GOB_SIZE * gobs_in_block;
    let stride = block_size * x_blocks;
    let base = (dst_y / y_blocks) * stride + (dst_x / x_per_gob) * block_size;
    let relative_y = dst_y % y_blocks;
    (base + (relative_y / GOB_SIZE_Y) * GOB_SIZE) as u64
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn gob_constants() {
        assert_eq!(GOB_SIZE, 512);
        assert_eq!(GOB_SIZE_X, 64);
        assert_eq!(GOB_SIZE_Y, 8);
        assert_eq!(GOB_SIZE_Z, 1);
    }

    #[test]
    fn swizzle_table_basic() {
        let table = make_swizzle_table();
        // Position (0,0) should be 0
        assert_eq!(table[0][0], 0);
        // Table should be GOB_SIZE_Y x GOB_SIZE_X
        assert_eq!(table.len(), GOB_SIZE_Y as usize);
        assert_eq!(table[0].len(), GOB_SIZE_X as usize);
    }

    #[test]
    fn calculate_size_linear() {
        let size = calculate_size(false, 4, 64, 64, 1, 0, 0);
        assert_eq!(size, 64 * 64 * 4);
    }

    #[test]
    fn calculate_size_tiled() {
        let size = calculate_size(true, 4, 64, 64, 1, 0, 0);
        // Should be aligned to GOB boundaries
        assert!(size >= 64 * 64 * 4);
    }

    #[test]
    fn pdep_basic() {
        // pdep with mask 0b1111 and value 0b1010 = deposit into first 4 bit positions
        assert_eq!(pdep(0b1111, 0b1010), 0b1010);
        // pdep with mask 0b10101 and value 0b111 = deposit bits at positions 0, 2, 4
        assert_eq!(pdep(0b10101, 0b111), 0b10101);
    }
}
