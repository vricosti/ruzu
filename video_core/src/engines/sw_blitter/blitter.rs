// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of video_core/engines/sw_blitter/blitter.h and blitter.cpp
//!
//! Software blit engine that performs 2D surface copies entirely on the CPU.
//! Supports nearest-neighbor and bilinear filtering, pitch-linear and
//! block-linear memory layouts, and format conversion via the converter module.

use super::converter::ConverterFactory;

// ── Constants ───────────────────────────────────────────────────────────────

/// Number of components in the intermediate representation (RGBA f32).
const IR_COMPONENTS: usize = 4;

// ── Filtering helpers ───────────────────────────────────────────────────────

/// Nearest-neighbor scaling for raw byte data.
///
/// Corresponds to the anonymous namespace `NearestNeighbor` function.
fn nearest_neighbor(
    input: &[u8],
    output: &mut [u8],
    src_width: u32,
    src_height: u32,
    dst_width: u32,
    dst_height: u32,
    bpp: usize,
) {
    let dx_du = ((src_width as f64 / dst_width as f64) * ((1u64 << 32) as f64)).round() as usize;
    let dy_dv = ((src_height as f64 / dst_height as f64) * ((1u64 << 32) as f64)).round() as usize;
    let mut src_y: usize = 0;
    for y in 0..dst_height as usize {
        let mut src_x: usize = 0;
        for x in 0..dst_width as usize {
            let read_from = ((src_y * src_width as usize + src_x) >> 32) * bpp;
            let write_to = (y * dst_width as usize + x) * bpp;
            output[write_to..write_to + bpp].copy_from_slice(&input[read_from..read_from + bpp]);
            src_x += dx_du;
        }
        src_y += dy_dv;
    }
}

/// Nearest-neighbor scaling for f32 intermediate-representation data.
///
/// Corresponds to the anonymous namespace `NearestNeighborFast` function.
fn nearest_neighbor_fast(
    input: &[f32],
    output: &mut [f32],
    src_width: u32,
    src_height: u32,
    dst_width: u32,
    dst_height: u32,
) {
    let dx_du = ((src_width as f64 / dst_width as f64) * ((1u64 << 32) as f64)).round() as usize;
    let dy_dv = ((src_height as f64 / dst_height as f64) * ((1u64 << 32) as f64)).round() as usize;
    let mut src_y: usize = 0;
    for y in 0..dst_height as usize {
        let mut src_x: usize = 0;
        for x in 0..dst_width as usize {
            let read_from = ((src_y * src_width as usize + src_x) >> 32) * IR_COMPONENTS;
            let write_to = (y * dst_width as usize + x) * IR_COMPONENTS;
            output[write_to..write_to + IR_COMPONENTS]
                .copy_from_slice(&input[read_from..read_from + IR_COMPONENTS]);
            src_x += dx_du;
        }
        src_y += dy_dv;
    }
}

/// Bilinear filtering for f32 intermediate-representation data.
///
/// Corresponds to the anonymous namespace `Bilinear` function.
fn bilinear(
    input: &[f32],
    output: &mut [f32],
    src_width: usize,
    src_height: usize,
    dst_width: usize,
    dst_height: usize,
) {
    let dx_du = if dst_width > 1 {
        (src_width - 1) as f32 / (dst_width - 1) as f32
    } else {
        0.0f32
    };
    let dy_dv = if dst_height > 1 {
        (src_height - 1) as f32 / (dst_height - 1) as f32
    } else {
        0.0f32
    };

    for y in 0..dst_height as u32 {
        for x in 0..dst_width as u32 {
            let x_low = (x as f32 * dx_du).floor();
            let y_low = (y as f32 * dy_dv).floor();
            let x_high = (x as f32 * dx_du).ceil();
            let y_high = (y as f32 * dy_dv).ceil();
            let weight_x = (x as f32 * dx_du) - x_low;
            let weight_y = (y as f32 * dy_dv) - y_low;

            let read_src = |in_x: f32, in_y: f32| -> usize {
                ((in_x as usize * src_width + in_y as usize) >> 32) * IR_COMPONENTS
            };

            let off_00 = read_src(x_low, y_low);
            let off_10 = read_src(x_high, y_low);
            let off_01 = read_src(x_low, y_high);
            let off_11 = read_src(x_high, y_high);

            let write_to = (y as usize * dst_width + x as usize) * IR_COMPONENTS;

            for i in 0..IR_COMPONENTS {
                let a = lerp(input[off_00 + i], input[off_10 + i], weight_x);
                let b = lerp(input[off_01 + i], input[off_11 + i], weight_x);
                output[write_to + i] = lerp(a, b, weight_y);
            }
        }
    }
}

/// Linear interpolation helper (matching `std::lerp`).
#[inline]
fn lerp(a: f32, b: f32, t: f32) -> f32 {
    a + t * (b - a)
}

/// Process pitch-linear surface data (pack or unpack).
///
/// When `unpack` is true, copies from a compact buffer into a pitched surface.
/// When `unpack` is false, copies from a pitched surface into a compact buffer.
///
/// Corresponds to the template `ProcessPitchLinear<bool>` function.
fn process_pitch_linear(
    input: &[u8],
    output: &mut [u8],
    extent_x: usize,
    extent_y: usize,
    pitch: u32,
    x0: u32,
    y0: u32,
    bpp: usize,
    unpack: bool,
) {
    let base_offset = x0 as usize * bpp;
    let copy_size = extent_x * bpp;
    for y in 0..extent_y {
        let first_offset = (y + y0 as usize) * pitch as usize + base_offset;
        let second_offset = y * extent_x * bpp;
        if unpack {
            output[first_offset..first_offset + copy_size]
                .copy_from_slice(&input[second_offset..second_offset + copy_size]);
        } else {
            output[second_offset..second_offset + copy_size]
                .copy_from_slice(&input[first_offset..first_offset + copy_size]);
        }
    }
}

// ── SoftwareBlitEngine ──────────────────────────────────────────────────────

/// Internal implementation state for the software blit engine.
struct BlitEngineImpl {
    tmp_buffer: Vec<u8>,
    src_buffer: Vec<u8>,
    dst_buffer: Vec<u8>,
    intermediate_src: Vec<f32>,
    intermediate_dst: Vec<f32>,
    converter_factory: ConverterFactory,
}

/// Software blit engine.
///
/// Corresponds to the C++ `SoftwareBlitEngine` class.
/// Provides CPU-side 2D surface blitting with format conversion and scaling.
pub struct SoftwareBlitEngine {
    _impl: BlitEngineImpl,
}

impl SoftwareBlitEngine {
    /// Create a new software blit engine.
    pub fn new() -> Self {
        Self {
            _impl: BlitEngineImpl {
                tmp_buffer: Vec::new(),
                src_buffer: Vec::new(),
                dst_buffer: Vec::new(),
                intermediate_src: Vec::new(),
                intermediate_dst: Vec::new(),
                converter_factory: ConverterFactory::new(),
            },
        }
    }

    /// Perform a 2D surface blit.
    ///
    /// Corresponds to `SoftwareBlitEngine::Blit`.
    /// Stubbed — full implementation requires memory manager access to read/write surface
    /// pixels, surface format definitions, and texture decoder integration for format conversion.
    /// Upstream: SoftwareBlitEngine::Blit() in video_core/engines/sw_blitter/blitter.cpp
    pub fn blit(&mut self, _src: &Surface, _dst: &Surface, _config: &BlitConfig) -> bool {
        log::warn!("SoftwareBlitEngine::blit: not yet implemented (requires memory manager and format integration)");
        false
    }
}

impl Default for SoftwareBlitEngine {
    fn default() -> Self {
        Self::new()
    }
}

// ── Surface / Config types used by Blit ─────────────────────────────────────

/// Memory layout for a 2D surface.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemoryLayout {
    BlockLinear,
    Pitch,
}

/// Surface descriptor for blit operations.
///
/// Mirrors `Fermi2D::Surface` fields.
#[derive(Debug, Clone)]
pub struct Surface {
    pub format: u32, // RenderTargetFormat raw value
    pub linear: MemoryLayout,
    pub width: u32,
    pub height: u32,
    pub depth: u32,
    pub pitch: u32,
    pub block_height: u32,
    pub block_depth: u32,
    pub address: u64,
}

/// Blit configuration parameters.
///
/// Mirrors `Fermi2D::Config` fields.
#[derive(Debug, Clone, Copy)]
pub struct BlitConfig {
    pub src_x0: u32,
    pub src_y0: u32,
    pub src_x1: u32,
    pub src_y1: u32,
    pub dst_x0: u32,
    pub dst_y0: u32,
    pub dst_x1: u32,
    pub dst_y1: u32,
    pub filter: BlitFilter,
}

/// Blit filter mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BlitFilter {
    PointSample,
    Bilinear,
}

// Keep the private helper functions accessible for testing.
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lerp() {
        assert!((lerp(0.0, 1.0, 0.5) - 0.5).abs() < 1e-6);
        assert!((lerp(2.0, 4.0, 0.25) - 2.5).abs() < 1e-6);
    }
}
