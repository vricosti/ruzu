// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of video_core/engines/sw_blitter/blitter.h and blitter.cpp
//!
//! Software blit engine that performs 2D surface copies entirely on the CPU.
//! Supports nearest-neighbor and bilinear filtering, pitch-linear and
//! block-linear memory layouts, and format conversion via the converter module.

use super::converter::ConverterFactory;
use crate::engines::fermi_2d::{Config, Filter, MemoryLayout, Surface};
use crate::gpu::RenderTargetFormat;
use crate::memory_manager::MemoryManager;
use crate::textures::decoders;
use parking_lot::Mutex;
use std::sync::Arc;

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
    memory_manager: Arc<Mutex<MemoryManager>>,
    imp: BlitEngineImpl,
}

impl SoftwareBlitEngine {
    /// Create a new software blit engine.
    pub fn new(memory_manager: Arc<Mutex<MemoryManager>>) -> Self {
        Self {
            memory_manager,
            imp: BlitEngineImpl {
                tmp_buffer: Vec::new(),
                src_buffer: Vec::new(),
                dst_buffer: Vec::new(),
                intermediate_src: Vec::new(),
                intermediate_dst: Vec::new(),
                converter_factory: ConverterFactory::new(),
            },
        }
    }

    fn bytes_per_pixel_from_render_target_format(format: RenderTargetFormat) -> u32 {
        match format {
            RenderTargetFormat::R32G32B32A32Float
            | RenderTargetFormat::R32G32B32A32Sint
            | RenderTargetFormat::R32G32B32A32Uint
            | RenderTargetFormat::R32G32B32X32Float
            | RenderTargetFormat::R32G32B32X32Sint
            | RenderTargetFormat::R32G32B32X32Uint => 16,
            RenderTargetFormat::R16G16B16A16Unorm
            | RenderTargetFormat::R16G16B16A16Snorm
            | RenderTargetFormat::R16G16B16A16Sint
            | RenderTargetFormat::R16G16B16A16Uint
            | RenderTargetFormat::R16G16B16A16Float
            | RenderTargetFormat::R32G32Float
            | RenderTargetFormat::R32G32Sint
            | RenderTargetFormat::R32G32Uint
            | RenderTargetFormat::R16G16B16X16Float => 8,
            RenderTargetFormat::A8R8G8B8Unorm
            | RenderTargetFormat::A8R8G8B8Srgb
            | RenderTargetFormat::A2B10G10R10Unorm
            | RenderTargetFormat::A2B10G10R10Uint
            | RenderTargetFormat::A8B8G8R8Unorm
            | RenderTargetFormat::A8B8G8R8Srgb
            | RenderTargetFormat::A8B8G8R8Snorm
            | RenderTargetFormat::A8B8G8R8Sint
            | RenderTargetFormat::A8B8G8R8Uint
            | RenderTargetFormat::R16G16Unorm
            | RenderTargetFormat::R16G16Snorm
            | RenderTargetFormat::R16G16Sint
            | RenderTargetFormat::R16G16Uint
            | RenderTargetFormat::R16G16Float
            | RenderTargetFormat::A2R10G10B10Unorm
            | RenderTargetFormat::B10G11R11Float
            | RenderTargetFormat::R32Sint
            | RenderTargetFormat::R32Uint
            | RenderTargetFormat::R32Float
            | RenderTargetFormat::X8R8G8B8Unorm
            | RenderTargetFormat::X8R8G8B8Srgb
            | RenderTargetFormat::X1R5G5B5Unorm
            | RenderTargetFormat::X8B8G8R8Unorm
            | RenderTargetFormat::X8B8G8R8Srgb => 4,
            RenderTargetFormat::R5G6B5Unorm
            | RenderTargetFormat::A1R5G5B5Unorm
            | RenderTargetFormat::R8G8Unorm
            | RenderTargetFormat::R8G8Snorm
            | RenderTargetFormat::R8G8Sint
            | RenderTargetFormat::R8G8Uint
            | RenderTargetFormat::R16Unorm
            | RenderTargetFormat::R16Snorm
            | RenderTargetFormat::R16Sint
            | RenderTargetFormat::R16Uint
            | RenderTargetFormat::R16Float => 2,
            RenderTargetFormat::R8Unorm
            | RenderTargetFormat::R8Snorm
            | RenderTargetFormat::R8Sint
            | RenderTargetFormat::R8Uint => 1,
            RenderTargetFormat::None => 0,
        }
    }

    fn get_surface_size(surface: &Surface, bytes_per_pixel: u32) -> usize {
        if surface.linear == MemoryLayout::BlockLinear {
            decoders::calculate_size(
                true,
                bytes_per_pixel,
                surface.width,
                surface.height,
                surface.depth,
                surface.block_height(),
                surface.block_depth(),
            )
        } else {
            (surface.pitch * surface.height) as usize
        }
    }

    /// Upstream `SoftwareBlitEngine::Blit`.
    ///
    /// This owner now performs both the read and write phases through the bound `MemoryManager`,
    /// which is the closest Rust equivalent to the upstream `GpuGuestMemory` /
    /// `GpuGuestMemoryScoped` owner path.
    pub fn blit(&mut self, src: &Surface, dst: &Surface, config: &Config) -> bool {
        let Some(src_dx) = config.src_x1.checked_sub(config.src_x0) else {
            return false;
        };
        let Some(src_dy) = config.src_y1.checked_sub(config.src_y0) else {
            return false;
        };
        let Some(dst_dx) = config.dst_x1.checked_sub(config.dst_x0) else {
            return false;
        };
        let Some(dst_dy) = config.dst_y1.checked_sub(config.dst_y0) else {
            return false;
        };
        let Ok(src_extent_x) = u32::try_from(src_dx) else {
            return false;
        };
        let Ok(src_extent_y) = u32::try_from(src_dy) else {
            return false;
        };
        let Ok(dst_extent_x) = u32::try_from(dst_dx) else {
            return false;
        };
        let Ok(dst_extent_y) = u32::try_from(dst_dy) else {
            return false;
        };
        let Ok(src_x0) = u32::try_from(config.src_x0) else {
            return false;
        };
        let Ok(src_y0) = u32::try_from(config.src_y0) else {
            return false;
        };
        let Ok(dst_x0) = u32::try_from(config.dst_x0) else {
            return false;
        };
        let Ok(dst_y0) = u32::try_from(config.dst_y0) else {
            return false;
        };

        let src_bytes_per_pixel = Self::bytes_per_pixel_from_render_target_format(src.format);
        let dst_bytes_per_pixel = Self::bytes_per_pixel_from_render_target_format(dst.format);
        if src_bytes_per_pixel == 0 || dst_bytes_per_pixel == 0 {
            return false;
        }

        let src_size = Self::get_surface_size(src, src_bytes_per_pixel);
        self.imp.tmp_buffer.resize(src_size, 0);
        if !self
            .memory_manager
            .lock()
            .read_block_owned(src.address(), &mut self.imp.tmp_buffer)
        {
            return false;
        }

        let src_copy_size = (src_extent_x * src_extent_y * src_bytes_per_pixel) as usize;
        let dst_copy_size = (dst_extent_x * dst_extent_y * dst_bytes_per_pixel) as usize;
        self.imp.src_buffer.resize(src_copy_size, 0);
        self.imp.dst_buffer.resize(dst_copy_size, 0);

        let no_passthrough = src.format != dst.format
            || src_extent_x != dst_extent_x
            || src_extent_y != dst_extent_y;

        if src.linear == MemoryLayout::BlockLinear {
            decoders::unswizzle_subrect(
                &mut self.imp.src_buffer,
                &self.imp.tmp_buffer,
                src_bytes_per_pixel,
                src.width,
                src.height,
                src.depth,
                src_x0,
                src_y0,
                src_extent_x,
                src_extent_y,
                src.block_height(),
                src.block_depth(),
                src_extent_x * src_bytes_per_pixel,
            );
        } else {
            process_pitch_linear(
                &self.imp.tmp_buffer,
                &mut self.imp.src_buffer,
                src_extent_x as usize,
                src_extent_y as usize,
                src.pitch,
                src_x0,
                src_y0,
                src_bytes_per_pixel as usize,
                false,
            );
        }

        if no_passthrough {
            if src.format != dst.format || config.filter == Filter::Bilinear {
                let input_converter = self
                    .imp
                    .converter_factory
                    .get_format_converter(src.format as u32);
                self.imp.intermediate_src.resize(
                    (src_copy_size / src_bytes_per_pixel as usize) * IR_COMPONENTS,
                    0.0,
                );
                self.imp.intermediate_dst.resize(
                    (dst_copy_size / dst_bytes_per_pixel as usize) * IR_COMPONENTS,
                    0.0,
                );
                input_converter.convert_to(&self.imp.src_buffer, &mut self.imp.intermediate_src);

                if config.filter != Filter::Bilinear {
                    nearest_neighbor_fast(
                        &self.imp.intermediate_src,
                        &mut self.imp.intermediate_dst,
                        src_extent_x,
                        src_extent_y,
                        dst_extent_x,
                        dst_extent_y,
                    );
                } else {
                    bilinear(
                        &self.imp.intermediate_src,
                        &mut self.imp.intermediate_dst,
                        src_extent_x as usize,
                        src_extent_y as usize,
                        dst_extent_x as usize,
                        dst_extent_y as usize,
                    );
                }

                let output_converter = self
                    .imp
                    .converter_factory
                    .get_format_converter(dst.format as u32);
                output_converter.convert_from(&self.imp.intermediate_dst, &mut self.imp.dst_buffer);
            } else {
                nearest_neighbor(
                    &self.imp.src_buffer,
                    &mut self.imp.dst_buffer,
                    src_extent_x,
                    src_extent_y,
                    dst_extent_x,
                    dst_extent_y,
                    dst_bytes_per_pixel as usize,
                );
            }
        } else {
            std::mem::swap(&mut self.imp.dst_buffer, &mut self.imp.src_buffer);
        }

        let dst_size = Self::get_surface_size(dst, dst_bytes_per_pixel);
        self.imp.tmp_buffer.resize(dst_size, 0);
        if !self
            .memory_manager
            .lock()
            .read_block_owned(dst.address(), &mut self.imp.tmp_buffer)
        {
            return false;
        }

        if dst.linear == MemoryLayout::BlockLinear {
            decoders::swizzle_subrect(
                &mut self.imp.tmp_buffer,
                &self.imp.dst_buffer,
                dst_bytes_per_pixel,
                dst.width,
                dst.height,
                dst.depth,
                dst_x0,
                dst_y0,
                dst_extent_x,
                dst_extent_y,
                dst.block_height(),
                dst.block_depth(),
                dst_extent_x * dst_bytes_per_pixel,
            );
        } else {
            process_pitch_linear(
                &self.imp.dst_buffer,
                &mut self.imp.tmp_buffer,
                dst_extent_x as usize,
                dst_extent_y as usize,
                dst.pitch,
                dst_x0,
                dst_y0,
                dst_bytes_per_pixel as usize,
                true,
            );
        }

        self.memory_manager
            .lock()
            .write_block_owned(dst.address(), &self.imp.tmp_buffer);
        true
    }
}

impl Default for SoftwareBlitEngine {
    fn default() -> Self {
        Self::new(Arc::new(Mutex::new(MemoryManager::new(0))))
    }
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

    #[test]
    fn blit_writes_pitch_linear_surface_through_memory_manager() {
        let mut blitter = SoftwareBlitEngine::default();
        let src = Surface {
            format: RenderTargetFormat::A8B8G8R8Unorm,
            linear: MemoryLayout::Pitch,
            block_dimensions: 0,
            depth: 1,
            layer: 0,
            pitch: 16,
            width: 4,
            height: 2,
            addr_upper: 0,
            addr_lower: 0x1000,
        };
        let dst = Surface {
            format: RenderTargetFormat::A8B8G8R8Unorm,
            linear: MemoryLayout::Pitch,
            block_dimensions: 0,
            depth: 1,
            layer: 0,
            pitch: 16,
            width: 4,
            height: 2,
            addr_upper: 0,
            addr_lower: 0x2000,
        };
        let config = Config {
            operation: crate::engines::fermi_2d::Operation::SrcCopy,
            filter: Filter::Point,
            must_accelerate: false,
            dst_x0: 0,
            dst_y0: 0,
            dst_x1: 4,
            dst_y1: 2,
            src_x0: 0,
            src_y0: 0,
            src_x1: 4,
            src_y1: 2,
        };
        let src_data: Vec<u8> = (0..32).collect();
        let expected = src_data.clone();
        let writes = std::sync::Arc::new(std::sync::Mutex::new(Vec::<(u64, Vec<u8>)>::new()));
        {
            let mut mm = blitter.memory_manager.lock();
            mm.map(0x1000, 0x3000, src_data.len() as u64, 0xFF, false);
            mm.map(0x2000, 0x4000, src_data.len() as u64, 0xFF, false);
        }
        blitter
            .memory_manager
            .lock()
            .set_guest_memory_reader(std::sync::Arc::new(move |addr, buf| match addr {
                0x3000 => buf.copy_from_slice(&src_data[..buf.len()]),
                0x4000 => buf.fill(0),
                _ => panic!("unexpected read addr 0x{addr:X}"),
            }));
        let writes_for_closure = std::sync::Arc::clone(&writes);
        blitter
            .memory_manager
            .lock()
            .set_guest_memory_writer(std::sync::Arc::new(move |addr, data| {
                writes_for_closure
                    .lock()
                    .unwrap()
                    .push((addr, data.to_vec()));
            }));

        assert!(blitter.blit(&src, &dst, &config));

        let writes = writes.lock().unwrap();
        assert_eq!(writes.len(), 1);
        assert_eq!(writes[0].0, 0x4000);
        assert_eq!(writes[0].1, expected);
    }
}
