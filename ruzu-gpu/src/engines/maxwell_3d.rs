// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Maxwell 3D engine — structured state tracking and clear operations.
//!
//! This is the main 3D rendering engine (NV class B197). It handles render
//! target configuration, clear operations, and draw call logging. Register
//! writes are stored in a flat array and side-effect methods (clear, draw)
//! are triggered on specific register writes.

use super::{ClassId, Engine, Framebuffer, ENGINE_REG_COUNT};

// ── Register offset constants (method addresses) ────────────────────────────

/// Render target array base. 8 targets, 0x10 words (0x40 bytes) each.
const RT_BASE: u32 = 0x0800;
/// Words per render target.
const RT_STRIDE: u32 = 0x10;

// Offsets within each render target (relative to RT_BASE + i*RT_STRIDE):
const RT_OFF_ADDRESS_HIGH: u32 = 0x00;
const RT_OFF_ADDRESS_LOW: u32 = 0x01;
const RT_OFF_WIDTH: u32 = 0x02;
const RT_OFF_HEIGHT: u32 = 0x03;
const RT_OFF_FORMAT: u32 = 0x04;

/// Clear color RGBA: 4 consecutive f32-as-u32 registers.
const CLEAR_COLOR_BASE: u32 = 0x0D80;
/// Clear depth value (f32 as u32 bits).
#[allow(dead_code)]
const CLEAR_DEPTH: u32 = 0x0D90;
/// Clear stencil value.
#[allow(dead_code)]
const CLEAR_STENCIL: u32 = 0x0DA0;
/// Clear surface trigger register.
const CLEAR_SURFACE: u32 = 0x19D0;
/// Draw begin/end register.
const DRAW_REG: u32 = 0x1614;

// ── Common render target formats ────────────────────────────────────────────

const RT_FORMAT_A8B8G8R8_UNORM: u32 = 0xD5;
const RT_FORMAT_A8B8G8R8_SRGB: u32 = 0xD6;
#[allow(dead_code)]
const RT_FORMAT_A8R8G8B8_UNORM: u32 = 0xCF;

pub struct Maxwell3D {
    regs: Box<[u32; ENGINE_REG_COUNT]>,
    /// Pending framebuffer output from clear/draw operations.
    pending_framebuffer: Option<Framebuffer>,
}

impl Maxwell3D {
    pub fn new() -> Self {
        Self {
            regs: Box::new([0u32; ENGINE_REG_COUNT]),
            pending_framebuffer: None,
        }
    }

    // ── Typed register accessors ────────────────────────────────────────

    /// GPU virtual address of render target `index` (0..7).
    fn rt_address(&self, index: usize) -> u64 {
        let base = (RT_BASE + index as u32 * RT_STRIDE) as usize;
        let high = self.regs[base + RT_OFF_ADDRESS_HIGH as usize] as u64;
        let low = self.regs[base + RT_OFF_ADDRESS_LOW as usize] as u64;
        (high << 32) | low
    }

    /// Width of render target `index`.
    fn rt_width(&self, index: usize) -> u32 {
        let base = (RT_BASE + index as u32 * RT_STRIDE) as usize;
        self.regs[base + RT_OFF_WIDTH as usize]
    }

    /// Height of render target `index`.
    fn rt_height(&self, index: usize) -> u32 {
        let base = (RT_BASE + index as u32 * RT_STRIDE) as usize;
        self.regs[base + RT_OFF_HEIGHT as usize]
    }

    /// Format of render target `index`.
    fn rt_format(&self, index: usize) -> u32 {
        let base = (RT_BASE + index as u32 * RT_STRIDE) as usize;
        self.regs[base + RT_OFF_FORMAT as usize]
    }

    /// Read the 4-component clear color as f32 values.
    fn clear_color_rgba(&self) -> [f32; 4] {
        let base = CLEAR_COLOR_BASE as usize;
        [
            f32::from_bits(self.regs[base]),
            f32::from_bits(self.regs[base + 1]),
            f32::from_bits(self.regs[base + 2]),
            f32::from_bits(self.regs[base + 3]),
        ]
    }

    // ── Side-effect handlers ────────────────────────────────────────────

    /// Handle clear_surface trigger.
    ///
    /// Bitfield:
    /// - bit 0: clear Z (depth)
    /// - bit 1: clear S (stencil)
    /// - bit 2: clear R
    /// - bit 3: clear G
    /// - bit 4: clear B
    /// - bit 5: clear A
    /// - bits 6..9: RT index
    fn handle_clear_surface(&mut self, flags: u32) {
        let rt_index = ((flags >> 6) & 0xF) as usize;
        let clear_r = flags & (1 << 2) != 0;
        let clear_g = flags & (1 << 3) != 0;
        let clear_b = flags & (1 << 4) != 0;
        let clear_a = flags & (1 << 5) != 0;

        if !clear_r && !clear_g && !clear_b && !clear_a {
            // No color channels to clear (depth/stencil only) — skip for now.
            log::trace!("Maxwell3D: clear_surface depth/stencil only, skipping");
            return;
        }

        if rt_index >= 8 {
            log::warn!("Maxwell3D: clear_surface invalid RT index {}", rt_index);
            return;
        }

        let gpu_va = self.rt_address(rt_index);
        let width = self.rt_width(rt_index);
        let height = self.rt_height(rt_index);
        let format = self.rt_format(rt_index);

        if width == 0 || height == 0 || gpu_va == 0 {
            log::trace!(
                "Maxwell3D: clear_surface skipped (width={}, height={}, va=0x{:X})",
                width,
                height,
                gpu_va
            );
            return;
        }

        let color = self.clear_color_rgba();

        log::debug!(
            "Maxwell3D: clear RT{} {}x{} fmt=0x{:X} color=[{:.3}, {:.3}, {:.3}, {:.3}] va=0x{:X}",
            rt_index,
            width,
            height,
            format,
            color[0],
            color[1],
            color[2],
            color[3],
            gpu_va,
        );

        // Convert clear color to RGBA8 bytes based on format.
        let pixel = format_clear_color(format, color, clear_r, clear_g, clear_b, clear_a);

        // Fill framebuffer.
        let pixel_count = (width as usize) * (height as usize);
        let mut pixels = vec![0u8; pixel_count * 4];
        for i in 0..pixel_count {
            let off = i * 4;
            pixels[off] = pixel[0];
            pixels[off + 1] = pixel[1];
            pixels[off + 2] = pixel[2];
            pixels[off + 3] = pixel[3];
        }

        self.pending_framebuffer = Some(Framebuffer {
            gpu_va,
            width,
            height,
            pixels,
        });
    }

    /// Handle draw register write. Logs the draw call but doesn't produce pixels.
    fn handle_draw(&mut self, value: u32) {
        // Draw begin has topology in bits 0..15 and vertex count info.
        // For now just log it.
        log::debug!("Maxwell3D: draw value=0x{:X}", value);
    }
}

/// Convert clear color f32s to 4 bytes based on render target format.
///
/// For formats where only certain channels are being cleared, uncleared
/// channels default to 0 (since we're generating a full framebuffer fill,
/// the partial clear only matters if we were compositing with existing data).
fn format_clear_color(
    format: u32,
    color: [f32; 4],
    clear_r: bool,
    clear_g: bool,
    clear_b: bool,
    clear_a: bool,
) -> [u8; 4] {
    let r = if clear_r {
        (color[0].clamp(0.0, 1.0) * 255.0) as u8
    } else {
        0
    };
    let g = if clear_g {
        (color[1].clamp(0.0, 1.0) * 255.0) as u8
    } else {
        0
    };
    let b = if clear_b {
        (color[2].clamp(0.0, 1.0) * 255.0) as u8
    } else {
        0
    };
    let a = if clear_a {
        (color[3].clamp(0.0, 1.0) * 255.0) as u8
    } else {
        0
    };

    match format {
        // A8B8G8R8: memory layout is [R, G, B, A] when read as bytes in LE.
        // The Switch GPU stores pixels as ABGR in u32 LE, which means
        // byte[0]=R, byte[1]=G, byte[2]=B, byte[3]=A in memory.
        RT_FORMAT_A8B8G8R8_UNORM | RT_FORMAT_A8B8G8R8_SRGB => [r, g, b, a],
        // Unknown format: default to RGBA layout.
        _ => {
            log::trace!(
                "Maxwell3D: unknown RT format 0x{:X}, using RGBA8 layout",
                format
            );
            [r, g, b, a]
        }
    }
}

impl Default for Maxwell3D {
    fn default() -> Self {
        Self::new()
    }
}

impl Engine for Maxwell3D {
    fn class_id(&self) -> ClassId {
        ClassId::Threed
    }

    fn write_reg(&mut self, method: u32, value: u32) {
        let idx = method as usize;
        if idx < ENGINE_REG_COUNT {
            self.regs[idx] = value;
        }

        // Detect side-effect triggers.
        match method {
            CLEAR_SURFACE => self.handle_clear_surface(value),
            DRAW_REG => self.handle_draw(value),
            _ => {}
        }

        log::trace!("Maxwell3D: reg[0x{:X}] = 0x{:X}", method, value);
    }

    fn take_framebuffer(&mut self) -> Option<Framebuffer> {
        self.pending_framebuffer.take()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_write_reg() {
        let mut engine = Maxwell3D::new();
        engine.write_reg(0x100, 0xDEAD);
        assert_eq!(engine.regs[0x100], 0xDEAD);
    }

    #[test]
    fn test_write_reg_high_method() {
        // Regression: methods above 0xFFF were silently dropped with old 0x1000 size.
        let mut engine = Maxwell3D::new();
        engine.write_reg(0x19D0, 0x1234);
        assert_eq!(engine.regs[0x19D0], 0x1234);
    }

    #[test]
    fn test_class_id() {
        let engine = Maxwell3D::new();
        assert_eq!(engine.class_id(), ClassId::Threed);
    }

    #[test]
    fn test_rt_accessors() {
        let mut engine = Maxwell3D::new();
        let rt0_base = RT_BASE as usize;

        // Set RT0: address = 0x0001_0000_2000, width=1280, height=720, format=0xD5
        engine.regs[rt0_base + RT_OFF_ADDRESS_HIGH as usize] = 0x0001;
        engine.regs[rt0_base + RT_OFF_ADDRESS_LOW as usize] = 0x0000_2000;
        engine.regs[rt0_base + RT_OFF_WIDTH as usize] = 1280;
        engine.regs[rt0_base + RT_OFF_HEIGHT as usize] = 720;
        engine.regs[rt0_base + RT_OFF_FORMAT as usize] = 0xD5;

        assert_eq!(engine.rt_address(0), 0x0001_0000_2000);
        assert_eq!(engine.rt_width(0), 1280);
        assert_eq!(engine.rt_height(0), 720);
        assert_eq!(engine.rt_format(0), 0xD5);
    }

    #[test]
    fn test_clear_color_accessor() {
        let mut engine = Maxwell3D::new();
        let base = CLEAR_COLOR_BASE as usize;

        engine.regs[base] = f32::to_bits(1.0); // R
        engine.regs[base + 1] = f32::to_bits(0.5); // G
        engine.regs[base + 2] = f32::to_bits(0.0); // B
        engine.regs[base + 3] = f32::to_bits(0.75); // A

        let color = engine.clear_color_rgba();
        assert_eq!(color[0], 1.0);
        assert_eq!(color[1], 0.5);
        assert_eq!(color[2], 0.0);
        assert_eq!(color[3], 0.75);
    }

    #[test]
    fn test_handle_clear_produces_framebuffer() {
        let mut engine = Maxwell3D::new();
        let rt0_base = RT_BASE as usize;

        // Configure RT0: 4x2 pixels, A8B8G8R8_UNORM, address 0x1000.
        engine.regs[rt0_base + RT_OFF_ADDRESS_HIGH as usize] = 0;
        engine.regs[rt0_base + RT_OFF_ADDRESS_LOW as usize] = 0x1000;
        engine.regs[rt0_base + RT_OFF_WIDTH as usize] = 4;
        engine.regs[rt0_base + RT_OFF_HEIGHT as usize] = 2;
        engine.regs[rt0_base + RT_OFF_FORMAT as usize] = RT_FORMAT_A8B8G8R8_UNORM;

        // Set clear color to solid red (R=1, G=0, B=0, A=1).
        let base = CLEAR_COLOR_BASE as usize;
        engine.regs[base] = f32::to_bits(1.0);
        engine.regs[base + 1] = f32::to_bits(0.0);
        engine.regs[base + 2] = f32::to_bits(0.0);
        engine.regs[base + 3] = f32::to_bits(1.0);

        // Trigger clear: clear RGBA on RT0 (bits 2-5 set, RT index 0).
        let flags = (1 << 2) | (1 << 3) | (1 << 4) | (1 << 5); // R|G|B|A
        engine.handle_clear_surface(flags);

        let fb = engine.take_framebuffer().expect("Should produce framebuffer");
        assert_eq!(fb.gpu_va, 0x1000);
        assert_eq!(fb.width, 4);
        assert_eq!(fb.height, 2);
        assert_eq!(fb.pixels.len(), 4 * 2 * 4); // 32 bytes

        // Every pixel should be [255, 0, 0, 255] (red).
        for chunk in fb.pixels.chunks_exact(4) {
            assert_eq!(chunk, &[255, 0, 0, 255]);
        }
    }

    #[test]
    fn test_clear_with_zero_dimensions_skipped() {
        let mut engine = Maxwell3D::new();
        // RT0 has zero width/height — clear should not produce framebuffer.
        let flags = (1 << 2) | (1 << 3) | (1 << 4) | (1 << 5);
        engine.handle_clear_surface(flags);
        assert!(engine.take_framebuffer().is_none());
    }

    #[test]
    fn test_clear_depth_only_skipped() {
        let mut engine = Maxwell3D::new();
        let rt0_base = RT_BASE as usize;
        engine.regs[rt0_base + RT_OFF_ADDRESS_LOW as usize] = 0x1000;
        engine.regs[rt0_base + RT_OFF_WIDTH as usize] = 4;
        engine.regs[rt0_base + RT_OFF_HEIGHT as usize] = 2;
        engine.regs[rt0_base + RT_OFF_FORMAT as usize] = RT_FORMAT_A8B8G8R8_UNORM;

        // Only depth clear (bit 0), no color channels.
        engine.handle_clear_surface(1);
        assert!(engine.take_framebuffer().is_none());
    }

    #[test]
    fn test_draw_logs_without_crash() {
        let mut engine = Maxwell3D::new();
        // Just ensure draw doesn't panic.
        engine.handle_draw(0x0001);
        assert!(engine.take_framebuffer().is_none());
    }

    #[test]
    fn test_take_framebuffer_returns_none_after_take() {
        let mut engine = Maxwell3D::new();
        let rt0_base = RT_BASE as usize;
        engine.regs[rt0_base + RT_OFF_ADDRESS_LOW as usize] = 0x1000;
        engine.regs[rt0_base + RT_OFF_WIDTH as usize] = 2;
        engine.regs[rt0_base + RT_OFF_HEIGHT as usize] = 2;
        engine.regs[rt0_base + RT_OFF_FORMAT as usize] = RT_FORMAT_A8B8G8R8_UNORM;

        let base = CLEAR_COLOR_BASE as usize;
        engine.regs[base] = f32::to_bits(0.0);
        engine.regs[base + 1] = f32::to_bits(1.0);
        engine.regs[base + 2] = f32::to_bits(0.0);
        engine.regs[base + 3] = f32::to_bits(1.0);

        let flags = (1 << 2) | (1 << 3) | (1 << 4) | (1 << 5);
        engine.handle_clear_surface(flags);

        assert!(engine.take_framebuffer().is_some());
        // Second take should return None.
        assert!(engine.take_framebuffer().is_none());
    }

    #[test]
    fn test_clear_via_write_reg() {
        let mut engine = Maxwell3D::new();
        let rt0_base = RT_BASE;

        // Write RT0 config via write_reg (as the command processor would).
        engine.write_reg(rt0_base + RT_OFF_ADDRESS_LOW, 0x5000);
        engine.write_reg(rt0_base + RT_OFF_WIDTH, 8);
        engine.write_reg(rt0_base + RT_OFF_HEIGHT, 4);
        engine.write_reg(rt0_base + RT_OFF_FORMAT, RT_FORMAT_A8B8G8R8_UNORM);

        // Set clear color to blue.
        engine.write_reg(CLEAR_COLOR_BASE, f32::to_bits(0.0));
        engine.write_reg(CLEAR_COLOR_BASE + 1, f32::to_bits(0.0));
        engine.write_reg(CLEAR_COLOR_BASE + 2, f32::to_bits(1.0));
        engine.write_reg(CLEAR_COLOR_BASE + 3, f32::to_bits(1.0));

        // Trigger clear via write_reg (this is the actual path).
        let flags = (1 << 2) | (1 << 3) | (1 << 4) | (1 << 5);
        engine.write_reg(CLEAR_SURFACE, flags);

        let fb = engine.take_framebuffer().expect("Should produce framebuffer");
        assert_eq!(fb.gpu_va, 0x5000);
        assert_eq!(fb.width, 8);
        assert_eq!(fb.height, 4);

        // Every pixel: [0, 0, 255, 255] (blue).
        for chunk in fb.pixels.chunks_exact(4) {
            assert_eq!(chunk, &[0, 0, 255, 255]);
        }
    }
}
