// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Maxwell 3D engine — structured state tracking, clear operations, and draw
//! call recording.
//!
//! This is the main 3D rendering engine (NV class B197). It handles render
//! target configuration, clear operations, and draw call state tracking.
//! Register writes are stored in a flat array and side-effect methods (clear,
//! draw begin/end) are triggered on specific register writes.

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

// ── Viewport registers ──────────────────────────────────────────────────────

/// Viewport transform base. 16 viewports, 8 words each.
/// Words: scale_x, scale_y, scale_z, translate_x, translate_y, translate_z, swizzle, snap.
const VP_TRANSFORM_BASE: u32 = 0x0A00;
const VP_TRANSFORM_STRIDE: u32 = 8;

// ── Scissor registers ───────────────────────────────────────────────────────

/// Scissor base. 16 scissors, 4 words each.
/// Words: enable, min_x|max_x(packed), min_y|max_y(packed), pad.
const SCISSOR_BASE: u32 = 0x0E00;
const SCISSOR_STRIDE: u32 = 4;

// ── Vertex buffer registers ─────────────────────────────────────────────────

/// Vertex buffer first vertex.
const VB_FIRST: u32 = 0x0D74;
/// Vertex buffer vertex count.
const VB_COUNT: u32 = 0x0D75;

/// Vertex stream array base. 32 streams, 4 words each.
/// Words: stride|enable, addr_high, addr_low, frequency.
const VERTEX_STREAM_BASE: u32 = 0x1C00;
const VERTEX_STREAM_STRIDE: u32 = 4;

/// Vertex stream limit array base. 32 streams, 2 words each.
#[allow(dead_code)]
const VERTEX_STREAM_LIMIT_BASE: u32 = 0x1F00;

// ── Index buffer registers ──────────────────────────────────────────────────

/// Index buffer base (7 words).
/// Words: addr_high, addr_low, limit_high, limit_low, format, first, count.
const IB_BASE: u32 = 0x17C8;
const IB_OFF_ADDR_HIGH: u32 = 0;
const IB_OFF_ADDR_LOW: u32 = 1;
#[allow(dead_code)]
const IB_OFF_LIMIT_HIGH: u32 = 2;
#[allow(dead_code)]
const IB_OFF_LIMIT_LOW: u32 = 3;
const IB_OFF_FORMAT: u32 = 4;
const IB_OFF_FIRST: u32 = 5;
const IB_OFF_COUNT: u32 = 6;

// ── Draw registers ──────────────────────────────────────────────────────────

/// Draw end trigger (previously DRAW_REG).
const DRAW_END: u32 = 0x1614;
/// Draw begin: sets topology.
const DRAW_BEGIN: u32 = 0x1615;

// ── Common render target formats ────────────────────────────────────────────

const RT_FORMAT_A8B8G8R8_UNORM: u32 = 0xD5;
const RT_FORMAT_A8B8G8R8_SRGB: u32 = 0xD6;
#[allow(dead_code)]
const RT_FORMAT_A8R8G8B8_UNORM: u32 = 0xCF;

// ── Draw state types ────────────────────────────────────────────────────────

/// GPU primitive topology.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum PrimitiveTopology {
    Points = 0,
    Lines = 1,
    LineLoop = 2,
    LineStrip = 3,
    Triangles = 4,
    TriangleStrip = 5,
    TriangleFan = 6,
    Quads = 7,
    QuadStrip = 8,
    Polygon = 9,
    LinesAdjacency = 10,
    LineStripAdjacency = 11,
    TrianglesAdjacency = 12,
    TriangleStripAdjacency = 13,
    Patches = 14,
}

impl PrimitiveTopology {
    pub fn from_raw(value: u32) -> Self {
        match value & 0xFFFF {
            0 => Self::Points,
            1 => Self::Lines,
            2 => Self::LineLoop,
            3 => Self::LineStrip,
            4 => Self::Triangles,
            5 => Self::TriangleStrip,
            6 => Self::TriangleFan,
            7 => Self::Quads,
            8 => Self::QuadStrip,
            9 => Self::Polygon,
            10 => Self::LinesAdjacency,
            11 => Self::LineStripAdjacency,
            12 => Self::TrianglesAdjacency,
            13 => Self::TriangleStripAdjacency,
            14 => Self::Patches,
            _ => {
                log::warn!("Maxwell3D: unknown topology {}, defaulting to Triangles", value & 0xFFFF);
                Self::Triangles
            }
        }
    }
}

/// Index buffer element format.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum IndexFormat {
    UnsignedByte = 0,
    UnsignedShort = 1,
    UnsignedInt = 2,
}

impl IndexFormat {
    pub fn from_raw(value: u32) -> Self {
        match value {
            0 => Self::UnsignedByte,
            1 => Self::UnsignedShort,
            2 => Self::UnsignedInt,
            _ => {
                log::warn!("Maxwell3D: unknown index format {}, defaulting to UnsignedInt", value);
                Self::UnsignedInt
            }
        }
    }

    pub fn size_bytes(&self) -> u32 {
        match self {
            Self::UnsignedByte => 1,
            Self::UnsignedShort => 2,
            Self::UnsignedInt => 4,
        }
    }
}

/// Information about an active vertex stream.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VertexStreamInfo {
    pub index: u32,
    pub address: u64,
    pub stride: u32,
    pub enabled: bool,
}

/// Viewport computed from scale/translate registers.
#[derive(Debug, Clone, PartialEq)]
pub struct ViewportInfo {
    pub x: f32,
    pub y: f32,
    pub width: f32,
    pub height: f32,
    pub depth_near: f32,
    pub depth_far: f32,
}

/// Scissor rectangle.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScissorInfo {
    pub enabled: bool,
    pub min_x: u32,
    pub max_x: u32,
    pub min_y: u32,
    pub max_y: u32,
}

/// A recorded draw call with all relevant state at the time of DRAW_END.
#[derive(Debug, Clone)]
pub struct DrawCall {
    pub topology: PrimitiveTopology,
    pub vertex_first: u32,
    pub vertex_count: u32,
    pub indexed: bool,
    pub index_buffer_addr: u64,
    pub index_buffer_count: u32,
    pub index_buffer_first: u32,
    pub index_format: IndexFormat,
    pub vertex_streams: Vec<VertexStreamInfo>,
    pub viewport: ViewportInfo,
    pub scissor: ScissorInfo,
}

// ── Engine struct ───────────────────────────────────────────────────────────

pub struct Maxwell3D {
    regs: Box<[u32; ENGINE_REG_COUNT]>,
    /// Pending framebuffer output from clear operations.
    pending_framebuffer: Option<Framebuffer>,
    /// Accumulated draw call records.
    draw_calls: Vec<DrawCall>,
    /// Current primitive topology (set on DRAW_BEGIN).
    current_topology: PrimitiveTopology,
    /// Whether the current draw is indexed (set when IB count is written).
    draw_indexed: bool,
}

impl Maxwell3D {
    pub fn new() -> Self {
        Self {
            regs: Box::new([0u32; ENGINE_REG_COUNT]),
            pending_framebuffer: None,
            draw_calls: Vec::new(),
            current_topology: PrimitiveTopology::Triangles,
            draw_indexed: false,
        }
    }

    // ── Render target accessors ──────────────────────────────────────────

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

    // ── Vertex stream accessors ──────────────────────────────────────────

    /// Read vertex stream `index` (0..31) info from registers.
    pub fn vertex_stream_info(&self, index: u32) -> VertexStreamInfo {
        let base = (VERTEX_STREAM_BASE + index * VERTEX_STREAM_STRIDE) as usize;
        let word0 = self.regs[base]; // stride in bits[11:0], enable in bit 12
        let addr_high = self.regs[base + 1] as u64;
        let addr_low = self.regs[base + 2] as u64;
        VertexStreamInfo {
            index,
            address: (addr_high << 32) | addr_low,
            stride: word0 & 0xFFF,
            enabled: (word0 & (1 << 12)) != 0,
        }
    }

    // ── Index buffer accessors ───────────────────────────────────────────

    /// Index buffer GPU address.
    pub fn index_buffer_addr(&self) -> u64 {
        let base = IB_BASE as usize;
        let high = self.regs[base + IB_OFF_ADDR_HIGH as usize] as u64;
        let low = self.regs[base + IB_OFF_ADDR_LOW as usize] as u64;
        (high << 32) | low
    }

    /// Index buffer element format.
    pub fn index_buffer_format(&self) -> IndexFormat {
        IndexFormat::from_raw(self.regs[(IB_BASE + IB_OFF_FORMAT) as usize])
    }

    /// Index buffer first index.
    pub fn index_buffer_first(&self) -> u32 {
        self.regs[(IB_BASE + IB_OFF_FIRST) as usize]
    }

    /// Index buffer element count.
    pub fn index_buffer_count(&self) -> u32 {
        self.regs[(IB_BASE + IB_OFF_COUNT) as usize]
    }

    // ── Viewport accessors ───────────────────────────────────────────────

    /// Compute viewport info for viewport `index` (0..15) from scale/translate.
    pub fn viewport_info(&self, index: u32) -> ViewportInfo {
        let base = (VP_TRANSFORM_BASE + index * VP_TRANSFORM_STRIDE) as usize;
        let scale_x = f32::from_bits(self.regs[base]);
        let scale_y = f32::from_bits(self.regs[base + 1]);
        let scale_z = f32::from_bits(self.regs[base + 2]);
        let translate_x = f32::from_bits(self.regs[base + 3]);
        let translate_y = f32::from_bits(self.regs[base + 4]);
        let translate_z = f32::from_bits(self.regs[base + 5]);

        // Viewport transform: x = translate - |scale|, width = 2*|scale|
        let width = scale_x.abs() * 2.0;
        let height = scale_y.abs() * 2.0;
        ViewportInfo {
            x: translate_x - scale_x.abs(),
            y: translate_y - scale_y.abs(),
            width,
            height,
            depth_near: translate_z - scale_z.abs(),
            depth_far: translate_z + scale_z.abs(),
        }
    }

    // ── Scissor accessors ────────────────────────────────────────────────

    /// Read scissor info for scissor `index` (0..15).
    pub fn scissor_info(&self, index: u32) -> ScissorInfo {
        let base = (SCISSOR_BASE + index * SCISSOR_STRIDE) as usize;
        let enable = self.regs[base];
        let x_packed = self.regs[base + 1]; // min_x[15:0] | max_x[31:16]
        let y_packed = self.regs[base + 2]; // min_y[15:0] | max_y[31:16]
        ScissorInfo {
            enabled: (enable & 1) != 0,
            min_x: x_packed & 0xFFFF,
            max_x: (x_packed >> 16) & 0xFFFF,
            min_y: y_packed & 0xFFFF,
            max_y: (y_packed >> 16) & 0xFFFF,
        }
    }

    // ── Draw call accessors ──────────────────────────────────────────────

    /// Drain accumulated draw call records.
    pub fn take_draw_calls(&mut self) -> Vec<DrawCall> {
        std::mem::take(&mut self.draw_calls)
    }

    // ── Side-effect handlers ─────────────────────────────────────────────

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

    /// Handle DRAW_BEGIN: captures topology from bits[15:0].
    fn handle_draw_begin(&mut self, value: u32) {
        self.current_topology = PrimitiveTopology::from_raw(value);
        log::debug!(
            "Maxwell3D: DRAW_BEGIN topology={:?}",
            self.current_topology
        );
    }

    /// Handle DRAW_END: builds DrawCall record from current register state.
    fn handle_draw_end(&mut self) {
        // Collect active vertex streams (scan all 32 slots).
        let mut vertex_streams = Vec::new();
        for i in 0..32 {
            let info = self.vertex_stream_info(i);
            if info.enabled {
                vertex_streams.push(info);
            }
        }

        let draw = DrawCall {
            topology: self.current_topology,
            vertex_first: self.regs[VB_FIRST as usize],
            vertex_count: self.regs[VB_COUNT as usize],
            indexed: self.draw_indexed,
            index_buffer_addr: self.index_buffer_addr(),
            index_buffer_count: self.index_buffer_count(),
            index_buffer_first: self.index_buffer_first(),
            index_format: self.index_buffer_format(),
            vertex_streams,
            viewport: self.viewport_info(0),
            scissor: self.scissor_info(0),
        };

        log::debug!(
            "Maxwell3D: DRAW_END {:?} verts={}/{} indexed={} streams={}",
            draw.topology,
            draw.vertex_first,
            draw.vertex_count,
            draw.indexed,
            draw.vertex_streams.len(),
        );

        self.draw_calls.push(draw);
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

        // Track draw_indexed flag when IB count register is written.
        if method == IB_BASE + IB_OFF_COUNT && value > 0 {
            self.draw_indexed = true;
        }

        // Detect side-effect triggers.
        match method {
            CLEAR_SURFACE => self.handle_clear_surface(value),
            DRAW_BEGIN => self.handle_draw_begin(value),
            DRAW_END => self.handle_draw_end(),
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

    // ── Existing tests ───────────────────────────────────────────────────

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
        // Just ensure draw begin/end doesn't panic.
        engine.handle_draw_begin(0x0004); // Triangles
        engine.handle_draw_end();
        assert!(engine.take_framebuffer().is_none());
        assert_eq!(engine.draw_calls.len(), 1);
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

    // ── New draw state tracking tests ────────────────────────────────────

    #[test]
    fn test_vertex_stream_accessors() {
        let mut engine = Maxwell3D::new();
        let base = VERTEX_STREAM_BASE as usize;

        // Stream 0: stride=64, enabled, addr=0x0000_1000_2000.
        engine.regs[base] = 64 | (1 << 12); // stride=64, enable bit 12
        engine.regs[base + 1] = 0x0000_1000; // addr_high
        engine.regs[base + 2] = 0x0000_2000; // addr_low

        let info = engine.vertex_stream_info(0);
        assert_eq!(info.index, 0);
        assert_eq!(info.stride, 64);
        assert!(info.enabled);
        assert_eq!(info.address, 0x0000_1000_0000_2000);
    }

    #[test]
    fn test_vertex_stream_disabled() {
        let mut engine = Maxwell3D::new();
        let base = VERTEX_STREAM_BASE as usize;

        // Stream 0: stride=32, NOT enabled (bit 12 clear).
        engine.regs[base] = 32;
        engine.regs[base + 1] = 0;
        engine.regs[base + 2] = 0x5000;

        let info = engine.vertex_stream_info(0);
        assert_eq!(info.stride, 32);
        assert!(!info.enabled);
    }

    #[test]
    fn test_index_buffer_accessors() {
        let mut engine = Maxwell3D::new();
        let base = IB_BASE as usize;

        engine.regs[base + IB_OFF_ADDR_HIGH as usize] = 0x0000_00AB;
        engine.regs[base + IB_OFF_ADDR_LOW as usize] = 0xCDEF_0000;
        engine.regs[base + IB_OFF_FORMAT as usize] = 1; // UnsignedShort
        engine.regs[base + IB_OFF_FIRST as usize] = 10;
        engine.regs[base + IB_OFF_COUNT as usize] = 500;

        assert_eq!(engine.index_buffer_addr(), 0xAB_CDEF_0000);
        assert_eq!(engine.index_buffer_format(), IndexFormat::UnsignedShort);
        assert_eq!(engine.index_buffer_format().size_bytes(), 2);
        assert_eq!(engine.index_buffer_first(), 10);
        assert_eq!(engine.index_buffer_count(), 500);
    }

    #[test]
    fn test_viewport_info() {
        let mut engine = Maxwell3D::new();
        let base = VP_TRANSFORM_BASE as usize;

        // VP0: scale=(640, -360, 0.5), translate=(640, 360, 0.5)
        // => x=0, y=0, width=1280, height=720, near=0, far=1
        engine.regs[base] = f32::to_bits(640.0); // scale_x
        engine.regs[base + 1] = f32::to_bits(-360.0); // scale_y
        engine.regs[base + 2] = f32::to_bits(0.5); // scale_z
        engine.regs[base + 3] = f32::to_bits(640.0); // translate_x
        engine.regs[base + 4] = f32::to_bits(360.0); // translate_y
        engine.regs[base + 5] = f32::to_bits(0.5); // translate_z

        let vp = engine.viewport_info(0);
        assert_eq!(vp.x, 0.0);
        assert_eq!(vp.y, 0.0);
        assert_eq!(vp.width, 1280.0);
        assert_eq!(vp.height, 720.0);
        assert_eq!(vp.depth_near, 0.0);
        assert_eq!(vp.depth_far, 1.0);
    }

    #[test]
    fn test_scissor_info() {
        let mut engine = Maxwell3D::new();
        let base = SCISSOR_BASE as usize;

        // Scissor 0: enabled, min_x=10, max_x=1270, min_y=20, max_y=700.
        engine.regs[base] = 1; // enabled
        engine.regs[base + 1] = 10 | (1270 << 16); // min_x | max_x
        engine.regs[base + 2] = 20 | (700 << 16); // min_y | max_y

        let sc = engine.scissor_info(0);
        assert!(sc.enabled);
        assert_eq!(sc.min_x, 10);
        assert_eq!(sc.max_x, 1270);
        assert_eq!(sc.min_y, 20);
        assert_eq!(sc.max_y, 700);
    }

    #[test]
    fn test_draw_begin_sets_topology() {
        let mut engine = Maxwell3D::new();
        engine.handle_draw_begin(4); // Triangles
        assert_eq!(engine.current_topology, PrimitiveTopology::Triangles);

        engine.handle_draw_begin(1); // Lines
        assert_eq!(engine.current_topology, PrimitiveTopology::Lines);
    }

    #[test]
    fn test_draw_end_creates_draw_call() {
        let mut engine = Maxwell3D::new();

        // Set up vertex stream 0.
        let vs_base = VERTEX_STREAM_BASE;
        engine.write_reg(vs_base, 32 | (1 << 12)); // stride=32, enabled
        engine.write_reg(vs_base + 1, 0); // addr_high
        engine.write_reg(vs_base + 2, 0x10000); // addr_low

        // Set vertex buffer first/count.
        engine.write_reg(VB_FIRST, 0);
        engine.write_reg(VB_COUNT, 36);

        // Set viewport 0.
        let vp_base = VP_TRANSFORM_BASE;
        engine.write_reg(vp_base, f32::to_bits(640.0));
        engine.write_reg(vp_base + 1, f32::to_bits(-360.0));
        engine.write_reg(vp_base + 2, f32::to_bits(0.5));
        engine.write_reg(vp_base + 3, f32::to_bits(640.0));
        engine.write_reg(vp_base + 4, f32::to_bits(360.0));
        engine.write_reg(vp_base + 5, f32::to_bits(0.5));

        // Draw: begin(Triangles) + end.
        engine.write_reg(DRAW_BEGIN, 4);
        engine.write_reg(DRAW_END, 0);

        let draws = engine.take_draw_calls();
        assert_eq!(draws.len(), 1);
        let d = &draws[0];
        assert_eq!(d.topology, PrimitiveTopology::Triangles);
        assert_eq!(d.vertex_first, 0);
        assert_eq!(d.vertex_count, 36);
        assert!(!d.indexed);
        assert_eq!(d.vertex_streams.len(), 1);
        assert_eq!(d.vertex_streams[0].stride, 32);
        assert_eq!(d.viewport.width, 1280.0);
    }

    #[test]
    fn test_multiple_draw_calls() {
        let mut engine = Maxwell3D::new();

        engine.write_reg(DRAW_BEGIN, 4); // Triangles
        engine.write_reg(DRAW_END, 0);
        engine.write_reg(DRAW_BEGIN, 1); // Lines
        engine.write_reg(DRAW_END, 0);

        let draws = engine.take_draw_calls();
        assert_eq!(draws.len(), 2);
        assert_eq!(draws[0].topology, PrimitiveTopology::Triangles);
        assert_eq!(draws[1].topology, PrimitiveTopology::Lines);

        // After take, should be empty.
        let draws2 = engine.take_draw_calls();
        assert!(draws2.is_empty());
    }

    #[test]
    fn test_draw_indexed_flag() {
        let mut engine = Maxwell3D::new();

        // Write IB count > 0 → sets draw_indexed.
        engine.write_reg(IB_BASE + IB_OFF_COUNT, 100);
        engine.write_reg(IB_BASE + IB_OFF_FORMAT, 2); // UnsignedInt

        engine.write_reg(DRAW_BEGIN, 4);
        engine.write_reg(DRAW_END, 0);

        let draws = engine.take_draw_calls();
        assert_eq!(draws.len(), 1);
        assert!(draws[0].indexed);
        assert_eq!(draws[0].index_format, IndexFormat::UnsignedInt);
        assert_eq!(draws[0].index_buffer_count, 100);
    }
}
