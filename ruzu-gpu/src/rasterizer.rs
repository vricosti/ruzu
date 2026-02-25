// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Software rasterizer for draw call rendering.
//!
//! Implements vertex fetching from GPU memory, viewport transform, and
//! triangle rasterization using barycentric coordinates. This is a fallback
//! rasterizer — all triangles are rendered as solid white (no shader execution).

use crate::engines::maxwell_3d::{
    DrawCall, IndexFormat, PrimitiveTopology, VertexAttribSize, VertexAttribType,
};
use crate::engines::Framebuffer;

/// Stateless software rasterizer. All state comes from `DrawCall` parameters.
pub struct SoftwareRasterizer;

impl SoftwareRasterizer {
    /// Render draw calls onto a framebuffer.
    ///
    /// If `base_framebuffer` is provided, draws on top of it. Otherwise creates
    /// a blank framebuffer from the first draw call's render target dimensions.
    /// Returns `None` only if there are no draw calls and no base framebuffer.
    pub fn render_draw_calls(
        draws: &[DrawCall],
        read_gpu: &dyn Fn(u64, &mut [u8]),
        base_framebuffer: Option<Framebuffer>,
    ) -> Option<Framebuffer> {
        if draws.is_empty() {
            return base_framebuffer;
        }

        // Determine framebuffer dimensions.
        let (fb_width, fb_height, gpu_va, mut pixels) = if let Some(fb) = base_framebuffer {
            let w = fb.width;
            let h = fb.height;
            let va = fb.gpu_va;
            (w, h, va, fb.pixels)
        } else {
            // Use first draw call's render target 0.
            let rt = &draws[0].render_targets[0];
            let w = if rt.width > 0 { rt.width } else { 1280 };
            let h = if rt.height > 0 { rt.height } else { 720 };
            let size = (w * h * 4) as usize;
            (w, h, rt.address, vec![0u8; size])
        };

        if fb_width == 0 || fb_height == 0 {
            return None;
        }

        for draw in draws {
            let positions = fetch_positions(draw, read_gpu);
            if positions.is_empty() {
                continue;
            }

            let triangles = process_topology(&positions, draw.topology);
            let viewport = &draw.viewports[0];

            // Determine effective viewport dimensions (fall back to RT size).
            let vp_width = if viewport.width > 0.0 {
                viewport.width
            } else {
                fb_width as f32
            };
            let vp_height = if viewport.height > 0.0 {
                viewport.height
            } else {
                fb_height as f32
            };
            let vp_x = viewport.x;
            let vp_y = viewport.y;

            let color = [255u8, 255, 255, 255]; // solid white

            for tri in &triangles {
                let p0 = viewport_transform(positions[tri[0]], vp_x, vp_y, vp_width, vp_height);
                let p1 = viewport_transform(positions[tri[1]], vp_x, vp_y, vp_width, vp_height);
                let p2 = viewport_transform(positions[tri[2]], vp_x, vp_y, vp_width, vp_height);

                rasterize_triangle(p0, p1, p2, color, &mut pixels, fb_width, fb_height);
            }
        }

        Some(Framebuffer {
            gpu_va,
            width: fb_width,
            height: fb_height,
            pixels,
        })
    }
}

/// Fetch vertex positions from GPU memory for a draw call.
fn fetch_positions(
    draw: &DrawCall,
    read_gpu: &dyn Fn(u64, &mut [u8]),
) -> Vec<[f32; 4]> {
    // Find the position attribute (index 0 by convention, or first Float with >= 2 components).
    let pos_attrib = draw.vertex_attribs.iter().find(|a| {
        a.buffer_index == 0
            || (a.attrib_type == VertexAttribType::Float && a.size.component_count() >= 2)
    });

    let attrib = match pos_attrib {
        Some(a) => a,
        None => {
            if draw.vertex_attribs.is_empty() {
                return vec![];
            }
            // Fall back to first attribute with >= 2 components.
            match draw.vertex_attribs.iter().find(|a| a.size.component_count() >= 2) {
                Some(a) => a,
                None => return vec![],
            }
        }
    };

    // Find the vertex stream this attribute references.
    let stream = draw
        .vertex_streams
        .iter()
        .find(|s| s.index == attrib.buffer_index);
    let stream = match stream {
        Some(s) => s,
        None => return vec![],
    };

    if stream.stride == 0 || stream.address == 0 {
        return vec![];
    }

    // Determine vertex indices.
    let indices = if draw.indexed {
        fetch_indices(draw, read_gpu)
    } else {
        let first = draw.vertex_first;
        let count = draw.vertex_count;
        (first..first + count).collect()
    };

    // Fetch position data for each vertex.
    let comp_count = attrib.size.component_count() as usize;
    let mut positions = Vec::with_capacity(indices.len());

    for idx in &indices {
        let vertex_addr =
            stream.address + (*idx as u64) * (stream.stride as u64) + (attrib.offset as u64);

        let mut pos = [0.0f32; 4];
        pos[3] = 1.0; // default w

        match attrib.size {
            VertexAttribSize::R32G32
            | VertexAttribSize::R32G32B32
            | VertexAttribSize::R32G32B32A32
            | VertexAttribSize::R32 => {
                let byte_count = comp_count * 4;
                let mut buf = vec![0u8; byte_count];
                read_gpu(vertex_addr, &mut buf);
                for (i, chunk) in buf.chunks_exact(4).enumerate().take(comp_count) {
                    pos[i] = f32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]);
                }
            }
            _ => {
                // For non-float32 formats, skip (unsupported without shader).
                continue;
            }
        }

        positions.push(pos);
    }

    positions
}

/// Fetch index buffer entries as u32 indices.
fn fetch_indices(draw: &DrawCall, read_gpu: &dyn Fn(u64, &mut [u8])) -> Vec<u32> {
    // If inline index data is available, use it.
    if !draw.inline_index_data.is_empty() {
        return parse_indices(&draw.inline_index_data, draw.index_format);
    }

    if draw.index_buffer_addr == 0 || draw.index_buffer_count == 0 {
        return vec![];
    }

    let count = draw.index_buffer_count as usize;
    let elem_size = match draw.index_format {
        IndexFormat::UnsignedByte => 1,
        IndexFormat::UnsignedShort => 2,
        IndexFormat::UnsignedInt => 4,
    };

    let first = draw.index_buffer_first as usize;
    let byte_offset = first * elem_size;
    let byte_count = count * elem_size;
    let mut buf = vec![0u8; byte_count];
    read_gpu(draw.index_buffer_addr + byte_offset as u64, &mut buf);

    parse_indices(&buf, draw.index_format)
}

/// Parse raw bytes into u32 index values.
fn parse_indices(data: &[u8], format: IndexFormat) -> Vec<u32> {
    match format {
        IndexFormat::UnsignedByte => data.iter().map(|&b| b as u32).collect(),
        IndexFormat::UnsignedShort => data
            .chunks_exact(2)
            .map(|c| u16::from_le_bytes([c[0], c[1]]) as u32)
            .collect(),
        IndexFormat::UnsignedInt => data
            .chunks_exact(4)
            .map(|c| u32::from_le_bytes([c[0], c[1], c[2], c[3]]))
            .collect(),
    }
}

/// Map clip-space position to screen pixel coordinates.
fn viewport_transform(
    pos: [f32; 4],
    vp_x: f32,
    vp_y: f32,
    vp_width: f32,
    vp_height: f32,
) -> [f32; 2] {
    let x = (pos[0] * 0.5 + 0.5) * vp_width + vp_x;
    let y = (pos[1] * 0.5 + 0.5) * vp_height + vp_y;
    [x, y]
}

/// Convert a list of vertex positions into triangle index triples based on topology.
fn process_topology(positions: &[[f32; 4]], topology: PrimitiveTopology) -> Vec<[usize; 3]> {
    let n = positions.len();
    let mut tris = Vec::new();

    match topology {
        PrimitiveTopology::Triangles => {
            let count = n / 3;
            for i in 0..count {
                tris.push([i * 3, i * 3 + 1, i * 3 + 2]);
            }
        }
        PrimitiveTopology::TriangleStrip => {
            if n < 3 {
                return tris;
            }
            for i in 0..n - 2 {
                if i % 2 == 0 {
                    tris.push([i, i + 1, i + 2]);
                } else {
                    tris.push([i, i + 2, i + 1]); // swap winding
                }
            }
        }
        PrimitiveTopology::TriangleFan => {
            if n < 3 {
                return tris;
            }
            for i in 1..n - 1 {
                tris.push([0, i, i + 1]);
            }
        }
        PrimitiveTopology::Quads => {
            let count = n / 4;
            for i in 0..count {
                let base = i * 4;
                tris.push([base, base + 1, base + 2]);
                tris.push([base, base + 2, base + 3]);
            }
        }
        _ => {
            log::warn!(
                "SoftwareRasterizer: unsupported topology {:?}, skipping",
                topology
            );
        }
    }

    tris
}

/// Rasterize a single triangle using barycentric coordinates.
fn rasterize_triangle(
    v0: [f32; 2],
    v1: [f32; 2],
    v2: [f32; 2],
    color: [u8; 4],
    pixels: &mut [u8],
    fb_width: u32,
    fb_height: u32,
) {
    // Compute bounding box.
    let min_x = v0[0].min(v1[0]).min(v2[0]).max(0.0) as u32;
    let max_x = (v0[0].max(v1[0]).max(v2[0]).ceil() as u32).min(fb_width.saturating_sub(1));
    let min_y = v0[1].min(v1[1]).min(v2[1]).max(0.0) as u32;
    let max_y = (v0[1].max(v1[1]).max(v2[1]).ceil() as u32).min(fb_height.saturating_sub(1));

    if min_x > max_x || min_y > max_y {
        return;
    }

    // Edge function: positive when point is on the left side of the edge.
    let edge = |a: [f32; 2], b: [f32; 2], p: [f32; 2]| -> f32 {
        (b[0] - a[0]) * (p[1] - a[1]) - (b[1] - a[1]) * (p[0] - a[0])
    };

    for y in min_y..=max_y {
        for x in min_x..=max_x {
            let p = [x as f32 + 0.5, y as f32 + 0.5];
            let w0 = edge(v1, v2, p);
            let w1 = edge(v2, v0, p);
            let w2 = edge(v0, v1, p);

            if w0 >= 0.0 && w1 >= 0.0 && w2 >= 0.0 {
                let offset = ((y * fb_width + x) * 4) as usize;
                if offset + 4 <= pixels.len() {
                    pixels[offset..offset + 4].copy_from_slice(&color);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::engines::maxwell_3d::*;

    fn default_draw_call() -> DrawCall {
        DrawCall {
            topology: PrimitiveTopology::Triangles,
            vertex_first: 0,
            vertex_count: 0,
            indexed: false,
            index_buffer_addr: 0,
            index_buffer_count: 0,
            index_buffer_first: 0,
            index_format: IndexFormat::UnsignedByte,
            vertex_streams: vec![],
            viewports: [ViewportInfo::default(); NUM_VIEWPORTS],
            scissors: [ScissorInfo::default(); NUM_VIEWPORTS],
            blend: [BlendInfo::default(); 8],
            blend_color: BlendColorInfo {
                r: 0.0,
                g: 0.0,
                b: 0.0,
                a: 0.0,
            },
            depth_stencil: DepthStencilInfo {
                depth_test_enable: false,
                depth_write_enable: false,
                depth_func: ComparisonOp::Always,
                depth_mode: DepthMode::ZeroToOne,
                stencil_enable: false,
                stencil_two_side: false,
                front: StencilFaceInfo::default(),
                back: StencilFaceInfo::default(),
            },
            rasterizer: RasterizerInfo {
                cull_enable: false,
                front_face: FrontFace::CCW,
                cull_face: CullFace::Back,
                polygon_mode_front: PolygonMode::Fill,
                polygon_mode_back: PolygonMode::Fill,
                line_width_smooth: 1.0,
                line_width_aliased: 1.0,
                depth_bias: 0.0,
                slope_scale_depth_bias: 0.0,
                depth_bias_clamp: 0.0,
            },
            program_base_address: 0,
            cb_bindings: [[ConstBufferBinding::default(); 18]; 5],
            vertex_attribs: vec![],
            shader_stages: [ShaderStageInfo::default(); 6],
            color_masks: [ColorMaskInfo::default(); 8],
            rt_control: RtControlInfo::default(),
            tex_header_pool_addr: 0,
            tex_header_pool_limit: 0,
            tex_sampler_pool_addr: 0,
            tex_sampler_pool_limit: 0,
            instance_count: 1,
            base_instance: 0,
            base_vertex: 0,
            inline_index_data: vec![],
            sampler_binding: SamplerBinding::Independently,
            render_targets: [RenderTargetInfo::default(); 8],
        }
    }

    #[test]
    fn test_topology_triangles() {
        let positions: Vec<[f32; 4]> = (0..6).map(|i| [i as f32, 0.0, 0.0, 1.0]).collect();
        let tris = process_topology(&positions, PrimitiveTopology::Triangles);
        assert_eq!(tris.len(), 2);
        assert_eq!(tris[0], [0, 1, 2]);
        assert_eq!(tris[1], [3, 4, 5]);
    }

    #[test]
    fn test_topology_triangle_strip() {
        let positions: Vec<[f32; 4]> = (0..5).map(|i| [i as f32, 0.0, 0.0, 1.0]).collect();
        let tris = process_topology(&positions, PrimitiveTopology::TriangleStrip);
        assert_eq!(tris.len(), 3);
        assert_eq!(tris[0], [0, 1, 2]);
        assert_eq!(tris[1], [1, 3, 2]); // winding swap
        assert_eq!(tris[2], [2, 3, 4]);
    }

    #[test]
    fn test_topology_triangle_fan() {
        let positions: Vec<[f32; 4]> = (0..4).map(|i| [i as f32, 0.0, 0.0, 1.0]).collect();
        let tris = process_topology(&positions, PrimitiveTopology::TriangleFan);
        assert_eq!(tris.len(), 2);
        assert_eq!(tris[0], [0, 1, 2]);
        assert_eq!(tris[1], [0, 2, 3]);
    }

    #[test]
    fn test_topology_quads() {
        let positions: Vec<[f32; 4]> = (0..8).map(|i| [i as f32, 0.0, 0.0, 1.0]).collect();
        let tris = process_topology(&positions, PrimitiveTopology::Quads);
        assert_eq!(tris.len(), 4);
        assert_eq!(tris[0], [0, 1, 2]);
        assert_eq!(tris[1], [0, 2, 3]);
        assert_eq!(tris[2], [4, 5, 6]);
        assert_eq!(tris[3], [4, 6, 7]);
    }

    #[test]
    fn test_viewport_transform() {
        // NDC (0,0) should map to viewport center.
        let result = viewport_transform([0.0, 0.0, 0.0, 1.0], 0.0, 0.0, 1280.0, 720.0);
        assert!((result[0] - 640.0).abs() < 0.01);
        assert!((result[1] - 360.0).abs() < 0.01);

        // NDC (-1,-1) should map to top-left.
        let result = viewport_transform([-1.0, -1.0, 0.0, 1.0], 0.0, 0.0, 1280.0, 720.0);
        assert!((result[0] - 0.0).abs() < 0.01);
        assert!((result[1] - 0.0).abs() < 0.01);

        // NDC (1,1) should map to bottom-right.
        let result = viewport_transform([1.0, 1.0, 0.0, 1.0], 0.0, 0.0, 1280.0, 720.0);
        assert!((result[0] - 1280.0).abs() < 0.01);
        assert!((result[1] - 720.0).abs() < 0.01);
    }

    #[test]
    fn test_rasterize_triangle_fills_pixels() {
        let width = 10u32;
        let height = 10u32;
        let mut pixels = vec![0u8; (width * height * 4) as usize];
        let color = [255u8, 0, 0, 255];

        // A triangle covering the upper-left quadrant.
        let v0 = [0.0, 0.0];
        let v1 = [5.0, 0.0];
        let v2 = [0.0, 5.0];
        rasterize_triangle(v0, v1, v2, color, &mut pixels, width, height);

        // Pixel (1, 1) should be inside the triangle.
        let offset = ((1 * width + 1) * 4) as usize;
        assert_eq!(&pixels[offset..offset + 4], &color);

        // Pixel (9, 9) should NOT be filled.
        let offset = ((9 * width + 9) * 4) as usize;
        assert_eq!(&pixels[offset..offset + 4], &[0, 0, 0, 0]);
    }

    #[test]
    fn test_rasterize_triangle_clips_to_bounds() {
        let width = 4u32;
        let height = 4u32;
        let mut pixels = vec![0u8; (width * height * 4) as usize];
        let color = [255u8, 255, 255, 255];

        // Triangle extending way past framebuffer edges.
        let v0 = [-10.0, -10.0];
        let v1 = [100.0, -10.0];
        let v2 = [-10.0, 100.0];
        rasterize_triangle(v0, v1, v2, color, &mut pixels, width, height);

        // All pixels should be filled (triangle covers everything).
        for y in 0..height {
            for x in 0..width {
                let offset = ((y * width + x) * 4) as usize;
                assert_eq!(
                    &pixels[offset..offset + 4],
                    &color,
                    "pixel ({}, {}) should be filled",
                    x,
                    y
                );
            }
        }
        // No out-of-bounds write — if we got here without panic, bounds are fine.
    }

    #[test]
    fn test_render_draw_calls_no_draws() {
        let base_fb = Framebuffer {
            gpu_va: 0x1000,
            width: 4,
            height: 4,
            pixels: vec![42u8; 64],
        };
        let result =
            SoftwareRasterizer::render_draw_calls(&[], &|_, _| {}, Some(base_fb));
        let fb = result.unwrap();
        assert_eq!(fb.width, 4);
        assert_eq!(fb.height, 4);
        // Pixels should be unchanged.
        assert!(fb.pixels.iter().all(|&p| p == 42));
    }

    #[test]
    fn test_render_draw_calls_with_base_fb() {
        // Create vertex data: a triangle at NDC positions covering the framebuffer.
        // 3 vertices, R32G32 format = 2 floats * 4 bytes = 8 bytes each
        let mut vertex_data = vec![0u8; 24];
        // v0: (-1, -1)
        vertex_data[0..4].copy_from_slice(&(-1.0f32).to_le_bytes());
        vertex_data[4..8].copy_from_slice(&(-1.0f32).to_le_bytes());
        // v1: (1, -1)
        vertex_data[8..12].copy_from_slice(&(1.0f32).to_le_bytes());
        vertex_data[12..16].copy_from_slice(&(-1.0f32).to_le_bytes());
        // v2: (-1, 1)
        vertex_data[16..20].copy_from_slice(&(-1.0f32).to_le_bytes());
        vertex_data[20..24].copy_from_slice(&(1.0f32).to_le_bytes());

        let vertex_data_clone = vertex_data.clone();
        let read_gpu = move |addr: u64, buf: &mut [u8]| {
            if addr == 0x5000 {
                let len = buf.len().min(vertex_data_clone.len());
                buf[..len].copy_from_slice(&vertex_data_clone[..len]);
            }
        };

        let mut draw = default_draw_call();
        draw.vertex_count = 3;
        draw.vertex_streams = vec![VertexStreamInfo {
            index: 0,
            address: 0x5000,
            stride: 8,
            enabled: true,
        }];
        draw.vertex_attribs = vec![VertexAttribInfo {
            buffer_index: 0,
            constant: false,
            offset: 0,
            size: VertexAttribSize::R32G32,
            attrib_type: VertexAttribType::Float,
            bgra: false,
        }];
        draw.viewports[0] = ViewportInfo {
            x: 0.0,
            y: 0.0,
            width: 4.0,
            height: 4.0,
            depth_near: 0.0,
            depth_far: 1.0,
        };

        let base_fb = Framebuffer {
            gpu_va: 0x1000,
            width: 4,
            height: 4,
            pixels: vec![0u8; 64], // black
        };

        let result =
            SoftwareRasterizer::render_draw_calls(&[draw], &read_gpu, Some(base_fb));
        let fb = result.unwrap();
        assert_eq!(fb.width, 4);
        assert_eq!(fb.height, 4);
        // At least some pixels should be white (255).
        assert!(fb.pixels.iter().any(|&p| p == 255));
    }

    #[test]
    fn test_parse_indices_unsigned_byte() {
        let data = vec![0u8, 1, 2, 3];
        let indices = parse_indices(&data, IndexFormat::UnsignedByte);
        assert_eq!(indices, vec![0, 1, 2, 3]);
    }

    #[test]
    fn test_parse_indices_unsigned_short() {
        let mut data = vec![0u8; 6];
        data[0..2].copy_from_slice(&0u16.to_le_bytes());
        data[2..4].copy_from_slice(&100u16.to_le_bytes());
        data[4..6].copy_from_slice(&200u16.to_le_bytes());
        let indices = parse_indices(&data, IndexFormat::UnsignedShort);
        assert_eq!(indices, vec![0, 100, 200]);
    }

    #[test]
    fn test_parse_indices_unsigned_int() {
        let mut data = vec![0u8; 8];
        data[0..4].copy_from_slice(&42u32.to_le_bytes());
        data[4..8].copy_from_slice(&99u32.to_le_bytes());
        let indices = parse_indices(&data, IndexFormat::UnsignedInt);
        assert_eq!(indices, vec![42, 99]);
    }
}
