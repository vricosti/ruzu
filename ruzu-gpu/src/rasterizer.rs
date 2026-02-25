// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Software rasterizer for draw call rendering.
//!
//! Implements vertex fetching from GPU memory, viewport transform, triangle
//! rasterization with barycentric coordinate interpolation, depth testing,
//! alpha blending, scissor clipping, color write masks, and back-face culling.

use crate::engines::maxwell_3d::{
    BlendColorInfo, BlendEquation, BlendFactor, BlendInfo, ColorMaskInfo, ComponentType,
    ComparisonOp, CullFace, DepthStencilInfo, DrawCall, FrontFace, IndexFormat,
    PrimitiveTopology, RasterizerInfo, SamplerDescriptor, ScissorInfo, SwizzleSource,
    TextureDescriptor, TextureFormat, TextureType, TicHeaderVersion, VertexAttribSize,
    VertexAttribType, WrapMode,
};
use crate::engines::Framebuffer;

/// Cached pre-decoded RGBA8 texture data, loaded once per draw call.
struct SampledTexture {
    data: Vec<u8>,
    width: u32,
    height: u32,
    wrap_u: WrapMode,
    wrap_v: WrapMode,
    normalized_coords: bool,
}

/// Return bytes per texel for supported uncompressed formats, or `None` for
/// compressed / unsupported formats.
fn bytes_per_texel(fmt: TextureFormat) -> Option<u32> {
    match fmt {
        TextureFormat::A8B8G8R8 | TextureFormat::R8G8B8A8 => Some(4),
        TextureFormat::B5G6R5 => Some(2),
        TextureFormat::R8G8 => Some(2),
        TextureFormat::R8 => Some(1),
        TextureFormat::R32G32B32A32 => Some(16),
        _ => None,
    }
}

/// Decode raw bytes of a single texel into RGBA8.
fn decode_texel(bytes: &[u8], fmt: TextureFormat, _comp_type: ComponentType) -> Option<[u8; 4]> {
    match fmt {
        TextureFormat::A8B8G8R8 => {
            // LE u32 layout: byte0=R, byte1=G, byte2=B, byte3=A
            Some([bytes[0], bytes[1], bytes[2], bytes[3]])
        }
        TextureFormat::R8G8B8A8 => {
            // LE u32 layout: byte0=A, byte1=B, byte2=G, byte3=R → reverse
            Some([bytes[3], bytes[2], bytes[1], bytes[0]])
        }
        TextureFormat::B5G6R5 => {
            let val = u16::from_le_bytes([bytes[0], bytes[1]]);
            let b = (val & 0x1F) as u8;
            let g = ((val >> 5) & 0x3F) as u8;
            let r = ((val >> 11) & 0x1F) as u8;
            Some([
                (r << 3) | (r >> 2),
                (g << 2) | (g >> 4),
                (b << 3) | (b >> 2),
                255,
            ])
        }
        TextureFormat::R8G8 => Some([bytes[0], bytes[1], 0, 255]),
        TextureFormat::R8 => Some([bytes[0], 0, 0, 255]),
        TextureFormat::R32G32B32A32 => {
            let r = f32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]);
            let g = f32::from_le_bytes([bytes[4], bytes[5], bytes[6], bytes[7]]);
            let b = f32::from_le_bytes([bytes[8], bytes[9], bytes[10], bytes[11]]);
            let a = f32::from_le_bytes([bytes[12], bytes[13], bytes[14], bytes[15]]);
            Some([
                (r.clamp(0.0, 1.0) * 255.0).round() as u8,
                (g.clamp(0.0, 1.0) * 255.0).round() as u8,
                (b.clamp(0.0, 1.0) * 255.0).round() as u8,
                (a.clamp(0.0, 1.0) * 255.0).round() as u8,
            ])
        }
        _ => None,
    }
}

/// Apply TIC component swizzle to an RGBA8 texel.
fn apply_swizzle(rgba: [u8; 4], swizzle: &[SwizzleSource; 4]) -> [u8; 4] {
    let mut out = [0u8; 4];
    for (i, src) in swizzle.iter().enumerate() {
        out[i] = match src {
            SwizzleSource::Zero => 0,
            SwizzleSource::R => rgba[0],
            SwizzleSource::G => rgba[1],
            SwizzleSource::B => rgba[2],
            SwizzleSource::A => rgba[3],
            SwizzleSource::OneInt | SwizzleSource::OneFloat => 255,
            SwizzleSource::Invalid => 0,
        };
    }
    out
}

/// Convert an sRGB 8-bit value to linear (approximate). Alpha is never converted.
fn srgb_to_linear(val: u8) -> u8 {
    let f = val as f32 / 255.0;
    let linear = if f <= 0.04045 {
        f / 12.92
    } else {
        ((f + 0.055) / 1.055).powf(2.4)
    };
    (linear * 255.0).round().clamp(0.0, 255.0) as u8
}

/// Map a floating-point UV coordinate to a texel index given a wrap mode.
fn apply_wrap_mode(coord: f32, size: u32, mode: WrapMode) -> u32 {
    if size == 0 {
        return 0;
    }
    let s = size as i32;
    match mode {
        WrapMode::Wrap => {
            let i = coord.floor() as i32;
            ((i % s + s) % s) as u32
        }
        WrapMode::ClampToEdge | WrapMode::Clamp => {
            let i = coord.floor() as i32;
            i.clamp(0, s - 1) as u32
        }
        WrapMode::Mirror => {
            let period = s * 2;
            let i = coord.floor() as i32;
            let m = ((i % period) + period) % period;
            if m < s { m as u32 } else { (period - 1 - m) as u32 }
        }
        _ => {
            // Border, MirrorOnce variants — fall back to clamp.
            let i = coord.floor() as i32;
            i.clamp(0, s - 1) as u32
        }
    }
}

// ── BlockLinear (GOB) deswizzle constants and functions ──────────────────────

/// GOB width in bytes.
const GOB_SIZE_X: u32 = 64;
/// GOB height in pixels.
const GOB_SIZE_Y: u32 = 8;
/// Total bytes per GOB (64 × 8 = 512).
const GOB_SIZE: u32 = 512;

/// Compute intra-GOB byte offset using the Tegra X1 swizzle formula.
/// `x` is the byte offset within the GOB row (0..63), `y` is the row (0..7).
fn gob_offset(x: u32, y: u32) -> u32 {
    ((x % 64) / 32) * 256
        + ((y % 8) / 2) * 64
        + ((x % 32) / 16) * 32
        + (y % 2) * 16
        + (x % 16)
}

/// Compute the memory size (in bytes) of a BlockLinear texture, aligned to
/// block boundaries.
fn block_linear_size(width: u32, height: u32, bpt: u32, block_height: u32) -> usize {
    let aligned_width = align_up(width * bpt, GOB_SIZE_X);
    let block_height_pixels = GOB_SIZE_Y << block_height;
    let aligned_height = align_up(height, block_height_pixels);
    (aligned_width * aligned_height) as usize
}

/// Convert BlockLinear-tiled data to a linear row-major pixel buffer.
fn deswizzle_block_linear(
    raw: &[u8],
    width: u32,
    height: u32,
    bpt: u32,
    block_height: u32,
) -> Vec<u8> {
    let linear_size = (width * height * bpt) as usize;
    let mut output = vec![0u8; linear_size];

    let gobs_in_x = align_up(width * bpt, GOB_SIZE_X) / GOB_SIZE_X;
    let block_height_gobs = 1u32 << block_height;
    let block_size_bytes = gobs_in_x * GOB_SIZE * block_height_gobs;

    for y in 0..height {
        for x in 0..width {
            let x_bytes = x * bpt;
            let gob_x = x_bytes / GOB_SIZE_X;
            let gob_y = y / GOB_SIZE_Y;
            let block_y = gob_y / block_height_gobs;
            let gob_in_block = gob_y % block_height_gobs;

            let block_offset = block_y * block_size_bytes
                + gob_x * GOB_SIZE * block_height_gobs
                + gob_in_block * GOB_SIZE;
            let intra = gob_offset(x_bytes % GOB_SIZE_X, y % GOB_SIZE_Y);

            let src = (block_offset + intra) as usize;
            let dst = (y * width * bpt + x * bpt) as usize;

            if src + bpt as usize <= raw.len() && dst + bpt as usize <= output.len() {
                output[dst..dst + bpt as usize].copy_from_slice(&raw[src..src + bpt as usize]);
            }
        }
    }

    output
}

/// Round `value` up to the next multiple of `align`.
fn align_up(value: u32, align: u32) -> u32 {
    (value + align - 1) / align * align
}

/// Nearest-neighbor sample from a pre-decoded texture. Returns normalised RGBA.
fn sample_texture(tex: &SampledTexture, u: f32, v: f32) -> [f32; 4] {
    let (su, sv) = if tex.normalized_coords {
        (u * tex.width as f32, v * tex.height as f32)
    } else {
        (u, v)
    };
    let tx = apply_wrap_mode(su, tex.width, tex.wrap_u);
    let ty = apply_wrap_mode(sv, tex.height, tex.wrap_v);
    let idx = (ty * tex.width + tx) as usize * 4;
    if idx + 4 > tex.data.len() {
        return [1.0, 1.0, 1.0, 1.0];
    }
    [
        tex.data[idx] as f32 / 255.0,
        tex.data[idx + 1] as f32 / 255.0,
        tex.data[idx + 2] as f32 / 255.0,
        tex.data[idx + 3] as f32 / 255.0,
    ]
}

/// Read UV (texture coordinate) attribute for all vertices of a draw call.
fn fetch_texture_coords(
    draw: &DrawCall,
    read_gpu: &dyn Fn(u64, &mut [u8]),
    vertex_count: usize,
) -> Option<Vec<[f32; 2]>> {
    // Heuristic: find a Float R32G32 attrib that is NOT the position attribute.
    let uv_attrib = draw
        .vertex_attribs
        .iter()
        .find(|a| {
            a.attrib_type == VertexAttribType::Float
                && a.size == VertexAttribSize::R32G32
                && a.buffer_index != 0
        })
        .or_else(|| {
            // Fallback: any Float attrib with 2 components, not the first attribute.
            draw.vertex_attribs.iter().skip(1).find(|a| {
                a.attrib_type == VertexAttribType::Float && a.size.component_count() == 2
            })
        })?;

    let stream = draw
        .vertex_streams
        .iter()
        .find(|s| s.index == uv_attrib.buffer_index)?;

    if stream.stride == 0 || stream.address == 0 {
        return None;
    }

    // Re-derive vertex indices (same as fetch_positions).
    let indices: Vec<u32> = if draw.indexed {
        fetch_indices(draw, read_gpu)
    } else {
        let first = draw.vertex_first;
        let count = draw.vertex_count;
        (first..first + count).collect()
    };

    let mut uvs = Vec::with_capacity(vertex_count);
    for idx in indices.iter().take(vertex_count) {
        let addr =
            stream.address + (*idx as u64) * (stream.stride as u64) + (uv_attrib.offset as u64);
        let mut buf = [0u8; 8];
        read_gpu(addr, &mut buf);
        let u = f32::from_le_bytes([buf[0], buf[1], buf[2], buf[3]]);
        let v = f32::from_le_bytes([buf[4], buf[5], buf[6], buf[7]]);
        uvs.push([u, v]);
    }

    // Pad if needed.
    while uvs.len() < vertex_count {
        uvs.push([0.0, 0.0]);
    }

    Some(uvs)
}

/// Load TIC/TSC entry 0, bulk-decode the texture to RGBA8.
fn load_texture(
    draw: &DrawCall,
    read_gpu: &dyn Fn(u64, &mut [u8]),
) -> Option<SampledTexture> {
    if draw.tex_header_pool_addr == 0 {
        return None;
    }

    // Read TIC entry 0 (32 bytes = 8 × u32).
    let mut tic_buf = [0u8; 32];
    read_gpu(draw.tex_header_pool_addr, &mut tic_buf);
    let mut tic_words = [0u32; 8];
    for (i, chunk) in tic_buf.chunks_exact(4).enumerate() {
        tic_words[i] = u32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]);
    }
    let tex_desc = TextureDescriptor::from_words(&tic_words);

    // Only support 2D textures with Pitch or BlockLinear layout.
    match tex_desc.texture_type {
        TextureType::Texture2D | TextureType::Texture2DNoMip => {}
        _ => return None,
    }

    let bpt = bytes_per_texel(tex_desc.format)?;
    if tex_desc.width == 0 || tex_desc.height == 0 || tex_desc.address == 0 {
        return None;
    }

    // Read TSC entry 0 (32 bytes) if sampler pool is available; otherwise use defaults.
    let (wrap_u, wrap_v) = if draw.tex_sampler_pool_addr != 0 {
        let mut tsc_buf = [0u8; 32];
        read_gpu(draw.tex_sampler_pool_addr, &mut tsc_buf);
        let mut tsc_words = [0u32; 8];
        for (i, chunk) in tsc_buf.chunks_exact(4).enumerate() {
            tsc_words[i] = u32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]);
        }
        let sam_desc = SamplerDescriptor::from_words(&tsc_words);
        (sam_desc.wrap_u, sam_desc.wrap_v)
    } else {
        (WrapMode::ClampToEdge, WrapMode::ClampToEdge)
    };

    // Bulk-read raw texture data — layout depends on header version.
    let raw = match tex_desc.header_version {
        TicHeaderVersion::Pitch => {
            let total_bytes = (tex_desc.width * tex_desc.height * bpt) as usize;
            let mut buf = vec![0u8; total_bytes];
            read_gpu(tex_desc.address, &mut buf);
            buf
        }
        TicHeaderVersion::BlockLinear => {
            let tiled_size = block_linear_size(
                tex_desc.width,
                tex_desc.height,
                bpt,
                tex_desc.block_height,
            );
            let mut tiled = vec![0u8; tiled_size];
            read_gpu(tex_desc.address, &mut tiled);
            deswizzle_block_linear(
                &tiled,
                tex_desc.width,
                tex_desc.height,
                bpt,
                tex_desc.block_height,
            )
        }
        _ => return None, // OneDBuffer, PitchColorKey, BlockLinearColorKey unsupported
    };

    // Pre-decode every texel to RGBA8.
    let texel_count = (tex_desc.width * tex_desc.height) as usize;
    let mut data = vec![0u8; texel_count * 4];
    let swizzle = [
        tex_desc.x_source,
        tex_desc.y_source,
        tex_desc.z_source,
        tex_desc.w_source,
    ];

    for i in 0..texel_count {
        let byte_off = i * bpt as usize;
        let end = byte_off + bpt as usize;
        if end > raw.len() {
            break;
        }
        let texel_bytes = &raw[byte_off..end];
        let rgba = match decode_texel(texel_bytes, tex_desc.format, tex_desc.r_type) {
            Some(c) => c,
            None => [255, 255, 255, 255],
        };
        let swizzled = apply_swizzle(rgba, &swizzle);
        let final_rgba = if tex_desc.srgb_conversion {
            [
                srgb_to_linear(swizzled[0]),
                srgb_to_linear(swizzled[1]),
                srgb_to_linear(swizzled[2]),
                swizzled[3], // alpha is never sRGB-converted
            ]
        } else {
            swizzled
        };
        data[i * 4] = final_rgba[0];
        data[i * 4 + 1] = final_rgba[1];
        data[i * 4 + 2] = final_rgba[2];
        data[i * 4 + 3] = final_rgba[3];
    }

    Some(SampledTexture {
        data,
        width: tex_desc.width,
        height: tex_desc.height,
        wrap_u,
        wrap_v,
        normalized_coords: tex_desc.normalized_coords,
    })
}

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
        write_gpu: &dyn Fn(u64, &[u8]),
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

        let mut depth_buf = vec![1.0f32; (fb_width * fb_height) as usize];

        for draw in draws {
            let positions = fetch_positions(draw, read_gpu);
            if positions.is_empty() {
                continue;
            }

            let vert_count = positions.len();
            let colors = fetch_vertex_colors(draw, read_gpu, vert_count);
            let texture = load_texture(draw, read_gpu);
            let uvs = if texture.is_some() {
                fetch_texture_coords(draw, read_gpu, vert_count)
            } else {
                None
            };
            let default_uv = [0.0f32, 0.0];

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

            for tri in &triangles {
                let p0 = viewport_transform(positions[tri[0]], vp_x, vp_y, vp_width, vp_height);
                let p1 = viewport_transform(positions[tri[1]], vp_x, vp_y, vp_width, vp_height);
                let p2 = viewport_transform(positions[tri[2]], vp_x, vp_y, vp_width, vp_height);

                let area = signed_area(p0, p1, p2);
                if should_cull(area, &draw.rasterizer) {
                    continue;
                }

                let c0 = colors[tri[0]];
                let c1 = colors[tri[1]];
                let c2 = colors[tri[2]];

                let uv0 = uvs.as_ref().map_or(default_uv, |u| u[tri[0]]);
                let uv1 = uvs.as_ref().map_or(default_uv, |u| u[tri[1]]);
                let uv2 = uvs.as_ref().map_or(default_uv, |u| u[tri[2]]);

                rasterize_triangle(
                    p0,
                    p1,
                    p2,
                    c0,
                    c1,
                    c2,
                    uv0,
                    uv1,
                    uv2,
                    texture.as_ref(),
                    &mut pixels,
                    &mut depth_buf,
                    fb_width,
                    fb_height,
                    &draw.scissors[0],
                    &draw.depth_stencil,
                    &draw.blend[0],
                    &draw.blend_color,
                    &draw.color_masks[0],
                );
            }
        }

        // Write rendered pixels back to GPU memory.
        if gpu_va != 0 {
            write_gpu(gpu_va, &pixels);
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

/// Fetch per-vertex RGBA colors from GPU memory.
///
/// Heuristic: finds the color attribute as (1) first attrib with `buffer_index == 1`,
/// or (2) first `UNorm` attrib with >= 3 components, or (3) second attrib overall.
/// Returns `[1.0, 1.0, 1.0, 1.0]` (white) per vertex if no color attribute found.
fn fetch_vertex_colors(
    draw: &DrawCall,
    read_gpu: &dyn Fn(u64, &mut [u8]),
    vertex_count: usize,
) -> Vec<[f32; 4]> {
    let white = [1.0f32, 1.0, 1.0, 1.0];

    // Find a color attribute using heuristics.
    let color_attrib = draw
        .vertex_attribs
        .iter()
        .find(|a| a.buffer_index == 1)
        .or_else(|| {
            draw.vertex_attribs.iter().find(|a| {
                a.attrib_type == VertexAttribType::UNorm && a.size.component_count() >= 3
            })
        })
        .or_else(|| {
            if draw.vertex_attribs.len() >= 2 {
                Some(&draw.vertex_attribs[1])
            } else {
                None
            }
        });

    let attrib = match color_attrib {
        Some(a) => a,
        None => return vec![white; vertex_count],
    };

    // Find the vertex stream for this attribute.
    let stream = draw
        .vertex_streams
        .iter()
        .find(|s| s.index == attrib.buffer_index);
    let stream = match stream {
        Some(s) => s,
        None => return vec![white; vertex_count],
    };

    if stream.stride == 0 || stream.address == 0 {
        return vec![white; vertex_count];
    }

    // Re-derive vertex indices (same as fetch_positions).
    let indices: Vec<u32> = if draw.indexed {
        fetch_indices(draw, read_gpu)
    } else {
        let first = draw.vertex_first;
        let count = draw.vertex_count;
        (first..first + count).collect()
    };

    let comp_count = attrib.size.component_count() as usize;
    let mut colors = Vec::with_capacity(vertex_count);

    for idx in indices.iter().take(vertex_count) {
        let addr =
            stream.address + (*idx as u64) * (stream.stride as u64) + (attrib.offset as u64);

        let mut color = white;

        match (attrib.size, attrib.attrib_type) {
            (VertexAttribSize::R8G8B8A8, VertexAttribType::UNorm)
            | (VertexAttribSize::R8G8B8, VertexAttribType::UNorm) => {
                let byte_count = comp_count;
                let mut buf = vec![0u8; byte_count];
                read_gpu(addr, &mut buf);
                for (i, &b) in buf.iter().enumerate().take(comp_count.min(4)) {
                    color[i] = b as f32 / 255.0;
                }
            }
            (
                VertexAttribSize::R32G32B32A32
                | VertexAttribSize::R32G32B32
                | VertexAttribSize::R32G32
                | VertexAttribSize::R32,
                VertexAttribType::Float,
            ) => {
                let byte_count = comp_count * 4;
                let mut buf = vec![0u8; byte_count];
                read_gpu(addr, &mut buf);
                for (i, chunk) in buf.chunks_exact(4).enumerate().take(comp_count.min(4)) {
                    color[i] = f32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]);
                }
            }
            _ => {
                // Unsupported color format — default white.
            }
        }

        colors.push(color);
    }

    // Pad with white if indices produced fewer than vertex_count.
    while colors.len() < vertex_count {
        colors.push(white);
    }

    colors
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

/// Map clip-space position to screen pixel coordinates, preserving Z for depth.
fn viewport_transform(
    pos: [f32; 4],
    vp_x: f32,
    vp_y: f32,
    vp_width: f32,
    vp_height: f32,
) -> [f32; 3] {
    let x = (pos[0] * 0.5 + 0.5) * vp_width + vp_x;
    let y = (pos[1] * 0.5 + 0.5) * vp_height + vp_y;
    [x, y, pos[2]]
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

/// Compute signed area of a triangle from screen-space vertices.
/// Positive = CCW winding, negative = CW winding.
fn signed_area(v0: [f32; 3], v1: [f32; 3], v2: [f32; 3]) -> f32 {
    (v1[0] - v0[0]) * (v2[1] - v0[1]) - (v2[0] - v0[0]) * (v1[1] - v0[1])
}

/// Determine whether a triangle should be culled based on winding and rasterizer state.
fn should_cull(area: f32, rasterizer: &RasterizerInfo) -> bool {
    if !rasterizer.cull_enable {
        return false;
    }

    // Determine if the triangle is front-facing.
    let is_front = match rasterizer.front_face {
        FrontFace::CCW => area > 0.0,
        FrontFace::CW => area < 0.0,
    };

    match rasterizer.cull_face {
        CullFace::Front => is_front,
        CullFace::Back => !is_front,
        CullFace::FrontAndBack => true,
    }
}

/// Evaluate a depth comparison function.
fn depth_test_passes(func: ComparisonOp, frag_z: f32, stored_z: f32) -> bool {
    match func {
        ComparisonOp::Never => false,
        ComparisonOp::Less => frag_z < stored_z,
        ComparisonOp::Equal => (frag_z - stored_z).abs() < f32::EPSILON,
        ComparisonOp::LessEqual => frag_z <= stored_z,
        ComparisonOp::Greater => frag_z > stored_z,
        ComparisonOp::NotEqual => (frag_z - stored_z).abs() >= f32::EPSILON,
        ComparisonOp::GreaterEqual => frag_z >= stored_z,
        ComparisonOp::Always => true,
    }
}

/// Apply a blend factor to produce per-channel multipliers.
fn apply_blend_factor(
    factor: BlendFactor,
    src: [f32; 4],
    dst: [f32; 4],
    constant: [f32; 4],
) -> [f32; 4] {
    match factor {
        BlendFactor::Zero => [0.0, 0.0, 0.0, 0.0],
        BlendFactor::One => [1.0, 1.0, 1.0, 1.0],
        BlendFactor::SrcColor => src,
        BlendFactor::OneMinusSrcColor => [1.0 - src[0], 1.0 - src[1], 1.0 - src[2], 1.0 - src[3]],
        BlendFactor::SrcAlpha => [src[3], src[3], src[3], src[3]],
        BlendFactor::OneMinusSrcAlpha => {
            let a = 1.0 - src[3];
            [a, a, a, a]
        }
        BlendFactor::DstAlpha => [dst[3], dst[3], dst[3], dst[3]],
        BlendFactor::OneMinusDstAlpha => {
            let a = 1.0 - dst[3];
            [a, a, a, a]
        }
        BlendFactor::DstColor => dst,
        BlendFactor::OneMinusDstColor => [1.0 - dst[0], 1.0 - dst[1], 1.0 - dst[2], 1.0 - dst[3]],
        BlendFactor::SrcAlphaSaturate => {
            let f = src[3].min(1.0 - dst[3]);
            [f, f, f, 1.0]
        }
        BlendFactor::ConstantColor => constant,
        BlendFactor::OneMinusConstantColor => {
            [1.0 - constant[0], 1.0 - constant[1], 1.0 - constant[2], 1.0 - constant[3]]
        }
        BlendFactor::ConstantAlpha => [constant[3], constant[3], constant[3], constant[3]],
        BlendFactor::OneMinusConstantAlpha => {
            let a = 1.0 - constant[3];
            [a, a, a, a]
        }
        // Dual-source blending — treat as One for now (no second color source in software).
        BlendFactor::Src1Color
        | BlendFactor::OneMinusSrc1Color
        | BlendFactor::Src1Alpha
        | BlendFactor::OneMinusSrc1Alpha => [1.0, 1.0, 1.0, 1.0],
    }
}

/// Apply a blend equation to combine source and destination terms.
fn blend_equation(eq: BlendEquation, sf: f32, df: f32) -> f32 {
    match eq {
        BlendEquation::Add => sf + df,
        BlendEquation::Subtract => sf - df,
        BlendEquation::ReverseSubtract => df - sf,
        BlendEquation::Min => sf.min(df),
        BlendEquation::Max => sf.max(df),
    }
}

/// Full blend pipeline: combine source and destination colors.
fn blend_colors(
    src: [f32; 4],
    dst: [f32; 4],
    blend: &BlendInfo,
    blend_color: &BlendColorInfo,
) -> [f32; 4] {
    if !blend.enabled {
        return src;
    }

    let constant = [blend_color.r, blend_color.g, blend_color.b, blend_color.a];
    let sf = apply_blend_factor(blend.color_src, src, dst, constant);
    let df = apply_blend_factor(blend.color_dst, src, dst, constant);

    let mut result = [0.0f32; 4];

    // Color channels (RGB).
    for i in 0..3 {
        result[i] = blend_equation(blend.color_op, src[i] * sf[i], dst[i] * df[i]).clamp(0.0, 1.0);
    }

    // Alpha channel.
    if blend.separate_alpha {
        let sa = apply_blend_factor(blend.alpha_src, src, dst, constant);
        let da = apply_blend_factor(blend.alpha_dst, src, dst, constant);
        result[3] =
            blend_equation(blend.alpha_op, src[3] * sa[3], dst[3] * da[3]).clamp(0.0, 1.0);
    } else {
        result[3] = blend_equation(blend.color_op, src[3] * sf[3], dst[3] * df[3]).clamp(0.0, 1.0);
    }

    result
}

/// Rasterize a single triangle with full per-pixel pipeline.
fn rasterize_triangle(
    v0: [f32; 3],
    v1: [f32; 3],
    v2: [f32; 3],
    c0: [f32; 4],
    c1: [f32; 4],
    c2: [f32; 4],
    uv0: [f32; 2],
    uv1: [f32; 2],
    uv2: [f32; 2],
    texture: Option<&SampledTexture>,
    pixels: &mut [u8],
    depth_buf: &mut [f32],
    fb_width: u32,
    fb_height: u32,
    scissor: &ScissorInfo,
    depth_stencil: &DepthStencilInfo,
    blend: &BlendInfo,
    blend_color: &BlendColorInfo,
    color_mask: &ColorMaskInfo,
) {
    // Compute bounding box.
    let mut min_x = v0[0].min(v1[0]).min(v2[0]).max(0.0) as u32;
    let mut max_x = (v0[0].max(v1[0]).max(v2[0]).ceil() as u32).min(fb_width.saturating_sub(1));
    let mut min_y = v0[1].min(v1[1]).min(v2[1]).max(0.0) as u32;
    let mut max_y = (v0[1].max(v1[1]).max(v2[1]).ceil() as u32).min(fb_height.saturating_sub(1));

    // Apply scissor clipping to bounding box.
    if scissor.enabled {
        min_x = min_x.max(scissor.min_x);
        max_x = max_x.min(scissor.max_x.saturating_sub(1));
        min_y = min_y.max(scissor.min_y);
        max_y = max_y.min(scissor.max_y.saturating_sub(1));
    }

    if min_x > max_x || min_y > max_y {
        return;
    }

    // Edge function: positive when point is on the left side of the edge.
    let edge = |a: [f32; 3], b: [f32; 3], p: [f32; 2]| -> f32 {
        (b[0] - a[0]) * (p[1] - a[1]) - (b[1] - a[1]) * (p[0] - a[0])
    };

    for y in min_y..=max_y {
        for x in min_x..=max_x {
            let p = [x as f32 + 0.5, y as f32 + 0.5];
            let w0 = edge(v1, v2, p);
            let w1 = edge(v2, v0, p);
            let w2 = edge(v0, v1, p);

            if w0 >= 0.0 && w1 >= 0.0 && w2 >= 0.0 {
                let w_sum = w0 + w1 + w2;
                if w_sum < f32::EPSILON {
                    continue; // degenerate triangle
                }

                let inv_w = 1.0 / w_sum;
                let b0 = w0 * inv_w;
                let b1 = w1 * inv_w;
                let b2 = w2 * inv_w;

                let pixel_idx = (y * fb_width + x) as usize;
                let offset = pixel_idx * 4;

                if offset + 4 > pixels.len() {
                    continue;
                }

                // Depth test.
                if depth_stencil.depth_test_enable {
                    let frag_z = v0[2] * b0 + v1[2] * b1 + v2[2] * b2;
                    if !depth_test_passes(depth_stencil.depth_func, frag_z, depth_buf[pixel_idx]) {
                        continue;
                    }
                    if depth_stencil.depth_write_enable {
                        depth_buf[pixel_idx] = frag_z;
                    }
                }

                // Interpolate vertex color.
                let mut src = [
                    c0[0] * b0 + c1[0] * b1 + c2[0] * b2,
                    c0[1] * b0 + c1[1] * b1 + c2[1] * b2,
                    c0[2] * b0 + c1[2] * b1 + c2[2] * b2,
                    c0[3] * b0 + c1[3] * b1 + c2[3] * b2,
                ];

                // Texture modulation.
                if let Some(tex) = texture {
                    let u = uv0[0] * b0 + uv1[0] * b1 + uv2[0] * b2;
                    let v = uv0[1] * b0 + uv1[1] * b1 + uv2[1] * b2;
                    let tex_color = sample_texture(tex, u, v);
                    src[0] *= tex_color[0];
                    src[1] *= tex_color[1];
                    src[2] *= tex_color[2];
                    src[3] *= tex_color[3];
                }

                // Read destination color for blending.
                let dst = [
                    pixels[offset] as f32 / 255.0,
                    pixels[offset + 1] as f32 / 255.0,
                    pixels[offset + 2] as f32 / 255.0,
                    pixels[offset + 3] as f32 / 255.0,
                ];

                let blended = blend_colors(src, dst, blend, blend_color);

                // Apply color write mask.
                if color_mask.r {
                    pixels[offset] = (blended[0] * 255.0).round().clamp(0.0, 255.0) as u8;
                }
                if color_mask.g {
                    pixels[offset + 1] = (blended[1] * 255.0).round().clamp(0.0, 255.0) as u8;
                }
                if color_mask.b {
                    pixels[offset + 2] = (blended[2] * 255.0).round().clamp(0.0, 255.0) as u8;
                }
                if color_mask.a {
                    pixels[offset + 3] = (blended[3] * 255.0).round().clamp(0.0, 255.0) as u8;
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

    /// Helper: default scissor (disabled).
    fn default_scissor() -> ScissorInfo {
        ScissorInfo {
            enabled: false,
            min_x: 0,
            max_x: 0,
            min_y: 0,
            max_y: 0,
        }
    }

    /// Helper: default depth/stencil (disabled).
    fn default_depth_stencil() -> DepthStencilInfo {
        DepthStencilInfo {
            depth_test_enable: false,
            depth_write_enable: false,
            depth_func: ComparisonOp::Always,
            depth_mode: DepthMode::ZeroToOne,
            stencil_enable: false,
            stencil_two_side: false,
            front: StencilFaceInfo::default(),
            back: StencilFaceInfo::default(),
        }
    }

    /// Helper: default blend (disabled).
    fn default_blend() -> BlendInfo {
        BlendInfo::default()
    }

    /// Helper: default blend color.
    fn default_blend_color() -> BlendColorInfo {
        BlendColorInfo {
            r: 0.0,
            g: 0.0,
            b: 0.0,
            a: 0.0,
        }
    }

    /// Helper: default color mask (all enabled).
    fn default_color_mask() -> ColorMaskInfo {
        ColorMaskInfo::default()
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
    fn test_viewport_transform_preserves_z() {
        let result = viewport_transform([0.0, 0.0, 0.75, 1.0], 0.0, 0.0, 100.0, 100.0);
        assert!((result[2] - 0.75).abs() < f32::EPSILON);
    }

    #[test]
    fn test_rasterize_triangle_fills_pixels() {
        let width = 10u32;
        let height = 10u32;
        let mut pixels = vec![0u8; (width * height * 4) as usize];
        let mut depth_buf = vec![1.0f32; (width * height) as usize];
        let white = [1.0f32, 1.0, 1.0, 1.0];

        // A triangle covering the upper-left quadrant.
        let v0 = [0.0, 0.0, 0.0];
        let v1 = [5.0, 0.0, 0.0];
        let v2 = [0.0, 5.0, 0.0];
        rasterize_triangle(
            v0, v1, v2, white, white, white,
            [0.0, 0.0], [0.0, 0.0], [0.0, 0.0], None, &mut pixels, &mut depth_buf,
            width, height, &default_scissor(), &default_depth_stencil(),
            &default_blend(), &default_blend_color(), &default_color_mask(),
        );

        // Pixel (1, 1) should be inside the triangle.
        let offset = ((1 * width + 1) * 4) as usize;
        assert_eq!(&pixels[offset..offset + 4], &[255, 255, 255, 255]);

        // Pixel (9, 9) should NOT be filled.
        let offset = ((9 * width + 9) * 4) as usize;
        assert_eq!(&pixels[offset..offset + 4], &[0, 0, 0, 0]);
    }

    #[test]
    fn test_rasterize_triangle_clips_to_bounds() {
        let width = 4u32;
        let height = 4u32;
        let mut pixels = vec![0u8; (width * height * 4) as usize];
        let mut depth_buf = vec![1.0f32; (width * height) as usize];
        let white = [1.0f32, 1.0, 1.0, 1.0];

        // Triangle extending way past framebuffer edges.
        let v0 = [-10.0, -10.0, 0.0];
        let v1 = [100.0, -10.0, 0.0];
        let v2 = [-10.0, 100.0, 0.0];
        rasterize_triangle(
            v0, v1, v2, white, white, white,
            [0.0, 0.0], [0.0, 0.0], [0.0, 0.0], None, &mut pixels, &mut depth_buf,
            width, height, &default_scissor(), &default_depth_stencil(),
            &default_blend(), &default_blend_color(), &default_color_mask(),
        );

        // All pixels should be filled (triangle covers everything).
        for y in 0..height {
            for x in 0..width {
                let offset = ((y * width + x) * 4) as usize;
                assert_eq!(
                    &pixels[offset..offset + 4],
                    &[255, 255, 255, 255],
                    "pixel ({}, {}) should be filled",
                    x,
                    y
                );
            }
        }
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
            SoftwareRasterizer::render_draw_calls(&[], &|_, _| {}, &|_, _| {}, Some(base_fb));
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
            if addr >= 0x5000 && addr < 0x5000 + vertex_data_clone.len() as u64 {
                let off = (addr - 0x5000) as usize;
                let len = buf.len().min(vertex_data_clone.len() - off);
                buf[..len].copy_from_slice(&vertex_data_clone[off..off + len]);
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
            SoftwareRasterizer::render_draw_calls(&[draw], &read_gpu, &|_, _| {}, Some(base_fb));
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

    // --- Phase 25 new tests ---

    #[test]
    fn test_depth_test_passes_helper() {
        assert!(!depth_test_passes(ComparisonOp::Never, 0.5, 1.0));
        assert!(depth_test_passes(ComparisonOp::Always, 0.5, 1.0));
        assert!(depth_test_passes(ComparisonOp::Less, 0.3, 0.5));
        assert!(!depth_test_passes(ComparisonOp::Less, 0.5, 0.3));
        assert!(!depth_test_passes(ComparisonOp::Less, 0.5, 0.5));
        assert!(depth_test_passes(ComparisonOp::Equal, 0.5, 0.5));
        assert!(!depth_test_passes(ComparisonOp::Equal, 0.5, 0.6));
        assert!(depth_test_passes(ComparisonOp::LessEqual, 0.5, 0.5));
        assert!(depth_test_passes(ComparisonOp::LessEqual, 0.3, 0.5));
        assert!(!depth_test_passes(ComparisonOp::LessEqual, 0.6, 0.5));
        assert!(depth_test_passes(ComparisonOp::Greater, 0.6, 0.5));
        assert!(!depth_test_passes(ComparisonOp::Greater, 0.3, 0.5));
        assert!(depth_test_passes(ComparisonOp::NotEqual, 0.3, 0.5));
        assert!(!depth_test_passes(ComparisonOp::NotEqual, 0.5, 0.5));
        assert!(depth_test_passes(ComparisonOp::GreaterEqual, 0.5, 0.5));
        assert!(depth_test_passes(ComparisonOp::GreaterEqual, 0.6, 0.5));
        assert!(!depth_test_passes(ComparisonOp::GreaterEqual, 0.3, 0.5));
    }

    #[test]
    fn test_blend_equation_variants() {
        assert!((blend_equation(BlendEquation::Add, 0.3, 0.5) - 0.8).abs() < 0.001);
        assert!((blend_equation(BlendEquation::Subtract, 0.5, 0.3) - 0.2).abs() < 0.001);
        assert!(
            (blend_equation(BlendEquation::ReverseSubtract, 0.3, 0.5) - 0.2).abs() < 0.001
        );
        assert!((blend_equation(BlendEquation::Min, 0.3, 0.5) - 0.3).abs() < 0.001);
        assert!((blend_equation(BlendEquation::Max, 0.3, 0.5) - 0.5).abs() < 0.001);
    }

    #[test]
    fn test_vertex_color_interpolation() {
        // Triangle at screen coords with red, green, blue vertices.
        let width = 10u32;
        let height = 10u32;
        let mut pixels = vec![0u8; (width * height * 4) as usize];
        let mut depth_buf = vec![1.0f32; (width * height) as usize];

        let v0 = [1.0, 1.0, 0.0f32];
        let v1 = [9.0, 1.0, 0.0];
        let v2 = [5.0, 9.0, 0.0];

        let c0 = [1.0, 0.0, 0.0, 1.0]; // red
        let c1 = [0.0, 1.0, 0.0, 1.0]; // green
        let c2 = [0.0, 0.0, 1.0, 1.0]; // blue

        rasterize_triangle(
            v0, v1, v2, c0, c1, c2,
            [0.0, 0.0], [0.0, 0.0], [0.0, 0.0], None, &mut pixels, &mut depth_buf,
            width, height, &default_scissor(), &default_depth_stencil(),
            &default_blend(), &default_blend_color(), &default_color_mask(),
        );

        // Center pixel (~5, ~4) should have a mix of colors (not pure white or black).
        let cx = 5u32;
        let cy = 4u32;
        let off = ((cy * width + cx) * 4) as usize;
        let r = pixels[off];
        let g = pixels[off + 1];
        let b = pixels[off + 2];
        // Should have contributions from all three vertices.
        assert!(r > 0, "red channel should be > 0");
        assert!(g > 0, "green channel should be > 0");
        assert!(b > 0, "blue channel should be > 0");
        // No single channel should be 255 at the centroid.
        assert!(r < 255 || g < 255 || b < 255, "should be a mix, not pure white");
    }

    #[test]
    fn test_depth_test_less() {
        let width = 4u32;
        let height = 4u32;
        let mut pixels = vec![0u8; (width * height * 4) as usize];
        let mut depth_buf = vec![1.0f32; (width * height) as usize];
        let ds = DepthStencilInfo {
            depth_test_enable: true,
            depth_write_enable: true,
            depth_func: ComparisonOp::Less,
            depth_mode: DepthMode::ZeroToOne,
            stencil_enable: false,
            stencil_two_side: false,
            front: StencilFaceInfo::default(),
            back: StencilFaceInfo::default(),
        };

        // First triangle at z=0.5, red.
        let v0 = [-1.0, -1.0, 0.5f32];
        let v1 = [20.0, -1.0, 0.5];
        let v2 = [-1.0, 20.0, 0.5];
        let red = [1.0, 0.0, 0.0, 1.0];
        rasterize_triangle(
            v0, v1, v2, red, red, red,
            [0.0, 0.0], [0.0, 0.0], [0.0, 0.0], None, &mut pixels, &mut depth_buf,
            width, height, &default_scissor(), &ds,
            &default_blend(), &default_blend_color(), &default_color_mask(),
        );

        let off = ((1 * width + 1) * 4) as usize;
        assert_eq!(pixels[off], 255); // red
        assert_eq!(pixels[off + 1], 0);

        // Second triangle at z=0.3 (nearer), green — should win.
        let green = [0.0, 1.0, 0.0, 1.0];
        let v0b = [-1.0, -1.0, 0.3f32];
        let v1b = [20.0, -1.0, 0.3];
        let v2b = [-1.0, 20.0, 0.3];
        rasterize_triangle(
            v0b, v1b, v2b, green, green, green,
            [0.0, 0.0], [0.0, 0.0], [0.0, 0.0], None, &mut pixels, &mut depth_buf,
            width, height, &default_scissor(), &ds,
            &default_blend(), &default_blend_color(), &default_color_mask(),
        );

        assert_eq!(pixels[off], 0);
        assert_eq!(pixels[off + 1], 255); // green overwrote

        // Third triangle at z=0.8 (farther), blue — should be rejected.
        let blue = [0.0, 0.0, 1.0, 1.0];
        let v0c = [-1.0, -1.0, 0.8f32];
        let v1c = [20.0, -1.0, 0.8];
        let v2c = [-1.0, 20.0, 0.8];
        rasterize_triangle(
            v0c, v1c, v2c, blue, blue, blue,
            [0.0, 0.0], [0.0, 0.0], [0.0, 0.0], None, &mut pixels, &mut depth_buf,
            width, height, &default_scissor(), &ds,
            &default_blend(), &default_blend_color(), &default_color_mask(),
        );

        assert_eq!(pixels[off], 0);
        assert_eq!(pixels[off + 1], 255); // still green
        assert_eq!(pixels[off + 2], 0); // blue rejected
    }

    #[test]
    fn test_depth_test_disabled() {
        let width = 4u32;
        let height = 4u32;
        let mut pixels = vec![0u8; (width * height * 4) as usize];
        let mut depth_buf = vec![1.0f32; (width * height) as usize];
        let ds = default_depth_stencil(); // disabled

        // First triangle: red at z=0.3.
        let red = [1.0, 0.0, 0.0, 1.0];
        rasterize_triangle(
            [-1.0, -1.0, 0.3], [20.0, -1.0, 0.3], [-1.0, 20.0, 0.3],
            red, red, red,
            [0.0, 0.0], [0.0, 0.0], [0.0, 0.0], None, &mut pixels, &mut depth_buf,
            width, height, &default_scissor(), &ds,
            &default_blend(), &default_blend_color(), &default_color_mask(),
        );

        // Second triangle: blue at z=0.8 (farther). Should still overwrite because depth disabled.
        let blue = [0.0, 0.0, 1.0, 1.0];
        rasterize_triangle(
            [-1.0, -1.0, 0.8], [20.0, -1.0, 0.8], [-1.0, 20.0, 0.8],
            blue, blue, blue,
            [0.0, 0.0], [0.0, 0.0], [0.0, 0.0], None, &mut pixels, &mut depth_buf,
            width, height, &default_scissor(), &ds,
            &default_blend(), &default_blend_color(), &default_color_mask(),
        );

        let off = ((1 * width + 1) * 4) as usize;
        assert_eq!(pixels[off], 0);
        assert_eq!(pixels[off + 2], 255); // blue overwrote
    }

    #[test]
    fn test_alpha_blend_src_alpha() {
        let width = 4u32;
        let height = 4u32;
        let mut pixels = vec![0u8; (width * height * 4) as usize];
        let mut depth_buf = vec![1.0f32; (width * height) as usize];

        // Fill background with solid red.
        let red = [1.0, 0.0, 0.0, 1.0];
        rasterize_triangle(
            [-1.0, -1.0, 0.0], [20.0, -1.0, 0.0], [-1.0, 20.0, 0.0],
            red, red, red,
            [0.0, 0.0], [0.0, 0.0], [0.0, 0.0], None, &mut pixels, &mut depth_buf,
            width, height, &default_scissor(), &default_depth_stencil(),
            &default_blend(), &default_blend_color(), &default_color_mask(),
        );

        // Overlay with 50% transparent green.
        let blend = BlendInfo {
            enabled: true,
            separate_alpha: false,
            color_op: BlendEquation::Add,
            color_src: BlendFactor::SrcAlpha,
            color_dst: BlendFactor::OneMinusSrcAlpha,
            alpha_op: BlendEquation::Add,
            alpha_src: BlendFactor::One,
            alpha_dst: BlendFactor::Zero,
        };
        let green_half = [0.0, 1.0, 0.0, 0.5];
        rasterize_triangle(
            [-1.0, -1.0, 0.0], [20.0, -1.0, 0.0], [-1.0, 20.0, 0.0],
            green_half, green_half, green_half,
            [0.0, 0.0], [0.0, 0.0], [0.0, 0.0], None, &mut pixels, &mut depth_buf,
            width, height, &default_scissor(), &default_depth_stencil(),
            &blend, &default_blend_color(), &default_color_mask(),
        );

        let off = ((1 * width + 1) * 4) as usize;
        let r = pixels[off];
        let g = pixels[off + 1];
        // Red should be about 127 (0.5 * 255), green about 127.
        assert!(r > 100 && r < 155, "red={}, expected ~127", r);
        assert!(g > 100 && g < 155, "green={}, expected ~127", g);
    }

    #[test]
    fn test_scissor_clipping() {
        let width = 10u32;
        let height = 10u32;
        let mut pixels = vec![0u8; (width * height * 4) as usize];
        let mut depth_buf = vec![1.0f32; (width * height) as usize];
        let white = [1.0f32, 1.0, 1.0, 1.0];

        let scissor = ScissorInfo {
            enabled: true,
            min_x: 3,
            max_x: 7, // exclusive upper bound
            min_y: 3,
            max_y: 7,
        };

        // Big triangle covering entire framebuffer.
        rasterize_triangle(
            [-1.0, -1.0, 0.0], [20.0, -1.0, 0.0], [-1.0, 20.0, 0.0],
            white, white, white,
            [0.0, 0.0], [0.0, 0.0], [0.0, 0.0], None, &mut pixels, &mut depth_buf,
            width, height, &scissor, &default_depth_stencil(),
            &default_blend(), &default_blend_color(), &default_color_mask(),
        );

        // Pixel (1, 1) should be empty (outside scissor).
        let off_outside = ((1 * width + 1) * 4) as usize;
        assert_eq!(pixels[off_outside], 0);

        // Pixel (4, 4) should be filled (inside scissor).
        let off_inside = ((4 * width + 4) * 4) as usize;
        assert_eq!(pixels[off_inside], 255);

        // Pixel (8, 8) should be empty (outside scissor).
        let off_far = ((8 * width + 8) * 4) as usize;
        assert_eq!(pixels[off_far], 0);
    }

    #[test]
    fn test_color_write_mask() {
        let width = 4u32;
        let height = 4u32;
        // Fill with green (0, 128, 0, 255).
        let mut pixels = vec![0u8; (width * height * 4) as usize];
        for i in 0..(width * height) as usize {
            pixels[i * 4] = 0;
            pixels[i * 4 + 1] = 128;
            pixels[i * 4 + 2] = 0;
            pixels[i * 4 + 3] = 255;
        }
        let mut depth_buf = vec![1.0f32; (width * height) as usize];

        // Write white but mask out green and alpha channels.
        let mask = ColorMaskInfo {
            r: true,
            g: false,
            b: true,
            a: false,
        };
        let white = [1.0f32, 1.0, 1.0, 1.0];

        rasterize_triangle(
            [-1.0, -1.0, 0.0], [20.0, -1.0, 0.0], [-1.0, 20.0, 0.0],
            white, white, white,
            [0.0, 0.0], [0.0, 0.0], [0.0, 0.0], None, &mut pixels, &mut depth_buf,
            width, height, &default_scissor(), &default_depth_stencil(),
            &default_blend(), &default_blend_color(), &mask,
        );

        let off = ((1 * width + 1) * 4) as usize;
        assert_eq!(pixels[off], 255);     // R written
        assert_eq!(pixels[off + 1], 128); // G preserved
        assert_eq!(pixels[off + 2], 255); // B written
        assert_eq!(pixels[off + 3], 255); // A preserved
    }

    #[test]
    fn test_backface_culling() {
        // signed_area([0,0,0], [1,0,0], [0,1,0]) = (1)*(1) - (0)*(0) = 1.0 (positive = CCW).
        let area = signed_area([0.0, 0.0, 0.0], [1.0, 0.0, 0.0], [0.0, 1.0, 0.0]);
        assert!(area > 0.0); // Positive = CCW winding

        // CCW front_face + Back culling: positive area = CCW = front → NOT culled.
        let rast_ccw_back = RasterizerInfo {
            cull_enable: true,
            front_face: FrontFace::CCW,
            cull_face: CullFace::Back,
            polygon_mode_front: PolygonMode::Fill,
            polygon_mode_back: PolygonMode::Fill,
            line_width_smooth: 1.0,
            line_width_aliased: 1.0,
            depth_bias: 0.0,
            slope_scale_depth_bias: 0.0,
            depth_bias_clamp: 0.0,
        };
        assert!(!should_cull(area, &rast_ccw_back));

        // CW front_face + Back culling: positive area = CCW ≠ CW → back face → IS culled.
        let rast_cw_back = RasterizerInfo {
            cull_enable: true,
            front_face: FrontFace::CW,
            cull_face: CullFace::Back,
            polygon_mode_front: PolygonMode::Fill,
            polygon_mode_back: PolygonMode::Fill,
            line_width_smooth: 1.0,
            line_width_aliased: 1.0,
            depth_bias: 0.0,
            slope_scale_depth_bias: 0.0,
            depth_bias_clamp: 0.0,
        };
        assert!(should_cull(area, &rast_cw_back));

        // CCW front_face + Front culling: positive area = CCW = front → IS culled.
        let rast_ccw_front = RasterizerInfo {
            cull_enable: true,
            front_face: FrontFace::CCW,
            cull_face: CullFace::Front,
            polygon_mode_front: PolygonMode::Fill,
            polygon_mode_back: PolygonMode::Fill,
            line_width_smooth: 1.0,
            line_width_aliased: 1.0,
            depth_bias: 0.0,
            slope_scale_depth_bias: 0.0,
            depth_bias_clamp: 0.0,
        };
        assert!(should_cull(area, &rast_ccw_front));

        // Cull disabled — never culled.
        let rast_off = RasterizerInfo {
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
        };
        assert!(!should_cull(area, &rast_off));

        // FrontAndBack culls everything.
        let rast_both = RasterizerInfo {
            cull_enable: true,
            front_face: FrontFace::CCW,
            cull_face: CullFace::FrontAndBack,
            polygon_mode_front: PolygonMode::Fill,
            polygon_mode_back: PolygonMode::Fill,
            line_width_smooth: 1.0,
            line_width_aliased: 1.0,
            depth_bias: 0.0,
            slope_scale_depth_bias: 0.0,
            depth_bias_clamp: 0.0,
        };
        assert!(should_cull(area, &rast_both));
    }

    #[test]
    fn test_no_color_attrib_defaults_white() {
        // Draw call with only a position attribute — no color.
        let mut vertex_data = vec![0u8; 24];
        vertex_data[0..4].copy_from_slice(&(-1.0f32).to_le_bytes());
        vertex_data[4..8].copy_from_slice(&(-1.0f32).to_le_bytes());
        vertex_data[8..12].copy_from_slice(&(1.0f32).to_le_bytes());
        vertex_data[12..16].copy_from_slice(&(-1.0f32).to_le_bytes());
        vertex_data[16..20].copy_from_slice(&(-1.0f32).to_le_bytes());
        vertex_data[20..24].copy_from_slice(&(1.0f32).to_le_bytes());

        let vertex_data_clone = vertex_data.clone();
        let read_gpu = move |addr: u64, buf: &mut [u8]| {
            if addr >= 0x5000 && addr < 0x5000 + vertex_data_clone.len() as u64 {
                let off = (addr - 0x5000) as usize;
                let len = buf.len().min(vertex_data_clone.len() - off);
                buf[..len].copy_from_slice(&vertex_data_clone[off..off + len]);
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
            pixels: vec![0u8; 64],
        };

        let result =
            SoftwareRasterizer::render_draw_calls(&[draw], &read_gpu, &|_, _| {}, Some(base_fb));
        let fb = result.unwrap();

        // Filled pixels should be white (255, 255, 255, 255).
        let has_white = fb.pixels.chunks_exact(4).any(|px| {
            px[0] == 255 && px[1] == 255 && px[2] == 255 && px[3] == 255
        });
        assert!(has_white, "with no color attribute, pixels should be white");
    }

    #[test]
    fn test_signed_area_winding() {
        // CCW in math coords, but we're in screen space.
        let area = signed_area([0.0, 0.0, 0.0], [10.0, 0.0, 0.0], [0.0, 10.0, 0.0]);
        // (10-0)*(10-0) - (0-0)*(0-0) = 100 > 0
        assert!(area > 0.0);

        // Reversed winding.
        let area_rev = signed_area([0.0, 0.0, 0.0], [0.0, 10.0, 0.0], [10.0, 0.0, 0.0]);
        assert!(area_rev < 0.0);
    }

    // --- Phase 26: Texture Sampling tests ---

    #[test]
    fn test_bytes_per_texel() {
        assert_eq!(bytes_per_texel(TextureFormat::A8B8G8R8), Some(4));
        assert_eq!(bytes_per_texel(TextureFormat::R8G8B8A8), Some(4));
        assert_eq!(bytes_per_texel(TextureFormat::B5G6R5), Some(2));
        assert_eq!(bytes_per_texel(TextureFormat::R8G8), Some(2));
        assert_eq!(bytes_per_texel(TextureFormat::R8), Some(1));
        assert_eq!(bytes_per_texel(TextureFormat::R32G32B32A32), Some(16));
        // Compressed formats return None.
        assert_eq!(bytes_per_texel(TextureFormat::Bc1Rgba), None);
        assert_eq!(bytes_per_texel(TextureFormat::Astc2d4x4), None);
    }

    #[test]
    fn test_decode_texel_a8b8g8r8() {
        // Bytes [R, G, B, A] in LE layout.
        let bytes = [100u8, 150, 200, 255];
        let rgba = decode_texel(&bytes, TextureFormat::A8B8G8R8, ComponentType::UNorm).unwrap();
        assert_eq!(rgba, [100, 150, 200, 255]);
    }

    #[test]
    fn test_decode_texel_b5g6r5() {
        // Pure red: R=31, G=0, B=0 → u16 = 31 << 11 = 0xF800.
        let val: u16 = 0xF800;
        let bytes = val.to_le_bytes();
        let rgba = decode_texel(&bytes, TextureFormat::B5G6R5, ComponentType::UNorm).unwrap();
        assert!(rgba[0] >= 248, "red={}, expected >=248", rgba[0]);
        assert_eq!(rgba[1], 0);
        assert_eq!(rgba[2], 0);
        assert_eq!(rgba[3], 255);
    }

    #[test]
    fn test_decode_texel_r8() {
        let bytes = [42u8];
        let rgba = decode_texel(&bytes, TextureFormat::R8, ComponentType::UNorm).unwrap();
        assert_eq!(rgba, [42, 0, 0, 255]);
    }

    #[test]
    fn test_decode_texel_float() {
        let mut bytes = [0u8; 16];
        bytes[0..4].copy_from_slice(&(0.5f32).to_le_bytes());
        bytes[4..8].copy_from_slice(&(0.25f32).to_le_bytes());
        bytes[8..12].copy_from_slice(&(1.0f32).to_le_bytes());
        bytes[12..16].copy_from_slice(&(0.75f32).to_le_bytes());
        let rgba =
            decode_texel(&bytes, TextureFormat::R32G32B32A32, ComponentType::Float).unwrap();
        assert_eq!(rgba[0], 128); // 0.5 * 255 = 127.5 → 128
        assert_eq!(rgba[1], 64);  // 0.25 * 255 = 63.75 → 64
        assert_eq!(rgba[2], 255); // 1.0 * 255 = 255
        assert_eq!(rgba[3], 191); // 0.75 * 255 = 191.25 → 191
    }

    #[test]
    fn test_apply_swizzle() {
        let rgba = [10, 20, 30, 40];
        // Identity: RGBA → RGBA.
        let identity = [SwizzleSource::R, SwizzleSource::G, SwizzleSource::B, SwizzleSource::A];
        assert_eq!(apply_swizzle(rgba, &identity), [10, 20, 30, 40]);

        // Swap R and B.
        let swap_rb = [SwizzleSource::B, SwizzleSource::G, SwizzleSource::R, SwizzleSource::A];
        assert_eq!(apply_swizzle(rgba, &swap_rb), [30, 20, 10, 40]);

        // Zero and One.
        let special = [SwizzleSource::Zero, SwizzleSource::OneFloat, SwizzleSource::R, SwizzleSource::Zero];
        assert_eq!(apply_swizzle(rgba, &special), [0, 255, 10, 0]);
    }

    #[test]
    fn test_apply_wrap_mode_repeat() {
        // Positive wrap.
        assert_eq!(apply_wrap_mode(4.5, 4, WrapMode::Wrap), 0);
        assert_eq!(apply_wrap_mode(5.0, 4, WrapMode::Wrap), 1);
        // Negative wrap.
        assert_eq!(apply_wrap_mode(-1.0, 4, WrapMode::Wrap), 3);
        assert_eq!(apply_wrap_mode(-0.5, 4, WrapMode::Wrap), 3);
    }

    #[test]
    fn test_apply_wrap_mode_clamp() {
        assert_eq!(apply_wrap_mode(-1.0, 4, WrapMode::ClampToEdge), 0);
        assert_eq!(apply_wrap_mode(0.5, 4, WrapMode::ClampToEdge), 0);
        assert_eq!(apply_wrap_mode(3.5, 4, WrapMode::ClampToEdge), 3);
        assert_eq!(apply_wrap_mode(10.0, 4, WrapMode::ClampToEdge), 3);
    }

    #[test]
    fn test_apply_wrap_mode_mirror() {
        // Within first period (0..4): direct mapping.
        assert_eq!(apply_wrap_mode(1.5, 4, WrapMode::Mirror), 1);
        // In reflected region (4..8): mirrors back.
        assert_eq!(apply_wrap_mode(5.0, 4, WrapMode::Mirror), 2);
        // Negative: mirrors.
        let result = apply_wrap_mode(-1.0, 4, WrapMode::Mirror);
        assert!(result < 4, "mirrored result {} should be < 4", result);
    }

    #[test]
    fn test_sample_texture_basic() {
        // 2x2 texture: red, green, blue, white.
        let data = vec![
            255, 0, 0, 255,    // (0,0) red
            0, 255, 0, 255,    // (1,0) green
            0, 0, 255, 255,    // (0,1) blue
            255, 255, 255, 255, // (1,1) white
        ];
        let tex = SampledTexture {
            data,
            width: 2,
            height: 2,
            wrap_u: WrapMode::ClampToEdge,
            wrap_v: WrapMode::ClampToEdge,
            normalized_coords: false,
        };

        // Sample at (0.5, 0.5) → texel (0, 0) = red.
        let c = sample_texture(&tex, 0.5, 0.5);
        assert!((c[0] - 1.0).abs() < 0.01, "R={}", c[0]);
        assert!(c[1] < 0.01);
        assert!(c[2] < 0.01);

        // Sample at (1.5, 0.5) → texel (1, 0) = green.
        let c = sample_texture(&tex, 1.5, 0.5);
        assert!(c[0] < 0.01);
        assert!((c[1] - 1.0).abs() < 0.01);
    }

    #[test]
    fn test_textured_triangle() {
        // Full pipeline: white vertices × solid red 2x2 texture → red pixels.
        let width = 8u32;
        let height = 8u32;
        let mut pixels = vec![0u8; (width * height * 4) as usize];
        let mut depth_buf = vec![1.0f32; (width * height) as usize];
        let white = [1.0f32, 1.0, 1.0, 1.0];

        // Solid red 2x2 texture.
        let tex = SampledTexture {
            data: vec![255, 0, 0, 255, 255, 0, 0, 255, 255, 0, 0, 255, 255, 0, 0, 255],
            width: 2,
            height: 2,
            wrap_u: WrapMode::ClampToEdge,
            wrap_v: WrapMode::ClampToEdge,
            normalized_coords: true,
        };

        // Large triangle covering the framebuffer, UVs spanning [0,1].
        rasterize_triangle(
            [-1.0, -1.0, 0.0], [20.0, -1.0, 0.0], [-1.0, 20.0, 0.0],
            white, white, white,
            [0.0, 0.0], [1.0, 0.0], [0.0, 1.0], Some(&tex),
            &mut pixels, &mut depth_buf,
            width, height, &default_scissor(), &default_depth_stencil(),
            &default_blend(), &default_blend_color(), &default_color_mask(),
        );

        // Center pixel should be red.
        let off = ((4 * width + 4) * 4) as usize;
        assert_eq!(pixels[off], 255, "R should be 255");
        assert_eq!(pixels[off + 1], 0, "G should be 0");
        assert_eq!(pixels[off + 2], 0, "B should be 0");
        assert_eq!(pixels[off + 3], 255, "A should be 255");
    }

    #[test]
    fn test_no_texture_backwards_compat() {
        // No TIC pool → load_texture returns None → identical to Phase 25.
        let draw = default_draw_call();
        assert_eq!(draw.tex_header_pool_addr, 0);
        let tex = load_texture(&draw, &|_, _| {});
        assert!(tex.is_none());
    }

    // --- Phase 27: BlockLinear Deswizzle + Framebuffer Writeback tests ---

    #[test]
    fn test_gob_offset_origin() {
        assert_eq!(gob_offset(0, 0), 0);
    }

    #[test]
    fn test_gob_offset_known_positions() {
        // (16, 0): first 16-byte group in second half of first row pair → 32
        assert_eq!(gob_offset(16, 0), 32);
        // (0, 1): second row of first row-pair → 16
        assert_eq!(gob_offset(0, 1), 16);
        // (0, 2): second row-pair → 64
        assert_eq!(gob_offset(0, 2), 64);
        // (32, 0): second 32-byte half → 256
        assert_eq!(gob_offset(32, 0), 256);
    }

    #[test]
    fn test_gob_offset_range() {
        // All 512 byte positions in a 64×8 GOB must be unique and < 512.
        let mut seen = vec![false; 512];
        for y in 0..8u32 {
            for x in 0..64u32 {
                let off = gob_offset(x, y) as usize;
                assert!(off < 512, "gob_offset({}, {}) = {} >= 512", x, y, off);
                assert!(!seen[off], "duplicate offset {} at ({}, {})", off, x, y);
                seen[off] = true;
            }
        }
        // All 512 positions should be covered.
        assert!(seen.iter().all(|&s| s), "not all GOB offsets covered");
    }

    #[test]
    fn test_align_up() {
        assert_eq!(align_up(0, 64), 0);
        assert_eq!(align_up(1, 64), 64);
        assert_eq!(align_up(64, 64), 64);
        assert_eq!(align_up(65, 64), 128);
        assert_eq!(align_up(100, 8), 104);
    }

    #[test]
    fn test_block_linear_size() {
        // 16×8 RGBA8 (bpt=4): width_bytes=64=1 GOB wide, height=8=1 GOB tall
        // block_height=0 → 1 GOB per block → 64×8 = 512 bytes
        assert_eq!(block_linear_size(16, 8, 4, 0), 512);

        // 32×8 RGBA8: width_bytes=128=2 GOBs wide, height=8=1 GOB tall
        assert_eq!(block_linear_size(32, 8, 4, 0), 1024);

        // 16×16 RGBA8, block_height=1 (2 GOBs/block): block_height_pixels=16
        // aligned_width=64, aligned_height=16 → 64×16=1024
        assert_eq!(block_linear_size(16, 16, 4, 1), 1024);

        // 16×9 RGBA8, block_height=0: aligned_height=16 (rounds 9 up to 8-multiple=16)
        assert_eq!(block_linear_size(16, 9, 4, 0), 1024);
    }

    #[test]
    fn test_deswizzle_block_linear_1x1_gob() {
        // 16×8 RGBA8 texture = exactly 1 GOB. Fill tiled buffer via gob_offset,
        // then deswizzle should produce linear row-major order.
        let width = 16u32;
        let height = 8u32;
        let bpt = 4u32;
        let block_height = 0u32;
        let mut tiled = vec![0u8; 512];

        // Write a unique byte pattern for each pixel using the GOB formula.
        for y in 0..height {
            for x in 0..width {
                let x_bytes = x * bpt;
                let off = gob_offset(x_bytes, y) as usize;
                // Store pixel ID = y * width + x as RGBA.
                let id = (y * width + x) as u8;
                tiled[off] = id;
                tiled[off + 1] = id;
                tiled[off + 2] = id;
                tiled[off + 3] = 255;
            }
        }

        let linear = deswizzle_block_linear(&tiled, width, height, bpt, block_height);
        assert_eq!(linear.len(), (width * height * bpt) as usize);

        // Verify each pixel in linear layout.
        for y in 0..height {
            for x in 0..width {
                let lin_off = ((y * width + x) * bpt) as usize;
                let id = (y * width + x) as u8;
                assert_eq!(
                    linear[lin_off], id,
                    "pixel ({}, {}) R mismatch", x, y
                );
            }
        }
    }

    #[test]
    fn test_deswizzle_block_linear_multi_gob() {
        // 32×8 RGBA8 = 2 GOB columns, 1 GOB row, block_height=0.
        let width = 32u32;
        let height = 8u32;
        let bpt = 4u32;
        let block_height = 0u32;
        let size = block_linear_size(width, height, bpt, block_height);
        let mut tiled = vec![0u8; size];

        // Write via the full deswizzle address formula.
        let gobs_in_x = align_up(width * bpt, GOB_SIZE_X) / GOB_SIZE_X;
        let bh_gobs = 1u32 << block_height;
        let block_size = gobs_in_x * GOB_SIZE * bh_gobs;

        for y in 0..height {
            for x in 0..width {
                let x_bytes = x * bpt;
                let gob_x = x_bytes / GOB_SIZE_X;
                let gob_y = y / GOB_SIZE_Y;
                let block_y = gob_y / bh_gobs;
                let gob_in_block = gob_y % bh_gobs;
                let base = block_y * block_size
                    + gob_x * GOB_SIZE * bh_gobs
                    + gob_in_block * GOB_SIZE;
                let intra = gob_offset(x_bytes % GOB_SIZE_X, y % GOB_SIZE_Y);
                let off = (base + intra) as usize;
                let id = ((y * width + x) & 0xFF) as u8;
                tiled[off] = id;
                tiled[off + 1] = id;
                tiled[off + 2] = id;
                tiled[off + 3] = 255;
            }
        }

        let linear = deswizzle_block_linear(&tiled, width, height, bpt, block_height);
        for y in 0..height {
            for x in 0..width {
                let lin_off = ((y * width + x) * bpt) as usize;
                let id = ((y * width + x) & 0xFF) as u8;
                assert_eq!(linear[lin_off], id, "pixel ({}, {}) mismatch", x, y);
            }
        }
    }

    #[test]
    fn test_deswizzle_block_linear_block_height_1() {
        // 16×16 RGBA8, block_height=1 (2 GOBs per block = 16-pixel-tall blocks).
        let width = 16u32;
        let height = 16u32;
        let bpt = 4u32;
        let block_height = 1u32;
        let size = block_linear_size(width, height, bpt, block_height);
        let mut tiled = vec![0u8; size];

        let gobs_in_x = align_up(width * bpt, GOB_SIZE_X) / GOB_SIZE_X;
        let bh_gobs = 1u32 << block_height;
        let block_size = gobs_in_x * GOB_SIZE * bh_gobs;

        for y in 0..height {
            for x in 0..width {
                let x_bytes = x * bpt;
                let gob_x = x_bytes / GOB_SIZE_X;
                let gob_y = y / GOB_SIZE_Y;
                let block_y = gob_y / bh_gobs;
                let gob_in_block = gob_y % bh_gobs;
                let base = block_y * block_size
                    + gob_x * GOB_SIZE * bh_gobs
                    + gob_in_block * GOB_SIZE;
                let intra = gob_offset(x_bytes % GOB_SIZE_X, y % GOB_SIZE_Y);
                let off = (base + intra) as usize;
                let id = ((y * width + x) & 0xFF) as u8;
                tiled[off] = id;
                tiled[off + 1] = !id;
                tiled[off + 2] = 0;
                tiled[off + 3] = 255;
            }
        }

        let linear = deswizzle_block_linear(&tiled, width, height, bpt, block_height);
        for y in 0..height {
            for x in 0..width {
                let lin_off = ((y * width + x) * bpt) as usize;
                let id = ((y * width + x) & 0xFF) as u8;
                assert_eq!(linear[lin_off], id, "R mismatch at ({}, {})", x, y);
                assert_eq!(linear[lin_off + 1], !id, "G mismatch at ({}, {})", x, y);
            }
        }
    }

    #[test]
    fn test_load_texture_block_linear() {
        // Build a BlockLinear 16×8 RGBA8 texture and verify load_texture deswizzles it.
        let width = 16u32;
        let height = 8u32;
        let bpt = 4u32;
        let block_height = 0u32;
        let mut tiled = vec![0u8; 512];

        // Fill tiled buffer: all pixels = solid green (0, 255, 0, 255).
        for y in 0..height {
            for x in 0..width {
                let x_bytes = x * bpt;
                let off = gob_offset(x_bytes, y) as usize;
                tiled[off] = 0;
                tiled[off + 1] = 255;
                tiled[off + 2] = 0;
                tiled[off + 3] = 255;
            }
        }

        // Build TIC words for BlockLinear A8B8G8R8 16×8 texture.
        let mut tic_words = [0u32; 8];
        // word0: format=A8B8G8R8(0x1D), types=UNorm(2), swizzle=RGBA(2,3,4,5)
        tic_words[0] = 0x1D
            | (2 << 7) | (2 << 10) | (2 << 13) | (2 << 16)
            | (2 << 19) | (3 << 22) | (4 << 25) | (5 << 28);
        // word1: addr_low = 0x2000
        tic_words[1] = 0x2000;
        // word2: header_version = BlockLinear(3) at [23:21]
        tic_words[2] = 3 << 21;
        // word3: block_height=0, block_depth=0
        tic_words[3] = 0;
        // word4: width=15(+1=16), texture_type=Texture2D(1) at [26:23]
        tic_words[4] = 15 | (1 << 23);
        // word5: height=7(+1=8), normalized=1
        tic_words[5] = 7 | (1 << 31);

        let mut tic_bytes = [0u8; 32];
        for (i, &w) in tic_words.iter().enumerate() {
            tic_bytes[i * 4..i * 4 + 4].copy_from_slice(&w.to_le_bytes());
        }

        let tiled_clone = tiled.clone();
        let tic_clone = tic_bytes;
        let read_gpu = move |addr: u64, buf: &mut [u8]| {
            if addr == 0x1000 {
                // TIC read
                let len = buf.len().min(32);
                buf[..len].copy_from_slice(&tic_clone[..len]);
            } else if addr >= 0x2000 && addr < 0x2000 + tiled_clone.len() as u64 {
                let off = (addr - 0x2000) as usize;
                let len = buf.len().min(tiled_clone.len() - off);
                buf[..len].copy_from_slice(&tiled_clone[off..off + len]);
            }
        };

        let mut draw = default_draw_call();
        draw.tex_header_pool_addr = 0x1000;

        let tex = load_texture(&draw, &read_gpu).expect("should load BlockLinear texture");
        assert_eq!(tex.width, 16);
        assert_eq!(tex.height, 8);
        // All pixels should be green.
        for i in 0..(width * height) as usize {
            assert_eq!(tex.data[i * 4], 0, "R at pixel {}", i);
            assert_eq!(tex.data[i * 4 + 1], 255, "G at pixel {}", i);
            assert_eq!(tex.data[i * 4 + 2], 0, "B at pixel {}", i);
            assert_eq!(tex.data[i * 4 + 3], 255, "A at pixel {}", i);
        }
    }

    #[test]
    fn test_framebuffer_writeback() {
        // render_draw_calls should call write_gpu with framebuffer pixels when gpu_va != 0.
        let base_fb = Framebuffer {
            gpu_va: 0x1000,
            width: 2,
            height: 2,
            pixels: vec![42u8; 16],
        };

        // Need at least one draw call (even with no vertices) to reach writeback.
        let draw = default_draw_call();

        let written = std::sync::Mutex::new(Vec::new());
        let write_gpu = |addr: u64, data: &[u8]| {
            written.lock().unwrap().push((addr, data.to_vec()));
        };

        let result = SoftwareRasterizer::render_draw_calls(
            &[draw],
            &|_, _| {},
            &write_gpu,
            Some(base_fb),
        );
        let fb = result.unwrap();
        let writes = written.into_inner().unwrap();
        assert_eq!(writes.len(), 1);
        assert_eq!(writes[0].0, 0x1000);
        assert_eq!(writes[0].1, fb.pixels);
    }

    #[test]
    fn test_framebuffer_writeback_no_va() {
        // gpu_va == 0 → no writeback.
        let base_fb = Framebuffer {
            gpu_va: 0,
            width: 2,
            height: 2,
            pixels: vec![0u8; 16],
        };

        let written = std::sync::Mutex::new(Vec::new());
        let write_gpu = |addr: u64, data: &[u8]| {
            written.lock().unwrap().push((addr, data.to_vec()));
        };

        SoftwareRasterizer::render_draw_calls(
            &[],
            &|_, _| {},
            &write_gpu,
            Some(base_fb),
        );
        let writes = written.into_inner().unwrap();
        assert!(writes.is_empty(), "no writeback should occur when gpu_va=0");
    }
}
