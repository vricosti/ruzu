// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_rasterizer.h and gl_rasterizer.cpp
//! Status: EN COURS
//!
//! OpenGL rasterizer — processes Maxwell 3D draw commands using OpenGL.
//! Implements [`RasterizerInterface`]. Currently delegates actual rendering
//! to the software rasterizer; GL-accelerated rendering will be added as
//! buffer/texture/shader caches are ported from zuyu.

use log::{debug, info};
use std::sync::Arc;
use std::time::Instant;

use common::settings;

use super::gl_buffer_cache::BufferCacheParams as OpenGLBufferCacheParams;
use super::gl_device::Device;
use super::gl_fence_manager::{Fence, FenceManagerOpenGL};
use super::gl_query_cache::QueryCache;
use super::gl_shader_cache::ShaderCache as OpenGLShaderCache;
use super::gl_texture_cache::TextureCache as OpenGLTextureCache;
use crate::buffer_cache::buffer_cache::BufferCache as CommonBufferCache;
use crate::buffer_cache::word_manager::DeviceTracker;
use crate::engines::draw_manager::DrawState;
use crate::engines::maxwell_3d::{
    BlendEquation, BlendFactor, ComparisonOp, CullFace, DepthStencilInfo, DrawCall, FrontFace,
    PolygonMode, RasterizerInfo, StencilFaceInfo, StencilOp,
};
use crate::engines::Framebuffer;
use crate::fence_manager::FenceManager;
use crate::framebuffer_config::FramebufferConfig;
use crate::host1x::syncpoint_manager::SyncpointManager;
use crate::query_cache::types::QueryPropertiesFlags;
use crate::query_cache_top::QueryType as VideoQueryType;
use crate::rasterizer::SoftwareRasterizer;
use crate::rasterizer_interface::{RasterizerDownloadArea, RasterizerInterface};
use crate::renderer_base::GuestMemoryWriter;
use crate::renderer_opengl::present::layer::FramebufferTextureInfo;
use crate::shader_cache::ShaderCache;

struct OpenGLDeviceTracker;
static OPENGL_DEVICE_TRACKER: OpenGLDeviceTracker = OpenGLDeviceTracker;

fn maxwell_to_video_core_query(query_type: u32) -> Option<VideoQueryType> {
    match query_type {
        x if x == crate::query_cache::types::QueryType::PrimitivesGenerated as u32
            || x == crate::query_cache::types::QueryType::VtgPrimitivesOut as u32 =>
        {
            Some(VideoQueryType::PrimitivesGenerated)
        }
        x if x == crate::query_cache::types::QueryType::ZPassPixelCount64 as u32 => {
            Some(VideoQueryType::SamplesPassed)
        }
        x if x == crate::query_cache::types::QueryType::StreamingPrimitivesSucceeded as u32 => {
            Some(VideoQueryType::TfbPrimitivesWritten)
        }
        _ => None,
    }
}

/// Adapter that implements `GpuMemoryAccess` for the buffer cache by
/// delegating to the channel's `MemoryManager`.
struct GpuMemoryAccessAdapter {
    mm: Arc<parking_lot::Mutex<crate::memory_manager::MemoryManager>>,
    cpu_reader: crate::shader_environment::GpuMemoryReader,
}

impl crate::buffer_cache::buffer_cache_base::GpuMemoryAccess for GpuMemoryAccessAdapter {
    fn gpu_to_cpu_address(&self, gpu_addr: u64) -> Option<u64> {
        self.mm.lock().gpu_to_cpu_address(gpu_addr)
    }

    fn read_u64(&self, gpu_addr: u64) -> Option<u64> {
        let mut buf = [0u8; 8];
        let reader = &*self.cpu_reader;
        self.mm.lock().read_block(gpu_addr, &mut buf, reader);
        Some(u64::from_le_bytes(buf))
    }

    fn read_u32(&self, gpu_addr: u64) -> Option<u32> {
        let mut buf = [0u8; 4];
        let reader = &*self.cpu_reader;
        self.mm.lock().read_block(gpu_addr, &mut buf, reader);
        Some(u32::from_le_bytes(buf))
    }

    fn is_within_gpu_address_range(&self, gpu_addr: u64) -> bool {
        self.mm.lock().is_within_gpu_address_range(gpu_addr)
    }

    fn max_continuous_range(&self, gpu_addr: u64, size: u64) -> u64 {
        self.mm.lock().max_continuous_range(gpu_addr, size)
    }

    fn get_memory_layout_size(&self, gpu_addr: u64) -> u64 {
        self.mm.lock().get_memory_layout_size(gpu_addr)
    }
}

/// Adapter that implements `DeviceMemoryAccess` for the buffer cache by
/// reading/writing through the CPU memory reader/writer callbacks.
struct DeviceMemoryAccessAdapter {
    device_reader: crate::renderer_base::DeviceMemoryReader,
}

impl crate::buffer_cache::buffer_cache_base::DeviceMemoryAccess for DeviceMemoryAccessAdapter {
    fn get_pointer(&self, _device_addr: u64) -> Option<*const u8> {
        None
    }

    fn read_block_unsafe(&self, device_addr: u64, dst: &mut [u8]) {
        (self.device_reader)(device_addr, dst);
    }

    fn write_block_unsafe(&self, device_addr: u64, src: &[u8]) {
        // DeviceMemoryAccess only exposes a reader shape today. Upstream
        // writes through `device_memory.WriteBlockUnsafe`; the Rust writer is
        // still wired separately through TextureCache / RendererBase.
        let _ = (device_addr, src);
    }
}

/// Adapter that implements `EngineState` for the buffer cache by reading
/// from a captured `DrawState` snapshot. Installed on the buffer cache at
/// the start of each `RasterizerOpenGL::draw` call so the cache's
/// `update_index_buffer` / `update_vertex_buffers` see the current draw's
/// register state.
///
/// Dirty flags always report `true` (forcing the cache to re-read every
/// draw) because the Rust port doesn't yet track per-field dirty bits on
/// the Maxwell3D register file the way upstream does.
struct DrawStateEngineAdapter {
    draw_state: DrawState,
}

impl crate::buffer_cache::buffer_cache_base::EngineState for DrawStateEngineAdapter {
    fn get_index_buffer(&self) -> crate::buffer_cache::buffer_cache_base::IndexBufferRef {
        let ds = &self.draw_state;
        let format_size = match ds.index_buffer.format {
            crate::engines::maxwell_3d::IndexFormat::UnsignedByte => 1u32,
            crate::engines::maxwell_3d::IndexFormat::UnsignedShort => 2,
            crate::engines::maxwell_3d::IndexFormat::UnsignedInt => 4,
        };
        crate::buffer_cache::buffer_cache_base::IndexBufferRef {
            start_address: ds.index_buffer_gpu_addr,
            end_address: ds.index_buffer_gpu_addr_end,
            count: ds.index_buffer.count,
            first: ds.index_buffer.first,
            format_size_in_bytes: format_size,
        }
    }

    fn get_inline_index_draw_indexes(&self) -> &[u8] {
        &self.draw_state.inline_index_draw_indexes
    }

    fn is_dirty(&self, _flag: crate::buffer_cache::buffer_cache_base::DirtyFlag) -> bool {
        true
    }

    fn clear_dirty(&mut self, _flag: crate::buffer_cache::buffer_cache_base::DirtyFlag) {}

    fn get_vertex_stream(
        &self,
        index: u32,
    ) -> crate::buffer_cache::buffer_cache_base::VertexStreamInfo {
        let Some(stream) = self.draw_state.vertex_streams.get(index as usize) else {
            return crate::buffer_cache::buffer_cache_base::VertexStreamInfo::default();
        };
        crate::buffer_cache::buffer_cache_base::VertexStreamInfo {
            address: stream.address,
            stride: stream.stride,
            enable: u32::from(stream.enabled),
        }
    }

    fn get_vertex_stream_limit(
        &self,
        index: u32,
    ) -> crate::buffer_cache::buffer_cache_base::VertexStreamLimit {
        let Some(limit) = self.draw_state.vertex_stream_limits.get(index as usize) else {
            return crate::buffer_cache::buffer_cache_base::VertexStreamLimit::default();
        };
        crate::buffer_cache::buffer_cache_base::VertexStreamLimit {
            address: limit.address,
        }
    }

    fn is_transform_feedback_enabled(&self) -> bool {
        false
    }

    fn get_transform_feedback_buffer(
        &self,
        _index: u32,
    ) -> crate::buffer_cache::buffer_cache_base::TransformFeedbackBufferInfo {
        crate::buffer_cache::buffer_cache_base::TransformFeedbackBufferInfo::default()
    }

    fn get_const_buffer(
        &self,
        _stage: usize,
        _cbuf_index: u32,
    ) -> crate::buffer_cache::buffer_cache_base::ConstBufferInfo {
        crate::buffer_cache::buffer_cache_base::ConstBufferInfo::default()
    }

    fn get_compute_launch_info(&self) -> crate::buffer_cache::buffer_cache_base::ComputeLaunchInfo {
        crate::buffer_cache::buffer_cache_base::ComputeLaunchInfo {
            const_buffer_enable_mask: 0,
            const_buffer_config: Vec::new(),
        }
    }
}

impl DeviceTracker for OpenGLDeviceTracker {
    fn update_pages_cached_count(&self, _addr: u64, _size: u64, _delta: i32) {}
}

/// Map a Maxwell `PrimitiveTopology` to the equivalent GL primitive enum.
///
/// Corresponds to the anonymous `MaxwellToGL::PrimitiveTopology` table in
/// upstream `src/video_core/renderer_opengl/maxwell_to_gl.h`. We inline the
/// minimal set needed by `RasterizerOpenGL::draw` to avoid pulling in a
/// full MaxwellToGL translation module until the draw path actually
/// executes GL calls.
/// Map a Maxwell `IndexFormat` to the matching `glDrawElements*` element type.
fn index_format_to_gl(format: crate::engines::draw_manager::IndexFormat) -> u32 {
    use crate::engines::draw_manager::IndexFormat::*;
    match format {
        UnsignedByte => gl::UNSIGNED_BYTE,
        UnsignedShort => gl::UNSIGNED_SHORT,
        UnsignedInt => gl::UNSIGNED_INT,
    }
}

/// Stride in bytes for one index of the given format. Used to compute the
/// IBO byte offset from `IndexBuffer::first` (which counts indices, not bytes).
fn index_format_stride(format: crate::engines::draw_manager::IndexFormat) -> usize {
    use crate::engines::draw_manager::IndexFormat::*;
    match format {
        UnsignedByte => 1,
        UnsignedShort => 2,
        UnsignedInt => 4,
    }
}

fn primitive_topology_to_gl(topology: crate::engines::draw_manager::PrimitiveTopology) -> u32 {
    use crate::engines::draw_manager::PrimitiveTopology::*;
    match topology {
        Points => gl::POINTS,
        Lines => gl::LINES,
        LineLoop => gl::LINE_LOOP,
        LineStrip => gl::LINE_STRIP,
        Triangles => gl::TRIANGLES,
        TriangleStrip => gl::TRIANGLE_STRIP,
        TriangleFan => gl::TRIANGLE_FAN,
        LinesAdjacency => gl::LINES_ADJACENCY,
        LineStripAdjacency => gl::LINE_STRIP_ADJACENCY,
        TrianglesAdjacency => gl::TRIANGLES_ADJACENCY,
        TriangleStripAdjacency => gl::TRIANGLE_STRIP_ADJACENCY,
        Patches => gl::PATCHES,
        _ => gl::TRIANGLES,
    }
}

fn vertex_attrib_type_raw(attrib_type: crate::engines::maxwell_3d::VertexAttribType) -> u32 {
    use crate::engines::maxwell_3d::VertexAttribType::*;
    match attrib_type {
        Invalid => 0,
        SNorm => 1,
        UNorm => 2,
        SInt => 3,
        UInt => 4,
        UScaled => 5,
        SScaled => 6,
        Float => 7,
    }
}

fn vertex_attrib_size_raw(size: crate::engines::maxwell_3d::VertexAttribSize) -> u32 {
    use crate::engines::maxwell_3d::VertexAttribSize::*;
    match size {
        Invalid => 0x00,
        R32G32B32A32 => 0x01,
        R32G32B32 => 0x02,
        R16G16B16A16 => 0x03,
        R32G32 => 0x04,
        R16G16B16 => 0x05,
        R8G8B8A8 => 0x0A,
        R16G16 => 0x0F,
        R32 => 0x12,
        R8G8B8 => 0x13,
        R8G8 => 0x18,
        R16 => 0x1B,
        R8 => 0x1D,
        A2B10G10R10 => 0x30,
        B10G11R11 => 0x31,
        G8R8 => 0x32,
        X8B8G8R8 => 0x33,
        A8 => 0x34,
    }
}

fn vertex_attrib_is_normalized(attrib_type: crate::engines::maxwell_3d::VertexAttribType) -> bool {
    matches!(
        attrib_type,
        crate::engines::maxwell_3d::VertexAttribType::SNorm
            | crate::engines::maxwell_3d::VertexAttribType::UNorm
    )
}

fn vertex_attrib_is_integer(attrib_type: crate::engines::maxwell_3d::VertexAttribType) -> bool {
    matches!(
        attrib_type,
        crate::engines::maxwell_3d::VertexAttribType::SInt
            | crate::engines::maxwell_3d::VertexAttribType::UInt
    )
}

fn comparison_op_to_gl(op: ComparisonOp) -> u32 {
    match op {
        ComparisonOp::Never => gl::NEVER,
        ComparisonOp::Less => gl::LESS,
        ComparisonOp::Equal => gl::EQUAL,
        ComparisonOp::LessEqual => gl::LEQUAL,
        ComparisonOp::Greater => gl::GREATER,
        ComparisonOp::NotEqual => gl::NOTEQUAL,
        ComparisonOp::GreaterEqual => gl::GEQUAL,
        ComparisonOp::Always => gl::ALWAYS,
    }
}

fn stencil_op_to_gl(op: StencilOp) -> u32 {
    match op {
        StencilOp::Keep => gl::KEEP,
        StencilOp::Zero => gl::ZERO,
        StencilOp::Replace => gl::REPLACE,
        StencilOp::IncrSat => gl::INCR,
        StencilOp::DecrSat => gl::DECR,
        StencilOp::Invert => gl::INVERT,
        StencilOp::Incr => gl::INCR_WRAP,
        StencilOp::Decr => gl::DECR_WRAP,
    }
}

fn blend_equation_to_gl(equation: BlendEquation) -> u32 {
    match equation {
        BlendEquation::Add => gl::FUNC_ADD,
        BlendEquation::Subtract => gl::FUNC_SUBTRACT,
        BlendEquation::ReverseSubtract => gl::FUNC_REVERSE_SUBTRACT,
        BlendEquation::Min => gl::MIN,
        BlendEquation::Max => gl::MAX,
    }
}

fn blend_factor_to_gl(factor: BlendFactor) -> u32 {
    match factor {
        BlendFactor::Zero => gl::ZERO,
        BlendFactor::One => gl::ONE,
        BlendFactor::SrcColor => gl::SRC_COLOR,
        BlendFactor::OneMinusSrcColor => gl::ONE_MINUS_SRC_COLOR,
        BlendFactor::SrcAlpha => gl::SRC_ALPHA,
        BlendFactor::OneMinusSrcAlpha => gl::ONE_MINUS_SRC_ALPHA,
        BlendFactor::DstAlpha => gl::DST_ALPHA,
        BlendFactor::OneMinusDstAlpha => gl::ONE_MINUS_DST_ALPHA,
        BlendFactor::DstColor => gl::DST_COLOR,
        BlendFactor::OneMinusDstColor => gl::ONE_MINUS_DST_COLOR,
        BlendFactor::SrcAlphaSaturate => gl::SRC_ALPHA_SATURATE,
        BlendFactor::Src1Color => gl::SRC1_COLOR,
        BlendFactor::OneMinusSrc1Color => gl::ONE_MINUS_SRC1_COLOR,
        BlendFactor::Src1Alpha => gl::SRC1_ALPHA,
        BlendFactor::OneMinusSrc1Alpha => gl::ONE_MINUS_SRC1_ALPHA,
        BlendFactor::ConstantColor => gl::CONSTANT_COLOR,
        BlendFactor::OneMinusConstantColor => gl::ONE_MINUS_CONSTANT_COLOR,
        BlendFactor::ConstantAlpha => gl::CONSTANT_ALPHA,
        BlendFactor::OneMinusConstantAlpha => gl::ONE_MINUS_CONSTANT_ALPHA,
    }
}

fn front_face_to_gl(face: FrontFace) -> u32 {
    match face {
        FrontFace::CW => gl::CW,
        FrontFace::CCW => gl::CCW,
    }
}

fn cull_face_to_gl(face: CullFace) -> u32 {
    match face {
        CullFace::Front => gl::FRONT,
        CullFace::Back => gl::BACK,
        CullFace::FrontAndBack => gl::FRONT_AND_BACK,
    }
}

fn polygon_mode_to_gl(mode: PolygonMode) -> u32 {
    match mode {
        PolygonMode::Point => gl::POINT,
        PolygonMode::Line => gl::LINE,
        PolygonMode::Fill => gl::FILL,
    }
}

fn sync_stencil_face(face: u32, info: StencilFaceInfo) {
    unsafe {
        gl::StencilFuncSeparate(
            face,
            comparison_op_to_gl(info.func),
            info.ref_value as i32,
            info.func_mask,
        );
        gl::StencilOpSeparate(
            face,
            stencil_op_to_gl(info.fail_op),
            stencil_op_to_gl(info.zfail_op),
            stencil_op_to_gl(info.zpass_op),
        );
        gl::StencilMaskSeparate(face, info.write_mask);
    }
}

fn sync_depth_stencil_state(depth_stencil: &DepthStencilInfo) {
    unsafe {
        gl::DepthMask(if depth_stencil.depth_write_enable {
            gl::TRUE
        } else {
            gl::FALSE
        });
        if depth_stencil.depth_test_enable {
            gl::Enable(gl::DEPTH_TEST);
            gl::DepthFunc(comparison_op_to_gl(depth_stencil.depth_func));
        } else {
            gl::Disable(gl::DEPTH_TEST);
        }

        if depth_stencil.stencil_enable {
            gl::Enable(gl::STENCIL_TEST);
            sync_stencil_face(gl::FRONT, depth_stencil.front);
            if depth_stencil.stencil_two_side {
                sync_stencil_face(gl::BACK, depth_stencil.back);
            } else {
                gl::StencilFuncSeparate(gl::BACK, gl::ALWAYS, 0, 0xFFFF_FFFF);
                gl::StencilOpSeparate(gl::BACK, gl::KEEP, gl::KEEP, gl::KEEP);
                gl::StencilMaskSeparate(gl::BACK, 0xFFFF_FFFF);
            }
        } else {
            gl::Disable(gl::STENCIL_TEST);
        }
    }
}

fn sync_rasterizer_state(rasterizer: &RasterizerInfo) {
    unsafe {
        if rasterizer.cull_enable {
            gl::Enable(gl::CULL_FACE);
            gl::CullFace(cull_face_to_gl(rasterizer.cull_face));
        } else {
            gl::Disable(gl::CULL_FACE);
        }
        gl::FrontFace(front_face_to_gl(rasterizer.front_face));
        gl::PolygonMode(gl::FRONT, polygon_mode_to_gl(rasterizer.polygon_mode_front));
        gl::PolygonMode(gl::BACK, polygon_mode_to_gl(rasterizer.polygon_mode_back));
        gl::LineWidth(rasterizer.line_width_aliased.max(1.0));
        // The generated GL bindings do not expose ARB_polygon_offset_clamp;
        // keep the upstream-owned state sync here and use core
        // glPolygonOffset until the extension wrapper is ported.
        gl::PolygonOffset(rasterizer.slope_scale_depth_bias, rasterizer.depth_bias);
    }
}

fn sync_fixed_function_state(draw_state: &DrawState) {
    unsafe {
        gl::Disable(gl::RASTERIZER_DISCARD);
        for (index, viewport) in draw_state.viewports.iter().enumerate() {
            gl::ViewportIndexedf(
                index as u32,
                viewport.x,
                viewport.y,
                viewport.width.max(1.0),
                viewport.height.max(1.0),
            );
            gl::DepthRangeIndexed(
                index as u32,
                viewport.depth_near as f64,
                viewport.depth_far as f64,
            );
        }

        for (index, scissor) in draw_state.scissors.iter().enumerate() {
            if scissor.enabled && scissor.max_x > scissor.min_x && scissor.max_y > scissor.min_y {
                gl::Enablei(gl::SCISSOR_TEST, index as u32);
                gl::ScissorIndexed(
                    index as u32,
                    scissor.min_x as i32,
                    scissor.min_y as i32,
                    (scissor.max_x - scissor.min_x) as i32,
                    (scissor.max_y - scissor.min_y) as i32,
                );
            } else {
                gl::Disablei(gl::SCISSOR_TEST, index as u32);
            }
        }

        for (rt, mask) in draw_state.color_masks.iter().enumerate() {
            gl::ColorMaski(
                rt as u32,
                if mask.r { gl::TRUE } else { gl::FALSE },
                if mask.g { gl::TRUE } else { gl::FALSE },
                if mask.b { gl::TRUE } else { gl::FALSE },
                if mask.a { gl::TRUE } else { gl::FALSE },
            );
        }

        gl::BlendColor(
            draw_state.blend_color.r,
            draw_state.blend_color.g,
            draw_state.blend_color.b,
            draw_state.blend_color.a,
        );
        for (rt, blend) in draw_state.blend.iter().enumerate() {
            if blend.enabled {
                gl::Enablei(gl::BLEND, rt as u32);
                gl::BlendEquationSeparatei(
                    rt as u32,
                    blend_equation_to_gl(blend.color_op),
                    blend_equation_to_gl(blend.alpha_op),
                );
                gl::BlendFuncSeparatei(
                    rt as u32,
                    blend_factor_to_gl(blend.color_src),
                    blend_factor_to_gl(blend.color_dst),
                    blend_factor_to_gl(blend.alpha_src),
                    blend_factor_to_gl(blend.alpha_dst),
                );
            } else {
                gl::Disablei(gl::BLEND, rt as u32);
            }
        }
    }
    sync_depth_stencil_state(&draw_state.depth_stencil);
    sync_rasterizer_state(&draw_state.rasterizer);
}

fn sync_vertex_formats(vao: u32, draw_state: &DrawState) {
    if vao == 0 {
        return;
    }
    let trace_attribs = std::env::var_os("RUZU_TRACE_VERTEX_ATTRIBS").is_some();
    // Upstream caps this at 16 to avoid OpenGL errors even though Maxwell
    // exposes 32 vertex attributes.
    for (index, attrib) in draw_state
        .vertex_attribs_snapshot
        .iter()
        .take(16)
        .enumerate()
    {
        let gl_index = index as u32;
        unsafe {
            if attrib.constant
                || attrib.size == crate::engines::maxwell_3d::VertexAttribSize::Invalid
            {
                gl::DisableVertexArrayAttrib(vao, gl_index);
                if trace_attribs {
                    info!(
                        "[VERTEX_ATTRIB] index={} disabled constant={} size={:?} type={:?} buffer={} offset=0x{:X}",
                        index,
                        attrib.constant,
                        attrib.size,
                        attrib.attrib_type,
                        attrib.buffer_index,
                        attrib.offset
                    );
                }
                continue;
            }

            gl::EnableVertexArrayAttrib(vao, gl_index);
            let component_count = attrib.size.component_count() as i32;
            let gl_format = crate::renderer_opengl::maxwell_to_gl::vertex_format(
                vertex_attrib_type_raw(attrib.attrib_type),
                vertex_attrib_size_raw(attrib.size),
            );
            if vertex_attrib_is_integer(attrib.attrib_type) {
                gl::VertexArrayAttribIFormat(
                    vao,
                    gl_index,
                    component_count,
                    gl_format,
                    attrib.offset,
                );
            } else {
                gl::VertexArrayAttribFormat(
                    vao,
                    gl_index,
                    component_count,
                    gl_format,
                    if vertex_attrib_is_normalized(attrib.attrib_type) {
                        gl::TRUE
                    } else {
                        gl::FALSE
                    },
                    attrib.offset,
                );
            }
            gl::VertexArrayAttribBinding(vao, gl_index, attrib.buffer_index);
            gl::VertexArrayBindingDivisor(vao, gl_index, 0);
            if trace_attribs {
                info!(
                    "[VERTEX_ATTRIB] index={} enabled size={:?} type={:?} buffer={} offset=0x{:X} components={} gl_format=0x{:X}",
                    index,
                    attrib.size,
                    attrib.attrib_type,
                    attrib.buffer_index,
                    attrib.offset,
                    component_count,
                    gl_format
                );
            }
        }
    }
}

/// OpenGL rasterizer matching zuyu's `RasterizerOpenGL`.
///
/// Processes draw calls from the Maxwell 3D engine using OpenGL.
pub struct RasterizerOpenGL {
    syncpoints: Arc<SyncpointManager>,
    fence_backend: FenceManagerOpenGL,
    fence_manager: FenceManager<Fence>,
    frame_count: u64,
    num_queued_commands: u32,
    has_written_global_memory: bool,
    buffer_cache: CommonBufferCache<OpenGLBufferCacheParams, OpenGLDeviceTracker>,
    texture_cache: OpenGLTextureCache,
    /// Generic region-tracking shader cache (region invalidation, guest address bookkeeping).
    /// Upstream inherits `GL::ShaderCache` from `VideoCommon::ShaderCache`; we keep them
    /// as two separate composed fields so each can evolve independently.
    shader_cache: ShaderCache,
    /// OpenGL-specific shader cache — owns compiled `GraphicsPipeline` / `ComputePipeline`
    /// objects and is the entry point for the draw hot path.
    gl_shader_cache: OpenGLShaderCache,
    query_cache: QueryCache,
    invalidate_gpu_cache_callback: Option<Arc<dyn Fn() + Send + Sync>>,
    /// Per-channel GPU memory manager, extracted from `ChannelState` in
    /// `bind_channel`. Used to build the `GpuMemoryAccess` adapter for the
    /// buffer cache.
    channel_memory_manager: Option<Arc<parking_lot::Mutex<crate::memory_manager::MemoryManager>>>,
    /// CPU/device memory reader callback, set via the shader cache's
    /// `set_gpu_memory_reader` path. Re-used for the buffer cache's
    /// `DeviceMemoryAccess` and for `GpuMemoryAccess::read_u32/u64`.
    cpu_memory_reader: Option<crate::shader_environment::GpuMemoryReader>,
    /// Raw guest/device memory reader installed through RendererBase.
    /// This is distinct from `cpu_memory_reader`: the latter accepts GPU VAs
    /// for shader fetches, while buffer-cache `DeviceMemoryAccess` receives
    /// already-resolved device/CPU addresses.
    device_memory_reader: Option<crate::renderer_base::DeviceMemoryReader>,
    /// GPU tick getter used for timestamped query writes.
    gpu_ticks_getter: Option<Arc<dyn Fn() -> u64 + Send + Sync>>,
    /// Transient Vertex Array Object that we bind before every `glDraw*` call
    /// to satisfy the GL core-profile requirement that "a VAO must be bound
    /// for any draw command". Attribute formats and vertex-buffer bindings are
    /// configured from the current draw-state snapshot before each draw. The
    /// VAO handle is `0` in
    /// `new_for_test` (no GL context), and any draw inside a unit test
    /// path would skip the GL call because the placeholder pipeline has
    /// no GL programs attached.
    transient_vao: u32,
}

// The OpenGL rasterizer is owned and used from the renderer thread. The newly restored
// cache owners still contain backend trait-object slots that are not marked `Send`, but
// this slice does not populate them yet. Matching the existing renderer ownership model,
// we keep the type movable to the renderer thread.
unsafe impl Send for RasterizerOpenGL {}

impl Drop for RasterizerOpenGL {
    fn drop(&mut self) {
        if self.transient_vao != 0 {
            unsafe { gl::DeleteVertexArrays(1, &self.transient_vao) };
            self.transient_vao = 0;
        }
    }
}

impl RasterizerOpenGL {
    /// Create a new rasterizer. Must be called with a current GL context.
    ///
    /// `device_memory` is the single shared `MaxwellDeviceMemoryManager`
    /// from `Host1x::memory_manager()`. Upstream:
    /// `RasterizerOpenGL::RasterizerOpenGL(emu_window, gpu, device_memory, ...)`.
    pub fn new(
        device: &Device,
        syncpoints: Arc<SyncpointManager>,
        device_memory: Arc<crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager>,
    ) -> Self {
        let mut transient_vao: u32 = 0;
        unsafe {
            gl::GenVertexArrays(1, &mut transient_vao);
            gl::BindVertexArray(transient_vao);
        }
        let mut buffer_cache = CommonBufferCache::new(&OPENGL_DEVICE_TRACKER);
        // Install the OpenGL buffer-cache runtime so `bind_host_*` methods
        // can issue GL calls once channel_state is populated.
        let gl_runtime = super::gl_buffer_cache::BufferCacheRuntime::new(device);
        buffer_cache.set_runtime(Box::new(gl_runtime));
        Self {
            syncpoints,
            fence_backend: FenceManagerOpenGL::new(),
            fence_manager: FenceManager::new(false),
            frame_count: 0,
            num_queued_commands: 0,
            has_written_global_memory: false,
            buffer_cache,
            texture_cache: OpenGLTextureCache::new(device_memory.clone()),
            shader_cache: ShaderCache::new(device_memory),
            gl_shader_cache: OpenGLShaderCache::new(),
            query_cache: QueryCache::new(),
            invalidate_gpu_cache_callback: None,
            channel_memory_manager: None,
            cpu_memory_reader: None,
            device_memory_reader: None,
            gpu_ticks_getter: None,
            transient_vao,
        }
    }

    #[cfg(test)]
    fn new_for_test(syncpoints: Arc<SyncpointManager>) -> Self {
        let test_device_memory = Arc::new(
            crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager::default(),
        );
        Self {
            syncpoints,
            fence_backend: FenceManagerOpenGL::new(),
            fence_manager: FenceManager::new(false),
            frame_count: 0,
            num_queued_commands: 0,
            has_written_global_memory: false,
            buffer_cache: CommonBufferCache::new(&OPENGL_DEVICE_TRACKER),
            texture_cache: OpenGLTextureCache::new(test_device_memory),
            shader_cache: ShaderCache::default(),
            gl_shader_cache: OpenGLShaderCache::new(),
            query_cache: QueryCache::new(),
            invalidate_gpu_cache_callback: None,
            channel_memory_manager: None,
            cpu_memory_reader: None,
            device_memory_reader: None,
            gpu_ticks_getter: None,
            transient_vao: 0,
        }
    }

    /// Rust adaptation for upstream `RasterizerOpenGL::InvalidateGPUCache()`,
    /// which delegates to the owning `GPU`.
    pub fn set_invalidate_gpu_cache_callback(&mut self, callback: Arc<dyn Fn() + Send + Sync>) {
        self.invalidate_gpu_cache_callback = Some(callback);
    }

    /// Install the GPU memory reader used by the OpenGL shader cache.
    ///
    /// Forwards to [`gl_shader_cache::ShaderCache::set_gpu_memory_reader`].
    /// The reader takes a *GPU virtual address* (already translated through
    /// `MemoryManager`) and writes `buf.len()` bytes into the destination
    /// slice. With the reader installed, `current_graphics_pipeline` will
    /// invoke `shader_recompiler::compile_shader_glsl` for each enabled
    /// stage on first lookup, instead of returning a placeholder pipeline.
    pub fn set_gpu_memory_reader(&mut self, reader: crate::shader_environment::GpuMemoryReader) {
        self.cpu_memory_reader = Some(Arc::clone(&reader));
        self.gl_shader_cache.set_gpu_memory_reader(reader);
        if let (Some(mm), Some(cpu_reader)) = (
            self.channel_memory_manager.as_ref(),
            self.cpu_memory_reader.as_ref(),
        ) {
            self.buffer_cache
                .set_gpu_memory(Box::new(GpuMemoryAccessAdapter {
                    mm: Arc::clone(mm),
                    cpu_reader: Arc::clone(cpu_reader),
                }));
        }
    }

    pub fn set_device_memory_reader(&mut self, reader: crate::renderer_base::DeviceMemoryReader) {
        self.device_memory_reader = Some(Arc::clone(&reader));
        self.buffer_cache
            .set_device_memory(Box::new(DeviceMemoryAccessAdapter {
                device_reader: reader,
            }));
    }

    pub fn set_gpu_ticks_getter(&mut self, getter: Arc<dyn Fn() -> u64 + Send + Sync>) {
        self.query_cache.set_gpu_ticks_getter(Arc::clone(&getter));
        self.gpu_ticks_getter = Some(getter);
    }

    pub fn set_guest_memory_writer(&mut self, writer: GuestMemoryWriter) {
        self.texture_cache.set_guest_memory_writer(writer);
    }

    /// Port of `RasterizerOpenGL::AccelerateDisplay`.
    pub fn accelerate_display(
        &mut self,
        config: &FramebufferConfig,
        framebuffer_addr: u64,
        _pixel_stride: u32,
    ) -> Option<FramebufferTextureInfo> {
        if framebuffer_addr == 0 {
            return None;
        }

        let texture_cache: *mut OpenGLTextureCache = &mut self.texture_cache;
        let framebuffer_view = unsafe {
            let _texture_lock = (*texture_cache).base.mutex.lock();
            (*texture_cache).try_find_framebuffer_image_view(config, framebuffer_addr)
        };

        let Some(framebuffer_view) = framebuffer_view else {
            if std::env::var_os("RUZU_TRACE_PRESENT").is_some() {
                log::info!(
                    "[PRESENT] AccelerateDisplay miss no_texture_cache_view addr=0x{:X} {}x{} stride={}",
                    framebuffer_addr,
                    config.width,
                    config.height,
                    config.stride
                );
            }
            return None;
        };

        if std::env::var_os("RUZU_TRACE_PRESENT").is_some() {
            log::info!(
                "[PRESENT] AccelerateDisplay hit view_id={} texture={} addr=0x{:X} size={}x{} scaled={}",
                framebuffer_view.view_id.index,
                framebuffer_view.display_texture,
                framebuffer_addr,
                framebuffer_view.width,
                framebuffer_view.height,
                framebuffer_view.scaled
            );
        }
        if std::env::var_os("RUZU_DUMP_PRESENT_TEXTURE").is_some() {
            use std::sync::atomic::{AtomicU64, Ordering};
            static DUMP_COUNT: AtomicU64 = AtomicU64::new(0);
            let dump_index = DUMP_COUNT.fetch_add(1, Ordering::Relaxed);
            if dump_index < 8 || dump_index.is_power_of_two() {
                let byte_count =
                    (framebuffer_view.width as usize) * (framebuffer_view.height as usize) * 4;
                let sample_count = if std::env::var_os("RUZU_DUMP_PRESENT_TEXTURE_FULL").is_some() {
                    byte_count
                } else {
                    byte_count.min(64 * 1024)
                };
                let mut pixels = vec![0u8; byte_count];
                if byte_count != 0 {
                    unsafe {
                        gl::GetTextureImage(
                            framebuffer_view.display_texture,
                            0,
                            gl::RGBA,
                            gl::UNSIGNED_BYTE,
                            pixels.len() as i32,
                            pixels.as_mut_ptr() as *mut _,
                        );
                    }
                }
                let sample = &pixels[..sample_count];
                let nonzero = sample.iter().filter(|&&byte| byte != 0).count();
                let min = sample.iter().copied().min().unwrap_or(0);
                let max = sample.iter().copied().max().unwrap_or(0);
                let first16 = sample
                    .iter()
                    .take(16)
                    .map(|byte| format!("{:02X}", byte))
                    .collect::<Vec<_>>()
                    .join(" ");
                let checksum = sample
                    .iter()
                    .fold(0u64, |acc, &byte| acc.wrapping_mul(16777619) ^ byte as u64);
                log::info!(
                    "[PRESENT_TEXTURE] #{} texture={} level_bytes={} sample={} nonzero={} min={} max={} checksum=0x{:X} first16=[{}]",
                    dump_index,
                    framebuffer_view.display_texture,
                    byte_count,
                    sample_count,
                    nonzero,
                    min,
                    max,
                    checksum,
                    first16
                );
            }
        }
        Some(FramebufferTextureInfo {
            display_texture: framebuffer_view.display_texture,
            width: framebuffer_view.width,
            height: framebuffer_view.height,
            scaled_width: framebuffer_view.width,
            scaled_height: framebuffer_view.height,
        })
    }

    /// Process draw calls and produce a framebuffer.
    ///
    /// Currently delegates to the software rasterizer. As more GL pipeline
    /// infrastructure is ported from zuyu (buffer cache, texture cache,
    /// shader cache, etc.), this will transition to GPU-accelerated rendering.
    pub fn render_draw_calls(
        &mut self,
        draw_calls: &[DrawCall],
        gpu_read: &dyn Fn(u64, &mut [u8]),
        framebuffer: Option<Framebuffer>,
    ) -> Option<Framebuffer> {
        if draw_calls.is_empty() {
            return framebuffer;
        }

        debug!(
            "RasterizerOpenGL: processing {} draw calls (frame {})",
            draw_calls.len(),
            self.frame_count
        );

        let write_backs: std::sync::Mutex<Vec<(u64, Vec<u8>)>> = std::sync::Mutex::new(Vec::new());
        let gpu_write = |gpu_va: u64, data: &[u8]| {
            write_backs.lock().unwrap().push((gpu_va, data.to_vec()));
        };

        SoftwareRasterizer::render_draw_calls(draw_calls, gpu_read, &gpu_write, framebuffer)
    }

    fn should_wait_async_flushes(&mut self) -> bool {
        let buffer_cache: *mut CommonBufferCache<OpenGLBufferCacheParams, OpenGLDeviceTracker> =
            &mut self.buffer_cache;
        let texture_cache: *mut OpenGLTextureCache = &mut self.texture_cache;
        unsafe {
            let _buffer_lock = (*buffer_cache).mutex.lock();
            let _texture_lock = (*texture_cache).base.mutex.lock();
            (*texture_cache).should_wait_async_flushes()
                || (*buffer_cache).should_wait_async_flushes()
                || self.query_cache.should_wait_async_flushes()
        }
    }

    fn should_flush_async(&mut self) -> bool {
        let buffer_cache: *mut CommonBufferCache<OpenGLBufferCacheParams, OpenGLDeviceTracker> =
            &mut self.buffer_cache;
        let texture_cache: *mut OpenGLTextureCache = &mut self.texture_cache;
        unsafe {
            let _buffer_lock = (*buffer_cache).mutex.lock();
            let _texture_lock = (*texture_cache).base.mutex.lock();
            (*texture_cache).has_uncommitted_flushes()
                || (*buffer_cache).has_uncommitted_flushes()
                || self.query_cache.has_uncommitted_flushes()
        }
    }

    fn pop_async_flushes(&mut self) {
        let buffer_cache: *mut CommonBufferCache<OpenGLBufferCacheParams, OpenGLDeviceTracker> =
            &mut self.buffer_cache;
        let texture_cache: *mut OpenGLTextureCache = &mut self.texture_cache;
        unsafe {
            let _buffer_lock = (*buffer_cache).mutex.lock();
            let _texture_lock = (*texture_cache).base.mutex.lock();
            (*texture_cache).pop_async_flushes();
            (*buffer_cache).pop_async_flushes();
        }
        self.query_cache.pop_async_flushes();
    }

    fn commit_async_flushes(&mut self) {
        let buffer_cache: *mut CommonBufferCache<OpenGLBufferCacheParams, OpenGLDeviceTracker> =
            &mut self.buffer_cache;
        let texture_cache: *mut OpenGLTextureCache = &mut self.texture_cache;
        unsafe {
            let _buffer_lock = (*buffer_cache).mutex.lock();
            let _texture_lock = (*texture_cache).base.mutex.lock();
            (*texture_cache).commit_async_flushes();
            (*buffer_cache).commit_async_flushes();
        }
        self.query_cache.commit_async_flushes();
    }

    fn accumulate_buffer_flushes(&mut self) {
        let buffer_cache: *mut CommonBufferCache<OpenGLBufferCacheParams, OpenGLDeviceTracker> =
            &mut self.buffer_cache;
        unsafe {
            let _buffer_lock = (*buffer_cache).mutex.lock();
            (*buffer_cache).accumulate_flushes();
        }
    }
}

impl RasterizerInterface for RasterizerOpenGL {
    /// Port of `RasterizerOpenGL::Draw(bool is_indexed, u32 instance_count)`
    /// (cpp:267) + `RasterizerOpenGL::PrepareDraw` (cpp:230).
    ///
    /// Upstream call chain (abridged):
    ///   `RasterizerOpenGL::Draw`
    ///   → `PrepareDraw(is_indexed, draw_func)`
    ///   → `ShaderCache::CurrentGraphicsPipeline()` (compiles pipeline on miss)
    ///   → `GraphicsPipeline::Configure(is_indexed)` (bind program, UBOs, textures, XFB)
    ///   → `SyncState()` (global GL state sync)
    ///   → read `maxwell3d->draw_manager->GetDrawState()` for topology and
    ///     vertex/index buffer bindings
    ///   → `glDrawElements{Instanced,BaseVertex,...}` or `glDrawArrays{Instanced,BaseInstance,...}`
    ///
    /// Current Rust state (steps 1–3 in the scoping plan):
    /// * `gl_shader_cache::ShaderCache::current_graphics_pipeline` returns a
    ///   placeholder `GraphicsPipeline` on miss, so the pipeline lookup path
    ///   is live at the control-flow level.
    /// * `DrawState` is now threaded from `DrawManager::process_draw` into
    ///   this function as `draw_state`, matching the upstream pattern of
    ///   `maxwell3d->draw_manager->GetDrawState()` — but without requiring
    ///   the rasterizer to carry a Maxwell3D reference.
    /// * Buffer-cache / texture-cache binding (`buffer_cache.mutex`,
    ///   `texture_cache.base.mutex`, `SyncState`, `SetEngine`) and the actual
    ///   `glDraw*` family are still deferred to step 4+. They require real
    ///   buffer/texture caches and a compiled GL program.
    ///
    /// The `MaxwellToGL::PrimitiveTopology` mapping is intentionally kept
    /// here (not on the pipeline) because upstream re-reads topology on
    /// every `Draw` — a single pipeline key may be drawn with multiple
    /// topologies in successive calls.
    fn draw(&mut self, draw_state: &DrawState, instance_count: u32) {
        let trace_draw = std::env::var_os("RUZU_PROFILE_GL_DRAW").is_some();
        let draw_start = Instant::now();
        if trace_draw {
            info!(
                "[GL_DRAW_PROFILE] begin indexed={} instances={} topology={:?} ib_count={} vb_count={} shader_addrs={:X?}",
                draw_state.draw_indexed,
                instance_count,
                draw_state.topology,
                draw_state.index_buffer.count,
                draw_state.vertex_buffer.count,
                draw_state.shader_program_addresses,
            );
        }

        let mut bound_draw_framebuffer = None;
        if let Some(mm) = self.channel_memory_manager.as_ref().cloned() {
            let step = Instant::now();
            let texture_cache: *mut OpenGLTextureCache = &mut self.texture_cache;
            bound_draw_framebuffer = unsafe {
                let _texture_lock = (*texture_cache).base.mutex.lock();
                (*texture_cache).update_render_targets_from_draw_state(draw_state, |gpu_addr| {
                    mm.lock().gpu_to_cpu_address(gpu_addr)
                });
                draw_state
                    .rt_control
                    .map
                    .iter()
                    .take(draw_state.rt_control.count.min(8) as usize)
                    .filter_map(|&target| draw_state.render_targets.get(target as usize))
                    .find_map(|rt| (*texture_cache).framebuffer_for_render_target(rt))
            };
            if trace_draw {
                info!(
                    "[GL_DRAW_PROFILE] update_render_targets_us={}",
                    step.elapsed().as_micros()
                );
            }
        } else if std::env::var_os("RUZU_TRACE_RT").is_some() {
            log::info!("[RT] miss no_channel_memory_manager");
        }
        if let Some((framebuffer, width, height)) = bound_draw_framebuffer {
            unsafe {
                gl::BindFramebuffer(gl::DRAW_FRAMEBUFFER, framebuffer);
            }
            if trace_draw || std::env::var_os("RUZU_TRACE_RT").is_some() {
                info!(
                    "[GL_DRAW_PROFILE] bind_draw_framebuffer framebuffer={} {}x{}",
                    framebuffer, width, height
                );
            }
        } else if std::env::var_os("RUZU_TRACE_RT").is_some() {
            info!("[RT] draw no framebuffer bound");
        }

        let step = Instant::now();
        let Some(pipeline) = self
            .gl_shader_cache
            .current_graphics_pipeline_with_shared_cache(&mut self.shader_cache)
        else {
            // No pipeline yet — either async compilation is in flight or
            // there is nothing to draw. Upstream silently skips in this case.
            debug!("RasterizerOpenGL::draw skipped — no graphics pipeline available");
            return;
        };
        if trace_draw {
            info!(
                "[GL_DRAW_PROFILE] current_graphics_pipeline_us={}",
                step.elapsed().as_micros()
            );
        }

        // Lazy GL-program build (gap 4). The shader cache stages GLSL
        // sources but never calls into GL itself, so the very first time
        // we hit this pipeline with a real GL context we materialise the
        // separable per-stage programs here. Failures are logged once and
        // leave the pipeline in its placeholder state so we don't retry
        // every frame.
        if !pipeline.has_gl_programs() && pipeline.glsl_sources.iter().any(|s| s.is_some()) {
            let step = Instant::now();
            if let Err((stage_index, msg)) = pipeline.build_from_sources() {
                log::warn!(
                    "RasterizerOpenGL::draw: pipeline build failed at stage {}: {}",
                    stage_index,
                    msg
                );
            }
            if trace_draw {
                info!(
                    "[GL_DRAW_PROFILE] build_from_sources_us={} has_programs={}",
                    step.elapsed().as_micros(),
                    pipeline.has_gl_programs()
                );
            }
        }

        let is_indexed = draw_state.draw_indexed;
        let pipeline_has_programs = pipeline.has_gl_programs();
        let step = Instant::now();
        // Mirrors upstream `GraphicsPipeline::ConfigureImpl`
        // (gl_graphics_pipeline.cpp:278-284): the very first thing the
        // pipeline configure does is `texture_cache.SynchronizeGraphicsDescriptors`
        // off `maxwell3d->regs`. Ruzu does the same here using the snapshot
        // captured in `DrawState::descriptor_sync_regs` because the rasterizer
        // has no Maxwell3D back-reference.
        self.texture_cache
            .base
            .synchronize_graphics_descriptors(draw_state.descriptor_sync_regs);
        pipeline.configure(is_indexed);

        // Per-stage texture / image descriptor collection — port of the
        // `config_stage` lambda body in upstream `ConfigureImpl`
        // (gl_graphics_pipeline.cpp:293-376). For each stage's shader info,
        // walk `texture_buffer_descriptors`, `image_buffer_descriptors`,
        // `texture_descriptors`, `image_descriptors`; for each descriptor
        // read `count` 32-bit TIC handles from its cbuf binding via the
        // GPU memory reader; push one `ImageViewInOut` per handle.
        //
        // The cbuf address comes from `draw_state.cb_bindings[stage][cbuf_index]
        // .address + offset` (Maxwell3D const-buffer state). Handle decode
        // splits the raw u32 into (tic_id, tsc_id) using `texture_pair` —
        // when `sampler_binding == ViaHeaderBinding` the two ids collapse.
        //
        // Gated behind `RUZU_TEXTURE_CACHE_FILL=1` while the upstream-faithful
        // descriptor collection is being verified end-to-end. Hardened with
        // explicit bounds checks on every index that could come from
        // shader-info (uninitialised or out-of-range u32s would otherwise
        // panic via array indexing and crash through the SIGSEGV handler).
        if std::env::var_os("RUZU_TEXTURE_CACHE_FILL").is_some() {
        const MAX_DESC_COUNT: u32 = 256; // sanity cap (upstream rarely > 16)
        const NUM_STAGES: usize = crate::renderer_opengl::gl_graphics_pipeline::NUM_STAGES;
        let max_cb_slots = crate::engines::maxwell_3d::MAX_CB_SLOTS;
        let num_shader_stages = draw_state.cb_bindings.len();

        let via_header_index = draw_state.descriptor_sync_regs.sampler_binding_via_header;
        let mut views: Vec<crate::texture_cache::texture_cache_base::ImageViewInOut> =
            Vec::with_capacity(64);
        let mut has_images = false;
        let gpu_memory_arc = self.texture_cache.base.device_memory.clone();
        let gpu_memory = &*gpu_memory_arc;

        let read_handle =
            |gpu_memory: &dyn crate::texture_cache::descriptor_table::GpuMemoryReader,
             stage: usize,
             cbuf_index: u32,
             offset: u32|
             -> Option<u32> {
                if stage >= num_shader_stages {
                    return None;
                }
                let cbuf_idx = cbuf_index as usize;
                if cbuf_idx >= max_cb_slots {
                    return None;
                }
                let binding = &draw_state.cb_bindings[stage][cbuf_idx];
                if !binding.enabled {
                    return None;
                }
                let addr = binding.address.checked_add(offset as u64)?;
                let mut buf = [0u8; 4];
                if !gpu_memory.read_block(addr, &mut buf) {
                    return None;
                }
                Some(u32::from_le_bytes(buf))
            };

        // For each descriptor: compute clamped count, then loop. Per-entry
        // index_offset uses checked u32 arithmetic so a malformed shift_left
        // can't silently wrap into a wildly wrong cbuf offset.
        let resolve_handle = |gpu_memory: &dyn crate::texture_cache::descriptor_table::GpuMemoryReader,
                              stage: usize,
                              cbuf_index: u32,
                              cbuf_offset: u32,
                              size_shift: u32,
                              idx: u32|
         -> Option<u32> {
            // `idx << size_shift` — checked shift + checked add.
            let shift = size_shift.min(31);
            let index_offset = idx.checked_shl(shift)?;
            let offset = cbuf_offset.checked_add(index_offset)?;
            read_handle(gpu_memory, stage, cbuf_index, offset)
        };

        for stage in 0..NUM_STAGES.min(num_shader_stages) {
            let Some(info) = pipeline.stage_infos[stage].as_ref() else {
                continue;
            };
            // Texture buffer descriptors — handle only (no sampler).
            for desc in &info.texture_buffer_descriptors {
                let count = desc.count.min(MAX_DESC_COUNT);
                for idx in 0..count {
                    if let Some(raw) = resolve_handle(
                        gpu_memory,
                        stage,
                        desc.cbuf_index,
                        desc.cbuf_offset,
                        desc.size_shift,
                        idx,
                    ) {
                        let (tic_id, _) =
                            crate::textures::texture::texture_pair(raw, via_header_index);
                        views.push(crate::texture_cache::texture_cache_base::ImageViewInOut {
                            index: tic_id,
                            blacklist: false,
                            id: Default::default(),
                        });
                    }
                }
            }
            // Image buffer descriptors — blacklist=false, no sampler. Upstream
            // calls these out separately under `Spec::has_image_buffers`.
            for desc in &info.image_buffer_descriptors {
                let count = desc.count.min(MAX_DESC_COUNT);
                for idx in 0..count {
                    if let Some(raw) = resolve_handle(
                        gpu_memory,
                        stage,
                        desc.cbuf_index,
                        desc.cbuf_offset,
                        desc.size_shift,
                        idx,
                    ) {
                        let (tic_id, _) =
                            crate::textures::texture::texture_pair(raw, via_header_index);
                        views.push(crate::texture_cache::texture_cache_base::ImageViewInOut {
                            index: tic_id,
                            blacklist: false,
                            id: Default::default(),
                        });
                    }
                }
            }
            // Sampled texture descriptors. Each handle yields one image-view
            // and one sampler-id; ruzu collects the sampler IDs lazily below
            // (GetGraphicsSamplerId would be the upstream call — TODO once
            // the sampler slot pool is concrete).
            for desc in &info.texture_descriptors {
                let count = desc.count.min(MAX_DESC_COUNT);
                for idx in 0..count {
                    if let Some(raw) = resolve_handle(
                        gpu_memory,
                        stage,
                        desc.cbuf_index,
                        desc.cbuf_offset,
                        desc.size_shift,
                        idx,
                    ) {
                        let (tic_id, _tsc_id) =
                            crate::textures::texture::texture_pair(raw, via_header_index);
                        views.push(crate::texture_cache::texture_cache_base::ImageViewInOut {
                            index: tic_id,
                            blacklist: false,
                            id: Default::default(),
                        });
                    }
                }
            }
            // Image (storage image) descriptors. Upstream tags
            // blacklist = desc.is_written so written-to render targets
            // can be detected and scaled down.
            for desc in &info.image_descriptors {
                has_images = true;
                let count = desc.count.min(MAX_DESC_COUNT);
                for idx in 0..count {
                    if let Some(raw) = resolve_handle(
                        gpu_memory,
                        stage,
                        desc.cbuf_index,
                        desc.cbuf_offset,
                        desc.size_shift,
                        idx,
                    ) {
                        let (tic_id, _) =
                            crate::textures::texture::texture_pair(raw, via_header_index);
                        views.push(crate::texture_cache::texture_cache_base::ImageViewInOut {
                            index: tic_id,
                            blacklist: desc.is_written,
                            id: Default::default(),
                        });
                    }
                }
            }
        }
        // Mirror upstream `FillGraphicsImageViews<has_images>(views)` from
        // gl_graphics_pipeline.cpp:380. `has_blacklists = has_images` per
        // upstream's `Spec::has_images` template parameter.
        self.texture_cache
            .base
            .fill_graphics_image_views(&mut views, has_images);
        drop(gpu_memory_arc);
        } // RUZU_TEXTURE_CACHE_FILL gate
        self.buffer_cache
            .set_graphics_base_uniform_bindings(&pipeline.base_uniform_bindings);
        if trace_draw {
            info!(
                "[GL_DRAW_PROFILE] pipeline_configure_us={}",
                step.elapsed().as_micros()
            );
        }

        // Install the engine-state adapter so buffer cache update calls can
        // read the current draw's index/vertex buffer state from DrawState.
        self.buffer_cache
            .set_engine_state(Box::new(DrawStateEngineAdapter {
                draw_state: draw_state.clone(),
            }));

        // Partial port of upstream `GraphicsPipeline::ConfigureImpl`
        // uniform-buffer setup. Upstream derives the enabled cbuf mask from
        // shader info, not from every Maxwell cbuf currently enabled. This is
        // required because GL bindings are compacted over the shader's
        // descriptors: if a shader only declares cbuf3, that cbuf is bound at
        // binding 0.
        let uniform_masks = pipeline.enabled_uniform_buffer_masks;
        let uniform_sizes = pipeline.uniform_buffer_sizes;
        for stage in 0..uniform_masks.len().min(draw_state.cb_bindings.len()) {
            let mut bits = uniform_masks[stage];
            let mut slot = 0u32;
            while bits != 0 {
                let skip = bits.trailing_zeros();
                slot += skip;
                bits >>= skip;

                let binding = draw_state.cb_bindings[stage][slot as usize];
                if binding.enabled && binding.address != 0 && binding.size != 0 {
                    self.buffer_cache.bind_graphics_uniform_buffer(
                        stage,
                        slot,
                        binding.address,
                        binding.size,
                    );
                } else {
                    self.buffer_cache
                        .disable_graphics_uniform_buffer(stage, slot);
                }

                slot += 1;
                bits >>= 1;
            }
        }
        self.buffer_cache
            .set_uniform_buffers_state(&uniform_masks, &uniform_sizes);

        // Buffer cache: refresh and bind host vertex/index buffers.
        // Mirrors upstream `RasterizerOpenGL::PrepareDraw`.
        let step = Instant::now();
        self.buffer_cache.update_graphics_buffers(is_indexed);
        if trace_draw {
            info!(
                "[GL_DRAW_PROFILE] update_graphics_buffers_us={}",
                step.elapsed().as_micros()
            );
        }
        let step = Instant::now();
        if self.transient_vao != 0 {
            unsafe {
                gl::BindVertexArray(self.transient_vao);
            }
        }
        self.buffer_cache.bind_host_geometry_buffers(is_indexed);
        for stage in 0..crate::buffer_cache::buffer_cache_base::NUM_STAGES as usize {
            self.buffer_cache.bind_host_stage_buffers(stage);
        }
        if trace_draw {
            info!(
                "[GL_DRAW_PROFILE] bind_host_geometry_buffers_us={}",
                step.elapsed().as_micros()
            );
        }

        let primitive_mode = primitive_topology_to_gl(draw_state.topology);
        let base_instance = draw_state.base_instance;
        let num_instances = instance_count;

        // Only issue real `glDraw*` calls when the pipeline carries
        // compiled GL programs *and* we have a transient VAO (i.e. the
        // production constructor ran). This keeps the unit-test path
        // (`new_for_test`, no GL context, placeholder pipelines) safe.
        let can_draw_gl = pipeline_has_programs && self.transient_vao != 0;
        if can_draw_gl {
            sync_fixed_function_state(draw_state);
            sync_vertex_formats(self.transient_vao, draw_state);
        }

        if is_indexed {
            let base_vertex = draw_state.base_index as i32;
            let num_vertices = draw_state.index_buffer.count;
            if can_draw_gl {
                let index_format = index_format_to_gl(draw_state.index_buffer.format);
                let index_offset = self.buffer_cache.index_offset();
                unsafe {
                    gl::BindVertexArray(self.transient_vao);
                    gl::DrawElementsInstancedBaseVertexBaseInstance(
                        primitive_mode,
                        num_vertices as i32,
                        index_format,
                        index_offset as *const _,
                        num_instances as i32,
                        base_vertex,
                        base_instance,
                    );
                }
            } else {
                debug!(
                    "RasterizerOpenGL::draw indexed prim=0x{:X} verts={} instances={} \
                     base_vertex={} base_instance={} — placeholder pipeline, no GL draw",
                    primitive_mode, num_vertices, num_instances, base_vertex, base_instance
                );
            }
        } else {
            let base_vertex = draw_state.vertex_buffer.first as i32;
            let num_vertices = draw_state.vertex_buffer.count;
            if can_draw_gl {
                unsafe {
                    gl::BindVertexArray(self.transient_vao);
                    gl::DrawArraysInstancedBaseInstance(
                        primitive_mode,
                        base_vertex,
                        num_vertices as i32,
                        num_instances as i32,
                        base_instance,
                    );
                }
            } else {
                debug!(
                    "RasterizerOpenGL::draw arrays prim=0x{:X} verts={} instances={} \
                     base_vertex={} base_instance={} — placeholder pipeline, no GL draw",
                    primitive_mode, num_vertices, num_instances, base_vertex, base_instance
                );
            }
        }
        if can_draw_gl && std::env::var_os("RUZU_TRACE_GL_DRAW_ERROR").is_some() {
            unsafe {
                let mut validate_status = 0;
                let pipeline_handle = pipeline.program_pipeline_handle();
                if pipeline_handle != 0 {
                    gl::ValidateProgramPipeline(pipeline_handle);
                    gl::GetProgramPipelineiv(
                        pipeline_handle,
                        gl::VALIDATE_STATUS,
                        &mut validate_status,
                    );
                }
                let mut attrib4_enabled = 0;
                gl::GetVertexAttribiv(4, gl::VERTEX_ATTRIB_ARRAY_ENABLED, &mut attrib4_enabled);
                let mut attrib4_buffer = 0;
                gl::GetVertexAttribiv(
                    4,
                    gl::VERTEX_ATTRIB_ARRAY_BUFFER_BINDING,
                    &mut attrib4_buffer,
                );
                let mut attrib_enabled = [0; 3];
                let mut attrib_buffer = [0; 3];
                for index in 0..3 {
                    gl::GetVertexAttribiv(
                        index as u32,
                        gl::VERTEX_ATTRIB_ARRAY_ENABLED,
                        &mut attrib_enabled[index],
                    );
                    gl::GetVertexAttribiv(
                        index as u32,
                        gl::VERTEX_ATTRIB_ARRAY_BUFFER_BINDING,
                        &mut attrib_buffer[index],
                    );
                }
                let mut element_buffer = 0;
                gl::GetIntegerv(gl::ELEMENT_ARRAY_BUFFER_BINDING, &mut element_buffer);
                let mut viewport = [0; 4];
                gl::GetIntegerv(gl::VIEWPORT, viewport.as_mut_ptr());
                let mut draw_fbo = 0;
                gl::GetIntegerv(gl::DRAW_FRAMEBUFFER_BINDING, &mut draw_fbo);
                let fbo_status = gl::CheckFramebufferStatus(gl::DRAW_FRAMEBUFFER);
                let depth_test = gl::IsEnabled(gl::DEPTH_TEST);
                let cull_face = gl::IsEnabled(gl::CULL_FACE);
                let scissor_test = gl::IsEnabledi(gl::SCISSOR_TEST, 0);
                let mut color_mask = [0; 4];
                gl::GetBooleani_v(gl::COLOR_WRITEMASK, 0, color_mask.as_mut_ptr());
                let gl_error = gl::GetError();
                info!(
                    "[GL_DRAW_ERROR] error=0x{:X} pipeline={} validate={} fbo={} fbo_status=0x{:X} viewport={:?} depth={} cull={} scissor0={} color_mask={:?} attrib_enabled={:?} attrib_buffer={:?} attrib4_enabled={} attrib4_buffer={} element_buffer={}",
                    gl_error,
                    pipeline_handle,
                    validate_status,
                    draw_fbo,
                    fbo_status,
                    viewport,
                    depth_test,
                    cull_face,
                    scissor_test,
                    color_mask,
                    attrib_enabled,
                    attrib_buffer,
                    attrib4_enabled,
                    attrib4_buffer,
                    element_buffer
                );
            }
        }
        self.num_queued_commands = self.num_queued_commands.saturating_add(1);
        if trace_draw {
            info!(
                "[GL_DRAW_PROFILE] end total_us={} queued_commands={}",
                draw_start.elapsed().as_micros(),
                self.num_queued_commands
            );
        }
    }

    fn draw_texture(&mut self) {
        debug!("RasterizerOpenGL::draw_texture");
    }

    fn clear(&mut self, draw_state: &DrawState, layer_count: u32) {
        let flags = draw_state.clear_state.flags;
        let clear_z = flags & (1 << 0) != 0;
        let clear_s = flags & (1 << 1) != 0;
        let clear_r = flags & (1 << 2) != 0;
        let clear_g = flags & (1 << 3) != 0;
        let clear_b = flags & (1 << 4) != 0;
        let clear_a = flags & (1 << 5) != 0;
        let use_color = clear_r || clear_g || clear_b || clear_a;
        let use_depth = clear_z;
        let use_stencil = clear_s;

        if !use_color && !use_depth && !use_stencil {
            return;
        }

        if !use_color {
            if std::env::var_os("RUZU_TRACE_CLEAR").is_some() {
                debug!(
                    "RasterizerOpenGL::clear depth/stencil-only skipped layers={}",
                    layer_count
                );
            }
            return;
        }

        let rt_index = ((flags >> 6) & 0xF) as usize;
        if rt_index >= draw_state.render_targets.len() {
            return;
        }
        let rt = draw_state.render_targets[rt_index];
        let Some(mm) = self.channel_memory_manager.as_ref().cloned() else {
            if std::env::var_os("RUZU_TRACE_CLEAR").is_some() {
                debug!("RasterizerOpenGL::clear skipped, no channel memory manager");
            }
            return;
        };

        let texture_cache: *mut OpenGLTextureCache = &mut self.texture_cache;
        let framebuffer = unsafe {
            let _texture_lock = (*texture_cache).base.mutex.lock();
            (*texture_cache).update_render_targets_from_draw_state(draw_state, |gpu_addr| {
                mm.lock().gpu_to_cpu_address(gpu_addr)
            });
            (*texture_cache).framebuffer_for_render_target(&rt)
        };
        let Some((framebuffer, width, height)) = framebuffer else {
            if std::env::var_os("RUZU_TRACE_CLEAR").is_some() {
                debug!(
                    "RasterizerOpenGL::clear skipped, no framebuffer rt={} gpu=0x{:X} {}x{} fmt=0x{:X}",
                    rt_index, rt.address, rt.width, rt.height, rt.format
                );
            }
            return;
        };

        unsafe {
            gl::BindFramebuffer(gl::DRAW_FRAMEBUFFER, framebuffer);
            gl::Viewport(0, 0, width as i32, height as i32);
            gl::Disablei(gl::SCISSOR_TEST, 0);
            gl::ColorMaski(
                0,
                if clear_r { gl::TRUE } else { gl::FALSE },
                if clear_g { gl::TRUE } else { gl::FALSE },
                if clear_b { gl::TRUE } else { gl::FALSE },
                if clear_a { gl::TRUE } else { gl::FALSE },
            );
            gl::ClearBufferfv(gl::COLOR, 0, draw_state.clear_state.color.as_ptr());
            gl::ColorMaski(0, gl::TRUE, gl::TRUE, gl::TRUE, gl::TRUE);
        }
        self.num_queued_commands = self.num_queued_commands.saturating_add(1);

        if std::env::var_os("RUZU_TRACE_CLEAR").is_some() {
            debug!(
                "RasterizerOpenGL::clear color rt={} gpu=0x{:X} fbo={} {}x{} rgba={:?} layers={}",
                rt_index,
                rt.address,
                framebuffer,
                width,
                height,
                draw_state.clear_state.color,
                layer_count
            );
        }
    }

    fn dispatch_compute(&mut self) {
        debug!("RasterizerOpenGL::dispatch_compute");
    }

    fn reset_counter(&mut self, query_type: u32) {
        let Some(mapped_query_type) = maxwell_to_video_core_query(query_type) else {
            return;
        };
        self.query_cache
            .set_commands_queued(self.num_queued_commands != 0);
        self.query_cache.reset_counter(mapped_query_type as u32);
    }

    fn query(
        &mut self,
        gpu_addr: u64,
        query_type: u32,
        flags: QueryPropertiesFlags,
        mut payload: u32,
        _subreport: u32,
    ) {
        if std::env::var_os("RUZU_TRACE_GL_QUERY").is_some() {
            log::info!(
                "RasterizerOpenGL::query gpu=0x{:X} type={} flags=0x{:X} payload=0x{:X} has_mm={}",
                gpu_addr,
                query_type,
                flags.bits(),
                payload,
                self.channel_memory_manager.is_some()
            );
        }
        let Some(mapped_query_type) = maxwell_to_video_core_query(query_type) else {
            if query_type != crate::query_cache::types::QueryType::Payload as u32 {
                payload = 1;
            }
            let Some(mm) = self.channel_memory_manager.as_ref().cloned() else {
                if std::env::var_os("RUZU_TRACE_GL_QUERY").is_some() {
                    log::info!(
                        "RasterizerOpenGL::query fallback drop gpu=0x{:X} type={} flags=0x{:X} payload=0x{:X}",
                        gpu_addr,
                        query_type,
                        flags.bits(),
                        payload
                    );
                }
                return;
            };
            let has_timeout = flags.contains(QueryPropertiesFlags::HAS_TIMEOUT);
            let is_fence = flags.contains(QueryPropertiesFlags::IS_A_FENCE);
            let gpu_ticks_getter = self.gpu_ticks_getter.as_ref().cloned();
            let operation = Box::new(move || {
                let mm = mm.lock();
                if has_timeout {
                    let gpu_ticks = gpu_ticks_getter
                        .as_ref()
                        .map(|getter| getter())
                        .unwrap_or(0);
                    mm.write_block_unsafe_owned(gpu_addr + 8, &gpu_ticks.to_le_bytes());
                    mm.write_block_unsafe_owned(gpu_addr, &(payload as u64).to_le_bytes());
                } else {
                    mm.write_block_unsafe_owned(gpu_addr, &payload.to_le_bytes());
                }
            });
            if is_fence {
                self.signal_fence(operation);
            } else {
                operation();
            }
            return;
        };

        if std::env::var_os("RUZU_TRACE_GL_QUERY").is_some() {
            log::info!(
                "RasterizerOpenGL::query mapped gpu=0x{:X} mapped_type={:?} flags=0x{:X}",
                gpu_addr,
                mapped_query_type,
                flags.bits()
            );
        }

        let this = self as *mut Self;
        let this_for_invalidate = this as usize;
        let timestamp = flags.contains(QueryPropertiesFlags::HAS_TIMEOUT).then(|| {
            self.gpu_ticks_getter
                .as_ref()
                .map(|getter| getter())
                .unwrap_or(0)
        });
        self.query_cache
            .set_commands_queued(self.num_queued_commands != 0);
        self.query_cache.query(
            gpu_addr,
            mapped_query_type,
            timestamp,
            move |func| unsafe { (*this).sync_operation(func) },
            move |addr, size| unsafe {
                (*(this_for_invalidate as *mut Self)).on_cache_invalidation(addr, size)
            },
        );
    }

    fn bind_graphics_uniform_buffer(
        &mut self,
        _stage: usize,
        _index: u32,
        _gpu_addr: u64,
        _size: u32,
    ) {
    }

    fn disable_graphics_uniform_buffer(&mut self, _stage: usize, _index: u32) {}

    fn signal_fence(&mut self, func: Box<dyn FnOnce() + Send>) {
        let this = self as *mut Self;
        let this_for_pop = this as usize;
        let should_flush_now = self.fence_manager.signal_fence(
            func,
            move |is_stubbed| unsafe { (*this).fence_backend.create_fence(is_stubbed) },
            move |fence| unsafe { (*this).fence_backend.queue_fence(fence) },
            move || unsafe { (*this).should_wait_async_flushes() },
            move |fence| unsafe { (*this).fence_backend.is_fence_signaled(fence) },
            move || unsafe { (*(this_for_pop as *mut Self)).pop_async_flushes() },
            move || unsafe { (*this).num_queued_commands != 0 || (*this).should_flush_async() },
            move || unsafe { (*this).commit_async_flushes() },
        );
        if should_flush_now {
            self.flush_commands();
        }
        self.invalidate_gpu_cache();
    }

    fn sync_operation(&mut self, func: Box<dyn FnOnce() + Send>) {
        self.fence_manager.sync_operation(func);
    }

    fn signal_sync_point(&mut self, id: u32) {
        if std::env::var_os("RUZU_TRACE_SYNCPOINT").is_some() {
            log::info!(
                "GLRasterizer::signal_sync_point id={} queued_commands={}",
                id,
                self.num_queued_commands
            );
        }
        let this = self as *mut Self;
        let this_for_pop = this as usize;
        let syncpoints = Arc::clone(&self.syncpoints);
        let should_flush_now = self.fence_manager.signal_sync_point(
            id,
            {
                let syncpoints = Arc::clone(&syncpoints);
                move |value| {
                    if std::env::var_os("RUZU_TRACE_SYNCPOINT").is_some() {
                        log::info!(
                            "GLRasterizer::signal_sync_point increment_guest id={}",
                            value
                        );
                    }
                    syncpoints.increment_guest(value)
                }
            },
            move |value| {
                if std::env::var_os("RUZU_TRACE_SYNCPOINT").is_some() {
                    log::info!(
                        "GLRasterizer::signal_sync_point increment_host id={}",
                        value
                    );
                }
                syncpoints.increment_host(value)
            },
            move |is_stubbed| unsafe { (*this).fence_backend.create_fence(is_stubbed) },
            move |fence| unsafe { (*this).fence_backend.queue_fence(fence) },
            move || unsafe { (*this).should_wait_async_flushes() },
            move |fence| unsafe { (*this).fence_backend.is_fence_signaled(fence) },
            move || unsafe { (*(this_for_pop as *mut Self)).pop_async_flushes() },
            move || unsafe { (*this).num_queued_commands != 0 || (*this).should_flush_async() },
            move || unsafe { (*this).commit_async_flushes() },
        );
        if std::env::var_os("RUZU_TRACE_SYNCPOINT").is_some() {
            log::info!(
                "GLRasterizer::signal_sync_point id={} should_flush_now={}",
                id,
                should_flush_now
            );
        }
        if should_flush_now {
            self.flush_commands();
        }
        self.invalidate_gpu_cache();
    }

    fn signal_reference(&mut self) {
        let this = self as *mut Self;
        let this_for_pop = this as usize;
        self.fence_manager.signal_ordering(
            move || unsafe { (*this).should_wait_async_flushes() },
            move |fence| unsafe { (*this).fence_backend.is_fence_signaled(fence) },
            move || unsafe { (*(this_for_pop as *mut Self)).pop_async_flushes() },
            move || unsafe { (*this).accumulate_buffer_flushes() },
        );
    }

    fn release_fences(&mut self, force: bool) {
        if std::env::var_os("RUZU_TRACE_SYNCPOINT").is_some() {
            log::info!("GLRasterizer::release_fences force={}", force,);
        }
        let this = self as *mut Self;
        self.fence_manager.wait_pending_fences(
            force,
            move || unsafe { (*this).should_wait_async_flushes() },
            move |fence| unsafe { (*this).fence_backend.is_fence_signaled(fence) },
            move |fence| unsafe { (*this).fence_backend.wait_fence(fence) },
            move || unsafe { (*this).pop_async_flushes() },
        );
    }

    fn flush_all(&mut self) {
        unsafe {
            gl::Flush();
        }
    }

    fn flush_region(&mut self, addr: u64, size: u64) {
        if addr == 0 || size == 0 {
            return;
        }
        self.texture_cache.download_memory(addr, size as usize);
        self.buffer_cache.download_memory(addr, size);
        self.query_cache
            .set_commands_queued(self.num_queued_commands != 0);
        self.query_cache.flush_region(addr, size as usize);
    }

    fn must_flush_region(&self, _addr: u64, _size: u64) -> bool {
        // The Rust OpenGL port does not yet own the upstream lock/runtime graph needed
        // for exact texture-cache flush area tracking. Keep the method conservative
        // until `TextureCache` channel/runtime ownership reaches parity.
        false
    }

    fn get_flush_area(&self, addr: u64, size: u64) -> RasterizerDownloadArea {
        const PAGE: u64 = 4096;
        RasterizerDownloadArea {
            start_address: addr & !(PAGE - 1),
            end_address: (addr + size + PAGE - 1) & !(PAGE - 1),
            preemptive: true,
        }
    }

    fn invalidate_region(&mut self, addr: u64, size: u64) {
        if addr == 0 || size == 0 {
            return;
        }
        self.texture_cache.write_memory(addr, size as usize);
        self.buffer_cache.write_memory(addr, size);
        self.shader_cache.invalidate_region(addr, size as usize);
        self.query_cache
            .set_commands_queued(self.num_queued_commands != 0);
        self.query_cache.invalidate_region(addr, size as usize);
    }

    fn on_cache_invalidation(&mut self, addr: u64, size: u64) {
        if addr == 0 || size == 0 {
            return;
        }
        self.texture_cache.write_memory(addr, size as usize);
        self.buffer_cache.write_memory(addr, size);
        self.shader_cache.on_cache_invalidation(addr, size as usize);
    }

    fn on_cpu_write(&mut self, addr: u64, size: u64) -> bool {
        if addr == 0 || size == 0 {
            return false;
        }
        if self.buffer_cache.on_cpu_write(addr, size) {
            return true;
        }
        self.texture_cache.write_memory(addr, size as usize);
        self.shader_cache.invalidate_region(addr, size as usize);
        false
    }

    fn invalidate_gpu_cache(&mut self) {
        if let Some(callback) = &self.invalidate_gpu_cache_callback {
            callback();
        }
    }

    fn unmap_memory(&mut self, addr: u64, size: u64) {
        if addr == 0 || size == 0 {
            return;
        }
        self.texture_cache.unmap_memory(addr, size as usize);
        self.buffer_cache.write_memory(addr, size);
        self.shader_cache.on_cache_invalidation(addr, size as usize);
    }

    fn modify_gpu_memory(&mut self, _as_id: usize, _addr: u64, _size: u64) {}

    fn flush_and_invalidate_region(&mut self, addr: u64, size: u64) {
        if settings::is_gpu_level_extreme(&settings::values()) {
            self.flush_region(addr, size);
        }
        self.invalidate_region(addr, size);
    }

    fn wait_for_idle(&mut self) {
        unsafe { gl::MemoryBarrier(gl::ALL_BARRIER_BITS) };
        self.signal_reference();
    }

    fn fragment_barrier(&mut self) {
        unsafe {
            gl::TextureBarrier();
            gl::MemoryBarrier(gl::FRAMEBUFFER_BARRIER_BIT | gl::TEXTURE_FETCH_BARRIER_BIT);
        }
    }

    fn tiled_cache_barrier(&mut self) {
        unsafe { gl::TextureBarrier() }
    }

    fn flush_commands(&mut self) {
        if self.num_queued_commands == 0 {
            return;
        }
        if self.has_written_global_memory {
            self.has_written_global_memory = false;
            unsafe { gl::MemoryBarrier(gl::BUFFER_UPDATE_BARRIER_BIT) };
        }
        unsafe {
            gl::Flush();
        }
        self.num_queued_commands = 0;
    }

    fn tick_frame(&mut self) {
        self.frame_count += 1;
        self.num_queued_commands = 0;
        self.fence_manager.tick_frame();
        self.texture_cache.tick_frame();
        self.buffer_cache.tick_frame();
    }

    fn accelerate_surface_copy(
        &mut self,
        _src: &crate::engines::fermi_2d::Surface,
        _dst: &crate::engines::fermi_2d::Surface,
        _copy_config: &crate::engines::fermi_2d::Config,
    ) -> bool {
        false
    }

    fn accelerate_inline_to_memory(&mut self, address: u64, copy_size: usize, memory: &[u8]) {
        if std::env::var_os("RUZU_TRACE_INLINE_TO_MEMORY").is_some() {
            log::info!(
                "RasterizerOpenGL::accelerate_inline_to_memory enter gpu=0x{:X} size={} has_mm={}",
                address,
                copy_size,
                self.channel_memory_manager.is_some()
            );
        }
        let Some(mm) = self.channel_memory_manager.as_ref().cloned() else {
            if std::env::var_os("RUZU_TRACE_INLINE_TO_MEMORY").is_some() {
                log::info!(
                    "RasterizerOpenGL::accelerate_inline_to_memory missing_channel_memory_manager gpu=0x{:X} size={}",
                    address,
                    copy_size
                );
            }
            return;
        };
        let mm = mm.lock();
        let cpu_addr = mm.gpu_to_cpu_address(address);
        if std::env::var_os("RUZU_TRACE_INLINE_TO_MEMORY").is_some() {
            log::info!(
                "RasterizerOpenGL::accelerate_inline_to_memory gpu=0x{:X} cpu={:?} size={} first=0x{:02X}",
                address,
                cpu_addr,
                copy_size,
                memory.first().copied().unwrap_or(0)
            );
        }
        if cpu_addr.is_none() {
            mm.write_block_owned(address, &memory[..copy_size]);
            if std::env::var_os("RUZU_TRACE_INLINE_TO_MEMORY").is_some() {
                log::info!(
                    "RasterizerOpenGL::accelerate_inline_to_memory fallback_write_block gpu=0x{:X} size={}",
                    address,
                    copy_size
                );
            }
            return;
        }
        mm.write_block_unsafe_owned(address, &memory[..copy_size]);
        let cpu_addr = cpu_addr.unwrap();
        if !self
            .buffer_cache
            .inline_memory(cpu_addr, copy_size, &memory[..copy_size])
        {
            self.buffer_cache.write_memory(cpu_addr, copy_size as u64);
        }
        self.texture_cache.write_memory(cpu_addr, copy_size);
        self.shader_cache.invalidate_region(cpu_addr, copy_size);
        self.query_cache.invalidate_region(cpu_addr, copy_size);
        if std::env::var_os("RUZU_TRACE_INLINE_TO_MEMORY").is_some() {
            log::info!(
                "RasterizerOpenGL::accelerate_inline_to_memory complete cpu=0x{:X} size={}",
                cpu_addr,
                copy_size
            );
        }
    }

    fn initialize_channel(&mut self, channel: &crate::control::channel_state::ChannelState) {
        self.shader_cache.create_channel(channel);
        self.query_cache.create_channel(channel);
        // Upstream `RasterizerOpenGL` also creates per-channel state for the
        // texture and buffer caches here. Those owners are still partially
        // reduced in Rust.
    }

    fn bind_channel(&mut self, channel: &crate::control::channel_state::ChannelState) {
        if self.buffer_cache.channel_state.is_none() {
            self.buffer_cache.channel_state = Some(Box::default());
        }
        self.shader_cache.bind_to_channel(channel.bind_id);
        self.query_cache.bind_to_channel(channel.bind_id);
        // Extract the channel's MemoryManager and store it so subsequent
        // draws can build GpuMemoryAccess adapters.
        if let Some(ref mm) = channel.memory_manager {
            if std::env::var_os("RUZU_TRACE_GL_QUERY").is_some() {
                log::info!(
                    "RasterizerOpenGL::bind_channel channel_id={} memory_manager={:p}",
                    channel.bind_id,
                    Arc::as_ptr(mm)
                );
            }
            self.channel_memory_manager = Some(Arc::clone(mm));

            // If readers were installed before bind_channel, install the
            // buffer-cache memory adapters now.
            if let Some(ref cpu_reader) = self.cpu_memory_reader {
                self.buffer_cache
                    .set_gpu_memory(Box::new(GpuMemoryAccessAdapter {
                        mm: Arc::clone(mm),
                        cpu_reader: Arc::clone(cpu_reader),
                    }));
            }
            if let Some(ref device_reader) = self.device_memory_reader {
                self.buffer_cache
                    .set_device_memory(Box::new(DeviceMemoryAccessAdapter {
                        device_reader: Arc::clone(device_reader),
                    }));
            }
        }
    }

    fn release_channel(&mut self, channel_id: i32) {
        self.shader_cache.erase_channel(channel_id);
        self.query_cache.erase_channel(channel_id);
        self.channel_memory_manager = None;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::memory_manager::MemoryManager;
    use common::settings;
    use common::settings_enums::GpuAccuracy;

    fn install_query_memory_manager(
        rast: &mut RasterizerOpenGL,
        writes: Arc<std::sync::Mutex<Vec<(u64, Vec<u8>)>>>,
    ) {
        let mut mm = MemoryManager::new(0);
        mm.map(0x1000, 0x9000_1000, 0x10000, 0, false);
        mm.set_guest_memory_writer(Arc::new(move |addr, data| {
            writes.lock().unwrap().push((addr, data.to_vec()));
        }));
        let mm = Arc::new(parking_lot::Mutex::new(mm));
        let mut channel = crate::control::channel_state::ChannelState::new(1);
        channel.program_id = 0xCAFE;
        channel.memory_manager = Some(Arc::clone(&mm));
        rast.channel_memory_manager = Some(Arc::clone(&mm));
        rast.query_cache.create_channel(&channel);
        rast.query_cache.bind_to_channel(channel.bind_id);
    }

    #[test]
    fn query_fence_defers_guest_write_until_release() {
        let syncpoints = Arc::new(SyncpointManager::new());
        let mut rast = RasterizerOpenGL::new_for_test(syncpoints);
        let writes = Arc::new(std::sync::Mutex::new(Vec::<(u64, Vec<u8>)>::new()));
        install_query_memory_manager(&mut rast, Arc::clone(&writes));
        rast.set_gpu_ticks_getter(Arc::new(|| 0));

        rast.query(0x1000, 0, QueryPropertiesFlags::IS_A_FENCE, 0x1234_5678, 0);

        assert!(writes.lock().unwrap().is_empty());

        rast.release_fences(true);

        let writes = writes.lock().unwrap();
        assert_eq!(writes.len(), 1);
        assert_eq!(writes[0].0, 0x9000_1000);
        assert_eq!(writes[0].1, 0x1234_5678u32.to_le_bytes().to_vec());
    }

    #[test]
    fn signal_reference_does_not_queue_reference_fence() {
        let syncpoints = Arc::new(SyncpointManager::new());
        let mut rast = RasterizerOpenGL::new_for_test(syncpoints);

        assert_eq!(rast.fence_manager.queued_fence_count(), 0);
        assert_eq!(rast.fence_manager.pending_operation_batch_count(), 0);

        rast.signal_reference();

        assert_eq!(rast.fence_manager.queued_fence_count(), 0);
        assert_eq!(rast.fence_manager.pending_operation_batch_count(), 0);
    }

    #[test]
    fn signal_reference_accumulates_buffer_flushes_like_upstream() {
        let syncpoints = Arc::new(SyncpointManager::new());
        let mut rast = RasterizerOpenGL::new_for_test(syncpoints);

        rast.buffer_cache
            .test_add_uncommitted_gpu_modified_range(0x1000, 0x1000);
        assert!(rast.buffer_cache.has_uncommitted_flushes());
        assert!(!rast.buffer_cache.should_wait_async_flushes());

        rast.signal_reference();

        assert!(!rast.buffer_cache.has_uncommitted_flushes());
        assert!(rast.buffer_cache.should_wait_async_flushes());
    }

    #[test]
    fn release_fences_pops_async_flushes_for_stubbed_fence() {
        let syncpoints = Arc::new(SyncpointManager::new());
        let mut rast = RasterizerOpenGL::new_for_test(syncpoints);

        rast.buffer_cache.test_push_committed_async_flush_ranges();
        assert!(rast.buffer_cache.should_wait_async_flushes());

        rast.signal_fence(Box::new(|| {}));
        rast.release_fences(true);

        assert!(!rast.buffer_cache.should_wait_async_flushes());
    }

    #[test]
    fn signal_fence_triggers_invalidate_gpu_cache_callback() {
        let syncpoints = Arc::new(SyncpointManager::new());
        let mut rast = RasterizerOpenGL::new_for_test(syncpoints);
        let hits = Arc::new(std::sync::atomic::AtomicU32::new(0));
        let hits_cb = Arc::clone(&hits);
        rast.set_invalidate_gpu_cache_callback(Arc::new(move || {
            hits_cb.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        }));

        rast.signal_fence(Box::new(|| {}));

        assert_eq!(hits.load(std::sync::atomic::Ordering::Relaxed), 1);
    }

    #[test]
    fn signal_fence_executes_callback_immediately_outside_gpu_high_mode() {
        let previous_gpu_accuracy = {
            let mut values = settings::values_mut();
            let previous = values.current_gpu_accuracy;
            values.current_gpu_accuracy = GpuAccuracy::Normal;
            previous
        };
        let syncpoints = Arc::new(SyncpointManager::new());
        let mut rast = RasterizerOpenGL::new_for_test(syncpoints);
        let hits = Arc::new(std::sync::atomic::AtomicU32::new(0));
        let hits_cb = Arc::clone(&hits);

        rast.signal_fence(Box::new(move || {
            hits_cb.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        }));

        assert_eq!(hits.load(std::sync::atomic::Ordering::Relaxed), 1);

        settings::values_mut().current_gpu_accuracy = previous_gpu_accuracy;
    }

    #[test]
    fn query_non_fence_payload_fallback_writes_immediately_and_preserves_payload() {
        let syncpoints = Arc::new(SyncpointManager::new());
        let mut rast = RasterizerOpenGL::new_for_test(syncpoints);
        let writes = Arc::new(std::sync::Mutex::new(Vec::<(u64, Vec<u8>)>::new()));
        install_query_memory_manager(&mut rast, Arc::clone(&writes));
        rast.set_gpu_ticks_getter(Arc::new(|| 0));

        rast.query(
            0x3000,
            crate::query_cache::types::QueryType::Payload as u32,
            QueryPropertiesFlags::empty(),
            0xCAFE_BABE,
            0,
        );

        let writes = writes.lock().unwrap();
        assert_eq!(writes.len(), 1);
        assert_eq!(writes[0].0, 0x9000_3000);
        assert_eq!(writes[0].1, 0xCAFE_BABEu32.to_le_bytes().to_vec());
    }

    #[test]
    fn query_has_timeout_payload_fallback_writes_immediately_and_preserves_payload() {
        let syncpoints = Arc::new(SyncpointManager::new());
        let mut rast = RasterizerOpenGL::new_for_test(syncpoints);
        let writes = Arc::new(std::sync::Mutex::new(Vec::<(u64, Vec<u8>)>::new()));
        install_query_memory_manager(&mut rast, Arc::clone(&writes));
        rast.set_gpu_ticks_getter(Arc::new(|| 0x0123_4567_89AB_CDEF));

        rast.query(
            0x4000,
            crate::query_cache::types::QueryType::Payload as u32,
            QueryPropertiesFlags::HAS_TIMEOUT,
            0xABCD_EF01,
            0,
        );

        let writes = writes.lock().unwrap();
        assert_eq!(writes.len(), 2);
        assert_eq!(writes[0].0, 0x9000_4008);
        assert_eq!(writes[0].1, 0x0123_4567_89AB_CDEFu64.to_le_bytes().to_vec());
        assert_eq!(writes[1].0, 0x9000_4000);
        assert_eq!(writes[1].1, 0xABCD_EF01u64.to_le_bytes().to_vec());
    }

    #[test]
    fn query_fallback_non_payload_fence_writes_one_after_release() {
        let syncpoints = Arc::new(SyncpointManager::new());
        let mut rast = RasterizerOpenGL::new_for_test(syncpoints);
        let writes = Arc::new(std::sync::Mutex::new(Vec::<(u64, Vec<u8>)>::new()));
        install_query_memory_manager(&mut rast, Arc::clone(&writes));

        rast.query(
            0x5000,
            crate::query_cache::types::QueryType::VerticesGenerated as u32,
            QueryPropertiesFlags::IS_A_FENCE,
            0xDEAD_BEEF,
            0,
        );

        assert!(writes.lock().unwrap().is_empty());

        rast.release_fences(true);

        let writes = writes.lock().unwrap();
        assert_eq!(writes.len(), 1);
        assert_eq!(writes[0].0, 0x9000_5000);
        assert_eq!(writes[0].1, 1u32.to_le_bytes().to_vec());
    }

    #[test]
    fn tick_frame_resets_queued_commands_like_upstream() {
        let syncpoints = Arc::new(SyncpointManager::new());
        let mut rast = RasterizerOpenGL::new_for_test(syncpoints);
        rast.num_queued_commands = 7;

        rast.tick_frame();

        assert_eq!(rast.num_queued_commands, 0);
        assert_eq!(rast.frame_count, 1);
    }
}
