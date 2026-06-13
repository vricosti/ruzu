// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_rasterizer.h and gl_rasterizer.cpp
//! Status: EN COURS
//!
//! OpenGL rasterizer — processes Maxwell 3D draw commands using OpenGL.
//! Implements [`RasterizerInterface`]. Currently delegates actual rendering
//! to the software rasterizer; GL-accelerated rendering will be added as
//! buffer/texture/shader caches are ported from zuyu.

use log::{debug, error, info, warn};
use std::ffi::c_void;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::sync::OnceLock;
use std::time::Instant;

use common::settings;

use super::gl_buffer_cache::BufferCacheParams as OpenGLBufferCacheParams;
use super::gl_device::Device;
use super::gl_fence_manager::{Fence, FenceManagerOpenGL};
use super::gl_query_cache::QueryCache;
use super::gl_shader_cache::ShaderCache as OpenGLShaderCache;
use super::gl_shader_manager::{ProgramManager, ProgramManagerHandle};
use super::gl_state_tracker::{dirty as GlDirty, StateTracker};
use super::gl_texture_cache::TextureCache as OpenGLTextureCache;
use crate::buffer_cache::buffer_cache::BufferCache as CommonBufferCache;
use crate::buffer_cache::word_manager::DeviceTracker;
use crate::engines::draw_manager::{
    DrawState, Maxwell3DClearView, Maxwell3DDrawView, Maxwell3DIndirectView,
};
use crate::engines::kepler_compute::DispatchCall;
use crate::engines::maxwell_3d::{
    BlendEquation, BlendFactor, ComparisonOp, CullFace, DepthMode, DrawCall, FillViaTriangleMode,
    FrontFace, PolygonMode, StencilFaceInfo, StencilOp,
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
use crate::texture_cache::texture_cache_base::ComputeDescriptorSyncRegs;

macro_rules! lock_two_reentrant_mutexes {
    ($first_mutex:expr, $second_mutex:expr, $first_guard:ident, $second_guard:ident) => {
        let $first_guard;
        let $second_guard;
        loop {
            let first_candidate = unsafe { (*$first_mutex).lock() };
            if let Some(second_candidate) = unsafe { (*$second_mutex).try_lock() } {
                $first_guard = first_candidate;
                $second_guard = second_candidate;
                break;
            }
            drop(first_candidate);
            std::thread::yield_now();

            let second_candidate = unsafe { (*$second_mutex).lock() };
            if let Some(first_candidate) = unsafe { (*$first_mutex).try_lock() } {
                $first_guard = first_candidate;
                $second_guard = second_candidate;
                break;
            }
            drop(second_candidate);
            std::thread::yield_now();
        }
    };
}

macro_rules! trace_gl_draw_stall {
    ($($arg:tt)*) => {
        if std::env::var_os("RUZU_TRACE_GL_DRAW_STALL").is_some() {
            eprintln!($($arg)*);
        }
    };
}

static GL_DRAW_LAST_SEQ: AtomicU64 = AtomicU64::new(0);
static GL_DRAW_LAST_STAGE: AtomicU64 = AtomicU64::new(0);
static GL_DRAW_LAST_DETAIL0: AtomicU64 = AtomicU64::new(u64::MAX);
static GL_DRAW_LAST_DETAIL1: AtomicU64 = AtomicU64::new(u64::MAX);
static GL_DRAW_STAGE_COUNTS: [AtomicU64; 57] = [const { AtomicU64::new(0) }; 57];

fn record_gl_draw_stage(seq: u64, stage: usize) {
    if std::env::var_os("RUZU_PROFILE_GL_DRAW_STALL").is_none() {
        return;
    }
    GL_DRAW_LAST_SEQ.store(seq, Ordering::Relaxed);
    GL_DRAW_LAST_STAGE.store(stage as u64, Ordering::Relaxed);
    if let Some(counter) = GL_DRAW_STAGE_COUNTS.get(stage) {
        counter.fetch_add(1, Ordering::Relaxed);
    }
}

fn record_gl_draw_stage_detail(seq: u64, stage: usize, detail0: u64, detail1: u64) {
    record_gl_draw_stage(seq, stage);
    if std::env::var_os("RUZU_PROFILE_GL_DRAW_STALL").is_none() {
        return;
    }
    GL_DRAW_LAST_DETAIL0.store(detail0, Ordering::Relaxed);
    GL_DRAW_LAST_DETAIL1.store(detail1, Ordering::Relaxed);
}

pub fn dump_gl_draw_stall_profile() {
    if GL_DRAW_LAST_SEQ.load(Ordering::Relaxed) == 0
        && GL_DRAW_STAGE_COUNTS[0].load(Ordering::Relaxed) == 0
    {
        return;
    }
    const NAMES: [&str; 57] = [
        "enter",
        "before_rt_prepare",
        "after_rt_prepare",
        "before_pipeline",
        "after_pipeline",
        "after_gpu_tick",
        "before_build_programs",
        "after_build_programs",
        "before_cache_locks",
        "after_cache_locks",
        "after_descriptor_sync",
        "after_pipeline_configure",
        "before_draw_call",
        "exit",
        "after_set_engine_state",
        "before_descriptor_walk",
        "after_descriptor_walk",
        "after_fill_image_views",
        "after_prepare_images",
        "after_materialize_views",
        "after_materialize_samplers",
        "after_descriptor_bind_setup",
        "after_bind_textures",
        "after_bind_images",
        "after_descriptor_block",
        "after_base_bindings",
        "after_uniform_buffers",
        "before_update_graphics_buffers",
        "after_update_graphics_buffers",
        "before_bind_host_buffers",
        "after_bind_host_buffers",
        "before_fixed_state_sync",
        "desc_stage_enter",
        "after_unbind_storage_buffers",
        "before_storage_buffers",
        "after_storage_buffers",
        "before_texture_buffers",
        "after_texture_buffers",
        "before_image_buffers",
        "after_image_buffers",
        "before_sampled_textures",
        "after_sampled_textures",
        "before_storage_images",
        "after_storage_images",
        "desc_stage_exit",
        "before_sampled_resolve",
        "after_sampled_resolve",
        "after_sampled_texture_pair",
        "before_sampled_sampler_resolve",
        "after_sampled_sampler_resolve",
        "after_sampled_push",
        "before_cbuf_address_lock",
        "after_cbuf_address_lock",
        "after_cbuf_address_check",
        "before_cbuf_read_lock",
        "after_cbuf_read_lock",
        "after_cbuf_read",
    ];
    let last_stage = GL_DRAW_LAST_STAGE.load(Ordering::Relaxed) as usize;
    let last_stage_name = NAMES.get(last_stage).copied().unwrap_or("unknown");
    eprintln!(
        "[GL_DRAW_STALL_PROFILE] last_seq={} last_stage={} ({})",
        GL_DRAW_LAST_SEQ.load(Ordering::Relaxed),
        last_stage,
        last_stage_name
    );
    eprintln!(
        "[GL_DRAW_STALL_PROFILE] last_detail0={} last_detail1={}",
        GL_DRAW_LAST_DETAIL0.load(Ordering::Relaxed),
        GL_DRAW_LAST_DETAIL1.load(Ordering::Relaxed)
    );
    for (index, name) in NAMES.iter().enumerate() {
        eprintln!(
            "[GL_DRAW_STALL_PROFILE]   {:02} {:<24} {}",
            index,
            name,
            GL_DRAW_STAGE_COUNTS[index].load(Ordering::Relaxed)
        );
    }
}

type GlDepthRangeIndexeddNV = unsafe extern "system" fn(u32, f64, f64);
type GlViewportSwizzleNV = unsafe extern "system" fn(u32, u32, u32, u32, u32);
type GlPolygonOffsetClamp = unsafe extern "system" fn(f32, f32, f32);
type GlAlphaFunc = unsafe extern "system" fn(u32, f32);

static GL_DEPTH_RANGE_INDEXEDDNV: OnceLock<Option<GlDepthRangeIndexeddNV>> = OnceLock::new();
static GL_VIEWPORT_SWIZZLE_NV: OnceLock<Option<GlViewportSwizzleNV>> = OnceLock::new();
static GL_POLYGON_OFFSET_CLAMP: OnceLock<Option<GlPolygonOffsetClamp>> = OnceLock::new();
static GL_ALPHA_FUNC: OnceLock<Option<GlAlphaFunc>> = OnceLock::new();
static GL_ALPHA_FUNC_MISSING_LOGGED: std::sync::atomic::AtomicBool =
    std::sync::atomic::AtomicBool::new(false);

fn load_optional_gl_function<T, F>(load_fn: &mut F, name: &'static str) -> Option<T>
where
    F: FnMut(&'static str) -> *const c_void,
{
    let ptr = load_fn(name);
    if ptr.is_null() {
        None
    } else {
        Some(unsafe { std::mem::transmute_copy::<*const c_void, T>(&ptr) })
    }
}

/// Load optional OpenGL entry points that are not emitted by the generated
/// `gl` bindings but are used by upstream `RasterizerOpenGL::SyncState`.
pub fn load_extra_functions<F>(load_fn: &mut F)
where
    F: FnMut(&'static str) -> *const c_void,
{
    let _ =
        GL_DEPTH_RANGE_INDEXEDDNV.set(load_optional_gl_function(load_fn, "glDepthRangeIndexeddNV"));
    let _ = GL_VIEWPORT_SWIZZLE_NV.set(load_optional_gl_function(load_fn, "glViewportSwizzleNV"));
    let _ = GL_POLYGON_OFFSET_CLAMP.set(load_optional_gl_function(load_fn, "glPolygonOffsetClamp"));
    let _ = GL_ALPHA_FUNC.set(load_optional_gl_function(load_fn, "glAlphaFunc"));
}

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
}

impl crate::buffer_cache::buffer_cache_base::GpuMemoryAccess for GpuMemoryAccessAdapter {
    fn gpu_to_cpu_address(&self, gpu_addr: u64) -> Option<u64> {
        self.mm.lock().gpu_to_cpu_address(gpu_addr)
    }

    fn read_u64(&self, gpu_addr: u64) -> Option<u64> {
        let mut buf = [0u8; 8];
        self.mm.lock().read_block(gpu_addr, &mut buf);
        Some(u64::from_le_bytes(buf))
    }

    fn read_u32(&self, gpu_addr: u64) -> Option<u32> {
        let mut buf = [0u8; 4];
        self.mm.lock().read_block(gpu_addr, &mut buf);
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

/// Adapter that implements `EngineState` for the buffer cache by combining
/// upstream `DrawState` fields with the current Maxwell3D draw-register view.
/// Installed on the buffer cache at the start of each `RasterizerOpenGL::draw`
/// call so `update_index_buffer` / `update_vertex_buffers` see the current
/// draw's register state without adding non-upstream fields to `DrawState`.
///
/// Dirty flags always report `true` (forcing the cache to re-read every
/// draw) because the Rust port doesn't yet track per-field dirty bits on
/// the Maxwell3D register file the way upstream does.
struct DrawStateEngineAdapter {
    draw_state: DrawState,
    registers: crate::engines::draw_manager::Maxwell3DDrawRegisters,
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
            start_address: self.registers.index_buffer_gpu_addr,
            end_address: self.registers.index_buffer_gpu_addr_end,
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
        let Some(stream) = self.registers.vertex_streams.get(index as usize) else {
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
        let Some(limit) = self.registers.vertex_stream_limits.get(index as usize) else {
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
        stage: usize,
        cbuf_index: u32,
    ) -> crate::buffer_cache::buffer_cache_base::ConstBufferInfo {
        let Some(stage_bindings) = self.registers.cb_bindings.get(stage) else {
            return crate::buffer_cache::buffer_cache_base::ConstBufferInfo::default();
        };
        let Some(binding) = stage_bindings.get(cbuf_index as usize) else {
            return crate::buffer_cache::buffer_cache_base::ConstBufferInfo::default();
        };
        crate::buffer_cache::buffer_cache_base::ConstBufferInfo {
            address: binding.address,
            size: binding.size,
            enabled: binding.enabled,
        }
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

fn parse_trace_u64_env(name: &str) -> Option<u64> {
    let value = std::env::var(name).ok()?;
    let value = value.trim();
    if let Some(hex) = value
        .strip_prefix("0x")
        .or_else(|| value.strip_prefix("0X"))
    {
        u64::from_str_radix(hex, 16).ok()
    } else {
        value.parse::<u64>().ok()
    }
}

fn trace_u64_env_cached(slot: &'static OnceLock<Option<u64>>, name: &str) -> Option<u64> {
    *slot.get_or_init(|| parse_trace_u64_env(name))
}

#[derive(Debug)]
struct GlDrawDebugFlags {
    profile_gl_draw: bool,
    trace_draw_summary: bool,
    trace_rt: bool,
    trace_texture_descriptors: bool,
    trace_ssbo_bind: bool,
    trace_bind_textures: bool,
    dump_bound_textures: bool,
    disable_sampler_bind: bool,
    trace_per_draw_bind: bool,
    force_disable_blend: bool,
    force_barrier: bool,
    trace_pre_draw_state: bool,
    trace_samples_passed: bool,
    predraw_clear_red: bool,
    force_no_cull: bool,
    force_no_primitive_restart: bool,
    force_sample_mask_all: bool,
    force_no_compat_kill_state: bool,
    force_simple_draw_state: bool,
    force_draw_fbo_validate: bool,
    force_draw_attachment_validate: bool,
    trace_draw_blend_state: bool,
    trace_draw_state_full: bool,
    trace_fbo_attach_at_draw: bool,
    trace_draw_bind_recheck: bool,
    force_vertex_binding_validate: bool,
    force_vertex_attrib_validate: bool,
    trace_guest_ebo_dump: bool,
    trace_ebo_dump: bool,
    trace_vbo_dump: bool,
    trace_vao_dump: bool,
    trace_sampler_dump: bool,
    trace_ubo_dump: bool,
    trace_any_samples_passed: bool,
    trace_rt_grid_phase: bool,
    trace_rt_grid: bool,
    trace_direct_tex_read: bool,
    trace_draw_readback: bool,
    trace_gl_draw_error: bool,
    trace_rt_readback: bool,
    dump_draw_buffers: bool,
    dump_draw_attrs: bool,
}

impl GlDrawDebugFlags {
    fn get() -> &'static Self {
        static FLAGS: OnceLock<GlDrawDebugFlags> = OnceLock::new();
        FLAGS.get_or_init(|| GlDrawDebugFlags {
            profile_gl_draw: std::env::var_os("RUZU_PROFILE_GL_DRAW").is_some(),
            trace_draw_summary: std::env::var_os("RUZU_TRACE_DRAW_SUMMARY").is_some(),
            trace_rt: std::env::var_os("RUZU_TRACE_RT").is_some(),
            trace_texture_descriptors: std::env::var_os("RUZU_TRACE_TEXTURE_DESCRIPTORS").is_some(),
            trace_ssbo_bind: std::env::var_os("RUZU_TRACE_SSBO_BIND").is_some(),
            trace_bind_textures: std::env::var_os("RUZU_TRACE_BIND_TEXTURES").is_some(),
            dump_bound_textures: std::env::var_os("RUZU_DUMP_BOUND_TEXTURES").is_some(),
            disable_sampler_bind: std::env::var_os("RUZU_DISABLE_SAMPLER_BIND").is_some(),
            trace_per_draw_bind: std::env::var_os("RUZU_TRACE_PER_DRAW_BIND").is_some(),
            force_disable_blend: std::env::var_os("RUZU_FORCE_DISABLE_BLEND").is_some(),
            force_barrier: std::env::var_os("RUZU_FORCE_BARRIER").is_some(),
            trace_pre_draw_state: std::env::var_os("RUZU_TRACE_PRE_DRAW_STATE").is_some(),
            trace_samples_passed: std::env::var_os("RUZU_TRACE_SAMPLES_PASSED").is_some(),
            predraw_clear_red: std::env::var_os("RUZU_PREDRAW_CLEAR_RED").is_some(),
            force_no_cull: std::env::var_os("RUZU_FORCE_NO_CULL").is_some(),
            force_no_primitive_restart: std::env::var_os("RUZU_FORCE_NO_PRIMITIVE_RESTART")
                .is_some(),
            force_sample_mask_all: std::env::var_os("RUZU_FORCE_SAMPLE_MASK_ALL").is_some(),
            force_no_compat_kill_state: std::env::var_os("RUZU_FORCE_NO_COMPAT_KILL_STATE")
                .is_some(),
            force_simple_draw_state: std::env::var_os("RUZU_FORCE_SIMPLE_DRAW_STATE").is_some(),
            force_draw_fbo_validate: std::env::var_os("RUZU_FORCE_DRAW_FBO_VALIDATE").is_some(),
            force_draw_attachment_validate: std::env::var_os("RUZU_FORCE_DRAW_ATTACHMENT_VALIDATE")
                .is_some(),
            trace_draw_blend_state: std::env::var_os("RUZU_TRACE_DRAW_BLEND_STATE").is_some(),
            trace_draw_state_full: std::env::var_os("RUZU_TRACE_DRAW_STATE_FULL").is_some(),
            trace_fbo_attach_at_draw: std::env::var_os("RUZU_TRACE_FBO_ATTACH_AT_DRAW").is_some(),
            trace_draw_bind_recheck: std::env::var_os("RUZU_TRACE_DRAW_BIND_RECHECK").is_some(),
            force_vertex_binding_validate: std::env::var_os("RUZU_FORCE_VERTEX_BINDING_VALIDATE")
                .is_some(),
            force_vertex_attrib_validate: std::env::var_os("RUZU_FORCE_VERTEX_ATTRIB_VALIDATE")
                .is_some(),
            trace_guest_ebo_dump: std::env::var_os("RUZU_TRACE_GUEST_EBO_DUMP").is_some(),
            trace_ebo_dump: std::env::var_os("RUZU_TRACE_EBO_DUMP").is_some(),
            trace_vbo_dump: std::env::var_os("RUZU_TRACE_VBO_DUMP").is_some(),
            trace_vao_dump: std::env::var_os("RUZU_TRACE_VAO_DUMP").is_some(),
            trace_sampler_dump: std::env::var_os("RUZU_TRACE_SAMPLER_DUMP").is_some(),
            trace_ubo_dump: std::env::var_os("RUZU_TRACE_UBO_DUMP").is_some(),
            trace_any_samples_passed: std::env::var_os("RUZU_TRACE_ANY_SAMPLES_PASSED").is_some(),
            trace_rt_grid_phase: std::env::var_os("RUZU_TRACE_RT_GRID_PHASE").is_some(),
            trace_rt_grid: std::env::var_os("RUZU_TRACE_RT_GRID").is_some(),
            trace_direct_tex_read: std::env::var_os("RUZU_TRACE_DIRECT_TEX_READ").is_some(),
            trace_draw_readback: std::env::var_os("RUZU_TRACE_DRAW_READBACK").is_some(),
            trace_gl_draw_error: std::env::var_os("RUZU_TRACE_GL_DRAW_ERROR").is_some(),
            trace_rt_readback: std::env::var_os("RUZU_TRACE_RT_READBACK").is_some(),
            dump_draw_buffers: std::env::var_os("RUZU_DUMP_DRAW_BUFFERS").is_some(),
            dump_draw_attrs: std::env::var_os("RUZU_DUMP_DRAW_ATTRS").is_some(),
        })
    }
}

fn should_trace_draw_state(draw_seq: u64, pipeline_handle: u32) -> bool {
    static PIPELINE: OnceLock<Option<u64>> = OnceLock::new();
    static SEQ_MIN: OnceLock<Option<u64>> = OnceLock::new();
    static SEQ_MAX: OnceLock<Option<u64>> = OnceLock::new();
    static TIME_START: OnceLock<Option<u64>> = OnceLock::new();
    static TIME_END: OnceLock<Option<u64>> = OnceLock::new();

    let pipeline_filter = trace_u64_env_cached(&PIPELINE, "RUZU_TRACE_DRAW_STATE_PIPELINE");
    let seq_min = trace_u64_env_cached(&SEQ_MIN, "RUZU_TRACE_DRAW_STATE_SEQ_MIN").unwrap_or(0);
    let seq_max =
        trace_u64_env_cached(&SEQ_MAX, "RUZU_TRACE_DRAW_STATE_SEQ_MAX").unwrap_or(u64::MAX);
    let time_start =
        trace_u64_env_cached(&TIME_START, "RUZU_TRACE_DRAW_STATE_TIME_START_MS").unwrap_or(0);
    let time_end =
        trace_u64_env_cached(&TIME_END, "RUZU_TRACE_DRAW_STATE_TIME_END_MS").unwrap_or(u64::MAX);
    let elapsed = trace_elapsed_ms();

    pipeline_filter.is_none_or(|target| target == pipeline_handle as u64)
        && draw_seq >= seq_min
        && draw_seq <= seq_max
        && elapsed >= time_start
        && elapsed <= time_end
}

fn should_trace_cbuf_bind(draw_seq: u64, pipeline_handle: u32) -> bool {
    static PIPELINES: OnceLock<Option<Vec<u64>>> = OnceLock::new();
    static SEQ_MIN: OnceLock<Option<u64>> = OnceLock::new();
    static SEQ_MAX: OnceLock<Option<u64>> = OnceLock::new();
    static TIME_START: OnceLock<Option<u64>> = OnceLock::new();
    static TIME_END: OnceLock<Option<u64>> = OnceLock::new();

    let pipeline_filter = trace_u64_targets_env(&PIPELINES, "RUZU_TRACE_CBUF_BIND_PIPELINE");
    let seq_min = trace_u64_env_cached(&SEQ_MIN, "RUZU_TRACE_CBUF_BIND_SEQ_MIN").unwrap_or(0);
    let seq_max =
        trace_u64_env_cached(&SEQ_MAX, "RUZU_TRACE_CBUF_BIND_SEQ_MAX").unwrap_or(u64::MAX);
    let time_start =
        trace_u64_env_cached(&TIME_START, "RUZU_TRACE_CBUF_BIND_TIME_START_MS").unwrap_or(0);
    let time_end =
        trace_u64_env_cached(&TIME_END, "RUZU_TRACE_CBUF_BIND_TIME_END_MS").unwrap_or(u64::MAX);
    let elapsed = trace_elapsed_ms();

    pipeline_filter
        .is_none_or(|targets| targets.is_empty() || targets.contains(&(pipeline_handle as u64)))
        && draw_seq >= seq_min
        && draw_seq <= seq_max
        && elapsed >= time_start
        && elapsed <= time_end
}

fn trace_elapsed_ms() -> u64 {
    static START: OnceLock<Instant> = OnceLock::new();
    START
        .get_or_init(Instant::now)
        .elapsed()
        .as_millis()
        .min(u128::from(u64::MAX)) as u64
}

fn trace_elapsed_us(start: Instant) -> u64 {
    start.elapsed().as_micros().min(u128::from(u64::MAX)) as u64
}

fn gl_draw_profile_min_us() -> u64 {
    static MIN_US: OnceLock<Option<u64>> = OnceLock::new();
    trace_u64_env_cached(&MIN_US, "RUZU_TRACE_GL_DRAW_PROFILE_MIN_US").unwrap_or(500)
}

fn decode_vertex_attrib_sample(
    attrib: crate::engines::maxwell_3d::VertexAttribInfo,
    bytes: &[u8],
) -> [f32; 4] {
    use crate::engines::maxwell_3d::{VertexAttribSize as Size, VertexAttribType as Type};

    let mut out = [0.0f32; 4];
    match (attrib.size, attrib.attrib_type) {
        (Size::R32G32B32A32, Type::Float)
        | (Size::R32G32B32, Type::Float)
        | (Size::R32G32, Type::Float)
        | (Size::R32, Type::Float) => {
            for (index, chunk) in bytes
                .chunks_exact(4)
                .take(attrib.size.component_count() as usize)
                .enumerate()
            {
                out[index] =
                    f32::from_bits(u32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]));
            }
        }
        (Size::R8G8B8A8, Type::UNorm)
        | (Size::R8G8B8, Type::UNorm)
        | (Size::R8G8, Type::UNorm)
        | (Size::R8, Type::UNorm)
        | (Size::A8, Type::UNorm) => {
            for (index, byte) in bytes
                .iter()
                .take(attrib.size.component_count() as usize)
                .enumerate()
            {
                out[index] = *byte as f32 / 255.0;
            }
        }
        (Size::R16G16B16A16, Type::UNorm)
        | (Size::R16G16B16, Type::UNorm)
        | (Size::R16G16, Type::UNorm)
        | (Size::R16, Type::UNorm) => {
            for (index, chunk) in bytes
                .chunks_exact(2)
                .take(attrib.size.component_count() as usize)
                .enumerate()
            {
                out[index] = u16::from_le_bytes([chunk[0], chunk[1]]) as f32 / 65535.0;
            }
        }
        _ => {}
    }
    out
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

fn sync_depth_test_state(
    draw_view: &mut Maxwell3DDrawView<'_>,
    state_tracker: &mut Option<&mut StateTracker>,
) {
    let flags = draw_view.dirty_flags();
    let tracker_dirty_depth_mask = exchange_tracker_dirty(state_tracker, GlDirty::DEPTH_MASK);
    unsafe {
        if flags[GlDirty::DEPTH_MASK as usize] || tracker_dirty_depth_mask {
            draw_view.clear_dirty_flag(GlDirty::DEPTH_MASK);
            gl::DepthMask(if draw_view.depth_stencil().depth_write_enable {
                gl::TRUE
            } else {
                gl::FALSE
            });
        }
    }

    let flags = draw_view.dirty_flags();
    let tracker_dirty_depth_test = exchange_tracker_dirty(state_tracker, GlDirty::DEPTH_TEST);
    if !flags[GlDirty::DEPTH_TEST as usize] && !tracker_dirty_depth_test {
        return;
    }
    draw_view.clear_dirty_flag(GlDirty::DEPTH_TEST);

    let depth_stencil = draw_view.depth_stencil();
    unsafe {
        if depth_stencil.depth_test_enable {
            gl::Enable(gl::DEPTH_TEST);
            gl::DepthFunc(comparison_op_to_gl(depth_stencil.depth_func));
        } else {
            gl::Disable(gl::DEPTH_TEST);
        }
    }
}

fn sync_stencil_test_state(
    draw_view: &mut Maxwell3DDrawView<'_>,
    state_tracker: &mut Option<&mut StateTracker>,
) {
    let flags = draw_view.dirty_flags();
    let tracker_dirty = exchange_tracker_dirty(state_tracker, GlDirty::STENCIL_TEST);
    if !flags[GlDirty::STENCIL_TEST as usize] && !tracker_dirty {
        return;
    }
    draw_view.clear_dirty_flag(GlDirty::STENCIL_TEST);

    let depth_stencil = draw_view.depth_stencil();
    unsafe {
        if depth_stencil.stencil_enable {
            gl::Enable(gl::STENCIL_TEST);
        } else {
            gl::Disable(gl::STENCIL_TEST);
        }
        sync_stencil_face(gl::FRONT, depth_stencil.front);
        if depth_stencil.stencil_two_side {
            sync_stencil_face(gl::BACK, depth_stencil.back);
        } else {
            gl::StencilFuncSeparate(gl::BACK, gl::ALWAYS, 0, 0xFFFF_FFFF);
            gl::StencilOpSeparate(gl::BACK, gl::KEEP, gl::KEEP, gl::KEEP);
            gl::StencilMaskSeparate(gl::BACK, 0xFFFF_FFFF);
        }
    }
}

fn sync_depth_clamp(
    draw_view: &mut Maxwell3DDrawView<'_>,
    state_tracker: &mut Option<&mut StateTracker>,
) {
    let flags = draw_view.dirty_flags();
    let tracker_dirty = exchange_tracker_dirty(state_tracker, GlDirty::DEPTH_CLAMP_ENABLED);
    if !flags[GlDirty::DEPTH_CLAMP_ENABLED as usize] && !tracker_dirty {
        return;
    }
    draw_view.clear_dirty_flag(GlDirty::DEPTH_CLAMP_ENABLED);

    unsafe {
        if draw_view.depth_clamp_enabled() {
            gl::Enable(gl::DEPTH_CLAMP);
        } else {
            gl::Disable(gl::DEPTH_CLAMP);
        }
    }
}

fn sync_framebuffer_srgb(
    draw_view: &mut Maxwell3DDrawView<'_>,
    state_tracker: &mut Option<&mut StateTracker>,
) {
    let flags = draw_view.dirty_flags();
    let tracker_dirty = exchange_tracker_dirty(state_tracker, GlDirty::FRAMEBUFFER_SRGB);
    if !flags[GlDirty::FRAMEBUFFER_SRGB as usize] && !tracker_dirty {
        return;
    }
    draw_view.clear_dirty_flag(GlDirty::FRAMEBUFFER_SRGB);
    unsafe {
        if draw_view.framebuffer_srgb() {
            gl::Enable(gl::FRAMEBUFFER_SRGB);
        } else {
            gl::Disable(gl::FRAMEBUFFER_SRGB);
        }
    }
}

fn viewport_front_face_to_gl(
    front_face: FrontFace,
    window_origin_flip_y: bool,
    viewport0_scale_y: f32,
) -> u32 {
    let mode = front_face_to_gl(front_face);
    let mut flip_faces = true;
    if window_origin_flip_y {
        flip_faces = !flip_faces;
    }
    if viewport0_scale_y < 0.0 {
        flip_faces = !flip_faces;
    }
    if !flip_faces {
        return mode;
    }
    match mode {
        gl::CW => gl::CCW,
        gl::CCW => gl::CW,
        _ => mode,
    }
}

fn clip_control_depth(depth_mode: DepthMode) -> u32 {
    match depth_mode {
        DepthMode::ZeroToOne => gl::ZERO_TO_ONE,
        DepthMode::MinusOneToOne => gl::NEGATIVE_ONE_TO_ONE,
    }
}

fn clip_control_origin(window_origin_lower_left: bool, viewport0_scale_y: f32) -> u32 {
    let mut flip_y = false;
    if viewport0_scale_y < 0.0 {
        flip_y = !flip_y;
    }
    if window_origin_lower_left {
        flip_y = !flip_y;
    }
    if flip_y {
        gl::UPPER_LEFT
    } else {
        gl::LOWER_LEFT
    }
}

fn viewport_swizzle_components(swizzle: u32) -> [u32; 4] {
    [
        crate::renderer_opengl::maxwell_to_gl::viewport_swizzle(swizzle & 0x7),
        crate::renderer_opengl::maxwell_to_gl::viewport_swizzle((swizzle >> 4) & 0x7),
        crate::renderer_opengl::maxwell_to_gl::viewport_swizzle((swizzle >> 8) & 0x7),
        crate::renderer_opengl::maxwell_to_gl::viewport_swizzle((swizzle >> 12) & 0x7),
    ]
}

fn scale_viewport_value(value: f32, scale: f32) -> f32 {
    let mut new_value = value * scale;
    if scale < 1.0 {
        new_value = new_value.abs().round().copysign(value);
    }
    new_value
}

fn exchange_tracker_dirty(state_tracker: &mut Option<&mut StateTracker>, flag: u8) -> bool {
    state_tracker
        .as_deref_mut()
        .is_some_and(|tracker| tracker.exchange(flag))
}

fn sync_rasterize_enable(
    draw_view: &mut Maxwell3DDrawView<'_>,
    state_tracker: &mut Option<&mut StateTracker>,
) {
    let flags = draw_view.dirty_flags();
    let tracker_dirty = exchange_tracker_dirty(state_tracker, GlDirty::RASTERIZE_ENABLE);
    if !flags[GlDirty::RASTERIZE_ENABLE as usize] && !tracker_dirty {
        return;
    }
    draw_view.clear_dirty_flag(GlDirty::RASTERIZE_ENABLE);
    unsafe {
        if draw_view.rasterize_enable() {
            gl::Disable(gl::RASTERIZER_DISCARD);
        } else {
            gl::Enable(gl::RASTERIZER_DISCARD);
        }
    }
}

fn sync_scissor_test(
    draw_view: &mut Maxwell3DDrawView<'_>,
    state_tracker: &mut Option<&mut StateTracker>,
) {
    let flags = draw_view.dirty_flags();
    let tracker_dirty = exchange_tracker_dirty(state_tracker, GlDirty::SCISSORS);
    let dirty_scissors = flags[GlDirty::SCISSORS as usize]
        || flags[GlDirty::RESCALE_SCISSORS as usize]
        || tracker_dirty;
    if !dirty_scissors {
        return;
    }

    let force = flags[GlDirty::RESCALE_SCISSORS as usize] || tracker_dirty;
    draw_view.clear_dirty_flag(GlDirty::SCISSORS);
    draw_view.clear_dirty_flag(GlDirty::RESCALE_SCISSORS);
    unsafe {
        for (index, scissor) in draw_view.scissors().iter().enumerate() {
            if !force && !flags[(GlDirty::SCISSOR_0 as usize) + index] {
                continue;
            }
            draw_view.clear_dirty_flag(GlDirty::SCISSOR_0 + index as u8);
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
    }
}

fn sync_color_mask(
    draw_view: &mut Maxwell3DDrawView<'_>,
    state_tracker: &mut Option<&mut StateTracker>,
) {
    let flags = draw_view.dirty_flags();
    let tracker_dirty = exchange_tracker_dirty(state_tracker, GlDirty::COLOR_MASKS);
    if !flags[GlDirty::COLOR_MASKS as usize] && !tracker_dirty {
        return;
    }

    draw_view.clear_dirty_flag(GlDirty::COLOR_MASKS);
    let force = flags[GlDirty::COLOR_MASK_COMMON as usize] || tracker_dirty;
    draw_view.clear_dirty_flag(GlDirty::COLOR_MASK_COMMON);
    let color_masks = draw_view.color_masks();
    unsafe {
        if draw_view.color_mask_common() {
            if force || flags[GlDirty::COLOR_MASK_0 as usize] {
                draw_view.clear_dirty_flag(GlDirty::COLOR_MASK_0);
                let mask = color_masks[0];
                gl::ColorMask(
                    if mask.r { gl::TRUE } else { gl::FALSE },
                    if mask.b { gl::TRUE } else { gl::FALSE },
                    if mask.g { gl::TRUE } else { gl::FALSE },
                    if mask.a { gl::TRUE } else { gl::FALSE },
                );
            }
        } else {
            for (rt, mask) in color_masks.iter().enumerate() {
                if !force && !flags[(GlDirty::COLOR_MASK_0 as usize) + rt] {
                    continue;
                }
                draw_view.clear_dirty_flag(GlDirty::COLOR_MASK_0 + rt as u8);
                gl::ColorMaski(
                    rt as u32,
                    if mask.r { gl::TRUE } else { gl::FALSE },
                    if mask.g { gl::TRUE } else { gl::FALSE },
                    if mask.b { gl::TRUE } else { gl::FALSE },
                    if mask.a { gl::TRUE } else { gl::FALSE },
                );
            }
        }
    }
}

fn sync_blend_state(
    draw_view: &mut Maxwell3DDrawView<'_>,
    state_tracker: &mut Option<&mut StateTracker>,
) {
    let flags = draw_view.dirty_flags();
    let tracker_dirty_blend_color = exchange_tracker_dirty(state_tracker, GlDirty::BLEND_COLOR);
    unsafe {
        if flags[GlDirty::BLEND_COLOR as usize] || tracker_dirty_blend_color {
            draw_view.clear_dirty_flag(GlDirty::BLEND_COLOR);
            gl::BlendColor(
                draw_view.blend_color().r,
                draw_view.blend_color().g,
                draw_view.blend_color().b,
                draw_view.blend_color().a,
            );
        }
    }

    let flags = draw_view.dirty_flags();
    let tracker_dirty_blend_states = exchange_tracker_dirty(state_tracker, GlDirty::BLEND_STATES);
    if !flags[GlDirty::BLEND_STATES as usize] && !tracker_dirty_blend_states {
        return;
    }

    draw_view.clear_dirty_flag(GlDirty::BLEND_STATES);
    unsafe {
        if !draw_view.blend_per_target_enabled() {
            let blend = draw_view.global_blend();
            if !blend.enabled {
                gl::Disable(gl::BLEND);
            } else {
                gl::Enable(gl::BLEND);
                gl::BlendFuncSeparate(
                    blend_factor_to_gl(blend.color_src),
                    blend_factor_to_gl(blend.color_dst),
                    blend_factor_to_gl(blend.alpha_src),
                    blend_factor_to_gl(blend.alpha_dst),
                );
                gl::BlendEquationSeparate(
                    blend_equation_to_gl(blend.color_op),
                    blend_equation_to_gl(blend.alpha_op),
                );
            }
        } else {
            let force =
                flags[GlDirty::BLEND_INDEPENDENT_ENABLED as usize] || tracker_dirty_blend_states;
            draw_view.clear_dirty_flag(GlDirty::BLEND_INDEPENDENT_ENABLED);
            for (rt, blend) in draw_view.blend().iter().enumerate() {
                if !force && !flags[(GlDirty::BLEND_STATE_0 as usize) + rt] {
                    continue;
                }
                draw_view.clear_dirty_flag(GlDirty::BLEND_STATE_0 + rt as u8);
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
    }
}

fn sync_logic_op_state(
    draw_view: &mut Maxwell3DDrawView<'_>,
    state_tracker: &mut Option<&mut StateTracker>,
) {
    let flags = draw_view.dirty_flags();
    let tracker_dirty = exchange_tracker_dirty(state_tracker, GlDirty::LOGIC_OP);
    if !flags[GlDirty::LOGIC_OP as usize] && !tracker_dirty {
        return;
    }
    draw_view.clear_dirty_flag(GlDirty::LOGIC_OP);

    let logic_op = draw_view.logic_op();
    unsafe {
        if logic_op.enabled {
            gl::Enable(gl::COLOR_LOGIC_OP);
            gl::LogicOp(crate::renderer_opengl::maxwell_to_gl::logic_op(logic_op.op));
        } else {
            gl::Disable(gl::COLOR_LOGIC_OP);
        }
    }
}

fn sync_cull_mode(
    draw_view: &mut Maxwell3DDrawView<'_>,
    state_tracker: &mut Option<&mut StateTracker>,
) {
    let flags = draw_view.dirty_flags();
    let tracker_dirty = exchange_tracker_dirty(state_tracker, GlDirty::CULL_TEST);
    if !flags[GlDirty::CULL_TEST as usize] && !tracker_dirty {
        return;
    }
    draw_view.clear_dirty_flag(GlDirty::CULL_TEST);

    let rasterizer = draw_view.rasterizer();
    unsafe {
        if rasterizer.cull_enable {
            gl::Enable(gl::CULL_FACE);
            gl::CullFace(cull_face_to_gl(rasterizer.cull_face));
        } else {
            gl::Disable(gl::CULL_FACE);
        }
    }
}

fn sync_polygon_modes(
    draw_view: &mut Maxwell3DDrawView<'_>,
    state_tracker: &mut Option<&mut StateTracker>,
    has_fill_rectangle: bool,
) {
    let flags = draw_view.dirty_flags();
    let tracker_dirty = exchange_tracker_dirty(state_tracker, GlDirty::POLYGON_MODES);
    if !flags[GlDirty::POLYGON_MODES as usize] && !tracker_dirty {
        return;
    }
    draw_view.clear_dirty_flag(GlDirty::POLYGON_MODES);

    let rasterizer = draw_view.rasterizer();
    unsafe {
        if rasterizer.fill_via_triangle_mode != FillViaTriangleMode::Disabled {
            if !has_fill_rectangle {
                error!("GL_NV_fill_rectangle used and not supported");
                gl::PolygonMode(gl::FRONT_AND_BACK, gl::FILL);
                return;
            }

            const GL_FILL_RECTANGLE_NV: u32 = 0x933C;
            draw_view.set_dirty_flag(GlDirty::POLYGON_MODE_FRONT);
            draw_view.set_dirty_flag(GlDirty::POLYGON_MODE_BACK);
            gl::PolygonMode(gl::FRONT_AND_BACK, GL_FILL_RECTANGLE_NV);
            return;
        }

        if rasterizer.polygon_mode_front == rasterizer.polygon_mode_back {
            draw_view.clear_dirty_flag(GlDirty::POLYGON_MODE_FRONT);
            draw_view.clear_dirty_flag(GlDirty::POLYGON_MODE_BACK);
            gl::PolygonMode(
                gl::FRONT_AND_BACK,
                polygon_mode_to_gl(rasterizer.polygon_mode_front),
            );
            return;
        }

        if flags[GlDirty::POLYGON_MODE_FRONT as usize] || tracker_dirty {
            draw_view.clear_dirty_flag(GlDirty::POLYGON_MODE_FRONT);
            gl::PolygonMode(gl::FRONT, polygon_mode_to_gl(rasterizer.polygon_mode_front));
        }

        if flags[GlDirty::POLYGON_MODE_BACK as usize] || tracker_dirty {
            draw_view.clear_dirty_flag(GlDirty::POLYGON_MODE_BACK);
            gl::PolygonMode(gl::BACK, polygon_mode_to_gl(rasterizer.polygon_mode_back));
        }
    }
}

fn sync_fragment_color_clamp_state(
    draw_view: &mut Maxwell3DDrawView<'_>,
    state_tracker: &mut Option<&mut StateTracker>,
) {
    let flags = draw_view.dirty_flags();
    let tracker_dirty = exchange_tracker_dirty(state_tracker, GlDirty::FRAGMENT_CLAMP_COLOR);
    if !flags[GlDirty::FRAGMENT_CLAMP_COLOR as usize] && !tracker_dirty {
        return;
    }
    draw_view.clear_dirty_flag(GlDirty::FRAGMENT_CLAMP_COLOR);

    const GL_CLAMP_FRAGMENT_COLOR: u32 = 0x891B;
    unsafe {
        gl::ClampColor(
            GL_CLAMP_FRAGMENT_COLOR,
            if draw_view.frag_color_clamp_any_enabled() {
                gl::TRUE as u32
            } else {
                gl::FALSE as u32
            },
        );
    }
}

fn sync_multi_sample_state(
    draw_view: &mut Maxwell3DDrawView<'_>,
    state_tracker: &mut Option<&mut StateTracker>,
) {
    let flags = draw_view.dirty_flags();
    let tracker_dirty = exchange_tracker_dirty(state_tracker, GlDirty::MULTISAMPLE_CONTROL);
    if !flags[GlDirty::MULTISAMPLE_CONTROL as usize] && !tracker_dirty {
        return;
    }
    draw_view.clear_dirty_flag(GlDirty::MULTISAMPLE_CONTROL);

    let control = draw_view.anti_alias_alpha_control();
    unsafe {
        if control.alpha_to_coverage {
            gl::Enable(gl::SAMPLE_ALPHA_TO_COVERAGE);
        } else {
            gl::Disable(gl::SAMPLE_ALPHA_TO_COVERAGE);
        }
        if control.alpha_to_one {
            gl::Enable(gl::SAMPLE_ALPHA_TO_ONE);
        } else {
            gl::Disable(gl::SAMPLE_ALPHA_TO_ONE);
        }
    }
}

fn sync_point_state(
    draw_view: &mut Maxwell3DDrawView<'_>,
    state_tracker: &mut Option<&mut StateTracker>,
    viewport_scale: f32,
) {
    let flags = draw_view.dirty_flags();
    let tracker_dirty = exchange_tracker_dirty(state_tracker, GlDirty::POINT_SIZE);
    if !flags[GlDirty::POINT_SIZE as usize] && !tracker_dirty {
        return;
    }
    draw_view.clear_dirty_flag(GlDirty::POINT_SIZE);

    const GL_POINT_SPRITE: u32 = 0x8861;
    let point = draw_view.point_state();
    unsafe {
        if point.point_sprite_enable {
            gl::Enable(GL_POINT_SPRITE);
        } else {
            gl::Disable(GL_POINT_SPRITE);
        }
        if point.point_size_attribute_enabled {
            gl::Enable(gl::PROGRAM_POINT_SIZE);
        } else {
            gl::Disable(gl::PROGRAM_POINT_SIZE);
        }
        gl::PointSize((point.point_size * viewport_scale).max(1.0));
    }
}

fn sync_line_state(
    draw_view: &mut Maxwell3DDrawView<'_>,
    state_tracker: &mut Option<&mut StateTracker>,
) {
    let flags = draw_view.dirty_flags();
    let tracker_dirty = exchange_tracker_dirty(state_tracker, GlDirty::LINE_WIDTH);
    if !flags[GlDirty::LINE_WIDTH as usize] && !tracker_dirty {
        return;
    }
    draw_view.clear_dirty_flag(GlDirty::LINE_WIDTH);

    let line = draw_view.line_state();
    unsafe {
        if line.line_anti_alias_enable {
            gl::Enable(gl::LINE_SMOOTH);
        } else {
            gl::Disable(gl::LINE_SMOOTH);
        }
        gl::LineWidth(if line.line_anti_alias_enable {
            line.line_width_smooth
        } else {
            line.line_width_aliased
        });
    }
}

fn sync_polygon_offset(
    draw_view: &mut Maxwell3DDrawView<'_>,
    state_tracker: &mut Option<&mut StateTracker>,
) {
    let flags = draw_view.dirty_flags();
    let tracker_dirty = exchange_tracker_dirty(state_tracker, GlDirty::POLYGON_OFFSET);
    if !flags[GlDirty::POLYGON_OFFSET as usize] && !tracker_dirty {
        return;
    }
    draw_view.clear_dirty_flag(GlDirty::POLYGON_OFFSET);

    let rasterizer = draw_view.rasterizer();
    unsafe {
        if rasterizer.polygon_offset_fill_enable {
            gl::Enable(gl::POLYGON_OFFSET_FILL);
        } else {
            gl::Disable(gl::POLYGON_OFFSET_FILL);
        }
        if rasterizer.polygon_offset_line_enable {
            gl::Enable(gl::POLYGON_OFFSET_LINE);
        } else {
            gl::Disable(gl::POLYGON_OFFSET_LINE);
        }
        if rasterizer.polygon_offset_point_enable {
            gl::Enable(gl::POLYGON_OFFSET_POINT);
        } else {
            gl::Disable(gl::POLYGON_OFFSET_POINT);
        }

        if rasterizer.polygon_offset_fill_enable
            || rasterizer.polygon_offset_line_enable
            || rasterizer.polygon_offset_point_enable
        {
            let units = rasterizer.depth_bias / 2.0;
            if let Some(Some(polygon_offset_clamp)) = GL_POLYGON_OFFSET_CLAMP.get() {
                polygon_offset_clamp(
                    rasterizer.slope_scale_depth_bias,
                    units,
                    rasterizer.depth_bias_clamp,
                );
            } else {
                gl::PolygonOffset(rasterizer.slope_scale_depth_bias, units);
            }
        }
    }
}

fn sync_alpha_test(
    draw_view: &mut Maxwell3DDrawView<'_>,
    state_tracker: &mut Option<&mut StateTracker>,
) {
    let flags = draw_view.dirty_flags();
    let tracker_dirty = exchange_tracker_dirty(state_tracker, GlDirty::ALPHA_TEST);
    if !flags[GlDirty::ALPHA_TEST as usize] && !tracker_dirty {
        return;
    }
    draw_view.clear_dirty_flag(GlDirty::ALPHA_TEST);

    const GL_ALPHA_TEST_COMPAT: u32 = 0x0BC0;
    unsafe {
        if draw_view.alpha_test_enabled() {
            gl::Enable(GL_ALPHA_TEST_COMPAT);
            if let Some(Some(alpha_func)) = GL_ALPHA_FUNC.get() {
                alpha_func(
                    comparison_op_to_gl(draw_view.alpha_test_func()),
                    draw_view.alpha_test_ref(),
                );
            } else if !GL_ALPHA_FUNC_MISSING_LOGGED.swap(true, Ordering::Relaxed) {
                warn!("glAlphaFunc unavailable; skipping fixed-function alpha-test func sync");
            }
        } else {
            gl::Disable(GL_ALPHA_TEST_COMPAT);
        }
    }
}

fn sync_primitive_restart(
    draw_view: &mut Maxwell3DDrawView<'_>,
    state_tracker: &mut Option<&mut StateTracker>,
) {
    let flags = draw_view.dirty_flags();
    let tracker_dirty = exchange_tracker_dirty(state_tracker, GlDirty::PRIMITIVE_RESTART);
    if !flags[GlDirty::PRIMITIVE_RESTART as usize] && !tracker_dirty {
        return;
    }
    draw_view.clear_dirty_flag(GlDirty::PRIMITIVE_RESTART);

    let primitive_restart = draw_view.primitive_restart();
    unsafe {
        if primitive_restart.enabled {
            gl::Enable(gl::PRIMITIVE_RESTART);
            gl::PrimitiveRestartIndex(primitive_restart.index);
        } else {
            gl::Disable(gl::PRIMITIVE_RESTART);
        }
    }
}

fn sync_viewport(
    draw_view: &mut Maxwell3DDrawView<'_>,
    state_tracker: &mut Option<&mut StateTracker>,
    has_depth_buffer_float: bool,
    has_viewport_swizzle: bool,
    viewport_scale: f32,
) {
    let flags = draw_view.dirty_flags();
    let rescale_viewports = flags[crate::dirty_flags::flags::RESCALE_VIEWPORTS as usize];
    let mut tracker_dirty_viewport = false;
    let mut tracker_dirty_clip_control = false;
    let mut tracker_dirty_front_face = false;
    let mut tracker_dirty_viewport_transform = false;
    if let Some(tracker) = state_tracker.as_deref_mut() {
        tracker_dirty_viewport =
            tracker.exchange(GlDirty::VIEWPORTS) || tracker.exchange(GlDirty::RESCALE_VIEWPORTS);
        tracker_dirty_clip_control = tracker.exchange(GlDirty::CLIP_CONTROL);
        tracker_dirty_front_face = tracker.exchange(GlDirty::FRONT_FACE);
        tracker_dirty_viewport_transform = tracker.exchange(GlDirty::VIEWPORT_TRANSFORM);
    }
    let dirty_viewport =
        flags[GlDirty::VIEWPORTS as usize] || rescale_viewports || tracker_dirty_viewport;
    let dirty_clip_control = flags[GlDirty::CLIP_CONTROL as usize] || tracker_dirty_clip_control;

    unsafe {
        if dirty_viewport
            || dirty_clip_control
            || flags[GlDirty::FRONT_FACE as usize]
            || tracker_dirty_front_face
        {
            draw_view.clear_dirty_flag(GlDirty::FRONT_FACE);
            gl::FrontFace(viewport_front_face_to_gl(
                draw_view.rasterizer().front_face,
                draw_view.window_origin_flip_y(),
                draw_view.viewport0_scale_y(),
            ));
        }

        if dirty_viewport || dirty_clip_control {
            draw_view.clear_dirty_flag(GlDirty::CLIP_CONTROL);
            let clip_origin = clip_control_origin(
                draw_view.window_origin_lower_left(),
                draw_view.viewport0_scale_y(),
            );
            let mut clip_depth = clip_control_depth(draw_view.depth_mode());
            if std::env::var_os("RUZU_FORCE_NEGATIVE_ONE_TO_ONE_CLIP").is_some() {
                clip_depth = gl::NEGATIVE_ONE_TO_ONE;
            }
            if let Some(tracker) = state_tracker.as_deref_mut() {
                tracker.clip_control(clip_origin, clip_depth);
                let y_negate = draw_view.window_origin_lower_left();
                if std::env::var_os("RUZU_FORCE_Y_NEGATE_REFRESH").is_some() {
                    tracker.set_y_negate(!y_negate);
                }
                tracker.set_y_negate(y_negate);
            } else {
                gl::ClipControl(clip_origin, clip_depth);
            }
        }
        if std::env::var_os("RUZU_FORCE_Y_NEGATE_REFRESH").is_some()
            && !(dirty_viewport || dirty_clip_control)
        {
            if let Some(tracker) = state_tracker.as_deref_mut() {
                let y_negate = draw_view.window_origin_lower_left();
                tracker.set_y_negate(!y_negate);
                tracker.set_y_negate(y_negate);
            }
        }

        if dirty_viewport {
            draw_view.clear_dirty_flag(GlDirty::VIEWPORTS);
            draw_view.clear_dirty_flag(GlDirty::VIEWPORT_TRANSFORM);
            draw_view.clear_dirty_flag(GlDirty::RESCALE_VIEWPORTS);
            let force = flags[GlDirty::VIEWPORT_TRANSFORM as usize]
                || rescale_viewports
                || tracker_dirty_viewport_transform
                || tracker_dirty_viewport;
            if !draw_view.viewport_scale_offset_enabled() {
                let surface_clip = draw_view.surface_clip();
                for index in 0..draw_view.viewport_transforms().len() {
                    if !force && !flags[(GlDirty::VIEWPORT_0 as usize) + index] {
                        continue;
                    }
                    draw_view.clear_dirty_flag(GlDirty::VIEWPORT_0 + index as u8);
                    gl::ViewportIndexedf(
                        index as u32,
                        surface_clip.x as f32,
                        surface_clip.y as f32,
                        (surface_clip.width as f32).max(1.0),
                        (surface_clip.height as f32).max(1.0),
                    );
                }
            } else {
                let reduce_z = if draw_view.depth_mode() == DepthMode::MinusOneToOne {
                    1.0
                } else {
                    0.0
                };
                for (index, viewport) in draw_view.viewport_transforms().iter().enumerate() {
                    if !force && !flags[(GlDirty::VIEWPORT_0 as usize) + index] {
                        continue;
                    }
                    draw_view.clear_dirty_flag(GlDirty::VIEWPORT_0 + index as u8);
                    let x = scale_viewport_value(
                        viewport.translate_x - viewport.scale_x,
                        viewport_scale,
                    );
                    let mut y = scale_viewport_value(
                        viewport.translate_y - viewport.scale_y,
                        viewport_scale,
                    );
                    let width = scale_viewport_value(viewport.scale_x * 2.0, viewport_scale);
                    let mut height = scale_viewport_value(viewport.scale_y * 2.0, viewport_scale);
                    if height < 0.0 {
                        y += height;
                        height = -height;
                    }
                    gl::ViewportIndexedf(index as u32, x, y, width.max(1.0), height.max(1.0));
                    let near_depth =
                        viewport.translate_z as f64 - viewport.scale_z as f64 * reduce_z;
                    let far_depth = viewport.translate_z as f64 + viewport.scale_z as f64;
                    if has_depth_buffer_float {
                        if let Some(Some(depth_range_indexed)) = GL_DEPTH_RANGE_INDEXEDDNV.get() {
                            depth_range_indexed(index as u32, near_depth, far_depth);
                        } else {
                            gl::DepthRangeIndexed(index as u32, near_depth, far_depth);
                        }
                    } else {
                        gl::DepthRangeIndexed(index as u32, near_depth, far_depth);
                    }
                    if has_viewport_swizzle {
                        if let Some(Some(viewport_swizzle)) = GL_VIEWPORT_SWIZZLE_NV.get() {
                            let swizzle = viewport_swizzle_components(viewport.swizzle);
                            viewport_swizzle(
                                index as u32,
                                swizzle[0],
                                swizzle[1],
                                swizzle[2],
                                swizzle[3],
                            );
                        }
                    }
                    let _ = viewport.snap_grid_precision;
                }
            }
        }
    }
}

fn sync_vertex_formats(
    vao: u32,
    draw_view: &mut Maxwell3DDrawView<'_>,
    state_tracker: &mut Option<&mut StateTracker>,
) {
    if vao == 0 {
        return;
    }
    let flags = draw_view.dirty_flags();
    let tracker_dirty_group = exchange_tracker_dirty(state_tracker, GlDirty::VERTEX_FORMATS);
    let mut tracker_dirty_formats = [false; 16];
    for (index, dirty) in tracker_dirty_formats.iter_mut().enumerate() {
        *dirty = exchange_tracker_dirty(state_tracker, GlDirty::VERTEX_FORMAT_0 + index as u8);
    }
    if !flags[GlDirty::VERTEX_FORMATS as usize] && !tracker_dirty_group {
        if !tracker_dirty_formats.iter().any(|dirty| *dirty) {
            return;
        }
    }
    draw_view.clear_dirty_flag(GlDirty::VERTEX_FORMATS);

    let trace_attribs = std::env::var_os("RUZU_TRACE_VERTEX_ATTRIBS").is_some();
    // Upstream caps this at 16 to avoid OpenGL errors even though Maxwell
    // exposes 32 vertex attributes.
    for (index, attrib) in draw_view.vertex_attribs().iter().take(16).enumerate() {
        if !flags[(GlDirty::VERTEX_FORMAT_0 as usize) + index] && !tracker_dirty_formats[index] {
            continue;
        }
        draw_view.clear_dirty_flag(GlDirty::VERTEX_FORMAT_0 + index as u8);

        let gl_index = index as u32;
        unsafe {
            if attrib.constant
                || attrib.size == crate::engines::maxwell_3d::VertexAttribSize::Invalid
            {
                gl::DisableVertexAttribArray(gl_index);
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

            gl::EnableVertexAttribArray(gl_index);
            let component_count = attrib.size.component_count() as i32;
            let gl_format = crate::renderer_opengl::maxwell_to_gl::vertex_format(
                vertex_attrib_type_raw(attrib.attrib_type),
                vertex_attrib_size_raw(attrib.size),
            );
            if vertex_attrib_is_integer(attrib.attrib_type) {
                gl::VertexAttribIFormat(gl_index, component_count, gl_format, attrib.offset);
            } else {
                gl::VertexAttribFormat(
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
            gl::VertexAttribBinding(gl_index, attrib.buffer_index);
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

fn sync_vertex_instances(
    draw_view: &mut Maxwell3DDrawView<'_>,
    state_tracker: &mut Option<&mut StateTracker>,
) {
    let flags = draw_view.dirty_flags();
    let tracker_dirty_group = exchange_tracker_dirty(state_tracker, GlDirty::VERTEX_INSTANCES);
    let mut tracker_dirty_instances = [false; 16];
    for (index, dirty) in tracker_dirty_instances.iter_mut().enumerate() {
        *dirty = exchange_tracker_dirty(state_tracker, GlDirty::VERTEX_INSTANCE_0 + index as u8);
    }
    if !flags[GlDirty::VERTEX_INSTANCES as usize] && !tracker_dirty_group {
        if !tracker_dirty_instances.iter().any(|dirty| *dirty) {
            return;
        }
    }
    draw_view.clear_dirty_flag(GlDirty::VERTEX_INSTANCES);

    for index in 0..16 {
        if !flags[(GlDirty::VERTEX_INSTANCE_0 as usize) + index] && !tracker_dirty_instances[index]
        {
            continue;
        }
        draw_view.clear_dirty_flag(GlDirty::VERTEX_INSTANCE_0 + index as u8);

        let stream = draw_view.vertex_streams()[index];
        let instancing_enabled = draw_view.vertex_stream_instances()[index] != 0;
        let divisor = if instancing_enabled {
            stream.frequency
        } else {
            0
        };
        unsafe {
            gl::VertexBindingDivisor(index as u32, divisor);
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
    /// Diagnostic-only monotonic draw sequence. Upstream only keeps
    /// `num_queued_commands`, which resets on flush; that is correct for
    /// command flushing but ambiguous for cross-log correlation.
    total_draw_count: u64,
    has_written_global_memory: bool,
    buffer_cache: CommonBufferCache<OpenGLBufferCacheParams, OpenGLDeviceTracker>,
    /// Shared OpenGL program manager reference.
    ///
    /// Upstream `RasterizerOpenGL` stores `ProgramManager&`, with the concrete
    /// manager owned by `RendererOpenGL`.
    #[allow(dead_code)]
    program_manager: ProgramManagerHandle,
    texture_cache: OpenGLTextureCache,
    /// Generic region-tracking shader cache (region invalidation, guest address bookkeeping).
    /// Upstream inherits `GL::ShaderCache` from `VideoCommon::ShaderCache`; we keep them
    /// as two separate composed fields so each can evolve independently.
    shader_cache: ShaderCache,
    /// OpenGL-specific shader cache — owns compiled `GraphicsPipeline` / `ComputePipeline`
    /// objects and is the entry point for the draw hot path.
    gl_shader_cache: OpenGLShaderCache,
    query_cache: QueryCache,
    /// State tracker owned by the rasterizer.
    ///
    /// Upstream stores `StateTracker& state_tracker` as a member reference in
    /// `RasterizerOpenGL`, with the concrete `StateTracker` owned by value in
    /// `RendererOpenGL`. Rust cannot express member references; instead the
    /// rasterizer owns the tracker directly and `RendererOpenGL` accesses it
    /// via [`Self::state_tracker_mut`]. This avoids the ABBA deadlock that the
    /// previous `Arc<Mutex<StateTracker>>` introduced against `texture_cache`
    /// (present took state_tracker -> texture_cache; draw took the reverse).
    state_tracker: Box<StateTracker>,
    has_depth_buffer_float: bool,
    has_viewport_swizzle: bool,
    has_fill_rectangle: bool,
    invalidate_gpu_cache_callback: Option<Arc<dyn Fn() + Send + Sync>>,
    /// Per-channel GPU memory manager, extracted from `ChannelState` in
    /// `bind_channel`. Used to build the `GpuMemoryAccess` adapter for the
    /// buffer cache.
    channel_memory_manager: Option<Arc<parking_lot::Mutex<crate::memory_manager::MemoryManager>>>,
    /// Compatibility GPU-memory reader callback installed by the renderer
    /// bridge. Runtime OpenGL shader compilation uses the channel
    /// `MemoryManager` through the shared shader cache; this callback remains
    /// available for reduced/non-owner paths and older buffer-cache adapters.
    cpu_memory_reader: Option<crate::shader_environment::GpuMemoryReader>,
    /// Raw guest/device memory reader installed through RendererBase.
    /// This is distinct from `cpu_memory_reader`: the latter accepts GPU VAs
    /// for shader fetches, while buffer-cache `DeviceMemoryAccess` receives
    /// already-resolved device/CPU addresses.
    device_memory_reader: Option<crate::renderer_base::DeviceMemoryReader>,
    /// GPU tick getter used for timestamped query writes.
    gpu_ticks_getter: Option<Arc<dyn Fn() -> u64 + Send + Sync>>,
    /// Callback to process pending GPU sync work from draw paths.
    ///
    /// Upstream `RasterizerOpenGL` stores a `Tegra::GPU&` and calls
    /// `gpu.TickWork()` directly in `PrepareDraw` / `DrawTexture`.
    /// Rust keeps the owner boundary explicit by receiving the same operation
    /// as a renderer-installed callback from `Gpu::bind_renderer`.
    gpu_tick_callback: Option<Arc<dyn Fn() + Send + Sync>>,
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

fn dump_gl_buffer_prefix(label: &str, handle: u32, offset: isize, max_bytes: usize) {
    if handle == 0 {
        log::warn!("[DRAW_BUFFER] {} handle=0", label);
        return;
    }
    unsafe {
        let mut size = 0i32;
        gl::GetNamedBufferParameteriv(handle, gl::BUFFER_SIZE, &mut size);
        let start = offset.max(0) as usize;
        let len = (size.max(0) as usize).saturating_sub(start).min(max_bytes);
        let mut bytes = vec![0u8; len];
        if len != 0 {
            while gl::GetError() != gl::NO_ERROR {}
            gl::GetNamedBufferSubData(
                handle,
                offset.max(0),
                len as isize,
                bytes.as_mut_ptr().cast(),
            );
        }
        let err = gl::GetError();
        log::warn!(
            "[DRAW_BUFFER] {} handle={} size={} offset={} first{}={:02X?} err=0x{:X}",
            label,
            handle,
            size,
            offset.max(0),
            len,
            bytes,
            err
        );
    }
}

fn compute_descriptor_sync_regs_from_dispatch(
    dispatch: &DispatchCall,
) -> ComputeDescriptorSyncRegs {
    ComputeDescriptorSyncRegs {
        linked_tsc: dispatch.qmd.linked_tsc,
        tic_addr: dispatch.tic_address,
        tic_limit: dispatch.tic_limit,
        tsc_addr: dispatch.tsc_address,
        tsc_limit: dispatch.tsc_limit,
    }
}

fn rt_sample_targets() -> Option<&'static [u64]> {
    static TARGETS: OnceLock<Option<Vec<u64>>> = OnceLock::new();
    TARGETS
        .get_or_init(|| {
            let spec = std::env::var("RUZU_TRACE_RT_SAMPLE_ADDRS").ok()?;
            let spec = spec.trim();
            if spec.is_empty() {
                return None;
            }
            if spec == "*" {
                return Some(Vec::new());
            }
            let targets = spec
                .split(',')
                .filter_map(|raw| {
                    let value = raw.trim();
                    if value.is_empty() {
                        return None;
                    }
                    if let Some(hex) = value
                        .strip_prefix("0x")
                        .or_else(|| value.strip_prefix("0X"))
                    {
                        u64::from_str_radix(hex, 16).ok()
                    } else {
                        value.parse::<u64>().ok()
                    }
                })
                .collect::<Vec<_>>();
            (!targets.is_empty()).then_some(targets)
        })
        .as_deref()
}

fn should_sample_rt_address(address: u64) -> bool {
    let Some(targets) = rt_sample_targets() else {
        return false;
    };
    targets.is_empty() || targets.contains(&address)
}

fn trace_u64_targets_env(
    slot: &'static OnceLock<Option<Vec<u64>>>,
    name: &str,
) -> Option<&'static [u64]> {
    slot.get_or_init(|| {
        let spec = std::env::var(name).ok()?;
        let spec = spec.trim();
        if spec.is_empty() {
            return None;
        }
        if spec == "*" {
            return Some(Vec::new());
        }
        let targets = spec
            .split(',')
            .filter_map(|raw| {
                let value = raw.trim();
                if value.is_empty() {
                    return None;
                }
                if let Some(hex) = value
                    .strip_prefix("0x")
                    .or_else(|| value.strip_prefix("0X"))
                {
                    u64::from_str_radix(hex, 16).ok()
                } else {
                    value.parse::<u64>().ok()
                }
            })
            .collect::<Vec<_>>();
        (!targets.is_empty()).then_some(targets)
    })
    .as_deref()
}

fn should_trace_rt_zeta_pipeline(pipeline: u64) -> bool {
    static TARGETS: OnceLock<Option<Vec<u64>>> = OnceLock::new();
    let Some(targets) = trace_u64_targets_env(&TARGETS, "RUZU_TRACE_RT_ZETA_BIND_PIPELINE") else {
        return true;
    };
    targets.is_empty() || targets.contains(&pipeline)
}

fn should_trace_rt_bind_address(address: u64) -> bool {
    static TARGETS: OnceLock<Option<Vec<u64>>> = OnceLock::new();
    let Some(targets) = trace_u64_targets_env(&TARGETS, "RUZU_TRACE_RT_BIND_ADDRS") else {
        return true;
    };
    targets.is_empty() || targets.contains(&address)
}

fn should_trace_texture_bind_address(address: u64) -> bool {
    static TARGETS: OnceLock<Option<Vec<u64>>> = OnceLock::new();
    let Some(targets) = trace_u64_targets_env(&TARGETS, "RUZU_TRACE_TEXTURE_BIND_ADDRS") else {
        return true;
    };
    targets.is_empty() || targets.contains(&address)
}

fn should_skip_draw_sampling_gpu_addr(address: u64) -> bool {
    static TARGETS: OnceLock<Option<Vec<u64>>> = OnceLock::new();
    let Some(targets) = trace_u64_targets_env(&TARGETS, "RUZU_SKIP_DRAWS_SAMPLING_GPU_ADDRS")
    else {
        return false;
    };
    targets.contains(&address)
}

fn texture_grid_targets() -> Option<&'static [u64]> {
    static TARGETS: OnceLock<Option<Vec<u64>>> = OnceLock::new();
    TARGETS
        .get_or_init(|| {
            let spec = std::env::var("RUZU_TRACE_TEXTURE_GRID_ADDRS").ok()?;
            let spec = spec.trim();
            if spec.is_empty() {
                return None;
            }
            if spec == "*" {
                return Some(Vec::new());
            }
            let targets = spec
                .split(',')
                .filter_map(|raw| {
                    let value = raw.trim();
                    if value.is_empty() {
                        return None;
                    }
                    if let Some(hex) = value
                        .strip_prefix("0x")
                        .or_else(|| value.strip_prefix("0X"))
                    {
                        u64::from_str_radix(hex, 16).ok()
                    } else {
                        value.parse::<u64>().ok()
                    }
                })
                .collect::<Vec<_>>();
            (!targets.is_empty()).then_some(targets)
        })
        .as_deref()
}

fn present_extra_gpu_addr_targets() -> Option<&'static [u64]> {
    static TARGETS: OnceLock<Option<Vec<u64>>> = OnceLock::new();
    TARGETS
        .get_or_init(|| {
            let spec = std::env::var("RUZU_DUMP_PRESENT_EXTRA_GPU_ADDRS").ok()?;
            let targets = spec
                .split(',')
                .filter_map(|raw| {
                    let value = raw.trim();
                    if value.is_empty() {
                        return None;
                    }
                    if let Some(hex) = value
                        .strip_prefix("0x")
                        .or_else(|| value.strip_prefix("0X"))
                    {
                        u64::from_str_radix(hex, 16).ok()
                    } else {
                        value.parse::<u64>().ok()
                    }
                })
                .collect::<Vec<_>>();
            (!targets.is_empty()).then_some(targets)
        })
        .as_deref()
}

fn should_trace_present_texture_index(present_index: u64) -> bool {
    static START: OnceLock<Option<u64>> = OnceLock::new();
    static END: OnceLock<Option<u64>> = OnceLock::new();
    let start = trace_u64_env_cached(&START, "RUZU_TRACE_PRESENT_TEXTURE_START").unwrap_or(0);
    let end = trace_u64_env_cached(&END, "RUZU_TRACE_PRESENT_TEXTURE_END").unwrap_or(u64::MAX);
    present_index >= start && present_index <= end
}

fn has_present_texture_trace_window() -> bool {
    std::env::var_os("RUZU_TRACE_PRESENT_TEXTURE_START").is_some()
        || std::env::var_os("RUZU_TRACE_PRESENT_TEXTURE_END").is_some()
}

unsafe fn trace_present_display_texture(
    present_index: u64,
    gpu_addr: u64,
    view_id: u64,
    texture: u32,
    width: u32,
    height: u32,
) {
    if texture == 0 || width == 0 || height == 0 {
        common::trace::emit_raw(
            common::trace::cat::PRESENT_TEXTURE,
            &[
                present_index,
                gpu_addr,
                view_id,
                texture as u64,
                width as u64,
                height as u64,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
            ],
        );
        return;
    }

    let mut old_pack_buffer = 0;
    let mut old_pack_alignment = 0;
    let mut old_pack_row_length = 0;
    gl::GetIntegerv(gl::PIXEL_PACK_BUFFER_BINDING, &mut old_pack_buffer);
    gl::GetIntegerv(gl::PACK_ALIGNMENT, &mut old_pack_alignment);
    gl::GetIntegerv(gl::PACK_ROW_LENGTH, &mut old_pack_row_length);
    gl::BindBuffer(gl::PIXEL_PACK_BUFFER, 0);
    gl::PixelStorei(gl::PACK_ALIGNMENT, 1);
    gl::PixelStorei(gl::PACK_ROW_LENGTH, 0);

    while gl::GetError() != gl::NO_ERROR {}
    let mut gl_error = gl::NO_ERROR;
    let mut samples = 0u64;
    let mut rgb_nonzero = 0u64;
    let mut alpha_nonzero = 0u64;
    let mut checksum = 0u64;
    let mut first_rgba = 0u64;
    let mut last_rgba = 0u64;
    for y_idx in 0..8 {
        let y = ((height - 1) * y_idx / 7) as i32;
        for x_idx in 0..8 {
            let x = ((width - 1) * x_idx / 7) as i32;
            let mut px = [0u8; 4];
            gl::GetTextureSubImage(
                texture,
                0,
                x,
                y,
                0,
                1,
                1,
                1,
                gl::RGBA,
                gl::UNSIGNED_BYTE,
                px.len() as i32,
                px.as_mut_ptr().cast(),
            );
            let err = gl::GetError();
            if err != gl::NO_ERROR {
                gl_error = err;
                break;
            }
            let rgba = u32::from_le_bytes(px) as u64;
            if samples == 0 {
                first_rgba = rgba;
            }
            last_rgba = rgba;
            if px[0] != 0 || px[1] != 0 || px[2] != 0 {
                rgb_nonzero += 1;
            }
            if px[3] != 0 {
                alpha_nonzero += 1;
            }
            checksum = checksum.wrapping_mul(16777619).wrapping_add(rgba);
            samples += 1;
        }
        if gl_error != gl::NO_ERROR {
            break;
        }
    }

    gl::BindBuffer(gl::PIXEL_PACK_BUFFER, old_pack_buffer as u32);
    gl::PixelStorei(gl::PACK_ALIGNMENT, old_pack_alignment);
    gl::PixelStorei(gl::PACK_ROW_LENGTH, old_pack_row_length);

    common::trace::emit_raw(
        common::trace::cat::PRESENT_TEXTURE,
        &[
            present_index,
            gpu_addr,
            view_id,
            texture as u64,
            width as u64,
            height as u64,
            0,
            gl_error as u64,
            samples,
            rgb_nonzero,
            alpha_nonzero,
            checksum,
            first_rgba,
            last_rgba,
        ],
    );
}

unsafe fn dump_present_display_texture_ppm(
    present_index: u64,
    gpu_addr: u64,
    view_id: u64,
    texture: u32,
    width: u32,
    height: u32,
) {
    let Some(output_dir) = std::env::var_os("RUZU_DUMP_PRESENT_TEXTURE_PPM_DIR") else {
        return;
    };
    static TARGET_INDEX: OnceLock<Option<u64>> = OnceLock::new();
    let target_index = *TARGET_INDEX.get_or_init(|| {
        std::env::var("RUZU_DUMP_PRESENT_TEXTURE_INDEX")
            .ok()
            .and_then(|value| value.parse::<u64>().ok())
    });
    if let Some(target_index) = target_index {
        if present_index != target_index {
            return;
        }
    } else if std::env::var_os("RUZU_DUMP_PRESENT_TEXTURE_ALL").is_none()
        && !(present_index < 8 || present_index.is_power_of_two())
    {
        return;
    }
    static EVERY: OnceLock<Option<u64>> = OnceLock::new();
    let every = *EVERY.get_or_init(|| {
        std::env::var("RUZU_DUMP_PRESENT_TEXTURE_PPM_EVERY")
            .ok()
            .and_then(|value| value.parse::<u64>().ok())
            .filter(|&value| value != 0)
    });
    if every.is_some_and(|every| present_index % every != 0) {
        return;
    }
    if texture == 0 || width == 0 || height == 0 {
        return;
    }

    let width = width as usize;
    let height = height as usize;
    let byte_count = width.saturating_mul(height).saturating_mul(4);
    if byte_count == 0 || byte_count > 256 * 1024 * 1024 {
        log::warn!(
            "[PRESENT_DISPLAY_PPM] skipped present={} addr=0x{:X} view={} texture={} size={}x{} bytes={}",
            present_index,
            gpu_addr,
            view_id,
            texture,
            width,
            height,
            byte_count
        );
        return;
    }

    let mut old_pack_buffer = 0;
    let mut old_pack_alignment = 0;
    let mut old_pack_row_length = 0;
    gl::GetIntegerv(gl::PIXEL_PACK_BUFFER_BINDING, &mut old_pack_buffer);
    gl::GetIntegerv(gl::PACK_ALIGNMENT, &mut old_pack_alignment);
    gl::GetIntegerv(gl::PACK_ROW_LENGTH, &mut old_pack_row_length);
    gl::BindBuffer(gl::PIXEL_PACK_BUFFER, 0);
    gl::PixelStorei(gl::PACK_ALIGNMENT, 1);
    gl::PixelStorei(gl::PACK_ROW_LENGTH, 0);

    while gl::GetError() != gl::NO_ERROR {}
    let mut rgba = vec![0u8; byte_count];
    gl::GetTextureImage(
        texture,
        0,
        gl::RGBA,
        gl::UNSIGNED_BYTE,
        rgba.len() as i32,
        rgba.as_mut_ptr().cast(),
    );
    let gl_error = gl::GetError();

    gl::BindBuffer(gl::PIXEL_PACK_BUFFER, old_pack_buffer as u32);
    gl::PixelStorei(gl::PACK_ALIGNMENT, old_pack_alignment);
    gl::PixelStorei(gl::PACK_ROW_LENGTH, old_pack_row_length);

    let mut ppm = Vec::with_capacity(width * height * 3 + 64);
    ppm.extend_from_slice(format!("P6\n{} {}\n255\n", width, height).as_bytes());
    let mut red_pixels = 0usize;
    let mut black_pixels = 0usize;
    let mut white_pixels = 0usize;
    let mut rgb_nonzero = 0usize;
    let mut checksum = 0u64;
    let mut first_rgba = 0u32;
    let mut last_rgba = 0u32;
    let mut samples = 0usize;
    for row in (0..height).rev() {
        let start = row * width * 4;
        let end = start + width * 4;
        for px in rgba[start..end].chunks_exact(4) {
            let rgba = u32::from_le_bytes([px[0], px[1], px[2], px[3]]);
            if samples == 0 {
                first_rgba = rgba;
            }
            last_rgba = rgba;
            if px[0] != 0 || px[1] != 0 || px[2] != 0 {
                rgb_nonzero += 1;
            }
            if px[0] > 128 && px[1] < 32 && px[2] < 32 {
                red_pixels += 1;
            }
            if px[0] < 8 && px[1] < 8 && px[2] < 8 {
                black_pixels += 1;
            }
            if px[0] > 240 && px[1] > 240 && px[2] > 240 {
                white_pixels += 1;
            }
            checksum = checksum.wrapping_mul(16777619).wrapping_add(rgba as u64);
            samples += 1;
            ppm.extend_from_slice(&px[..3]);
        }
    }

    let mut path = std::path::PathBuf::from(output_dir);
    if let Err(err) = std::fs::create_dir_all(&path) {
        log::warn!(
            "[PRESENT_DISPLAY_PPM] failed to create {}: {}",
            path.display(),
            err
        );
        return;
    }
    path.push(format!(
        "present_{present_index:06}_addr_{gpu_addr:016X}_view_{view_id}_tex_{texture}.ppm"
    ));
    match std::fs::write(&path, ppm) {
        Ok(()) => log::info!(
            "[PRESENT_DISPLAY_PPM] wrote {} present={} addr=0x{:X} view={} texture={} gl_error=0x{:X} samples={} rgb_nonzero={} red={} black={} white={} checksum=0x{:X} first_rgba=0x{:08X} last_rgba=0x{:08X}",
            path.display(),
            present_index,
            gpu_addr,
            view_id,
            texture,
            gl_error,
            samples,
            rgb_nonzero,
            red_pixels,
            black_pixels,
            white_pixels,
            checksum,
            first_rgba,
            last_rgba,
        ),
        Err(err) => log::warn!(
            "[PRESENT_DISPLAY_PPM] failed to write {}: {}",
            path.display(),
            err
        ),
    }
}

fn should_trace_texture_grid_address(address: u64, draw_seq: u64) -> bool {
    let Some(targets) = texture_grid_targets() else {
        return false;
    };
    static SEQ_MIN: OnceLock<Option<u64>> = OnceLock::new();
    static SEQ_MAX: OnceLock<Option<u64>> = OnceLock::new();
    static TIME_START: OnceLock<Option<u64>> = OnceLock::new();
    static TIME_END: OnceLock<Option<u64>> = OnceLock::new();
    let seq_min = trace_u64_env_cached(&SEQ_MIN, "RUZU_TRACE_TEXTURE_GRID_SEQ_MIN").unwrap_or(0);
    let seq_max =
        trace_u64_env_cached(&SEQ_MAX, "RUZU_TRACE_TEXTURE_GRID_SEQ_MAX").unwrap_or(u64::MAX);
    let time_start =
        trace_u64_env_cached(&TIME_START, "RUZU_TRACE_TEXTURE_GRID_TIME_START_MS").unwrap_or(0);
    let time_end =
        trace_u64_env_cached(&TIME_END, "RUZU_TRACE_TEXTURE_GRID_TIME_END_MS").unwrap_or(u64::MAX);
    let elapsed = trace_elapsed_ms();
    draw_seq >= seq_min
        && draw_seq <= seq_max
        && elapsed >= time_start
        && elapsed <= time_end
        && (targets.is_empty() || targets.contains(&address))
}

unsafe fn trace_texture_grid_sample(
    draw_seq: u64,
    pipeline: u64,
    stage: u64,
    unit: u64,
    view_id: u64,
    image_id: u64,
    view_gpu_addr: u64,
    view_format: u64,
    view_type: u64,
    view_swizzle: [u8; 4],
    handle: u32,
) {
    if handle == 0 {
        return;
    }
    let mut width = 0i32;
    let mut height = 0i32;
    let mut depth = 0i32;
    gl::GetTextureLevelParameteriv(handle, 0, gl::TEXTURE_WIDTH, &mut width);
    gl::GetTextureLevelParameteriv(handle, 0, gl::TEXTURE_HEIGHT, &mut height);
    gl::GetTextureLevelParameteriv(handle, 0, gl::TEXTURE_DEPTH, &mut depth);
    if width <= 0 || height <= 0 {
        log::warn!(
            "[TEXTURE_GRID] draw_seq={} pipeline={} stage={} unit={} view_id={} image_id={} gpu=0x{:X} handle={} size={}x{}x{} skipped=empty",
            draw_seq,
            pipeline,
            stage,
            unit,
            view_id,
            image_id,
            view_gpu_addr,
            handle,
            width,
            height,
            depth,
        );
        return;
    }

    let mut nonzero = 0u32;
    let mut alpha_nonzero = 0u32;
    let mut checksum = 0u64;
    let mut first_rgba = 0u32;
    let mut samples = 0u32;
    let mut gl_error = gl::NO_ERROR;
    let mut gl_swizzle = [0i32; 4];
    let is_depth_format = matches!(
        view_format,
        x if x == crate::surface::PixelFormat::D32Float as u64
            || x == crate::surface::PixelFormat::D16Unorm as u64
            || x == crate::surface::PixelFormat::S8UintD24Unorm as u64
            || x == crate::surface::PixelFormat::D24UnormS8Uint as u64
            || x == crate::surface::PixelFormat::D32FloatS8Uint as u64
            || x == crate::surface::PixelFormat::X8D24Unorm as u64
    );

    while gl::GetError() != gl::NO_ERROR {}
    gl::GetTextureParameteriv(handle, gl::TEXTURE_SWIZZLE_RGBA, gl_swizzle.as_mut_ptr());
    for y_idx in 0..4 {
        let y = ((height - 1) * y_idx / 3).max(0);
        for x_idx in 0..4 {
            let x = ((width - 1) * x_idx / 3).max(0);
            let rgba = if is_depth_format {
                let mut depth = 0.0f32;
                gl::GetTextureSubImage(
                    handle,
                    0,
                    x,
                    y,
                    0,
                    1,
                    1,
                    1,
                    gl::DEPTH_COMPONENT,
                    gl::FLOAT,
                    std::mem::size_of::<f32>() as i32,
                    (&mut depth as *mut f32).cast(),
                );
                depth.to_bits()
            } else {
                let mut px = [0u8; 4];
                gl::GetTextureSubImage(
                    handle,
                    0,
                    x,
                    y,
                    0,
                    1,
                    1,
                    1,
                    gl::RGBA,
                    gl::UNSIGNED_BYTE,
                    4,
                    px.as_mut_ptr().cast(),
                );
                u32::from_le_bytes(px)
            };
            let err = gl::GetError();
            if err != gl::NO_ERROR {
                gl_error = err;
                break;
            }
            if samples == 0 {
                first_rgba = rgba;
            }
            if rgba != 0 {
                nonzero += 1;
            }
            if !is_depth_format && (rgba >> 24) != 0 {
                alpha_nonzero += 1;
            }
            checksum = checksum.wrapping_mul(16777619).wrapping_add(rgba as u64);
            samples += 1;
        }
        if gl_error != gl::NO_ERROR {
            break;
        }
    }

    log::warn!(
        "[TEXTURE_GRID] draw_seq={} pipeline={} stage={} unit={} view_id={} image_id={} gpu=0x{:X} format={} view_type={} swizzle={:?} gl_swizzle={:?} handle={} size={}x{}x{} samples={} rgb_nonzero={} alpha_nonzero={} checksum=0x{:X} first_rgba=0x{:08X} gl_error=0x{:X}",
        draw_seq,
        pipeline,
        stage,
        unit,
        view_id,
        image_id,
        view_gpu_addr,
        view_format,
        view_type,
        view_swizzle,
        gl_swizzle,
        handle,
        width,
        height,
        depth,
        samples,
        nonzero,
        alpha_nonzero,
        checksum,
        first_rgba,
        gl_error,
    );
}

fn should_trace_rt_sample_draw(pipeline_handle: u64, draw_seq: u64) -> bool {
    static PIPELINE_FILTER: OnceLock<Option<u64>> = OnceLock::new();
    static SEQ_MIN: OnceLock<Option<u64>> = OnceLock::new();
    static SEQ_MAX: OnceLock<Option<u64>> = OnceLock::new();
    static TIME_START: OnceLock<Option<u64>> = OnceLock::new();
    static TIME_END: OnceLock<Option<u64>> = OnceLock::new();
    let pipeline_filter = trace_u64_env_cached(&PIPELINE_FILTER, "RUZU_TRACE_RT_SAMPLE_PIPELINE");
    let seq_min = trace_u64_env_cached(&SEQ_MIN, "RUZU_TRACE_RT_SAMPLE_SEQ_MIN").unwrap_or(0);
    let seq_max =
        trace_u64_env_cached(&SEQ_MAX, "RUZU_TRACE_RT_SAMPLE_SEQ_MAX").unwrap_or(u64::MAX);
    let time_start =
        trace_u64_env_cached(&TIME_START, "RUZU_TRACE_RT_SAMPLE_TIME_START_MS").unwrap_or(0);
    let time_end =
        trace_u64_env_cached(&TIME_END, "RUZU_TRACE_RT_SAMPLE_TIME_END_MS").unwrap_or(u64::MAX);
    let elapsed = trace_elapsed_ms();
    pipeline_filter.is_none_or(|target| target == pipeline_handle)
        && draw_seq >= seq_min
        && draw_seq <= seq_max
        && elapsed >= time_start
        && elapsed <= time_end
}

fn should_trace_rt_sample_window() -> bool {
    static MATCH_COUNT: AtomicU64 = AtomicU64::new(0);

    let skip = parse_trace_u64_env("RUZU_TRACE_RT_SAMPLE_MATCH_SKIP").unwrap_or(0);
    let limit = parse_trace_u64_env("RUZU_TRACE_RT_SAMPLE_MATCH_LIMIT").unwrap_or(u64::MAX);
    let index = MATCH_COUNT.fetch_add(1, Ordering::Relaxed);
    index >= skip && index.saturating_sub(skip) < limit
}

unsafe fn emit_rt_grid_phase(
    phase: u64,
    draw_seq: u64,
    pipeline: u64,
    framebuffer: u32,
    rt_address: u64,
    width: u32,
    height: u32,
) {
    let mut old_read_fb = 0;
    let mut old_pack_buffer = 0;
    let mut old_pack_alignment = 0;
    let mut old_pack_row_length = 0;
    gl::GetIntegerv(gl::READ_FRAMEBUFFER_BINDING, &mut old_read_fb);
    gl::GetIntegerv(gl::PIXEL_PACK_BUFFER_BINDING, &mut old_pack_buffer);
    gl::GetIntegerv(gl::PACK_ALIGNMENT, &mut old_pack_alignment);
    gl::GetIntegerv(gl::PACK_ROW_LENGTH, &mut old_pack_row_length);
    gl::BindFramebuffer(gl::READ_FRAMEBUFFER, framebuffer);
    gl::ReadBuffer(gl::COLOR_ATTACHMENT0);
    gl::BindBuffer(gl::PIXEL_PACK_BUFFER, 0);
    gl::PixelStorei(gl::PACK_ALIGNMENT, 1);
    gl::PixelStorei(gl::PACK_ROW_LENGTH, 0);

    while gl::GetError() != gl::NO_ERROR {}
    let mut hit_cells = 0u64;
    let mut nonzero_bytes = 0u64;
    let mut first_hit_xy = u64::MAX;
    let mut last_hit_xy = u64::MAX;
    let mut first_hit_rgba = 0u64;
    let mut last_hit_rgba = 0u64;
    let pack_xy = |x: u32, y: u32| -> u64 { ((x as u64) << 32) | y as u64 };
    for gy in 0..8u32 {
        for gx in 0..8u32 {
            let x = gx * width.saturating_sub(1) / 7;
            let y = gy * height.saturating_sub(1) / 7;
            let mut px = [0u8; 4];
            gl::ReadPixels(
                x as i32,
                y as i32,
                1,
                1,
                gl::RGBA,
                gl::UNSIGNED_BYTE,
                px.as_mut_ptr() as *mut _,
            );
            let nz = px.iter().filter(|&&byte| byte != 0).count() as u64;
            if nz != 0 {
                let xy = pack_xy(x, y);
                let rgba = u32::from_le_bytes(px) as u64;
                hit_cells += 1;
                nonzero_bytes += nz;
                if first_hit_xy == u64::MAX {
                    first_hit_xy = xy;
                    first_hit_rgba = rgba;
                }
                last_hit_xy = xy;
                last_hit_rgba = rgba;
            }
        }
    }
    let gl_error = gl::GetError();
    if first_hit_xy == u64::MAX {
        first_hit_xy = 0;
        last_hit_xy = 0;
    }
    gl::BindFramebuffer(gl::READ_FRAMEBUFFER, old_read_fb as u32);
    gl::BindBuffer(gl::PIXEL_PACK_BUFFER, old_pack_buffer as u32);
    gl::PixelStorei(gl::PACK_ALIGNMENT, old_pack_alignment);
    gl::PixelStorei(gl::PACK_ROW_LENGTH, old_pack_row_length);

    common::trace::emit_raw(
        common::trace::cat::RT_GRID_PHASE,
        &[
            phase,
            draw_seq,
            pipeline,
            framebuffer as u64,
            rt_address,
            width as u64,
            height as u64,
            hit_cells,
            nonzero_bytes,
            first_hit_xy,
            first_hit_rgba,
            last_hit_xy,
            last_hit_rgba,
            gl_error as u64,
        ],
    );
    // RUZU_TRACE_RT_GRID_STATE=1 — also dump the GL pipeline state that gates
    // fragment output for this draw (diagnoses "draw executes but writes
    // nothing": color mask, blending, depth/stencil/scissor).
    if std::env::var_os("RUZU_TRACE_RT_GRID_STATE").is_some() {
        let mut color_mask = [0i32; 4];
        gl::GetIntegeri_v(gl::COLOR_WRITEMASK, 0, color_mask.as_mut_ptr());
        let blend = gl::IsEnabledi(gl::BLEND, 0);
        let mut blend_src_rgb = 0i32;
        let mut blend_dst_rgb = 0i32;
        let mut blend_src_a = 0i32;
        let mut blend_dst_a = 0i32;
        gl::GetIntegeri_v(gl::BLEND_SRC_RGB, 0, &mut blend_src_rgb);
        gl::GetIntegeri_v(gl::BLEND_DST_RGB, 0, &mut blend_dst_rgb);
        gl::GetIntegeri_v(gl::BLEND_SRC_ALPHA, 0, &mut blend_src_a);
        gl::GetIntegeri_v(gl::BLEND_DST_ALPHA, 0, &mut blend_dst_a);
        let depth_test = gl::IsEnabled(gl::DEPTH_TEST);
        let mut depth_func = 0i32;
        gl::GetIntegerv(gl::DEPTH_FUNC, &mut depth_func);
        let mut depth_mask = 0i32;
        gl::GetIntegerv(gl::DEPTH_WRITEMASK, &mut depth_mask);
        let stencil_test = gl::IsEnabled(gl::STENCIL_TEST);
        let scissor_test = gl::IsEnabledi(gl::SCISSOR_TEST, 0);
        let mut scissor_box = [0i32; 4];
        gl::GetIntegeri_v(gl::SCISSOR_BOX, 0, scissor_box.as_mut_ptr());
        let mut viewport = [0f32; 4];
        gl::GetFloati_v(gl::VIEWPORT, 0, viewport.as_mut_ptr());
        let raster_discard = gl::IsEnabled(gl::RASTERIZER_DISCARD);
        let cull = gl::IsEnabled(gl::CULL_FACE);
        let mut cull_mode = 0i32;
        gl::GetIntegerv(gl::CULL_FACE_MODE, &mut cull_mode);
        let mut front_face = 0i32;
        gl::GetIntegerv(gl::FRONT_FACE, &mut front_face);
        let mut polygon_mode = 0i32;
        gl::GetIntegerv(gl::POLYGON_MODE, &mut polygon_mode);
        let mut depth_clamp = 0u8;
        depth_clamp = gl::IsEnabled(gl::DEPTH_CLAMP);
        eprintln!(
            "[RT_GRID_STATE2] raster_discard={} cull={} cull_mode={:#X} front_face={:#X} \
             polygon_mode={:#X} depth_clamp={}",
            raster_discard, cull, cull_mode, front_face, polygon_mode, depth_clamp
        );
        // RUZU_TRACE_RT_GRID_UBO=1 — dump the first 8 floats of every uniform
        // buffer bound to indices 0-3 (what THIS draw's vertex shader reads —
        // diagnoses degenerate transforms from bad cbuf uploads).
        if std::env::var_os("RUZU_TRACE_RT_GRID_UBO").is_some() {
            for binding in 0..4u32 {
                let mut buf = 0i32;
                gl::GetIntegeri_v(gl::UNIFORM_BUFFER_BINDING, binding, &mut buf);
                if buf == 0 {
                    continue;
                }
                let mut start = 0i64;
                let mut size = 0i64;
                gl::GetInteger64i_v(gl::UNIFORM_BUFFER_START, binding, &mut start);
                gl::GetInteger64i_v(gl::UNIFORM_BUFFER_SIZE, binding, &mut size);
                let mut floats = [0f32; 32];
                let read = (size.max(0) as usize).min(128);
                if read > 0 {
                    gl::GetNamedBufferSubData(
                        buf as u32,
                        start as isize,
                        read as isize,
                        floats.as_mut_ptr() as *mut _,
                    );
                }
                // Also read vec4 indices 33-34 (bytes 528..560) — the sprite
                // rect transform the MK8D world quads read (vs_cbuf3[33/34]).
                let mut hi = [0f32; 8];
                if size >= 560 {
                    gl::GetNamedBufferSubData(
                        buf as u32,
                        start as isize + 528,
                        32,
                        hi.as_mut_ptr() as *mut _,
                    );
                }
                eprintln!(
                    "[RT_GRID_UBO] phase={} draw_seq={} pipeline={} binding={} buf={} start={} size={} f[0..8]={:?} f[132..140]={:?} gl_error=0x{:X}",
                    phase,
                    draw_seq,
                    pipeline,
                    binding,
                    buf,
                    start,
                    size,
                    floats,
                    hi,
                    gl::GetError(),
                );
            }
        }
        // RUZU_TRACE_RT_GRID_UNITS=1 — read the center texel of every texture
        // currently bound to units 0-7 (what THIS draw actually samples).
        if std::env::var_os("RUZU_TRACE_RT_GRID_UNITS").is_some() {
            for unit in 0..8u32 {
                let mut tex = 0i32;
                gl::GetIntegeri_v(gl::TEXTURE_BINDING_2D, unit, &mut tex);
                if tex == 0 {
                    continue;
                }
                let mut w = 0i32;
                let mut h = 0i32;
                gl::GetTextureLevelParameteriv(tex as u32, 0, gl::TEXTURE_WIDTH, &mut w);
                gl::GetTextureLevelParameteriv(tex as u32, 0, gl::TEXTURE_HEIGHT, &mut h);
                let mut px = [0u8; 4];
                if w > 0 && h > 0 {
                    gl::GetTextureSubImage(
                        tex as u32,
                        0,
                        w / 2,
                        h / 2,
                        0,
                        1,
                        1,
                        1,
                        gl::RGBA,
                        gl::UNSIGNED_BYTE,
                        4,
                        px.as_mut_ptr() as *mut _,
                    );
                }
                eprintln!(
                    "[RT_GRID_UNIT] phase={} draw_seq={} unit={} tex={} {}x{} center={:02X?} gl_error=0x{:X}",
                    phase,
                    draw_seq,
                    unit,
                    tex,
                    w,
                    h,
                    px,
                    gl::GetError(),
                );
            }
        }
        // RUZU_TRACE_RT_GRID_TEX=<gl texture id> — also read a few texels of
        // that texture at probe time (diagnoses what a composite draw WOULD
        // sample from the scene RT at this exact point in the GL stream).
        if let Ok(spec) = std::env::var("RUZU_TRACE_RT_GRID_TEX") {
            if let Ok(tex) = spec.trim().parse::<u32>() {
                let mut px = [[0u8; 4]; 3];
                let pts = [(960i32, 540i32), (300, 300), (1600, 800)];
                for (i, (x, y)) in pts.iter().enumerate() {
                    gl::GetTextureSubImage(
                        tex,
                        0,
                        *x,
                        *y,
                        0,
                        1,
                        1,
                        1,
                        gl::RGBA,
                        gl::UNSIGNED_BYTE,
                        4,
                        px[i].as_mut_ptr() as *mut _,
                    );
                }
                eprintln!(
                    "[RT_GRID_TEX] phase={} draw_seq={} tex={} px(960,540)={:02X?} px(300,300)={:02X?} px(1600,800)={:02X?} gl_error=0x{:X}",
                    phase,
                    draw_seq,
                    tex,
                    px[0],
                    px[1],
                    px[2],
                    gl::GetError(),
                );
            }
        }
        eprintln!(
            "[RT_GRID_STATE] phase={} draw_seq={} fbo={} rt0=0x{:X} color_mask={:?} blend={} \
             blend_func=({:#X},{:#X},{:#X},{:#X}) depth_test={} depth_func={:#X} depth_mask={} \
             stencil={} scissor={} scissor_box={:?} viewport={:?}",
            phase,
            draw_seq,
            framebuffer,
            rt_address,
            color_mask,
            blend,
            blend_src_rgb,
            blend_dst_rgb,
            blend_src_a,
            blend_dst_a,
            depth_test,
            depth_func,
            depth_mask,
            stencil_test,
            scissor_test,
            scissor_box,
            viewport,
        );
    }
}

const GL_VERTEX_BINDING_OFFSET: u32 = 0x82D7;
const GL_VERTEX_BINDING_STRIDE: u32 = 0x82D8;

impl RasterizerOpenGL {
    pub fn total_draw_count(&self) -> u64 {
        self.total_draw_count
    }

    fn sync_state(
        draw_view: &mut Maxwell3DDrawView<'_>,
        state_tracker: &mut StateTracker,
        vao: u32,
        has_depth_buffer_float: bool,
        has_viewport_swizzle: bool,
        has_fill_rectangle: bool,
        viewport_scale: f32,
    ) {
        let mut state_tracker = Some(state_tracker);
        sync_viewport(
            draw_view,
            &mut state_tracker,
            has_depth_buffer_float,
            has_viewport_swizzle,
            viewport_scale,
        );
        sync_rasterize_enable(draw_view, &mut state_tracker);
        sync_polygon_modes(draw_view, &mut state_tracker, has_fill_rectangle);
        sync_color_mask(draw_view, &mut state_tracker);
        sync_fragment_color_clamp_state(draw_view, &mut state_tracker);
        sync_multi_sample_state(draw_view, &mut state_tracker);
        sync_depth_test_state(draw_view, &mut state_tracker);
        sync_depth_clamp(draw_view, &mut state_tracker);
        sync_stencil_test_state(draw_view, &mut state_tracker);
        sync_blend_state(draw_view, &mut state_tracker);
        sync_logic_op_state(draw_view, &mut state_tracker);
        sync_cull_mode(draw_view, &mut state_tracker);
        sync_primitive_restart(draw_view, &mut state_tracker);
        sync_scissor_test(draw_view, &mut state_tracker);
        sync_point_state(draw_view, &mut state_tracker, viewport_scale);
        sync_line_state(draw_view, &mut state_tracker);
        sync_polygon_offset(draw_view, &mut state_tracker);
        sync_alpha_test(draw_view, &mut state_tracker);
        sync_framebuffer_srgb(draw_view, &mut state_tracker);
        sync_vertex_formats(vao, draw_view, &mut state_tracker);
        sync_vertex_instances(draw_view, &mut state_tracker);
    }

    /// Create a new rasterizer. Must be called with a current GL context.
    ///
    /// `device_memory` is the single shared `MaxwellDeviceMemoryManager`
    /// from `Host1x::memory_manager()`. Upstream:
    /// `RasterizerOpenGL::RasterizerOpenGL(emu_window, gpu, device_memory, ...)`.
    pub fn new(
        device: &Device,
        syncpoints: Arc<SyncpointManager>,
        device_memory: Arc<crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager>,
        program_manager: ProgramManagerHandle,
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
        let mut state_tracker = Box::new(StateTracker::new());
        Self {
            syncpoints,
            fence_backend: FenceManagerOpenGL::new(),
            fence_manager: FenceManager::new(false),
            frame_count: 0,
            num_queued_commands: 0,
            total_draw_count: 0,
            has_written_global_memory: false,
            buffer_cache,
            program_manager: program_manager.clone(),
            texture_cache: OpenGLTextureCache::new(
                device_memory.clone(),
                device,
                program_manager,
                state_tracker.as_mut(),
            ),
            shader_cache: ShaderCache::new(device_memory),
            gl_shader_cache: OpenGLShaderCache::new(device),
            query_cache: QueryCache::new(),
            state_tracker,
            has_depth_buffer_float: device.has_depth_buffer_float(),
            has_viewport_swizzle: device.has_viewport_swizzle(),
            has_fill_rectangle: device.has_fill_rectangle(),
            invalidate_gpu_cache_callback: None,
            channel_memory_manager: None,
            cpu_memory_reader: None,
            device_memory_reader: None,
            gpu_ticks_getter: None,
            gpu_tick_callback: None,
            transient_vao,
        }
    }

    #[cfg(test)]
    fn new_for_test(syncpoints: Arc<SyncpointManager>) -> Self {
        let test_device_memory = Arc::new(
            crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager::default(),
        );
        let program_manager = ProgramManager::new_shared_with_caps(false, false);
        let mut state_tracker = Box::new(StateTracker::new());
        Self {
            syncpoints,
            fence_backend: FenceManagerOpenGL::new(),
            fence_manager: FenceManager::new(false),
            frame_count: 0,
            num_queued_commands: 0,
            total_draw_count: 0,
            has_written_global_memory: false,
            buffer_cache: CommonBufferCache::new(&OPENGL_DEVICE_TRACKER),
            program_manager: program_manager.clone(),
            texture_cache: OpenGLTextureCache::new_with_caps(
                test_device_memory,
                true,
                false,
                false,
                false,
                program_manager,
                state_tracker.as_mut(),
            ),
            shader_cache: ShaderCache::default(),
            gl_shader_cache: OpenGLShaderCache::new_for_test(),
            query_cache: QueryCache::new(),
            state_tracker,
            has_depth_buffer_float: false,
            has_viewport_swizzle: false,
            has_fill_rectangle: false,
            invalidate_gpu_cache_callback: None,
            channel_memory_manager: None,
            cpu_memory_reader: None,
            device_memory_reader: None,
            gpu_ticks_getter: None,
            gpu_tick_callback: None,
            transient_vao: 0,
        }
    }

    /// Rust adaptation for upstream `RasterizerOpenGL::InvalidateGPUCache()`,
    /// which delegates to the owning `GPU`.
    pub fn set_invalidate_gpu_cache_callback(&mut self, callback: Arc<dyn Fn() + Send + Sync>) {
        self.invalidate_gpu_cache_callback = Some(callback);
    }

    /// Install the compatibility GPU memory reader published by the renderer.
    ///
    /// Actual graphics pipeline creation goes through the shared `ShaderCache`
    /// / `GraphicsEnvironment` owner graph. The reader is still stored here
    /// for compatibility paths that have not yet been fully moved to
    /// channel-owned `MemoryManager` access.
    pub fn set_gpu_memory_reader(&mut self, reader: crate::shader_environment::GpuMemoryReader) {
        self.cpu_memory_reader = Some(Arc::clone(&reader));
        if let Some(mm) = self.channel_memory_manager.as_ref() {
            self.buffer_cache
                .set_gpu_memory(Box::new(GpuMemoryAccessAdapter { mm: Arc::clone(mm) }));
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

    pub fn set_gpu_tick_callback(&mut self, callback: Arc<dyn Fn() + Send + Sync>) {
        self.gpu_tick_callback = Some(callback);
    }

    fn tick_gpu_work(&self) {
        if let Some(callback) = self.gpu_tick_callback.as_ref() {
            callback();
        }
    }

    fn query_fallback(
        &mut self,
        gpu_addr: u64,
        query_type: u32,
        flags: QueryPropertiesFlags,
        mut payload: u32,
        _subreport: u32,
    ) {
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
        if !is_fence
            && query_type == crate::query_cache::types::QueryType::Payload as u32
            && !settings::is_gpu_level_high(&settings::values())
        {
            let mm = mm.lock();
            if has_timeout {
                let gpu_ticks = gpu_ticks_getter
                    .as_ref()
                    .map(|getter| getter())
                    .unwrap_or(0);
                mm.write_block_unsafe(gpu_addr + 8, &gpu_ticks.to_le_bytes());
                mm.write_block_unsafe(gpu_addr, &(payload as u64).to_le_bytes());
            } else {
                mm.write_block_unsafe(gpu_addr, &payload.to_le_bytes());
            }
            return;
        }
        let operation = Box::new(move || {
            let mm = mm.lock();
            if has_timeout {
                let gpu_ticks = gpu_ticks_getter
                    .as_ref()
                    .map(|getter| getter())
                    .unwrap_or(0);
                mm.write_block_unsafe(gpu_addr + 8, &gpu_ticks.to_le_bytes());
                mm.write_block_unsafe(gpu_addr, &(payload as u64).to_le_bytes());
            } else {
                mm.write_block_unsafe(gpu_addr, &payload.to_le_bytes());
            }
        });
        if is_fence {
            RasterizerInterface::signal_fence(self, operation);
        } else {
            RasterizerInterface::sync_operation(self, operation);
        }
    }

    pub fn set_guest_memory_writer(&mut self, writer: GuestMemoryWriter) {
        self.texture_cache.set_guest_memory_writer(writer);
    }

    /// Mutable access to the rasterizer-owned state tracker. Matches upstream
    /// `RendererOpenGL`'s direct access to `rasterizer.state_tracker` member.
    pub fn state_tracker_mut(&mut self) -> &mut StateTracker {
        &mut self.state_tracker
    }

    /// Diagnostic-only present-time readback for GPU addresses selected by
    /// `RUZU_DUMP_PRESENT_EXTRA_GPU_ADDRS`. Called from the present PPM hook
    /// so full texture readbacks happen only on already-selected present
    /// indices instead of during every `AccelerateDisplay` probe.
    pub fn trace_present_images_by_gpu_addr_env(&self, present_index: u64) {
        if let Some(targets) = present_extra_gpu_addr_targets() {
            self.texture_cache
                .trace_present_images_by_gpu_addr(present_index, targets);
        }
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
        let present_extra_targets = present_extra_gpu_addr_targets();
        let dump_present_texture = std::env::var_os("RUZU_DUMP_PRESENT_TEXTURE").is_some();
        let dump_present_texture_ppm =
            std::env::var_os("RUZU_DUMP_PRESENT_TEXTURE_PPM_DIR").is_some();
        let dump_present_extra_ppm = present_extra_targets.is_some()
            && std::env::var_os("RUZU_DUMP_PRESENT_EXTRA_ON_PPM").is_none()
            && std::env::var_os("RUZU_DUMP_PRESENT_EXTRA_PPM_DIR").is_some();
        if dump_present_texture || dump_present_texture_ppm || dump_present_extra_ppm {
            use std::sync::atomic::{AtomicU64, Ordering};
            const GL_TEXTURE_VIEW_MIN_LEVEL: u32 = 0x82DB;
            const GL_TEXTURE_VIEW_NUM_LEVELS: u32 = 0x82DC;
            const GL_TEXTURE_VIEW_MIN_LAYER: u32 = 0x82DD;
            const GL_TEXTURE_VIEW_NUM_LAYERS: u32 = 0x82DE;
            static DUMP_COUNT: AtomicU64 = AtomicU64::new(0);
            let dump_index = DUMP_COUNT.fetch_add(1, Ordering::Relaxed);
            let target_index = std::env::var("RUZU_DUMP_PRESENT_TEXTURE_INDEX")
                .ok()
                .and_then(|value| value.parse::<u64>().ok());
            let should_dump = if let Some(target) = target_index {
                dump_index == target
            } else {
                std::env::var_os("RUZU_DUMP_PRESENT_TEXTURE_ALL").is_some()
                    || dump_index < 8
                    || dump_index.is_power_of_two()
            };
            let should_dump = should_dump
                && (!has_present_texture_trace_window()
                    || should_trace_present_texture_index(dump_index));
            if should_dump {
                if dump_present_texture {
                    let byte_count =
                        (framebuffer_view.width as usize) * (framebuffer_view.height as usize) * 4;
                    let sample_count =
                        if std::env::var_os("RUZU_DUMP_PRESENT_TEXTURE_FULL").is_some() {
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
                    let mut internal_format = 0i32;
                    let mut immutable_format = 0i32;
                    let mut immutable_levels = 0i32;
                    let mut base_level = 0i32;
                    let mut max_level = 0i32;
                    let mut min_filter = 0i32;
                    let mut mag_filter = 0i32;
                    let mut compare_mode = 0i32;
                    let mut view_min_level = 0i32;
                    let mut view_num_levels = 0i32;
                    let mut view_min_layer = 0i32;
                    let mut view_num_layers = 0i32;
                    let mut swizzle = [0i32; 4];
                    unsafe {
                        gl::GetTextureLevelParameteriv(
                            framebuffer_view.display_texture,
                            0,
                            gl::TEXTURE_INTERNAL_FORMAT,
                            &mut internal_format,
                        );
                        gl::GetTextureParameteriv(
                            framebuffer_view.display_texture,
                            gl::TEXTURE_IMMUTABLE_FORMAT,
                            &mut immutable_format,
                        );
                        gl::GetTextureParameteriv(
                            framebuffer_view.display_texture,
                            gl::TEXTURE_IMMUTABLE_LEVELS,
                            &mut immutable_levels,
                        );
                        gl::GetTextureParameteriv(
                            framebuffer_view.display_texture,
                            gl::TEXTURE_BASE_LEVEL,
                            &mut base_level,
                        );
                        gl::GetTextureParameteriv(
                            framebuffer_view.display_texture,
                            gl::TEXTURE_MAX_LEVEL,
                            &mut max_level,
                        );
                        gl::GetTextureParameteriv(
                            framebuffer_view.display_texture,
                            gl::TEXTURE_MIN_FILTER,
                            &mut min_filter,
                        );
                        gl::GetTextureParameteriv(
                            framebuffer_view.display_texture,
                            gl::TEXTURE_MAG_FILTER,
                            &mut mag_filter,
                        );
                        gl::GetTextureParameteriv(
                            framebuffer_view.display_texture,
                            gl::TEXTURE_COMPARE_MODE,
                            &mut compare_mode,
                        );
                        gl::GetTextureParameteriv(
                            framebuffer_view.display_texture,
                            GL_TEXTURE_VIEW_MIN_LEVEL,
                            &mut view_min_level,
                        );
                        gl::GetTextureParameteriv(
                            framebuffer_view.display_texture,
                            GL_TEXTURE_VIEW_NUM_LEVELS,
                            &mut view_num_levels,
                        );
                        gl::GetTextureParameteriv(
                            framebuffer_view.display_texture,
                            GL_TEXTURE_VIEW_MIN_LAYER,
                            &mut view_min_layer,
                        );
                        gl::GetTextureParameteriv(
                            framebuffer_view.display_texture,
                            GL_TEXTURE_VIEW_NUM_LAYERS,
                            &mut view_num_layers,
                        );
                        gl::GetTextureParameteriv(
                            framebuffer_view.display_texture,
                            gl::TEXTURE_SWIZZLE_RGBA,
                            swizzle.as_mut_ptr(),
                        );
                    }
                    log::info!(
                        "[PRESENT_TEXTURE] #{} texture={} level_bytes={} sample={} nonzero={} min={} max={} checksum=0x{:X} ifmt=0x{:X} immutable={} levels={} base={} max_level={} min_filter=0x{:X} mag_filter=0x{:X} compare=0x{:X} view_level={}+{} view_layer={}+{} swizzle=[0x{:X},0x{:X},0x{:X},0x{:X}] first16=[{}]",
                        dump_index,
                        framebuffer_view.display_texture,
                        byte_count,
                        sample_count,
                        nonzero,
                        min,
                        max,
                        checksum,
                        internal_format,
                        immutable_format,
                        immutable_levels,
                        base_level,
                        max_level,
                        min_filter,
                        mag_filter,
                        compare_mode,
                        view_min_level,
                        view_num_levels,
                        view_min_layer,
                        view_num_layers,
                        swizzle[0],
                        swizzle[1],
                        swizzle[2],
                        swizzle[3],
                        first16
                    );
                }
                if let Some(targets) = present_extra_targets {
                    self.texture_cache
                        .trace_present_images_by_gpu_addr(dump_index, targets);
                }
                if dump_present_texture_ppm {
                    unsafe {
                        dump_present_display_texture_ppm(
                            dump_index,
                            framebuffer_addr,
                            framebuffer_view.view_id.index as u64,
                            framebuffer_view.display_texture,
                            framebuffer_view.width,
                            framebuffer_view.height,
                        );
                    }
                }
            }
        }
        let trace_present_texture = common::trace::is_enabled(common::trace::cat::PRESENT_TEXTURE);
        let trace_present_image_metadata =
            common::trace::is_enabled(common::trace::cat::PRESENT_IMAGE_SELECT)
                && present_extra_gpu_addr_targets().is_some();
        if trace_present_texture || trace_present_image_metadata {
            use std::sync::atomic::{AtomicU64, Ordering};
            static PRESENT_TRACE_COUNT: AtomicU64 = AtomicU64::new(0);
            let present_index = PRESENT_TRACE_COUNT.fetch_add(1, Ordering::Relaxed);
            if should_trace_present_texture_index(present_index) {
                if trace_present_texture {
                    unsafe {
                        trace_present_display_texture(
                            present_index,
                            framebuffer_addr,
                            framebuffer_view.view_id.index as u64,
                            framebuffer_view.display_texture,
                            framebuffer_view.width,
                            framebuffer_view.height,
                        );
                    }
                }
                if let Some(targets) = present_extra_gpu_addr_targets() {
                    self.texture_cache
                        .trace_present_images_by_gpu_addr(present_index, targets);
                }
            }
        }
        let resolution = settings::values().resolution_info.clone();
        let scaled_width = if framebuffer_view.scaled {
            resolution.scale_up_u32(framebuffer_view.width)
        } else {
            framebuffer_view.width
        };
        let scaled_height = if framebuffer_view.scaled {
            resolution.scale_up_u32(framebuffer_view.height)
        } else {
            framebuffer_view.height
        };
        Some(FramebufferTextureInfo {
            display_texture: framebuffer_view.display_texture,
            width: framebuffer_view.width,
            height: framebuffer_view.height,
            scaled_width,
            scaled_height,
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
        let buffer_mutex: *const _ = unsafe { &(*buffer_cache).mutex };
        let texture_mutex: *const _ = unsafe { &(*texture_cache).base.mutex };
        lock_two_reentrant_mutexes!(buffer_mutex, texture_mutex, _buffer_lock, _texture_lock);
        unsafe {
            (*texture_cache).should_wait_async_flushes()
                || (*buffer_cache).should_wait_async_flushes()
                || self.query_cache.should_wait_async_flushes()
        }
    }

    fn should_flush_async(&mut self) -> bool {
        let buffer_cache: *mut CommonBufferCache<OpenGLBufferCacheParams, OpenGLDeviceTracker> =
            &mut self.buffer_cache;
        let texture_cache: *mut OpenGLTextureCache = &mut self.texture_cache;
        let buffer_mutex: *const _ = unsafe { &(*buffer_cache).mutex };
        let texture_mutex: *const _ = unsafe { &(*texture_cache).base.mutex };
        lock_two_reentrant_mutexes!(buffer_mutex, texture_mutex, _buffer_lock, _texture_lock);
        unsafe {
            (*texture_cache).has_uncommitted_flushes()
                || (*buffer_cache).has_uncommitted_flushes()
                || self.query_cache.has_uncommitted_flushes()
        }
    }

    fn pop_async_flushes(&mut self) {
        let buffer_cache: *mut CommonBufferCache<OpenGLBufferCacheParams, OpenGLDeviceTracker> =
            &mut self.buffer_cache;
        let texture_cache: *mut OpenGLTextureCache = &mut self.texture_cache;
        let buffer_mutex: *const _ = unsafe { &(*buffer_cache).mutex };
        let texture_mutex: *const _ = unsafe { &(*texture_cache).base.mutex };
        lock_two_reentrant_mutexes!(buffer_mutex, texture_mutex, _buffer_lock, _texture_lock);
        unsafe {
            (*texture_cache).pop_async_flushes();
            (*buffer_cache).pop_async_flushes();
        }
        self.query_cache.pop_async_flushes();
    }

    fn commit_async_flushes(&mut self) {
        let buffer_cache: *mut CommonBufferCache<OpenGLBufferCacheParams, OpenGLDeviceTracker> =
            &mut self.buffer_cache;
        let texture_cache: *mut OpenGLTextureCache = &mut self.texture_cache;
        let buffer_mutex: *const _ = unsafe { &(*buffer_cache).mutex };
        let texture_mutex: *const _ = unsafe { &(*texture_cache).base.mutex };
        lock_two_reentrant_mutexes!(buffer_mutex, texture_mutex, _buffer_lock, _texture_lock);
        unsafe {
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
    /// * `DrawState` is now restricted to upstream `DrawManager::State`
    ///   fields. Maxwell3D register access comes through `Maxwell3DDrawView`
    ///   until the rasterizer can own a live channel/Maxwell3D reference.
    /// * Buffer-cache / texture-cache binding (`buffer_cache.mutex`,
    ///   `texture_cache.base.mutex`, `SyncState`, `SetEngine`) and the actual
    ///   `glDraw*` family are still deferred to step 4+. They require real
    ///   buffer/texture caches and a compiled GL program.
    ///
    /// The `MaxwellToGL::PrimitiveTopology` mapping is intentionally kept
    /// here (not on the pipeline) because upstream re-reads topology on
    /// every `Draw` — a single pipeline key may be drawn with multiple
    /// topologies in successive calls.
    fn draw(&mut self, mut draw_view: Maxwell3DDrawView<'_>, instance_count: u32) {
        // Upstream `RasterizerOpenGL::PrepareDraw` starts with
        // `gpu_memory->FlushCaching()` — flush the CB_DATA invalidation
        // accumulator so per-draw constant-buffer writes reach the buffer
        // cache before the draw consumes them.
        if let Some(mm) = self.channel_memory_manager.as_ref().cloned() {
            mm.lock().flush_caching();
        }
        let draw_state = draw_view.draw_state();
        let gl_debug = GlDrawDebugFlags::get();
        let trace_draw = gl_debug.profile_gl_draw;
        let trace_draw_summary = gl_debug.trace_draw_summary;
        let trace_draw_profile_ring =
            common::trace::is_enabled(common::trace::cat::GL_DRAW_PROFILE);
        let profile_draw_timing = trace_draw || trace_draw_profile_ring;
        let draw_start = profile_draw_timing.then(Instant::now);
        let draw_no = self.num_queued_commands;
        let draw_seq = self.total_draw_count;
        let mut profile_pipeline_us = 0;
        let mut profile_rt_us = 0;
        let mut profile_build_us = 0;
        let mut profile_configure_us = 0;
        let mut profile_update_buffers_us = 0;
        let mut profile_bind_buffers_us = 0;
        let mut profile_sync_draw_us = 0;
        let mut skip_draw_due_to_sampling_addr = false;
        let gpu_tick_callback = self.gpu_tick_callback.as_ref().cloned();
        record_gl_draw_stage(draw_seq, 0);
        trace_gl_draw_stall!(
            "[GL_DRAW_STALL] seq={} enter indexed={} instances={} ib_count={} vb_count={}",
            draw_seq,
            draw_state.draw_indexed,
            instance_count,
            draw_state.index_buffer.count,
            draw_state.vertex_buffer.count
        );
        if trace_draw {
            info!(
                "[GL_DRAW_PROFILE] begin indexed={} instances={} topology={:?} ib_count={} vb_count={} shader_addrs={:X?}",
                draw_state.draw_indexed,
                instance_count,
                draw_state.topology,
                draw_state.index_buffer.count,
                draw_state.vertex_buffer.count,
                draw_view.shader_program_addresses(),
            );
        }

        // Pipeline lookup MUST happen before any FBO/state binding. Upstream
        // (`gl_rasterizer.cpp::PrepareDraw`) does this first: `if (!pipeline)
        // return;` runs before `pipeline->Configure` (which is where the FBO
        // is eventually bound via `state_tracker.BindFramebuffer`). If we bind
        // the draw framebuffer first and then early-return on a pipeline miss,
        // the next swap presents that FBO's stale (often black) texture —
        // observed as MK8D's "1-of-3 flinger buffers stays black" symptom.
        let step = profile_draw_timing.then(Instant::now);
        record_gl_draw_stage(draw_seq, 3);
        trace_gl_draw_stall!("[GL_DRAW_STALL] seq={} before_pipeline", draw_seq);
        let Some(pipeline) = self
            .gl_shader_cache
            .current_graphics_pipeline_with_shared_cache(&mut self.shader_cache)
        else {
            // No pipeline yet — either async compilation is in flight or
            // there is nothing to draw. Upstream silently skips in this case.
            if let Some(callback) = gpu_tick_callback.as_ref() {
                callback();
            }
            debug!("RasterizerOpenGL::draw skipped — no graphics pipeline available");
            return;
        };

        let mut bound_draw_framebuffer = None;
        record_gl_draw_stage(draw_seq, 4);
        trace_gl_draw_stall!("[GL_DRAW_STALL] seq={} after_pipeline", draw_seq);
        if let Some(callback) = gpu_tick_callback.as_ref() {
            callback();
        }
        record_gl_draw_stage(draw_seq, 5);
        trace_gl_draw_stall!("[GL_DRAW_STALL] seq={} after_gpu_tick", draw_seq);
        if trace_draw {
            info!(
                "[GL_DRAW_PROFILE] current_graphics_pipeline_us={}",
                step.map(|start| start.elapsed().as_micros()).unwrap_or(0)
            );
        }
        if let Some(step) = step {
            profile_pipeline_us = trace_elapsed_us(step);
        }

        // Lazy GL-program build (gap 4). The shader cache stages GLSL
        // sources but never calls into GL itself, so the very first time
        // we hit this pipeline with a real GL context we materialise the
        // separable per-stage programs here. Failures are logged once and
        // leave the pipeline in its placeholder state so we don't retry
        // every frame.
        let pipeline_handle_before_build = pipeline.program_pipeline_handle();
        let pipeline_sources_mask =
            pipeline
                .glsl_sources
                .iter()
                .enumerate()
                .fold(0u64, |mask, (index, source)| {
                    if source.as_ref().is_some_and(|source| !source.is_empty()) {
                        mask | (1u64 << index)
                    } else {
                        mask
                    }
                });
        let mut pipeline_build_attempted = false;
        let mut pipeline_build_failed = false;
        if !pipeline.has_gl_programs() && pipeline_sources_mask != 0 {
            pipeline_build_attempted = true;
            let step = profile_draw_timing.then(Instant::now);
            record_gl_draw_stage(draw_seq, 6);
            trace_gl_draw_stall!("[GL_DRAW_STALL] seq={} before_build_programs", draw_seq);
            if let Err((stage_index, msg)) = pipeline.build_from_sources() {
                pipeline_build_failed = true;
                log::warn!(
                    "RasterizerOpenGL::draw: pipeline build failed at stage {}: {}",
                    stage_index,
                    msg
                );
            }
            if trace_draw {
                info!(
                    "[GL_DRAW_PROFILE] build_from_sources_us={} has_programs={}",
                    step.map(|start| start.elapsed().as_micros()).unwrap_or(0),
                    pipeline.has_gl_programs()
                );
            }
            if let Some(step) = step {
                profile_build_us = trace_elapsed_us(step);
            }
            record_gl_draw_stage(draw_seq, 7);
            trace_gl_draw_stall!("[GL_DRAW_STALL] seq={} after_build_programs", draw_seq);
        }

        let is_indexed = draw_state.draw_indexed;
        let pipeline_has_programs = pipeline.has_gl_programs();
        let pipeline_handle_after_build = pipeline.program_pipeline_handle();
        if !pipeline_has_programs {
            // Upstream `PrepareDraw` returns before taking cache locks or
            // binding render targets when `CurrentGraphicsPipeline()` cannot
            // provide a drawable pipeline (missing shader, async build still
            // pending, or compile failure). Ruzu's placeholder pipeline object
            // must behave the same way; otherwise a non-drawing command can
            // still advance framebuffer/texture state and expose stale clear
            // contents on the next present.
            if let Some(callback) = gpu_tick_callback.as_ref() {
                callback();
            }
            debug!("RasterizerOpenGL::draw skipped — graphics pipeline has no GL programs");
            return;
        }
        let step = profile_draw_timing.then(Instant::now);
        // Mirrors upstream `RasterizerOpenGL::PrepareDraw`
        // (gl_rasterizer.cpp:248): after pipeline lookup/build, hold
        // buffer_cache.mutex + texture_cache.mutex through pipeline
        // configuration, cache synchronization, and the draw call. Do not move
        // this above shader-cache lookup/build; upstream does not hold these
        // cache locks while finding the graphics pipeline.
        //
        // Ruzu's channel `MemoryManager` is behind an extra Rust mutex. Acquire
        // it before the cache mutexes so descriptor-table cbuf reads don't
        // deadlock against paths that already hold the memory-manager lock and
        // then enter buffer/texture cache code. Upstream has no equivalent
        // `Tegra::MemoryManager` mutex on `gpu_memory->Read<u32>`.
        let cbuf_memory_manager = self.channel_memory_manager.as_ref().cloned();
        let _lo_chmm = cbuf_memory_manager
            .as_ref()
            .map(|_| common::lock_order::guard("channel_mm"));
        let cbuf_mm_guard = cbuf_memory_manager.as_ref().map(|mm| mm.lock());
        let buffer_mutex: *const _ = &self.buffer_cache.mutex;
        let texture_mutex: *const _ = &self.texture_cache.base.mutex;
        record_gl_draw_stage(draw_seq, 8);
        trace_gl_draw_stall!("[GL_DRAW_STALL] seq={} before_cache_locks", draw_seq);
        let _lo_buf = common::lock_order::guard("buffer_cache");
        let _lo_tex = common::lock_order::guard("texture_cache");
        lock_two_reentrant_mutexes!(
            buffer_mutex,
            texture_mutex,
            _buffer_mutex_guard,
            _texture_mutex_guard
        );
        record_gl_draw_stage(draw_seq, 9);
        trace_gl_draw_stall!("[GL_DRAW_STALL] seq={} after_cache_locks", draw_seq);
        let mut trace_any_samples_rt = false;

        // Mirrors upstream `GraphicsPipeline::ConfigureImpl`
        // (gl_graphics_pipeline.cpp:278-284): the very first thing the
        // pipeline configure does is `texture_cache.SynchronizeGraphicsDescriptors`
        // off `maxwell3d->regs`. Ruzu routes this through the draw view so the
        // backend entry point can move toward upstream's live Maxwell access
        // without a persistent unsafe engine pointer.
        self.texture_cache
            .base
            .synchronize_graphics_descriptors(draw_view.descriptor_sync_regs());
        record_gl_draw_stage(draw_seq, 10);
        trace_gl_draw_stall!("[GL_DRAW_STALL] seq={} after_descriptor_sync", draw_seq);
        pipeline.configure(is_indexed);
        record_gl_draw_stage(draw_seq, 11);
        trace_gl_draw_stall!("[GL_DRAW_STALL] seq={} after_pipeline_configure", draw_seq);
        let descriptor_sync_regs = draw_view.descriptor_sync_regs();
        let uniform_masks = pipeline.enabled_uniform_buffer_masks;
        let uniform_sizes = pipeline.uniform_buffer_sizes;

        self.buffer_cache
            .set_uniform_buffers_state(&uniform_masks, &uniform_sizes);
        self.buffer_cache
            .set_graphics_base_uniform_bindings(&pipeline.base_uniform_bindings);
        self.buffer_cache
            .set_graphics_base_storage_bindings(&pipeline.base_storage_bindings);
        self.buffer_cache
            .set_enable_storage_buffers(pipeline.use_storage_buffers);
        record_gl_draw_stage(draw_seq, 25);
        trace_gl_draw_stall!("[GL_DRAW_STALL] seq={} after_base_bindings", draw_seq);

        // Install the engine-state adapter before descriptor configuration.
        // Upstream `GraphicsPipeline::ConfigureImpl` reads SSBO addresses from
        // `maxwell3d->state.shader_stages[stage].const_buffers` while walking
        // storage descriptors; ruzu's buffer cache needs the same cbuf snapshot
        // before `bind_graphics_storage_buffer` runs.
        self.buffer_cache
            .set_engine_state(Box::new(DrawStateEngineAdapter {
                draw_state: draw_state.clone(),
                registers: draw_view.registers(),
            }));
        record_gl_draw_stage(draw_seq, 14);
        trace_gl_draw_stall!("[GL_DRAW_STALL] seq={} after_set_engine_state", draw_seq);

        // Per-stage texture / image descriptor collection — port of the
        // `config_stage` lambda body in upstream `ConfigureImpl`
        // (gl_graphics_pipeline.cpp:293-376). For each stage's shader info,
        // walk `texture_buffer_descriptors`, `image_buffer_descriptors`,
        // `texture_descriptors`, `image_descriptors`; for each descriptor
        // read `count` 32-bit TIC handles from its cbuf binding via the
        // GPU memory reader; push one `ImageViewInOut` per handle.
        //
        // The cbuf address comes from the draw-view cbuf snapshot. Handle decode
        // splits the raw u32 into (tic_id, tsc_id) using `texture_pair` —
        // when `sampler_binding == ViaHeaderBinding` the two ids collapse.
        //
        // Upstream does this unconditionally as part of
        // `GraphicsPipeline::ConfigureImpl`. Hardened with explicit bounds
        // checks on every index that could come from shader-info
        // (uninitialised or out-of-range u32s would otherwise panic via array
        // indexing and crash through the SIGSEGV handler).
        {
            record_gl_draw_stage(draw_seq, 15);
            trace_gl_draw_stall!("[GL_DRAW_STALL] seq={} before_descriptor_walk", draw_seq);
            const MAX_DESC_COUNT: u32 = 256; // sanity cap (upstream rarely > 16)
            const NUM_STAGES: usize = crate::renderer_opengl::gl_graphics_pipeline::NUM_STAGES;
            let max_cb_slots = crate::engines::maxwell_3d::MAX_CB_SLOTS;
            let cb_bindings = draw_view.cb_bindings();
            let num_shader_stages = cb_bindings.len();
            let trace_texture_descriptors = gl_debug.trace_texture_descriptors;

            let via_header_index = descriptor_sync_regs.sampler_binding_via_header;
            let mut views: Vec<crate::texture_cache::texture_cache_base::ImageViewInOut> =
                Vec::with_capacity(64);
            // Sampler ids parallel to the sampled-texture entries pushed into
            // `views`. Upstream stores these in
            // `std::array<SamplerId, MAX_TEXTURES> samplers` and walks them
            // 1:1 with `texture_descriptors`. Ruzu mirrors with a Vec because
            // descriptor counts are runtime-data, not template-parameterised.
            let mut sampler_ids: Vec<crate::texture_cache::types::SamplerId> =
                Vec::with_capacity(64);
            let mut has_images = false;
            let cbuf_device_reader = self.device_memory_reader.as_ref().cloned();

            let read_handle = |stage: usize, cbuf_index: u32, offset: u32| -> Option<u32> {
                if stage >= num_shader_stages {
                    return None;
                }
                let cbuf_idx = cbuf_index as usize;
                if cbuf_idx >= max_cb_slots {
                    return None;
                }
                let binding = &cb_bindings[stage][cbuf_idx];
                if !binding.enabled {
                    return None;
                }
                let addr = binding.address.checked_add(offset as u64)?;
                let mm = cbuf_mm_guard.as_ref()?;
                let reader = cbuf_device_reader.as_ref()?;
                let detail = ((cbuf_index as u64) << 32) | offset as u64;
                record_gl_draw_stage_detail(draw_seq, 51, stage as u64, detail);
                record_gl_draw_stage_detail(draw_seq, 52, stage as u64, detail);
                let address_valid = mm.gpu_to_cpu_address(addr).is_some();
                record_gl_draw_stage_detail(draw_seq, 53, stage as u64, detail);
                if !address_valid {
                    return None;
                }
                let mut buf = [0u8; 4];
                record_gl_draw_stage_detail(draw_seq, 54, stage as u64, detail);
                record_gl_draw_stage_detail(draw_seq, 55, stage as u64, detail);
                mm.read_block(addr, &mut buf);
                record_gl_draw_stage_detail(draw_seq, 56, stage as u64, detail);
                Some(u32::from_le_bytes(buf))
            };

            // For each descriptor: compute clamped count, then loop. Per-entry
            // index_offset uses checked u32 arithmetic so a malformed shift_left
            // can't silently wrap into a wildly wrong cbuf offset.
            let resolve_handle = |stage: usize,
                                  cbuf_index: u32,
                                  cbuf_offset: u32,
                                  size_shift: u32,
                                  idx: u32|
             -> Option<u32> {
                // `idx << size_shift` — checked shift + checked add.
                let shift = size_shift.min(31);
                let index_offset = idx.checked_shl(shift)?;
                let offset = cbuf_offset.checked_add(index_offset)?;
                read_handle(stage, cbuf_index, offset)
            };

            let resolve_texture_handle = |stage: usize,
                                          has_secondary: bool,
                                          cbuf_index: u32,
                                          cbuf_offset: u32,
                                          shift_left: u32,
                                          secondary_cbuf_index: u32,
                                          secondary_cbuf_offset: u32,
                                          secondary_shift_left: u32,
                                          size_shift: u32,
                                          idx: u32|
             -> Option<u32> {
                let shift = size_shift.min(31);
                let index_offset = idx.checked_shl(shift)?;
                let offset = cbuf_offset.checked_add(index_offset)?;
                if has_secondary {
                    let second_offset = secondary_cbuf_offset.checked_add(index_offset)?;
                    debug_assert!(shift_left < 32);
                    debug_assert!(secondary_shift_left < 32);
                    let lhs = read_handle(stage, cbuf_index, offset)? << shift_left;
                    let rhs = read_handle(stage, secondary_cbuf_index, second_offset)?
                        << secondary_shift_left;
                    Some(lhs | rhs)
                } else {
                    read_handle(stage, cbuf_index, offset)
                }
            };

            for stage in 0..NUM_STAGES.min(num_shader_stages) {
                record_gl_draw_stage_detail(draw_seq, 32, stage as u64, 0);
                let Some(info) = pipeline.stage_infos[stage].as_ref() else {
                    continue;
                };
                self.buffer_cache.unbind_graphics_storage_buffers(stage);
                record_gl_draw_stage_detail(draw_seq, 33, stage as u64, 0);
                record_gl_draw_stage_detail(
                    draw_seq,
                    34,
                    stage as u64,
                    info.storage_buffers_descriptors.len() as u64,
                );
                if pipeline.use_storage_buffers {
                    if gl_debug.trace_ssbo_bind && !info.storage_buffers_descriptors.is_empty() {
                        log::info!(
                            "[SSBO_CONFIG] pipeline={} stage={} descriptors={} base_binding={}",
                            pipeline.program_pipeline_handle(),
                            stage,
                            info.storage_buffers_descriptors.len(),
                            pipeline.base_storage_bindings[stage]
                        );
                    }
                    for (ssbo_index, desc) in info.storage_buffers_descriptors.iter().enumerate() {
                        if desc.count != 1 {
                            log::warn!(
                                "RasterizerOpenGL: storage buffer descriptor count {} is not ported",
                                desc.count
                            );
                            continue;
                        }
                        if let (Some(mm), Some(reader)) =
                            (cbuf_mm_guard.as_ref(), cbuf_device_reader.as_ref())
                        {
                            self.buffer_cache
                                .bind_graphics_storage_buffer_with_gpu_reader(
                                    stage,
                                    ssbo_index,
                                    desc.cbuf_index,
                                    desc.cbuf_offset,
                                    desc.is_written,
                                    |gpu_addr| mm.gpu_to_cpu_address(gpu_addr),
                                    |gpu_addr| mm.get_memory_layout_size(gpu_addr),
                                    |gpu_addr, out| {
                                        mm.read_block(gpu_addr, out);
                                        true
                                    },
                                );
                        } else {
                            self.buffer_cache.bind_graphics_storage_buffer(
                                stage,
                                ssbo_index,
                                desc.cbuf_index,
                                desc.cbuf_offset,
                                desc.is_written,
                            );
                        }
                    }
                }
                record_gl_draw_stage_detail(draw_seq, 35, stage as u64, 0);
                if trace_texture_descriptors {
                    log::warn!(
                        "[TEX_DESC] stage={} texture_buffers={} image_buffers={} textures={} images={}",
                        stage,
                        info.texture_buffer_descriptors.len(),
                        info.image_buffer_descriptors.len(),
                        info.texture_descriptors.len(),
                        info.image_descriptors.len(),
                    );
                }
                // Texture buffer descriptors — handle only (no sampler).
                record_gl_draw_stage_detail(
                    draw_seq,
                    36,
                    stage as u64,
                    info.texture_buffer_descriptors.len() as u64,
                );
                for desc in &info.texture_buffer_descriptors {
                    let count = desc.count.min(MAX_DESC_COUNT);
                    for idx in 0..count {
                        if let Some(raw) = resolve_texture_handle(
                            stage,
                            desc.has_secondary,
                            desc.cbuf_index,
                            desc.cbuf_offset,
                            desc.shift_left,
                            desc.secondary_cbuf_index,
                            desc.secondary_cbuf_offset,
                            desc.secondary_shift_left,
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
                record_gl_draw_stage_detail(draw_seq, 37, stage as u64, 0);
                // Image buffer descriptors — blacklist=false, no sampler. Upstream
                // calls these out separately under `Spec::has_image_buffers`.
                record_gl_draw_stage_detail(
                    draw_seq,
                    38,
                    stage as u64,
                    info.image_buffer_descriptors.len() as u64,
                );
                for desc in &info.image_buffer_descriptors {
                    let count = desc.count.min(MAX_DESC_COUNT);
                    for idx in 0..count {
                        if let Some(raw) = resolve_handle(
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
                record_gl_draw_stage_detail(draw_seq, 39, stage as u64, 0);
                // Sampled texture descriptors. Each handle yields one image-view
                // and one sampler-id. Per upstream (gl_graphics_pipeline.cpp:489):
                // `texture_cache.GetSampler(*(samplers_it++))` — the sampler is
                // resolved from the TSC id alongside the image view from the
                // TIC id, walking parallel arrays.
                record_gl_draw_stage_detail(
                    draw_seq,
                    40,
                    stage as u64,
                    info.texture_descriptors.len() as u64,
                );
                for (desc_index, desc) in info.texture_descriptors.iter().enumerate() {
                    let count = desc.count.min(MAX_DESC_COUNT);
                    for idx in 0..count {
                        let packed_desc = ((desc_index as u64) << 32) | idx as u64;
                        record_gl_draw_stage_detail(draw_seq, 45, stage as u64, packed_desc);
                        let raw = resolve_texture_handle(
                            stage,
                            desc.has_secondary,
                            desc.cbuf_index,
                            desc.cbuf_offset,
                            desc.shift_left,
                            desc.secondary_cbuf_index,
                            desc.secondary_cbuf_offset,
                            desc.secondary_shift_left,
                            desc.size_shift,
                            idx,
                        );
                        record_gl_draw_stage_detail(draw_seq, 46, stage as u64, packed_desc);
                        if trace_texture_descriptors {
                            let binding = cb_bindings.get(stage).and_then(|stage_bindings| {
                                stage_bindings.get(desc.cbuf_index as usize)
                            });
                            log::warn!(
                                "[TEX_DESC] sampled stage={} idx={} cbuf={} offset=0x{:X} shift={} count={} cbuf_enabled={} cbuf_addr=0x{:X} raw={:?}",
                                stage,
                                idx,
                                desc.cbuf_index,
                                desc.cbuf_offset,
                                desc.size_shift,
                                desc.count,
                                binding.map(|b| b.enabled).unwrap_or(false),
                                binding.map(|b| b.address).unwrap_or(0),
                                raw,
                            );
                        }
                        if let Some(raw) = raw {
                            let (tic_id, tsc_id) =
                                crate::textures::texture::texture_pair(raw, via_header_index);
                            record_gl_draw_stage_detail(draw_seq, 47, stage as u64, packed_desc);
                            views.push(crate::texture_cache::texture_cache_base::ImageViewInOut {
                                index: tic_id,
                                blacklist: false,
                                id: Default::default(),
                            });
                            // Resolve the TSC handle through the texture
                            // cache's per-index descriptor table, in lock-step
                            // with the TIC view so the bind loop can walk both
                            // arrays in parallel like upstream.
                            record_gl_draw_stage_detail(draw_seq, 48, stage as u64, packed_desc);
                            let sampler_id =
                                self.texture_cache.base.get_graphics_sampler_id(tsc_id);
                            record_gl_draw_stage_detail(draw_seq, 49, stage as u64, packed_desc);
                            sampler_ids.push(sampler_id);
                            record_gl_draw_stage_detail(draw_seq, 50, stage as u64, packed_desc);
                        }
                    }
                }
                record_gl_draw_stage_detail(draw_seq, 41, stage as u64, 0);
                // Image (storage image) descriptors. Upstream tags
                // blacklist = desc.is_written so written-to render targets
                // can be detected and scaled down.
                record_gl_draw_stage_detail(
                    draw_seq,
                    42,
                    stage as u64,
                    info.image_descriptors.len() as u64,
                );
                for desc in &info.image_descriptors {
                    has_images = true;
                    let count = desc.count.min(MAX_DESC_COUNT);
                    for idx in 0..count {
                        if let Some(raw) = resolve_handle(
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
                record_gl_draw_stage_detail(draw_seq, 43, stage as u64, 0);
                record_gl_draw_stage_detail(draw_seq, 44, stage as u64, 0);
            }
            record_gl_draw_stage(draw_seq, 16);
            trace_gl_draw_stall!("[GL_DRAW_STALL] seq={} after_descriptor_walk", draw_seq);
            // Mirror upstream `FillGraphicsImageViews<has_images>(views)` from
            // gl_graphics_pipeline.cpp:380. `has_blacklists = has_images` per
            // upstream's `Spec::has_images` template parameter.
            self.texture_cache
                .fill_graphics_image_views(&mut views, has_images);
            record_gl_draw_stage(draw_seq, 17);
            trace_gl_draw_stall!("[GL_DRAW_STALL] seq={} after_fill_image_views", draw_seq);

            // Upstream `prepare_stage` calls `PrepareImageView` before
            // binding every sampled image view. Ruzu has already resolved the
            // view ids here, so prepare the parent image ids through the
            // OpenGL-backed bridge before materialising GL views.
            let mut image_ids = Vec::new();
            let mut seen = std::collections::HashSet::new();
            for view in &views {
                if !view.id.is_valid() {
                    continue;
                }
                let view_base = self.texture_cache.base.slot_image_views.get(view.id);
                let image_id = view_base.image_id;
                if image_id.is_valid() && seen.insert(image_id) {
                    image_ids.push(image_id);
                }
            }
            for image_id in image_ids {
                self.texture_cache
                    .prepare_image_without_gpu_reader(image_id, false, false);
            }
            record_gl_draw_stage(draw_seq, 18);
            trace_gl_draw_stall!("[GL_DRAW_STALL] seq={} after_prepare_images", draw_seq);

            // Slice 11: materialise the GL-side `Image` + `ImageView` for every
            // view-id that `fill_graphics_image_views` produced. Upstream does
            // this inside `slot_image_views.insert(runtime, info, image_id,
            // image, slot_images)` because the slot pool stores the backend
            // type directly; ruzu's base pool stores `ImageViewBase`, so the
            // GL wrapper lazily mirrors the slots into its HashMaps here.
            // Required before any future `glBindTextureUnit` step can resolve
            // a view-id to a real GL texture name.
            self.texture_cache.materialize_views(&views);
            record_gl_draw_stage(draw_seq, 19);
            trace_gl_draw_stall!("[GL_DRAW_STALL] seq={} after_materialize_views", draw_seq);
            // Slice 13: materialise the GL `Sampler` objects for the same
            // batch of sampled-texture descriptors. Mirrors upstream's
            // `slot_samplers[id]` access in `prepare_stage` — the only
            // difference is timing (upstream populates lazily inside
            // FindSampler; ruzu separates id-alloc from backend ctor).
            self.texture_cache.materialize_samplers(&sampler_ids);
            record_gl_draw_stage(draw_seq, 20);
            trace_gl_draw_stall!(
                "[GL_DRAW_STALL] seq={} after_materialize_samplers",
                draw_seq
            );

            // Upstream `GraphicsPipeline::ConfigureImpl` performs
            // `FillGraphicsImageViews` before `UpdateRenderTargets`; keep the
            // same ordering so sampled-image alias synchronization observes the
            // source image before a render-target prepare can synchronize stale
            // parent contents back into an aliased child mip.
            if let Some(mm) = cbuf_mm_guard.as_ref() {
                let rt_step = profile_draw_timing.then(Instant::now);
                let render_targets = draw_view.render_targets();
                record_gl_draw_stage(draw_seq, 1);
                trace_gl_draw_stall!("[GL_DRAW_STALL] seq={} before_rt_prepare", draw_seq);
                self.texture_cache
                    .update_render_targets_from_snapshot(&render_targets, |gpu_addr| {
                        mm.gpu_to_cpu_address(gpu_addr)
                    });
                self.texture_cache.prepare_render_targets_from_snapshot(
                    &render_targets,
                    None,
                    false,
                    None,
                );
                let surface_clip = draw_view.surface_clip();
                bound_draw_framebuffer = self
                    .texture_cache
                    .framebuffer_for_render_targets_from_snapshot(
                        &render_targets,
                        crate::texture_cache::types::Extent2D {
                            width: surface_clip.width,
                            height: surface_clip.height,
                        },
                    );
                record_gl_draw_stage(draw_seq, 2);
                trace_gl_draw_stall!("[GL_DRAW_STALL] seq={} after_rt_prepare", draw_seq);
                if trace_draw {
                    info!(
                        "[GL_DRAW_PROFILE] update_render_targets_us={}",
                        rt_step
                            .map(|start| start.elapsed().as_micros())
                            .unwrap_or(0)
                    );
                }
                if let Some(rt_step) = rt_step {
                    profile_rt_us = trace_elapsed_us(rt_step);
                }
            } else if gl_debug.trace_rt {
                log::info!("[RT] miss no_channel_memory_manager");
            }
            if let Some((framebuffer, width, height)) = bound_draw_framebuffer {
                unsafe {
                    gl::BindFramebuffer(gl::DRAW_FRAMEBUFFER, framebuffer);
                }
                static RT_BIND_SEQ_MIN: OnceLock<Option<u64>> = OnceLock::new();
                static RT_BIND_SEQ_MAX: OnceLock<Option<u64>> = OnceLock::new();
                static RT_BIND_TIME_START: OnceLock<Option<u64>> = OnceLock::new();
                static RT_BIND_TIME_END: OnceLock<Option<u64>> = OnceLock::new();
                let trace_rt_bind_seq_min =
                    trace_u64_env_cached(&RT_BIND_SEQ_MIN, "RUZU_TRACE_RT_BIND_SEQ_MIN")
                        .unwrap_or(0);
                let trace_rt_bind_seq_max =
                    trace_u64_env_cached(&RT_BIND_SEQ_MAX, "RUZU_TRACE_RT_BIND_SEQ_MAX")
                        .unwrap_or(u64::MAX);
                let trace_rt_bind_time_start =
                    trace_u64_env_cached(&RT_BIND_TIME_START, "RUZU_TRACE_RT_BIND_TIME_START_MS")
                        .unwrap_or(0);
                let trace_rt_bind_time_end =
                    trace_u64_env_cached(&RT_BIND_TIME_END, "RUZU_TRACE_RT_BIND_TIME_END_MS")
                        .unwrap_or(u64::MAX);
                let trace_rt_bind_elapsed = trace_elapsed_ms();
                if (common::trace::is_enabled(common::trace::cat::RT_BIND)
                    || common::trace::is_enabled(common::trace::cat::RT_ZETA_BIND))
                    && draw_seq >= trace_rt_bind_seq_min
                    && draw_seq <= trace_rt_bind_seq_max
                    && trace_rt_bind_elapsed >= trace_rt_bind_time_start
                    && trace_rt_bind_elapsed <= trace_rt_bind_time_end
                {
                    let render_targets = draw_view.render_targets();
                    let surface_clip = draw_view.surface_clip();
                    let pack_size = |w: u32, h: u32| -> u64 { ((w as u64) << 32) | h as u64 };
                    let pack_rt_map = || -> u64 {
                        let mut packed = 0u64;
                        for (index, &target) in
                            render_targets.rt_control.map.iter().take(8).enumerate()
                        {
                            packed |= (target as u64 & 0xff) << (index * 8);
                        }
                        packed
                    };
                    let rt0 = render_targets.render_targets[0];
                    let rt1 = render_targets.render_targets[1];
                    if common::trace::is_enabled(common::trace::cat::RT_BIND)
                        && should_trace_rt_bind_address(rt0.address)
                    {
                        common::trace::emit_raw(
                            common::trace::cat::RT_BIND,
                            &[
                                draw_seq,
                                pipeline.program_pipeline_handle() as u64,
                                framebuffer as u64,
                                width as u64,
                                height as u64,
                                render_targets.rt_control.count as u64,
                                pack_rt_map(),
                                rt0.address,
                                rt0.format as u64,
                                pack_size(rt0.width, rt0.height),
                                rt1.address,
                                rt1.format as u64,
                                pack_size(rt1.width, rt1.height),
                                pack_size(surface_clip.width, surface_clip.height),
                            ],
                        );
                        common::trace::emit_raw(
                            common::trace::cat::RT_BIND,
                            &[
                                u64::MAX,
                                draw_seq,
                                pipeline_handle_before_build as u64,
                                pipeline_handle_after_build as u64,
                                pipeline_sources_mask,
                                pipeline_has_programs as u64,
                                (pipeline_has_programs && self.transient_vao != 0) as u64,
                                rt0.address,
                                rt0.format as u64,
                                ((width as u64) << 32) | height as u64,
                                framebuffer as u64,
                                pipeline_build_attempted as u64,
                                pipeline_build_failed as u64,
                            ],
                        );
                    }
                    let pipeline_handle = pipeline.program_pipeline_handle() as u64;
                    if common::trace::is_enabled(common::trace::cat::RT_ZETA_BIND)
                        && should_trace_rt_zeta_pipeline(pipeline_handle)
                    {
                        let zeta = render_targets.zeta;
                        let depth_stencil = draw_view.depth_stencil();
                        common::trace::emit_raw(
                            common::trace::cat::RT_ZETA_BIND,
                            &[
                                draw_seq,
                                pipeline_handle,
                                framebuffer as u64,
                                zeta.enabled as u64,
                                zeta.address,
                                zeta.format as u64,
                                pack_size(zeta.width, zeta.height),
                                rt0.address,
                                rt0.format as u64,
                                pack_size(rt0.width, rt0.height),
                                depth_stencil.depth_test_enable as u64,
                                depth_stencil.depth_write_enable as u64,
                                depth_stencil.depth_func as u64,
                                depth_stencil.depth_mode as u64,
                            ],
                        );
                    }
                }
                if trace_draw || gl_debug.trace_rt {
                    info!(
                        "[GL_DRAW_PROFILE] bind_draw_framebuffer framebuffer={} {}x{}",
                        framebuffer, width, height
                    );
                }
            } else if gl_debug.trace_rt {
                info!("[RT] draw no framebuffer bound");
            }
            trace_any_samples_rt = gl_debug.trace_any_samples_passed
                && common::trace::is_enabled(common::trace::cat::RT_BIND)
                && bound_draw_framebuffer.is_some()
                && {
                    let rt0 = draw_view.render_targets().render_targets[0];
                    rt0.address != 0 && should_trace_rt_bind_address(rt0.address)
                };

            // Slice 12: collect GL texture handles for the sampled-texture
            // descriptors and bulk-bind via `glBindTextures` — port of upstream
            // `prepare_stage` body (gl_graphics_pipeline.cpp:456-535) + the
            // final `glBindTextures(0, texture_binding, textures.data())`
            // call at line 553. Iterates `views` in the same order they were
            // pushed: skip `num_texture_buffers + num_image_buffers` per
            // stage (those bind through the buffer cache via
            // `BindGraphicsTextureBuffer`), then walk `texture_descriptors`.
            // Storage-image binding (`info.image_descriptors`) and per-unit
            // sampler binding are deferred to follow-up slices.
            const MAX_TEXTURES: usize =
                crate::renderer_opengl::gl_graphics_pipeline::MAX_TEXTURES as usize;
            const MAX_IMAGES: usize =
                crate::renderer_opengl::gl_graphics_pipeline::MAX_IMAGES as usize;
            let mut textures: [u32; MAX_TEXTURES] = [0; MAX_TEXTURES];
            // Parallel sampler-handle scratch buffer (Slice 13). Upstream uses
            // `std::array<GLuint, MAX_TEXTURES> gl_samplers` and asserts
            // `texture_binding == sampler_binding` at the end.
            let mut gl_samplers: [u32; MAX_TEXTURES] = [0; MAX_TEXTURES];
            let mut bound_texture_view_ids: [crate::texture_cache::types::ImageViewId;
                MAX_TEXTURES] = [crate::texture_cache::types::NULL_IMAGE_VIEW_ID; MAX_TEXTURES];
            // Storage-image handle scratch (Slice 14). Mirrors upstream
            // `std::array<GLuint, MAX_IMAGES> images`.
            let mut images: [u32; MAX_IMAGES] = [0; MAX_IMAGES];
            let mut texture_binding: usize = 0;
            let mut image_binding: usize = 0;
            let mut sampler_it: usize = 0;
            let mut views_it: usize = 0;
            let trace_texture_bind = common::trace::is_enabled(common::trace::cat::TEXTURE_BIND);
            let trace_texture_bind_addr =
                common::trace::is_enabled(common::trace::cat::TEXTURE_BIND_ADDR);
            let trace_texture_bind_any = trace_texture_bind || trace_texture_bind_addr;
            let (trace_texture_bind_pipeline, trace_texture_bind_in_window) =
                if trace_texture_bind_any {
                    static TEXTURE_BIND_PIPELINE: OnceLock<Option<u64>> = OnceLock::new();
                    static TEXTURE_BIND_SEQ_MIN: OnceLock<Option<u64>> = OnceLock::new();
                    static TEXTURE_BIND_SEQ_MAX: OnceLock<Option<u64>> = OnceLock::new();
                    static TEXTURE_BIND_TIME_START: OnceLock<Option<u64>> = OnceLock::new();
                    static TEXTURE_BIND_TIME_END: OnceLock<Option<u64>> = OnceLock::new();
                    let pipeline_filter = trace_u64_env_cached(
                        &TEXTURE_BIND_PIPELINE,
                        "RUZU_TRACE_TEXTURE_BIND_PIPELINE",
                    )
                    .map(|value| value as u32);
                    let seq_min = trace_u64_env_cached(
                        &TEXTURE_BIND_SEQ_MIN,
                        "RUZU_TRACE_TEXTURE_BIND_SEQ_MIN",
                    )
                    .unwrap_or(0);
                    let seq_max = trace_u64_env_cached(
                        &TEXTURE_BIND_SEQ_MAX,
                        "RUZU_TRACE_TEXTURE_BIND_SEQ_MAX",
                    )
                    .unwrap_or(u64::MAX);
                    let time_start = trace_u64_env_cached(
                        &TEXTURE_BIND_TIME_START,
                        "RUZU_TRACE_TEXTURE_BIND_TIME_START_MS",
                    )
                    .unwrap_or(0);
                    let time_end = trace_u64_env_cached(
                        &TEXTURE_BIND_TIME_END,
                        "RUZU_TRACE_TEXTURE_BIND_TIME_END_MS",
                    )
                    .unwrap_or(u64::MAX);
                    let elapsed = trace_elapsed_ms();
                    (
                        pipeline_filter,
                        draw_seq >= seq_min
                            && draw_seq <= seq_max
                            && elapsed >= time_start
                            && elapsed <= time_end,
                    )
                } else {
                    (None, false)
                };
            let trace_texture_bind_for_pipeline = trace_texture_bind
                && trace_texture_bind_in_window
                && match trace_texture_bind_pipeline {
                    Some(target) => target == pipeline.program_pipeline_handle(),
                    None => true,
                };
            let trace_texture_bind_addr_for_pipeline = trace_texture_bind_addr
                && trace_texture_bind_in_window
                && match trace_texture_bind_pipeline {
                    Some(target) => target == pipeline.program_pipeline_handle(),
                    None => true,
                };
            for stage in 0..NUM_STAGES.min(num_shader_stages) {
                let Some(info) = pipeline.stage_infos[stage].as_ref() else {
                    continue;
                };
                // Slice 17: port of upstream bind_stage_info
                // (gl_graphics_pipeline.cpp:386-420). For texture-buffer and
                // image-buffer descriptors, feed the view's gpu_addr / size /
                // format into the buffer cache so its UpdateGraphicsBuffers
                // pass binds the right TBO. `UnbindGraphicsTextureBuffers`
                // first to reset stale slots from a prior pipeline.
                self.buffer_cache.unbind_graphics_texture_buffers(stage);
                let mut tbo_index: u32 = 0;
                // Texture buffers come first in the views[] order (Slice 6).
                for desc in &info.texture_buffer_descriptors {
                    let count = desc.count.min(MAX_DESC_COUNT);
                    for _ in 0..count {
                        if views_it >= views.len() {
                            break;
                        }
                        let view_id = views[views_it].id;
                        views_it += 1;
                        if view_id.is_valid() {
                            let view_base = self
                                .texture_cache
                                .base
                                .slot_image_views
                                .get(view_id)
                                .clone();
                            let internal_format =
                                super::gl_texture_cache::present_internal_format(view_base.format);
                            self.buffer_cache.bind_graphics_texture_buffer(
                                stage,
                                tbo_index as usize,
                                view_base.gpu_addr,
                                view_base.size.width,
                                internal_format,
                                false, // is_written: sampled buffers are read-only
                                false, // is_image: false for texture buffers
                            );
                        }
                        if texture_binding < MAX_TEXTURES {
                            textures[texture_binding] = 0;
                            gl_samplers[texture_binding] = 0;
                            bound_texture_view_ids[texture_binding] = view_id;
                            texture_binding += 1;
                        }
                        tbo_index += 1;
                    }
                }
                // Image buffers follow — same flow, is_image = true.
                for desc in &info.image_buffer_descriptors {
                    let count = desc.count.min(MAX_DESC_COUNT);
                    for _ in 0..count {
                        if views_it >= views.len() {
                            break;
                        }
                        let view_id = views[views_it].id;
                        views_it += 1;
                        if view_id.is_valid() {
                            let view_base = self
                                .texture_cache
                                .base
                                .slot_image_views
                                .get(view_id)
                                .clone();
                            let internal_format =
                                super::gl_texture_cache::present_internal_format(view_base.format);
                            self.buffer_cache.bind_graphics_texture_buffer(
                                stage,
                                tbo_index as usize,
                                view_base.gpu_addr,
                                view_base.size.width,
                                internal_format,
                                desc.is_written,
                                true,
                            );
                        }
                        if image_binding < MAX_IMAGES {
                            images[image_binding] = 0;
                            image_binding += 1;
                        }
                        tbo_index += 1;
                    }
                }

                for desc in &info.texture_descriptors {
                    let count = desc.count.min(MAX_DESC_COUNT);
                    for _ in 0..count {
                        if views_it >= views.len() || texture_binding >= MAX_TEXTURES {
                            break;
                        }
                        let view_id = views[views_it].id;
                        views_it += 1;
                        // `materialize_views` only inserts entries with
                        // valid view_ids; missing entries → bind 0 so the
                        // GL state is well-defined (sample produces black /
                        // undefined).
                        let (handle, view_supports_aniso) = self
                            .texture_cache
                            .get_image_view(view_id)
                            .map(|iv| {
                                (
                                    iv.handle_for_texture_type(desc.texture_type),
                                    iv.supports_anisotropy(),
                                )
                            })
                            .unwrap_or((0, false));
                        if trace_texture_bind_addr_for_pipeline {
                            let (image_id, view_gpu_addr, view_width, view_height) = if view_id
                                .is_valid()
                            {
                                let view = self.texture_cache.base.slot_image_views.get(view_id);
                                (
                                    view.image_id.index as u64,
                                    view.gpu_addr,
                                    view.size.width as u64,
                                    view.size.height as u64,
                                )
                            } else {
                                (u64::MAX, 0, 0, 0)
                            };
                            if should_trace_texture_bind_address(view_gpu_addr) {
                                common::trace::emit_raw(
                                    common::trace::cat::TEXTURE_BIND_ADDR,
                                    &[
                                        draw_seq as u64,
                                        pipeline.program_pipeline_handle() as u64,
                                        stage as u64,
                                        texture_binding as u64,
                                        view_id.index as u64,
                                        image_id,
                                        view_gpu_addr,
                                        view_width,
                                        view_height,
                                        handle as u64,
                                        desc.texture_type as u64,
                                        desc.is_depth as u64,
                                        desc.is_multisample as u64,
                                    ],
                                );
                            }
                        }
                        if view_id.is_valid() {
                            let view = self.texture_cache.base.slot_image_views.get(view_id);
                            if should_skip_draw_sampling_gpu_addr(view.gpu_addr) {
                                skip_draw_due_to_sampling_addr = true;
                            }
                            if should_trace_texture_grid_address(view.gpu_addr, draw_seq) {
                                unsafe {
                                    trace_texture_grid_sample(
                                        draw_seq as u64,
                                        pipeline.program_pipeline_handle() as u64,
                                        stage as u64,
                                        texture_binding as u64,
                                        view_id.index as u64,
                                        view.image_id.index as u64,
                                        view.gpu_addr,
                                        view.format as u64,
                                        view.view_type as u64,
                                        view.swizzle,
                                        handle,
                                    );
                                }
                            }
                        }
                        if trace_texture_bind_for_pipeline {
                            let mut width = 0i32;
                            let mut height = 0i32;
                            let mut depth = 0i32;
                            let mut sample0 = [0u8; 4];
                            let mut sample_mid = [0u8; 4];
                            if handle != 0 {
                                unsafe {
                                    gl::GetTextureLevelParameteriv(
                                        handle,
                                        0,
                                        gl::TEXTURE_WIDTH,
                                        &mut width,
                                    );
                                    gl::GetTextureLevelParameteriv(
                                        handle,
                                        0,
                                        gl::TEXTURE_HEIGHT,
                                        &mut height,
                                    );
                                    gl::GetTextureLevelParameteriv(
                                        handle,
                                        0,
                                        gl::TEXTURE_DEPTH,
                                        &mut depth,
                                    );
                                    if width > 0 && height > 0 {
                                        gl::GetTextureSubImage(
                                            handle,
                                            0,
                                            0,
                                            0,
                                            0,
                                            1,
                                            1,
                                            1,
                                            gl::RGBA,
                                            gl::UNSIGNED_BYTE,
                                            4,
                                            sample0.as_mut_ptr().cast(),
                                        );
                                        gl::GetTextureSubImage(
                                            handle,
                                            0,
                                            width / 2,
                                            height / 2,
                                            0,
                                            1,
                                            1,
                                            1,
                                            gl::RGBA,
                                            gl::UNSIGNED_BYTE,
                                            4,
                                            sample_mid.as_mut_ptr().cast(),
                                        );
                                    }
                                }
                            }
                            let (view_type, image_id, view_gpu_addr) = if view_id.is_valid() {
                                let view = self.texture_cache.base.slot_image_views.get(view_id);
                                (
                                    view.view_type as u64,
                                    view.image_id.index as u64,
                                    view.gpu_addr,
                                )
                            } else {
                                (u64::MAX, u64::MAX, 0)
                            };
                            let pack_rgba = |px: [u8; 4]| -> u64 { u32::from_le_bytes(px) as u64 };
                            common::trace::emit_raw(
                                common::trace::cat::TEXTURE_BIND,
                                &[
                                    draw_seq as u64,
                                    pipeline.program_pipeline_handle() as u64,
                                    stage as u64,
                                    texture_binding as u64,
                                    desc.texture_type as u64,
                                    view_id.index as u64,
                                    view_type,
                                    image_id,
                                    handle as u64,
                                    width.max(0) as u64,
                                    height.max(0) as u64,
                                    depth.max(0) as u64,
                                    pack_rgba(sample0),
                                    pack_rgba(sample_mid),
                                ],
                            );
                        }
                        textures[texture_binding] = handle;
                        bound_texture_view_ids[texture_binding] = view_id;

                        // Slice 13/15: parallel sampler resolution with
                        // anisotropy fallback. Upstream `prepare_stage`
                        // (gl_graphics_pipeline.cpp:489-493) picks the
                        // descriptor's added-anisotropy fallback handle when
                        // the texture format cannot carry the configured
                        // anisotropy — without the gate, MSAA-resolve or
                        // single-channel formats would be over-filtered.
                        let sampler_handle = if sampler_it < sampler_ids.len() {
                            let sid = sampler_ids[sampler_it];
                            sampler_it += 1;
                            self.texture_cache
                                .get_sampler(sid)
                                .map(|s| {
                                    let use_fallback =
                                        s.has_added_anisotropy() && !view_supports_aniso;
                                    if use_fallback {
                                        s.handle_with_default_anisotropy()
                                    } else {
                                        s.handle()
                                    }
                                })
                                .unwrap_or(0)
                        } else {
                            0
                        };
                        gl_samplers[texture_binding] = sampler_handle;
                        bound_texture_view_ids[texture_binding] = view_id;

                        texture_binding += 1;
                    }
                }
                // Storage-image (image-load/store) descriptors — port of
                // upstream prepare_stage image loop (gl_graphics_pipeline.cpp
                // :496-509). Each entry resolves to a per-format `storage_view`
                // handle returned by `ImageView::StorageView(type, format)`.
                // Writable descriptors should call `MarkModification(image_id)`
                // upstream — deferred until cache write-tracking is wired.
                for desc in &info.image_descriptors {
                    let count = desc.count.min(MAX_DESC_COUNT);
                    for _ in 0..count {
                        if views_it >= views.len() || image_binding >= MAX_IMAGES {
                            break;
                        }
                        let view_id = views[views_it].id;
                        views_it += 1;
                        // Slice 16: written-to storage images need the
                        // backing image's modification_tick bumped so the
                        // cache's download/flush logic knows it diverged
                        // from the guest copy (upstream:
                        // gl_graphics_pipeline.cpp:499-501).
                        if desc.is_written && view_id.is_valid() {
                            let parent_id = self
                                .texture_cache
                                .base
                                .slot_image_views
                                .get(view_id)
                                .image_id;
                            if parent_id.is_valid() {
                                self.texture_cache.base.mark_modification_by_id(parent_id);
                            }
                        }
                        let handle = self
                            .texture_cache
                            .get_image_view_mut(view_id)
                            .map(|iv| iv.storage_view(desc.texture_type, desc.format))
                            .unwrap_or(0);
                        images[image_binding] = handle;
                        image_binding += 1;
                    }
                }
                if info.uses_render_area {
                    let surface_clip = draw_view.surface_clip();
                    let program = pipeline.source_programs[stage];
                    if program != 0 {
                        unsafe {
                            gl::ProgramUniform4f(
                                program,
                                1,
                                surface_clip.width as f32,
                                surface_clip.height as f32,
                                0.0,
                                0.0,
                            );
                        }
                    }
                }
            }
            record_gl_draw_stage(draw_seq, 21);
            trace_gl_draw_stall!(
                "[GL_DRAW_STALL] seq={} after_descriptor_bind_setup",
                draw_seq
            );
            if texture_binding != 0 {
                // Upstream asserts `texture_binding == sampler_binding` and
                // batches both. With Slice 13, ruzu now does the same — the
                // sampler_it / texture_binding lockstep above guarantees
                // matching counts as long as `materialize_samplers` keeps up.
                if gl_debug.trace_bind_textures {
                    use std::sync::atomic::{AtomicBool, Ordering};
                    static ONCE: AtomicBool = AtomicBool::new(false);
                    if !ONCE.swap(true, Ordering::Relaxed) {
                        let non_zero_tex = textures[..texture_binding]
                            .iter()
                            .filter(|&&h| h != 0)
                            .count();
                        let non_zero_smp = gl_samplers[..texture_binding]
                            .iter()
                            .filter(|&&h| h != 0)
                            .count();
                        log::warn!(
                            "[GL_BIND_TEX] first bind: count={} tex_non_zero={} smp_non_zero={}",
                            texture_binding,
                            non_zero_tex,
                            non_zero_smp,
                        );
                    }
                }
                if gl_debug.dump_bound_textures {
                    use std::sync::atomic::{AtomicUsize, Ordering};
                    static DUMPS: AtomicUsize = AtomicUsize::new(0);
                    let dump_pipeline = std::env::var("RUZU_DUMP_BOUND_TEXTURES_PIPELINE")
                        .ok()
                        .and_then(|value| value.parse::<u32>().ok());
                    let dump_seq_min = std::env::var("RUZU_DUMP_BOUND_TEXTURES_SEQ_MIN")
                        .ok()
                        .and_then(|value| value.parse::<u64>().ok())
                        .unwrap_or(0);
                    let dump_limit = std::env::var("RUZU_DUMP_BOUND_TEXTURES_LIMIT")
                        .ok()
                        .and_then(|value| value.parse::<usize>().ok())
                        .unwrap_or(8);
                    static DUMP_ADDRS: OnceLock<Option<Vec<u64>>> = OnceLock::new();
                    let dump_addrs =
                        trace_u64_targets_env(&DUMP_ADDRS, "RUZU_DUMP_BOUND_TEXTURES_ADDRS");
                    let dump_matches = dump_pipeline
                        .is_none_or(|target| target == pipeline.program_pipeline_handle())
                        && draw_seq >= dump_seq_min;
                    if dump_matches {
                        for (unit, &handle) in textures[..texture_binding].iter().enumerate() {
                            if handle == 0 {
                                continue;
                            }
                            let sampler = gl_samplers[unit];
                            let mut width = 0i32;
                            let mut height = 0i32;
                            let mut depth = 0i32;
                            let mut internal_format = 0i32;
                            let mut texture_max_level = 0i32;
                            let mut sampler_min_filter = 0i32;
                            let mut sampler_mag_filter = 0i32;
                            let mut sampler_compare_mode = 0i32;
                            let mut sampler_wrap_s = 0i32;
                            let mut sampler_wrap_t = 0i32;
                            let mut sampler_wrap_r = 0i32;
                            let mut sampler_min_lod = 0f32;
                            let mut sampler_max_lod = 0f32;
                            let mut sampler_border = [0f32; 4];
                            let view_id = bound_texture_view_ids[unit];
                            let (
                                view_gpu_addr,
                                image_gpu_addr,
                                image_cpu_addr,
                                view_swizzle,
                                view_type,
                                view_range_base_level,
                                view_range_base_layer,
                                view_range_levels,
                                view_range_layers,
                                view_flags,
                                view_size,
                            ) = if view_id.is_valid() {
                                let view = self.texture_cache.base.slot_image_views.get(view_id);
                                let image = self.texture_cache.base.slot_images.get(view.image_id);
                                (
                                    view.gpu_addr,
                                    image.gpu_addr,
                                    image.cpu_addr,
                                    view.swizzle,
                                    view.view_type as u32,
                                    view.range.base.level,
                                    view.range.base.layer,
                                    view.range.extent.levels,
                                    view.range.extent.layers,
                                    view.flags.bits(),
                                    view.size,
                                )
                            } else {
                                (
                                    0,
                                    0,
                                    0,
                                    [0; 4],
                                    0,
                                    0,
                                    0,
                                    0,
                                    0,
                                    0,
                                    crate::texture_cache::types::Extent3D::default(),
                                )
                            };
                            if let Some(targets) = dump_addrs {
                                if !targets.is_empty() && !targets.contains(&view_gpu_addr) {
                                    continue;
                                }
                            }
                            if DUMPS.load(Ordering::Relaxed) >= dump_limit {
                                continue;
                            }
                            let dump_index = DUMPS.fetch_add(1, Ordering::Relaxed);
                            if dump_index >= dump_limit {
                                continue;
                            }
                            unsafe {
                                gl::GetTextureLevelParameteriv(
                                    handle,
                                    0,
                                    gl::TEXTURE_WIDTH,
                                    &mut width,
                                );
                                gl::GetTextureLevelParameteriv(
                                    handle,
                                    0,
                                    gl::TEXTURE_HEIGHT,
                                    &mut height,
                                );
                                gl::GetTextureLevelParameteriv(
                                    handle,
                                    0,
                                    gl::TEXTURE_DEPTH,
                                    &mut depth,
                                );
                                gl::GetTextureLevelParameteriv(
                                    handle,
                                    0,
                                    gl::TEXTURE_INTERNAL_FORMAT,
                                    &mut internal_format,
                                );
                                gl::GetTextureParameteriv(
                                    handle,
                                    gl::TEXTURE_MAX_LEVEL,
                                    &mut texture_max_level,
                                );
                                if sampler != 0 {
                                    gl::GetSamplerParameteriv(
                                        sampler,
                                        gl::TEXTURE_MIN_FILTER,
                                        &mut sampler_min_filter,
                                    );
                                    gl::GetSamplerParameteriv(
                                        sampler,
                                        gl::TEXTURE_MAG_FILTER,
                                        &mut sampler_mag_filter,
                                    );
                                    gl::GetSamplerParameteriv(
                                        sampler,
                                        gl::TEXTURE_COMPARE_MODE,
                                        &mut sampler_compare_mode,
                                    );
                                    gl::GetSamplerParameteriv(
                                        sampler,
                                        gl::TEXTURE_WRAP_S,
                                        &mut sampler_wrap_s,
                                    );
                                    gl::GetSamplerParameteriv(
                                        sampler,
                                        gl::TEXTURE_WRAP_T,
                                        &mut sampler_wrap_t,
                                    );
                                    gl::GetSamplerParameteriv(
                                        sampler,
                                        gl::TEXTURE_WRAP_R,
                                        &mut sampler_wrap_r,
                                    );
                                    gl::GetSamplerParameterfv(
                                        sampler,
                                        gl::TEXTURE_MIN_LOD,
                                        &mut sampler_min_lod,
                                    );
                                    gl::GetSamplerParameterfv(
                                        sampler,
                                        gl::TEXTURE_MAX_LOD,
                                        &mut sampler_max_lod,
                                    );
                                    gl::GetSamplerParameterfv(
                                        sampler,
                                        gl::TEXTURE_BORDER_COLOR,
                                        sampler_border.as_mut_ptr(),
                                    );
                                }
                            }
                            let full_len = (width.max(0) as usize)
                                .saturating_mul(height.max(1) as usize)
                                .saturating_mul(depth.max(1) as usize)
                                .saturating_mul(4);
                            if full_len == 0 || full_len > 64 * 1024 * 1024 {
                                log::warn!(
                                    "[BOUND_TEX] seq={} batch={} bind_dump={} unit={} view_id={} view_gpu=0x{:X} image_gpu=0x{:X} image_cpu=0x{:X} view_type={} range={}:{}+{}:{} flags=0x{:X} view_size={}x{}x{} swizzle={:?} handle={} sampler={} size={}x{}x{} ifmt=0x{:X} tex_max_level={} min=0x{:X} mag=0x{:X} cmp=0x{:X} wrap=(0x{:X},0x{:X},0x{:X}) lod=({:.3},{:.3}) border={:?} skipped_readback_bytes={}",
                                    draw_seq,
                                    draw_no,
                                    dump_index,
                                    unit,
                                    view_id.index,
                                    view_gpu_addr,
                                    image_gpu_addr,
                                    image_cpu_addr,
                                    view_type,
                                    view_range_base_level,
                                    view_range_base_layer,
                                    view_range_levels,
                                    view_range_layers,
                                    view_flags,
                                    view_size.width,
                                    view_size.height,
                                    view_size.depth,
                                    view_swizzle,
                                    handle,
                                    sampler,
                                    width,
                                    height,
                                    depth,
                                    internal_format,
                                    texture_max_level,
                                    sampler_min_filter,
                                    sampler_mag_filter,
                                    sampler_compare_mode,
                                    sampler_wrap_s,
                                    sampler_wrap_t,
                                    sampler_wrap_r,
                                    sampler_min_lod,
                                    sampler_max_lod,
                                    sampler_border,
                                    full_len,
                                );
                                continue;
                            }
                            let mut bytes = vec![0u8; full_len];
                            unsafe {
                                while gl::GetError() != gl::NO_ERROR {}
                                gl::GetTextureImage(
                                    handle,
                                    0,
                                    gl::RGBA,
                                    gl::UNSIGNED_BYTE,
                                    full_len as i32,
                                    bytes.as_mut_ptr().cast(),
                                );
                                let err = gl::GetError();
                                let nonzero = bytes.iter().filter(|&&b| b != 0).count();
                                let color_nonzero = bytes
                                    .chunks_exact(4)
                                    .filter(|px| px[0] != 0 || px[1] != 0 || px[2] != 0)
                                    .count();
                                let checksum = bytes.iter().fold(0u64, |acc, &b| {
                                    acc.wrapping_mul(16777619).wrapping_add(b as u64)
                                });
                                log::warn!(
                                    "[BOUND_TEX] seq={} batch={} bind_dump={} unit={} view_id={} view_gpu=0x{:X} image_gpu=0x{:X} image_cpu=0x{:X} view_type={} range={}:{}+{}:{} flags=0x{:X} view_size={}x{}x{} swizzle={:?} handle={} sampler={} size={}x{}x{} ifmt=0x{:X} tex_max_level={} min=0x{:X} mag=0x{:X} cmp=0x{:X} wrap=(0x{:X},0x{:X},0x{:X}) lod=({:.3},{:.3}) border={:?} err=0x{:X} nonzero={} color_nonzero={} crc=0x{:X} first16={:02X?}",
                                    draw_seq,
                                    draw_no,
                                    dump_index,
                                    unit,
                                    view_id.index,
                                    view_gpu_addr,
                                    image_gpu_addr,
                                    image_cpu_addr,
                                    view_type,
                                    view_range_base_level,
                                    view_range_base_layer,
                                    view_range_levels,
                                    view_range_layers,
                                    view_flags,
                                    view_size.width,
                                    view_size.height,
                                    view_size.depth,
                                    view_swizzle,
                                    handle,
                                    sampler,
                                    width,
                                    height,
                                    depth,
                                    internal_format,
                                    texture_max_level,
                                    sampler_min_filter,
                                    sampler_mag_filter,
                                    sampler_compare_mode,
                                    sampler_wrap_s,
                                    sampler_wrap_t,
                                    sampler_wrap_r,
                                    sampler_min_lod,
                                    sampler_max_lod,
                                    sampler_border,
                                    err,
                                    nonzero,
                                    color_nonzero,
                                    checksum,
                                    &bytes[..16.min(bytes.len())],
                                );
                                if err == gl::NO_ERROR {
                                    if let Some(output_dir) =
                                        std::env::var_os("RUZU_DUMP_BOUND_TEXTURES_DIR")
                                    {
                                        let mut path = std::path::PathBuf::from(output_dir);
                                        if let Err(err) = std::fs::create_dir_all(&path) {
                                            log::warn!(
                                                "[BOUND_TEX_DUMP] failed to create {}: {}",
                                                path.display(),
                                                err
                                            );
                                        } else {
                                            let w = width.max(0) as usize;
                                            let h = height.max(0) as usize;
                                            let d = depth.max(1) as usize;
                                            let slice_len = w.saturating_mul(h).saturating_mul(4);
                                            path.push(format!(
                                                "seq_{draw_seq}_dump_{dump_index}_unit_{unit}_gpu_{view_gpu_addr:016X}_{}x{}x{}.rgba",
                                                w, h, d
                                            ));
                                            match std::fs::write(&path, &bytes) {
                                                Ok(()) => log::warn!(
                                                    "[BOUND_TEX_DUMP] wrote {}",
                                                    path.display()
                                                ),
                                                Err(err) => log::warn!(
                                                    "[BOUND_TEX_DUMP] failed to write {}: {}",
                                                    path.display(),
                                                    err
                                                ),
                                            }
                                            for slice in 0..d {
                                                let Some(slice_bytes) = bytes.get(
                                                    slice.saturating_mul(slice_len)
                                                        ..slice
                                                            .saturating_add(1)
                                                            .saturating_mul(slice_len),
                                                ) else {
                                                    break;
                                                };
                                                let slice_suffix = if d > 1 {
                                                    format!("_z{slice}")
                                                } else {
                                                    String::new()
                                                };
                                                path.set_file_name(format!(
                                                    "seq_{draw_seq}_dump_{dump_index}_unit_{unit}_gpu_{view_gpu_addr:016X}_{}x{}{}.ppm",
                                                    w, h, slice_suffix
                                                ));
                                                let mut ppm = Vec::with_capacity(
                                                    w.saturating_mul(h).saturating_mul(3) + 64,
                                                );
                                                ppm.extend_from_slice(
                                                    format!("P6\n{} {}\n255\n", w, h).as_bytes(),
                                                );
                                                for px in slice_bytes.chunks_exact(4) {
                                                    ppm.extend_from_slice(&px[..3]);
                                                }
                                                match std::fs::write(&path, ppm) {
                                                    Ok(()) => log::warn!(
                                                        "[BOUND_TEX_DUMP] wrote {}",
                                                        path.display()
                                                    ),
                                                    Err(err) => log::warn!(
                                                        "[BOUND_TEX_DUMP] failed to write {}: {}",
                                                        path.display(),
                                                        err
                                                    ),
                                                }
                                                path.set_file_name(format!(
                                                    "seq_{draw_seq}_dump_{dump_index}_unit_{unit}_gpu_{view_gpu_addr:016X}_{}x{}{}.alpha.pgm",
                                                    w, h, slice_suffix
                                                ));
                                                let mut alpha =
                                                    Vec::with_capacity(w.saturating_mul(h) + 64);
                                                alpha.extend_from_slice(
                                                    format!("P5\n{} {}\n255\n", w, h).as_bytes(),
                                                );
                                                for px in slice_bytes.chunks_exact(4) {
                                                    alpha.push(px[3]);
                                                }
                                                match std::fs::write(&path, alpha) {
                                                    Ok(()) => log::warn!(
                                                        "[BOUND_TEX_DUMP] wrote {}",
                                                        path.display()
                                                    ),
                                                    Err(err) => log::warn!(
                                                        "[BOUND_TEX_DUMP] failed to write {}: {}",
                                                        path.display(),
                                                        err
                                                    ),
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                // Upstream `GraphicsPipeline::ConfigureImpl` calls
                // `buffer_cache.UpdateGraphicsBuffers` and
                // `buffer_cache.BindHostStageBuffers(stage)` before the
                // final `glBindTextures`. The OpenGL buffer-cache runtime
                // writes texture-buffer object handles into `textures` via
                // `SetImagePointers`; without this, texture-buffer descriptors
                // remain bound as texture 0 and shaders sample black.
                if let Some(mm) = cbuf_mm_guard.as_ref() {
                    self.buffer_cache.update_graphics_buffers_with_gpu_resolver(
                        is_indexed,
                        |gpu_addr| mm.gpu_to_cpu_address(gpu_addr),
                        |gpu_addr| mm.is_within_gpu_address_range(gpu_addr),
                        |gpu_addr, size| mm.max_continuous_range(gpu_addr, size),
                    );
                } else {
                    self.buffer_cache.update_graphics_buffers(is_indexed);
                }
                self.buffer_cache
                    .set_image_pointers(textures.as_mut_ptr(), images.as_mut_ptr());
                for stage in 0..crate::buffer_cache::buffer_cache_base::NUM_STAGES as usize {
                    self.buffer_cache.bind_host_stage_buffers(stage);
                }
                self.buffer_cache
                    .set_image_pointers(std::ptr::null_mut(), std::ptr::null_mut());
                unsafe {
                    gl::BindTextures(0, texture_binding as i32, textures.as_ptr());
                    if gl_debug.disable_sampler_bind {
                        let null_samplers = [0u32; MAX_TEXTURES];
                        gl::BindSamplers(0, texture_binding as i32, null_samplers.as_ptr());
                    } else {
                        gl::BindSamplers(0, texture_binding as i32, gl_samplers.as_ptr());
                    }
                }
            }
            record_gl_draw_stage(draw_seq, 22);
            trace_gl_draw_stall!("[GL_DRAW_STALL] seq={} after_bind_textures", draw_seq);
            if image_binding != 0 {
                // Slice 14: upstream `glBindImageTextures(0, image_binding,
                // images.data())` at gl_graphics_pipeline.cpp:557.
                unsafe {
                    gl::BindImageTextures(0, image_binding as i32, images.as_ptr());
                }
            }
            record_gl_draw_stage(draw_seq, 23);
            trace_gl_draw_stall!("[GL_DRAW_STALL] seq={} after_bind_images", draw_seq);
            if gl_debug.trace_per_draw_bind {
                let mut samples: [[u8; 4]; 4] = [[0; 4]; 4];
                let mut samples_far: [[u8; 4]; 4] = [[0; 4]; 4];
                let mut samples_mid: [[u8; 4]; 4] = [[0; 4]; 4];
                let mut sizes: [[i32; 2]; 4] = [[0; 2]; 4];
                let unit_count = texture_binding.min(4);
                for unit in 0..unit_count {
                    let tex = textures[unit];
                    if tex == 0 {
                        continue;
                    }
                    unsafe {
                        let mut w = 0i32;
                        let mut h = 0i32;
                        gl::GetTextureLevelParameteriv(tex, 0, gl::TEXTURE_WIDTH, &mut w);
                        gl::GetTextureLevelParameteriv(tex, 0, gl::TEXTURE_HEIGHT, &mut h);
                        sizes[unit] = [w, h];
                        gl::GetTextureSubImage(
                            tex,
                            0,
                            0,
                            0,
                            0,
                            1,
                            1,
                            1,
                            gl::RGBA,
                            gl::UNSIGNED_BYTE,
                            4,
                            samples[unit].as_mut_ptr() as *mut _,
                        );
                        if w > 1 && h > 1 {
                            let mx = (w / 2).max(1);
                            let my = (h / 2).max(1);
                            gl::GetTextureSubImage(
                                tex,
                                0,
                                mx,
                                my,
                                0,
                                1,
                                1,
                                1,
                                gl::RGBA,
                                gl::UNSIGNED_BYTE,
                                4,
                                samples_mid[unit].as_mut_ptr() as *mut _,
                            );
                            gl::GetTextureSubImage(
                                tex,
                                0,
                                w - 1,
                                h - 1,
                                0,
                                1,
                                1,
                                1,
                                gl::RGBA,
                                gl::UNSIGNED_BYTE,
                                4,
                                samples_far[unit].as_mut_ptr() as *mut _,
                            );
                        }
                    }
                }
                let mut summary = String::new();
                for unit in 0..unit_count {
                    if !summary.is_empty() {
                        summary.push(' ');
                    }
                    summary.push_str(&format!(
                        "u{}=tex{}({}x{})/c{:02X?}/m{:02X?}/f{:02X?}",
                        unit,
                        textures[unit],
                        sizes[unit][0],
                        sizes[unit][1],
                        samples[unit],
                        samples_mid[unit],
                        samples_far[unit]
                    ));
                }
                let mut fb_srgb: u8 = 0;
                let mut blend_enabled: u8 = 0;
                let mut blend_eq_rgb: i32 = 0;
                let mut blend_src_rgb: i32 = 0;
                let mut blend_dst_rgb: i32 = 0;
                let mut color_mask: [u8; 4] = [0; 4];
                let mut draw_fb: i32 = 0;
                let mut depth_test: u8 = 0;
                let mut stencil_test: u8 = 0;
                let mut sample_alpha_to_coverage: u8 = 0;
                let mut rasterizer_discard: u8 = 0;
                unsafe {
                    fb_srgb = gl::IsEnabled(gl::FRAMEBUFFER_SRGB);
                    blend_enabled = gl::IsEnabledi(gl::BLEND, 0);
                    gl::GetIntegeri_v(gl::BLEND_EQUATION_RGB, 0, &mut blend_eq_rgb);
                    gl::GetIntegeri_v(gl::BLEND_SRC_RGB, 0, &mut blend_src_rgb);
                    gl::GetIntegeri_v(gl::BLEND_DST_RGB, 0, &mut blend_dst_rgb);
                    gl::GetBooleani_v(gl::COLOR_WRITEMASK, 0, color_mask.as_mut_ptr());
                    gl::GetIntegerv(gl::DRAW_FRAMEBUFFER_BINDING, &mut draw_fb);
                    depth_test = gl::IsEnabled(gl::DEPTH_TEST);
                    stencil_test = gl::IsEnabled(gl::STENCIL_TEST);
                    sample_alpha_to_coverage = gl::IsEnabled(gl::SAMPLE_ALPHA_TO_COVERAGE);
                    rasterizer_discard = gl::IsEnabled(gl::RASTERIZER_DISCARD);
                }
                let mut ubo_handle: i32 = 0;
                let mut ubo_offset: i64 = 0;
                let mut ubo_size: i64 = 0;
                let mut ubo_bytes = [0u8; 96];
                unsafe {
                    gl::GetIntegeri_v(gl::UNIFORM_BUFFER_BINDING, 0, &mut ubo_handle);
                    gl::GetInteger64i_v(gl::UNIFORM_BUFFER_START, 0, &mut ubo_offset);
                    gl::GetInteger64i_v(gl::UNIFORM_BUFFER_SIZE, 0, &mut ubo_size);
                    if ubo_handle > 0 {
                        gl::GetNamedBufferSubData(
                            ubo_handle as u32,
                            ubo_offset as isize,
                            ubo_bytes.len() as isize,
                            ubo_bytes.as_mut_ptr() as *mut _,
                        );
                    }
                }
                let ubo_floats: [f32; 24] = unsafe { std::mem::transmute(ubo_bytes) };
                log::warn!(
                    "[PER_DRAW_BIND] draw_fb={} fb_srgb={} blend_en={} eq=0x{:X} src=0x{:X} dst=0x{:X} cmask={:?} depth={} sten={} a2c={} disc={} texture_binding={} image_binding={} samples=[{}] ubo0=h{}/off{}/size{} ubo0_floats[0..24]={:?}",
                    draw_fb,
                    fb_srgb,
                    blend_enabled,
                    blend_eq_rgb,
                    blend_src_rgb,
                    blend_dst_rgb,
                    color_mask,
                    depth_test,
                    stencil_test,
                    sample_alpha_to_coverage,
                    rasterizer_discard,
                    texture_binding,
                    image_binding,
                    summary,
                    ubo_handle,
                    ubo_offset,
                    ubo_size,
                    ubo_floats,
                );
            }
        }
        record_gl_draw_stage(draw_seq, 24);
        trace_gl_draw_stall!("[GL_DRAW_STALL] seq={} after_descriptor_block", draw_seq);
        if trace_draw {
            info!(
                "[GL_DRAW_PROFILE] pipeline_configure_us={}",
                step.map(|start| start.elapsed().as_micros()).unwrap_or(0)
            );
        }
        if let Some(step) = step {
            profile_configure_us = trace_elapsed_us(step);
        }

        // Partial port of upstream `GraphicsPipeline::ConfigureImpl`
        // uniform-buffer setup. Upstream derives the enabled cbuf mask from
        // shader info, not from every Maxwell cbuf currently enabled. This is
        // required because GL bindings are compacted over the shader's
        // descriptors: if a shader only declares cbuf3, that cbuf is bound at
        // binding 0.
        let cb_bindings = draw_view.cb_bindings();
        let trace_cbuf_bind_enabled = common::trace::is_enabled(common::trace::cat::CBUF_BIND);
        let trace_cbuf_bind = trace_cbuf_bind_enabled
            && should_trace_cbuf_bind(draw_seq, pipeline.program_pipeline_handle());
        let trace_cbuf_vec4_count = if trace_cbuf_bind {
            static VEC4_COUNT: OnceLock<Option<u64>> = OnceLock::new();
            trace_u64_env_cached(&VEC4_COUNT, "RUZU_TRACE_CBUF_VEC4_COUNT")
                .unwrap_or(3)
                .clamp(1, 32) as usize
        } else {
            0
        };
        for stage in 0..uniform_masks.len().min(cb_bindings.len()) {
            let mut bits = uniform_masks[stage];
            let mut slot = 0u32;
            let mut binding_index = 0u32;
            while bits != 0 {
                let skip = bits.trailing_zeros();
                slot += skip;
                bits >>= skip;

                let binding = cb_bindings[stage][slot as usize];
                if trace_cbuf_bind {
                    let used_size = uniform_sizes[stage][slot as usize];
                    let mut first_words = vec![0u32; trace_cbuf_vec4_count * 4];
                    if binding.enabled && binding.address != 0 && binding.size != 0 {
                        if let (Some(mm), Some(reader)) =
                            (&self.channel_memory_manager, &self.device_memory_reader)
                        {
                            let mm = mm.lock();
                            let read_size =
                                std::cmp::min(trace_cbuf_vec4_count * 16, binding.size as usize);
                            let mut bytes = vec![0u8; read_size];
                            if read_size != 0 {
                                mm.read_block(binding.address, &mut bytes);
                                for (index, word) in first_words.iter_mut().enumerate() {
                                    let start = index * 4;
                                    if start + 4 <= read_size {
                                        *word = u32::from_le_bytes([
                                            bytes[start],
                                            bytes[start + 1],
                                            bytes[start + 2],
                                            bytes[start + 3],
                                        ]);
                                    }
                                }
                            }
                        }
                    }
                    for vec4_index in 0..trace_cbuf_vec4_count {
                        let base = vec4_index * 4;
                        common::trace::emit_raw(
                            common::trace::cat::CBUF_BIND,
                            &[
                                self.total_draw_count,
                                pipeline.program_pipeline_handle() as u64,
                                stage as u64,
                                slot as u64,
                                binding_index as u64,
                                vec4_index as u64,
                                binding.address,
                                binding.size as u64,
                                used_size as u64,
                                binding.enabled as u64,
                                first_words[base] as u64,
                                first_words[base + 1] as u64,
                                first_words[base + 2] as u64,
                                first_words[base + 3] as u64,
                            ],
                        );
                    }
                }
                if skip_draw_due_to_sampling_addr
                    && stage == 4
                    && slot == 3
                    && std::env::var_os("RUZU_TRACE_SKIP_DRAW_CBUF").is_some()
                {
                    use std::sync::atomic::{AtomicU64, Ordering as AtomicOrdering};
                    static COUNT: AtomicU64 = AtomicU64::new(0);
                    let n = COUNT.fetch_add(1, AtomicOrdering::Relaxed);
                    if n < 24 || n.is_power_of_two() {
                        let mut words = [0u32; 8];
                        if binding.enabled && binding.address != 0 && binding.size != 0 {
                            if let Some(mm) = cbuf_mm_guard.as_ref() {
                                let mut bytes = [0u8; 32];
                                let read_size = bytes.len().min(binding.size as usize);
                                mm.read_block(binding.address, &mut bytes[..read_size]);
                                for (index, word) in words.iter_mut().enumerate() {
                                    let offset = index * 4;
                                    if offset + 4 <= read_size {
                                        *word = u32::from_le_bytes([
                                            bytes[offset],
                                            bytes[offset + 1],
                                            bytes[offset + 2],
                                            bytes[offset + 3],
                                        ]);
                                    }
                                }
                            }
                        }
                        log::warn!(
                            "[SKIP_DRAW_CBUF] n={} draw_seq={} pipeline={} stage={} slot={} gpu=0x{:X} size={} enabled={} vec0=[{:.6},{:.6},{:.6},{:.6}] vec1=[{:.6},{:.6},{:.6},{:.6}] raw=[{:08X},{:08X},{:08X},{:08X},{:08X},{:08X},{:08X},{:08X}]",
                            n,
                            draw_seq,
                            pipeline.program_pipeline_handle(),
                            stage,
                            slot,
                            binding.address,
                            binding.size,
                            binding.enabled,
                            f32::from_bits(words[0]),
                            f32::from_bits(words[1]),
                            f32::from_bits(words[2]),
                            f32::from_bits(words[3]),
                            f32::from_bits(words[4]),
                            f32::from_bits(words[5]),
                            f32::from_bits(words[6]),
                            f32::from_bits(words[7]),
                            words[0],
                            words[1],
                            words[2],
                            words[3],
                            words[4],
                            words[5],
                            words[6],
                            words[7],
                        );
                    }
                }
                if binding.enabled && binding.address != 0 && binding.size != 0 {
                    let device_addr = cbuf_mm_guard
                        .as_ref()
                        .and_then(|mm| mm.gpu_to_cpu_address(binding.address))
                        .unwrap_or(binding.address);
                    self.buffer_cache
                        .bind_graphics_uniform_buffer_with_device_addr(
                            stage,
                            slot,
                            device_addr,
                            binding.size,
                        );
                } else {
                    self.buffer_cache
                        .disable_graphics_uniform_buffer(stage, slot);
                }

                slot += 1;
                binding_index += 1;
                bits >>= 1;
            }
        }
        record_gl_draw_stage(draw_seq, 26);
        trace_gl_draw_stall!("[GL_DRAW_STALL] seq={} after_uniform_buffers", draw_seq);

        // Buffer cache: refresh and bind host vertex/index buffers.
        // Mirrors upstream `RasterizerOpenGL::PrepareDraw`.
        let step = profile_draw_timing.then(Instant::now);
        record_gl_draw_stage(draw_seq, 27);
        trace_gl_draw_stall!(
            "[GL_DRAW_STALL] seq={} before_update_graphics_buffers",
            draw_seq
        );
        if let Some(mm) = cbuf_mm_guard.as_ref() {
            self.buffer_cache.update_graphics_buffers_with_gpu_resolver(
                is_indexed,
                |gpu_addr| mm.gpu_to_cpu_address(gpu_addr),
                |gpu_addr| mm.is_within_gpu_address_range(gpu_addr),
                |gpu_addr, size| mm.max_continuous_range(gpu_addr, size),
            );
        } else {
            self.buffer_cache.update_graphics_buffers(is_indexed);
        }
        drop(cbuf_mm_guard);
        record_gl_draw_stage(draw_seq, 28);
        trace_gl_draw_stall!(
            "[GL_DRAW_STALL] seq={} after_update_graphics_buffers",
            draw_seq
        );
        if trace_draw {
            info!(
                "[GL_DRAW_PROFILE] update_graphics_buffers_us={}",
                step.map(|start| start.elapsed().as_micros()).unwrap_or(0)
            );
        }
        if let Some(step) = step {
            profile_update_buffers_us = trace_elapsed_us(step);
        }
        let step = profile_draw_timing.then(Instant::now);
        record_gl_draw_stage(draw_seq, 29);
        trace_gl_draw_stall!("[GL_DRAW_STALL] seq={} before_bind_host_buffers", draw_seq);
        if self.transient_vao != 0 {
            unsafe {
                gl::BindVertexArray(self.transient_vao);
            }
        }
        self.buffer_cache.bind_host_geometry_buffers(is_indexed);
        for stage in 0..crate::buffer_cache::buffer_cache_base::NUM_STAGES as usize {
            self.buffer_cache.bind_host_stage_buffers(stage);
        }
        record_gl_draw_stage(draw_seq, 30);
        trace_gl_draw_stall!("[GL_DRAW_STALL] seq={} after_bind_host_buffers", draw_seq);
        if trace_draw {
            info!(
                "[GL_DRAW_PROFILE] bind_host_geometry_buffers_us={}",
                step.map(|start| start.elapsed().as_micros()).unwrap_or(0)
            );
        }
        if let Some(step) = step {
            profile_bind_buffers_us = trace_elapsed_us(step);
        }

        let primitive_mode = primitive_topology_to_gl(draw_state.topology);
        let base_instance = draw_state.base_instance;
        let num_instances = instance_count;

        // Only issue real `glDraw*` calls when the pipeline carries
        // compiled GL programs *and* we have a transient VAO (i.e. the
        // production constructor ran). This keeps the unit-test path
        // (`new_for_test`, no GL context, placeholder pipelines) safe.
        let can_draw_gl = pipeline_has_programs && self.transient_vao != 0;
        let pipeline_handle = pipeline.program_pipeline_handle() as u64;
        let trace_rt_sample_this_draw = can_draw_gl
            && common::trace::is_enabled(common::trace::cat::RT_SAMPLE)
            && rt_sample_targets().is_some()
            && should_trace_rt_sample_draw(pipeline_handle, draw_seq)
            && should_trace_rt_sample_window();
        let sync_draw_step = profile_draw_timing.then(Instant::now);
        if can_draw_gl {
            record_gl_draw_stage(draw_seq, 31);
            trace_gl_draw_stall!("[GL_DRAW_STALL] seq={} before_fixed_state_sync", draw_seq);
            let viewport_scale = if self.texture_cache.is_rescaling_active() {
                settings::values().resolution_info.up_factor
            } else {
                1.0
            };
            Self::sync_state(
                &mut draw_view,
                &mut self.state_tracker,
                self.transient_vao,
                self.has_depth_buffer_float,
                self.has_viewport_swizzle,
                self.has_fill_rectangle,
                viewport_scale,
            );
            if gl_debug.force_disable_blend {
                unsafe {
                    for i in 0..8 {
                        gl::Disablei(gl::BLEND, i);
                    }
                }
            }
            if gl_debug.force_barrier {
                unsafe {
                    gl::MemoryBarrier(gl::ALL_BARRIER_BITS);
                }
            }
            record_gl_draw_stage(draw_seq, 12);
            let trace_pre_draw_state = gl_debug.trace_pre_draw_state
                && std::env::var("RUZU_TRACE_PRE_DRAW_STATE_PIPELINE")
                    .ok()
                    .and_then(|value| value.parse::<u32>().ok())
                    .is_none_or(|target| target == pipeline.program_pipeline_handle());
            if trace_pre_draw_state {
                let mut blend_en: u8 = 0;
                let mut blend_eq_rgb: i32 = 0;
                let mut blend_src_rgb: i32 = 0;
                let mut blend_dst_rgb: i32 = 0;
                let mut blend_eq_a: i32 = 0;
                let mut blend_src_a: i32 = 0;
                let mut blend_dst_a: i32 = 0;
                let mut color_mask: [u8; 4] = [0; 4];
                let mut depth_test: u8 = 0;
                let mut depth_mask: u8 = 0;
                let mut stencil_test: u8 = 0;
                let mut sample_alpha_to_coverage: u8 = 0;
                let mut rasterizer_discard: u8 = 0;
                let mut viewport0 = [0i32; 4];
                let mut scissor_box = [0i32; 4];
                let mut scissor_en: u8 = 0;
                let mut draw_fb = 0i32;
                let mut fb_srgb: u8 = 0;
                unsafe {
                    blend_en = gl::IsEnabledi(gl::BLEND, 0);
                    gl::GetIntegeri_v(gl::BLEND_EQUATION_RGB, 0, &mut blend_eq_rgb);
                    gl::GetIntegeri_v(gl::BLEND_SRC_RGB, 0, &mut blend_src_rgb);
                    gl::GetIntegeri_v(gl::BLEND_DST_RGB, 0, &mut blend_dst_rgb);
                    gl::GetIntegeri_v(gl::BLEND_EQUATION_ALPHA, 0, &mut blend_eq_a);
                    gl::GetIntegeri_v(gl::BLEND_SRC_ALPHA, 0, &mut blend_src_a);
                    gl::GetIntegeri_v(gl::BLEND_DST_ALPHA, 0, &mut blend_dst_a);
                    gl::GetBooleani_v(gl::COLOR_WRITEMASK, 0, color_mask.as_mut_ptr());
                    depth_test = gl::IsEnabled(gl::DEPTH_TEST);
                    let mut dm = gl::FALSE;
                    gl::GetBooleanv(gl::DEPTH_WRITEMASK, &mut dm);
                    depth_mask = dm;
                    stencil_test = gl::IsEnabled(gl::STENCIL_TEST);
                    sample_alpha_to_coverage = gl::IsEnabled(gl::SAMPLE_ALPHA_TO_COVERAGE);
                    rasterizer_discard = gl::IsEnabled(gl::RASTERIZER_DISCARD);
                    gl::GetIntegeri_v(gl::VIEWPORT, 0, viewport0.as_mut_ptr());
                    scissor_en = gl::IsEnabledi(gl::SCISSOR_TEST, 0);
                    gl::GetIntegeri_v(gl::SCISSOR_BOX, 0, scissor_box.as_mut_ptr());
                    gl::GetIntegerv(gl::DRAW_FRAMEBUFFER_BINDING, &mut draw_fb);
                    fb_srgb = gl::IsEnabled(gl::FRAMEBUFFER_SRGB);
                }
                let mut cull_face_en: u8 = 0;
                let mut cull_face_mode: i32 = 0;
                let mut front_face: i32 = 0;
                let mut polygon_mode: [i32; 2] = [0; 2];
                let mut primitive_restart_en: u8 = 0;
                let mut primitive_restart_idx: i32 = 0;
                let mut pipeline_bind: i32 = 0;
                let mut vao: i32 = 0;
                let mut active_tex: i32 = 0;
                let mut fbo_status: u32 = 0;
                let mut sample_coverage_en: u8 = 0;
                let mut multi_sample_en: u8 = 0;
                unsafe {
                    cull_face_en = gl::IsEnabled(gl::CULL_FACE);
                    gl::GetIntegerv(gl::CULL_FACE_MODE, &mut cull_face_mode);
                    gl::GetIntegerv(gl::FRONT_FACE, &mut front_face);
                    gl::GetIntegerv(gl::POLYGON_MODE, polygon_mode.as_mut_ptr());
                    primitive_restart_en = gl::IsEnabled(gl::PRIMITIVE_RESTART);
                    gl::GetIntegerv(gl::PRIMITIVE_RESTART_INDEX, &mut primitive_restart_idx);
                    gl::GetIntegerv(gl::PROGRAM_PIPELINE_BINDING, &mut pipeline_bind);
                    gl::GetIntegerv(gl::VERTEX_ARRAY_BINDING, &mut vao);
                    gl::GetIntegerv(gl::ACTIVE_TEXTURE, &mut active_tex);
                    fbo_status =
                        gl::CheckNamedFramebufferStatus(draw_fb as u32, gl::DRAW_FRAMEBUFFER);
                    sample_coverage_en = gl::IsEnabled(gl::SAMPLE_COVERAGE);
                    multi_sample_en = gl::IsEnabled(gl::MULTISAMPLE);
                }
                log::warn!(
                    "[PRE_DRAW] draw_fb={} fbo_status=0x{:X} fb_srgb={} blend_en={} eq_rgb=0x{:X} eq_a=0x{:X} src_rgb=0x{:X} dst_rgb=0x{:X} src_a=0x{:X} dst_a=0x{:X} cmask={:?} depth_test={} depth_mask={} sten={} a2c={} disc={} viewport0={:?} scissor0_en={} scissor_box={:?} cull_en={} cull_mode=0x{:X} front_face=0x{:X} poly_mode={:?} prim_restart_en={} prim_restart_idx=0x{:X} pipeline={} vao={} active_tex=0x{:X} samp_cov_en={} multisample={}",
                    draw_fb,
                    fbo_status,
                    fb_srgb,
                    blend_en,
                    blend_eq_rgb,
                    blend_eq_a,
                    blend_src_rgb,
                    blend_dst_rgb,
                    blend_src_a,
                    blend_dst_a,
                    color_mask,
                    depth_test,
                    depth_mask,
                    stencil_test,
                    sample_alpha_to_coverage,
                    rasterizer_discard,
                    viewport0,
                    scissor_en,
                    scissor_box,
                    cull_face_en,
                    cull_face_mode,
                    front_face,
                    polygon_mode,
                    primitive_restart_en,
                    primitive_restart_idx,
                    pipeline_bind,
                    vao,
                    active_tex,
                    sample_coverage_en,
                    multi_sample_en,
                );
            }
        }

        let trace_samples = can_draw_gl && gl_debug.trace_samples_passed;
        let mut samples_query = 0u32;
        if trace_samples {
            unsafe {
                gl::GenQueries(1, &mut samples_query);
                gl::BeginQuery(gl::SAMPLES_PASSED, samples_query);
            }
        }

        if is_indexed {
            let base_vertex = draw_state.base_index as i32;
            let num_vertices = draw_state.index_buffer.count;
            if can_draw_gl {
                let index_format = index_format_to_gl(draw_state.index_buffer.format);
                let index_offset = self.buffer_cache.index_offset();
                if gl_debug.predraw_clear_red {
                    if let Some((framebuffer, _, _)) = bound_draw_framebuffer {
                        unsafe {
                            let red = [1.0f32, 0.0, 0.0, 1.0];
                            gl::ClearNamedFramebufferfv(framebuffer, gl::COLOR, 0, red.as_ptr());
                        }
                    }
                }
                if gl_debug.force_no_cull {
                    unsafe {
                        gl::Disable(gl::CULL_FACE);
                    }
                }
                if gl_debug.force_no_primitive_restart {
                    unsafe {
                        gl::Disable(gl::PRIMITIVE_RESTART);
                        gl::Disable(gl::PRIMITIVE_RESTART_FIXED_INDEX);
                    }
                }
                if gl_debug.force_sample_mask_all {
                    unsafe {
                        gl::Disable(gl::SAMPLE_MASK);
                        gl::SampleMaski(0, u32::MAX);
                    }
                }
                if gl_debug.force_no_compat_kill_state {
                    const GL_ALPHA_TEST_COMPAT: u32 = 0x0BC0;
                    unsafe {
                        gl::Disable(gl::COLOR_LOGIC_OP);
                        gl::Disable(GL_ALPHA_TEST_COMPAT);
                        gl::Disable(gl::RASTERIZER_DISCARD);
                    }
                }
                if gl_debug.force_simple_draw_state {
                    if let Some((_, width, height)) = bound_draw_framebuffer {
                        unsafe {
                            gl::Disable(gl::DEPTH_TEST);
                            gl::Disable(gl::STENCIL_TEST);
                            gl::Disable(gl::SCISSOR_TEST);
                            gl::Disable(gl::BLEND);
                            gl::Disable(gl::CULL_FACE);
                            gl::Disable(gl::RASTERIZER_DISCARD);
                            gl::Disable(gl::PRIMITIVE_RESTART);
                            gl::Disable(gl::PRIMITIVE_RESTART_FIXED_INDEX);
                            gl::Disable(gl::SAMPLE_MASK);
                            gl::SampleMaski(0, u32::MAX);
                            for index in 0..8 {
                                gl::Disable(gl::CLIP_DISTANCE0 + index);
                            }
                            gl::ColorMask(gl::TRUE, gl::TRUE, gl::TRUE, gl::TRUE);
                            gl::Viewport(0, 0, width as i32, height as i32);
                        }
                    }
                }
                if gl_debug.force_draw_fbo_validate {
                    unsafe {
                        let _ = gl::CheckFramebufferStatus(gl::DRAW_FRAMEBUFFER);
                    }
                }
                if gl_debug.force_draw_attachment_validate {
                    if let Some((fb, _, _)) = bound_draw_framebuffer {
                        unsafe {
                            let mut attached = 0i32;
                            gl::GetNamedFramebufferAttachmentParameteriv(
                                fb,
                                gl::COLOR_ATTACHMENT0,
                                gl::FRAMEBUFFER_ATTACHMENT_OBJECT_NAME,
                                &mut attached,
                            );
                            if attached > 0 {
                                let mut width = 0i32;
                                gl::GetTextureLevelParameteriv(
                                    attached as u32,
                                    0,
                                    gl::TEXTURE_WIDTH,
                                    &mut width,
                                );
                            }
                        }
                    }
                }
                let trace_draw_state_for_pipeline =
                    should_trace_draw_state(draw_seq, pipeline.program_pipeline_handle());
                if common::trace::is_enabled(common::trace::cat::GL_DRAW_STATE)
                    && trace_draw_state_for_pipeline
                {
                    const GL_ALPHA_TEST_COMPAT: u32 = 0x0BC0;
                    const GL_TRANSFORM_FEEDBACK_PAUSED: u32 = 0x8E23;
                    const GL_TRANSFORM_FEEDBACK_ACTIVE: u32 = 0x8E24;
                    unsafe {
                        let mut draw_buf0 = 0i32;
                        gl::GetIntegerv(gl::DRAW_BUFFER0, &mut draw_buf0);
                        let status = gl::CheckFramebufferStatus(gl::DRAW_FRAMEBUFFER);
                        let bound_fb = bound_draw_framebuffer.map(|(fb, _, _)| fb).unwrap_or(0);
                        let mut attached_obj = 0i32;
                        let mut attached_type = 0i32;
                        let mut attached_level = 0i32;
                        let mut attached_layer = 0i32;
                        let mut attached_layered = 0i32;
                        if bound_fb != 0 {
                            gl::GetNamedFramebufferAttachmentParameteriv(
                                bound_fb,
                                gl::COLOR_ATTACHMENT0,
                                gl::FRAMEBUFFER_ATTACHMENT_OBJECT_NAME,
                                &mut attached_obj,
                            );
                            gl::GetNamedFramebufferAttachmentParameteriv(
                                bound_fb,
                                gl::COLOR_ATTACHMENT0,
                                gl::FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE,
                                &mut attached_type,
                            );
                            gl::GetNamedFramebufferAttachmentParameteriv(
                                bound_fb,
                                gl::COLOR_ATTACHMENT0,
                                gl::FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL,
                                &mut attached_level,
                            );
                            gl::GetNamedFramebufferAttachmentParameteriv(
                                bound_fb,
                                gl::COLOR_ATTACHMENT0,
                                gl::FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER,
                                &mut attached_layer,
                            );
                            gl::GetNamedFramebufferAttachmentParameteriv(
                                bound_fb,
                                gl::COLOR_ATTACHMENT0,
                                gl::FRAMEBUFFER_ATTACHMENT_LAYERED,
                                &mut attached_layered,
                            );
                        }
                        common::trace::emit_raw(
                            common::trace::cat::GL_DRAW_STATE,
                            &[
                                1,
                                draw_seq,
                                pipeline.program_pipeline_handle() as u64,
                                bound_fb as u64,
                                status as u64,
                                draw_buf0 as u64,
                                attached_obj as u64,
                                attached_type as u64,
                                attached_level as u64,
                                attached_layer as u64,
                                attached_layered as u64,
                            ],
                        );

                        let rasterizer_discard = gl::IsEnabled(gl::RASTERIZER_DISCARD);
                        let color_logic_op = gl::IsEnabled(gl::COLOR_LOGIC_OP);
                        let alpha_test = gl::IsEnabled(GL_ALPHA_TEST_COMPAT);
                        let depth_clamp = gl::IsEnabled(gl::DEPTH_CLAMP);
                        let primitive_restart = gl::IsEnabled(gl::PRIMITIVE_RESTART);
                        let primitive_restart_fixed =
                            gl::IsEnabled(gl::PRIMITIVE_RESTART_FIXED_INDEX);
                        let sample_mask = gl::IsEnabled(gl::SAMPLE_MASK);
                        let mut sample_mask_value = 0i32;
                        gl::GetIntegeri_v(gl::SAMPLE_MASK_VALUE, 0, &mut sample_mask_value);
                        let mut clip_mask = 0u32;
                        for index in 0..8 {
                            if gl::IsEnabled(gl::CLIP_DISTANCE0 + index) != 0 {
                                clip_mask |= 1 << index;
                            }
                        }
                        let mut transform_feedback_active = gl::FALSE;
                        let mut transform_feedback_paused = gl::FALSE;
                        gl::GetBooleanv(
                            GL_TRANSFORM_FEEDBACK_ACTIVE,
                            &mut transform_feedback_active,
                        );
                        gl::GetBooleanv(
                            GL_TRANSFORM_FEEDBACK_PAUSED,
                            &mut transform_feedback_paused,
                        );
                        common::trace::emit_raw(
                            common::trace::cat::GL_DRAW_STATE,
                            &[
                                2,
                                draw_seq,
                                pipeline.program_pipeline_handle() as u64,
                                rasterizer_discard as u64,
                                color_logic_op as u64,
                                alpha_test as u64,
                                depth_clamp as u64,
                                primitive_restart as u64,
                                primitive_restart_fixed as u64,
                                sample_mask as u64,
                                sample_mask_value as u32 as u64,
                                clip_mask as u64,
                                transform_feedback_active as u64,
                                transform_feedback_paused as u64,
                            ],
                        );

                        let mut color_mask = [0u8; 4];
                        gl::GetBooleani_v(gl::COLOR_WRITEMASK, 0, color_mask.as_mut_ptr());
                        let color_mask_pack = (color_mask[0] as u64)
                            | ((color_mask[1] as u64) << 8)
                            | ((color_mask[2] as u64) << 16)
                            | ((color_mask[3] as u64) << 24);
                        let blend0 = gl::IsEnabledi(gl::BLEND, 0);
                        let scissor0 = gl::IsEnabledi(gl::SCISSOR_TEST, 0);
                        let depth_test = gl::IsEnabled(gl::DEPTH_TEST);
                        let mut depth_mask = gl::FALSE;
                        gl::GetBooleanv(gl::DEPTH_WRITEMASK, &mut depth_mask);
                        let stencil_test = gl::IsEnabled(gl::STENCIL_TEST);
                        let cull_face = gl::IsEnabled(gl::CULL_FACE);
                        let mut front_face = 0i32;
                        let mut cull_face_mode = 0i32;
                        gl::GetIntegerv(gl::FRONT_FACE, &mut front_face);
                        gl::GetIntegerv(gl::CULL_FACE_MODE, &mut cull_face_mode);
                        let polygon_offset_fill = gl::IsEnabled(gl::POLYGON_OFFSET_FILL);
                        let framebuffer_srgb = gl::IsEnabled(gl::FRAMEBUFFER_SRGB);
                        common::trace::emit_raw(
                            common::trace::cat::GL_DRAW_STATE,
                            &[
                                3,
                                draw_seq,
                                pipeline.program_pipeline_handle() as u64,
                                color_mask_pack,
                                blend0 as u64,
                                scissor0 as u64,
                                depth_test as u64,
                                depth_mask as u64,
                                stencil_test as u64,
                                cull_face as u64,
                                front_face as u64,
                                cull_face_mode as u64,
                                polygon_offset_fill as u64,
                                framebuffer_srgb as u64,
                            ],
                        );
                    }
                }
                if gl_debug.trace_draw_blend_state && trace_draw_state_for_pipeline {
                    unsafe {
                        let mut blend_eq_rgb = 0i32;
                        let mut blend_src_rgb = 0i32;
                        let mut blend_dst_rgb = 0i32;
                        let mut blend_eq_a = 0i32;
                        let mut blend_src_a = 0i32;
                        let mut blend_dst_a = 0i32;
                        gl::GetIntegeri_v(gl::BLEND_EQUATION_RGB, 0, &mut blend_eq_rgb);
                        gl::GetIntegeri_v(gl::BLEND_SRC_RGB, 0, &mut blend_src_rgb);
                        gl::GetIntegeri_v(gl::BLEND_DST_RGB, 0, &mut blend_dst_rgb);
                        gl::GetIntegeri_v(gl::BLEND_EQUATION_ALPHA, 0, &mut blend_eq_a);
                        gl::GetIntegeri_v(gl::BLEND_SRC_ALPHA, 0, &mut blend_src_a);
                        gl::GetIntegeri_v(gl::BLEND_DST_ALPHA, 0, &mut blend_dst_a);
                        log::warn!(
                            "[DRAW_BLEND_STATE] draw_seq={} pipeline={} blend0={} eq_rgb=0x{:X} src_rgb=0x{:X} dst_rgb=0x{:X} eq_a=0x{:X} src_a=0x{:X} dst_a=0x{:X}",
                            draw_seq,
                            pipeline.program_pipeline_handle(),
                            gl::IsEnabledi(gl::BLEND, 0),
                            blend_eq_rgb,
                            blend_src_rgb,
                            blend_dst_rgb,
                            blend_eq_a,
                            blend_src_a,
                            blend_dst_a,
                        );
                    }
                }
                if gl_debug.trace_draw_state_full && trace_draw_state_for_pipeline {
                    unsafe {
                        let mut draw_buf0 = 0i32;
                        gl::GetIntegerv(gl::DRAW_BUFFER0, &mut draw_buf0);
                        let status = gl::CheckFramebufferStatus(gl::DRAW_FRAMEBUFFER);
                        let mut wm = [0i32; 4];
                        gl::GetIntegeri_v(gl::COLOR_WRITEMASK, 0, wm.as_mut_ptr());
                        let mut viewport = [0i32; 4];
                        gl::GetIntegerv(gl::VIEWPORT, viewport.as_mut_ptr());
                        let mut scissor_box = [0i32; 4];
                        gl::GetIntegerv(gl::SCISSOR_BOX, scissor_box.as_mut_ptr());
                        let scissor_enabled = gl::IsEnabled(gl::SCISSOR_TEST);
                        let depth_enabled = gl::IsEnabled(gl::DEPTH_TEST);
                        let cull_enabled = gl::IsEnabled(gl::CULL_FACE);
                        let mut cull_face = 0i32;
                        gl::GetIntegerv(gl::CULL_FACE_MODE, &mut cull_face);
                        let mut front_face = 0i32;
                        gl::GetIntegerv(gl::FRONT_FACE, &mut front_face);
                        let blend_enabled = gl::IsEnabled(gl::BLEND);
                        let mut pipeline_handle = 0i32;
                        gl::GetIntegerv(gl::PROGRAM_PIPELINE_BINDING, &mut pipeline_handle);
                        let mut program = 0i32;
                        gl::GetIntegerv(gl::CURRENT_PROGRAM, &mut program);
                        let mut rasterizer_discard = gl::IsEnabled(gl::RASTERIZER_DISCARD);
                        let mut multisample = gl::IsEnabled(gl::MULTISAMPLE);
                        let a2c_enabled = gl::IsEnabled(gl::SAMPLE_ALPHA_TO_COVERAGE);
                        let a2one_enabled = gl::IsEnabled(gl::SAMPLE_ALPHA_TO_ONE);
                        let sample_coverage = gl::IsEnabled(gl::SAMPLE_COVERAGE);
                        let sample_shading = gl::IsEnabled(gl::SAMPLE_SHADING);
                        let sample_mask = gl::IsEnabled(gl::SAMPLE_MASK);
                        let mut sample_mask_value = 0i32;
                        gl::GetIntegeri_v(gl::SAMPLE_MASK_VALUE, 0, &mut sample_mask_value);
                        let depth_clamp = gl::IsEnabled(gl::DEPTH_CLAMP);
                        let mut clip_origin = 0i32;
                        let mut clip_depth = 0i32;
                        gl::GetIntegerv(gl::CLIP_ORIGIN, &mut clip_origin);
                        gl::GetIntegerv(gl::CLIP_DEPTH_MODE, &mut clip_depth);
                        let mut blend0 = gl::IsEnabledi(gl::BLEND, 0);
                        let mut depth_mask = 0i32;
                        gl::GetIntegerv(gl::DEPTH_WRITEMASK, &mut depth_mask);
                        let bound_fb = bound_draw_framebuffer.map(|(fb, _, _)| fb).unwrap_or(0);
                        let srgb_enabled = gl::IsEnabled(gl::FRAMEBUFFER_SRGB);
                        let mut vao = 0i32;
                        gl::GetIntegerv(gl::VERTEX_ARRAY_BINDING, &mut vao);
                        let polygon_offset_fill = gl::IsEnabled(gl::POLYGON_OFFSET_FILL);
                        let stencil_test = gl::IsEnabled(gl::STENCIL_TEST);
                        let primitive_restart = gl::IsEnabled(gl::PRIMITIVE_RESTART);
                        let primitive_restart_fixed =
                            gl::IsEnabled(gl::PRIMITIVE_RESTART_FIXED_INDEX);
                        let mut clip_mask = 0u32;
                        for index in 0..8 {
                            if gl::IsEnabled(gl::CLIP_DISTANCE0 + index) != 0 {
                                clip_mask |= 1 << index;
                            }
                        }
                        let mut attached_obj_at_draw = 0i32;
                        gl::GetNamedFramebufferAttachmentParameteriv(
                            bound_fb,
                            gl::COLOR_ATTACHMENT0,
                            gl::FRAMEBUFFER_ATTACHMENT_OBJECT_NAME,
                            &mut attached_obj_at_draw,
                        );
                        // verify the attached texture itself
                        let mut tex_width = 0i32;
                        if attached_obj_at_draw > 0 {
                            gl::GetTextureLevelParameteriv(
                                attached_obj_at_draw as u32,
                                0,
                                gl::TEXTURE_WIDTH,
                                &mut tex_width,
                            );
                        }
                        log::warn!(
                            "[DRAW_STATE_EXT] seq={} fb={} srgb={} vao={} polyoff={} stencil={} prest={} prest_fix={} smask={} smask_val=0x{:X} clip_mask=0x{:X} attached={} tex_w={}",
                            draw_seq, bound_fb, srgb_enabled, vao,
                            polygon_offset_fill, stencil_test,
                            primitive_restart, primitive_restart_fixed,
                            sample_mask, sample_mask_value, clip_mask,
                            attached_obj_at_draw, tex_width,
                        );
                        log::warn!(
                            "[DRAW_STATE_FULL] seq={} fb={} drawbuf0=0x{:X} status=0x{:X} wm=[{},{},{},{}] vp=[{},{},{},{}] sc_en={} sc_box=[{},{},{},{}] depth={} dw={} cull={}+0x{:X}/0x{:X} blend={} blend0={} a2c={} a2one={} sc={} ss={} dclamp={} clip=0x{:X}/0x{:X} depth_mode={:?} pipeline={} program={} rast_disc={} ms={}",
                            draw_seq, bound_fb, draw_buf0, status,
                            wm[0], wm[1], wm[2], wm[3],
                            viewport[0], viewport[1], viewport[2], viewport[3],
                            scissor_enabled, scissor_box[0], scissor_box[1], scissor_box[2], scissor_box[3],
                            depth_enabled, depth_mask, cull_enabled, cull_face, front_face,
                            blend_enabled, blend0, a2c_enabled, a2one_enabled,
                            sample_coverage, sample_shading, depth_clamp,
                            clip_origin, clip_depth, draw_view.depth_mode(),
                            pipeline_handle, program, rasterizer_discard, multisample,
                        );
                    }
                }
                if gl_debug.trace_fbo_attach_at_draw {
                    if let Some((fb, _, _)) = bound_draw_framebuffer {
                        unsafe {
                            let mut attached_obj = 0i32;
                            let mut attached_type = 0i32;
                            let mut attached_lvl = 0i32;
                            let mut attached_layer = 0i32;
                            let mut attached_layered = 0i32;
                            gl::GetNamedFramebufferAttachmentParameteriv(
                                fb,
                                gl::COLOR_ATTACHMENT0,
                                gl::FRAMEBUFFER_ATTACHMENT_OBJECT_NAME,
                                &mut attached_obj,
                            );
                            gl::GetNamedFramebufferAttachmentParameteriv(
                                fb,
                                gl::COLOR_ATTACHMENT0,
                                gl::FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE,
                                &mut attached_type,
                            );
                            gl::GetNamedFramebufferAttachmentParameteriv(
                                fb,
                                gl::COLOR_ATTACHMENT0,
                                gl::FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL,
                                &mut attached_lvl,
                            );
                            gl::GetNamedFramebufferAttachmentParameteriv(
                                fb,
                                gl::COLOR_ATTACHMENT0,
                                gl::FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER,
                                &mut attached_layer,
                            );
                            gl::GetNamedFramebufferAttachmentParameteriv(
                                fb,
                                gl::COLOR_ATTACHMENT0,
                                gl::FRAMEBUFFER_ATTACHMENT_LAYERED,
                                &mut attached_layered,
                            );
                            let mut draw_buf0 = 0i32;
                            gl::GetNamedFramebufferParameteriv(
                                fb,
                                gl::DRAW_BUFFER0 as u32,
                                &mut draw_buf0,
                            );
                            let mut default_width = 0i32;
                            let mut default_height = 0i32;
                            gl::GetNamedFramebufferParameteriv(
                                fb,
                                gl::FRAMEBUFFER_DEFAULT_WIDTH,
                                &mut default_width,
                            );
                            gl::GetNamedFramebufferParameteriv(
                                fb,
                                gl::FRAMEBUFFER_DEFAULT_HEIGHT,
                                &mut default_height,
                            );
                            let status = gl::CheckNamedFramebufferStatus(fb, gl::DRAW_FRAMEBUFFER);
                            log::warn!(
                                "[FBO_ATTACH_AT_DRAW] seq={} fb={} attached_obj={} attached_type=0x{:X} lvl={} layer={} layered={} draw_buf0=0x{:X} default={}x{} status=0x{:X}",
                                draw_seq, fb,
                                attached_obj, attached_type, attached_lvl, attached_layer,
                                attached_layered, draw_buf0, default_width, default_height, status,
                            );
                        }
                    }
                }
                if gl_debug.trace_draw_bind_recheck {
                    unsafe {
                        let mut actual_draw_fb = 0i32;
                        gl::GetIntegerv(gl::DRAW_FRAMEBUFFER_BINDING, &mut actual_draw_fb);
                        let expected = bound_draw_framebuffer.map(|(fb, _, _)| fb).unwrap_or(0);
                        if (actual_draw_fb as u32) != expected {
                            log::warn!(
                                "[DRAW_BIND_MISMATCH] seq={} expected_fb={} actual_fb={}",
                                draw_seq,
                                expected,
                                actual_draw_fb,
                            );
                        }
                    }
                }
                if gl_debug.force_vertex_binding_validate {
                    unsafe {
                        let mut buffer = 0i32;
                        gl::GetIntegeri_v(gl::VERTEX_BINDING_BUFFER, 0, &mut buffer);
                    }
                }
                if gl_debug.force_vertex_attrib_validate {
                    unsafe {
                        let mut binding = 0i32;
                        gl::GetVertexAttribiv(0, gl::VERTEX_ATTRIB_BINDING, &mut binding);
                    }
                }
                if gl_debug.trace_guest_ebo_dump {
                    let bound_fb_id = bound_draw_framebuffer.map(|(fb, _, _)| fb).unwrap_or(0);
                    let ib_gpu = draw_view.index_buffer_gpu_addr();
                    let ib_format = draw_state.index_buffer.format;
                    let format_size = ib_format.size_bytes() as usize;
                    let ib_first = draw_state.index_buffer.first;
                    let ib_count = draw_state.index_buffer.count.min(6);
                    if let (Some(mm), Some(reader)) = (
                        self.channel_memory_manager.as_ref(),
                        self.device_memory_reader.as_ref(),
                    ) {
                        let read_addr = ib_gpu + (ib_first as u64) * (format_size as u64);
                        let read_bytes = ib_count as usize * format_size;
                        if read_bytes > 0 && read_bytes <= 64 {
                            let mut buf = vec![0u8; read_bytes];
                            mm.lock().read_block(read_addr, &mut buf);
                            let mut indices = [0u64; 6];
                            for i in 0..ib_count as usize {
                                let off = i * format_size;
                                indices[i] = match format_size {
                                    1 => buf[off] as u64,
                                    2 => u16::from_le_bytes([buf[off], buf[off + 1]]) as u64,
                                    4 => u32::from_le_bytes([
                                        buf[off],
                                        buf[off + 1],
                                        buf[off + 2],
                                        buf[off + 3],
                                    ]) as u64,
                                    _ => 0,
                                };
                            }
                            log::warn!(
                                "[GUEST_EBO] seq={} fbo={} ib_gpu=0x{:X} first={} count={} fmt={:?} sz={} indices=[{},{},{},{},{},{}]",
                                draw_seq, bound_fb_id, ib_gpu, ib_first, ib_count, ib_format, format_size,
                                indices[0], indices[1], indices[2], indices[3], indices[4], indices[5],
                            );
                        }
                    }
                }
                let draw_dump_enabled_for_pipeline = if gl_debug.trace_ebo_dump
                    || gl_debug.trace_vbo_dump
                    || gl_debug.trace_vao_dump
                {
                    static DRAW_DUMP_PIPELINE: OnceLock<Option<u64>> = OnceLock::new();
                    trace_u64_env_cached(&DRAW_DUMP_PIPELINE, "RUZU_TRACE_DRAW_DUMP_PIPELINE")
                        .is_none_or(|target| target as u32 == pipeline.program_pipeline_handle())
                } else {
                    false
                };
                if gl_debug.trace_ebo_dump && draw_dump_enabled_for_pipeline {
                    let bound_fb_id = bound_draw_framebuffer.map(|(fb, _, _)| fb).unwrap_or(0);
                    unsafe {
                        let mut ebo = 0i32;
                        gl::GetIntegerv(gl::ELEMENT_ARRAY_BUFFER_BINDING, &mut ebo);
                        if ebo != 0 {
                            let mut indices = [0u32; 6];
                            gl::GetNamedBufferSubData(
                                ebo as u32,
                                index_offset as isize,
                                (indices.len() * 4) as isize,
                                indices.as_mut_ptr() as *mut _,
                            );
                            log::warn!(
                                "[EBO_DUMP] seq={} fbo={} ebo={} index_offset={} indices=[{},{},{},{},{},{}]",
                                draw_seq, bound_fb_id, ebo, index_offset,
                                indices[0], indices[1], indices[2], indices[3], indices[4], indices[5],
                            );
                        }
                    }
                }
                if gl_debug.trace_vbo_dump && draw_dump_enabled_for_pipeline {
                    let bound_fb_id = bound_draw_framebuffer.map(|(fb, _, _)| fb).unwrap_or(0);
                    unsafe {
                        // Get current VAO + bind state for attribute 0
                        let mut vao = 0i32;
                        gl::GetIntegerv(gl::VERTEX_ARRAY_BINDING, &mut vao);
                        let mut vbo = 0i32;
                        gl::GetVertexAttribiv(0, gl::VERTEX_ATTRIB_ARRAY_BUFFER_BINDING, &mut vbo);
                        let mut stride = 0i32;
                        gl::GetVertexAttribiv(0, gl::VERTEX_ATTRIB_ARRAY_STRIDE, &mut stride);
                        let mut binding_stride = 0i32;
                        gl::GetIntegeri_v(GL_VERTEX_BINDING_STRIDE, 0, &mut binding_stride);
                        let mut size_attr = 0i32;
                        gl::GetVertexAttribiv(0, gl::VERTEX_ATTRIB_ARRAY_SIZE, &mut size_attr);
                        let mut type_attr = 0i32;
                        gl::GetVertexAttribiv(0, gl::VERTEX_ATTRIB_ARRAY_TYPE, &mut type_attr);
                        let mut binding_offset = 0i64;
                        gl::GetInteger64i_v(gl::VERTEX_BINDING_OFFSET, 0, &mut binding_offset);
                        let mut ebo = 0i32;
                        gl::GetIntegerv(gl::ELEMENT_ARRAY_BUFFER_BINDING, &mut ebo);
                        let read_bytes = 48usize;
                        let mut data = vec![0u8; read_bytes];
                        if vbo != 0 {
                            gl::GetNamedBufferSubData(
                                vbo as u32,
                                binding_offset as isize,
                                read_bytes as isize,
                                data.as_mut_ptr() as *mut _,
                            );
                        }
                        let nonzero = data.iter().filter(|&&b| b != 0).count();
                        let mut floats = [0.0f32; 12];
                        for (i, chunk) in data.chunks_exact(4).take(12).enumerate() {
                            floats[i] =
                                f32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]);
                        }
                        let mut strided_pos = [[0.0f32; 3]; 4];
                        let mut strided_attr1 = [[0.0f32; 2]; 4];
                        if vbo != 0 && binding_stride > 0 {
                            for vertex in 0..4 {
                                let mut bytes = [0u8; 32];
                                gl::GetNamedBufferSubData(
                                    vbo as u32,
                                    (binding_offset + (vertex as i64 * binding_stride as i64))
                                        as isize,
                                    bytes.len() as isize,
                                    bytes.as_mut_ptr() as *mut _,
                                );
                                for component in 0..3 {
                                    let start = component * 4;
                                    strided_pos[vertex][component] = f32::from_le_bytes([
                                        bytes[start],
                                        bytes[start + 1],
                                        bytes[start + 2],
                                        bytes[start + 3],
                                    ]);
                                }
                                for component in 0..2 {
                                    let start = 24 + component * 4;
                                    strided_attr1[vertex][component] = f32::from_le_bytes([
                                        bytes[start],
                                        bytes[start + 1],
                                        bytes[start + 2],
                                        bytes[start + 3],
                                    ]);
                                }
                            }
                        }
                        log::warn!(
                            "[VBO_DUMP] seq={} fbo={} vao={} vbo={} ebo={} attrib_stride={} binding_stride={} size={} type=0x{:X} offset={} nonzero={}/{} raw0=[{:.2},{:.2}] raw1=[{:.2},{:.2}] pos0=[{:.2},{:.2},{:.2}] pos1=[{:.2},{:.2},{:.2}] pos2=[{:.2},{:.2},{:.2}] pos3=[{:.2},{:.2},{:.2}] uv0=[{:.3},{:.3}] uv1=[{:.3},{:.3}] uv2=[{:.3},{:.3}] uv3=[{:.3},{:.3}]",
                            draw_seq, bound_fb_id, vao, vbo, ebo, stride, binding_stride, size_attr, type_attr, binding_offset,
                            nonzero, read_bytes,
                            floats[0], floats[1],
                            floats[2], floats[3],
                            strided_pos[0][0], strided_pos[0][1], strided_pos[0][2],
                            strided_pos[1][0], strided_pos[1][1], strided_pos[1][2],
                            strided_pos[2][0], strided_pos[2][1], strided_pos[2][2],
                            strided_pos[3][0], strided_pos[3][1], strided_pos[3][2],
                            strided_attr1[0][0], strided_attr1[0][1],
                            strided_attr1[1][0], strided_attr1[1][1],
                            strided_attr1[2][0], strided_attr1[2][1],
                            strided_attr1[3][0], strided_attr1[3][1],
                        );
                    }
                }
                if gl_debug.trace_vao_dump && draw_dump_enabled_for_pipeline {
                    let bound_fb_id = bound_draw_framebuffer.map(|(fb, _, _)| fb).unwrap_or(0);
                    unsafe {
                        let mut vao = 0i32;
                        gl::GetIntegerv(gl::VERTEX_ARRAY_BINDING, &mut vao);
                        for attrib in 0..8 {
                            let mut enabled = 0i32;
                            let mut size = 0i32;
                            let mut ty = 0i32;
                            let mut normalized = 0i32;
                            let mut relative_offset = 0i32;
                            let mut binding = 0i32;
                            gl::GetVertexAttribiv(
                                attrib,
                                gl::VERTEX_ATTRIB_ARRAY_ENABLED,
                                &mut enabled,
                            );
                            gl::GetVertexAttribiv(attrib, gl::VERTEX_ATTRIB_ARRAY_SIZE, &mut size);
                            gl::GetVertexAttribiv(attrib, gl::VERTEX_ATTRIB_ARRAY_TYPE, &mut ty);
                            gl::GetVertexAttribiv(
                                attrib,
                                gl::VERTEX_ATTRIB_ARRAY_NORMALIZED,
                                &mut normalized,
                            );
                            gl::GetVertexAttribiv(
                                attrib,
                                gl::VERTEX_ATTRIB_RELATIVE_OFFSET,
                                &mut relative_offset,
                            );
                            gl::GetVertexAttribiv(attrib, gl::VERTEX_ATTRIB_BINDING, &mut binding);
                            if enabled == 0 {
                                continue;
                            }
                            let mut buffer = 0i32;
                            let mut offset = 0i64;
                            let mut stride = 0i32;
                            let mut divisor = 0i32;
                            gl::GetIntegeri_v(
                                gl::VERTEX_BINDING_BUFFER,
                                binding as u32,
                                &mut buffer,
                            );
                            gl::GetInteger64i_v(
                                gl::VERTEX_BINDING_OFFSET,
                                binding as u32,
                                &mut offset,
                            );
                            gl::GetIntegeri_v(
                                gl::VERTEX_BINDING_STRIDE,
                                binding as u32,
                                &mut stride,
                            );
                            gl::GetIntegeri_v(
                                gl::VERTEX_BINDING_DIVISOR,
                                binding as u32,
                                &mut divisor,
                            );
                            log::warn!(
                                "[VAO_DUMP] seq={} fbo={} vao={} attrib={} binding={} buffer={} offset={} stride={} divisor={} size={} type=0x{:X} norm={} rel_off={}",
                                draw_seq,
                                bound_fb_id,
                                vao,
                                attrib,
                                binding,
                                buffer,
                                offset,
                                stride,
                                divisor,
                                size,
                                ty,
                                normalized,
                                relative_offset
                            );
                        }
                    }
                }
                if gl_debug.trace_sampler_dump {
                    let bound_fb_id = bound_draw_framebuffer.map(|(fb, _, _)| fb).unwrap_or(0);
                    unsafe {
                        for unit in 0..4 {
                            gl::ActiveTexture(gl::TEXTURE0 + unit);
                            let mut t2d = 0i32;
                            gl::GetIntegerv(gl::TEXTURE_BINDING_2D, &mut t2d);
                            let mut t2da = 0i32;
                            gl::GetIntegerv(gl::TEXTURE_BINDING_2D_ARRAY, &mut t2da);
                            let mut sampler = 0i32;
                            gl::GetIntegerv(gl::SAMPLER_BINDING, &mut sampler);
                            if t2d != 0 || t2da != 0 {
                                log::warn!(
                                    "[SAMPLER] seq={} fb={} unit={} tex_2d={} tex_2d_array={} sampler={}",
                                    draw_seq, bound_fb_id, unit, t2d, t2da, sampler,
                                );
                            }
                        }
                    }
                }
                if gl_debug.trace_ubo_dump && trace_draw_state_for_pipeline {
                    let bound_fb_id = bound_draw_framebuffer.map(|(fb, _, _)| fb).unwrap_or(0);
                    unsafe {
                        for binding_idx in 0..8 {
                            let mut ubo_handle = 0i32;
                            let mut offset = 0i64;
                            let mut size = 0i64;
                            gl::GetIntegeri_v(
                                gl::UNIFORM_BUFFER_BINDING,
                                binding_idx,
                                &mut ubo_handle,
                            );
                            if ubo_handle == 0 {
                                continue;
                            }
                            gl::GetInteger64i_v(gl::UNIFORM_BUFFER_START, binding_idx, &mut offset);
                            gl::GetInteger64i_v(gl::UNIFORM_BUFFER_SIZE, binding_idx, &mut size);
                            let read_bytes = (size as usize).min(544);
                            let mut data = vec![0u8; read_bytes];
                            if read_bytes > 0 {
                                gl::GetNamedBufferSubData(
                                    ubo_handle as u32,
                                    offset as isize,
                                    read_bytes as isize,
                                    data.as_mut_ptr() as *mut _,
                                );
                            }
                            let nonzero = data.iter().filter(|&&b| b != 0).count();
                            let mut floats = [0.0f32; 136];
                            for (i, chunk) in data.chunks_exact(4).take(136).enumerate() {
                                floats[i] =
                                    f32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]);
                            }
                            log::warn!(
                                "[UBO_DUMP] seq={} pipeline={} fbo={} binding={} ubo={} offset={} size={} nonzero={}/{} f4[0]=[{:.3},{:.3},{:.3},{:.3}] f4[1]=[{:.3},{:.3},{:.3},{:.3}] f4[2]=[{:.3},{:.3},{:.3},{:.3}] f4[3]=[{:.3},{:.3},{:.3},{:.3}] f4[4]=[{:.3},{:.3},{:.3},{:.3}] f4[28]=[{:.3},{:.3},{:.3},{:.3}] f4[29]=[{:.3},{:.3},{:.3},{:.3}] f4[30]=[{:.3},{:.3},{:.3},{:.3}] f4[31]=[{:.3},{:.3},{:.3},{:.3}] f4[32]=[{:.3},{:.3},{:.3},{:.3}] f4[33]=[{:.3},{:.3},{:.3},{:.3}]",
                                draw_seq, pipeline.program_pipeline_handle(), bound_fb_id, binding_idx, ubo_handle, offset, size,
                                nonzero, read_bytes,
                                floats[0], floats[1], floats[2], floats[3],
                                floats[4], floats[5], floats[6], floats[7],
                                floats[8], floats[9], floats[10], floats[11],
                                floats[12], floats[13], floats[14], floats[15],
                                floats[16], floats[17], floats[18], floats[19],
                                floats[112], floats[113], floats[114], floats[115],
                                floats[116], floats[117], floats[118], floats[119],
                                floats[120], floats[121], floats[122], floats[123],
                                floats[124], floats[125], floats[126], floats[127],
                                floats[128], floats[129], floats[130], floats[131],
                                floats[132], floats[133], floats[134], floats[135],
                            );
                        }
                    }
                }
                let trace_any_samples = gl_debug.trace_any_samples_passed
                    && (trace_draw_state_for_pipeline || trace_any_samples_rt);
                if common::trace::is_enabled(common::trace::cat::GL_DRAW_STATE)
                    && trace_draw_state_for_pipeline
                {
                    common::trace::emit_raw(
                        common::trace::cat::GL_DRAW_STATE,
                        &[
                            5,
                            draw_seq,
                            pipeline.program_pipeline_handle() as u64,
                            1,
                            primitive_mode as u64,
                            num_vertices as u64,
                            num_instances as u64,
                            base_vertex as u32 as u64,
                            base_instance as u64,
                            index_offset as u64,
                            index_format as u64,
                            draw_state.vertex_buffer.first as u64,
                            draw_state.vertex_buffer.count as u64,
                            draw_state.index_buffer.first as u64,
                        ],
                    );
                }
                if gl_debug.trace_rt_grid_phase && trace_rt_sample_this_draw {
                    if let Some((framebuffer, width, height)) = bound_draw_framebuffer {
                        let rt0 = draw_view.render_targets().render_targets[0];
                        if rt0.address != 0 && should_sample_rt_address(rt0.address) {
                            unsafe {
                                emit_rt_grid_phase(
                                    0,
                                    draw_seq,
                                    pipeline_handle,
                                    framebuffer,
                                    rt0.address,
                                    width,
                                    height,
                                );
                            }
                        }
                    }
                }
                if skip_draw_due_to_sampling_addr {
                    log::warn!(
                        "[SKIP_DRAW_SAMPLING_GPU_ADDR] draw_seq={} pipeline={} indexed=1 prim=0x{:X} verts={} instances={}",
                        draw_seq,
                        pipeline.program_pipeline_handle(),
                        primitive_mode,
                        num_vertices,
                        num_instances,
                    );
                } else {
                    unsafe {
                        let mut any_samples_query = 0u32;
                        if trace_any_samples {
                            while gl::GetError() != gl::NO_ERROR {}
                            gl::GenQueries(1, &mut any_samples_query);
                            if any_samples_query != 0 {
                                gl::BeginQuery(gl::SAMPLES_PASSED, any_samples_query);
                            }
                        }
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
                        let mut any_samples = 0u64;
                        if any_samples_query != 0 {
                            gl::EndQuery(gl::SAMPLES_PASSED);
                            gl::GetQueryObjectui64v(
                                any_samples_query,
                                gl::QUERY_RESULT,
                                &mut any_samples,
                            );
                            gl::DeleteQueries(1, &any_samples_query);
                        }
                        let gl_error_after_draw = if trace_any_samples {
                            gl::GetError()
                        } else {
                            gl::NO_ERROR
                        };
                        if trace_any_samples {
                            common::trace::emit_raw(
                                common::trace::cat::GL_DRAW_STATE,
                                &[
                                    4,
                                    draw_seq,
                                    pipeline.program_pipeline_handle() as u64,
                                    (any_samples_query != 0) as u64,
                                    (any_samples != 0) as u64,
                                    gl_error_after_draw as u64,
                                    1,
                                    primitive_mode as u64,
                                    num_vertices as u64,
                                    num_instances as u64,
                                ],
                            );
                        }
                        if trace_any_samples_rt {
                            if let Some((framebuffer, width, height)) = bound_draw_framebuffer {
                                let rt0 = draw_view.render_targets().render_targets[0];
                                common::trace::emit_raw(
                                    common::trace::cat::RT_BIND,
                                    &[
                                        u64::MAX - 1,
                                        draw_seq,
                                        pipeline.program_pipeline_handle() as u64,
                                        (any_samples_query != 0) as u64,
                                        (any_samples != 0) as u64,
                                        gl_error_after_draw as u64,
                                        1,
                                        primitive_mode as u64,
                                        num_vertices as u64,
                                        num_instances as u64,
                                        ((width as u64) << 32) | height as u64,
                                        rt0.address,
                                        framebuffer as u64,
                                    ],
                                );
                            }
                        }
                    }
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
                let trace_draw_state_for_pipeline =
                    should_trace_draw_state(draw_seq, pipeline.program_pipeline_handle());
                let trace_any_samples = gl_debug.trace_any_samples_passed
                    && (trace_draw_state_for_pipeline || trace_any_samples_rt);
                if common::trace::is_enabled(common::trace::cat::GL_DRAW_STATE)
                    && trace_draw_state_for_pipeline
                {
                    common::trace::emit_raw(
                        common::trace::cat::GL_DRAW_STATE,
                        &[
                            5,
                            draw_seq,
                            pipeline.program_pipeline_handle() as u64,
                            0,
                            primitive_mode as u64,
                            num_vertices as u64,
                            num_instances as u64,
                            base_vertex as u32 as u64,
                            base_instance as u64,
                            0,
                            0,
                            draw_state.vertex_buffer.first as u64,
                            draw_state.vertex_buffer.count as u64,
                            0,
                        ],
                    );
                }
                if gl_debug.trace_rt_grid_phase && trace_rt_sample_this_draw {
                    if let Some((framebuffer, width, height)) = bound_draw_framebuffer {
                        let rt0 = draw_view.render_targets().render_targets[0];
                        if rt0.address != 0 && should_sample_rt_address(rt0.address) {
                            unsafe {
                                emit_rt_grid_phase(
                                    0,
                                    draw_seq,
                                    pipeline_handle,
                                    framebuffer,
                                    rt0.address,
                                    width,
                                    height,
                                );
                            }
                        }
                    }
                }
                if skip_draw_due_to_sampling_addr {
                    log::warn!(
                        "[SKIP_DRAW_SAMPLING_GPU_ADDR] draw_seq={} pipeline={} indexed=0 prim=0x{:X} verts={} instances={}",
                        draw_seq,
                        pipeline.program_pipeline_handle(),
                        primitive_mode,
                        num_vertices,
                        num_instances,
                    );
                } else {
                    unsafe {
                        let mut any_samples_query = 0u32;
                        if trace_any_samples {
                            while gl::GetError() != gl::NO_ERROR {}
                            gl::GenQueries(1, &mut any_samples_query);
                            if any_samples_query != 0 {
                                gl::BeginQuery(gl::SAMPLES_PASSED, any_samples_query);
                            }
                        }
                        gl::BindVertexArray(self.transient_vao);
                        gl::DrawArraysInstancedBaseInstance(
                            primitive_mode,
                            base_vertex,
                            num_vertices as i32,
                            num_instances as i32,
                            base_instance,
                        );
                        let mut any_samples = 0u64;
                        if any_samples_query != 0 {
                            gl::EndQuery(gl::SAMPLES_PASSED);
                            gl::GetQueryObjectui64v(
                                any_samples_query,
                                gl::QUERY_RESULT,
                                &mut any_samples,
                            );
                            gl::DeleteQueries(1, &any_samples_query);
                        }
                        let gl_error_after_draw = if trace_any_samples {
                            gl::GetError()
                        } else {
                            gl::NO_ERROR
                        };
                        if trace_any_samples {
                            common::trace::emit_raw(
                                common::trace::cat::GL_DRAW_STATE,
                                &[
                                    4,
                                    draw_seq,
                                    pipeline.program_pipeline_handle() as u64,
                                    (any_samples_query != 0) as u64,
                                    (any_samples != 0) as u64,
                                    gl_error_after_draw as u64,
                                    0,
                                    primitive_mode as u64,
                                    num_vertices as u64,
                                    num_instances as u64,
                                ],
                            );
                        }
                        if trace_any_samples_rt {
                            if let Some((framebuffer, width, height)) = bound_draw_framebuffer {
                                let rt0 = draw_view.render_targets().render_targets[0];
                                common::trace::emit_raw(
                                    common::trace::cat::RT_BIND,
                                    &[
                                        u64::MAX - 1,
                                        draw_seq,
                                        pipeline.program_pipeline_handle() as u64,
                                        (any_samples_query != 0) as u64,
                                        (any_samples != 0) as u64,
                                        gl_error_after_draw as u64,
                                        0,
                                        primitive_mode as u64,
                                        num_vertices as u64,
                                        num_instances as u64,
                                        ((width as u64) << 32) | height as u64,
                                        rt0.address,
                                        framebuffer as u64,
                                    ],
                                );
                            }
                        }
                    }
                }
            } else {
                debug!(
                    "RasterizerOpenGL::draw arrays prim=0x{:X} verts={} instances={} \
                     base_vertex={} base_instance={} — placeholder pipeline, no GL draw",
                    primitive_mode, num_vertices, num_instances, base_vertex, base_instance
                );
            }
        }
        if let Some(sync_draw_step) = sync_draw_step {
            profile_sync_draw_us = trace_elapsed_us(sync_draw_step);
        }
        if can_draw_gl && common::trace::is_enabled(common::trace::cat::RT_SAMPLE) {
            if !trace_rt_sample_this_draw {
                // Keep the category enabled globally while avoiding heavy GL
                // readbacks on unrelated draws.
            } else if let Some((framebuffer, width, height)) = bound_draw_framebuffer {
                let rt0 = draw_view.render_targets().render_targets[0];
                if rt0.address != 0 && should_sample_rt_address(rt0.address) {
                    unsafe {
                        if gl_debug.trace_rt_grid_phase {
                            emit_rt_grid_phase(
                                1,
                                draw_seq,
                                pipeline_handle,
                                framebuffer,
                                rt0.address,
                                width,
                                height,
                            );
                        }
                        let sample_width = width.min(4) as i32;
                        let sample_height = height.min(4) as i32;
                        let mut old_read_fb = 0;
                        let mut old_pack_buffer = 0;
                        let mut old_pack_alignment = 0;
                        let mut old_pack_row_length = 0;
                        gl::GetIntegerv(gl::READ_FRAMEBUFFER_BINDING, &mut old_read_fb);
                        gl::GetIntegerv(gl::PIXEL_PACK_BUFFER_BINDING, &mut old_pack_buffer);
                        gl::GetIntegerv(gl::PACK_ALIGNMENT, &mut old_pack_alignment);
                        gl::GetIntegerv(gl::PACK_ROW_LENGTH, &mut old_pack_row_length);
                        gl::BindFramebuffer(gl::READ_FRAMEBUFFER, framebuffer);
                        gl::ReadBuffer(gl::COLOR_ATTACHMENT0);
                        gl::BindBuffer(gl::PIXEL_PACK_BUFFER, 0);
                        gl::PixelStorei(gl::PACK_ALIGNMENT, 1);
                        gl::PixelStorei(gl::PACK_ROW_LENGTH, 0);

                        while gl::GetError() != gl::NO_ERROR {}
                        let mut pixels = [0u8; 4 * 4 * 4];
                        gl::ReadPixels(
                            0,
                            0,
                            sample_width,
                            sample_height,
                            gl::RGBA,
                            gl::UNSIGNED_BYTE,
                            pixels.as_mut_ptr() as *mut _,
                        );
                        let gl_error = gl::GetError();
                        static RT_PROBE_X: OnceLock<Option<u64>> = OnceLock::new();
                        static RT_PROBE_Y: OnceLock<Option<u64>> = OnceLock::new();
                        let probe_x = trace_u64_env_cached(&RT_PROBE_X, "RUZU_TRACE_RT_PROBE_X")
                            .unwrap_or(0) as u32;
                        let probe_y = trace_u64_env_cached(&RT_PROBE_Y, "RUZU_TRACE_RT_PROBE_Y")
                            .unwrap_or(0) as u32;
                        let probe_x = probe_x.min(width.saturating_sub(1));
                        let probe_y = probe_y.min(height.saturating_sub(1));
                        let mut probe = [0u8; 4];
                        if gl_error == gl::NO_ERROR && width != 0 && height != 0 {
                            gl::ReadPixels(
                                probe_x as i32,
                                probe_y as i32,
                                1,
                                1,
                                gl::RGBA,
                                gl::UNSIGNED_BYTE,
                                probe.as_mut_ptr() as *mut _,
                            );
                        }
                        let gl_error = if gl_error == gl::NO_ERROR {
                            gl::GetError()
                        } else {
                            gl_error
                        };
                        let trace_grid = gl_debug.trace_rt_grid
                            && gl_error == gl::NO_ERROR
                            && width > 0
                            && height > 0;
                        let mut grid_hit_cells = 0u64;
                        let mut grid_nonzero_bytes = 0u64;
                        let mut first_hit_xy = u64::MAX;
                        let mut last_hit_xy = u64::MAX;
                        let mut first_hit_rgba = 0u64;
                        let mut last_hit_rgba = 0u64;
                        if trace_grid {
                            let pack_xy = |x: u32, y: u32| -> u64 { ((x as u64) << 32) | y as u64 };
                            for gy in 0..8u32 {
                                for gx in 0..8u32 {
                                    let x = gx * width.saturating_sub(1) / 7;
                                    let y = gy * height.saturating_sub(1) / 7;
                                    let mut px = [0u8; 4];
                                    gl::ReadPixels(
                                        x as i32,
                                        y as i32,
                                        1,
                                        1,
                                        gl::RGBA,
                                        gl::UNSIGNED_BYTE,
                                        px.as_mut_ptr() as *mut _,
                                    );
                                    let nz = px.iter().filter(|&&byte| byte != 0).count() as u64;
                                    if nz != 0 {
                                        let xy = pack_xy(x, y);
                                        let rgba = u32::from_le_bytes(px) as u64;
                                        grid_hit_cells += 1;
                                        grid_nonzero_bytes += nz;
                                        if first_hit_xy == u64::MAX {
                                            first_hit_xy = xy;
                                            first_hit_rgba = rgba;
                                        }
                                        last_hit_xy = xy;
                                        last_hit_rgba = rgba;
                                    }
                                }
                            }
                            if first_hit_xy == u64::MAX {
                                first_hit_xy = 0;
                                last_hit_xy = 0;
                            }
                        }

                        gl::BindFramebuffer(gl::READ_FRAMEBUFFER, old_read_fb as u32);
                        gl::BindBuffer(gl::PIXEL_PACK_BUFFER, old_pack_buffer as u32);
                        gl::PixelStorei(gl::PACK_ALIGNMENT, old_pack_alignment);
                        gl::PixelStorei(gl::PACK_ROW_LENGTH, old_pack_row_length);

                        let sample_len = (sample_width * sample_height * 4) as usize;
                        let mut rgb_nonzero = 0u64;
                        let mut alpha_nonzero = 0u64;
                        for px in pixels[..sample_len].chunks_exact(4) {
                            rgb_nonzero +=
                                px[0..3].iter().filter(|&&byte| byte != 0).count() as u64;
                            alpha_nonzero += u64::from(px[3] != 0);
                        }
                        let checksum = pixels[..sample_len]
                            .iter()
                            .fold(0u64, |acc, &byte| acc.wrapping_mul(16777619) ^ byte as u64);
                        let probe_rgba = u32::from_le_bytes(probe);
                        common::trace::emit_raw(
                            common::trace::cat::RT_SAMPLE,
                            &[
                                draw_seq,
                                pipeline_handle,
                                framebuffer as u64,
                                rt0.address,
                                rt0.format as u64,
                                width as u64,
                                height as u64,
                                probe_x as u64,
                                probe_y as u64,
                                rgb_nonzero,
                                alpha_nonzero,
                                checksum,
                                probe_rgba as u64,
                                gl_error as u64,
                            ],
                        );
                        if trace_grid {
                            common::trace::emit_raw(
                                common::trace::cat::RT_GRID,
                                &[
                                    draw_seq,
                                    pipeline_handle,
                                    framebuffer as u64,
                                    rt0.address,
                                    width as u64,
                                    height as u64,
                                    8,
                                    8,
                                    grid_hit_cells,
                                    grid_nonzero_bytes,
                                    first_hit_xy,
                                    first_hit_rgba,
                                    last_hit_xy,
                                    last_hit_rgba,
                                ],
                            );
                        }
                    }
                }
            }
        }
        if trace_samples {
            unsafe {
                gl::EndQuery(gl::SAMPLES_PASSED);
                let mut samples = 0u64;
                gl::GetQueryObjectui64v(samples_query, gl::QUERY_RESULT, &mut samples);
                gl::DeleteQueries(1, &samples_query);
                let bound_fb_id = bound_draw_framebuffer.map(|(fb, _, _)| fb).unwrap_or(0);
                log::warn!(
                    "[SAMPLES_PASSED] seq={} fbo={} samples={} indexed={} verts={} instances={} base_instance={}",
                    draw_seq,
                    bound_fb_id,
                    samples,
                    is_indexed,
                    if is_indexed {
                        draw_state.index_buffer.count
                    } else {
                        draw_state.vertex_buffer.count
                    },
                    num_instances,
                    base_instance
                );
            }
        }
        if can_draw_gl && gl_debug.trace_direct_tex_read {
            if let Some((framebuffer, _, _)) = bound_draw_framebuffer {
                unsafe {
                    gl::Finish();
                    let mut attached = 0i32;
                    gl::GetNamedFramebufferAttachmentParameteriv(
                        framebuffer,
                        gl::COLOR_ATTACHMENT0,
                        gl::FRAMEBUFFER_ATTACHMENT_OBJECT_NAME,
                        &mut attached,
                    );
                    if attached > 0 {
                        let mut pixels = [0u8; 16]; // 4 pixels worth
                        gl::GetTextureSubImage(
                            attached as u32,
                            0,
                            0,
                            0,
                            0,
                            2,
                            2,
                            1,
                            gl::RGBA,
                            gl::UNSIGNED_BYTE,
                            pixels.len() as i32,
                            pixels.as_mut_ptr() as *mut _,
                        );
                        let err = gl::GetError();
                        let nz = pixels.iter().filter(|&&b| b != 0).count();
                        log::warn!(
                            "[DIRECT_TEX] seq={} fb={} attached={} px=[{:02X?}] nonzero={} err=0x{:X}",
                            draw_seq, framebuffer, attached, &pixels[..8], nz, err,
                        );
                    }
                }
            }
        }
        if can_draw_gl && gl_debug.trace_draw_readback {
            if let Some((framebuffer, w, h)) = bound_draw_framebuffer {
                unsafe {
                    gl::Finish();
                    let mut prev_read_fb = 0i32;
                    gl::GetIntegerv(gl::READ_FRAMEBUFFER_BINDING, &mut prev_read_fb);
                    gl::BindFramebuffer(gl::READ_FRAMEBUFFER, framebuffer);
                    let mut grid_nonzero_cells = 0u32;
                    let mut first_hit: (i32, i32, [u8; 4]) = (-1, -1, [0; 4]);
                    let mut last_hit: (i32, i32, [u8; 4]) = (-1, -1, [0; 4]);
                    let mut total_grid_nonzero_bytes = 0u32;
                    for gy in 0..8 {
                        for gx in 0..8 {
                            let x = (gx * (w as i32) / 8).max(0);
                            let y = (gy * (h as i32) / 8).max(0);
                            let mut px = [0u8; 4];
                            gl::ReadPixels(
                                x,
                                y,
                                1,
                                1,
                                gl::RGBA,
                                gl::UNSIGNED_BYTE,
                                px.as_mut_ptr() as *mut _,
                            );
                            let nz_bytes = px.iter().filter(|&&b| b != 0).count() as u32;
                            if nz_bytes > 0 {
                                grid_nonzero_cells += 1;
                                total_grid_nonzero_bytes += nz_bytes;
                                if first_hit.0 < 0 {
                                    first_hit = (x, y, px);
                                }
                                last_hit = (x, y, px);
                            }
                        }
                    }
                    let err = gl::GetError();
                    log::warn!(
                        "[DRAW_READBACK] seq={} fbo={} grid_cells_with_pixels={}/64 nz_bytes={} first_hit=({},{}):[{:02X}{:02X}{:02X}{:02X}] last_hit=({},{}):[{:02X}{:02X}{:02X}{:02X}] err=0x{:X}",
                        draw_seq, framebuffer,
                        grid_nonzero_cells, total_grid_nonzero_bytes,
                        first_hit.0, first_hit.1,
                        first_hit.2[0], first_hit.2[1], first_hit.2[2], first_hit.2[3],
                        last_hit.0, last_hit.1,
                        last_hit.2[0], last_hit.2[1], last_hit.2[2], last_hit.2[3],
                        err,
                    );
                    gl::BindFramebuffer(gl::READ_FRAMEBUFFER, prev_read_fb as u32);
                }
            }
        }
        if can_draw_gl && gl_debug.trace_gl_draw_error {
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
        let rt_readback = gl_debug.trace_rt_readback
            && should_trace_rt_sample_draw(pipeline.program_pipeline_handle() as u64, draw_seq);
        static DUMP_RT_ADDR: OnceLock<Option<u64>> = OnceLock::new();
        static DUMP_RT_SEQ: OnceLock<Option<u64>> = OnceLock::new();
        static DUMP_RT_LIMIT: OnceLock<Option<u64>> = OnceLock::new();
        static DUMP_RT_TIME_START: OnceLock<Option<u64>> = OnceLock::new();
        static DUMP_RT_TIME_END: OnceLock<Option<u64>> = OnceLock::new();
        static DUMP_RT_COUNT: AtomicU64 = AtomicU64::new(0);
        let rt0_addr_for_debug = draw_view.render_targets().render_targets[0].address;
        let rt_dump_elapsed_ms = trace_elapsed_ms();
        let rt_dump_in_time_window = rt_dump_elapsed_ms
            >= trace_u64_env_cached(&DUMP_RT_TIME_START, "RUZU_DUMP_RT_TIME_START_MS").unwrap_or(0)
            && rt_dump_elapsed_ms
                <= trace_u64_env_cached(&DUMP_RT_TIME_END, "RUZU_DUMP_RT_TIME_END_MS")
                    .unwrap_or(u64::MAX);
        let should_dump_rt_addr_unlimited =
            trace_u64_env_cached(&DUMP_RT_ADDR, "RUZU_DUMP_RT_ADDR") == Some(rt0_addr_for_debug);
        let should_dump_rt_addr = should_dump_rt_addr_unlimited
            && rt_dump_in_time_window
            && DUMP_RT_COUNT.load(Ordering::Relaxed)
                < trace_u64_env_cached(&DUMP_RT_LIMIT, "RUZU_DUMP_RT_LIMIT").unwrap_or(u64::MAX);
        let should_dump_rt_seq = trace_u64_env_cached(&DUMP_RT_SEQ, "RUZU_DUMP_RT_SEQ")
            == Some(draw_seq)
            && rt_dump_in_time_window;
        let should_trace_summary = if trace_draw_summary {
            static SUMMARY_LIMIT: OnceLock<Option<u64>> = OnceLock::new();
            static SUMMARY_SEQ_MIN: OnceLock<Option<u64>> = OnceLock::new();
            static SUMMARY_SEQ_MAX: OnceLock<Option<u64>> = OnceLock::new();
            static SUMMARY_RT_ADDR: OnceLock<Option<u64>> = OnceLock::new();
            static SUMMARY_TIME_START: OnceLock<Option<u64>> = OnceLock::new();
            static SUMMARY_TIME_END: OnceLock<Option<u64>> = OnceLock::new();
            let summary_limit =
                trace_u64_env_cached(&SUMMARY_LIMIT, "RUZU_TRACE_DRAW_SUMMARY_LIMIT").unwrap_or(64);
            let summary_seq_min =
                trace_u64_env_cached(&SUMMARY_SEQ_MIN, "RUZU_TRACE_DRAW_SUMMARY_SEQ_MIN")
                    .unwrap_or(0);
            let summary_seq_max =
                trace_u64_env_cached(&SUMMARY_SEQ_MAX, "RUZU_TRACE_DRAW_SUMMARY_SEQ_MAX")
                    .unwrap_or(u64::MAX);
            let summary_rt_addr =
                trace_u64_env_cached(&SUMMARY_RT_ADDR, "RUZU_TRACE_DRAW_SUMMARY_RT_ADDR");
            let summary_elapsed_ms = trace_elapsed_ms();
            let summary_time_start =
                trace_u64_env_cached(&SUMMARY_TIME_START, "RUZU_TRACE_DRAW_SUMMARY_TIME_START_MS")
                    .unwrap_or(0);
            let summary_time_end =
                trace_u64_env_cached(&SUMMARY_TIME_END, "RUZU_TRACE_DRAW_SUMMARY_TIME_END_MS")
                    .unwrap_or(u64::MAX);
            summary_rt_addr.is_none_or(|target| target == rt0_addr_for_debug)
                && draw_seq >= summary_seq_min
                && draw_seq <= summary_seq_max
                && draw_seq.saturating_sub(summary_seq_min) < summary_limit
                && summary_elapsed_ms >= summary_time_start
                && summary_elapsed_ms <= summary_time_end
        } else {
            false
        };
        if should_trace_summary {
            unsafe {
                let mut draw_fbo = 0;
                let mut pipeline_handle = 0;
                let mut viewport = [0; 4];
                let mut scissor_box = [0; 4];
                let mut color_mask = [0; 4];
                let mut depth_test = 0;
                let mut cull_face = 0;
                let mut scissor_test = 0;
                let mut blend_enabled = 0;
                let mut blend_eq_rgb = 0;
                let mut blend_eq_alpha = 0;
                let mut blend_src_rgb = 0;
                let mut blend_dst_rgb = 0;
                let mut blend_src_alpha = 0;
                let mut blend_dst_alpha = 0;
                let mut attrib_enabled = [0; 4];
                let mut attrib_buffer = [0; 4];
                let mut attrib_pointer = [std::ptr::null_mut(); 4];
                let mut vertex_binding_offset = [0; 4];
                let mut vertex_binding_stride = [0; 4];
                let mut element_buffer = 0;
                gl::GetIntegerv(gl::DRAW_FRAMEBUFFER_BINDING, &mut draw_fbo);
                gl::GetIntegerv(gl::PROGRAM_PIPELINE_BINDING, &mut pipeline_handle);
                gl::GetIntegerv(gl::VIEWPORT, viewport.as_mut_ptr());
                gl::GetIntegerv(gl::SCISSOR_BOX, scissor_box.as_mut_ptr());
                gl::GetBooleani_v(gl::COLOR_WRITEMASK, 0, color_mask.as_mut_ptr());
                depth_test = gl::IsEnabled(gl::DEPTH_TEST) as i32;
                cull_face = gl::IsEnabled(gl::CULL_FACE) as i32;
                scissor_test = gl::IsEnabledi(gl::SCISSOR_TEST, 0) as i32;
                blend_enabled = gl::IsEnabledi(gl::BLEND, 0) as i32;
                gl::GetIntegeri_v(gl::BLEND_EQUATION_RGB, 0, &mut blend_eq_rgb);
                gl::GetIntegeri_v(gl::BLEND_EQUATION_ALPHA, 0, &mut blend_eq_alpha);
                gl::GetIntegeri_v(gl::BLEND_SRC_RGB, 0, &mut blend_src_rgb);
                gl::GetIntegeri_v(gl::BLEND_DST_RGB, 0, &mut blend_dst_rgb);
                gl::GetIntegeri_v(gl::BLEND_SRC_ALPHA, 0, &mut blend_src_alpha);
                gl::GetIntegeri_v(gl::BLEND_DST_ALPHA, 0, &mut blend_dst_alpha);
                for index in 0..4 {
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
                    gl::GetVertexAttribPointerv(
                        index as u32,
                        gl::VERTEX_ATTRIB_ARRAY_POINTER,
                        &mut attrib_pointer[index],
                    );
                    gl::GetIntegeri_v(
                        GL_VERTEX_BINDING_OFFSET,
                        index as u32,
                        &mut vertex_binding_offset[index],
                    );
                    gl::GetIntegeri_v(
                        GL_VERTEX_BINDING_STRIDE,
                        index as u32,
                        &mut vertex_binding_stride[index],
                    );
                }
                gl::GetIntegerv(gl::ELEMENT_ARRAY_BUFFER_BINDING, &mut element_buffer);
                let fbo_status = gl::CheckFramebufferStatus(gl::DRAW_FRAMEBUFFER);
                let gl_error = gl::GetError();
                let index_format_size = draw_state.index_buffer.format.size_bytes();
                let index_offset = self.buffer_cache.index_offset();
                let rt_translator = self.channel_memory_manager.as_ref().cloned();
                let rt_summary: Vec<String> = draw_view
                    .render_targets()
                    .render_targets
                    .iter()
                    .take(4)
                    .enumerate()
                    .map(|(index, rt)| {
                        let device_addr = rt_translator
                            .as_ref()
                            .and_then(|mm| mm.lock().gpu_to_cpu_address(rt.address));
                        format!(
                            "{}:gpu=0x{:X}/dev={}/{}x{}/fmt=0x{:X}",
                            index,
                            rt.address,
                            device_addr
                                .map(|addr| format!("0x{:X}", addr))
                                .unwrap_or_else(|| "None".to_string()),
                            rt.width,
                            rt.height,
                            rt.format
                        )
                    })
                    .collect();
                log::info!(
                    "[DRAW_SUMMARY] seq={} batch={} can_draw={} indexed={} mode=0x{:X} verts={} instances={} base_instance={} base_index={} vb_first={} vb_count={} ib_first={} ib_count={} ib_format={:?} ib_format_size={} ib_gpu=0x{:X} ib_gpu_end=0x{:X} ib_gl_offset={} rt_count={} rt_map={:?} rt0_3=[{}] fbo={} bound_rt={:?} pipeline={} programs={:?} viewport={:?} scissor_box={:?} depth={} cull={} scissor0={} blend={} blend_eq=0x{:X}/0x{:X} blend_src=0x{:X}/0x{:X} blend_dst=0x{:X}/0x{:X} color_mask={:?} attrib_enabled={:?} attrib_buffer={:?} attrib_pointer={:?} vertex_binding_offset={:?} vertex_binding_stride={:?} element_buffer={} fbo_status=0x{:X} gl_error=0x{:X}",
                    draw_seq,
                    draw_no,
                    can_draw_gl,
                    is_indexed,
                    primitive_mode,
                    if is_indexed { draw_state.index_buffer.count } else { draw_state.vertex_buffer.count },
                    num_instances,
                    base_instance,
                    draw_state.base_index,
                    draw_state.vertex_buffer.first,
                    draw_state.vertex_buffer.count,
                    draw_state.index_buffer.first,
                    draw_state.index_buffer.count,
                    draw_state.index_buffer.format,
                    index_format_size,
                    draw_view.index_buffer_gpu_addr(),
                    draw_view.index_buffer_gpu_addr_end(),
                    index_offset,
                    draw_view.render_targets().rt_control.count,
                    draw_view.render_targets().rt_control.map,
                    rt_summary.join(","),
                    draw_fbo,
                    bound_draw_framebuffer,
                    pipeline_handle,
                    pipeline.source_programs,
                    viewport,
                    scissor_box,
                    depth_test,
                    cull_face,
                    scissor_test,
                    blend_enabled,
                    blend_eq_rgb,
                    blend_eq_alpha,
                    blend_src_rgb,
                    blend_src_alpha,
                    blend_dst_rgb,
                    blend_dst_alpha,
                    color_mask,
                    attrib_enabled,
                    attrib_buffer,
                    attrib_pointer.map(|ptr| ptr as usize),
                    vertex_binding_offset,
                    vertex_binding_stride,
                    element_buffer,
                    fbo_status,
                    gl_error,
                );
                if gl_debug.dump_draw_buffers {
                    if let (Some(mm), Some(reader)) = (
                        self.channel_memory_manager.as_ref(),
                        self.device_memory_reader.as_ref(),
                    ) {
                        let dump_guest_gpu = |label: &str, gpu_addr: u64, max_bytes: usize| {
                            if let Some(device_addr) = mm.lock().gpu_to_cpu_address(gpu_addr) {
                                let mut bytes = vec![0u8; max_bytes];
                                reader(device_addr, &mut bytes);
                                log::warn!(
                                    "[DRAW_GUEST] {} gpu=0x{:X} device=0x{:X} first{}={:02X?}",
                                    label,
                                    gpu_addr,
                                    device_addr,
                                    bytes.len(),
                                    bytes
                                );
                            } else {
                                log::warn!("[DRAW_GUEST] {} gpu=0x{:X} unmapped", label, gpu_addr);
                            }
                        };
                        dump_guest_gpu("index", draw_view.index_buffer_gpu_addr(), 32);
                        dump_guest_gpu("vertex0", draw_view.vertex_streams()[0].address, 192);
                    }
                    dump_gl_buffer_prefix(
                        "attrib0",
                        attrib_buffer[0] as u32,
                        vertex_binding_offset[0] as isize
                            + draw_view.vertex_attribs()[0].offset as isize,
                        192,
                    );
                    dump_gl_buffer_prefix(
                        "element",
                        element_buffer as u32,
                        self.buffer_cache.index_offset() as isize,
                        64,
                    );
                }
            }
        }
        if can_draw_gl && gl_debug.dump_draw_attrs {
            let pipeline_filter = parse_trace_u64_env("RUZU_DUMP_DRAW_ATTRS_PIPELINE");
            let seq_min = parse_trace_u64_env("RUZU_DUMP_DRAW_ATTRS_SEQ_MIN").unwrap_or(0);
            let seq_max = parse_trace_u64_env("RUZU_DUMP_DRAW_ATTRS_SEQ_MAX").unwrap_or(u64::MAX);
            let limit = parse_trace_u64_env("RUZU_DUMP_DRAW_ATTRS_LIMIT").unwrap_or(32);
            let attr_count = parse_trace_u64_env("RUZU_DUMP_DRAW_ATTRS_ATTR_COUNT")
                .unwrap_or(2)
                .min(16) as usize;
            static DRAW_ATTR_DUMP_COUNT: AtomicU64 = AtomicU64::new(0);
            let pipeline_handle = pipeline.program_pipeline_handle() as u64;
            let matches_pipeline = pipeline_filter.is_none_or(|target| target == pipeline_handle);
            if matches_pipeline && draw_seq >= seq_min && draw_seq <= seq_max {
                let dump_index = DRAW_ATTR_DUMP_COUNT.fetch_add(1, Ordering::Relaxed);
                if dump_index < limit {
                    if let (Some(mm), Some(reader)) = (
                        self.channel_memory_manager.as_ref(),
                        self.device_memory_reader.as_ref(),
                    ) {
                        let vertex_streams = draw_view.vertex_streams();
                        let vertex_attribs = draw_view.vertex_attribs();
                        let read_guest = |gpu_addr: u64, len: usize| -> Option<(u64, Vec<u8>)> {
                            let device_addr = mm.lock().gpu_to_cpu_address(gpu_addr)?;
                            let mut bytes = vec![0u8; len];
                            reader(device_addr, &mut bytes);
                            Some((device_addr, bytes))
                        };
                        let read_index = |ordinal: u32| -> Option<u32> {
                            let stride = index_format_stride(draw_state.index_buffer.format);
                            let gpu_addr = draw_view.index_buffer_gpu_addr()
                                + (draw_state.index_buffer.first as u64 + ordinal as u64)
                                    * stride as u64;
                            let (_, bytes) = read_guest(gpu_addr, stride)?;
                            Some(match draw_state.index_buffer.format {
                                crate::engines::draw_manager::IndexFormat::UnsignedByte => {
                                    bytes.first().copied().unwrap_or_default() as u32
                                }
                                crate::engines::draw_manager::IndexFormat::UnsignedShort => {
                                    u16::from_le_bytes([
                                        bytes.first().copied().unwrap_or_default(),
                                        bytes.get(1).copied().unwrap_or_default(),
                                    ]) as u32
                                }
                                crate::engines::draw_manager::IndexFormat::UnsignedInt => {
                                    u32::from_le_bytes([
                                        bytes.first().copied().unwrap_or_default(),
                                        bytes.get(1).copied().unwrap_or_default(),
                                        bytes.get(2).copied().unwrap_or_default(),
                                        bytes.get(3).copied().unwrap_or_default(),
                                    ])
                                }
                            })
                        };

                        let vertex_count = if is_indexed {
                            draw_state.index_buffer.count
                        } else {
                            draw_state.vertex_buffer.count
                        };
                        let samples = vertex_count.min(6);
                        for sample in 0..samples {
                            let vertex_index = if is_indexed {
                                read_index(sample)
                                    .map(|index| index.wrapping_add(draw_state.base_index))
                                    .unwrap_or(draw_state.base_index)
                            } else {
                                draw_state.vertex_buffer.first.wrapping_add(sample)
                            };
                            for attr_index in 0..attr_count {
                                let attrib = vertex_attribs[attr_index];
                                let stream = vertex_streams[attrib.buffer_index as usize];
                                let byte_len = attrib.size.size_bytes() as usize;
                                if byte_len == 0 || !stream.enabled || attrib.constant {
                                    log::warn!(
                                        "[DRAW_ATTR] seq={} pipeline={} sample={} vertex={} attr={} inactive stream={} enabled={} constant={} size={:?} type={:?}",
                                        draw_seq,
                                        pipeline_handle,
                                        sample,
                                        vertex_index,
                                        attr_index,
                                        attrib.buffer_index,
                                        stream.enabled,
                                        attrib.constant,
                                        attrib.size,
                                        attrib.attrib_type
                                    );
                                    continue;
                                }
                                let gpu_addr = stream.address
                                    + vertex_index as u64 * stream.stride as u64
                                    + attrib.offset as u64;
                                if let Some((device_addr, bytes)) = read_guest(gpu_addr, byte_len) {
                                    let decoded = decode_vertex_attrib_sample(attrib, &bytes);
                                    log::warn!(
                                        "[DRAW_ATTR] seq={} pipeline={} indexed={} sample={} vertex={} attr={} stream={} stream_gpu=0x{:X} stride={} attr_offset=0x{:X} gpu=0x{:X} device=0x{:X} size={:?} type={:?} bgra={} raw={:02X?} decoded={:?}",
                                        draw_seq,
                                        pipeline_handle,
                                        is_indexed,
                                        sample,
                                        vertex_index,
                                        attr_index,
                                        attrib.buffer_index,
                                        stream.address,
                                        stream.stride,
                                        attrib.offset,
                                        gpu_addr,
                                        device_addr,
                                        attrib.size,
                                        attrib.attrib_type,
                                        attrib.bgra,
                                        bytes,
                                        decoded
                                    );
                                } else {
                                    log::warn!(
                                        "[DRAW_ATTR] seq={} pipeline={} sample={} vertex={} attr={} gpu=0x{:X} unmapped stream={} stride={} offset=0x{:X}",
                                        draw_seq,
                                        pipeline_handle,
                                        sample,
                                        vertex_index,
                                        attr_index,
                                        gpu_addr,
                                        attrib.buffer_index,
                                        stream.stride,
                                        attrib.offset
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }
        if can_draw_gl && (rt_readback || should_dump_rt_addr || should_dump_rt_seq) {
            if let Some((framebuffer, width, height)) = bound_draw_framebuffer {
                unsafe {
                    let sample_width = width.min(32) as i32;
                    let sample_height = height.min(32) as i32;
                    let mut old_read_fb = 0;
                    let mut old_pack_buffer = 0;
                    let mut old_pack_alignment = 0;
                    let mut old_pack_row_length = 0;
                    gl::GetIntegerv(gl::READ_FRAMEBUFFER_BINDING, &mut old_read_fb);
                    gl::GetIntegerv(gl::PIXEL_PACK_BUFFER_BINDING, &mut old_pack_buffer);
                    gl::GetIntegerv(gl::PACK_ALIGNMENT, &mut old_pack_alignment);
                    gl::GetIntegerv(gl::PACK_ROW_LENGTH, &mut old_pack_row_length);
                    gl::BindFramebuffer(gl::READ_FRAMEBUFFER, framebuffer);
                    gl::ReadBuffer(gl::COLOR_ATTACHMENT0);
                    gl::BindBuffer(gl::PIXEL_PACK_BUFFER, 0);
                    gl::PixelStorei(gl::PACK_ALIGNMENT, 1);
                    gl::PixelStorei(gl::PACK_ROW_LENGTH, 0);

                    let max_x = width.saturating_sub(sample_width as u32) as i32;
                    let max_y = height.saturating_sub(sample_height as u32) as i32;
                    let origins = [
                        (0, 0),
                        (max_x / 2, max_y / 2),
                        (max_x, max_y),
                        (0, max_y),
                        (max_x, 0),
                    ];
                    let mut summaries = Vec::with_capacity(origins.len());
                    let mut gl_error = 0;
                    for (origin_x, origin_y) in origins {
                        let mut pixels = vec![0u8; (sample_width * sample_height * 4) as usize];
                        gl::ReadPixels(
                            origin_x,
                            origin_y,
                            sample_width,
                            sample_height,
                            gl::RGBA,
                            gl::UNSIGNED_BYTE,
                            pixels.as_mut_ptr() as *mut _,
                        );
                        gl_error |= gl::GetError();

                        let mut rgb_nonzero = 0usize;
                        let mut alpha_nonzero = 0usize;
                        let mut red_pixels = 0usize;
                        let mut black_pixels = 0usize;
                        let mut white_pixels = 0usize;
                        let mut rgba_sum = [0u64; 4];
                        for px in pixels.chunks_exact(4) {
                            rgb_nonzero += px[0..3].iter().filter(|&&byte| byte != 0).count();
                            alpha_nonzero += usize::from(px[3] != 0);
                            red_pixels += usize::from(px[0] > 128 && px[1] < 32 && px[2] < 32);
                            black_pixels += usize::from(px[0] < 8 && px[1] < 8 && px[2] < 8);
                            white_pixels += usize::from(px[0] > 240 && px[1] > 240 && px[2] > 240);
                            for component in 0..4 {
                                rgba_sum[component] += px[component] as u64;
                            }
                        }
                        let checksum = pixels
                            .iter()
                            .fold(0u64, |acc, &byte| acc.wrapping_mul(16777619) ^ byte as u64);
                        summaries.push(format!(
                            "@{},{} rgb={} a={} red={} black={} white={} sum={:?} crc=0x{:X}",
                            origin_x,
                            origin_y,
                            rgb_nonzero,
                            alpha_nonzero,
                            red_pixels,
                            black_pixels,
                            white_pixels,
                            rgba_sum,
                            checksum
                        ));
                    }
                    let mut attached_tex: i32 = 0;
                    gl::GetNamedFramebufferAttachmentParameteriv(
                        framebuffer,
                        gl::COLOR_ATTACHMENT0,
                        gl::FRAMEBUFFER_ATTACHMENT_OBJECT_NAME,
                        &mut attached_tex,
                    );
                    let mut tex_bytes = [0u8; 16];
                    if attached_tex > 0 {
                        gl::GetTextureSubImage(
                            attached_tex as u32,
                            0,
                            0,
                            0,
                            0,
                            2,
                            2,
                            1,
                            gl::RGBA,
                            gl::UNSIGNED_BYTE,
                            tex_bytes.len() as i32,
                            tex_bytes.as_mut_ptr() as *mut _,
                        );
                    }
                    if (should_dump_rt_seq || should_dump_rt_addr) && width != 0 && height != 0 {
                        if should_dump_rt_addr {
                            DUMP_RT_COUNT.fetch_add(1, Ordering::Relaxed);
                        }
                        let mut pixels = vec![0u8; width as usize * height as usize * 4];
                        gl::ReadPixels(
                            0,
                            0,
                            width as i32,
                            height as i32,
                            gl::RGBA,
                            gl::UNSIGNED_BYTE,
                            pixels.as_mut_ptr() as *mut _,
                        );
                        gl_error |= gl::GetError();
                        let dir = std::env::var("RUZU_DUMP_RT_DIR")
                            .unwrap_or_else(|_| "/tmp/ruzu_rt_dumps".to_string());
                        if let Err(err) = std::fs::create_dir_all(&dir) {
                            log::warn!("[RT_DUMP] failed to create {}: {}", dir, err);
                        } else {
                            let path = format!(
                                "{}/rt_seq_{}_gpu_{:X}_{}x{}.ppm",
                                dir, draw_seq, rt0_addr_for_debug, width, height
                            );
                            let mut ppm = Vec::with_capacity(
                                "P6\n4294967295 4294967295\n255\n".len()
                                    + width as usize * height as usize * 3,
                            );
                            ppm.extend_from_slice(
                                format!("P6\n{} {}\n255\n", width, height).as_bytes(),
                            );
                            for row in (0..height as usize).rev() {
                                let row_start = row * width as usize * 4;
                                for x in 0..width as usize {
                                    let px = row_start + x * 4;
                                    ppm.extend_from_slice(&pixels[px..px + 3]);
                                }
                            }
                            match std::fs::write(&path, ppm) {
                                Ok(()) => log::warn!(
                                    "[RT_DUMP] seq={} fbo={} attached={} wrote {} gl_error=0x{:X}",
                                    draw_seq,
                                    framebuffer,
                                    attached_tex,
                                    path,
                                    gl_error
                                ),
                                Err(err) => {
                                    log::warn!("[RT_DUMP] failed to write {}: {}", path, err)
                                }
                            }
                        }
                    }

                    gl::BindFramebuffer(gl::READ_FRAMEBUFFER, old_read_fb as u32);
                    gl::BindBuffer(gl::PIXEL_PACK_BUFFER, old_pack_buffer as u32);
                    gl::PixelStorei(gl::PACK_ALIGNMENT, old_pack_alignment);
                    gl::PixelStorei(gl::PACK_ROW_LENGTH, old_pack_row_length);

                    log::info!(
                        "[RT_READBACK] seq={} batch={} rt0_gpu=0x{:X} rt0_fmt=0x{:X} rt0_size={}x{} fbo={} attached={} tex_bytes={:02X?} {}x{} sample={}x{} regions=[{}] gl_error=0x{:X}",
                        draw_seq,
                        draw_no,
                        rt0_addr_for_debug,
                        draw_view.render_targets().render_targets[0].format,
                        draw_view.render_targets().render_targets[0].width,
                        draw_view.render_targets().render_targets[0].height,
                        framebuffer,
                        attached_tex,
                        tex_bytes,
                        width,
                        height,
                        sample_width,
                        sample_height,
                        summaries.join("; "),
                        gl_error
                    );
                }
            }
        }
        self.num_queued_commands = self.num_queued_commands.saturating_add(1);
        self.total_draw_count = self.total_draw_count.saturating_add(1);
        record_gl_draw_stage(draw_seq, 13);
        let profile_total_us = draw_start.map(trace_elapsed_us).unwrap_or(0);
        if trace_draw_profile_ring && profile_total_us >= gl_draw_profile_min_us() {
            common::trace::emit_raw(
                common::trace::cat::GL_DRAW_PROFILE,
                &[
                    draw_seq,
                    pipeline_handle,
                    is_indexed as u64,
                    primitive_mode as u64,
                    if is_indexed {
                        draw_state.index_buffer.count as u64
                    } else {
                        draw_state.vertex_buffer.count as u64
                    },
                    num_instances as u64,
                    profile_total_us,
                    profile_pipeline_us,
                    profile_rt_us,
                    profile_build_us,
                    profile_configure_us,
                    profile_update_buffers_us,
                    profile_bind_buffers_us,
                    profile_sync_draw_us,
                ],
            );
        }
        if trace_draw {
            info!(
                "[GL_DRAW_PROFILE] end total_us={} queued_commands={}",
                draw_start
                    .map(|start| start.elapsed().as_micros())
                    .unwrap_or(0),
                self.num_queued_commands
            );
        }
        if let Some(callback) = gpu_tick_callback.as_ref() {
            callback();
        }
    }

    /// Port of `RasterizerOpenGL::DrawIndirect`.
    fn draw_indirect(&mut self, indirect_view: Maxwell3DIndirectView<'_>) {
        let params = *indirect_view.params();
        let draw_state = indirect_view.draw_state();
        let cache_params = crate::buffer_cache::buffer_cache_base::DrawIndirectParams {
            indirect_start_address: params.indirect_start_address,
            count_start_address: params.count_start_address,
            buffer_size: params.buffer_size as u64,
            max_draw_counts: params.max_draw_counts as u32,
            stride: params.stride as u32,
            include_count: params.include_count,
        };

        self.buffer_cache.set_draw_indirect(Some(cache_params));

        if params.is_byte_count {
            log::warn!("RasterizerOpenGL::draw_indirect byte-count path is not ported yet");
            self.buffer_cache.set_draw_indirect(None);
            return;
        }
        if params.include_count {
            log::warn!("RasterizerOpenGL::draw_indirect count-buffer path is not ported yet");
            self.buffer_cache.set_draw_indirect(None);
            return;
        }

        self.buffer_cache.update_graphics_buffers(params.is_indexed);
        self.buffer_cache
            .bind_host_geometry_buffers(params.is_indexed);
        for stage in 0..crate::buffer_cache::buffer_cache_base::NUM_STAGES as usize {
            self.buffer_cache.bind_host_stage_buffers(stage);
        }

        let (buffer_id, offset) = self.buffer_cache.get_draw_indirect_buffer();
        let handle = self.buffer_cache.get_buffer_gpu_handle(buffer_id);
        if handle == 0 {
            log::warn!("RasterizerOpenGL::draw_indirect skipped: missing GL indirect buffer");
            self.buffer_cache.set_draw_indirect(None);
            return;
        }

        let primitive_mode = primitive_topology_to_gl(draw_state.topology);
        unsafe {
            gl::BindBuffer(gl::DRAW_INDIRECT_BUFFER, handle);
            let gl_offset = offset as usize as *const c_void;
            if params.is_indexed {
                let format = index_format_to_gl(draw_state.index_buffer.format);
                gl::MultiDrawElementsIndirect(
                    primitive_mode,
                    format,
                    gl_offset,
                    params.max_draw_counts as i32,
                    params.stride as i32,
                );
            } else {
                gl::MultiDrawArraysIndirect(
                    primitive_mode,
                    gl_offset,
                    params.max_draw_counts as i32,
                    params.stride as i32,
                );
            }
        }

        self.buffer_cache.set_draw_indirect(None);
        self.num_queued_commands = self.num_queued_commands.saturating_add(1);
        self.total_draw_count = self.total_draw_count.saturating_add(1);
        self.tick_gpu_work();
    }

    fn draw_texture(&mut self) {
        debug!("RasterizerOpenGL::draw_texture");
        self.tick_gpu_work();
    }

    fn clear(&mut self, clear_view: Maxwell3DClearView<'_>, layer_count: u32) {
        // Upstream `RasterizerOpenGL::Clear` starts with
        // `gpu_memory->FlushCaching()`.
        if let Some(mm) = self.channel_memory_manager.as_ref().cloned() {
            mm.lock().flush_caching();
        }
        let clear_state = clear_view.clear_state();
        let render_targets = clear_view.render_targets();
        let flags = clear_state.flags;
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

        let rt_index = ((flags >> 6) & 0xF) as usize;
        if use_color && rt_index >= render_targets.render_targets.len() {
            return;
        }
        let Some(mm) = self.channel_memory_manager.as_ref().cloned() else {
            if std::env::var_os("RUZU_TRACE_CLEAR").is_some() {
                debug!("RasterizerOpenGL::clear skipped, no channel memory manager");
            }
            return;
        };

        let texture_cache: *mut OpenGLTextureCache = &mut self.texture_cache;
        let framebuffer = unsafe {
            let _texture_lock = (*texture_cache).base.mutex.lock();
            (*texture_cache).update_render_targets_from_snapshot(&render_targets, |gpu_addr| {
                mm.lock().gpu_to_cpu_address(gpu_addr)
            });
            let clear_scissor = clear_view.use_scissor().then(|| clear_view.scissor(0));
            let mm_for_read = mm.clone();
            (*texture_cache).prepare_render_targets_from_snapshot(
                &render_targets,
                Some(&mut |gpu_addr, out| {
                    mm_for_read.lock().read_block(gpu_addr, out);
                    true
                }),
                true,
                clear_scissor,
            );
            (*texture_cache).framebuffer_for_render_targets_from_snapshot(
                &render_targets,
                crate::texture_cache::types::Extent2D::default(),
            )
        };
        let Some((framebuffer, width, height)) = framebuffer else {
            if std::env::var_os("RUZU_TRACE_CLEAR").is_some() {
                debug!(
                    "RasterizerOpenGL::clear skipped, no framebuffer flags=0x{:X}",
                    flags
                );
            }
            return;
        };
        unsafe {
            self.state_tracker.bind_framebuffer(framebuffer);
            self.state_tracker.notify_viewport0();
            gl::Viewport(0, 0, width as i32, height as i32);
            if clear_view.use_scissor() {
                self.state_tracker.notify_scissor0();
                let scissor = clear_view.scissor(0);
                if scissor.enabled && scissor.max_x > scissor.min_x && scissor.max_y > scissor.min_y
                {
                    gl::Enablei(gl::SCISSOR_TEST, 0);
                    gl::ScissorIndexed(
                        0,
                        scissor.min_x as i32,
                        scissor.min_y as i32,
                        (scissor.max_x - scissor.min_x) as i32,
                        (scissor.max_y - scissor.min_y) as i32,
                    );
                } else {
                    gl::Disablei(gl::SCISSOR_TEST, 0);
                }
            } else {
                self.state_tracker.notify_scissor0();
                gl::Disablei(gl::SCISSOR_TEST, 0);
            }
            if use_color {
                self.state_tracker.notify_color_mask(rt_index);
                gl::ColorMaski(
                    rt_index as u32,
                    if clear_r { gl::TRUE } else { gl::FALSE },
                    if clear_g { gl::TRUE } else { gl::FALSE },
                    if clear_b { gl::TRUE } else { gl::FALSE },
                    if clear_a { gl::TRUE } else { gl::FALSE },
                );
                self.state_tracker.notify_framebuffer_srgb();
                if clear_view.framebuffer_srgb() {
                    gl::Enable(gl::FRAMEBUFFER_SRGB);
                } else {
                    gl::Disable(gl::FRAMEBUFFER_SRGB);
                }
                gl::ClearBufferfv(gl::COLOR, rt_index as i32, clear_state.color.as_ptr());
            }
            if use_depth {
                self.state_tracker.notify_depth_mask();
                gl::DepthMask(gl::TRUE);
            }
            if use_depth && use_stencil {
                gl::ClearBufferfi(gl::DEPTH_STENCIL, 0, clear_state.depth, clear_state.stencil);
            } else if use_depth {
                gl::ClearBufferfv(gl::DEPTH, 0, &clear_state.depth);
            } else if use_stencil {
                gl::ClearBufferiv(gl::STENCIL, 0, &clear_state.stencil);
            }
        }
        if std::env::var_os("RUZU_TRACE_CLEAR_WARN").is_some() {
            let rt = render_targets
                .render_targets
                .get(rt_index)
                .copied()
                .unwrap_or_default();
            log::warn!(
                "[CLEAR_WARN] rt={} gpu=0x{:X} fbo={} {}x{} rgba={:?} depth={} stencil={} flags=0x{:X}",
                rt_index,
                rt.address,
                framebuffer,
                width,
                height,
                clear_state.color,
                clear_state.depth,
                clear_state.stencil,
                flags,
            );
        }
        self.num_queued_commands = self.num_queued_commands.saturating_add(1);

        if std::env::var_os("RUZU_TRACE_CLEAR").is_some() {
            debug!(
                "RasterizerOpenGL::clear rt={} fbo={} {}x{} rgba={:?} depth={} stencil={} layers={}",
                rt_index,
                framebuffer,
                width,
                height,
                clear_state.color,
                clear_state.depth,
                clear_state.stencil,
                layer_count
            );
        }
    }

    fn dispatch_compute(&mut self) {
        // Upstream `RasterizerOpenGL::DispatchCompute` starts with
        // `gpu_memory->FlushCaching()`.
        if let Some(mm) = self.channel_memory_manager.as_ref().cloned() {
            mm.lock().flush_caching();
        }
        debug!("RasterizerOpenGL::dispatch_compute");
    }

    fn dispatch_compute_with_call(&mut self, dispatch: &DispatchCall) {
        // Upstream `RasterizerOpenGL::DispatchCompute` starts with
        // `gpu_memory->FlushCaching()`, then obtains the current compute
        // pipeline whose `Configure()` synchronizes compute TIC/TSC descriptors.
        if let Some(mm) = self.channel_memory_manager.as_ref().cloned() {
            mm.lock().flush_caching();
        }
        self.texture_cache
            .base
            .synchronize_compute_descriptors(compute_descriptor_sync_regs_from_dispatch(dispatch));
        debug!(
            "RasterizerOpenGL::dispatch_compute_with_call grid=({},{},{}) block=({},{},{}) code=0x{:X}",
            dispatch.qmd.grid_dim_x,
            dispatch.qmd.grid_dim_y,
            dispatch.qmd.grid_dim_z,
            dispatch.qmd.block_dim_x,
            dispatch.qmd.block_dim_y,
            dispatch.qmd.block_dim_z,
            dispatch.code_address
        );
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
            self.query_fallback(gpu_addr, query_type, flags, payload, _subreport);
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
        unsafe {
            let texture_mutex: *const _ = &self.texture_cache.base.mutex;
            let _texture_guard = (*texture_mutex).lock();
            self.texture_cache.download_memory(addr, size as usize);
        }
        unsafe {
            let buffer_mutex: *const _ = &self.buffer_cache.mutex;
            let _buffer_guard = (*buffer_mutex).lock();
            self.buffer_cache.download_memory(addr, size);
        }
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
        unsafe {
            let texture_mutex: *const _ = &self.texture_cache.base.mutex;
            let _texture_guard = (*texture_mutex).lock();
            let texture_cache: *mut OpenGLTextureCache = &self.texture_cache as *const _ as *mut _;
            if let Some(area) = (*texture_cache).get_flush_area(addr, size) {
                return area;
            }
        }

        unsafe {
            let buffer_mutex: *const _ = &self.buffer_cache.mutex;
            let _buffer_guard = (*buffer_mutex).lock();
            let buffer_cache: *mut CommonBufferCache<OpenGLBufferCacheParams, OpenGLDeviceTracker> =
                &self.buffer_cache as *const _ as *mut _;
            if let Some(area) = (*buffer_cache).get_flush_area(addr, size) {
                return RasterizerDownloadArea {
                    start_address: area.start_address,
                    end_address: area.end_address,
                    preemptive: area.preemtive,
                };
            }
        }

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
        unsafe {
            let texture_mutex: *const _ = &self.texture_cache.base.mutex;
            let _texture_guard = (*texture_mutex).lock();
            self.texture_cache.write_memory(addr, size as usize);
        }
        unsafe {
            let buffer_mutex: *const _ = &self.buffer_cache.mutex;
            let _buffer_guard = (*buffer_mutex).lock();
            self.buffer_cache.write_memory(addr, size);
        }
        self.shader_cache.invalidate_region(addr, size as usize);
        self.query_cache
            .set_commands_queued(self.num_queued_commands != 0);
        self.query_cache.invalidate_region(addr, size as usize);
    }

    fn on_cache_invalidation(&mut self, addr: u64, size: u64) {
        if addr == 0 || size == 0 {
            return;
        }
        // Mirrors upstream `RasterizerOpenGL::OnCacheInvalidation`
        // (gl_rasterizer.cpp:693-707): take per-cache mutexes in order
        // (texture_cache, buffer_cache) before mutating cache state.
        //
        // The sentinel `Mutex<()>` lives INSIDE the cache it protects,
        // so we acquire it through a raw pointer to avoid Rust's borrow
        // checker rejecting `&mut self.texture_cache` while a guard
        // borrows `&self.texture_cache.base.mutex` immutably. Upstream
        // C++ does this trivially (`std::scoped_lock lock{cache.mutex}`)
        // — the unsafe block matches that semantics.
        unsafe {
            let texture_mutex: *const _ = &self.texture_cache.base.mutex;
            let _texture_guard = (*texture_mutex).lock();
            self.texture_cache.write_memory(addr, size as usize);
        }
        unsafe {
            let buffer_mutex: *const _ = &self.buffer_cache.mutex;
            let _buffer_guard = (*buffer_mutex).lock();
            self.buffer_cache.write_memory(addr, size);
        }
        self.shader_cache.on_cache_invalidation(addr, size as usize);
    }

    fn on_cpu_write(&mut self, addr: u64, size: u64) -> bool {
        if addr == 0 || size == 0 {
            return false;
        }
        // Mirrors upstream `RasterizerOpenGL::OnCPUWrite`
        // (gl_rasterizer.cpp:671-691): take per-cache mutexes before
        // mutating cache state. Without these locks, CPU emulation
        // threads invoking on_cpu_write via the JIT memory-write
        // trampoline race with the GPU thread using the same caches
        // for rendering — observed as a `hashbrown::Tag::full` SIGSEGV
        // ~60s into MK8D (see project_mk8d_deterministic_wedge_2026_05_16).
        //
        // See on_cache_invalidation above for why the locks are taken
        // through raw pointers.
        let buffer_handled = unsafe {
            let _lo_buf = common::lock_order::guard("buffer_cache");
            let buffer_mutex: *const _ = &self.buffer_cache.mutex;
            let _buffer_guard = (*buffer_mutex).lock();
            self.buffer_cache.on_cpu_write(addr, size)
        };
        if buffer_handled {
            return true;
        }
        unsafe {
            let _lo_tex = common::lock_order::guard("texture_cache");
            let texture_mutex: *const _ = &self.texture_cache.base.mutex;
            let _texture_guard = (*texture_mutex).lock();
            self.texture_cache.write_memory(addr, size as usize);
        }
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
        unsafe {
            let _lo_tex = common::lock_order::guard("texture_cache");
            let texture_mutex: *const _ = &self.texture_cache.base.mutex;
            let _texture_guard = (*texture_mutex).lock();
            self.texture_cache.unmap_memory(addr, size as usize);
        }
        unsafe {
            let _lo_buf = common::lock_order::guard("buffer_cache");
            let buffer_mutex: *const _ = &self.buffer_cache.mutex;
            let _buffer_guard = (*buffer_mutex).lock();
            self.buffer_cache.write_memory(addr, size);
        }
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
        unsafe {
            let texture_mutex: *const _ = &self.texture_cache.base.mutex;
            let _texture_guard = (*texture_mutex).lock();
            self.texture_cache.tick_frame();
        }
        unsafe {
            let buffer_mutex: *const _ = &self.buffer_cache.mutex;
            let _buffer_guard = (*buffer_mutex).lock();
            self.buffer_cache.tick_frame();
        }
    }

    fn accelerate_surface_copy(
        &mut self,
        src: &crate::engines::fermi_2d::Surface,
        dst: &crate::engines::fermi_2d::Surface,
        copy_config: &crate::engines::fermi_2d::Config,
    ) -> bool {
        let Some(mm) = self.channel_memory_manager.as_ref().cloned() else {
            return false;
        };
        let texture_cache: *mut OpenGLTextureCache = &mut self.texture_cache;
        let accelerated = unsafe {
            let _texture_lock = (*texture_cache).base.mutex.lock();
            (*texture_cache).blit_image(
                dst,
                src,
                copy_config,
                |gpu_addr| mm.lock().gpu_to_cpu_address(gpu_addr),
                |gpu_addr, out| {
                    let guard = mm.lock();
                    guard.read_block(gpu_addr, out);
                    true
                },
            )
        };
        accelerated
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
            mm.write_block(address, &memory[..copy_size]);
            if std::env::var_os("RUZU_TRACE_INLINE_TO_MEMORY").is_some() {
                log::info!(
                    "RasterizerOpenGL::accelerate_inline_to_memory fallback_write_block gpu=0x{:X} size={}",
                    address,
                    copy_size
                );
            }
            return;
        }
        mm.write_block_unsafe(address, &memory[..copy_size]);
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
            self.texture_cache
                .base
                .set_channel_gpu_memory(Arc::clone(mm));

            // The buffer cache reads through the channel's upstream-shaped
            // MemoryManager owner.
            self.buffer_cache
                .set_gpu_memory(Box::new(GpuMemoryAccessAdapter { mm: Arc::clone(mm) }));
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
        self.texture_cache.base.clear_channel_gpu_memory();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
    use crate::memory_manager::MemoryManager;
    use common::settings;
    use common::settings_enums::GpuAccuracy;

    fn install_query_memory_manager(rast: &mut RasterizerOpenGL) -> Vec<u8> {
        let device_memory = Arc::new(MaxwellDeviceMemoryManager::default());
        let mut backing = vec![0u8; 0x10000];
        device_memory.smmu_set_physical_base_for_test(backing.as_ptr() as usize);
        device_memory.smmu_map_with_cpu_backing(
            0x9000_1000,
            backing.as_mut_ptr(),
            0x4000_0000,
            backing.len(),
            5,
            true,
        );

        let mut mm = MemoryManager::new_with_geometry_and_device_memory(
            0,
            Arc::clone(&device_memory),
            32,
            0x1_0000_0000,
            16,
            12,
        );
        mm.map(0x1000, 0x9000_1000, 0x10000, 0, false);
        let mm = Arc::new(parking_lot::Mutex::new(mm));
        let mut channel = crate::control::channel_state::ChannelState::new(1);
        channel.program_id = 0xCAFE;
        channel.memory_manager = Some(Arc::clone(&mm));
        rast.channel_memory_manager = Some(Arc::clone(&mm));
        rast.query_cache.create_channel(&channel);
        rast.query_cache.bind_to_channel(channel.bind_id);
        backing
    }

    #[test]
    fn query_fence_defers_guest_write_until_release() {
        let syncpoints = Arc::new(SyncpointManager::new());
        let mut rast = RasterizerOpenGL::new_for_test(syncpoints);
        let backing = install_query_memory_manager(&mut rast);
        rast.set_gpu_ticks_getter(Arc::new(|| 0));

        rast.query(0x1000, 0, QueryPropertiesFlags::IS_A_FENCE, 0x1234_5678, 0);

        assert_eq!(&backing[0..4], &[0; 4]);

        rast.release_fences(true);

        assert_eq!(&backing[0..4], &0x1234_5678u32.to_le_bytes());
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
    fn compute_descriptor_sync_regs_come_from_dispatch_call() {
        let mut qmd = crate::engines::kepler_compute::QueueMetaData::default();
        qmd.linked_tsc = true;
        qmd.grid_dim_x = 2;
        qmd.grid_dim_y = 3;
        qmd.grid_dim_z = 4;
        qmd.block_dim_x = 8;
        qmd.block_dim_y = 1;
        qmd.block_dim_z = 1;

        let dispatch = DispatchCall {
            qmd,
            qmd_address: 0x1000,
            code_address: 0x2000,
            tsc_address: 0x3000,
            tsc_limit: 1,
            tic_address: 0x4000,
            tic_limit: 6,
            tex_cb_index: 0,
        };

        let regs = compute_descriptor_sync_regs_from_dispatch(&dispatch);

        assert!(regs.linked_tsc);
        assert_eq!(regs.tic_addr, 0x4000);
        assert_eq!(regs.tic_limit, 6);
        assert_eq!(regs.tsc_addr, 0x3000);
        assert_eq!(regs.tsc_limit, 1);
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
        let backing = install_query_memory_manager(&mut rast);
        rast.set_gpu_ticks_getter(Arc::new(|| 0));

        rast.query(
            0x3000,
            crate::query_cache::types::QueryType::Payload as u32,
            QueryPropertiesFlags::empty(),
            0xCAFE_BABE,
            0,
        );

        assert_eq!(&backing[0x2000..0x2004], &0xCAFE_BABEu32.to_le_bytes());
    }

    #[test]
    fn query_has_timeout_payload_fallback_writes_immediately_and_preserves_payload() {
        let syncpoints = Arc::new(SyncpointManager::new());
        let mut rast = RasterizerOpenGL::new_for_test(syncpoints);
        let backing = install_query_memory_manager(&mut rast);
        rast.set_gpu_ticks_getter(Arc::new(|| 0x0123_4567_89AB_CDEF));

        rast.query(
            0x4000,
            crate::query_cache::types::QueryType::Payload as u32,
            QueryPropertiesFlags::HAS_TIMEOUT,
            0xABCD_EF01,
            0,
        );

        assert_eq!(&backing[0x3000..0x3008], &0xABCD_EF01u64.to_le_bytes());
        assert_eq!(
            &backing[0x3008..0x3010],
            &0x0123_4567_89AB_CDEFu64.to_le_bytes()
        );
    }

    #[test]
    fn query_fallback_non_payload_fence_writes_one_after_release() {
        let syncpoints = Arc::new(SyncpointManager::new());
        let mut rast = RasterizerOpenGL::new_for_test(syncpoints);
        let backing = install_query_memory_manager(&mut rast);

        rast.query(
            0x5000,
            crate::query_cache::types::QueryType::VerticesGenerated as u32,
            QueryPropertiesFlags::IS_A_FENCE,
            0xDEAD_BEEF,
            0,
        );

        assert_eq!(&backing[0x4000..0x4004], &[0; 4]);

        rast.release_fences(true);

        assert_eq!(&backing[0x4000..0x4004], &1u32.to_le_bytes());
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

    #[test]
    fn clip_control_depth_matches_maxwell_depth_mode() {
        assert_eq!(clip_control_depth(DepthMode::ZeroToOne), gl::ZERO_TO_ONE);
        assert_eq!(
            clip_control_depth(DepthMode::MinusOneToOne),
            gl::NEGATIVE_ONE_TO_ONE
        );
    }

    #[test]
    fn clip_control_origin_matches_upstream_flip_rules() {
        assert_eq!(clip_control_origin(false, 1.0), gl::LOWER_LEFT);
        assert_eq!(clip_control_origin(false, -1.0), gl::UPPER_LEFT);
        assert_eq!(clip_control_origin(true, 1.0), gl::UPPER_LEFT);
        assert_eq!(clip_control_origin(true, -1.0), gl::LOWER_LEFT);
    }

    #[test]
    fn viewport_front_face_matches_upstream_flip_rules() {
        assert_eq!(
            viewport_front_face_to_gl(FrontFace::CCW, false, 1.0),
            gl::CW
        );
        assert_eq!(
            viewport_front_face_to_gl(FrontFace::CCW, false, -1.0),
            gl::CCW
        );
        assert_eq!(
            viewport_front_face_to_gl(FrontFace::CCW, true, 1.0),
            gl::CCW
        );
        assert_eq!(
            viewport_front_face_to_gl(FrontFace::CCW, true, -1.0),
            gl::CW
        );
    }

    #[test]
    fn viewport_scale_matches_upstream_rounding_rules() {
        assert_eq!(scale_viewport_value(10.0, 2.0), 20.0);
        assert_eq!(scale_viewport_value(10.25, 0.5), 5.0);
        assert_eq!(scale_viewport_value(-10.25, 0.5), -5.0);
    }

    #[test]
    fn viewport_swizzle_components_match_upstream_bitfields() {
        let base = crate::renderer_opengl::maxwell_to_gl::viewport_swizzle(0);
        assert_eq!(
            viewport_swizzle_components(0x3210),
            [base, base + 1, base + 2, base + 3]
        );
    }
}
