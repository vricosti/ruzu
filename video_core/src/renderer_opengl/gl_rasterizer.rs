// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_opengl/gl_rasterizer.h and gl_rasterizer.cpp
//! Status: EN COURS
//!
//! OpenGL rasterizer — processes Maxwell 3D draw commands using OpenGL.
//! Implements [`RasterizerInterface`]. Currently delegates actual rendering
//! to the software rasterizer; GL-accelerated rendering will be added as
//! buffer/texture/shader caches are ported from zuyu.

use log::debug;
use std::sync::Arc;

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
use crate::engines::maxwell_3d::DrawCall;
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
    cpu_reader: crate::shader_environment::GpuMemoryReader,
}

impl crate::buffer_cache::buffer_cache_base::DeviceMemoryAccess for DeviceMemoryAccessAdapter {
    fn get_pointer(&self, _device_addr: u64) -> Option<*const u8> {
        None
    }

    fn read_block_unsafe(&self, device_addr: u64, dst: &mut [u8]) {
        (self.cpu_reader)(device_addr, dst);
    }

    fn write_block_unsafe(&self, _device_addr: u64, _src: &[u8]) {
        // Write-back not yet wired.
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
        _index: u32,
    ) -> crate::buffer_cache::buffer_cache_base::VertexStreamInfo {
        crate::buffer_cache::buffer_cache_base::VertexStreamInfo::default()
    }

    fn get_vertex_stream_limit(
        &self,
        _index: u32,
    ) -> crate::buffer_cache::buffer_cache_base::VertexStreamLimit {
        crate::buffer_cache::buffer_cache_base::VertexStreamLimit::default()
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
    /// GPU tick getter used for timestamped query writes.
    gpu_ticks_getter: Option<Arc<dyn Fn() -> u64 + Send + Sync>>,
    /// Transient (placeholder) Vertex Array Object that we bind before
    /// every `glDraw*` call to satisfy the GL core-profile requirement that
    /// "a VAO must be bound for any draw command". Until the buffer cache
    /// is wired into the GL pipeline (issuing `glVertexAttribFormat` /
    /// `glBindVertexBuffer` per stream), this VAO is empty — draws still
    /// run, they just rasterise nothing useful. The VAO handle is `0` in
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
        if let Some(mm) = self.channel_memory_manager.as_ref().cloned() {
            let texture_cache: *mut OpenGLTextureCache = &mut self.texture_cache;
            unsafe {
                let _texture_lock = (*texture_cache).base.mutex.lock();
                (*texture_cache).update_render_targets_from_draw_state(draw_state, |gpu_addr| {
                    mm.lock().gpu_to_cpu_address(gpu_addr)
                });
            }
        } else if std::env::var_os("RUZU_TRACE_RT").is_some() {
            log::info!("[RT] miss no_channel_memory_manager");
        }

        let Some(pipeline) = self
            .gl_shader_cache
            .current_graphics_pipeline_with_shared_cache(&mut self.shader_cache)
        else {
            // No pipeline yet — either async compilation is in flight or
            // there is nothing to draw. Upstream silently skips in this case.
            debug!("RasterizerOpenGL::draw skipped — no graphics pipeline available");
            return;
        };

        // Lazy GL-program build (gap 4). The shader cache stages GLSL
        // sources but never calls into GL itself, so the very first time
        // we hit this pipeline with a real GL context we materialise the
        // separable per-stage programs here. Failures are logged once and
        // leave the pipeline in its placeholder state so we don't retry
        // every frame.
        if !pipeline.has_gl_programs() && pipeline.glsl_sources.iter().any(|s| s.is_some()) {
            if let Err((stage_index, msg)) = pipeline.build_from_sources() {
                log::warn!(
                    "RasterizerOpenGL::draw: pipeline build failed at stage {}: {}",
                    stage_index,
                    msg
                );
            }
        }

        let is_indexed = draw_state.draw_indexed;
        let pipeline_has_programs = pipeline.has_gl_programs();
        pipeline.configure(is_indexed);

        // Install the engine-state adapter so buffer cache update calls can
        // read the current draw's index/vertex buffer state from DrawState.
        self.buffer_cache
            .set_engine_state(Box::new(DrawStateEngineAdapter {
                draw_state: draw_state.clone(),
            }));
        // Buffer cache: refresh and bind host vertex/index buffers.
        // Mirrors upstream `RasterizerOpenGL::PrepareDraw`.
        self.buffer_cache.update_graphics_buffers(is_indexed);
        self.buffer_cache.bind_host_geometry_buffers(is_indexed);

        let primitive_mode = primitive_topology_to_gl(draw_state.topology);
        let base_instance = draw_state.base_instance;
        let num_instances = instance_count;

        // Only issue real `glDraw*` calls when the pipeline carries
        // compiled GL programs *and* we have a transient VAO (i.e. the
        // production constructor ran). This keeps the unit-test path
        // (`new_for_test`, no GL context, placeholder pipelines) safe.
        let can_draw_gl = pipeline_has_programs && self.transient_vao != 0;

        if is_indexed {
            let base_vertex = draw_state.base_index as i32;
            let num_vertices = draw_state.index_buffer.count;
            if can_draw_gl {
                let index_format = index_format_to_gl(draw_state.index_buffer.format);
                let index_offset = (draw_state.index_buffer.first as usize)
                    * index_format_stride(draw_state.index_buffer.format);
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
        self.num_queued_commands = self.num_queued_commands.saturating_add(1);
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

            // If we already have the CPU reader (set_gpu_memory_reader was
            // called before bind_channel), install GPU + device memory
            // access on the buffer cache now.
            if let Some(ref cpu_reader) = self.cpu_memory_reader {
                self.buffer_cache
                    .set_gpu_memory(Box::new(GpuMemoryAccessAdapter {
                        mm: Arc::clone(mm),
                        cpu_reader: Arc::clone(cpu_reader),
                    }));
                self.buffer_cache
                    .set_device_memory(Box::new(DeviceMemoryAccessAdapter {
                        cpu_reader: Arc::clone(cpu_reader),
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
