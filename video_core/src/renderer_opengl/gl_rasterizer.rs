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
use super::gl_shader_cache::ShaderCache as OpenGLShaderCache;
use crate::buffer_cache::buffer_cache::BufferCache as CommonBufferCache;
use crate::buffer_cache::word_manager::DeviceTracker;
use crate::engines::draw_manager::DrawState;
use crate::engines::maxwell_3d::DrawCall;
use crate::engines::Framebuffer;
use crate::fence_manager::FenceManager;
use crate::host1x::syncpoint_manager::SyncpointManager;
use crate::query_cache::types::QueryPropertiesFlags;
use crate::query_cache_top::QueryCacheLegacy;
use crate::rasterizer::SoftwareRasterizer;
use crate::rasterizer_interface::{RasterizerDownloadArea, RasterizerInterface};
use crate::shader_cache::ShaderCache;
use crate::texture_cache::texture_cache_base::TextureCacheBase;

struct OpenGLDeviceTracker;
static OPENGL_DEVICE_TRACKER: OpenGLDeviceTracker = OpenGLDeviceTracker;

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

    fn get_compute_launch_info(
        &self,
    ) -> crate::buffer_cache::buffer_cache_base::ComputeLaunchInfo {
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
    texture_cache: TextureCacheBase,
    /// Generic region-tracking shader cache (region invalidation, guest address bookkeeping).
    /// Upstream inherits `GL::ShaderCache` from `VideoCommon::ShaderCache`; we keep them
    /// as two separate composed fields so each can evolve independently.
    shader_cache: ShaderCache,
    /// OpenGL-specific shader cache — owns compiled `GraphicsPipeline` / `ComputePipeline`
    /// objects and is the entry point for the draw hot path.
    gl_shader_cache: OpenGLShaderCache,
    query_cache: QueryCacheLegacy,
    invalidate_gpu_cache_callback: Option<Arc<dyn Fn() + Send + Sync>>,
    /// Per-channel GPU memory manager, extracted from `ChannelState` in
    /// `bind_channel`. Used to build the `GpuMemoryAccess` adapter for the
    /// buffer cache.
    channel_memory_manager:
        Option<Arc<parking_lot::Mutex<crate::memory_manager::MemoryManager>>>,
    /// CPU/device memory reader callback, set via the shader cache's
    /// `set_gpu_memory_reader` path. Re-used for the buffer cache's
    /// `DeviceMemoryAccess` and for `GpuMemoryAccess::read_u32/u64`.
    cpu_memory_reader: Option<crate::shader_environment::GpuMemoryReader>,
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
    pub fn new(device: &Device, syncpoints: Arc<SyncpointManager>) -> Self {
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
            fence_manager: FenceManager::new(),
            frame_count: 0,
            num_queued_commands: 0,
            has_written_global_memory: false,
            buffer_cache,
            texture_cache: TextureCacheBase::new(),
            shader_cache: ShaderCache::new(),
            gl_shader_cache: OpenGLShaderCache::new(),
            query_cache: QueryCacheLegacy::new(),
            invalidate_gpu_cache_callback: None,
            channel_memory_manager: None,
            cpu_memory_reader: None,
            transient_vao,
        }
    }

    #[cfg(test)]
    fn new_for_test(syncpoints: Arc<SyncpointManager>) -> Self {
        Self {
            syncpoints,
            fence_backend: FenceManagerOpenGL::new(),
            fence_manager: FenceManager::new(),
            frame_count: 0,
            num_queued_commands: 0,
            has_written_global_memory: false,
            buffer_cache: CommonBufferCache::new(&OPENGL_DEVICE_TRACKER),
            texture_cache: TextureCacheBase::new(),
            shader_cache: ShaderCache::new(),
            gl_shader_cache: OpenGLShaderCache::new(),
            query_cache: QueryCacheLegacy::new(),
            invalidate_gpu_cache_callback: None,
            channel_memory_manager: None,
            cpu_memory_reader: None,
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
    pub fn set_gpu_memory_reader(
        &mut self,
        reader: crate::shader_environment::GpuMemoryReader,
    ) {
        self.cpu_memory_reader = Some(Arc::clone(&reader));
        self.gl_shader_cache.set_gpu_memory_reader(reader);
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
    ///   `texture_cache.mutex`, `SyncState`, `SetEngine`) and the actual
    ///   `glDraw*` family are still deferred to step 4+. They require real
    ///   buffer/texture caches and a compiled GL program.
    ///
    /// The `MaxwellToGL::PrimitiveTopology` mapping is intentionally kept
    /// here (not on the pipeline) because upstream re-reads topology on
    /// every `Draw` — a single pipeline key may be drawn with multiple
    /// topologies in successive calls.
    fn draw(&mut self, draw_state: &DrawState, instance_count: u32) {
        // Hand the per-stage shader program addresses snapshotted by
        // `DrawManager::process_draw` to the GL shader cache so that
        // `current_graphics_pipeline` can build a real, Maxwell-derived
        // `GraphicsPipelineKey` instead of always reusing the default key.
        // Upstream `RasterizerOpenGL` reaches the same data via its
        // Maxwell3D back-pointer; Rust threads it through `DrawState`.
        self.gl_shader_cache
            .set_pending_program_addresses(draw_state.shader_program_addresses);

        let Some(pipeline) = self.gl_shader_cache.current_graphics_pipeline() else {
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
        if !pipeline.has_gl_programs()
            && pipeline.glsl_sources.iter().any(|s| s.is_some())
        {
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
        self.buffer_cache.set_engine_state(Box::new(DrawStateEngineAdapter {
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

    fn clear(&mut self, _layer_count: u32) {
        debug!("RasterizerOpenGL::clear");
    }

    fn dispatch_compute(&mut self) {
        debug!("RasterizerOpenGL::dispatch_compute");
    }

    fn reset_counter(&mut self, _query_type: u32) {}

    fn query(
        &mut self,
        gpu_addr: u64,
        _query_type: u32,
        flags: QueryPropertiesFlags,
        gpu_ticks: u64,
        payload: u32,
        _subreport: u32,
        gpu_write: Arc<dyn Fn(u64, &[u8]) + Send + Sync>,
    ) {
        let has_timeout = flags.contains(QueryPropertiesFlags::HAS_TIMEOUT);
        let func = Box::new(move || {
            if has_timeout {
                gpu_write(gpu_addr + 8, &gpu_ticks.to_le_bytes());
                gpu_write(gpu_addr, &(payload as u64).to_le_bytes());
            } else {
                gpu_write(gpu_addr, &payload.to_le_bytes());
            }
        });
        if flags.contains(QueryPropertiesFlags::IS_A_FENCE) {
            self.signal_fence(func);
            return;
        }
        func();
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
        let should_flush = self.num_queued_commands != 0;
        let should_flush_now = self.fence_manager.signal_fence(
            func,
            |is_stubbed| self.fence_backend.create_fence(is_stubbed),
            |fence| self.fence_backend.queue_fence(fence),
            || false,
            |fence| self.fence_backend.is_fence_signaled(fence),
            || {},
            move || should_flush,
            || {},
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
        let should_flush = self.num_queued_commands != 0;
        let syncpoints = Arc::clone(&self.syncpoints);
        let should_flush_now = self.fence_manager.signal_sync_point(
            id,
            {
                let syncpoints = Arc::clone(&syncpoints);
                move |value| syncpoints.increment_guest(value)
            },
            move |value| syncpoints.increment_host(value),
            |is_stubbed| self.fence_backend.create_fence(is_stubbed),
            |fence| self.fence_backend.queue_fence(fence),
            || false,
            |fence| self.fence_backend.is_fence_signaled(fence),
            || {},
            move || should_flush,
            || {},
        );
        if should_flush_now {
            self.flush_commands();
        }
        self.invalidate_gpu_cache();
    }

    fn signal_reference(&mut self) {
        self.fence_manager.signal_ordering(
            || false,
            |fence| self.fence_backend.is_fence_signaled(fence),
            || {},
            || {},
        );
    }

    fn release_fences(&mut self, force: bool) {
        self.fence_manager.wait_pending_fences(
            force,
            || false,
            |fence| self.fence_backend.is_fence_signaled(fence),
            |fence| self.fence_backend.wait_fence(fence),
            || {},
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

    fn accelerate_surface_copy(&mut self) -> bool {
        false
    }

    fn accelerate_inline_to_memory(&mut self, _address: u64, _copy_size: usize, _memory: &[u8]) {}

    fn initialize_channel(
        &mut self,
        _channel: &crate::control::channel_state::ChannelState,
    ) {
        // Upstream `RasterizerOpenGL` inherits from `ChannelSetupCaches`
        // which allocates per-channel state for the buffer/texture/shader
        // caches. The Rust port initialises the buffer cache's channel
        // state lazily on first `bind_channel` instead.
    }

    fn bind_channel(
        &mut self,
        channel: &crate::control::channel_state::ChannelState,
    ) {
        if self.buffer_cache.channel_state.is_none() {
            self.buffer_cache.channel_state = Some(Box::default());
        }
        // Extract the channel's MemoryManager and store it so subsequent
        // draws can build GpuMemoryAccess adapters.
        if let Some(ref mm) = channel.memory_manager {
            self.channel_memory_manager = Some(Arc::clone(mm));

            // If we already have the CPU reader (set_gpu_memory_reader was
            // called before bind_channel), install GPU + device memory
            // access on the buffer cache now.
            if let Some(ref cpu_reader) = self.cpu_memory_reader {
                self.buffer_cache.set_gpu_memory(Box::new(GpuMemoryAccessAdapter {
                    mm: Arc::clone(mm),
                    cpu_reader: Arc::clone(cpu_reader),
                }));
                self.buffer_cache.set_device_memory(Box::new(DeviceMemoryAccessAdapter {
                    cpu_reader: Arc::clone(cpu_reader),
                }));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use common::settings;
    use common::settings_enums::GpuAccuracy;

    #[test]
    fn query_fence_defers_guest_write_until_release() {
        let syncpoints = Arc::new(SyncpointManager::new());
        let mut rast = RasterizerOpenGL::new_for_test(syncpoints);
        let writes = Arc::new(std::sync::Mutex::new(Vec::<(u64, Vec<u8>)>::new()));
        let writes_cb = Arc::clone(&writes);

        rast.query(
            0x1000,
            0,
            QueryPropertiesFlags::IS_A_FENCE,
            0,
            0x1234_5678,
            0,
            Arc::new(move |addr, data| {
                writes_cb.lock().unwrap().push((addr, data.to_vec()));
            }),
        );

        assert!(writes.lock().unwrap().is_empty());

        rast.release_fences(false);

        let writes = writes.lock().unwrap();
        assert_eq!(writes.len(), 1);
        assert_eq!(writes[0].0, 0x1000);
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
    fn query_non_payload_preserves_payload() {
        let syncpoints = Arc::new(SyncpointManager::new());
        let mut rast = RasterizerOpenGL::new_for_test(syncpoints);
        let writes = Arc::new(std::sync::Mutex::new(Vec::<(u64, Vec<u8>)>::new()));
        let writes_cb = Arc::clone(&writes);

        rast.query(
            0x3000,
            3,
            QueryPropertiesFlags::empty(),
            0,
            0xCAFE_BABE,
            0,
            Arc::new(move |addr, data| {
                writes_cb.lock().unwrap().push((addr, data.to_vec()));
            }),
        );

        let writes = writes.lock().unwrap();
        assert_eq!(writes.len(), 1);
        assert_eq!(writes[0].0, 0x3000);
        assert_eq!(writes[0].1, 0xCAFE_BABEu32.to_le_bytes().to_vec());
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
