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

use super::gl_device::Device;
use super::gl_fence_manager::{Fence, FenceManagerOpenGL};
use crate::engines::maxwell_3d::DrawCall;
use crate::engines::Framebuffer;
use crate::fence_manager::FenceManager;
use crate::host1x::syncpoint_manager::SyncpointManager;
use crate::query_cache::types::QueryPropertiesFlags;
use crate::rasterizer::SoftwareRasterizer;
use crate::rasterizer_interface::{RasterizerDownloadArea, RasterizerInterface};

/// OpenGL rasterizer matching zuyu's `RasterizerOpenGL`.
///
/// Processes draw calls from the Maxwell 3D engine using OpenGL.
pub struct RasterizerOpenGL {
    syncpoints: Arc<SyncpointManager>,
    fence_backend: FenceManagerOpenGL,
    fence_manager: FenceManager<Fence>,
    frame_count: u64,
    num_queued_commands: u32,
    invalidate_gpu_cache_callback: Option<Arc<dyn Fn() + Send + Sync>>,
}

impl RasterizerOpenGL {
    /// Create a new rasterizer. Must be called with a current GL context.
    pub fn new(_device: &Device, syncpoints: Arc<SyncpointManager>) -> Self {
        Self {
            syncpoints,
            fence_backend: FenceManagerOpenGL::new(),
            fence_manager: FenceManager::new(),
            frame_count: 0,
            num_queued_commands: 0,
            invalidate_gpu_cache_callback: None,
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
            invalidate_gpu_cache_callback: None,
        }
    }

    /// Rust adaptation for upstream `RasterizerOpenGL::InvalidateGPUCache()`,
    /// which delegates to the owning `GPU`.
    pub fn set_invalidate_gpu_cache_callback(
        &mut self,
        callback: Arc<dyn Fn() + Send + Sync>,
    ) {
        self.invalidate_gpu_cache_callback = Some(callback);
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

        let write_backs: std::sync::Mutex<Vec<(u64, Vec<u8>)>> =
            std::sync::Mutex::new(Vec::new());
        let gpu_write = |gpu_va: u64, data: &[u8]| {
            write_backs.lock().unwrap().push((gpu_va, data.to_vec()));
        };

        SoftwareRasterizer::render_draw_calls(draw_calls, gpu_read, &gpu_write, framebuffer)
    }
}

impl RasterizerInterface for RasterizerOpenGL {
    fn draw(&mut self, _is_indexed: bool, _instance_count: u32) {
        debug!("RasterizerOpenGL::draw");
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
        unsafe { gl::Flush(); }
    }

    fn flush_region(&mut self, _addr: u64, _size: u64) {}

    fn must_flush_region(&self, _addr: u64, _size: u64) -> bool {
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

    fn invalidate_region(&mut self, _addr: u64, _size: u64) {}

    fn on_cache_invalidation(&mut self, _addr: u64, _size: u64) {}

    fn on_cpu_write(&mut self, _addr: u64, _size: u64) -> bool {
        false
    }

    fn invalidate_gpu_cache(&mut self) {
        if let Some(callback) = &self.invalidate_gpu_cache_callback {
            callback();
        }
    }

    fn unmap_memory(&mut self, _addr: u64, _size: u64) {}

    fn modify_gpu_memory(&mut self, _as_id: usize, _addr: u64, _size: u64) {}

    fn flush_and_invalidate_region(&mut self, _addr: u64, _size: u64) {}

    fn wait_for_idle(&mut self) {
        unsafe { gl::MemoryBarrier(gl::ALL_BARRIER_BITS) };
        self.signal_reference();
    }

    fn fragment_barrier(&mut self) {
        unsafe { gl::MemoryBarrier(gl::FRAMEBUFFER_BARRIER_BIT); }
    }

    fn tiled_cache_barrier(&mut self) {
        unsafe { gl::MemoryBarrier(gl::FRAMEBUFFER_BARRIER_BIT); }
    }

    fn flush_commands(&mut self) {
        if self.num_queued_commands == 0 {
            return;
        }
        unsafe { gl::Flush(); }
        self.num_queued_commands = 0;
    }

    fn tick_frame(&mut self) {
        self.frame_count += 1;
        self.fence_manager.tick_frame();
    }

    fn accelerate_surface_copy(&mut self) -> bool {
        false
    }

    fn accelerate_inline_to_memory(&mut self, _address: u64, _copy_size: usize, _memory: &[u8]) {}
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
}
