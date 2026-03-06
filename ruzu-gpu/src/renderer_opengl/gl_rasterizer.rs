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
use crate::engines::maxwell_3d::DrawCall;
use crate::engines::Framebuffer;
use crate::rasterizer::SoftwareRasterizer;
use crate::rasterizer_interface::{RasterizerDownloadArea, RasterizerInterface};
use crate::syncpoint::SyncpointManager;

/// OpenGL rasterizer matching zuyu's `RasterizerOpenGL`.
///
/// Processes draw calls from the Maxwell 3D engine using OpenGL.
pub struct RasterizerOpenGL {
    syncpoints: Arc<SyncpointManager>,
    frame_count: u64,
}

impl RasterizerOpenGL {
    /// Create a new rasterizer. Must be called with a current GL context.
    pub fn new(_device: &Device, syncpoints: Arc<SyncpointManager>) -> Self {
        Self {
            syncpoints,
            frame_count: 0,
        }
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
        has_timeout: bool,
        payload: u32,
        _subreport: u32,
        gpu_write: &dyn Fn(u64, &[u8]),
    ) {
        if has_timeout {
            let ticks: u64 = 0;
            gpu_write(gpu_addr + 8, &ticks.to_le_bytes());
            gpu_write(gpu_addr, &(payload as u64).to_le_bytes());
        } else {
            gpu_write(gpu_addr, &payload.to_le_bytes());
        }
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

    fn signal_fence(&mut self, func: Box<dyn FnOnce()>) {
        unsafe { gl::Finish(); }
        func();
    }

    fn sync_operation(&mut self, func: Box<dyn FnOnce()>) {
        func();
    }

    fn signal_sync_point(&mut self, id: u32) {
        self.syncpoints.increment(id);
    }

    fn signal_reference(&mut self) {}

    fn release_fences(&mut self, _force: bool) {}

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

    fn invalidate_gpu_cache(&mut self) {}

    fn unmap_memory(&mut self, _addr: u64, _size: u64) {}

    fn modify_gpu_memory(&mut self, _as_id: usize, _addr: u64, _size: u64) {}

    fn flush_and_invalidate_region(&mut self, _addr: u64, _size: u64) {}

    fn wait_for_idle(&mut self) {
        unsafe { gl::Finish(); }
    }

    fn fragment_barrier(&mut self) {
        unsafe { gl::MemoryBarrier(gl::FRAMEBUFFER_BARRIER_BIT); }
    }

    fn tiled_cache_barrier(&mut self) {
        unsafe { gl::MemoryBarrier(gl::FRAMEBUFFER_BARRIER_BIT); }
    }

    fn flush_commands(&mut self) {
        unsafe { gl::Flush(); }
    }

    fn tick_frame(&mut self) {
        self.frame_count += 1;
    }

    fn accelerate_surface_copy(&mut self) -> bool {
        false
    }

    fn accelerate_inline_to_memory(&mut self, _address: u64, _copy_size: usize, _memory: &[u8]) {}
}
