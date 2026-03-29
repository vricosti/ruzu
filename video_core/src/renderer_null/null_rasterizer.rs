// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/renderer_null/null_rasterizer.h and null_rasterizer.cpp
//! Status: COMPLET
//!
//! Null rasterizer — all drawing and rendering operations are no-ops.
//! Functional methods (query, signal_fence, sync_operation, signal_sync_point)
//! still perform their required side effects.

use std::sync::Arc;

use log::trace;

use crate::host1x::syncpoint_manager::SyncpointManager;
use crate::rasterizer_interface::{RasterizerDownloadArea, RasterizerInterface};

// ── AccelerateDMA ──────────────────────────────────────────────────────────

/// Null DMA accelerator — claims all DMA operations succeed without doing work.
///
/// Corresponds to zuyu's `Null::AccelerateDMA`.
pub struct AccelerateDMA;

impl AccelerateDMA {
    pub fn new() -> Self {
        Self
    }

    /// Pretend buffer copy succeeded.
    pub fn buffer_copy(&self, _start_address: u64, _end_address: u64, _amount: u64) -> bool {
        true
    }

    /// Pretend buffer clear succeeded.
    pub fn buffer_clear(&self, _src_address: u64, _amount: u64, _value: u32) -> bool {
        true
    }

    /// Image-to-buffer copy: not accelerated in null backend.
    pub fn image_to_buffer(&self) -> bool {
        false
    }

    /// Buffer-to-image copy: not accelerated in null backend.
    pub fn buffer_to_image(&self) -> bool {
        false
    }
}

impl Default for AccelerateDMA {
    fn default() -> Self {
        Self::new()
    }
}

// ── RasterizerNull ─────────────────────────────────────────────────────────

/// Null rasterizer — all rendering operations are no-ops.
///
/// Corresponds to zuyu's `Null::RasterizerNull`.
/// Implements [`RasterizerInterface`] with stub implementations.
/// Functional side effects (queries, fences, syncpoints) are preserved.
pub struct RasterizerNull {
    syncpoints: Arc<SyncpointManager>,
    accelerate_dma: AccelerateDMA,
}

impl RasterizerNull {
    pub fn new(syncpoints: Arc<SyncpointManager>) -> Self {
        Self {
            syncpoints,
            accelerate_dma: AccelerateDMA::new(),
        }
    }

    /// Access the DMA accelerator.
    pub fn access_accelerate_dma(&self) -> &AccelerateDMA {
        &self.accelerate_dma
    }
}

impl RasterizerInterface for RasterizerNull {
    // ── Drawing (all no-ops) ────────────────────────────────────────────

    fn draw(&mut self, _is_indexed: bool, _instance_count: u32) {
        trace!("RasterizerNull::draw (no-op)");
    }

    fn draw_texture(&mut self) {
        trace!("RasterizerNull::draw_texture (no-op)");
    }

    fn clear(&mut self, _layer_count: u32) {
        trace!("RasterizerNull::clear (no-op)");
    }

    fn dispatch_compute(&mut self) {
        trace!("RasterizerNull::dispatch_compute (no-op)");
    }

    // ── Queries ─────────────────────────────────────────────────────────

    fn reset_counter(&mut self, _query_type: u32) {}

    /// Write query result to GPU memory.
    ///
    /// Matches zuyu: if `has_timeout` is true, writes a u64 ticks value at
    /// gpu_addr+8 and the payload as u64 at gpu_addr. Otherwise writes
    /// payload as u32 at gpu_addr.
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

    // ── Uniform buffers (no-ops) ────────────────────────────────────────

    fn bind_graphics_uniform_buffer(
        &mut self,
        _stage: usize,
        _index: u32,
        _gpu_addr: u64,
        _size: u32,
    ) {
    }

    fn disable_graphics_uniform_buffer(&mut self, _stage: usize, _index: u32) {}

    // ── Synchronization ─────────────────────────────────────────────────

    /// Execute fence callback immediately (null backend has no GPU latency).
    fn signal_fence(&mut self, func: Box<dyn FnOnce()>) {
        func();
    }

    /// Execute sync operation immediately.
    fn sync_operation(&mut self, func: Box<dyn FnOnce()>) {
        func();
    }

    /// Increment the syncpoint value.
    ///
    /// Matches zuyu's `RasterizerNull::SignalSyncPoint()` which increments
    /// both guest and host syncpoints through Host1x.
    fn signal_sync_point(&mut self, id: u32) {
        self.syncpoints.increment_guest(id);
        self.syncpoints.increment_host(id);
    }

    fn signal_reference(&mut self) {}

    fn release_fences(&mut self, _force: bool) {}

    // ── Cache management (no-ops) ───────────────────────────────────────

    fn flush_all(&mut self) {}

    fn flush_region(&mut self, _addr: u64, _size: u64) {}

    fn must_flush_region(&self, _addr: u64, _size: u64) -> bool {
        false
    }

    /// Get the flush area for a given address range, aligned to page boundaries.
    fn get_flush_area(&self, addr: u64, size: u64) -> RasterizerDownloadArea {
        const DEVICE_PAGESIZE: u64 = 4096;
        RasterizerDownloadArea {
            start_address: addr & !(DEVICE_PAGESIZE - 1),
            end_address: (addr + size + DEVICE_PAGESIZE - 1) & !(DEVICE_PAGESIZE - 1),
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

    // ── Barriers / misc (no-ops) ────────────────────────────────────────

    fn wait_for_idle(&mut self) {}

    fn fragment_barrier(&mut self) {}

    fn tiled_cache_barrier(&mut self) {}

    fn flush_commands(&mut self) {}

    fn tick_frame(&mut self) {}

    // ── Acceleration ────────────────────────────────────────────────────

    /// Pretend surface copy succeeded.
    fn accelerate_surface_copy(&mut self) -> bool {
        true
    }

    fn accelerate_inline_to_memory(&mut self, _address: u64, _copy_size: usize, _memory: &[u8]) {}

    // ── Channel management ──────────────────────────────────────────────

    fn initialize_channel(&mut self, _channel_id: i32) {
        trace!("RasterizerNull::initialize_channel (no-op)");
    }

    fn bind_channel(&mut self, _channel_id: i32) {
        trace!("RasterizerNull::bind_channel (no-op)");
    }

    fn release_channel(&mut self, _channel_id: i32) {
        trace!("RasterizerNull::release_channel (no-op)");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rasterizer_null_noop() {
        let sp = Arc::new(SyncpointManager::new());
        let mut rast = RasterizerNull::new(sp);

        rast.draw(false, 1);
        rast.draw(true, 4);
        rast.draw_texture();
        rast.clear(1);
        rast.dispatch_compute();
        rast.flush_all();
        rast.wait_for_idle();
        rast.tick_frame();
        assert!(!rast.must_flush_region(0, 0));
        assert!(!rast.on_cpu_write(0, 0));
        assert!(rast.accelerate_surface_copy());
    }

    #[test]
    fn test_signal_sync_point() {
        let sp = Arc::new(SyncpointManager::new());
        let mut rast = RasterizerNull::new(sp.clone());
        rast.signal_sync_point(1);
        assert_eq!(sp.get_guest_syncpoint_value(1), 1);
        assert_eq!(sp.get_host_syncpoint_value(1), 1);
    }

    #[test]
    fn test_signal_fence_executes_immediately() {
        let sp = Arc::new(SyncpointManager::new());
        let mut rast = RasterizerNull::new(sp);

        let executed = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false));
        let flag = executed.clone();
        rast.signal_fence(Box::new(move || {
            flag.store(true, std::sync::atomic::Ordering::SeqCst);
        }));
        assert!(executed.load(std::sync::atomic::Ordering::SeqCst));
    }

    #[test]
    fn test_query_without_timeout() {
        let sp = Arc::new(SyncpointManager::new());
        let mut rast = RasterizerNull::new(sp);

        let written = std::sync::Mutex::new(Vec::new());
        rast.query(0x1000, 0, false, 42, 0, &|addr, data| {
            written.lock().unwrap().push((addr, data.to_vec()));
        });

        let w = written.lock().unwrap();
        assert_eq!(w.len(), 1);
        assert_eq!(w[0].0, 0x1000);
        assert_eq!(w[0].1, 42u32.to_le_bytes().to_vec());
    }

    #[test]
    fn test_query_with_timeout() {
        let sp = Arc::new(SyncpointManager::new());
        let mut rast = RasterizerNull::new(sp);

        let written = std::sync::Mutex::new(Vec::new());
        rast.query(0x2000, 0, true, 99, 0, &|addr, data| {
            written.lock().unwrap().push((addr, data.to_vec()));
        });

        let w = written.lock().unwrap();
        assert_eq!(w.len(), 2);
        assert_eq!(w[0].0, 0x2008);
        assert_eq!(w[0].1, 0u64.to_le_bytes().to_vec());
        assert_eq!(w[1].0, 0x2000);
        assert_eq!(w[1].1, 99u64.to_le_bytes().to_vec());
    }

    #[test]
    fn test_get_flush_area_alignment() {
        let sp = Arc::new(SyncpointManager::new());
        let rast = RasterizerNull::new(sp);

        let area = rast.get_flush_area(0x1234, 0x100);
        assert_eq!(area.start_address, 0x1000);
        assert_eq!(area.end_address, 0x2000);
        assert!(area.preemptive);
    }

    #[test]
    fn test_accelerate_dma() {
        let dma = AccelerateDMA::new();
        assert!(dma.buffer_copy(0, 0x1000, 0x1000));
        assert!(dma.buffer_clear(0, 0x1000, 0));
        assert!(!dma.image_to_buffer());
        assert!(!dma.buffer_to_image());
    }

    #[test]
    fn test_trait_object() {
        let sp = Arc::new(SyncpointManager::new());
        let mut rast: Box<dyn RasterizerInterface> =
            Box::new(RasterizerNull::new(sp));

        // Should work through the trait object
        rast.draw(false, 1);
        rast.clear(1);
        rast.flush_all();
        rast.tick_frame();
    }
}
