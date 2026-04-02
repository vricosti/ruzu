// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/rasterizer_interface.h
//! Status: COMPLET
//!
//! Abstract interface for GPU rasterizer backends. Each renderer
//! (Null, OpenGL, Vulkan) provides its own implementation.

use std::sync::Arc;

use crate::control::channel_state::ChannelState;
use crate::query_cache::types::QueryPropertiesFlags;

/// Shader loading callback stages.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LoadCallbackStage {
    Prepare,
    Build,
    Complete,
}

/// Callback for disk resource loading progress.
pub type DiskResourceLoadCallback = Box<dyn Fn(LoadCallbackStage, usize, usize)>;

/// Download area for flushing GPU caches to CPU memory.
#[derive(Debug, Clone)]
pub struct RasterizerDownloadArea {
    pub start_address: u64,
    pub end_address: u64,
    pub preemptive: bool,
}

/// Abstract rasterizer interface — corresponds to zuyu's `VideoCore::RasterizerInterface`.
///
/// Defines the full set of operations a GPU rasterizer must support.
/// Methods with default implementations are optional (matching C++ virtual with body).
/// Methods without defaults are pure virtual (= 0) in the C++.
pub trait RasterizerInterface {
    // ── Drawing ─────────────────────────────────────────────────────────

    /// Dispatch a draw invocation.
    fn draw(&mut self, is_indexed: bool, instance_count: u32);

    /// Dispatch an indirect draw invocation.
    fn draw_indirect(&mut self) {}

    /// Dispatch a draw texture invocation.
    fn draw_texture(&mut self);

    /// Clear the current framebuffer.
    fn clear(&mut self, layer_count: u32);

    /// Dispatch a compute shader invocation.
    fn dispatch_compute(&mut self);

    // ── Queries ──────────────────────────────────────────────────────────

    /// Reset the counter of a query.
    fn reset_counter(&mut self, query_type: u32);

    /// Record a GPU query and cache it.
    ///
    /// `gpu_write` writes data to GPU virtual address space.
    fn query(
        &mut self,
        gpu_addr: u64,
        query_type: u32,
        flags: QueryPropertiesFlags,
        gpu_ticks: u64,
        payload: u32,
        subreport: u32,
        gpu_write: Arc<dyn Fn(u64, &[u8]) + Send + Sync>,
    );

    // ── Uniform buffers ─────────────────────────────────────────────────

    /// Signal a uniform buffer binding.
    fn bind_graphics_uniform_buffer(&mut self, stage: usize, index: u32, gpu_addr: u64, size: u32);

    /// Signal disabling of a uniform buffer.
    fn disable_graphics_uniform_buffer(&mut self, stage: usize, index: u32);

    // ── Synchronization ─────────────────────────────────────────────────

    /// Signal a GPU-based semaphore as a fence.
    fn signal_fence(&mut self, func: Box<dyn FnOnce() + Send>);

    /// Send an operation to be done after a certain amount of flushes.
    fn sync_operation(&mut self, func: Box<dyn FnOnce() + Send>);

    /// Signal a GPU-based syncpoint as a fence.
    fn signal_sync_point(&mut self, value: u32);

    /// Signal a GPU-based reference point.
    fn signal_reference(&mut self);

    /// Release all pending fences.
    fn release_fences(&mut self, force: bool);

    // ── Cache management ────────────────────────────────────────────────

    /// Flush all caches to Switch memory.
    fn flush_all(&mut self);

    /// Flush caches of the specified region to Switch memory.
    fn flush_region(&mut self, addr: u64, size: u64);

    /// Check if the specified memory area requires flushing to CPU memory.
    fn must_flush_region(&self, addr: u64, size: u64) -> bool;

    /// Get the download area for flushing a region.
    fn get_flush_area(&self, addr: u64, size: u64) -> RasterizerDownloadArea;

    /// Invalidate caches of the specified region.
    fn invalidate_region(&mut self, addr: u64, size: u64);

    /// Invalidate multiple regions at once.
    fn inner_invalidation(&mut self, sequences: &[(u64, usize)]) {
        for &(addr, size) in sequences {
            self.invalidate_region(addr, size as u64);
        }
    }

    /// Notify that caches of the specified region are desynced with guest.
    fn on_cache_invalidation(&mut self, addr: u64, size: u64);

    /// Notify of a CPU write to the specified region.
    fn on_cpu_write(&mut self, addr: u64, size: u64) -> bool;

    /// Sync memory between guest and host.
    fn invalidate_gpu_cache(&mut self);

    /// Unmap a memory range.
    fn unmap_memory(&mut self, addr: u64, size: u64);

    /// Notify that GPU memory backing changed.
    fn modify_gpu_memory(&mut self, as_id: usize, addr: u64, size: u64);

    /// Flush and invalidate caches of the specified region.
    fn flush_and_invalidate_region(&mut self, addr: u64, size: u64);

    // ── Barriers / sync ─────────────────────────────────────────────────

    /// Wait for previous primitive and compute operations.
    fn wait_for_idle(&mut self);

    /// Wait for reads and writes to render targets and flush caches.
    fn fragment_barrier(&mut self);

    /// Make available previous render target writes.
    fn tiled_cache_barrier(&mut self);

    /// Send all written commands to the host GPU.
    fn flush_commands(&mut self);

    /// Notify that a frame is about to finish.
    fn tick_frame(&mut self);

    // ── Acceleration ────────────────────────────────────────────────────

    /// Attempt conditional rendering acceleration.
    fn accelerate_conditional_rendering(&mut self) -> bool {
        false
    }

    /// Attempt to use a faster method to perform a surface copy.
    fn accelerate_surface_copy(&mut self) -> bool {
        false
    }

    /// Accelerate inline memory write.
    fn accelerate_inline_to_memory(&mut self, address: u64, copy_size: usize, memory: &[u8]);

    // ── Disk resources ──────────────────────────────────────────────────

    /// Initialize disk cached resources for the game being emulated.
    fn load_disk_resources(&mut self, _title_id: u64) {}

    // ── Channel management ──────────────────────────────────────────────

    /// Initialize a GPU channel.
    fn initialize_channel(&mut self, _channel: &ChannelState) {}

    /// Bind a GPU channel.
    fn bind_channel(&mut self, _channel: &ChannelState) {}

    /// Release a GPU channel.
    fn release_channel(&mut self, _channel_id: i32) {}

    // ── Transform feedback ──────────────────────────────────────────────

    /// Register the address as a Transform Feedback Object.
    fn register_transform_feedback(&mut self, _tfb_object_addr: u64) {}

    /// Returns true when the rasterizer has Draw Transform Feedback capabilities.
    fn has_draw_transform_feedback(&self) -> bool {
        false
    }
}
