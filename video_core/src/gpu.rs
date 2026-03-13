// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/gpu.h and video_core/gpu.cpp
//!
//! GPU controller: manages channels, engines, rendering, and host synchronization.

use std::collections::{HashMap, VecDeque};
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{Arc, Condvar, Mutex};

use crate::dma_pusher::CommandList;
use crate::framebuffer_config::FramebufferConfig;
use crate::rasterizer_download_area::RasterizerDownloadArea;

/// Device address type.
pub type DAddr = u64;

/// Render target format enumeration.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum RenderTargetFormat {
    None = 0x0,
    R32G32B32A32Float = 0xC0,
    R32G32B32A32Sint = 0xC1,
    R32G32B32A32Uint = 0xC2,
    R32G32B32X32Float = 0xC3,
    R32G32B32X32Sint = 0xC4,
    R32G32B32X32Uint = 0xC5,
    R16G16B16A16Unorm = 0xC6,
    R16G16B16A16Snorm = 0xC7,
    R16G16B16A16Sint = 0xC8,
    R16G16B16A16Uint = 0xC9,
    R16G16B16A16Float = 0xCA,
    R32G32Float = 0xCB,
    R32G32Sint = 0xCC,
    R32G32Uint = 0xCD,
    R16G16B16X16Float = 0xCE,
    A8R8G8B8Unorm = 0xCF,
    A8R8G8B8Srgb = 0xD0,
    A2B10G10R10Unorm = 0xD1,
    A2B10G10R10Uint = 0xD2,
    A8B8G8R8Unorm = 0xD5,
    A8B8G8R8Srgb = 0xD6,
    A8B8G8R8Snorm = 0xD7,
    A8B8G8R8Sint = 0xD8,
    A8B8G8R8Uint = 0xD9,
    R16G16Unorm = 0xDA,
    R16G16Snorm = 0xDB,
    R16G16Sint = 0xDC,
    R16G16Uint = 0xDD,
    R16G16Float = 0xDE,
    A2R10G10B10Unorm = 0xDF,
    B10G11R11Float = 0xE0,
    R32Sint = 0xE3,
    R32Uint = 0xE4,
    R32Float = 0xE5,
    X8R8G8B8Unorm = 0xE6,
    X8R8G8B8Srgb = 0xE7,
    R5G6B5Unorm = 0xE8,
    A1R5G5B5Unorm = 0xE9,
    R8G8Unorm = 0xEA,
    R8G8Snorm = 0xEB,
    R8G8Sint = 0xEC,
    R8G8Uint = 0xED,
    R16Unorm = 0xEE,
    R16Snorm = 0xEF,
    R16Sint = 0xF0,
    R16Uint = 0xF1,
    R16Float = 0xF2,
    R8Unorm = 0xF3,
    R8Snorm = 0xF4,
    R8Sint = 0xF5,
    R8Uint = 0xF6,
    X1R5G5B5Unorm = 0xF8,
    X8B8G8R8Unorm = 0xF9,
    X8B8G8R8Srgb = 0xFA,
}

/// Depth buffer format enumeration.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum DepthFormat {
    Z32Float = 0xA,
    Z16Unorm = 0x13,
    Z24UnormS8Uint = 0x14,
    X8Z24Unorm = 0x15,
    S8Z24Unorm = 0x16,
    S8Uint = 0x17,
    V8Z24Unorm = 0x18,
    Z32FloatX24S8Uint = 0x19,
}

/// The GPU controller, using pimpl pattern matching upstream.
pub struct Gpu {
    is_async: bool,
    use_nvdec: bool,
    shutting_down: AtomicBool,

    // Sync request infrastructure
    sync_requests: Mutex<VecDeque<Box<dyn FnOnce() + Send>>>,
    current_sync_fence: AtomicU64,
    last_sync_fence: Mutex<u64>,
    sync_request_cv: Condvar,

    // Channel management
    new_channel_id: Mutex<i32>,
    bound_channel: Mutex<i32>,
    // In the full port:
    // renderer: Option<Box<dyn RendererBase>>,
    // rasterizer: *mut RasterizerInterface,
    // host1x: &Host1x,
    // gpu_thread: ThreadManager,
    // scheduler: Box<Scheduler>,
    // channels: HashMap<i32, Arc<ChannelState>>,
}

impl Gpu {
    /// Creates a new GPU controller.
    pub fn new(is_async: bool, use_nvdec: bool) -> Self {
        Self {
            is_async,
            use_nvdec,
            shutting_down: AtomicBool::new(false),
            sync_requests: Mutex::new(VecDeque::new()),
            current_sync_fence: AtomicU64::new(0),
            last_sync_fence: Mutex::new(0),
            sync_request_cv: Condvar::new(),
            new_channel_id: Mutex::new(1),
            bound_channel: Mutex::new(-1),
        }
    }

    /// Flush all current written commands into the host GPU for execution.
    pub fn flush_commands(&self) {
        // NOTE: Full implementation calls rasterizer->FlushCommands().
        // Stubbed until rasterizer integration is complete.
        log::warn!("Gpu::flush_commands: rasterizer not integrated, skipping flush");
    }

    /// Synchronizes CPU writes with Host GPU memory.
    pub fn invalidate_gpu_cache(&self) {
        // NOTE: Full implementation calls system.GatherGPUDirtyMemory then
        // rasterizer->OnCacheInvalidation for each dirty range.
        // Stubbed until system/rasterizer integration is complete.
        log::warn!("Gpu::invalidate_gpu_cache: system integration not available, skipping");
    }

    /// Signal the ending of command list.
    pub fn on_command_list_end(&self) {
        // NOTE: Full implementation calls rasterizer->ReleaseFences(false) then
        // Settings::UpdateGPUAccuracy().
        // Stubbed until rasterizer integration is complete.
        log::warn!("Gpu::on_command_list_end: rasterizer not integrated, skipping");
    }

    /// Request a host GPU memory flush from the CPU.
    pub fn request_flush(&self, _addr: DAddr, _size: usize) -> u64 {
        // NOTE: Full implementation calls RequestSyncOperation which enqueues a flush.
        // Returns the next fence counter value.
        // Stubbed until rasterizer integration is complete.
        let fence = self.current_sync_fence.load(Ordering::Relaxed) + 1;
        log::warn!("Gpu::request_flush: rasterizer not integrated, returning fence {}", fence);
        fence
    }

    /// Obtains current flush request fence id.
    pub fn current_sync_request_fence(&self) -> u64 {
        self.current_sync_fence.load(Ordering::Relaxed)
    }

    /// Wait for a sync operation to complete.
    pub fn wait_for_sync_operation(&self, fence: u64) {
        let mut guard = self.sync_requests.lock().unwrap();
        while self.current_sync_fence.load(Ordering::Relaxed) < fence {
            guard = self.sync_request_cv.wait(guard).unwrap();
        }
    }

    /// Tick pending requests within the GPU.
    pub fn tick_work(&self) {
        let mut requests = self.sync_requests.lock().unwrap();
        while let Some(request) = requests.pop_front() {
            drop(requests);
            request();
            self.current_sync_fence.fetch_add(1, Ordering::Release);
            requests = self.sync_requests.lock().unwrap();
            self.sync_request_cv.notify_all();
        }
    }

    /// Returns the GPU ticks.
    pub fn get_ticks(&self) -> u64 {
        // NOTE: Full implementation calls system.CoreTiming().GetGPUTicks()
        // and divides by 256 when use_fast_gpu_time is enabled.
        // Stubbed until CoreTiming integration is complete.
        0
    }

    /// Returns whether async GPU mode is enabled.
    pub fn is_async(&self) -> bool {
        self.is_async
    }

    /// Returns whether NVDEC is enabled.
    pub fn use_nvdec(&self) -> bool {
        self.use_nvdec
    }

    /// Start the GPU thread.
    pub fn start(&self) {
        // NOTE: Full implementation calls gpu_thread.StartThread(*renderer, renderer->Context(), *scheduler).
        // Stubbed until renderer/gpu_thread integration is complete.
        log::warn!("Gpu::start: gpu_thread and renderer not integrated, skipping");
    }

    /// Notify shutdown.
    pub fn notify_shutdown(&self) {
        self.shutting_down.store(true, Ordering::Relaxed);
    }

    /// Push GPU command entries to be processed.
    pub fn push_gpu_entries(&self, _channel: i32, _entries: CommandList) {
        // NOTE: Full implementation calls gpu_thread.SubmitList(channel, entries).
        // Stubbed until gpu_thread integration is complete.
        log::warn!("Gpu::push_gpu_entries: gpu_thread not integrated, dropping entries");
    }

    /// Notify rasterizer about a CPU read.
    pub fn on_cpu_read(&self, addr: DAddr, _size: u64) -> RasterizerDownloadArea {
        // NOTE: Full implementation calls rasterizer->GetFlushArea, then
        // RequestSyncOperation to flush the area, then WaitForSyncOperation.
        // Return a preemptive area covering the address.
        log::warn!("Gpu::on_cpu_read: rasterizer not integrated, returning empty area");
        RasterizerDownloadArea {
            start_address: addr,
            end_address: addr,
            preemtive: true,
        }
    }

    /// Flush a region.
    pub fn flush_region(&self, _addr: DAddr, _size: u64) {
        // NOTE: Full implementation calls gpu_thread.FlushRegion(addr, size).
        // Stubbed until gpu_thread integration is complete.
        log::warn!("Gpu::flush_region: gpu_thread not integrated, skipping flush");
    }

    /// Invalidate a region.
    pub fn invalidate_region(&self, _addr: DAddr, _size: u64) {
        // NOTE: Full implementation calls gpu_thread.InvalidateRegion(addr, size).
        // Stubbed until gpu_thread integration is complete.
        log::warn!("Gpu::invalidate_region: gpu_thread not integrated, skipping invalidation");
    }

    /// Notify rasterizer of a CPU write.
    pub fn on_cpu_write(&self, _addr: DAddr, _size: u64) -> bool {
        // NOTE: Full implementation calls rasterizer->OnCPUWrite(addr, size).
        // Stubbed until rasterizer integration is complete; return false (no cache hit).
        false
    }

    /// Flush and invalidate a region.
    pub fn flush_and_invalidate_region(&self, _addr: DAddr, _size: u64) {
        // NOTE: Full implementation calls gpu_thread.FlushAndInvalidateRegion(addr, size).
        // Stubbed until gpu_thread integration is complete.
        log::warn!("Gpu::flush_and_invalidate_region: gpu_thread not integrated, skipping");
    }

    /// Request framebuffer compositing.
    pub fn request_composite(&self, _layers: Vec<FramebufferConfig>) {
        // NOTE: Full implementation enqueues a sync operation that calls
        // renderer->Composite(layers) after all pending fences are signaled.
        // Stubbed until renderer and syncpoint integration is complete.
        log::warn!("Gpu::request_composite: renderer not integrated, skipping composite");
    }

    /// Get the applet capture buffer.
    pub fn get_applet_capture_buffer(&self) -> Vec<u8> {
        // NOTE: Full implementation calls renderer->GetAppletCaptureBuffer() via sync operation.
        // Stubbed until renderer integration is complete.
        Vec::new()
    }

    /// Renderer frame end notification.
    pub fn renderer_frame_end_notify(&self) {
        // NOTE: Full implementation calls system.GetPerfStats().EndGameFrame().
        // Stubbed until perf stats integration is complete.
    }
}
