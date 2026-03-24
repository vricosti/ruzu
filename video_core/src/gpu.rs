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
use crate::renderer_base::RendererBase;

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

    /// The renderer backend (OpenGL, Vulkan, or Null).
    /// Upstream: `std::unique_ptr<VideoCore::RendererBase> renderer` in GPU::Impl.
    renderer: Mutex<Option<Box<dyn RendererBase>>>,

    /// Rasterizer extracted from renderer.
    /// Upstream: `VideoCore::RasterizerInterface* rasterizer` in GPU::Impl.
    rasterizer: std::sync::atomic::AtomicPtr<()>,

    /// GPU channel scheduler.
    /// Upstream: `std::unique_ptr<Tegra::Control::Scheduler> scheduler` in GPU::Impl.
    /// Initialized after Gpu is constructed (needs self-referential pointer).
    scheduler: Mutex<Option<Box<crate::control::scheduler::Scheduler>>>,

    /// GPU command thread manager.
    /// Upstream: `VideoCommon::GPUThread::ThreadManager gpu_thread` in GPU::Impl.
    gpu_thread: Mutex<crate::gpu_thread::ThreadManager>,

    /// Registered GPU channels.
    /// Upstream: `std::unordered_map<s32, std::shared_ptr<ChannelState>> channels`.
    channels: Mutex<HashMap<i32, Arc<parking_lot::Mutex<crate::control::channel_state::ChannelState>>>>,
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
            renderer: Mutex::new(None),
            rasterizer: std::sync::atomic::AtomicPtr::new(std::ptr::null_mut()),
            scheduler: Mutex::new(None),
            gpu_thread: Mutex::new(crate::gpu_thread::ThreadManager::new(is_async)),
            channels: Mutex::new(HashMap::new()),
        }
    }

    /// Initialize the scheduler. Must be called after Gpu is placed at its
    /// final address (the scheduler stores a raw pointer back to the Gpu).
    ///
    /// Upstream creates the scheduler in the GPU::Impl constructor:
    ///   `scheduler{std::make_unique<Control::Scheduler>(gpu)}`
    /// We defer it because Rust cannot take `&self` in the constructor.
    pub fn init_scheduler(&self) {
        let scheduler = unsafe {
            crate::control::scheduler::Scheduler::new(self as *const Gpu)
        };
        *self.scheduler.lock().unwrap() = Some(Box::new(scheduler));
    }

    /// Binds a renderer to the GPU.
    ///
    /// Upstream: `GPU::Impl::BindRenderer(unique_ptr<RendererBase> renderer_)`
    /// Also extracts the rasterizer and binds it to host1x memory manager.
    pub fn bind_renderer(&self, renderer: Box<dyn RendererBase>) {
        // Extract rasterizer from renderer.
        // Upstream: rasterizer = renderer->ReadRasterizer();
        let rasterizer_ptr = renderer.read_rasterizer();
        self.rasterizer.store(rasterizer_ptr as *mut (), Ordering::Release);
        // Upstream also does:
        // host1x.MemoryManager().BindInterface(rasterizer);
        // host1x.GMMU().BindRasterizer(rasterizer);
        log::info!("Gpu::bind_renderer: renderer bound (vendor: {})", renderer.get_device_vendor());
        *self.renderer.lock().unwrap() = Some(renderer);
    }

    /// Bind a GPU channel by its bind ID.
    ///
    /// Matches upstream `GPU::Impl::BindChannel(s32 channel_id)`.
    /// Sets the active channel for command processing.
    pub fn bind_channel(&self, channel_id: i32) {
        *self.bound_channel.lock().unwrap() = channel_id;
    }

    /// Create a new GPU channel and register it with the scheduler.
    ///
    /// Matches upstream `GPU::Impl::CreateChannel(s32 channel_id)`.
    pub fn create_channel(&self, channel_id: i32) -> Arc<parking_lot::Mutex<crate::control::channel_state::ChannelState>> {
        let channel_state = Arc::new(parking_lot::Mutex::new(
            crate::control::channel_state::ChannelState::new(channel_id),
        ));
        self.channels.lock().unwrap().insert(channel_id, channel_state.clone());

        // Register with scheduler.
        if let Some(ref mut scheduler) = *self.scheduler.lock().unwrap() {
            scheduler.declare_channel(channel_state.clone());
        }

        channel_state
    }

    /// Allocate a new channel ID.
    ///
    /// Matches upstream `GPU::Impl::AllocateChannel()`.
    pub fn allocate_channel(&self) -> i32 {
        let mut id = self.new_channel_id.lock().unwrap();
        let channel_id = *id;
        *id += 1;
        channel_id
    }

    /// Returns a reference to the renderer, if bound.
    pub fn renderer(&self) -> std::sync::MutexGuard<'_, Option<Box<dyn RendererBase>>> {
        self.renderer.lock().unwrap()
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
    /// Queue a synchronization operation to be executed by TickWork().
    /// Returns a fence number that can be passed to wait_for_sync_operation.
    ///
    /// Matches upstream `GPU::Impl::RequestSyncOperation(Func&& action)`.
    pub fn request_sync_operation(&self, action: Box<dyn FnOnce() + Send>) -> u64 {
        let mut requests = self.sync_requests.lock().unwrap();
        let mut last = self.last_sync_fence.lock().unwrap();
        *last += 1;
        let fence = *last;
        requests.push_back(action);
        fence
    }

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
    ///
    /// Upstream: `GPU::Impl::Start()` calls `Settings::UpdateGPUAccuracy()`
    /// then `gpu_thread.StartThread(*renderer, renderer->Context(), *scheduler)`.
    pub fn start(&self) {
        // Initialize scheduler if not already done.
        if self.scheduler.lock().unwrap().is_none() {
            self.init_scheduler();
        }

        let mut renderer_guard = self.renderer.lock().unwrap();
        if let Some(ref mut renderer) = *renderer_guard {
            log::info!("Gpu::start: GPU started (renderer bound, starting GPU thread)");
            // Upstream: gpu_thread.StartThread(*renderer, renderer->Context(), *scheduler)
            let gpu_ptr = self as *const Gpu;
            let renderer_ptr = renderer.as_mut() as *mut dyn RendererBase;
            let context_ptr = renderer.context_ptr();
            let scheduler_guard = self.scheduler.lock().unwrap();
            let scheduler_ptr = scheduler_guard
                .as_ref()
                .map(|s| s.as_ref() as *const crate::control::scheduler::Scheduler)
                .unwrap_or(std::ptr::null());
            drop(scheduler_guard);
            drop(renderer_guard);
            // Safety: Gpu, renderer, and scheduler outlive the ThreadManager.
            unsafe {
                self.gpu_thread.lock().unwrap().start_thread(
                    gpu_ptr,
                    renderer_ptr,
                    context_ptr,
                    scheduler_ptr,
                );
            }
        } else {
            log::warn!("Gpu::start: no renderer bound");
        }
    }

    /// Notify shutdown.
    pub fn notify_shutdown(&self) {
        self.shutting_down.store(true, Ordering::Relaxed);
    }

    /// Push GPU command entries to be processed.
    /// Matches upstream `GPU::Impl::PushGPUEntries(s32, CommandList&&)`.
    pub fn push_gpu_entries(&self, channel: i32, entries: CommandList) {
        self.gpu_thread.lock().unwrap().submit_list(channel, entries);
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
    /// Matches upstream `GPU::Impl::FlushRegion(DAddr, u64)`.
    pub fn flush_region(&self, addr: DAddr, size: u64) {
        self.gpu_thread.lock().unwrap().flush_region(addr, size);
    }

    /// Invalidate a region.
    /// Matches upstream `GPU::Impl::InvalidateRegion(DAddr, u64)`.
    pub fn invalidate_region(&self, addr: DAddr, size: u64) {
        self.gpu_thread.lock().unwrap().invalidate_region(addr, size);
    }

    /// Notify rasterizer of a CPU write.
    /// Matches upstream `GPU::Impl::OnCPUWrite(DAddr, u64)`.
    pub fn on_cpu_write(&self, _addr: DAddr, _size: u64) -> bool {
        // Upstream: calls rasterizer->OnCPUWrite(addr, size).
        // Returns true if GPU caches were affected.
        // Requires rasterizer integration.
        false
    }

    /// Flush and invalidate a region.
    /// Matches upstream `GPU::Impl::FlushAndInvalidateRegion(DAddr, u64)`.
    pub fn flush_and_invalidate_region(&self, addr: DAddr, size: u64) {
        self.gpu_thread.lock().unwrap().flush_and_invalidate_region(addr, size);
    }

    /// Request framebuffer compositing.
    ///
    /// Upstream: `GPU::Impl::RequestComposite(layers, fences)` enqueues a sync
    /// operation that waits for fences then calls `renderer->Composite(layers)`.
    /// Request a composite (frame presentation).
    ///
    /// Matches upstream `GPU::Impl::RequestComposite(layers, fences)`:
    /// queues the composite as a sync operation, signals the GPU thread
    /// via TickGPU, then waits for execution.
    ///
    /// Simplified: upstream also handles NvFence gating for multi-fence
    /// swap chains. We skip fence gating and composite directly.
    pub fn request_composite(&self, layers: Vec<FramebufferConfig>) {
        // Capture a raw pointer to self for the callback via usize (Send-safe).
        // Safety: the Gpu outlives the sync request (we wait for it below).
        let gpu_addr = self as *const Gpu as usize;
        let wait_fence = self.request_sync_operation(Box::new(move || {
            let gpu = unsafe { &*(gpu_addr as *const Gpu) };
            let mut renderer_guard = gpu.renderer.lock().unwrap();
            if let Some(ref mut renderer) = *renderer_guard {
                renderer.composite(&layers);
            }
        }));
        self.gpu_thread.lock().unwrap().tick_gpu();
        self.wait_for_sync_operation(wait_fence);
    }

    /// Get the applet capture buffer.
    ///
    /// Matches upstream: queues via sync request + TickGPU + wait.
    pub fn get_applet_capture_buffer(&self) -> Vec<u8> {
        use std::sync::{Arc, Mutex};
        let result = Arc::new(Mutex::new(Vec::new()));
        let result_clone = result.clone();
        let gpu_addr = self as *const Gpu as usize;
        let wait_fence = self.request_sync_operation(Box::new(move || {
            let gpu = unsafe { &*(gpu_addr as *const Gpu) };
            let renderer_guard = gpu.renderer.lock().unwrap();
            if let Some(ref renderer) = *renderer_guard {
                *result_clone.lock().unwrap() = renderer.get_applet_capture_buffer();
            }
        }));
        self.gpu_thread.lock().unwrap().tick_gpu();
        self.wait_for_sync_operation(wait_fence);
        Arc::try_unwrap(result).unwrap().into_inner().unwrap()
    }

    /// Renderer frame end notification.
    pub fn renderer_frame_end_notify(&self) {
        // NOTE: Full implementation calls system.GetPerfStats().EndGameFrame().
        // Stubbed until perf stats integration is complete.
    }
}
