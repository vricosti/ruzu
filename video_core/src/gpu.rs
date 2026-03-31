// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/gpu.h and video_core/gpu.cpp
//!
//! GPU controller: manages channels, engines, rendering, and host synchronization.

use std::collections::{HashMap, VecDeque};
use std::sync::atomic::{AtomicBool, AtomicU64, AtomicUsize, Ordering};
use std::sync::{Arc, Condvar, Mutex};

use crate::dma_pusher::CommandList;
use crate::framebuffer_config::FramebufferConfig;
use crate::rasterizer_download_area::RasterizerDownloadArea;
use crate::renderer_base::RendererBase;
use common::settings;
use ruzu_core::core::SystemRef;
use ruzu_core::gpu_core::{
    GpuChannelHandle, GpuCommandList, GpuCoreInterface, GpuMemoryManagerHandle,
};

struct VideoGpuChannelHandle {
    gpu: *const Gpu,
    channel_state: Arc<parking_lot::Mutex<crate::control::channel_state::ChannelState>>,
}

struct VideoGpuMemoryManagerHandle {
    memory_manager: Arc<parking_lot::Mutex<crate::memory_manager::MemoryManager>>,
}

unsafe impl Send for VideoGpuChannelHandle {}
unsafe impl Sync for VideoGpuChannelHandle {}
unsafe impl Send for VideoGpuMemoryManagerHandle {}
unsafe impl Sync for VideoGpuMemoryManagerHandle {}

static NEXT_MEMORY_MANAGER_ID: AtomicUsize = AtomicUsize::new(1);

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
    ///
    /// Rust trait objects are fat pointers, so preserve both the data and
    /// vtable words instead of truncating to a thin pointer.
    rasterizer: Mutex<Option<[usize; 2]>>,

    /// GPU channel scheduler.
    /// Upstream: `std::unique_ptr<Tegra::Control::Scheduler> scheduler` in GPU::Impl.
    /// Initialized after Gpu is constructed (needs self-referential pointer).
    scheduler: Mutex<Option<Box<crate::control::scheduler::Scheduler>>>,

    /// GPU command thread manager.
    /// Upstream: `VideoCommon::GPUThread::ThreadManager gpu_thread` in GPU::Impl.
    gpu_thread: Mutex<crate::gpu_thread::ThreadManager>,

    /// Registered GPU channels.
    /// Upstream: `std::unordered_map<s32, std::shared_ptr<ChannelState>> channels`.
    channels:
        Mutex<HashMap<i32, Arc<parking_lot::Mutex<crate::control::channel_state::ChannelState>>>>,

    /// Owner-local bridge for GPU VA command fetches that still need core guest memory access.
    /// This is a temporary Rust adaptation until `video_core::MemoryManager` owns the same
    /// backing memory integration as upstream.
    guest_memory_reader: Mutex<Option<Arc<dyn Fn(u64, &mut [u8]) + Send + Sync>>>,
    guest_memory_writer: Mutex<Option<Arc<dyn Fn(u64, &[u8]) + Send + Sync>>>,
    /// Upstream owner: `Core::System& system`.
    system: Mutex<SystemRef>,
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
            rasterizer: Mutex::new(None),
            scheduler: Mutex::new(None),
            gpu_thread: Mutex::new(crate::gpu_thread::ThreadManager::new(is_async)),
            channels: Mutex::new(HashMap::new()),
            guest_memory_reader: Mutex::new(None),
            guest_memory_writer: Mutex::new(None),
            system: Mutex::new(SystemRef::null()),
        }
    }

    pub fn set_system_ref(&self, system: SystemRef) {
        *self.system.lock().unwrap() = system;
    }

    /// Initialize the scheduler. Must be called after Gpu is placed at its
    /// final address (the scheduler stores a raw pointer back to the Gpu).
    ///
    /// Upstream creates the scheduler in the GPU::Impl constructor:
    ///   `scheduler{std::make_unique<Control::Scheduler>(gpu)}`
    /// We defer it because Rust cannot take `&self` in the constructor.
    pub fn init_scheduler(&self) {
        let scheduler = unsafe { crate::control::scheduler::Scheduler::new(self as *const Gpu) };
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
        *self.rasterizer.lock().unwrap() = Some(unsafe { std::mem::transmute(rasterizer_ptr) });
        // Upstream also does:
        // host1x.MemoryManager().BindInterface(rasterizer);
        // host1x.GMMU().BindRasterizer(rasterizer);
        log::info!(
            "Gpu::bind_renderer: renderer bound (vendor: {})",
            renderer.get_device_vendor()
        );
        *self.renderer.lock().unwrap() = Some(renderer);
    }

    /// Bind a GPU channel by its bind ID.
    ///
    /// Matches upstream `GPU::Impl::BindChannel(s32 channel_id)`.
    /// Sets the active channel for command processing.
    pub fn bind_channel(&self, channel_id: i32) {
        {
            let mut bound_channel = self.bound_channel.lock().unwrap();
            if *bound_channel == channel_id {
                return;
            }
            *bound_channel = channel_id;
        }

        let channel_state = {
            let channels = self.channels.lock().unwrap();
            channels.get(&channel_id).cloned()
        };
        let Some(channel_state) = channel_state else {
            panic!("Gpu::bind_channel missing channel {}", channel_id);
        };

        if let Some(rasterizer) = self.rasterizer_ptr() {
            let rasterizer = unsafe { &mut *rasterizer };
            rasterizer.bind_channel(channel_state.lock().bind_id);
        }
    }

    fn rasterizer_ptr(&self) -> Option<*mut dyn crate::rasterizer_interface::RasterizerInterface> {
        (*self.rasterizer.lock().unwrap()).map(|raw| unsafe { std::mem::transmute(raw) })
    }

    pub fn set_guest_memory_reader(&self, reader: Arc<dyn Fn(u64, &mut [u8]) + Send + Sync>) {
        *self.guest_memory_reader.lock().unwrap() = Some(reader);
    }

    pub fn read_guest_memory(&self, addr: u64, output: &mut [u8]) -> bool {
        let Some(reader) = self.guest_memory_reader.lock().unwrap().clone() else {
            return false;
        };
        reader(addr, output);
        true
    }

    /// Set the guest memory writer callback.
    pub fn set_guest_memory_writer(&self, writer: Arc<dyn Fn(u64, &[u8]) + Send + Sync>) {
        *self.guest_memory_writer.lock().unwrap() = Some(writer);
    }

    /// Write guest memory at the given CPU/device address.
    pub fn write_guest_memory(&self, addr: u64, data: &[u8]) {
        let Some(writer) = self.guest_memory_writer.lock().unwrap().clone() else {
            return;
        };
        writer(addr, data);
    }

    /// Create a new GPU channel and register it with the scheduler.
    ///
    /// Matches upstream `GPU::Impl::CreateChannel(s32 channel_id)`.
    pub fn create_channel(
        &self,
        channel_id: i32,
    ) -> Arc<parking_lot::Mutex<crate::control::channel_state::ChannelState>> {
        let channel_state = Arc::new(parking_lot::Mutex::new(
            crate::control::channel_state::ChannelState::new(channel_id),
        ));
        self.channels
            .lock()
            .unwrap()
            .insert(channel_id, channel_state.clone());

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
        if let Some(rasterizer) = self.rasterizer_ptr() {
            unsafe { &mut *rasterizer }.flush_commands();
        }
    }

    /// Synchronizes CPU writes with Host GPU memory.
    pub fn invalidate_gpu_cache(&self) {
        let Some(rasterizer) = self.rasterizer_ptr() else {
            return;
        };
        let system = *self.system.lock().unwrap();
        if system.is_null() {
            log::warn!("Gpu::invalidate_gpu_cache: system ref not set");
            return;
        }
        let rasterizer = unsafe { &mut *rasterizer };
        system.get().gather_gpu_dirty_memory(&mut |addr, size| {
            rasterizer.on_cache_invalidation(addr, size as u64);
        });
    }

    /// Signal the ending of command list.
    pub fn on_command_list_end(&self) {
        if let Some(rasterizer) = self.rasterizer_ptr() {
            unsafe { &mut *rasterizer }.release_fences(false);
        }
        // Upstream also does Settings::UpdateGPUAccuracy().
    }

    /// Request a host GPU memory flush from the CPU.
    pub fn request_flush(&self, _addr: DAddr, _size: usize) -> u64 {
        // NOTE: Full implementation calls RequestSyncOperation which enqueues a flush.
        // Returns the next fence counter value.
        // Stubbed until rasterizer integration is complete.
        let fence = self.current_sync_fence.load(Ordering::Relaxed) + 1;
        log::warn!(
            "Gpu::request_flush: rasterizer not integrated, returning fence {}",
            fence
        );
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
        let system = *self.system.lock().unwrap();
        if system.is_null() {
            return 0;
        }

        let mut gpu_tick = system.get().core_timing().lock().unwrap().get_gpu_ticks();
        if *settings::values().use_fast_gpu_time.get_value() {
            gpu_tick /= 256;
        }

        gpu_tick
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
        self.gpu_thread
            .lock()
            .unwrap()
            .submit_list(channel, entries);
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
        self.gpu_thread
            .lock()
            .unwrap()
            .invalidate_region(addr, size);
    }

    /// Notify rasterizer of a CPU write.
    /// Matches upstream `GPU::Impl::OnCPUWrite(DAddr, u64)`.
    pub fn on_cpu_write(&self, _addr: DAddr, _size: u64) -> bool {
        let Some(rasterizer) = self.rasterizer_ptr() else {
            return false;
        };
        unsafe { &mut *rasterizer }.on_cpu_write(_addr, _size)
    }

    /// Flush and invalidate a region.
    /// Matches upstream `GPU::Impl::FlushAndInvalidateRegion(DAddr, u64)`.
    pub fn flush_and_invalidate_region(&self, addr: DAddr, size: u64) {
        self.gpu_thread
            .lock()
            .unwrap()
            .flush_and_invalidate_region(addr, size);
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

impl GpuChannelHandle for VideoGpuChannelHandle {
    fn bind_memory_manager(&self, memory_manager: Arc<dyn GpuMemoryManagerHandle>) {
        let memory_manager = memory_manager
            .as_any()
            .downcast_ref::<VideoGpuMemoryManagerHandle>()
            .expect("GPU memory manager handle must come from video_core::gpu::Gpu");
        let mut channel_state = self.channel_state.lock();
        channel_state.memory_manager = Some(Arc::clone(&memory_manager.memory_manager));
    }

    fn init_channel(&self, program_id: u64) {
        let mut channel_state = self.channel_state.lock();
        if channel_state.initialized {
            return;
        }

        let gpu = unsafe { &*self.gpu };
        channel_state.init(gpu, program_id);
        log::info!(
            "VideoGpuChannelHandle::init_channel: program_id={:#x} maxwell_3d={}",
            program_id,
            channel_state.maxwell_3d.is_some()
        );
        if let Some(rasterizer) = gpu.rasterizer_ptr() {
            let rasterizer = unsafe { &mut *rasterizer };
            channel_state.bind_rasterizer(rasterizer);
            rasterizer.initialize_channel(channel_state.bind_id);
        }
    }

    fn bind_id(&self) -> i32 {
        self.channel_state.lock().bind_id
    }
}

impl GpuMemoryManagerHandle for VideoGpuMemoryManagerHandle {
    fn as_any(&self) -> &(dyn std::any::Any + Send + Sync) {
        self
    }

    fn map(&self, gpu_addr: u64, device_addr: u64, size: u64, kind: u32, is_big_pages: bool) {
        self.memory_manager
            .lock()
            .map(gpu_addr, device_addr, size, kind, is_big_pages);
    }

    fn map_sparse(&self, gpu_addr: u64, size: u64, is_big_pages: bool) {
        self.memory_manager
            .lock()
            .map_sparse(gpu_addr, size, is_big_pages);
    }

    fn unmap(&self, gpu_addr: u64, size: u64) {
        self.memory_manager.lock().unmap(gpu_addr, size);
    }
}

impl GpuCoreInterface for Gpu {
    fn as_any(&self) -> &(dyn std::any::Any + Send) {
        self
    }

    fn allocate_channel_handle(&self) -> Arc<dyn GpuChannelHandle> {
        let channel_id = self.allocate_channel();
        let channel_state = self.create_channel(channel_id);
        Arc::new(VideoGpuChannelHandle {
            gpu: self as *const Gpu,
            channel_state,
        })
    }

    fn allocate_memory_manager_handle(
        &self,
        address_space_bits: u64,
        split_address: u64,
        big_page_bits: u64,
        page_bits: u64,
    ) -> Arc<dyn GpuMemoryManagerHandle> {
        let id = NEXT_MEMORY_MANAGER_ID.fetch_add(1, Ordering::AcqRel);
        Arc::new(VideoGpuMemoryManagerHandle {
            memory_manager: Arc::new(parking_lot::Mutex::new(
                crate::memory_manager::MemoryManager::new_with_geometry(
                    id,
                    address_space_bits,
                    split_address,
                    big_page_bits,
                    page_bits,
                ),
            )),
        })
    }

    fn init_address_space(&self, memory_manager: Arc<dyn GpuMemoryManagerHandle>) {
        let handle = memory_manager
            .as_any()
            .downcast_ref::<VideoGpuMemoryManagerHandle>()
            .expect("GPU memory manager handle must originate from video_core::Gpu");
        if self.rasterizer_ptr().is_none() {
            return;
        }
        let Some(rasterizer) = self.rasterizer_ptr() else {
            return;
        };
        let rasterizer = unsafe { &mut *rasterizer };
        handle.memory_manager.lock().bind_rasterizer(rasterizer);
    }

    fn push_gpu_entries(&self, channel_id: i32, entries: GpuCommandList) {
        let command_list = CommandList {
            command_lists: entries
                .command_lists
                .into_iter()
                .map(|header| crate::dma_pusher::CommandListHeader { raw: header.raw })
                .collect(),
            prefetch_command_list: entries
                .prefetch_command_list
                .into_iter()
                .map(|header| crate::dma_pusher::CommandHeader { raw: header.raw })
                .collect(),
        };
        Gpu::push_gpu_entries(self, channel_id, command_list);
    }

    fn on_cpu_write(&self, addr: u64, size: u64) -> bool {
        Gpu::on_cpu_write(self, addr, size)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::memory_manager::MemoryManager;
    use crate::rasterizer_interface::{RasterizerDownloadArea, RasterizerInterface};
    use std::sync::{Arc, Mutex as StdMutex, OnceLock};

    struct FakeRasterizer {
        initialized_channels: Arc<StdMutex<Vec<i32>>>,
        bound_channels: Arc<StdMutex<Vec<i32>>>,
    }

    impl RasterizerInterface for FakeRasterizer {
        fn draw(&mut self, _is_indexed: bool, _instance_count: u32) {}
        fn draw_texture(&mut self) {}
        fn clear(&mut self, _layer_count: u32) {}
        fn dispatch_compute(&mut self) {}
        fn reset_counter(&mut self, _query_type: u32) {}
        fn query(
            &mut self,
            _gpu_addr: u64,
            _query_type: u32,
            _flags: crate::query_cache::types::QueryPropertiesFlags,
            _gpu_ticks: u64,
            _payload: u32,
            _subreport: u32,
            _gpu_write: Arc<dyn Fn(u64, &[u8]) + Send + Sync>,
        ) {
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
        fn signal_fence(&mut self, _func: Box<dyn FnOnce() + Send>) {}
        fn sync_operation(&mut self, _func: Box<dyn FnOnce() + Send>) {}
        fn signal_sync_point(&mut self, _value: u32) {}
        fn signal_reference(&mut self) {}
        fn release_fences(&mut self, _force: bool) {}
        fn flush_all(&mut self) {}
        fn flush_region(&mut self, _addr: u64, _size: u64) {}
        fn must_flush_region(&self, _addr: u64, _size: u64) -> bool {
            false
        }
        fn get_flush_area(&self, addr: u64, _size: u64) -> RasterizerDownloadArea {
            RasterizerDownloadArea {
                start_address: addr,
                end_address: addr,
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
        fn wait_for_idle(&mut self) {}
        fn fragment_barrier(&mut self) {}
        fn tiled_cache_barrier(&mut self) {}
        fn flush_commands(&mut self) {}
        fn tick_frame(&mut self) {}
        fn accelerate_inline_to_memory(
            &mut self,
            _address: u64,
            _copy_size: usize,
            _memory: &[u8],
        ) {
        }
        fn initialize_channel(&mut self, channel_id: i32) {
            self.initialized_channels.lock().unwrap().push(channel_id);
        }
        fn bind_channel(&mut self, channel_id: i32) {
            self.bound_channels.lock().unwrap().push(channel_id);
        }
    }

    #[test]
    fn init_channel_binds_rasterizer_and_initializes_channel() {
        let gpu = Gpu::new(false, false);
        let channel_state = gpu.create_channel(7);
        channel_state.lock().memory_manager = Some(Arc::new(parking_lot::Mutex::new(
            MemoryManager::new_with_geometry(1, 32, 0x1_0000_0000, 16, 12),
        )));

        let initialized_channels = Arc::new(StdMutex::new(Vec::new()));
        let bound_channels = Arc::new(StdMutex::new(Vec::new()));
        let rasterizer = Box::new(FakeRasterizer {
            initialized_channels: initialized_channels.clone(),
            bound_channels: bound_channels.clone(),
        });
        let rasterizer_ptr: *mut FakeRasterizer = Box::into_raw(rasterizer);
        *gpu.rasterizer.lock().unwrap() =
            Some(unsafe { std::mem::transmute(rasterizer_ptr as *mut dyn RasterizerInterface) });

        let handle = VideoGpuChannelHandle {
            gpu: &gpu as *const Gpu,
            channel_state: channel_state.clone(),
        };
        handle.init_channel(0x1234);

        assert!(channel_state.lock().initialized);
        assert!(channel_state
            .lock()
            .memory_manager
            .as_ref()
            .unwrap()
            .lock()
            .has_bound_rasterizer());
        assert_eq!(*initialized_channels.lock().unwrap(), vec![7]);
        assert!(bound_channels.lock().unwrap().is_empty());

        unsafe {
            drop(Box::from_raw(rasterizer_ptr));
        }
    }

    #[test]
    fn bind_channel_notifies_rasterizer_once_per_new_channel() {
        let gpu = Gpu::new(false, false);
        gpu.create_channel(7);

        let bound_channels = Arc::new(StdMutex::new(Vec::new()));
        let rasterizer = Box::new(FakeRasterizer {
            initialized_channels: Arc::new(StdMutex::new(Vec::new())),
            bound_channels: bound_channels.clone(),
        });
        let rasterizer_ptr: *mut FakeRasterizer = Box::into_raw(rasterizer);
        *gpu.rasterizer.lock().unwrap() =
            Some(unsafe { std::mem::transmute(rasterizer_ptr as *mut dyn RasterizerInterface) });

        gpu.bind_channel(7);
        gpu.bind_channel(7);

        assert_eq!(*bound_channels.lock().unwrap(), vec![7]);

        unsafe {
            drop(Box::from_raw(rasterizer_ptr));
        }
    }

    #[test]
    fn get_ticks_uses_core_timing_and_fast_gpu_time_setting() {
        static SETTINGS_LOCK: OnceLock<StdMutex<()>> = OnceLock::new();
        let _guard = SETTINGS_LOCK
            .get_or_init(|| StdMutex::new(()))
            .lock()
            .unwrap();

        let system = ruzu_core::core::System::new();
        system.core_timing.lock().unwrap().add_ticks(512);
        let base_gpu_ticks = system.core_timing.lock().unwrap().get_gpu_ticks();

        let gpu = Gpu::new(false, false);
        gpu.set_system_ref(ruzu_core::core::SystemRef::from_ref(&system));

        let previous_fast_gpu_time = {
            let values = settings::values();
            *values.use_fast_gpu_time.get_value()
        };

        settings::values_mut().use_fast_gpu_time.set_value(false);
        assert_eq!(gpu.get_ticks(), base_gpu_ticks);

        settings::values_mut().use_fast_gpu_time.set_value(true);
        assert_eq!(gpu.get_ticks(), base_gpu_ticks / 256);

        settings::values_mut()
            .use_fast_gpu_time
            .set_value(previous_fast_gpu_time);
    }
}
