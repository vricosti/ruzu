// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/gpu_thread.h and video_core/gpu_thread.cpp
//!
//! Threaded GPU command queue for asynchronous GPU processing.
//! Matches upstream structure: ThreadManager owns a dedicated OS thread
//! that pops commands from an MPSC queue and dispatches them.

use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{Arc, Condvar, Mutex};
use std::thread::JoinHandle;

use crate::control::scheduler::Scheduler;
use crate::dma_pusher::CommandList;

/// Device address type.
pub type DAddr = u64;

// ---------------------------------------------------------------------------
// Command types — matches upstream gpu_thread.h
// ---------------------------------------------------------------------------

/// Command to signal that a command list is ready for processing.
pub struct SubmitListCommand {
    pub channel: i32,
    pub entries: CommandList,
}

/// Command to flush a region.
pub struct FlushRegionCommand {
    pub addr: DAddr,
    pub size: u64,
}

/// Command to invalidate a region.
pub struct InvalidateRegionCommand {
    pub addr: DAddr,
    pub size: u64,
}

/// Command to flush and invalidate a region.
pub struct FlushAndInvalidateRegionCommand {
    pub addr: DAddr,
    pub size: u64,
}

/// Command to make the GPU process pending requests.
pub struct GpuTickCommand;

/// All possible GPU thread commands.
/// Matches upstream `CommandData` variant.
pub enum CommandData {
    None,
    SubmitList(SubmitListCommand),
    FlushRegion(FlushRegionCommand),
    InvalidateRegion(InvalidateRegionCommand),
    FlushAndInvalidateRegion(FlushAndInvalidateRegionCommand),
    GpuTick(GpuTickCommand),
}

/// Container for a command with fence tracking.
/// Matches upstream `CommandDataContainer`.
pub struct CommandDataContainer {
    pub data: CommandData,
    pub fence: u64,
    pub block: bool,
}

impl Default for CommandDataContainer {
    fn default() -> Self {
        Self {
            data: CommandData::None,
            fence: 0,
            block: false,
        }
    }
}

// ---------------------------------------------------------------------------
// SynchState — matches upstream gpu_thread.h SynchState
// ---------------------------------------------------------------------------

/// Synchronization state for the GPU thread.
///
/// Upstream uses `Common::MPSCQueue<CommandDataContainer>` (bounded SPSC + mutex).
/// We use `Mutex<VecDeque>` + Condvar for equivalent blocking semantics.
pub struct SynchState {
    pub write_lock: Mutex<()>,
    pub queue: Mutex<std::collections::VecDeque<CommandDataContainer>>,
    /// Condvar to wake the GPU thread when a command is enqueued.
    pub queue_cv: Condvar,
    pub last_fence: AtomicU64,
    pub signaled_fence: AtomicU64,
    /// Condvar to notify callers that a blocking command has completed.
    pub cv: Condvar,
}

impl SynchState {
    pub fn new() -> Self {
        Self {
            write_lock: Mutex::new(()),
            queue: Mutex::new(std::collections::VecDeque::new()),
            queue_cv: Condvar::new(),
            last_fence: AtomicU64::new(0),
            signaled_fence: AtomicU64::new(0),
            cv: Condvar::new(),
        }
    }

    /// Pop a command from the queue, blocking until one is available or stop is requested.
    /// Matches upstream `state.queue.PopWait(next, stop_token)`.
    pub fn pop_wait(&self, stop: &AtomicBool) -> Option<CommandDataContainer> {
        let mut queue = self.queue.lock().unwrap();
        loop {
            if stop.load(Ordering::Relaxed) {
                return None;
            }
            if let Some(cmd) = queue.pop_front() {
                return Some(cmd);
            }
            queue = self.queue_cv.wait(queue).unwrap();
        }
    }

    /// Push a command to the queue and wake the consumer.
    /// Matches upstream `state.queue.EmplaceWait(...)`.
    pub fn emplace(&self, cmd: CommandDataContainer) {
        let mut queue = self.queue.lock().unwrap();
        queue.push_back(cmd);
        self.queue_cv.notify_one();
    }
}

// ---------------------------------------------------------------------------
// ThreadManager — matches upstream gpu_thread.h/cpp ThreadManager
// ---------------------------------------------------------------------------

/// Manager for the GPU processing thread.
///
/// Matches upstream `VideoCommon::GPUThread::ThreadManager`.
pub struct ThreadManager {
    is_async: bool,
    state: Arc<SynchState>,
    stop: Arc<AtomicBool>,
    thread: Option<JoinHandle<()>>,
    /// Rasterizer fat pointer as [usize; 2] for direct calls
    /// (InvalidateRegion, FlushAndInvalidateRegion).
    /// Matches upstream `VideoCore::RasterizerInterface* rasterizer`.
    /// [0, 0] means null. Set during start_thread from renderer.ReadRasterizer().
    rasterizer_raw: [usize; 2],
}

// Safety: ThreadManager is accessed under Gpu's Mutex lock.
// The rasterizer pointer is valid for the lifetime of the renderer.
unsafe impl Send for ThreadManager {}

impl ThreadManager {
    /// Creates a new thread manager.
    /// Matches upstream `ThreadManager::ThreadManager(Core::System&, bool)`.
    pub fn new(is_async: bool) -> Self {
        Self {
            is_async,
            state: Arc::new(SynchState::new()),
            stop: Arc::new(AtomicBool::new(false)),
            thread: None,
            rasterizer_raw: [0, 0],
        }
    }

    /// Get the rasterizer pointer, or None if not set.
    fn rasterizer(&self) -> Option<*mut dyn crate::rasterizer_interface::RasterizerInterface> {
        if self.rasterizer_raw[0] == 0 {
            None
        } else {
            Some(unsafe { std::mem::transmute(self.rasterizer_raw) })
        }
    }

    /// Creates and starts the GPU thread.
    ///
    /// Matches upstream `ThreadManager::StartThread(renderer, context, scheduler)`.
    /// The thread runs `run_thread` which pops commands and dispatches them.
    ///
    /// # Safety
    /// `gpu_ptr`, `context_ptr`, `renderer_ptr`, and `scheduler_ptr` must remain
    /// valid for the lifetime of the thread.
    pub unsafe fn start_thread(
        &mut self,
        gpu_ptr: *const crate::gpu::Gpu,
        renderer_ptr: *mut dyn crate::renderer_base::RendererBase,
        context_ptr: *mut dyn ruzu_core::frontend::graphics_context::GraphicsContext,
        scheduler_ptr: *const Scheduler,
    ) {
        // Extract rasterizer from renderer, matching upstream:
        //   rasterizer = renderer.ReadRasterizer();
        let rasterizer_fat = unsafe { &*renderer_ptr }.read_rasterizer();
        self.rasterizer_raw = unsafe { std::mem::transmute(rasterizer_fat) };

        let state = self.state.clone();
        let stop = self.stop.clone();
        let gpu = gpu_ptr as usize;
        let sched = scheduler_ptr as usize;
        // Copy rasterizer fat pointer for the thread.
        let rasterizer_vtable = self.rasterizer_raw;
        let ctx_raw: [usize; 2] = if context_ptr.is_null() {
            [0, 0]
        } else {
            unsafe { std::mem::transmute(context_ptr) }
        };

        let handle = std::thread::Builder::new()
            .name("GPU".to_string())
            .spawn(move || {
                log::info!("GPU thread started");
                let gpu_ref = unsafe { &*(gpu as *const crate::gpu::Gpu) };
                let scheduler_ref = unsafe { &*(sched as *const Scheduler) };

                // Upstream: auto current_context = context.Acquire();
                let has_context = ctx_raw[0] != 0;
                if has_context {
                    let context: *mut dyn ruzu_core::frontend::graphics_context::GraphicsContext =
                        unsafe { std::mem::transmute(ctx_raw) };
                    unsafe { &mut *context }.make_current();
                }

                run_thread(&state, &stop, gpu_ref, scheduler_ref, rasterizer_vtable);

                // Release context on exit.
                if has_context {
                    let context: *mut dyn ruzu_core::frontend::graphics_context::GraphicsContext =
                        unsafe { std::mem::transmute(ctx_raw) };
                    unsafe { &mut *context }.done_current();
                }
                log::info!("GPU thread exiting");
            })
            .expect("Failed to spawn GPU thread");

        self.thread = Some(handle);
    }

    /// Push GPU command entries to be processed.
    /// Matches upstream `ThreadManager::SubmitList(s32, CommandList&&)`.
    pub fn submit_list(&self, channel: i32, entries: CommandList) {
        self.push_command(CommandData::SubmitList(SubmitListCommand {
            channel,
            entries,
        }), false);
    }

    /// Notify rasterizer that a region should be flushed to Switch memory.
    /// Matches upstream `ThreadManager::FlushRegion(DAddr, u64)`.
    pub fn flush_region(&self, addr: DAddr, size: u64) {
        if !self.is_async {
            self.push_command(
                CommandData::FlushRegion(FlushRegionCommand { addr, size }),
                false,
            );
            return;
        }
        // In async mode with extreme GPU accuracy, upstream does:
        //   gpu.RequestFlush(addr, size) → TickGPU() → WaitForSyncOperation()
        // In non-extreme async mode, upstream skips flush entirely.
    }

    /// Notify rasterizer that a region should be invalidated.
    /// Matches upstream `ThreadManager::InvalidateRegion(DAddr, u64)`.
    ///
    /// Upstream calls directly on the rasterizer (NOT queued to the GPU thread).
    pub fn invalidate_region(&self, addr: DAddr, size: u64) {
        if let Some(rasterizer) = self.rasterizer() {
            // Safety: rasterizer pointer is valid for the lifetime of the renderer.
            unsafe { &mut *rasterizer }.on_cache_invalidation(addr, size);
        }
    }

    /// Notify rasterizer that a region should be flushed and invalidated.
    /// Matches upstream `ThreadManager::FlushAndInvalidateRegion(DAddr, u64)`.
    ///
    /// Upstream calls directly on the rasterizer (NOT queued).
    /// Flush is skipped in async mode (only invalidation runs).
    pub fn flush_and_invalidate_region(&self, addr: DAddr, size: u64) {
        if let Some(rasterizer) = self.rasterizer() {
            // Safety: rasterizer pointer is valid for the lifetime of the renderer.
            unsafe { &mut *rasterizer }.on_cache_invalidation(addr, size);
        }
    }

    /// Tick the GPU to process pending requests.
    /// Matches upstream `ThreadManager::TickGPU()`.
    pub fn tick_gpu(&self) {
        self.push_command(CommandData::GpuTick(GpuTickCommand), false);
    }

    /// Push a command to be executed by the GPU thread.
    /// Matches upstream `ThreadManager::PushCommand(CommandData&&, bool)`.
    fn push_command(&self, command_data: CommandData, mut block: bool) -> u64 {
        if !self.is_async {
            block = true;
        }

        let fence = {
            let _lock = self.state.write_lock.lock().unwrap();
            let fence = self.state.last_fence.fetch_add(1, Ordering::Relaxed) + 1;

            self.state.emplace(CommandDataContainer {
                data: command_data,
                fence,
                block,
            });

            fence
            // _lock dropped here before blocking wait
        };

        if block {
            // Wait for the command to be processed.
            let signaled = self.state.signaled_fence.load(Ordering::Relaxed);
            if fence <= signaled {
                // Already processed (GPU thread was fast)
                return fence;
            }
            log::trace!("push_command: waiting for fence {} (signaled={})", fence, signaled);
            let lock = self.state.write_lock.lock().unwrap();
            let _guard = self.state.cv.wait_while(lock, |_| {
                fence > self.state.signaled_fence.load(Ordering::Relaxed)
            }).unwrap();
            log::trace!("push_command: fence {} done", fence);
        }

        fence
    }

    /// Request the GPU thread to stop.
    pub fn request_stop(&self) {
        self.stop.store(true, Ordering::Relaxed);
        // Wake the thread if it's waiting on the queue
        self.state.queue_cv.notify_all();
    }
}

impl Drop for ThreadManager {
    fn drop(&mut self) {
        self.request_stop();
        if let Some(thread) = self.thread.take() {
            let _ = thread.join();
        }
    }
}

// ---------------------------------------------------------------------------
// RunThread — matches upstream gpu_thread.cpp RunThread()
// ---------------------------------------------------------------------------

/// The GPU thread entry point.
///
/// Matches upstream `static void RunThread(stop_token, system, renderer, context, scheduler, state)`.
/// Pops commands from the queue and dispatches them.
fn run_thread(
    state: &SynchState,
    stop: &AtomicBool,
    gpu: &crate::gpu::Gpu,
    scheduler: &Scheduler,
    rasterizer_raw: [usize; 2],
) {
    while !stop.load(Ordering::Relaxed) {
        let Some(next) = state.pop_wait(stop) else {
            break; // Stop requested
        };

        match next.data {
            CommandData::SubmitList(submit) => {
                scheduler.push(submit.channel, submit.entries);
            }
            CommandData::GpuTick(_) => {
                gpu.tick_work();
            }
            CommandData::FlushRegion(flush) => {
                // Upstream: rasterizer->FlushRegion(flush.addr, flush.size)
                if rasterizer_raw[0] != 0 {
                    let rasterizer: *mut dyn crate::rasterizer_interface::RasterizerInterface =
                        unsafe { std::mem::transmute(rasterizer_raw) };
                    unsafe { &mut *rasterizer }.flush_region(flush.addr, flush.size);
                }
            }
            CommandData::InvalidateRegion(inv) => {
                // Upstream: rasterizer->OnCacheInvalidation(inv.addr, inv.size)
                if rasterizer_raw[0] != 0 {
                    let rasterizer: *mut dyn crate::rasterizer_interface::RasterizerInterface =
                        unsafe { std::mem::transmute(rasterizer_raw) };
                    unsafe { &mut *rasterizer }.on_cache_invalidation(inv.addr, inv.size);
                }
            }
            CommandData::FlushAndInvalidateRegion(_) => {
                // Upstream: ASSERT(false) — should not reach here
                unreachable!("FlushAndInvalidateRegion should not be queued");
            }
            CommandData::None => {}
        }

        // Signal fence completion.
        state.signaled_fence.store(next.fence, Ordering::Release);
        if next.block {
            let _lock = state.write_lock.lock().unwrap();
            state.cv.notify_all();
        }
    }
}
