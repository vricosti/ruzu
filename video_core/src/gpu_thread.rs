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
// Upstream uses Tegra::CommandList from dma_pusher.h.
// The Scheduler in ruzu currently takes control::channel_state::CommandList (Vec<Vec<u32>>).
// Use that type here for compatibility until the types are unified.
use crate::control::channel_state::CommandList;

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
}

impl ThreadManager {
    /// Creates a new thread manager.
    /// Matches upstream `ThreadManager::ThreadManager(Core::System&, bool)`.
    pub fn new(is_async: bool) -> Self {
        Self {
            is_async,
            state: Arc::new(SynchState::new()),
            stop: Arc::new(AtomicBool::new(false)),
            thread: None,
        }
    }

    /// Creates and starts the GPU thread.
    ///
    /// Matches upstream `ThreadManager::StartThread(renderer, context, scheduler)`.
    /// The thread runs `run_thread` which pops commands and dispatches them.
    ///
    /// # Safety
    /// `gpu_ptr` must remain valid for the lifetime of the thread (i.e., until Drop).
    pub unsafe fn start_thread(&mut self, gpu_ptr: *const crate::gpu::Gpu, scheduler_ptr: *const Scheduler) {
        let state = self.state.clone();
        let stop = self.stop.clone();
        let gpu = gpu_ptr as usize; // Send-safe via usize
        let sched = scheduler_ptr as usize;

        let handle = std::thread::Builder::new()
            .name("GPU".to_string())
            .spawn(move || {
                log::info!("GPU thread started");
                // Safety: gpu_ptr and scheduler_ptr are valid for the thread's lifetime.
                let gpu_ref = unsafe { &*(gpu as *const crate::gpu::Gpu) };
                let scheduler_ref = unsafe { &*(sched as *const Scheduler) };
                run_thread(&state, &stop, gpu_ref, scheduler_ref);
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
        // For now, skip flush in async mode (matches upstream behavior for non-extreme).
    }

    /// Notify rasterizer that a region should be invalidated.
    /// Matches upstream `ThreadManager::InvalidateRegion(DAddr, u64)`.
    pub fn invalidate_region(&self, _addr: DAddr, _size: u64) {
        // Upstream: rasterizer->OnCacheInvalidation(addr, size)
        // Requires rasterizer integration.
    }

    /// Notify rasterizer that a region should be flushed and invalidated.
    /// Matches upstream `ThreadManager::FlushAndInvalidateRegion(DAddr, u64)`.
    pub fn flush_and_invalidate_region(&self, _addr: DAddr, _size: u64) {
        // Upstream: rasterizer->OnCacheInvalidation(addr, size)
        // Skip flush in async mode.
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

        let _lock = self.state.write_lock.lock().unwrap();
        let fence = self.state.last_fence.fetch_add(1, Ordering::Relaxed) + 1;

        self.state.emplace(CommandDataContainer {
            data: command_data,
            fence,
            block,
        });

        if block {
            // Wait for the command to be processed.
            // Matches upstream: CondvarWait(state.cv, lk, stop_token, ...)
            let lock = self.state.write_lock.lock().unwrap();
            let _guard = self.state.cv.wait_while(lock, |_| {
                fence > self.state.signaled_fence.load(Ordering::Relaxed)
            }).unwrap();
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
                // Requires rasterizer integration.
                let _ = (flush.addr, flush.size);
            }
            CommandData::InvalidateRegion(inv) => {
                // Upstream: rasterizer->OnCacheInvalidation(inv.addr, inv.size)
                let _ = (inv.addr, inv.size);
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
