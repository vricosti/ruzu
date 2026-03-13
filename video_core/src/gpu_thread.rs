// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/gpu_thread.h and video_core/gpu_thread.cpp
//!
//! Threaded GPU command queue for asynchronous GPU processing.

use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Condvar, Mutex};
use std::thread::JoinHandle;

use crate::dma_pusher::CommandList;

/// Device address type.
pub type DAddr = u64;

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
pub enum CommandData {
    None,
    SubmitList(SubmitListCommand),
    FlushRegion(FlushRegionCommand),
    InvalidateRegion(InvalidateRegionCommand),
    FlushAndInvalidateRegion(FlushAndInvalidateRegionCommand),
    GpuTick(GpuTickCommand),
}

/// Container for a command with fence tracking.
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

/// Synchronization state for the GPU thread.
pub struct SynchState {
    pub write_lock: Mutex<()>,
    pub queue: Mutex<Vec<CommandDataContainer>>,
    pub last_fence: AtomicU64,
    pub signaled_fence: AtomicU64,
    pub cv: Condvar,
}

impl SynchState {
    pub fn new() -> Self {
        Self {
            write_lock: Mutex::new(()),
            queue: Mutex::new(Vec::new()),
            last_fence: AtomicU64::new(0),
            signaled_fence: AtomicU64::new(0),
            cv: Condvar::new(),
        }
    }
}

/// Manager for the GPU processing thread.
pub struct ThreadManager {
    is_async: bool,
    state: Arc<SynchState>,
    thread: Option<JoinHandle<()>>,
}

impl ThreadManager {
    /// Creates a new thread manager.
    pub fn new(is_async: bool) -> Self {
        Self {
            is_async,
            state: Arc::new(SynchState::new()),
            thread: None,
        }
    }

    /// Creates and starts the GPU thread.
    pub fn start_thread(&mut self) {
        // NOTE: Full implementation spawns a dedicated OS thread that calls
        // RunThread(renderer, context, scheduler). Stubbed until renderer and
        // scheduler integration is complete.
        log::warn!("ThreadManager::start_thread: renderer/scheduler not integrated, GPU thread not started");
    }

    /// Push GPU command entries to be processed.
    pub fn submit_list(&mut self, channel: i32, entries: CommandList) {
        self.push_command(CommandData::SubmitList(SubmitListCommand {
            channel,
            entries,
        }), false);
    }

    /// Notify rasterizer that a region should be flushed to Switch memory.
    pub fn flush_region(&mut self, addr: DAddr, size: u64) {
        if !self.is_async {
            self.push_command(
                CommandData::FlushRegion(FlushRegionCommand { addr, size }),
                false,
            );
            return;
        }
        // NOTE: In async mode with extreme GPU accuracy, the upstream pushes a
        // FlushRegion command with block=true and uses a GPU sync operation.
        // Stubbed until GPU sync operations are integrated.
        log::warn!("ThreadManager::flush_region: async GPU sync not integrated, dropping flush");
    }

    /// Notify rasterizer that a region should be invalidated.
    pub fn invalidate_region(&self, _addr: DAddr, _size: u64) {
        // NOTE: Full implementation calls rasterizer->OnCacheInvalidation(addr, size).
        // Stubbed until rasterizer integration is complete.
        log::warn!("ThreadManager::invalidate_region: rasterizer not integrated, skipping");
    }

    /// Notify rasterizer that a region should be flushed and invalidated.
    pub fn flush_and_invalidate_region(&self, _addr: DAddr, _size: u64) {
        // NOTE: Full implementation skips the flush in async mode and calls
        // rasterizer->OnCacheInvalidation(addr, size).
        // Stubbed until rasterizer integration is complete.
        log::warn!("ThreadManager::flush_and_invalidate_region: rasterizer not integrated, skipping");
    }

    /// Tick the GPU to process pending requests.
    pub fn tick_gpu(&mut self) {
        self.push_command(CommandData::GpuTick(GpuTickCommand), false);
    }

    /// Push a command to be executed by the GPU thread.
    fn push_command(&mut self, command_data: CommandData, mut block: bool) -> u64 {
        if !self.is_async {
            block = true;
        }

        let _lock = self.state.write_lock.lock().unwrap();
        let fence = self.state.last_fence.fetch_add(1, std::sync::atomic::Ordering::Relaxed) + 1;

        let mut queue = self.state.queue.lock().unwrap();
        queue.push(CommandDataContainer {
            data: command_data,
            fence,
            block,
        });
        drop(queue);

        if block {
            // Wait for the command to be processed
            let lock = self.state.write_lock.lock().unwrap();
            let _guard = self.state.cv.wait_while(lock, |_| {
                fence > self.state.signaled_fence.load(Ordering::Relaxed)
            }).unwrap();
        }

        fence
    }
}

impl Drop for ThreadManager {
    fn drop(&mut self) {
        if let Some(thread) = self.thread.take() {
            let _ = thread.join();
        }
    }
}
