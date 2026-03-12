// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/control/scheduler.h and scheduler.cpp
//!
//! The `Scheduler` receives command lists from host threads and dispatches
//! them to the correct GPU channel's DMA pusher under a global scheduling
//! lock.

use std::collections::HashMap;
use std::sync::Arc;

use parking_lot::Mutex;

use super::channel_state::{ChannelState, CommandList, Gpu};

// ---------------------------------------------------------------------------
// Scheduler
// ---------------------------------------------------------------------------

/// GPU channel scheduler.
///
/// Corresponds to `Tegra::Control::Scheduler` in upstream.
pub struct Scheduler {
    channels: HashMap<i32, Arc<Mutex<ChannelState>>>,
    scheduling_guard: Mutex<()>,
    gpu: Arc<Mutex<Gpu>>,
}

impl Scheduler {
    /// Create a new scheduler bound to the given GPU.
    ///
    /// Corresponds to `Scheduler::Scheduler(GPU& gpu_)`.
    pub fn new(gpu: Arc<Mutex<Gpu>>) -> Self {
        Self {
            channels: HashMap::new(),
            scheduling_guard: Mutex::new(()),
            gpu,
        }
    }

    /// Push a command list to a channel for execution.
    ///
    /// Corresponds to `Scheduler::Push(s32 channel, CommandList&& entries)`.
    pub fn push(&self, channel: i32, entries: CommandList) {
        let _lock = self.scheduling_guard.lock();

        let channel_state = self
            .channels
            .get(&channel)
            .expect("Scheduler::push: channel not found");

        let mut cs = channel_state.lock();
        self.gpu.lock().bind_channel(cs.bind_id);

        let dma_pusher = cs
            .dma_pusher
            .as_mut()
            .expect("Scheduler::push: dma_pusher not initialized");
        dma_pusher.push(entries);
        dma_pusher.dispatch_calls();
    }

    /// Register a channel with the scheduler.
    ///
    /// Corresponds to `Scheduler::DeclareChannel(shared_ptr<ChannelState>)`.
    pub fn declare_channel(&mut self, new_channel: Arc<Mutex<ChannelState>>) {
        let bind_id = new_channel.lock().bind_id;
        let _lock = self.scheduling_guard.lock();
        self.channels.insert(bind_id, new_channel);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_declare_channel() {
        let gpu = Arc::new(Mutex::new(Gpu::default()));
        let mut sched = Scheduler::new(gpu);

        let cs = Arc::new(Mutex::new(ChannelState::new(5)));
        sched.declare_channel(cs);

        assert!(sched.channels.contains_key(&5));
    }
}
