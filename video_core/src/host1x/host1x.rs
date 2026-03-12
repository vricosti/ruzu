// SPDX-FileCopyrightText: Copyright 2024 ruzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `video_core/host1x/host1x.h` and `host1x.cpp`.
//!
//! Main Host1x class: owns the syncpoint manager, device memory manager,
//! GMMU, allocator, frame queue, and active CDMA devices.

use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use log::error;

use crate::host1x::ffmpeg::ffmpeg::Frame;
use crate::host1x::syncpoint_manager::SyncpointManager;

// --------------------------------------------------------------------------
// FrameQueue
// --------------------------------------------------------------------------

/// Thread-safe queue for decoded video frames, indexed by NVDEC file descriptor
/// and memory offset. Supports both presentation-order and decode-order queuing.
///
/// Port of `Tegra::Host1x::FrameQueue`.
pub struct FrameQueue {
    inner: Mutex<FrameQueueInner>,
}

struct FrameQueueInner {
    /// Presentation-order frames: fd -> deque of (offset, frame).
    presentation_order: HashMap<i32, Vec<(u64, Arc<Frame>)>>,
    /// Decode-order frames: fd -> map of offset -> frame.
    decode_order: HashMap<i32, HashMap<u64, Arc<Frame>>>,
}

impl FrameQueue {
    pub fn new() -> Self {
        Self {
            inner: Mutex::new(FrameQueueInner {
                presentation_order: HashMap::new(),
                decode_order: HashMap::new(),
            }),
        }
    }

    /// Register a new NVDEC file descriptor.
    pub fn open(&self, fd: i32) {
        let mut inner = self.inner.lock().unwrap();
        inner.presentation_order.entry(fd).or_default();
        inner.decode_order.entry(fd).or_default();
    }

    /// Unregister an NVDEC file descriptor.
    pub fn close(&self, fd: i32) {
        let mut inner = self.inner.lock().unwrap();
        inner.presentation_order.remove(&fd);
        inner.decode_order.remove(&fd);
    }

    /// Search all FDs for a frame matching the given offset.
    /// Returns the FD that owns it, or -1 if not found.
    ///
    /// Port of `FrameQueue::VicFindNvdecFdFromOffset`.
    pub fn vic_find_nvdec_fd_from_offset(&self, search_offset: u64) -> i32 {
        let inner = self.inner.lock().unwrap();

        for (fd, frames) in &inner.presentation_order {
            for (offset, _) in frames {
                if *offset == search_offset {
                    return *fd;
                }
            }
        }

        for (fd, frames) in &inner.decode_order {
            for (offset, _) in frames {
                if *offset == search_offset {
                    return *fd;
                }
            }
        }

        -1
    }

    /// Push a frame in presentation order.
    pub fn push_present_order(&self, fd: i32, offset: u64, frame: Arc<Frame>) {
        let mut inner = self.inner.lock().unwrap();
        if let Some(queue) = inner.presentation_order.get_mut(&fd) {
            queue.push((offset, frame));
        }
    }

    /// Push a frame in decode order (keyed by offset, replaces existing).
    pub fn push_decode_order(&self, fd: i32, offset: u64, frame: Arc<Frame>) {
        let mut inner = self.inner.lock().unwrap();
        if let Some(map) = inner.decode_order.get_mut(&fd) {
            map.insert(offset, frame);
        }
    }

    /// Retrieve a frame for the given FD and offset.
    /// Prefers presentation order; falls back to decode order.
    pub fn get_frame(&self, fd: i32, offset: u64) -> Option<Arc<Frame>> {
        if fd == -1 {
            return None;
        }

        let mut inner = self.inner.lock().unwrap();

        // Try presentation order first.
        if let Some(queue) = inner.presentation_order.get_mut(&fd) {
            if !queue.is_empty() {
                // Pop the front element (presentation order).
                let (_offset, frame) = queue.remove(0);
                return Some(frame);
            }
        }

        // Fall back to decode order.
        if let Some(map) = inner.decode_order.get_mut(&fd) {
            if let Some(frame) = map.remove(&offset) {
                return Some(frame);
            }
        }

        None
    }
}

impl Default for FrameQueue {
    fn default() -> Self {
        Self::new()
    }
}

// --------------------------------------------------------------------------
// ChannelType
// --------------------------------------------------------------------------

/// Host1x channel types.
///
/// Port of `Tegra::Host1x::ChannelType`.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ChannelType {
    MsEnc = 0,
    Vic = 1,
    Gpu = 2,
    NvDec = 3,
    Display = 4,
    NvJpg = 5,
    TSec = 6,
    Max = 7,
}

impl ChannelType {
    pub fn from_u32(value: u32) -> Option<Self> {
        match value {
            0 => Some(Self::MsEnc),
            1 => Some(Self::Vic),
            2 => Some(Self::Gpu),
            3 => Some(Self::NvDec),
            4 => Some(Self::Display),
            5 => Some(Self::NvJpg),
            6 => Some(Self::TSec),
            7 => Some(Self::Max),
            _ => None,
        }
    }
}

// --------------------------------------------------------------------------
// Host1x
// --------------------------------------------------------------------------

/// Main Host1x subsystem.
///
/// Port of `Tegra::Host1x::Host1x`.
pub struct Host1x {
    syncpoint_manager: Arc<SyncpointManager>,
    frame_queue: Arc<FrameQueue>,
    // TODO: memory_manager: MaxwellDeviceMemoryManager,
    // TODO: gmmu_manager: MemoryManager,
    // TODO: allocator: FlatAllocator<u32>,
    // TODO: devices: HashMap<i32, Box<dyn CdmaPusher>>,
}

impl Host1x {
    pub fn new() -> Self {
        Self {
            syncpoint_manager: Arc::new(SyncpointManager::new()),
            frame_queue: Arc::new(FrameQueue::new()),
        }
    }

    pub fn syncpoint_manager(&self) -> &Arc<SyncpointManager> {
        &self.syncpoint_manager
    }

    pub fn frame_queue(&self) -> &Arc<FrameQueue> {
        &self.frame_queue
    }

    /// Start a device (NvDec, VIC, etc.) on the given file descriptor.
    ///
    /// Port of `Host1x::StartDevice`.
    pub fn start_device(&mut self, fd: i32, channel_type: ChannelType, _syncpt: u32) {
        match channel_type {
            ChannelType::NvDec => {
                // TODO: Create Nvdec CDmaPusher and insert into devices map.
                self.frame_queue.open(fd);
                log::info!("Started NvDec device fd={}", fd);
            }
            ChannelType::Vic => {
                // TODO: Create Vic CDmaPusher and insert into devices map.
                log::info!("Started VIC device fd={}", fd);
            }
            _ => {
                error!(
                    "Unimplemented host1x device {:?} ({})",
                    channel_type, channel_type as u32
                );
            }
        }
    }

    /// Stop a device on the given file descriptor.
    ///
    /// Port of `Host1x::StopDevice`.
    pub fn stop_device(&mut self, fd: i32, _channel_type: ChannelType) {
        // TODO: Remove from devices map.
        let _ = fd;
    }

    /// Push command entries to a device.
    ///
    /// Port of `Host1x::PushEntries`.
    pub fn push_entries(&mut self, _fd: i32, _entries: Vec<u32>) {
        // TODO: Look up device by fd and call push_entries on it.
        todo!("Host1x::push_entries — requires CDmaPusher integration")
    }
}

impl Default for Host1x {
    fn default() -> Self {
        Self::new()
    }
}
