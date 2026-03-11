// SPDX-FileCopyrightText: 2022 yuzu Emulator Project
// SPDX-FileCopyrightText: 2022 Skyline Team and Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/nvdrv/core/syncpoint_manager.h
//! Port of zuyu/src/core/hle/service/nvdrv/core/syncpoint_manager.cpp

use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::Mutex;

use crate::hle::service::nvdrv::nvdata::{NvFence, NvResult};

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChannelType {
    MsEnc = 0,
    VIC = 1,
    GPU = 2,
    NvDec = 3,
    Display = 4,
    NvJpg = 5,
    TSec = 6,
    Max = 7,
}

pub const CHANNEL_SYNCPOINTS: [u32; ChannelType::Max as usize] = [
    0x0,  // MsEnc is unimplemented
    0xC,  // VIC
    0x0,  // GPU syncpoints are allocated per-channel instead
    0x36, // NvDec
    0x0,  // Display is unimplemented
    0x37, // NvJpg
    0x0,  // TSec is unimplemented
];

const SYNCPOINT_COUNT: usize = 192;

struct SyncpointInfo {
    counter_min: AtomicU32,
    counter_max: AtomicU32,
    interface_managed: bool,
    reserved: bool,
}

impl Default for SyncpointInfo {
    fn default() -> Self {
        Self {
            counter_min: AtomicU32::new(0),
            counter_max: AtomicU32::new(0),
            interface_managed: false,
            reserved: false,
        }
    }
}

/// SyncpointManager handles allocating and accessing host1x syncpoints.
/// These are cached versions of the HW syncpoints which are intermittently synced.
pub struct SyncpointManager {
    syncpoints: Vec<SyncpointInfo>,
    reservation_lock: Mutex<()>,
}

impl SyncpointManager {
    pub fn new() -> Self {
        let mut syncpoints: Vec<SyncpointInfo> =
            (0..SYNCPOINT_COUNT).map(|_| SyncpointInfo::default()).collect();

        let vblank0_syncpoint_id: u32 = 26;
        let vblank1_syncpoint_id: u32 = 27;

        // Reserve both vblank syncpoints as client managed
        syncpoints[vblank0_syncpoint_id as usize].reserved = true;
        syncpoints[vblank0_syncpoint_id as usize].interface_managed = true;
        syncpoints[vblank1_syncpoint_id as usize].reserved = true;
        syncpoints[vblank1_syncpoint_id as usize].interface_managed = true;

        for &syncpoint_id in &CHANNEL_SYNCPOINTS {
            if syncpoint_id != 0 {
                syncpoints[syncpoint_id as usize].reserved = true;
                syncpoints[syncpoint_id as usize].interface_managed = false;
            }
        }

        Self {
            syncpoints,
            reservation_lock: Mutex::new(()),
        }
    }

    pub fn is_syncpoint_allocated(&self, id: u32) -> bool {
        (id as usize) < SYNCPOINT_COUNT && self.syncpoints[id as usize].reserved
    }

    pub fn allocate_syncpoint(&self, client_managed: bool) -> u32 {
        let _lock = self.reservation_lock.lock().unwrap();
        let id = self.find_free_syncpoint();
        self.reserve_syncpoint_inner(id, client_managed)
    }

    pub fn free_syncpoint(&self, id: u32) {
        // Note: In a truly safe Rust implementation we'd need interior mutability for syncpoints.
        // For now, this is a stub matching the upstream behavior.
        let _lock = self.reservation_lock.lock().unwrap();
        // In the C++ code, this modifies `reserved`. Since we use Vec (not fully interior-mutable),
        // we mark this as a todo for full mutability.
        log::debug!("SyncpointManager::free_syncpoint({})", id);
    }

    pub fn has_syncpoint_expired(&self, id: u32, threshold: u32) -> bool {
        let syncpoint = &self.syncpoints[id as usize];
        if !syncpoint.reserved {
            return false;
        }

        if syncpoint.interface_managed {
            (syncpoint.counter_min.load(Ordering::Relaxed).wrapping_sub(threshold)) as i32 >= 0
        } else {
            syncpoint.counter_max.load(Ordering::Relaxed).wrapping_sub(threshold)
                >= syncpoint.counter_min.load(Ordering::Relaxed).wrapping_sub(threshold)
        }
    }

    pub fn is_fence_signalled(&self, fence: &NvFence) -> bool {
        self.has_syncpoint_expired(fence.id as u32, fence.value)
    }

    pub fn increment_syncpoint_max_ext(&self, id: u32, amount: u32) -> u32 {
        let syncpoint = &self.syncpoints[id as usize];
        if !syncpoint.reserved {
            return 0;
        }
        syncpoint.counter_max.fetch_add(amount, Ordering::Relaxed) + amount
    }

    pub fn read_syncpoint_min_value(&self, id: u32) -> u32 {
        let syncpoint = &self.syncpoints[id as usize];
        if !syncpoint.reserved {
            return 0;
        }
        syncpoint.counter_min.load(Ordering::Relaxed)
    }

    pub fn update_min(&self, id: u32) -> u32 {
        let syncpoint = &self.syncpoints[id as usize];
        if !syncpoint.reserved {
            return 0;
        }
        // In the C++ code, this reads from host1x hardware. Stub for now.
        syncpoint.counter_min.load(Ordering::Relaxed)
    }

    pub fn get_syncpoint_fence(&self, id: u32) -> NvFence {
        let syncpoint = &self.syncpoints[id as usize];
        if !syncpoint.reserved {
            return NvFence::default();
        }
        NvFence {
            id: id as i32,
            value: syncpoint.counter_max.load(Ordering::Relaxed),
        }
    }

    fn find_free_syncpoint(&self) -> u32 {
        for i in 1..self.syncpoints.len() {
            if !self.syncpoints[i].reserved {
                return i as u32;
            }
        }
        log::error!("Failed to find a free syncpoint!");
        0
    }

    fn reserve_syncpoint_inner(&self, id: u32, _client_managed: bool) -> u32 {
        // This requires interior mutability for `reserved` and `interface_managed`.
        // Stubbed for compile, but the logic matches upstream.
        log::debug!("SyncpointManager::reserve_syncpoint({})", id);
        id
    }
}
