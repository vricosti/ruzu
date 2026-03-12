// SPDX-FileCopyrightText: 2022 yuzu Emulator Project
// SPDX-FileCopyrightText: 2022 Skyline Team and Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/nvdrv/core/syncpoint_manager.h
//! Port of zuyu/src/core/hle/service/nvdrv/core/syncpoint_manager.cpp

use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::Mutex;

use crate::hle::service::nvdrv::nvdata::NvFence;

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

/// Maps each channel ID to a constant syncpoint.
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
///
/// Refer to Chapter 14 of the Tegra X1 TRM for an exhaustive overview of them.
/// https://http.download.nvidia.com/tegra-public-appnotes/host1x.html
pub struct SyncpointManager {
    /// Uses Mutex for interior mutability since `reserved` and `interface_managed` need mutation.
    /// The AtomicU32 fields (counter_min, counter_max) can be accessed without the lock.
    syncpoints: Mutex<Vec<SyncpointInfo>>,
}

impl SyncpointManager {
    pub fn new() -> Self {
        let mut syncpoints: Vec<SyncpointInfo> =
            (0..SYNCPOINT_COUNT).map(|_| SyncpointInfo::default()).collect();

        const VBLANK0_SYNCPOINT_ID: usize = 26;
        const VBLANK1_SYNCPOINT_ID: usize = 27;

        // Reserve both vblank syncpoints as client managed as they use Continuous Mode.
        // Refer to section 14.3.5.3 of the TRM for more information on Continuous Mode.
        syncpoints[VBLANK0_SYNCPOINT_ID].reserved = true;
        syncpoints[VBLANK0_SYNCPOINT_ID].interface_managed = true;
        syncpoints[VBLANK1_SYNCPOINT_ID].reserved = true;
        syncpoints[VBLANK1_SYNCPOINT_ID].interface_managed = true;

        for &syncpoint_id in &CHANNEL_SYNCPOINTS {
            if syncpoint_id != 0 {
                syncpoints[syncpoint_id as usize].reserved = true;
                syncpoints[syncpoint_id as usize].interface_managed = false;
            }
        }

        Self {
            syncpoints: Mutex::new(syncpoints),
        }
    }

    /// Checks if the given syncpoint is both allocated and below the number of HW syncpoints.
    pub fn is_syncpoint_allocated(&self, id: u32) -> bool {
        let syncpoints = self.syncpoints.lock().unwrap();
        (id as usize) < SYNCPOINT_COUNT && syncpoints[id as usize].reserved
    }

    /// Finds a free syncpoint and reserves it.
    /// Returns the ID of the reserved syncpoint.
    pub fn allocate_syncpoint(&self, client_managed: bool) -> u32 {
        let mut syncpoints = self.syncpoints.lock().unwrap();
        let id = Self::find_free_syncpoint_inner(&syncpoints);
        Self::reserve_syncpoint_inner(&mut syncpoints, id, client_managed)
    }

    /// Frees the usage of a syncpoint.
    pub fn free_syncpoint(&self, id: u32) {
        let mut syncpoints = self.syncpoints.lock().unwrap();
        let syncpoint = &mut syncpoints[id as usize];
        assert!(syncpoint.reserved, "Syncpoint {} is not reserved", id);
        syncpoint.reserved = false;
    }

    /// Checks if a syncpoint has expired relative to a threshold.
    ///
    /// https://github.com/Jetson-TX1-AndroidTV/android_kernel_jetson_tx1_hdmi_primary/blob/8f74a72394efb871cb3f886a3de2998cd7ff2990/drivers/gpu/host1x/syncpt.c#L259
    pub fn has_syncpoint_expired(&self, id: u32, threshold: u32) -> bool {
        let syncpoints = self.syncpoints.lock().unwrap();
        let syncpoint = &syncpoints[id as usize];

        if !syncpoint.reserved {
            debug_assert!(false, "Syncpoint {} is not reserved", id);
            return false;
        }

        // If the interface manages counters then we don't keep track of the maximum value as it
        // handles sanity checking the values then
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

    /// Atomically increments the maximum value of a syncpoint by the given amount.
    /// Returns the new max value of the syncpoint.
    pub fn increment_syncpoint_max_ext(&self, id: u32, amount: u32) -> u32 {
        let syncpoints = self.syncpoints.lock().unwrap();
        let syncpoint = &syncpoints[id as usize];

        if !syncpoint.reserved {
            debug_assert!(false, "Syncpoint {} is not reserved", id);
            return 0;
        }

        syncpoint.counter_max.fetch_add(amount, Ordering::Relaxed) + amount
    }

    /// Returns the minimum value of the syncpoint.
    pub fn read_syncpoint_min_value(&self, id: u32) -> u32 {
        let syncpoints = self.syncpoints.lock().unwrap();
        let syncpoint = &syncpoints[id as usize];

        if !syncpoint.reserved {
            debug_assert!(false, "Syncpoint {} is not reserved", id);
            return 0;
        }

        syncpoint.counter_min.load(Ordering::Relaxed)
    }

    /// Synchronises the minimum value of the syncpoint with the GPU.
    /// Returns the new minimum value of the syncpoint.
    ///
    /// Note: In the C++ code, this reads from host1x hardware via
    /// `host1x.GetSyncpointManager().GetHostSyncpointValue(id)`.
    /// Since we don't have host1x integration yet, we return the current counter_min.
    pub fn update_min(&self, id: u32) -> u32 {
        let syncpoints = self.syncpoints.lock().unwrap();
        let syncpoint = &syncpoints[id as usize];

        if !syncpoint.reserved {
            debug_assert!(false, "Syncpoint {} is not reserved", id);
            return 0;
        }

        // TODO: In the C++ code:
        // syncpoint.counter_min = host1x.GetSyncpointManager().GetHostSyncpointValue(id);
        // For now, just return the current value.
        syncpoint.counter_min.load(Ordering::Relaxed)
    }

    /// Returns a fence that will be signalled once this syncpoint hits its maximum value.
    pub fn get_syncpoint_fence(&self, id: u32) -> NvFence {
        let syncpoints = self.syncpoints.lock().unwrap();
        let syncpoint = &syncpoints[id as usize];

        if !syncpoint.reserved {
            debug_assert!(false, "Syncpoint {} is not reserved", id);
            return NvFence::default();
        }

        NvFence {
            id: id as i32,
            value: syncpoint.counter_max.load(Ordering::Relaxed),
        }
    }

    /// Finds the first free syncpoint.
    /// Note: reservation_lock (the Mutex) should be held when calling this.
    fn find_free_syncpoint_inner(syncpoints: &[SyncpointInfo]) -> u32 {
        for i in 1..syncpoints.len() {
            if !syncpoints[i].reserved {
                return i as u32;
            }
        }
        log::error!("Failed to find a free syncpoint!");
        0
    }

    /// Reserves a syncpoint by ID.
    /// Note: reservation_lock (the Mutex) should be held when calling this.
    fn reserve_syncpoint_inner(syncpoints: &mut [SyncpointInfo], id: u32, client_managed: bool) -> u32 {
        let syncpoint = &mut syncpoints[id as usize];

        if syncpoint.reserved {
            debug_assert!(false, "Requested syncpoint {} is in use", id);
            return 0;
        }

        syncpoint.reserved = true;
        syncpoint.interface_managed = client_managed;

        id
    }
}
