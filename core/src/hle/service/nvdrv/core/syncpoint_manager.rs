// SPDX-FileCopyrightText: 2022 yuzu Emulator Project
// SPDX-FileCopyrightText: 2022 Skyline Team and Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/nvdrv/core/syncpoint_manager.h
//! Port of zuyu/src/core/hle/service/nvdrv/core/syncpoint_manager.cpp

use std::sync::atomic::{AtomicBool, AtomicU32, Ordering};
use std::sync::Mutex;

use crate::core::SystemRef;
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
    interface_managed: AtomicBool,
    reserved: AtomicBool,
}

impl Default for SyncpointInfo {
    fn default() -> Self {
        Self {
            counter_min: AtomicU32::new(0),
            counter_max: AtomicU32::new(0),
            interface_managed: AtomicBool::new(false),
            reserved: AtomicBool::new(false),
        }
    }
}

/// SyncpointManager handles allocating and accessing host1x syncpoints.
/// These are cached versions of the HW syncpoints which are intermittently synced.
///
/// Refer to Chapter 14 of the Tegra X1 TRM for an exhaustive overview of them.
/// https://http.download.nvidia.com/tegra-public-appnotes/host1x.html
pub struct SyncpointManager {
    system: SystemRef,
    syncpoints: [SyncpointInfo; SYNCPOINT_COUNT],
    reservation_lock: Mutex<()>,
}

impl SyncpointManager {
    pub fn new() -> Self {
        Self::new_with_system(SystemRef::null())
    }

    pub fn new_with_system(system: SystemRef) -> Self {
        let syncpoints = std::array::from_fn(|_| SyncpointInfo::default());

        const VBLANK0_SYNCPOINT_ID: usize = 26;
        const VBLANK1_SYNCPOINT_ID: usize = 27;

        // Reserve both vblank syncpoints as client managed as they use Continuous Mode.
        // Refer to section 14.3.5.3 of the TRM for more information on Continuous Mode.
        syncpoints[VBLANK0_SYNCPOINT_ID]
            .reserved
            .store(true, Ordering::Relaxed);
        syncpoints[VBLANK0_SYNCPOINT_ID]
            .interface_managed
            .store(true, Ordering::Relaxed);
        syncpoints[VBLANK1_SYNCPOINT_ID]
            .reserved
            .store(true, Ordering::Relaxed);
        syncpoints[VBLANK1_SYNCPOINT_ID]
            .interface_managed
            .store(true, Ordering::Relaxed);

        for &syncpoint_id in &CHANNEL_SYNCPOINTS {
            if syncpoint_id != 0 {
                syncpoints[syncpoint_id as usize]
                    .reserved
                    .store(true, Ordering::Relaxed);
                syncpoints[syncpoint_id as usize]
                    .interface_managed
                    .store(false, Ordering::Relaxed);
            }
        }

        Self {
            system,
            syncpoints,
            reservation_lock: Mutex::new(()),
        }
    }

    /// Checks if the given syncpoint is both allocated and below the number of HW syncpoints.
    pub fn is_syncpoint_allocated(&self, id: u32) -> bool {
        self.syncpoints
            .get(id as usize)
            .map(|syncpoint| syncpoint.reserved.load(Ordering::Relaxed))
            .unwrap_or(false)
    }

    /// Finds a free syncpoint and reserves it.
    /// Returns the ID of the reserved syncpoint.
    pub fn allocate_syncpoint(&self, client_managed: bool) -> u32 {
        let _lock = self.reservation_lock.lock().unwrap();
        self.reserve_syncpoint_inner(self.find_free_syncpoint_inner(), client_managed)
    }

    /// Frees the usage of a syncpoint.
    pub fn free_syncpoint(&self, id: u32) {
        let _lock = self.reservation_lock.lock().unwrap();
        let syncpoint = &self.syncpoints[id as usize];
        assert!(
            syncpoint.reserved.load(Ordering::Relaxed),
            "Syncpoint {} is not reserved",
            id
        );
        syncpoint.reserved.store(false, Ordering::Relaxed);
    }

    /// Checks if a syncpoint has expired relative to a threshold.
    ///
    /// https://github.com/Jetson-TX1-AndroidTV/android_kernel_jetson_tx1_hdmi_primary/blob/8f74a72394efb871cb3f886a3de2998cd7ff2990/drivers/gpu/host1x/syncpt.c#L259
    pub fn has_syncpoint_expired(&self, id: u32, threshold: u32) -> bool {
        let Some(syncpoint) = self.syncpoints.get(id as usize) else {
            debug_assert!(false, "Syncpoint {} is out of range", id);
            return false;
        };

        if !syncpoint.reserved.load(Ordering::Relaxed) {
            debug_assert!(false, "Syncpoint {} is not reserved", id);
            return false;
        }

        // If the interface manages counters then we don't keep track of the maximum value as it
        // handles sanity checking the values then
        if syncpoint.interface_managed.load(Ordering::Relaxed) {
            (syncpoint
                .counter_min
                .load(Ordering::Relaxed)
                .wrapping_sub(threshold)) as i32
                >= 0
        } else {
            syncpoint
                .counter_max
                .load(Ordering::Relaxed)
                .wrapping_sub(threshold)
                >= syncpoint
                    .counter_min
                    .load(Ordering::Relaxed)
                    .wrapping_sub(threshold)
        }
    }

    pub fn is_fence_signalled(&self, fence: &NvFence) -> bool {
        // Refresh counter_min from host1x before checking — same rationale as
        // `read_syncpoint_min_value`. Upstream's `IsFenceSignalled` reads
        // `counter_min`/`counter_max` directly under the assumption that
        // host1x has been keeping them in sync, but ruzu's bridge only fires
        // on explicit `update_min` calls. Without this refresh, the polling-
        // wait short-circuit in `nvhost_ctrl::IocCtrlEventWait` sees stale
        // counter_min and returns "signalled" prematurely (task #164).
        let _ = self.update_min(fence.id as u32);

        let result = self.has_syncpoint_expired(fence.id as u32, fence.value);
        if std::env::var_os("RUZU_TRACE_SYNCPOINT").is_some() {
            log::info!(
                "NvdrvSyncpointManager::is_fence_signalled id={} value={} -> {}",
                fence.id,
                fence.value,
                result
            );
        }
        result
    }

    /// Atomically increments the maximum value of a syncpoint by the given amount.
    /// Returns the new max value of the syncpoint.
    pub fn increment_syncpoint_max_ext(&self, id: u32, amount: u32) -> u32 {
        let Some(syncpoint) = self.syncpoints.get(id as usize) else {
            debug_assert!(false, "Syncpoint {} is out of range", id);
            return 0;
        };

        if !syncpoint.reserved.load(Ordering::Relaxed) {
            debug_assert!(false, "Syncpoint {} is not reserved", id);
            return 0;
        }

        let new_max = syncpoint.counter_max.fetch_add(amount, Ordering::Relaxed) + amount;
        if std::env::var_os("RUZU_TRACE_SYNCPOINT").is_some() {
            log::info!(
                "NvdrvSyncpointManager::increment_syncpoint_max_ext id={} amount={} new_max={}",
                id,
                amount,
                new_max
            );
        }
        new_max
    }

    /// Returns the maximum (predicted) value of the syncpoint.
    pub fn read_syncpoint_max_value(&self, id: u32) -> u32 {
        self.syncpoints[id as usize]
            .counter_max
            .load(Ordering::Relaxed)
    }

    /// Returns the minimum value of the syncpoint.
    ///
    /// Before returning, this refreshes `counter_min` from the host1x
    /// syncpoint manager (the GPU-side source of truth). Without this
    /// refresh, `counter_min` only updates when `update_min` is explicitly
    /// invoked (e.g. inside the IocCtrlEventWait failure path), which
    /// leaves the value stale on every other read site — including the
    /// "fence already signalled?" short-circuit at the top of
    /// IocCtrlEventWait. A stale `counter_min` makes `is_fence_signalled`
    /// return true with a 0-value fence on a freshly-initialized syncpoint,
    /// which makes MK8D's polling-wait loop exit prematurely (task #164).
    ///
    /// Performance-wise this adds one host1x atomic load + one `counter_min`
    /// atomic store per read; negligible compared to an ioctl roundtrip.
    pub fn read_syncpoint_min_value(&self, id: u32) -> u32 {
        let _ = self.update_min(id);

        let Some(syncpoint) = self.syncpoints.get(id as usize) else {
            debug_assert!(false, "Syncpoint {} is out of range", id);
            return 0;
        };

        if !syncpoint.reserved.load(Ordering::Relaxed) {
            debug_assert!(false, "Syncpoint {} is not reserved", id);
            return 0;
        }

        let value = syncpoint.counter_min.load(Ordering::Relaxed);
        if std::env::var_os("RUZU_TRACE_SYNCPOINT").is_some() {
            log::info!(
                "NvdrvSyncpointManager::read_syncpoint_min_value id={} -> {}",
                id,
                value
            );
        }
        value
    }

    /// Synchronises the minimum value of the syncpoint with the GPU.
    /// Returns the new minimum value of the syncpoint.
    ///
    /// Note: In the C++ code, this reads from host1x hardware via
    /// `host1x.GetSyncpointManager().GetHostSyncpointValue(id)`.
    /// If ruzu is constructed without a host1x core, this returns the current counter_min.
    pub fn update_min(&self, id: u32) -> u32 {
        let Some(syncpoint) = self.syncpoints.get(id as usize) else {
            debug_assert!(false, "Syncpoint {} is out of range", id);
            return 0;
        };

        if !syncpoint.reserved.load(Ordering::Relaxed) {
            debug_assert!(false, "Syncpoint {} is not reserved", id);
            return 0;
        }

        let host_value = self
            .system
            .get()
            .host1x_core()
            .map(|host1x| host1x.get_host_syncpoint_value(id))
            .unwrap_or_else(|| syncpoint.counter_min.load(Ordering::Relaxed));
        if std::env::var_os("RUZU_TRACE_SYNCPOINT").is_some() {
            log::info!(
                "NvdrvSyncpointManager::update_min id={} old_min={} max={} host_value={}",
                id,
                syncpoint.counter_min.load(Ordering::Relaxed),
                syncpoint.counter_max.load(Ordering::Relaxed),
                host_value
            );
        }
        syncpoint.counter_min.store(host_value, Ordering::Relaxed);
        host_value
    }

    /// Returns a fence that will be signalled once this syncpoint hits its maximum value.
    pub fn get_syncpoint_fence(&self, id: u32) -> NvFence {
        let Some(syncpoint) = self.syncpoints.get(id as usize) else {
            debug_assert!(false, "Syncpoint {} is out of range", id);
            return NvFence::default();
        };

        if !syncpoint.reserved.load(Ordering::Relaxed) {
            debug_assert!(false, "Syncpoint {} is not reserved", id);
            return NvFence::default();
        }

        NvFence {
            id: id as i32,
            value: syncpoint.counter_max.load(Ordering::Relaxed),
        }
    }

    /// Marks all queued work for a syncpoint as completed.
    ///
    /// Upstream updates `counter_min` from host1x hardware in `UpdateMin()`. The Rust port still
    /// lacks that bridge on this path, so synchronous nvdrv stubs use this helper to keep
    /// min/max coherent after they complete work immediately.
    pub fn signal_syncpoint(&self, id: u32) -> u32 {
        let Some(syncpoint) = self.syncpoints.get(id as usize) else {
            debug_assert!(false, "Syncpoint {} is out of range", id);
            return 0;
        };

        if !syncpoint.reserved.load(Ordering::Relaxed) {
            debug_assert!(false, "Syncpoint {} is not reserved", id);
            return 0;
        }

        let max = syncpoint.counter_max.load(Ordering::Relaxed);
        syncpoint.counter_min.store(max, Ordering::Relaxed);
        max
    }

    pub fn register_host_action(
        &self,
        id: u32,
        expected_value: u32,
        action: Box<dyn FnOnce() + Send>,
    ) -> Option<u64> {
        if std::env::var_os("RUZU_TRACE_SYNCPOINT").is_some() {
            log::info!(
                "NvdrvSyncpointManager::register_host_action id={} expected={}",
                id,
                expected_value
            );
        }
        self.system
            .get()
            .host1x_core()
            .and_then(|host1x| host1x.register_host_action(id, expected_value, action))
    }

    pub fn deregister_host_action(&self, id: u32, handle: u64) {
        if let Some(host1x) = self.system.get().host1x_core() {
            host1x.deregister_host_action(id, handle);
        }
    }

    pub fn wait_host(&self, id: u32, expected_value: u32) {
        if std::env::var_os("RUZU_TRACE_SYNCPOINT").is_some() {
            log::info!(
                "NvdrvSyncpointManager::wait_host id={} expected={}",
                id,
                expected_value
            );
        }
        if let Some(host1x) = self.system.get().host1x_core() {
            host1x.wait_host(id, expected_value);
        }
    }

    /// Finds the first free syncpoint.
    /// Note: reservation_lock should be held when calling this.
    fn find_free_syncpoint_inner(&self) -> u32 {
        for i in 1..self.syncpoints.len() {
            if !self.syncpoints[i].reserved.load(Ordering::Relaxed) {
                return i as u32;
            }
        }
        log::error!("Failed to find a free syncpoint!");
        0
    }

    /// Reserves a syncpoint by ID.
    /// Note: reservation_lock should be held when calling this.
    fn reserve_syncpoint_inner(&self, id: u32, client_managed: bool) -> u32 {
        let syncpoint = &self.syncpoints[id as usize];

        if syncpoint.reserved.load(Ordering::Relaxed) {
            debug_assert!(false, "Requested syncpoint {} is in use", id);
            return 0;
        }

        syncpoint.reserved.store(true, Ordering::Relaxed);
        syncpoint
            .interface_managed
            .store(client_managed, Ordering::Relaxed);

        id
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn constructor_reserves_upstream_static_syncpoints() {
        let manager = SyncpointManager::new();

        assert!(manager.is_syncpoint_allocated(26));
        assert!(manager.is_syncpoint_allocated(27));
        assert!(manager.is_syncpoint_allocated(CHANNEL_SYNCPOINTS[ChannelType::VIC as usize] as u32));
        assert!(
            manager.is_syncpoint_allocated(CHANNEL_SYNCPOINTS[ChannelType::NvDec as usize] as u32)
        );
        assert!(
            manager.is_syncpoint_allocated(CHANNEL_SYNCPOINTS[ChannelType::NvJpg as usize] as u32)
        );
    }

    #[test]
    fn allocate_and_free_only_change_reservation_state() {
        let manager = SyncpointManager::new();

        let id = manager.allocate_syncpoint(false);
        assert_ne!(id, 0);
        assert!(manager.is_syncpoint_allocated(id));

        manager.increment_syncpoint_max_ext(id, 5);
        assert_eq!(manager.read_syncpoint_max_value(id), 5);

        manager.free_syncpoint(id);
        assert!(!manager.is_syncpoint_allocated(id));
    }

    #[test]
    fn has_syncpoint_expired_matches_upstream_counter_math() {
        let manager = SyncpointManager::new();
        let id = manager.allocate_syncpoint(false);

        manager.increment_syncpoint_max_ext(id, 10);
        assert!(manager.has_syncpoint_expired(id, 5));
        assert!(manager.has_syncpoint_expired(id, 10));
        assert!(!manager.has_syncpoint_expired(id, 11));

        manager.signal_syncpoint(id);
        assert!(manager.has_syncpoint_expired(id, 10));
        assert!(!manager.has_syncpoint_expired(id, 11));
    }
}
