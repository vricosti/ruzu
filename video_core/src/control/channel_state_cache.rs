// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/video_core/control/channel_state_cache.h,
//!            zuyu/src/video_core/control/channel_state_cache.cpp, and
//!            zuyu/src/video_core/control/channel_state_cache.inc
//!
//! Provides per-channel cache state and a generic `ChannelSetupCaches<P>`
//! container that tracks channel ↔ address-space mappings.
//!
//! The C++ code uses a class template (`ChannelSetupCaches<P>`) with a
//! separate `.inc` file for the template method bodies.  In Rust this is
//! expressed as a generic struct with an `impl<P>` block.

use std::collections::{HashMap, VecDeque};

use parking_lot::Mutex;

use super::channel_state::ChannelState;

// ---------------------------------------------------------------------------
// ChannelInfo — non-generic, corresponds to VideoCommon::ChannelInfo
// ---------------------------------------------------------------------------

/// Snapshot of a channel's key references, taken at channel creation time.
///
/// Corresponds to `VideoCommon::ChannelInfo` (channel_state_cache.h).
/// The C++ version holds raw references; here we store indices / IDs that
/// can be resolved through the owning `ChannelState`.
pub struct ChannelInfo {
    /// Index into the owning `ChannelState`'s Maxwell3D engine.
    /// Upstream: `Tegra::Engines::Maxwell3D& maxwell3d`.
    pub maxwell3d_index: usize,
    /// Index into the owning `ChannelState`'s KeplerCompute engine.
    /// Upstream: `Tegra::Engines::KeplerCompute& kepler_compute`.
    pub kepler_compute_index: usize,
    /// Index into the owning `ChannelState`'s GPU memory manager.
    /// Upstream: `Tegra::MemoryManager& gpu_memory`.
    pub gpu_memory_index: usize,
    /// Program ID copied from the channel state.
    pub program_id: u64,
}

impl ChannelInfo {
    /// Construct from a `ChannelState`.
    ///
    /// Corresponds to `ChannelInfo::ChannelInfo(Tegra::Control::ChannelState&)`.
    pub fn from_channel_state(channel_state: &ChannelState) -> Self {
        // Upstream dereferences the unique_ptrs; we capture indices/IDs for
        // later resolution.  When real engine types exist these will hold
        // proper references or Arc handles.
        Self {
            maxwell3d_index: 0,      // placeholder
            kepler_compute_index: 0, // placeholder
            gpu_memory_index: 0,     // placeholder
            program_id: channel_state.program_id,
        }
    }
}

// ---------------------------------------------------------------------------
// AddressSpaceRef — inner bookkeeping struct
// ---------------------------------------------------------------------------

/// Tracks reference-counted address-space registrations.
///
/// Corresponds to `ChannelSetupCaches<P>::AddressSpaceRef`.
struct AddressSpaceRef {
    ref_count: usize,
    storage_id: usize,
    gpu_memory_id: usize,
}

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

/// Sentinel value for an unbound channel.
///
/// Corresponds to `ChannelSetupCaches<P>::UNSET_CHANNEL`.
const UNSET_CHANNEL: usize = usize::MAX;

// ---------------------------------------------------------------------------
// ChannelSetupCaches<P>
// ---------------------------------------------------------------------------

/// Generic per-channel cache container.
///
/// Corresponds to `VideoCommon::ChannelSetupCaches<P>` (header + `.inc`).
///
/// The type parameter `P` is the per-channel cache payload (e.g.
/// `ChannelInfo`).  Upstream instantiates this as
/// `ChannelSetupCaches<ChannelInfo>`.
pub struct ChannelSetupCaches<P> {
    // -- "current" state (updated by bind_to_channel) ---------------------
    channel_state: Option<usize>,
    current_channel_id: usize,
    current_address_space: usize,

    /// Cached Maxwell3D index for the currently bound channel.
    pub maxwell3d: Option<usize>,
    /// Cached KeplerCompute index for the currently bound channel.
    pub kepler_compute: Option<usize>,
    /// Cached GPU memory ID for the currently bound channel.
    pub gpu_memory: Option<usize>,
    /// Program ID of the currently bound channel.
    pub program_id: u64,

    // -- storage ----------------------------------------------------------
    channel_storage: VecDeque<P>,
    free_channel_ids: VecDeque<usize>,
    channel_map: HashMap<i32, usize>,
    active_channel_ids: Vec<usize>,
    address_spaces: HashMap<usize, AddressSpaceRef>,

    /// Protects the above collections.
    ///
    /// Upstream: `mutable std::mutex config_mutex`.
    config_mutex: Mutex<()>,
}

impl<P> ChannelSetupCaches<P> {
    /// Create an empty cache set.
    pub fn new() -> Self {
        Self {
            channel_state: None,
            current_channel_id: UNSET_CHANNEL,
            current_address_space: 0,
            maxwell3d: None,
            kepler_compute: None,
            gpu_memory: None,
            program_id: 0,
            channel_storage: VecDeque::new(),
            free_channel_ids: VecDeque::new(),
            channel_map: HashMap::new(),
            active_channel_ids: Vec::new(),
            address_spaces: HashMap::new(),
            config_mutex: Mutex::new(()),
        }
    }

    /// Create channel state.
    ///
    /// Corresponds to `ChannelSetupCaches<P>::CreateChannel` (channel_state_cache.inc).
    pub fn create_channel(&mut self, channel: &ChannelState)
    where
        P: FromChannelState,
    {
        // Note: upstream locks config_mutex here, but &mut self already
        // guarantees exclusive access in Rust.
        assert!(
            !self.channel_map.contains_key(&channel.bind_id) && channel.bind_id >= 0,
            "duplicate or negative bind_id in create_channel"
        );

        let new_id = if let Some(id) = self.free_channel_ids.pop_front() {
            self.channel_storage[id] = P::from_channel_state(channel);
            id
        } else {
            self.channel_storage
                .push_back(P::from_channel_state(channel));
            self.channel_storage.len() - 1
        };

        self.channel_map.insert(channel.bind_id, new_id);

        if self.current_channel_id != UNSET_CHANNEL {
            self.channel_state = Some(self.current_channel_id);
        }

        self.active_channel_ids.push(new_id);

        // Address-space bookkeeping.
        if let Some(ref mm) = channel.memory_manager {
            let mm_locked = mm.lock();
            let mm_id = mm_locked.get_id();
            if let Some(entry) = self.address_spaces.get_mut(&mm_id) {
                entry.ref_count += 1;
                return;
            }
            let storage_id = self.address_spaces.len();
            self.address_spaces.insert(
                mm_id,
                AddressSpaceRef {
                    ref_count: 1,
                    storage_id,
                    gpu_memory_id: mm_id,
                },
            );
            self.on_gpu_as_register(mm_id);
        }
    }

    /// Bind a channel for execution.
    ///
    /// Corresponds to `ChannelSetupCaches<P>::BindToChannel` (channel_state_cache.inc).
    pub fn bind_to_channel(&mut self, id: i32)
    where
        P: ChannelCacheAccessor,
    {
        // Note: upstream locks config_mutex; &mut self provides exclusivity.

        let &storage_id = self
            .channel_map
            .get(&id)
            .expect("bind_to_channel: unknown channel id");
        assert!(id >= 0, "bind_to_channel: negative id");

        self.current_channel_id = storage_id;
        self.channel_state = Some(storage_id);

        let state = &self.channel_storage[storage_id];
        self.maxwell3d = Some(state.maxwell3d_ref());
        self.kepler_compute = Some(state.kepler_compute_ref());
        self.gpu_memory = Some(state.gpu_memory_ref());
        self.program_id = state.program_id_val();
        self.current_address_space = state.gpu_memory_ref();
    }

    /// Erase channel's state.
    ///
    /// Corresponds to `ChannelSetupCaches<P>::EraseChannel` (channel_state_cache.inc).
    pub fn erase_channel(&mut self, id: i32) {
        // Note: upstream locks config_mutex; &mut self provides exclusivity.

        let &storage_id = self
            .channel_map
            .get(&id)
            .expect("erase_channel: unknown channel id");
        assert!(id >= 0, "erase_channel: negative id");

        self.free_channel_ids.push_back(storage_id);
        self.channel_map.remove(&id);

        if storage_id == self.current_channel_id {
            self.current_channel_id = UNSET_CHANNEL;
            self.channel_state = None;
            self.maxwell3d = None;
            self.kepler_compute = None;
            self.gpu_memory = None;
            self.program_id = 0;
        } else if self.current_channel_id != UNSET_CHANNEL {
            self.channel_state = Some(self.current_channel_id);
        }

        if let Some(pos) = self
            .active_channel_ids
            .iter()
            .position(|&x| x == storage_id)
        {
            self.active_channel_ids.remove(pos);
        }
    }

    /// Look up a `MemoryManager` GPU-memory ID by address-space map id.
    ///
    /// Corresponds to `ChannelSetupCaches<P>::GetFromID`.
    pub fn get_from_id(&self, id: usize) -> Option<usize> {
        // Note: upstream locks config_mutex; &mut self provides exclusivity.
        self.address_spaces.get(&id).map(|r| r.gpu_memory_id)
    }

    /// Look up the storage id for an address-space map id.
    ///
    /// Corresponds to `ChannelSetupCaches<P>::getStorageID`.
    pub fn get_storage_id(&self, id: usize) -> Option<usize> {
        // Note: upstream locks config_mutex; &mut self provides exclusivity.
        self.address_spaces.get(&id).map(|r| r.storage_id)
    }

    /// Hook called when a new GPU address space is registered.
    ///
    /// Corresponds to `ChannelSetupCaches<P>::OnGPUASRegister`.
    /// Override in derived types if needed.
    #[allow(unused_variables)]
    fn on_gpu_as_register(&mut self, map_id: usize) {
        // Default: no-op, matching upstream.
    }
}

impl<P> Default for ChannelSetupCaches<P> {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// Helper traits — replace C++ template parameter interface
// ---------------------------------------------------------------------------

/// Construct a `P` from a `ChannelState`.
///
/// Mirrors the upstream pattern where `ChannelSetupCaches<P>::CreateChannel`
/// placement-news a `P` from a `ChannelState&`.
pub trait FromChannelState {
    fn from_channel_state(state: &ChannelState) -> Self;
}

/// Accessor trait for reading cached engine references out of a `P`.
///
/// Mirrors the member accesses in `ChannelSetupCaches<P>::BindToChannel`.
pub trait ChannelCacheAccessor {
    fn maxwell3d_ref(&self) -> usize;
    fn kepler_compute_ref(&self) -> usize;
    fn gpu_memory_ref(&self) -> usize;
    fn program_id_val(&self) -> u64;
}

// ---------------------------------------------------------------------------
// Trait implementations for ChannelInfo
// ---------------------------------------------------------------------------

impl FromChannelState for ChannelInfo {
    fn from_channel_state(state: &ChannelState) -> Self {
        ChannelInfo::from_channel_state(state)
    }
}

impl ChannelCacheAccessor for ChannelInfo {
    fn maxwell3d_ref(&self) -> usize {
        self.maxwell3d_index
    }

    fn kepler_compute_ref(&self) -> usize {
        self.kepler_compute_index
    }

    fn gpu_memory_ref(&self) -> usize {
        self.gpu_memory_index
    }

    fn program_id_val(&self) -> u64 {
        self.program_id
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unset_channel_sentinel() {
        assert_eq!(UNSET_CHANNEL, usize::MAX);
    }

    #[test]
    fn test_channel_setup_caches_new() {
        let caches: ChannelSetupCaches<ChannelInfo> = ChannelSetupCaches::new();
        assert_eq!(caches.current_channel_id, UNSET_CHANNEL);
        assert!(caches.channel_map.is_empty());
        assert!(caches.active_channel_ids.is_empty());
    }

    #[test]
    fn test_create_and_erase_channel() {
        use crate::memory_manager::MemoryManager;
        use parking_lot::Mutex;
        use std::sync::Arc;

        let mut caches: ChannelSetupCaches<ChannelInfo> = ChannelSetupCaches::new();

        let mut cs = ChannelState::new(7);
        cs.program_id = 0x1234;
        cs.memory_manager = Some(Arc::new(Mutex::new(MemoryManager::new(99))));

        caches.create_channel(&cs);
        assert!(caches.channel_map.contains_key(&7));
        assert_eq!(caches.active_channel_ids.len(), 1);

        caches.erase_channel(7);
        assert!(!caches.channel_map.contains_key(&7));
        assert!(caches.active_channel_ids.is_empty());
    }

    #[test]
    fn test_bind_to_channel() {
        use crate::memory_manager::MemoryManager;
        use parking_lot::Mutex;
        use std::sync::Arc;

        let mut caches: ChannelSetupCaches<ChannelInfo> = ChannelSetupCaches::new();

        let mut cs = ChannelState::new(3);
        cs.program_id = 0xABCD;
        cs.memory_manager = Some(Arc::new(Mutex::new(MemoryManager::new(42))));

        caches.create_channel(&cs);
        caches.bind_to_channel(3);

        assert_eq!(caches.program_id, 0xABCD);
        assert!(caches.channel_state.is_some());
    }
}
