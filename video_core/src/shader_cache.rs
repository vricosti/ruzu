// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/shader_cache.h and video_core/shader_cache.cpp
//!
//! Shader binary caching and invalidation.

use std::collections::HashMap;
use std::sync::Mutex;

use crate::control::channel_state::ChannelState;
use crate::control::channel_state_cache::{ChannelInfo, ChannelSetupCaches};
use crate::host1x::gpu_device_memory_manager::MaxwellDeviceMemoryManager;
use crate::shader_environment::GenericEnvironment;

/// Virtual address type.
pub type VAddr = u64;

const YUZU_PAGEBITS: u64 = 14;
const YUZU_PAGESIZE: u64 = 1 << YUZU_PAGEBITS;
const NUM_PROGRAMS: usize = 6;

/// Information about a compiled shader.
#[derive(Debug, Default)]
pub struct ShaderInfo {
    pub unique_hash: u64,
    pub size_bytes: usize,
}

/// An entry in the shader lookup cache.
struct Entry {
    addr_start: VAddr,
    addr_end: VAddr,
    data: *mut ShaderInfo,
    is_memory_marked: bool,
}

impl Entry {
    fn overlaps(&self, start: VAddr, end: VAddr) -> bool {
        start < self.addr_end && self.addr_start < end
    }
}

/// Shader cache that tracks compiled shaders by their guest memory address.
///
/// Handles invalidation when guest memory is modified.
pub struct ShaderCache {
    device_memory: MaxwellDeviceMemoryManager,
    channel_caches: ChannelSetupCaches<ChannelInfo>,
    lookup_mutex: Mutex<()>,
    invalidation_mutex: Mutex<()>,
    lookup_cache: HashMap<u64, Box<Entry>>,
    invalidation_cache: HashMap<u64, Vec<*mut Entry>>,
    storage: Vec<Box<ShaderInfo>>,
    marked_for_removal: Vec<*mut Entry>,
    shader_infos: [Option<usize>; NUM_PROGRAMS],
    last_shaders_valid: bool,
}

// Safety: Entry pointers are only used within locked sections.
unsafe impl Send for ShaderCache {}
unsafe impl Sync for ShaderCache {}

impl ShaderCache {
    pub fn new() -> Self {
        Self {
            device_memory: MaxwellDeviceMemoryManager::default(),
            channel_caches: ChannelSetupCaches::new(),
            lookup_mutex: Mutex::new(()),
            invalidation_mutex: Mutex::new(()),
            lookup_cache: HashMap::new(),
            invalidation_cache: HashMap::new(),
            storage: Vec::new(),
            marked_for_removal: Vec::new(),
            shader_infos: [None; NUM_PROGRAMS],
            last_shaders_valid: false,
        }
    }

    /// Port of the shared `ShaderCache` channel-owner `CreateChannel` edge.
    pub fn create_channel(&mut self, channel: &ChannelState) {
        self.channel_caches.create_channel(channel);
    }

    /// Port of the shared `ShaderCache` channel-owner `BindToChannel` edge.
    pub fn bind_to_channel(&mut self, channel_id: i32) {
        self.channel_caches.bind_to_channel(channel_id);
    }

    /// Port of the shared `ShaderCache` channel-owner `EraseChannel` edge.
    pub fn erase_channel(&mut self, channel_id: i32) {
        self.channel_caches.erase_channel(channel_id);
    }

    /// Reduced Rust accessor for the currently bound shared channel owner.
    pub fn current_channel_info(&self) -> Option<&ChannelInfo> {
        self.channel_caches.current_channel_state()
    }

    /// Reduced Rust accessor for the shared shader-stage cache state.
    pub fn last_shaders_valid(&self) -> bool {
        self.last_shaders_valid
    }

    /// Reduced Rust accessor for the shared shader-info owner slots.
    pub fn shader_info_slots(&self) -> &[Option<usize>; NUM_PROGRAMS] {
        &self.shader_infos
    }

    /// Removes shaders inside a given region.
    pub fn invalidate_region(&mut self, addr: VAddr, size: usize) {
        // Note: upstream locks invalidation_mutex here, but &mut self already
        // guarantees exclusive access in Rust.
        self.invalidate_pages_in_region(addr, size);
        self.remove_pending_shaders();
    }

    /// Unmarks a memory region as cached and marks it for removal.
    pub fn on_cache_invalidation(&mut self, addr: VAddr, size: usize) {
        self.invalidate_pages_in_region(addr, size);
    }

    /// Flushes delayed removal operations.
    pub fn sync_guest_host(&mut self) {
        self.remove_pending_shaders();
    }

    /// Port of `ShaderCache::Register`.
    pub fn register(&mut self, data: Box<ShaderInfo>, addr: VAddr, size: usize) {
        let addr_end = addr + size as u64;
        let data_ptr = (&*data as *const ShaderInfo).cast_mut();
        let entry = self.new_entry(addr, addr_end, data_ptr);

        let page_end = (addr_end + YUZU_PAGESIZE - 1) >> YUZU_PAGEBITS;
        for page in (addr >> YUZU_PAGEBITS)..page_end {
            self.invalidation_cache.entry(page).or_default().push(entry);
        }

        self.storage.push(data);
        self.device_memory.update_pages_cached_count(addr, size, 1);
    }

    fn invalidate_pages_in_region(&mut self, addr: VAddr, size: usize) {
        let addr_end = addr + size as u64;
        let page_end = (addr_end + YUZU_PAGESIZE - 1) >> YUZU_PAGEBITS;
        for page in (addr >> YUZU_PAGEBITS)..page_end {
            let Some(mut entries) = self.invalidation_cache.remove(&page) else {
                continue;
            };
            self.invalidate_page_entries(&mut entries, addr, addr_end);
            if !entries.is_empty() {
                self.invalidation_cache.insert(page, entries);
            }
        }
    }

    fn remove_pending_shaders(&mut self) {
        if self.marked_for_removal.is_empty() {
            return;
        }

        // Remove duplicates (port of std::ranges::sort + std::unique in upstream).
        self.marked_for_removal.sort_by_key(|p| *p as usize);
        self.marked_for_removal.dedup();

        let mut removed_shaders: Vec<*mut ShaderInfo> = Vec::new();

        for &entry_ptr in &self.marked_for_removal {
            let entry = unsafe { &*entry_ptr };

            // Remove this entry from all invalidation cache pages it spans.
            let entry_page_start = entry.addr_start >> YUZU_PAGEBITS;
            let entry_page_end = (entry.addr_end + YUZU_PAGESIZE - 1) >> YUZU_PAGEBITS;
            for pg in entry_page_start..entry_page_end {
                if let Some(page_entries) = self.invalidation_cache.get_mut(&pg) {
                    if let Some(pos) = page_entries.iter().position(|&e| e == entry_ptr) {
                        page_entries.swap_remove(pos);
                    }
                }
            }

            removed_shaders.push(entry.data);

            // Remove from lookup cache.
            self.lookup_cache.remove(&entry.addr_start);
        }
        self.marked_for_removal.clear();

        // Remove from storage (port of RemoveShadersFromStorage).
        if !removed_shaders.is_empty() {
            self.remove_shaders_from_storage(&removed_shaders);
        }
    }

    /// Port of `ShaderCache::InvalidatePageEntries`.
    fn invalidate_page_entries(
        &mut self,
        entries: &mut Vec<*mut Entry>,
        addr: VAddr,
        addr_end: VAddr,
    ) {
        let mut index = 0;
        while index < entries.len() {
            let entry_ptr = entries[index];
            let entry = unsafe { &mut *entry_ptr };
            if !entry.overlaps(addr, addr_end) {
                index += 1;
                continue;
            }

            self.unmark_memory(entry);
            self.remove_entry_from_invalidation_cache(entry);
            self.marked_for_removal.push(entry_ptr);
        }
    }

    /// Port of `ShaderCache::RemoveEntryFromInvalidationCache`.
    fn remove_entry_from_invalidation_cache(&mut self, entry: &Entry) {
        let page_end = (entry.addr_end + YUZU_PAGESIZE - 1) >> YUZU_PAGEBITS;
        for page in (entry.addr_start >> YUZU_PAGEBITS)..page_end {
            let Some(entries) = self.invalidation_cache.get_mut(&page) else {
                continue;
            };
            if let Some(position) = entries
                .iter()
                .position(|existing| std::ptr::eq(*existing, entry))
            {
                entries.remove(position);
            }
            if entries.is_empty() {
                self.invalidation_cache.remove(&page);
            }
        }
    }

    /// Port of `ShaderCache::UnmarkMemory`.
    fn unmark_memory(&mut self, entry: &mut Entry) {
        if !entry.is_memory_marked {
            return;
        }
        entry.is_memory_marked = false;
        self.device_memory.update_pages_cached_count(
            entry.addr_start,
            (entry.addr_end - entry.addr_start) as usize,
            -1,
        );
    }

    /// Port of `ShaderCache::RemoveShadersFromStorage`.
    fn remove_shaders_from_storage(&mut self, removed_shaders: &[*mut ShaderInfo]) {
        self.storage.retain(|shader| {
            let ptr: *mut ShaderInfo = (&**shader as *const ShaderInfo).cast_mut();
            !removed_shaders.contains(&ptr)
        });
    }

    /// Port of `ShaderCache::NewEntry`.
    fn new_entry(&mut self, addr: VAddr, addr_end: VAddr, data: *mut ShaderInfo) -> *mut Entry {
        let mut entry = Box::new(Entry {
            addr_start: addr,
            addr_end,
            data,
            is_memory_marked: true,
        });
        let entry_ptr: *mut Entry = &mut *entry;
        self.lookup_cache.insert(addr, entry);
        entry_ptr
    }

    /// Try to get a cached shader at the given address.
    pub fn try_get(&self, addr: VAddr) -> Option<&ShaderInfo> {
        let _lock = self.lookup_mutex.lock().unwrap();
        self.lookup_cache
            .get(&addr)
            .map(|entry| unsafe { &*entry.data })
    }

    /// Port of `ShaderCache::MakeShaderInfo`.
    pub fn make_shader_info(
        &mut self,
        env: &mut GenericEnvironment,
        cpu_addr: VAddr,
    ) -> &ShaderInfo {
        let mut info = Box::new(ShaderInfo::default());
        if let Some(cached_hash) = env.analyze() {
            info.unique_hash = cached_hash;
            info.size_bytes = env.cached_size_bytes();
        } else {
            info.unique_hash = env.calculate_hash();
            info.size_bytes = env.read_size_bytes();
        }
        let size_bytes = info.size_bytes;
        self.register(info, cpu_addr, size_bytes);
        self.try_get(cpu_addr)
            .expect("registered shader info must be reachable through lookup cache")
    }
}

impl Default for ShaderCache {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::memory_manager::MemoryManager;
    use crate::shader_environment::GenericEnvironment;
    use parking_lot::Mutex as ParkingLotMutex;
    use std::sync::Arc;

    #[test]
    fn shader_cache_starts_with_upstream_shared_state_defaults() {
        let cache = ShaderCache::new();
        assert!(cache.current_channel_info().is_none());
        assert_eq!(cache.shader_info_slots(), &[None; NUM_PROGRAMS]);
        assert!(!cache.last_shaders_valid());
    }

    #[test]
    fn shader_cache_channel_owner_tracks_bound_channel_info() {
        let mut cache = ShaderCache::new();
        let mut channel = ChannelState::new(7);
        channel.program_id = 0x1234;
        channel.memory_manager = Some(Arc::new(ParkingLotMutex::new(MemoryManager::new(0))));
        channel.maxwell_3d = Some(Box::default());
        channel.kepler_compute = Some(Box::default());

        cache.create_channel(&channel);
        cache.bind_to_channel(7);

        let info = cache
            .current_channel_info()
            .expect("channel should be bound into shared shader-cache owner");
        assert_eq!(info.program_id, 0x1234);
        assert_ne!(info.maxwell3d, 0);
        assert_ne!(info.kepler_compute, 0);
        assert!(info.gpu_memory.is_some());
    }

    #[test]
    fn register_creates_lookup_and_invalidation_entries() {
        let mut cache = ShaderCache::new();
        cache.register(
            Box::new(ShaderInfo {
                unique_hash: 0x1234,
                size_bytes: 0x200,
            }),
            0x4000,
            0x200,
        );

        let shader = cache
            .try_get(0x4000)
            .expect("registered shader should be cached");
        assert_eq!(shader.unique_hash, 0x1234);
        assert!(cache
            .invalidation_cache
            .values()
            .any(|entries| !entries.is_empty()));
    }

    #[test]
    fn make_shader_info_registers_analyzed_shader() {
        let program_base = 0x1_0000_0000;
        let sentinel_offset = 0x80usize;
        let sentinel = 0xE2400FFF00000F00u64;

        let mut backing = vec![0u8; 0x2000];
        backing[sentinel_offset..sentinel_offset + 8].copy_from_slice(&sentinel.to_le_bytes());
        let backing = Arc::new(backing);
        let reader = Arc::new(move |gpu_addr: u64, dst: &mut [u8]| {
            dst.fill(0);
            let offset = (gpu_addr - program_base) as usize;
            if offset >= backing.len() {
                return;
            }
            let available = backing.len() - offset;
            let count = available.min(dst.len());
            dst[..count].copy_from_slice(&backing[offset..offset + count]);
        });

        let mut env = GenericEnvironment::new()
            .with_gpu_read(reader)
            .with_program(program_base, 0);
        let _ = env.read_instruction(0);
        let mut cache = ShaderCache::new();

        let shader = cache.make_shader_info(&mut env, 0x9000);
        assert_ne!(shader.unique_hash, 0);
        assert!(shader.size_bytes >= 8);
        assert!(cache.try_get(0x9000).is_some());
    }
}
