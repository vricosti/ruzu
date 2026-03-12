// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/shader_cache.h and video_core/shader_cache.cpp
//!
//! Shader binary caching and invalidation.

use std::collections::HashMap;
use std::sync::Mutex;

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
    lookup_mutex: Mutex<()>,
    invalidation_mutex: Mutex<()>,
    lookup_cache: HashMap<u64, Box<Entry>>,
    invalidation_cache: HashMap<u64, Vec<*mut Entry>>,
    storage: Vec<Box<ShaderInfo>>,
    marked_for_removal: Vec<*mut Entry>,
    // In the full port:
    // device_memory: &MaxwellDeviceMemoryManager,
    // shader_infos: [Option<&ShaderInfo>; NUM_PROGRAMS],
    // last_shaders_valid: bool,
}

// Safety: Entry pointers are only used within locked sections.
unsafe impl Send for ShaderCache {}
unsafe impl Sync for ShaderCache {}

impl ShaderCache {
    pub fn new() -> Self {
        Self {
            lookup_mutex: Mutex::new(()),
            invalidation_mutex: Mutex::new(()),
            lookup_cache: HashMap::new(),
            invalidation_cache: HashMap::new(),
            storage: Vec::new(),
            marked_for_removal: Vec::new(),
        }
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

    fn invalidate_pages_in_region(&mut self, addr: VAddr, size: usize) {
        let addr_end = addr + size as u64;
        let page_end = (addr_end + YUZU_PAGESIZE - 1) >> YUZU_PAGEBITS;
        let mut page = addr >> YUZU_PAGEBITS;
        while page < page_end {
            if let Some(entries) = self.invalidation_cache.get_mut(&page) {
                // TODO: invalidate page entries and mark for removal
                let _ = entries;
            }
            page += 1;
        }
    }

    fn remove_pending_shaders(&mut self) {
        if self.marked_for_removal.is_empty() {
            return;
        }
        self.marked_for_removal.clear();
        // TODO: full removal logic
    }

    /// Try to get a cached shader at the given address.
    pub fn try_get(&self, addr: VAddr) -> Option<&ShaderInfo> {
        let _lock = self.lookup_mutex.lock().unwrap();
        self.lookup_cache
            .get(&addr)
            .map(|entry| unsafe { &*entry.data })
    }
}

impl Default for ShaderCache {
    fn default() -> Self {
        Self::new()
    }
}
