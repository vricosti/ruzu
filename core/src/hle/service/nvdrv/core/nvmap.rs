// SPDX-FileCopyrightText: 2022 yuzu Emulator Project
// SPDX-FileCopyrightText: 2022 Skyline Team and Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/nvdrv/core/nvmap.h
//! Port of zuyu/src/core/hle/service/nvdrv/core/nvmap.cpp

use std::collections::{HashMap, LinkedList};
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::{Arc, Mutex, Weak};

use crate::hle::service::nvdrv::core::container::SessionId;
use crate::hle::service::nvdrv::nvdata::NvResult;

const PAGE_SIZE: u64 = 0x1000;

/// Handle flags mirroring the C++ BitField union.
#[repr(C)]
#[derive(Debug, Clone, Copy, Default)]
pub struct HandleFlags {
    pub raw: u32,
}

impl HandleFlags {
    pub fn map_uncached(&self) -> bool {
        self.raw & 1 != 0
    }

    pub fn set_map_uncached(&mut self, val: bool) {
        if val {
            self.raw |= 1;
        } else {
            self.raw &= !1;
        }
    }

    pub fn keep_uncached_after_free(&self) -> bool {
        (self.raw >> 2) & 1 != 0
    }

    pub fn set_keep_uncached_after_free(&mut self, val: bool) {
        if val {
            self.raw |= 1 << 2;
        } else {
            self.raw &= !(1 << 2);
        }
    }
}

pub type HandleId = u32;

/// A handle to a contiguous block of memory in an application's address space.
///
/// Interior mutability is achieved via `Mutex<HandleInner>` since many operations
/// need to modify handle state behind shared references.
pub struct Handle {
    pub id: HandleId,
    pub orig_size: u64,
    inner: Mutex<HandleInner>,
}

/// Mutable state of a Handle, protected by the Handle's mutex.
pub struct HandleInner {
    pub align: u64,
    pub size: u64,
    pub aligned_size: u64,
    pub dupes: i32,
    pub internal_dupes: i32,
    pub pins: i64,
    pub pin_virt_address: u32,
    pub flags: HandleFlags,
    pub address: u64,
    pub is_shared_mem_mapped: bool,
    pub kind: u8,
    pub allocated: bool,
    pub in_heap: bool,
    pub session_id: SessionId,
    pub d_address: u64,
}

impl Handle {
    pub fn new(size: u64, id: HandleId) -> Self {
        Self {
            id,
            orig_size: size,
            inner: Mutex::new(HandleInner {
                align: 0,
                size,
                aligned_size: size,
                dupes: 1,
                internal_dupes: 0,
                pins: 0,
                pin_virt_address: 0,
                flags: HandleFlags { raw: 0 },
                address: 0,
                is_shared_mem_mapped: false,
                kind: 0,
                allocated: false,
                in_heap: false,
                session_id: SessionId { id: 0 },
                d_address: 0,
            }),
        }
    }

    /// Sets up the handle with the given memory config, can allocate memory from the tmem
    /// if a 0 address is passed.
    pub fn alloc(
        &self,
        p_flags: HandleFlags,
        p_align: u32,
        p_kind: u8,
        p_address: u64,
        p_session_id: SessionId,
    ) -> NvResult {
        let mut inner = self.inner.lock().unwrap();

        // Handles cannot be allocated twice
        if inner.allocated {
            return NvResult::AccessDenied;
        }

        inner.flags = p_flags;
        inner.kind = p_kind;
        inner.align = if (p_align as u64) < PAGE_SIZE {
            PAGE_SIZE
        } else {
            p_align as u64
        };
        inner.session_id = p_session_id;

        // This flag is only applicable for handles with an address passed
        if p_address != 0 {
            inner.flags.set_keep_uncached_after_free(false);
        } else {
            log::error!("Mapping nvmap handles without a CPU side address is unimplemented!");
        }

        inner.size = (inner.size + PAGE_SIZE - 1) & !(PAGE_SIZE - 1);
        inner.aligned_size = (inner.size + inner.align - 1) & !(inner.align - 1);
        inner.address = p_address;
        inner.allocated = true;

        NvResult::Success
    }

    /// Increases the dupe counter of the handle for the given session.
    pub fn duplicate(&self, internal_session: bool) -> NvResult {
        let mut inner = self.inner.lock().unwrap();

        // Unallocated handles cannot be duplicated as duplication requires memory accounting (in HOS)
        if !inner.allocated {
            return NvResult::BadValue;
        }

        // If we internally use FromId the duplication tracking of handles won't work accurately due
        // to us not implementing per-process handle refs.
        if internal_session {
            inner.internal_dupes += 1;
        } else {
            inner.dupes += 1;
        }

        NvResult::Success
    }

    /// Locks the inner state and returns the guard for direct access.
    pub fn lock_inner(&self) -> std::sync::MutexGuard<'_, HandleInner> {
        self.inner.lock().unwrap()
    }
}

/// Encapsulates the result of a FreeHandle operation.
pub struct FreeInfo {
    pub address: u64,
    pub size: u64,
    pub was_uncached: bool,
    pub can_unlock: bool,
}

/// Each new handle ID is an increment of 4 from the previous.
const HANDLE_ID_INCREMENT: u32 = 4;

/// The nvmap core class holds the global state for nvmap and provides methods to manage handles.
pub struct NvMap {
    handles: Mutex<HashMap<HandleId, Arc<Handle>>>,
    unmap_queue: Mutex<LinkedList<Arc<Handle>>>,
    next_handle_id: AtomicU32,
}

impl NvMap {
    pub fn new() -> Self {
        Self {
            handles: Mutex::new(HashMap::new()),
            unmap_queue: Mutex::new(LinkedList::new()),
            next_handle_id: AtomicU32::new(HANDLE_ID_INCREMENT),
        }
    }

    /// Creates an unallocated handle of the given size.
    pub fn create_handle(&self, size: u64) -> Result<Arc<Handle>, NvResult> {
        if size == 0 {
            return Err(NvResult::BadValue);
        }

        let id = self
            .next_handle_id
            .fetch_add(HANDLE_ID_INCREMENT, Ordering::Relaxed);
        let handle = Arc::new(Handle::new(size, id));
        self.add_handle(handle.clone());
        Ok(handle)
    }

    pub fn get_handle(&self, handle_id: HandleId) -> Option<Arc<Handle>> {
        let handles = self.handles.lock().unwrap();
        handles.get(&handle_id).cloned()
    }

    pub fn get_handle_address(&self, handle_id: HandleId) -> u64 {
        let handles = self.handles.lock().unwrap();
        handles
            .get(&handle_id)
            .map(|h| {
                let inner = h.lock_inner();
                inner.d_address
            })
            .unwrap_or(0)
    }

    /// Maps a handle into the SMMU address space.
    /// This operation is refcounted, the number of calls to this must eventually match the
    /// number of calls to `unpin_handle`.
    ///
    /// Note: Full pin logic requires host1x SMMU integration which is not yet available.
    /// This implementation handles the refcounting and unmap queue management faithfully,
    /// but the actual SMMU mapping is stubbed.
    pub fn pin_handle(&self, handle_id: HandleId, _low_area_pin: bool) -> u64 {
        let handle = match self.get_handle(handle_id) {
            Some(h) => h,
            None => return 0,
        };

        let mut inner = handle.lock_inner();

        if inner.pins == 0 {
            // If we're in the unmap queue we can just remove ourselves and return since we're
            // already mapped
            {
                let mut unmap_queue = self.unmap_queue.lock().unwrap();
                // Check if this handle is in the unmap queue and remove it
                let mut found = false;
                let mut new_queue = LinkedList::new();
                while let Some(queued) = unmap_queue.pop_front() {
                    if queued.id == handle_id && !found {
                        found = true;
                        // Skip this entry - removing from queue
                    } else {
                        new_queue.push_back(queued);
                    }
                }
                *unmap_queue = new_queue;

                if found {
                    inner.pins += 1;
                    return inner.d_address;
                }
            }

            // Upstream allocates SMMU address space via host1x.Allocator() and maps
            // through host1x.GMMU().Map(). It handles two cases:
            //   1. low_area_pin: allocates from a small-page region via host1x.Allocator()
            //   2. normal pin: allocates from the big-page SMMU allocator
            // Both map the handle's backing memory into the GPU address space.
            // Until Host1x GMMU integration is available, use the CPU address as a
            // placeholder device address so that pinning still returns a valid address.
            if inner.d_address == 0 && inner.address != 0 {
                inner.d_address = inner.address;
            }
        }

        inner.pins += 1;
        inner.d_address
    }

    /// When this has been called an equal number of times to `pin_handle` for the supplied
    /// handle it will be added to a list of handles to be freed when necessary.
    pub fn unpin_handle(&self, handle_id: HandleId) {
        let handle = match self.get_handle(handle_id) {
            Some(h) => h,
            None => return,
        };

        let mut inner = handle.lock_inner();
        inner.pins -= 1;

        if inner.pins < 0 {
            log::warn!("Pin count imbalance detected!");
        } else if inner.pins == 0 {
            drop(inner);
            // Add to the unmap queue allowing this handle's memory to be freed if needed
            let mut unmap_queue = self.unmap_queue.lock().unwrap();
            unmap_queue.push_back(handle);
        }
    }

    /// Tries to duplicate a handle.
    pub fn duplicate_handle(&self, handle_id: HandleId, internal_session: bool) {
        let handle = match self.get_handle(handle_id) {
            Some(h) => h,
            None => {
                log::error!("Unregistered handle!");
                return;
            }
        };

        let result = handle.duplicate(internal_session);
        if result != NvResult::Success {
            log::error!("Could not duplicate handle!");
        }
    }

    /// Tries to free a handle and remove a single dupe.
    /// If a handle has no dupes left and has no other users a FreeInfo struct will be returned
    /// describing the prior state of the handle.
    pub fn free_handle(&self, handle_id: HandleId, internal_session: bool) -> Option<FreeInfo> {
        // We use a weak ptr here so we can tell when the handle has been freed and report that
        // back to guest
        let handle_weak: Weak<Handle>;
        let free_info: FreeInfo;

        {
            let handle = match self.get_handle(handle_id) {
                Some(h) => h,
                None => return None,
            };
            handle_weak = Arc::downgrade(&handle);

            let mut inner = handle.lock_inner();

            if internal_session {
                inner.internal_dupes -= 1;
                if inner.internal_dupes < 0 {
                    log::warn!("Internal duplicate count imbalance detected!");
                }
            } else {
                inner.dupes -= 1;
                if inner.dupes < 0 {
                    log::warn!("User duplicate count imbalance detected!");
                } else if inner.dupes == 0 {
                    // Force unmap the handle
                    if inner.d_address != 0 {
                        // In the C++ code, this calls UnmapHandle which unmaps from SMMU.
                        // We remove from unmap queue and clear d_address.
                        let mut unmap_queue = self.unmap_queue.lock().unwrap();
                        let mut new_queue = LinkedList::new();
                        while let Some(queued) = unmap_queue.pop_front() {
                            if queued.id != handle_id {
                                new_queue.push_back(queued);
                            }
                        }
                        *unmap_queue = new_queue;

                        // Upstream calls UnmapHandle which:
                        //   host1x.GMMU().Unmap(pin_virt_address, aligned_size)
                        //   host1x.Allocator().Free(pin_virt_address, aligned_size)
                        // Until Host1x GMMU integration is available, just clear the addresses.
                        inner.pin_virt_address = 0;
                        inner.d_address = 0;
                        inner.in_heap = false;
                    }

                    inner.pins = 0;
                }
            }

            // Try to remove the shared ptr to the handle from the map
            if inner.dupes == 0 && inner.internal_dupes == 0 {
                let mut handles = self.handles.lock().unwrap();
                handles.remove(&handle_id);
                log::debug!("Removed nvmap handle: {}", handle_id);
            } else {
                log::debug!(
                    "Tried to free nvmap handle: {} but didn't as it still has duplicates",
                    handle_id
                );
            }

            free_info = FreeInfo {
                address: inner.address,
                size: inner.size,
                was_uncached: inner.flags.map_uncached(),
                can_unlock: true,
            };
        }

        // If the handle hasn't been freed from memory, mark that
        if handle_weak.strong_count() > 0 {
            log::debug!(
                "nvmap handle: {} wasn't freed as it is still in use",
                handle_id
            );
            return Some(FreeInfo {
                can_unlock: false,
                ..free_info
            });
        }

        Some(free_info)
    }

    /// Unmaps all handles belonging to a given session.
    pub fn unmap_all_handles(&self, session_id: SessionId) {
        // Take a snapshot of handles to avoid holding the lock during free
        let handles_copy: Vec<(HandleId, Arc<Handle>)> = {
            let handles = self.handles.lock().unwrap();
            handles.iter().map(|(&id, h)| (id, h.clone())).collect()
        };

        for (id, handle) in handles_copy {
            let should_free = {
                let inner = handle.lock_inner();
                inner.session_id.id == session_id.id && inner.dupes > 0
            };
            if should_free {
                self.free_handle(id, false);
            }
        }
    }

    fn add_handle(&self, handle: Arc<Handle>) {
        let mut handles = self.handles.lock().unwrap();
        handles.insert(handle.id, handle);
    }
}
