// SPDX-FileCopyrightText: 2022 yuzu Emulator Project
// SPDX-FileCopyrightText: 2022 Skyline Team and Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/nvdrv/core/nvmap.h
//! Port of zuyu/src/core/hle/service/nvdrv/core/nvmap.cpp

use std::collections::HashMap;
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::{Arc, Mutex};

use crate::hle::service::nvdrv::core::container::SessionId;
use crate::hle::service::nvdrv::nvdata::NvResult;

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
pub struct Handle {
    pub align: u64,
    pub size: u64,
    pub aligned_size: u64,
    pub orig_size: u64,
    pub dupes: i32,
    pub internal_dupes: i32,
    pub id: HandleId,
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
    pub mutex: Mutex<()>,
}

impl Handle {
    pub fn new(size: u64, id: HandleId) -> Self {
        Self {
            align: 0,
            size,
            aligned_size: size,
            orig_size: size,
            dupes: 1,
            internal_dupes: 0,
            id,
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
            mutex: Mutex::new(()),
        }
    }

    pub fn alloc(
        &mut self,
        p_flags: HandleFlags,
        p_align: u32,
        p_kind: u8,
        p_address: u64,
        p_session_id: SessionId,
    ) -> NvResult {
        let _lock = self.mutex.lock().unwrap();
        if self.allocated {
            return NvResult::AccessDenied;
        }

        self.flags = p_flags;
        self.kind = p_kind;
        const PAGE_SIZE: u64 = 0x1000;
        self.align = if (p_align as u64) < PAGE_SIZE {
            PAGE_SIZE
        } else {
            p_align as u64
        };
        self.session_id = p_session_id;

        if p_address != 0 {
            self.flags.set_keep_uncached_after_free(false);
        } else {
            log::error!("Mapping nvmap handles without a CPU side address is unimplemented!");
        }

        self.size = (self.size + PAGE_SIZE - 1) & !(PAGE_SIZE - 1);
        self.aligned_size = (self.size + self.align - 1) & !(self.align - 1);
        self.address = p_address;
        self.allocated = true;

        NvResult::Success
    }

    pub fn duplicate(&mut self, internal_session: bool) -> NvResult {
        let _lock = self.mutex.lock().unwrap();
        if !self.allocated {
            return NvResult::BadValue;
        }
        if internal_session {
            self.internal_dupes += 1;
        } else {
            self.dupes += 1;
        }
        NvResult::Success
    }
}

/// Encapsulates the result of a FreeHandle operation.
pub struct FreeInfo {
    pub address: u64,
    pub size: u64,
    pub was_uncached: bool,
    pub can_unlock: bool,
}

const HANDLE_ID_INCREMENT: u32 = 4;

/// The nvmap core class holds the global state for nvmap and provides methods to manage handles.
pub struct NvMap {
    handles: Mutex<HashMap<HandleId, Arc<Handle>>>,
    next_handle_id: AtomicU32,
}

impl NvMap {
    pub fn new() -> Self {
        Self {
            handles: Mutex::new(HashMap::new()),
            next_handle_id: AtomicU32::new(HANDLE_ID_INCREMENT),
        }
    }

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
            .map(|h| h.d_address)
            .unwrap_or(0)
    }

    pub fn pin_handle(&self, handle_id: HandleId, _low_area_pin: bool) -> u64 {
        // Stubbed: Full pin logic requires host1x SMMU integration.
        // Returns d_address for now.
        if let Some(handle) = self.get_handle(handle_id) {
            handle.d_address
        } else {
            0
        }
    }

    pub fn unpin_handle(&self, handle_id: HandleId) {
        // Stubbed: Full unpin logic requires host1x SMMU integration.
        log::debug!("NvMap::unpin_handle({})", handle_id);
    }

    pub fn duplicate_handle(&self, handle_id: HandleId, internal_session: bool) {
        if let Some(_handle) = self.get_handle(handle_id) {
            // Would need &mut access to call handle.duplicate(). Stubbed.
            log::debug!(
                "NvMap::duplicate_handle({}, internal={})",
                handle_id,
                internal_session
            );
        } else {
            log::error!("NvMap::duplicate_handle: Unregistered handle!");
        }
    }

    pub fn free_handle(&self, handle_id: HandleId, _internal_session: bool) -> Option<FreeInfo> {
        // Stubbed: Full free logic requires reference counting with interior mutability.
        let handles = self.handles.lock().unwrap();
        if let Some(handle) = handles.get(&handle_id) {
            Some(FreeInfo {
                address: handle.address,
                size: handle.size,
                was_uncached: handle.flags.map_uncached(),
                can_unlock: true,
            })
        } else {
            None
        }
    }

    pub fn unmap_all_handles(&self, _session_id: SessionId) {
        // Stubbed
        log::debug!("NvMap::unmap_all_handles");
    }

    fn add_handle(&self, handle: Arc<Handle>) {
        let mut handles = self.handles.lock().unwrap();
        handles.insert(handle.id, handle);
    }
}
