// SPDX-FileCopyrightText: 2022 yuzu Emulator Project
// SPDX-FileCopyrightText: 2022 Skyline Team and Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/nvdrv/core/container.h
//! Port of zuyu/src/core/hle/service/nvdrv/core/container.cpp

use std::collections::VecDeque;
use std::sync::Mutex;

use super::heap_mapper::HeapMapper;
use super::nvmap::NvMap;
use super::syncpoint_manager::SyncpointManager;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct SessionId {
    pub id: usize,
}

pub struct Session {
    pub id: SessionId,
    /// In the C++ code, this holds a pointer to the KProcess.
    /// We omit this since we don't have kernel integration yet.
    pub has_preallocated_area: bool,
    pub mapper: Option<HeapMapper>,
    pub is_active: bool,
    pub ref_count: i32,
}

impl Session {
    pub fn new(id: SessionId) -> Self {
        Self {
            id,
            has_preallocated_area: false,
            mapper: None,
            is_active: false,
            ref_count: 0,
        }
    }
}

pub struct Host1xDeviceFileData {
    pub syncpts_accumulated: VecDeque<u32>,
}

impl Default for Host1xDeviceFileData {
    fn default() -> Self {
        Self {
            syncpts_accumulated: VecDeque::new(),
        }
    }
}

/// Inner state protected by a single mutex, matching the C++ ContainerImpl pattern.
struct ContainerInner {
    sessions: Vec<Session>,
    new_ids: usize,
    id_pool: VecDeque<usize>,
}

/// Container manages syncpoints on the host and provides access to NvMap and SyncpointManager.
pub struct Container {
    file: NvMap,
    manager: SyncpointManager,
    device_file_data: Host1xDeviceFileData,
    inner: Mutex<ContainerInner>,
}

impl Container {
    pub fn new() -> Self {
        Self {
            file: NvMap::new(),
            manager: SyncpointManager::new(),
            device_file_data: Host1xDeviceFileData::default(),
            inner: Mutex::new(ContainerInner {
                sessions: Vec::new(),
                new_ids: 0,
                id_pool: VecDeque::new(),
            }),
        }
    }

    /// Opens a session. If the same process already has an active session,
    /// increments its ref count and returns the existing session ID.
    ///
    /// In the C++ code, this takes a KProcess* and checks process identity.
    /// Since we don't have kernel integration yet, each call creates a new session.
    /// The heap preallocation optimization is also omitted (requires page table queries).
    pub fn open_session(&self) -> SessionId {
        let mut inner = self.inner.lock().unwrap();

        // In the C++ code, we'd check for an existing active session for the same process.
        // Without KProcess*, we always create a new session.

        let new_id;
        if let Some(recycled_id) = inner.id_pool.pop_front() {
            new_id = recycled_id;
            if new_id < inner.sessions.len() {
                inner.sessions[new_id] = Session::new(SessionId { id: new_id });
            }
        } else {
            new_id = inner.new_ids;
            inner.new_ids += 1;
            inner.sessions.push(Session::new(SessionId { id: new_id }));
        }

        let session = &mut inner.sessions[new_id];
        session.is_active = true;
        session.ref_count = 1;

        // NOTE: In the C++ code, if the process is an application, it performs heap preallocation
        // by scanning the process page table for the largest contiguous heap region and
        // preallocating SMMU space. This is omitted here as it requires kernel page table access.

        SessionId { id: new_id }
    }

    /// Closes a session by decrementing its ref count.
    /// When the ref count reaches zero, unmaps all handles and cleans up.
    pub fn close_session(&self, session_id: SessionId) {
        // First, handle ref counting and session cleanup under the lock
        let should_unmap = {
            let mut inner = self.inner.lock().unwrap();

            if session_id.id >= inner.sessions.len() {
                return;
            }

            let session = &mut inner.sessions[session_id.id];
            session.ref_count -= 1;
            if session.ref_count > 0 {
                return;
            }

            // Session is being fully closed
            session.is_active = false;
            session.has_preallocated_area = false;
            session.mapper = None;

            // In the C++ code:
            // smmu.UnregisterProcess(sessions[session_id.id].asid);
            inner.id_pool.push_front(session_id.id);

            true
        };

        // Unmap all handles outside the inner lock to avoid deadlock
        // (unmap_all_handles needs to lock NvMap's handles)
        if should_unmap {
            self.file.unmap_all_handles(session_id);
        }
    }

    /// Gets a reference to a session by ID.
    /// In the C++ code, this uses an atomic_thread_fence(acquire) before returning.
    pub fn get_session(&self, session_id: SessionId) -> Option<SessionId> {
        let inner = self.inner.lock().unwrap();
        if session_id.id < inner.sessions.len() && inner.sessions[session_id.id].is_active {
            Some(session_id)
        } else {
            None
        }
    }

    pub fn get_nv_map_file(&self) -> &NvMap {
        &self.file
    }

    pub fn get_syncpoint_manager(&self) -> &SyncpointManager {
        &self.manager
    }

    pub fn host1x_device_file(&self) -> &Host1xDeviceFileData {
        &self.device_file_data
    }
}
