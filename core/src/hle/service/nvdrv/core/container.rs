// SPDX-FileCopyrightText: 2022 yuzu Emulator Project
// SPDX-FileCopyrightText: 2022 Skyline Team and Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/nvdrv/core/container.h
//! Port of zuyu/src/core/hle/service/nvdrv/core/container.cpp

use std::collections::VecDeque;
use std::sync::{Mutex, Weak};

use super::heap_mapper::HeapMapper;
use super::nvmap::NvMap;
use super::syncpoint_manager::SyncpointManager;
use crate::hle::kernel::k_process::KProcess;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct SessionId {
    pub id: usize,
}

pub struct Session {
    pub id: SessionId,
    /// Matches upstream `Kernel::KProcess* process`.
    pub process: Option<Weak<Mutex<KProcess>>>,
    /// Process identity used for session reuse. This mirrors the upstream
    /// pointer-identity comparison in a Rust-friendly form.
    pub process_identity: usize,
    pub has_preallocated_area: bool,
    pub mapper: Option<HeapMapper>,
    pub is_active: bool,
    pub ref_count: i32,
}

impl Session {
    pub fn new(id: SessionId, process: &std::sync::Arc<Mutex<KProcess>>) -> Self {
        let process_identity = std::sync::Arc::as_ptr(process) as usize;
        Self {
            id,
            process: Some(std::sync::Arc::downgrade(process)),
            process_identity,
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
    /// Matches upstream `Container::OpenSession(KProcess* process)` for the
    /// session reuse and ownership checks. ASID/SMMU registration and heap
    /// preallocation remain unimplemented here.
    pub fn open_session(&self, process: &std::sync::Arc<Mutex<KProcess>>) -> SessionId {
        let mut inner = self.inner.lock().unwrap();
        let process_identity = std::sync::Arc::as_ptr(process) as usize;

        for session in &mut inner.sessions {
            if !session.is_active {
                continue;
            }
            if session.process_identity == process_identity {
                session.ref_count += 1;
                return session.id;
            }
        }

        let new_id;
        if let Some(recycled_id) = inner.id_pool.pop_front() {
            new_id = recycled_id;
            if new_id < inner.sessions.len() {
                inner.sessions[new_id] = Session::new(SessionId { id: new_id }, process);
            }
        } else {
            new_id = inner.new_ids;
            inner.new_ids += 1;
            inner
                .sessions
                .push(Session::new(SessionId { id: new_id }, process));
        }

        let session = &mut inner.sessions[new_id];
        session.is_active = true;
        session.ref_count = 1;

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
            session.process = None;
            session.process_identity = 0;
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

    /// Returns the process owner for an active session.
    ///
    /// This is the Rust counterpart of upstream `GetSession(session_id)->process`.
    pub fn get_session_process(
        &self,
        session_id: SessionId,
    ) -> Option<std::sync::Arc<Mutex<KProcess>>> {
        let inner = self.inner.lock().unwrap();
        let session = inner.sessions.get(session_id.id)?;
        if !session.is_active {
            return None;
        }
        session.process.as_ref()?.upgrade()
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

#[cfg(test)]
mod tests {
    use std::sync::{Arc, Mutex};

    use super::Container;
    use crate::hle::kernel::k_process::KProcess;

    #[test]
    fn open_session_reuses_active_session_for_same_process() {
        let container = Container::new();
        let process = Arc::new(Mutex::new(KProcess::new()));

        let first = container.open_session(&process);
        let second = container.open_session(&process);

        assert_eq!(first, second);

        container.close_session(first);
        let third = container.open_session(&process);

        assert_eq!(third, first);
    }
}
