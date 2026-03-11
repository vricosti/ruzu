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

struct ContainerImpl {
    file: NvMap,
    manager: SyncpointManager,
    device_file_data: Host1xDeviceFileData,
    sessions: Vec<Session>,
    new_ids: usize,
    id_pool: VecDeque<usize>,
    session_guard: Mutex<()>,
}

/// Container manages syncpoints on the host and provides access to NvMap and SyncpointManager.
pub struct Container {
    file: NvMap,
    manager: SyncpointManager,
    device_file_data: Host1xDeviceFileData,
    sessions: Mutex<Vec<Session>>,
    new_ids: Mutex<usize>,
    id_pool: Mutex<VecDeque<usize>>,
}

impl Container {
    pub fn new() -> Self {
        Self {
            file: NvMap::new(),
            manager: SyncpointManager::new(),
            device_file_data: Host1xDeviceFileData::default(),
            sessions: Mutex::new(Vec::new()),
            new_ids: Mutex::new(0),
            id_pool: Mutex::new(VecDeque::new()),
        }
    }

    pub fn open_session(&self) -> SessionId {
        let mut sessions = self.sessions.lock().unwrap();
        let mut new_ids = self.new_ids.lock().unwrap();
        let mut id_pool = self.id_pool.lock().unwrap();

        let new_id;
        if let Some(recycled_id) = id_pool.pop_front() {
            new_id = recycled_id;
            if new_id < sessions.len() {
                sessions[new_id] = Session::new(SessionId { id: new_id });
            }
        } else {
            new_id = *new_ids;
            *new_ids += 1;
            sessions.push(Session::new(SessionId { id: new_id }));
        }

        let session = &mut sessions[new_id];
        session.is_active = true;
        session.ref_count = 1;

        SessionId { id: new_id }
    }

    pub fn close_session(&self, session_id: SessionId) {
        let mut sessions = self.sessions.lock().unwrap();
        let mut id_pool = self.id_pool.lock().unwrap();

        if session_id.id < sessions.len() {
            let session = &mut sessions[session_id.id];
            session.ref_count -= 1;
            if session.ref_count > 0 {
                return;
            }
            session.is_active = false;
            session.has_preallocated_area = false;
            session.mapper = None;
            id_pool.push_front(session_id.id);
        }

        drop(sessions);
        self.file.unmap_all_handles(session_id);
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
