// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/storage.h
//! Port of zuyu/src/core/hle/service/am/service/storage.cpp

/// IPC command table for IStorage:
/// - 0: Open
/// - 1: OpenTransferStorage
pub struct IStorage {
    pub data: Vec<u8>,
    // TODO: LibraryAppletStorage reference
}

impl IStorage {
    pub fn new(data: Vec<u8>) -> Self {
        Self { data }
    }

    pub fn get_data(&self) -> &[u8] {
        &self.data
    }
}
