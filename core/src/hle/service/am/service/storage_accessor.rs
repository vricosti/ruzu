// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/storage_accessor.h
//! Port of zuyu/src/core/hle/service/am/service/storage_accessor.cpp

/// IStorageAccessor service.
pub struct IStorageAccessor {
    // TODO: LibraryAppletStorage reference
}

impl IStorageAccessor {
    pub fn new() -> Self {
        Self {}
    }
}

/// ITransferStorageAccessor service.
pub struct ITransferStorageAccessor {
    // TODO: LibraryAppletStorage reference
}

impl ITransferStorageAccessor {
    pub fn new() -> Self {
        Self {}
    }
}
