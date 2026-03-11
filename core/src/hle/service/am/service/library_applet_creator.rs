// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/library_applet_creator.h
//! Port of zuyu/src/core/hle/service/am/service/library_applet_creator.cpp

/// IPC command table for ILibraryAppletCreator:
/// - 0: CreateLibraryApplet
/// - 10: CreateStorage
/// - 11: CreateTransferMemoryStorage
/// - 12: CreateHandleStorage
pub struct ILibraryAppletCreator {
    // TODO: WindowSystem reference, Applet reference
}

impl ILibraryAppletCreator {
    pub fn new() -> Self {
        Self {}
    }
}
