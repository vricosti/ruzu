// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/lock_accessor.h
//! Port of zuyu/src/core/hle/service/am/service/lock_accessor.cpp

/// IPC command table for ILockAccessor:
/// - 1: TryLock
/// - 2: Unlock
/// - 3: GetEvent
/// - 4: IsLocked
pub struct ILockAccessor {
    pub is_locked: bool,
    // TODO: ServiceContext, Event, Mutex
}

impl ILockAccessor {
    pub fn new() -> Self {
        Self { is_locked: false }
    }
}
