// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/os/multi_wait.h
//! Port of zuyu/src/core/hle/service/os/multi_wait.cpp
//!
//! MultiWait — waits on multiple synchronization objects.

use super::multi_wait_holder::MultiWaitHolder;

/// MultiWait — manages a list of MultiWaitHolders to wait on.
///
/// Corresponds to `MultiWait` in upstream multi_wait.h / multi_wait.cpp.
pub struct MultiWait {
    wait_list: Vec<*mut MultiWaitHolder>,
}

impl MultiWait {
    pub fn new() -> Self {
        Self {
            wait_list: Vec::new(),
        }
    }

    /// WaitAny — block until any holder is signaled.
    pub fn wait_any(&self) -> Option<*mut MultiWaitHolder> {
        // TODO: implement with kernel synchronization
        None
    }

    /// TryWaitAny — non-blocking check.
    pub fn try_wait_any(&self) -> Option<*mut MultiWaitHolder> {
        // TODO: implement with kernel synchronization
        None
    }
}
