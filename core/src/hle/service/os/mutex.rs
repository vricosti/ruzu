// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/os/mutex.h
//! Port of zuyu/src/core/hle/service/os/mutex.cpp
//!
//! Mutex — kernel-event-based mutex for HLE services.

/// Mutex — uses a kernel event for mutual exclusion.
///
/// Corresponds to `Mutex` in upstream mutex.h / mutex.cpp.
pub struct Mutex {
    // TODO: KEvent handle, System reference
}

impl Mutex {
    pub fn new() -> Self {
        Self {}
    }

    /// Acquire the mutex.
    pub fn lock(&self) {
        // TODO: implement with KEvent wait loop
    }

    /// Release the mutex.
    pub fn unlock(&self) {
        // TODO: implement with KEvent signal
    }
}
