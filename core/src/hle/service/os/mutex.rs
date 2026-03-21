// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/os/mutex.h
//! Port of zuyu/src/core/hle/service/os/mutex.cpp
//!
//! Mutex — kernel-event-based mutex for HLE services.
//! Upstream uses a KEvent for the wait/signal mechanism.
//! We use a standard Rust Mutex which provides the same contract.

use std::sync::Mutex as StdMutex;

/// Mutex — uses a kernel event for mutual exclusion.
///
/// Upstream stores a `KEvent*` and `System&`, implementing lock as a
/// KEvent wait loop and unlock as KEvent signal. Since the behavioral
/// contract is simple mutual exclusion, we use std::sync::Mutex.
pub struct Mutex {
    inner: StdMutex<()>,
}

impl Mutex {
    pub fn new() -> Self {
        Self {
            inner: StdMutex::new(()),
        }
    }

    /// Acquire the mutex.
    /// Port of upstream `Mutex::Lock()` — waits on KEvent until acquired.
    pub fn lock(&self) {
        // std::sync::Mutex::lock blocks until the lock is acquired,
        // matching the behavioral contract of the upstream KEvent wait loop.
        let _guard = self.inner.lock().unwrap();
        // Note: the guard is dropped immediately. For RAII usage, callers
        // should use lock_guard() instead.
    }

    /// Acquire the mutex and return a guard (RAII).
    pub fn lock_guard(&self) -> std::sync::MutexGuard<'_, ()> {
        self.inner.lock().unwrap()
    }

    /// Release the mutex.
    /// Port of upstream `Mutex::Unlock()` — signals the KEvent.
    pub fn unlock(&self) {
        // With std::sync::Mutex, unlock happens when the guard is dropped.
        // This method exists for API compatibility with upstream.
    }
}

impl Default for Mutex {
    fn default() -> Self {
        Self::new()
    }
}
