// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/os/multi_wait.h
//! Port of zuyu/src/core/hle/service/os/multi_wait.cpp
//!
//! MultiWait — waits on multiple synchronization objects.
//! Upstream wraps svcWaitSynchronization/KSynchronizationObject::Wait.

use super::multi_wait_holder::MultiWaitHolder;

/// MultiWait — manages a list of MultiWaitHolders to wait on.
///
/// Upstream stores an intrusive list of MultiWaitHolder and calls
/// svcWaitSynchronization on all their native handles.
pub struct MultiWait {
    pub(crate) holders: Vec<*mut MultiWaitHolder>,
}

// Safety: MultiWait holders are managed by the service layer on a single thread.
unsafe impl Send for MultiWait {}
unsafe impl Sync for MultiWait {}

impl MultiWait {
    pub fn new() -> Self {
        Self {
            holders: Vec::new(),
        }
    }

    /// Link a holder to this MultiWait.
    pub fn link_holder(&mut self, holder: *mut MultiWaitHolder) {
        unsafe {
            (*holder).link_to_multi_wait(self as *mut MultiWait);
        }
    }

    /// Unlink a holder from this MultiWait.
    pub fn unlink_holder(&mut self, holder: *mut MultiWaitHolder) {
        unsafe {
            (*holder).unlink_from_multi_wait();
        }
    }

    /// Port of upstream `MultiWait::MoveAll`.
    ///
    /// IMPORTANT: callers must NOT pass an `other` that was created via
    /// `std::mem::take()` from another `MultiWait` field. Each holder stores a
    /// raw pointer to the `MultiWait` it belongs to, and `mem::take` moves the
    /// holders Vec into a new MultiWait at a different memory location, which
    /// makes those raw pointers stale. `unlink_from_multi_wait` then no-ops
    /// (because it points to the old, now-empty location), and this loop spins
    /// forever. Always pass `&mut field` from the same struct as `self`.
    pub fn move_all(&mut self, other: &mut MultiWait) {
        while let Some(holder) = other.holders.first().copied() {
            unsafe {
                (*holder).unlink_from_multi_wait();
                (*holder).link_to_multi_wait(self as *mut MultiWait);
            }
        }
    }

    pub fn holders_snapshot(&self) -> Vec<*mut MultiWaitHolder> {
        self.holders.clone()
    }

    /// WaitAny — block until any holder is signaled, return the signaled holder.
    /// Port of upstream `MultiWait::WaitAny()`.
    /// Upstream calls svcWaitSynchronization on all holder handles.
    pub fn wait_any(&self) -> Option<*mut MultiWaitHolder> {
        // Check if any holder is already signaled.
        for &holder in &self.holders {
            unsafe {
                if (*holder).is_signaled() {
                    return Some(holder);
                }
            }
        }

        // If no holder is signaled, block-wait.
        // Upstream: svcWaitSynchronization(handles, count, timeout).
        // We poll with a short sleep to avoid busy-waiting.
        loop {
            let holders = self.holders_snapshot();
            for &holder in &holders {
                unsafe {
                    if (*holder).is_signaled() {
                        return Some(holder);
                    }
                }
            }
            std::thread::sleep(std::time::Duration::from_micros(100));
        }
    }

    /// TryWaitAny — non-blocking check, return the first signaled holder if any.
    /// Port of upstream `MultiWait::TryWaitAny()`.
    pub fn try_wait_any(&self) -> Option<*mut MultiWaitHolder> {
        for &holder in &self.holders {
            unsafe {
                if (*holder).is_signaled() {
                    return Some(holder);
                }
            }
        }
        None
    }
}

impl Default for MultiWait {
    fn default() -> Self {
        Self::new()
    }
}
