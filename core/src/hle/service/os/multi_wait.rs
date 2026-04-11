// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/os/multi_wait.h
//! Port of zuyu/src/core/hle/service/os/multi_wait.cpp
//!
//! MultiWait — waits on multiple synchronization objects.
//! Upstream wraps svcWaitSynchronization/KSynchronizationObject::Wait.

use crate::hle::kernel::k_synchronization_object;
use crate::hle::kernel::kernel::KernelCore;
use crate::hle::result::RESULT_SUCCESS;

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
    pub fn wait_any(&self, kernel: &KernelCore) -> Option<*mut MultiWaitHolder> {
        self.timed_wait_impl(kernel, -1)
    }

    /// TryWaitAny — non-blocking check, return the first signaled holder if any.
    /// Port of upstream `MultiWait::TryWaitAny()`.
    pub fn try_wait_any(&self, kernel: &KernelCore) -> Option<*mut MultiWaitHolder> {
        self.timed_wait_impl(kernel, 0)
    }

    fn timed_wait_impl(
        &self,
        kernel: &KernelCore,
        timeout_ns: i64,
    ) -> Option<*mut MultiWaitHolder> {
        let holders = self.holders_snapshot();
        if holders.is_empty() {
            return None;
        }

        let current_thread = match kernel.get_current_emu_thread() {
            Some(thread) => thread,
            None => return self.local_timed_wait(&holders, timeout_ns),
        };
        let process = match current_thread
            .lock()
            .unwrap()
            .parent
            .as_ref()
            .and_then(|parent| parent.upgrade())
        {
            Some(process) => process,
            None => return self.local_timed_wait(&holders, timeout_ns),
        };
        let scheduler = kernel
            .current_scheduler()
            .cloned()
            .or_else(|| {
                current_thread
                    .lock()
                    .unwrap()
                    .scheduler
                    .as_ref()
                    .and_then(|scheduler| scheduler.upgrade())
            })
            .or_else(|| {
                process
                    .lock()
                    .unwrap()
                    .scheduler
                    .as_ref()
                    .and_then(|scheduler| scheduler.upgrade())
            });
        let Some(scheduler) = scheduler else {
            return self.local_timed_wait(&holders, timeout_ns);
        };

        let mut object_ids = Vec::with_capacity(holders.len());
        for holder in &holders {
            let Some(object_id) = (unsafe { &**holder }).object_id() else {
                return self.local_timed_wait(&holders, timeout_ns);
            };
            object_ids.push(object_id);
        }

        let mut out_index = -1;
        let result = k_synchronization_object::wait(
            &process,
            &current_thread,
            &scheduler,
            &mut out_index,
            object_ids,
            timeout_ns,
        );

        if result == RESULT_SUCCESS && out_index >= 0 {
            holders.get(out_index as usize).copied()
        } else {
            None
        }
    }

    fn local_timed_wait(
        &self,
        holders: &[*mut MultiWaitHolder],
        timeout_ns: i64,
    ) -> Option<*mut MultiWaitHolder> {
        for &holder in holders {
            unsafe {
                if (*holder).is_signaled() {
                    return Some(holder);
                }
            }
        }

        if timeout_ns == 0 {
            return None;
        }

        loop {
            for &holder in holders {
                unsafe {
                    if (*holder).is_signaled() {
                        return Some(holder);
                    }
                }
            }
            std::thread::sleep(std::time::Duration::from_micros(100));
        }
    }
}

impl Default for MultiWait {
    fn default() -> Self {
        Self::new()
    }
}
