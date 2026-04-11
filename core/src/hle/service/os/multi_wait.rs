// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/os/multi_wait.h
//! Port of zuyu/src/core/hle/service/os/multi_wait.cpp
//!
//! MultiWait — waits on multiple synchronization objects.
//! Upstream wraps svcWaitSynchronization/KSynchronizationObject::Wait.

use std::ffi::OsStr;

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
    fn boot_trace_enabled() -> bool {
        std::env::var_os("RUZU_APPLET_BOOT_TRACE").is_some_and(|value| value != OsStr::new("0"))
    }

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
    /// Rust move-aware adaptation of upstream intrusive-list splicing.
    ///
    /// Upstream can splice holders without repairing owner pointers because
    /// the intrusive node lives in a stable pointee. Rust service owners can
    /// move `MultiWait` values, so this method drains `other.holders`
    /// directly and rewrites each holder backlink instead of relying on the
    /// previous `holder.multi_wait` owner pointer still being valid.
    pub fn move_all(&mut self, other: &mut MultiWait) {
        let moved: Vec<*mut MultiWaitHolder> = other.holders.drain(..).collect();
        for holder in moved {
            unsafe {
                (*holder).reset_multi_wait_linkage_for_owner_move();
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

    /// Rust-only helper for host-side service threads that must avoid the
    /// kernel-backed wait path entirely.
    ///
    /// This performs the same non-blocking signaled scan as the local fallback
    /// path without consulting `KernelCore` or `WaitSynchronization`.
    pub fn try_wait_any_local(&self) -> Option<*mut MultiWaitHolder> {
        let holders = self.holders_snapshot();
        self.local_try_wait_any(&holders)
    }

    fn timed_wait_impl(
        &self,
        kernel: &KernelCore,
        timeout_ns: i64,
    ) -> Option<*mut MultiWaitHolder> {
        let trace_boot = Self::boot_trace_enabled();
        let holders = self.holders_snapshot();
        if holders.is_empty() {
            return None;
        }

        let current_thread = match kernel.get_current_emu_thread() {
            Some(thread) => thread,
            None => {
                if trace_boot {
                    log::info!("MultiWait::timed_wait_impl: falling back local (no current_emu_thread)");
                }
                return self.local_timed_wait(&holders, timeout_ns);
            }
        };
        let process = match current_thread
            .lock()
            .unwrap()
            .parent
            .as_ref()
            .and_then(|parent| parent.upgrade())
        {
            Some(process) => process,
            None => {
                if trace_boot {
                    log::info!("MultiWait::timed_wait_impl: falling back local (no process)");
                }
                return self.local_timed_wait(&holders, timeout_ns);
            }
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
            if trace_boot {
                log::info!("MultiWait::timed_wait_impl: falling back local (no scheduler)");
            }
            return self.local_timed_wait(&holders, timeout_ns);
        };

        let mut object_ids = Vec::with_capacity(holders.len());
        for holder in &holders {
            let Some(object_id) = (unsafe { &**holder }).object_id() else {
                if trace_boot {
                    log::info!(
                        "MultiWait::timed_wait_impl: falling back local (holder missing object_id)"
                    );
                }
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
        if let Some(holder) = self.local_try_wait_any(holders) {
            return Some(holder);
        }

        if timeout_ns == 0 {
            return None;
        }

        loop {
            if let Some(holder) = self.local_try_wait_any(holders) {
                return Some(holder);
            }
            std::thread::sleep(std::time::Duration::from_micros(100));
        }
    }

    fn local_try_wait_any(&self, holders: &[*mut MultiWaitHolder]) -> Option<*mut MultiWaitHolder> {
        for &holder in holders {
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
