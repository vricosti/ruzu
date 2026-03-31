// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of video_core/fence_manager.h
//!
//! GPU fence/barrier management for synchronizing GPU and CPU operations.

use std::collections::VecDeque;
use std::sync::Mutex;

use common::settings;

use crate::delayed_destruction_ring::DelayedDestructionRing;

/// Base trait for fence objects.
pub trait FenceBase {
    /// Returns true if this fence is stubbed (no actual GPU wait needed).
    fn is_stubbed(&self) -> bool;
}

/// Generic fence manager that coordinates fence lifecycle, async flushes,
/// and deferred operations.
///
/// The upstream C++ uses CRTP templates; here we use a trait-based approach.
pub struct FenceManager<F: FenceBase> {
    fences: VecDeque<F>,
    uncommitted_operations: VecDeque<Box<dyn FnOnce() + Send>>,
    pending_operations: VecDeque<VecDeque<Box<dyn FnOnce() + Send>>>,
    guard: Mutex<()>,
    ring_guard: Mutex<()>,
    delayed_destruction_ring: DelayedDestructionRing<F, 8>,
}

impl<F: FenceBase> FenceManager<F> {
    pub fn new() -> Self {
        Self {
            fences: VecDeque::new(),
            uncommitted_operations: VecDeque::new(),
            pending_operations: VecDeque::new(),
            guard: Mutex::new(()),
            ring_guard: Mutex::new(()),
            delayed_destruction_ring: DelayedDestructionRing::new(),
        }
    }

    /// Notify the fence manager about a new frame.
    pub fn tick_frame(&mut self) {
        let _lock = self.ring_guard.lock().unwrap();
        self.delayed_destruction_ring.tick();
    }

    /// Queue a sync operation to execute when the next fence is signaled.
    pub fn sync_operation(&mut self, func: Box<dyn FnOnce() + Send>) {
        self.uncommitted_operations.push_back(func);
    }

    /// Signal a fence with an associated callback.
    ///
    /// Port of `FenceManager::SignalFence()`.
    ///
    /// In the full implementation, this:
    /// 1. Tries to release pending fences
    /// 2. Commits async flushes from texture/buffer/query caches
    /// 3. Creates a new backend fence
    /// 4. Moves uncommitted operations into the pending queue
    /// 5. Queues the fence into the backend
    /// 6. Optionally delays the callback for high GPU accuracy
    pub fn signal_fence<FC, FQ, FSW, FIS, FPF, FSHF, FCAF>(
        &mut self,
        func: Box<dyn FnOnce() + Send>,
        mut create_fence: FC,
        mut queue_fence: FQ,
        mut should_wait_async_flushes: FSW,
        mut is_fence_signaled: FIS,
        mut pop_async_flushes: FPF,
        mut should_flush: FSHF,
        mut commit_async_flushes: FCAF,
    ) -> bool
    where
        FC: FnMut(bool) -> F,
        FQ: FnMut(&mut F),
        FSW: FnMut() -> bool,
        FIS: FnMut(&F) -> bool,
        FPF: FnMut(),
        FSHF: FnMut() -> bool,
        FCAF: FnMut(),
    {
        let delay_fence = settings::is_gpu_level_high(&settings::values());
        let mut func = Some(func);
        self.try_release_pending_fences(false, |fence, force_wait| {
            if should_wait_async_flushes() {
                if force_wait {
                    pop_async_flushes();
                    return true;
                }
                return false;
            }
            let signaled = fence.is_stubbed() || is_fence_signaled(fence);
            if signaled {
                pop_async_flushes();
            }
            signaled
        });

        let should_flush = should_flush();
        commit_async_flushes();
        let new_fence = create_fence(!should_flush);
        if delay_fence {
            self.uncommitted_operations
                .push_back(func.take().expect("fence callback consumed once"));
        }
        let batch = std::mem::take(&mut self.uncommitted_operations);
        self.pending_operations.push_back(batch);

        let mut new_fence = new_fence;
        queue_fence(&mut new_fence);
        if !delay_fence {
            func.take().expect("fence callback consumed once")();
        }
        self.fences.push_back(new_fence);
        should_flush
    }

    /// Signal a syncpoint increment.
    ///
    /// Port of `FenceManager::SignalSyncPoint()`.
    ///
    /// In the full implementation, this increments the guest syncpoint
    /// and creates a fence whose callback increments the host syncpoint.
    pub fn signal_sync_point<FG, FH, FC, FQ, FSW, FIS, FPF, FSHF, FCAF>(
        &mut self,
        value: u32,
        mut increment_guest: FG,
        mut increment_host: FH,
        create_fence: FC,
        queue_fence: FQ,
        should_wait_async_flushes: FSW,
        is_fence_signaled: FIS,
        pop_async_flushes: FPF,
        should_flush: FSHF,
        commit_async_flushes: FCAF,
    ) -> bool
    where
        FG: FnMut(u32),
        FH: FnMut(u32) + Send + 'static,
        FC: FnMut(bool) -> F,
        FQ: FnMut(&mut F),
        FSW: FnMut() -> bool,
        FIS: FnMut(&F) -> bool,
        FPF: FnMut(),
        FSHF: FnMut() -> bool,
        FCAF: FnMut(),
    {
        increment_guest(value);
        self.signal_fence(
            Box::new(move || increment_host(value)),
            create_fence,
            queue_fence,
            should_wait_async_flushes,
            is_fence_signaled,
            pop_async_flushes,
            should_flush,
            commit_async_flushes,
        )
    }

    /// Wait for all pending fences to complete.
    ///
    /// Port of `FenceManager::WaitPendingFences()`.
    pub fn wait_pending_fences<FS, FW, FSW, FPF>(
        &mut self,
        force: bool,
        mut should_wait_async_flushes: FSW,
        mut is_fence_signaled: FS,
        mut wait_fence: FW,
        mut pop_async_flushes: FPF,
    ) where
        FSW: FnMut() -> bool,
        FS: FnMut(&F) -> bool,
        FW: FnMut(&F),
        FPF: FnMut(),
    {
        if !force {
            self.try_release_pending_fences(false, |fence, should_wait| {
                if should_wait_async_flushes() {
                    if should_wait {
                        wait_fence(fence);
                        pop_async_flushes();
                        return true;
                    }
                    return false;
                }
                let signaled = fence.is_stubbed() || is_fence_signaled(fence);
                if signaled {
                    pop_async_flushes();
                }
                signaled
            });
            return;
        }
        self.try_release_pending_fences(true, |fence, _| {
            if should_wait_async_flushes() {
                wait_fence(fence);
                pop_async_flushes();
                return true;
            }
            wait_fence(fence);
            true
        });
    }

    /// Signal ordering (accumulate flushes).
    ///
    /// Port of `FenceManager::SignalOrdering()`.
    ///
    /// In the full implementation, this tries to release pending fences and
    /// calls buffer_cache.AccumulateFlushes().
    pub fn signal_ordering<FS, FSW, FPF, FAF>(
        &mut self,
        mut should_wait_async_flushes: FSW,
        mut is_fence_signaled: FS,
        mut pop_async_flushes: FPF,
        mut accumulate_flushes: FAF,
    ) where
        FSW: FnMut() -> bool,
        FS: FnMut(&F) -> bool,
        FPF: FnMut(),
        FAF: FnMut(),
    {
        self.try_release_pending_fences(false, |fence, should_wait| {
            if should_wait_async_flushes() {
                if should_wait {
                    pop_async_flushes();
                    return true;
                }
                return false;
            }
            let signaled = fence.is_stubbed() || is_fence_signaled(fence);
            if signaled {
                pop_async_flushes();
            }
            signaled
        });
        accumulate_flushes();
    }

    /// Try to release pending fences that have been signaled.
    ///
    /// Port of `FenceManager::TryReleasePendingFences()`.
    fn try_release_pending_fences<FW>(&mut self, force_wait: bool, mut fence_waiter: FW)
    where
        FW: FnMut(&F, bool) -> bool,
    {
        while !self.fences.is_empty() {
            let should_release = {
                let current_fence = self.fences.front().unwrap();
                if current_fence.is_stubbed() {
                    true
                } else {
                    fence_waiter(current_fence, force_wait)
                }
            };
            if !should_release {
                return;
            }
            let current_fence = self.fences.pop_front().unwrap();

            if let Some(operations) = self.pending_operations.pop_front() {
                for op in operations {
                    op();
                }
            }

            let _lock = self.ring_guard.lock().unwrap();
            self.delayed_destruction_ring.push(current_fence);
        }
    }

    #[cfg(test)]
    pub(crate) fn queued_fence_count(&self) -> usize {
        self.fences.len()
    }

    #[cfg(test)]
    pub(crate) fn pending_operation_batch_count(&self) -> usize {
        self.pending_operations.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use common::settings;
    use common::settings_enums::GpuAccuracy;
    use std::sync::{
        atomic::{AtomicBool, AtomicU32, Ordering},
        Arc,
    };

    #[derive(Default)]
    struct TestFence {
        stubbed: bool,
    }

    impl FenceBase for TestFence {
        fn is_stubbed(&self) -> bool {
            self.stubbed
        }
    }

    #[test]
    fn signal_ordering_accumulates_flushes_after_releasing_signaled_fences() {
        let mut manager = FenceManager::<TestFence>::new();
        let released = Arc::new(AtomicBool::new(false));
        let accumulated = Arc::new(AtomicBool::new(false));
        let popped = Arc::new(AtomicBool::new(false));

        manager.fences.push_back(TestFence { stubbed: false });
        manager
            .pending_operations
            .push_back(VecDeque::from([Box::new({
                let released = Arc::clone(&released);
                move || {
                    released.store(true, Ordering::Relaxed);
                }
            }) as Box<dyn FnOnce() + Send>]));

        manager.signal_ordering(
            || false,
            |_| true,
            {
                let popped = Arc::clone(&popped);
                move || popped.store(true, Ordering::Relaxed)
            },
            {
                let accumulated = Arc::clone(&accumulated);
                move || accumulated.store(true, Ordering::Relaxed)
            },
        );

        assert!(released.load(Ordering::Relaxed));
        assert!(popped.load(Ordering::Relaxed));
        assert!(accumulated.load(Ordering::Relaxed));
        assert_eq!(manager.queued_fence_count(), 0);
    }

    #[test]
    fn signal_fence_commits_flush_and_invalidate_in_upstream_order() {
        let previous_gpu_accuracy = {
            let mut values = settings::values_mut();
            let previous = values.current_gpu_accuracy;
            values.current_gpu_accuracy = GpuAccuracy::Normal;
            previous
        };

        let mut manager = FenceManager::<TestFence>::new();
        let committed = Arc::new(AtomicBool::new(false));
        let flushed = Arc::new(AtomicBool::new(false));
        let invalidated = Arc::new(AtomicBool::new(false));
        let callback_hit = Arc::new(AtomicBool::new(false));

        manager.signal_fence(
            Box::new({
                let callback_hit = Arc::clone(&callback_hit);
                move || callback_hit.store(true, Ordering::Relaxed)
            }),
            |is_stubbed| TestFence {
                stubbed: is_stubbed,
            },
            |_| {},
            || false,
            |_| true,
            || {},
            || true,
            {
                let committed = Arc::clone(&committed);
                move || committed.store(true, Ordering::Relaxed)
            },
        );

        assert!(committed.load(Ordering::Relaxed));
        assert!(callback_hit.load(Ordering::Relaxed));
        assert_eq!(manager.queued_fence_count(), 1);
        assert!(!flushed.load(Ordering::Relaxed));
        assert!(!invalidated.load(Ordering::Relaxed));

        settings::values_mut().current_gpu_accuracy = previous_gpu_accuracy;
    }

    #[test]
    fn signal_sync_point_increments_guest_then_host() {
        let previous_gpu_accuracy = {
            let mut values = settings::values_mut();
            let previous = values.current_gpu_accuracy;
            values.current_gpu_accuracy = GpuAccuracy::Normal;
            previous
        };

        let mut manager = FenceManager::<TestFence>::new();
        let guest = Arc::new(AtomicU32::new(0));
        let host = Arc::new(AtomicU32::new(0));

        manager.signal_sync_point(
            7,
            {
                let guest = Arc::clone(&guest);
                move |value| guest.store(value, Ordering::Relaxed)
            },
            {
                let host = Arc::clone(&host);
                move |value| host.store(value, Ordering::Relaxed)
            },
            |is_stubbed| TestFence {
                stubbed: is_stubbed,
            },
            |_| {},
            || false,
            |_| true,
            || {},
            || false,
            || {},
        );

        assert_eq!(guest.load(Ordering::Relaxed), 7);
        assert_eq!(host.load(Ordering::Relaxed), 7);

        settings::values_mut().current_gpu_accuracy = previous_gpu_accuracy;
    }
}
