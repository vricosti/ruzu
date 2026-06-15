// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of `video_core/fence_manager.h`.

use std::collections::VecDeque;
use std::sync::{
    atomic::{AtomicBool, AtomicU64, Ordering},
    Arc, Condvar, Mutex,
};
use std::thread::{self, JoinHandle};

use common::settings;

use crate::delayed_destruction_ring::DelayedDestructionRing;

type Operation = Box<dyn FnOnce() + Send + 'static>;
static FENCE_TRACE_ID: AtomicU64 = AtomicU64::new(1);

/// Base trait for fence objects.
pub trait FenceBase {
    /// Returns true if this fence is stubbed (no actual GPU wait needed).
    fn is_stubbed(&self) -> bool;

    /// Wait until the backend fence completes.
    fn wait_for_fence(&self);
}

struct PendingFence<F: FenceBase + Send + 'static> {
    trace_id: u64,
    fence: F,
    pre_operations: VecDeque<Operation>,
    operations: VecDeque<Operation>,
}

struct FenceManagerState<F: FenceBase + Send + 'static> {
    fences: VecDeque<PendingFence<F>>,
    uncommitted_operations: VecDeque<Operation>,
}

struct FenceManagerShared<F: FenceBase + Send + 'static> {
    state: Mutex<FenceManagerState<F>>,
    ring_guard: Mutex<DelayedDestructionRing<F, 8>>,
    cv: Condvar,
    stop_requested: AtomicBool,
}

/// Generic fence manager that coordinates fence lifecycle, async flushes,
/// and deferred operations.
///
/// The upstream C++ uses CRTP templates; here we use a generic owner with
/// callback parameters supplied by the owning rasterizer.
pub struct FenceManager<F: FenceBase + Send + 'static> {
    shared: Arc<FenceManagerShared<F>>,
    has_async_check: bool,
    fence_thread: Option<JoinHandle<()>>,
}

impl<F: FenceBase + Send + 'static> FenceManager<F> {
    pub fn new(has_async_check: bool) -> Self {
        let shared = Arc::new(FenceManagerShared {
            state: Mutex::new(FenceManagerState {
                fences: VecDeque::new(),
                uncommitted_operations: VecDeque::new(),
            }),
            ring_guard: Mutex::new(DelayedDestructionRing::new()),
            cv: Condvar::new(),
            stop_requested: AtomicBool::new(false),
        });
        let fence_thread = if has_async_check {
            let shared = Arc::clone(&shared);
            Some(
                thread::Builder::new()
                    .name("GPUFencingThread".to_string())
                    .spawn(move || release_thread_func(shared))
                    .expect("failed to spawn GPUFencingThread"),
            )
        } else {
            None
        };
        Self {
            shared,
            has_async_check,
            fence_thread,
        }
    }

    /// Notify the fence manager about a new frame.
    pub fn tick_frame(&mut self) {
        let mut ring = self.shared.ring_guard.lock().unwrap();
        ring.tick();
    }

    /// Port of `FenceManager::SignalOrdering()`.
    pub fn signal_ordering<FS, FSW, FPF, FAF>(
        &mut self,
        mut should_wait_async_flushes: FSW,
        mut is_fence_signaled: FS,
        mut pop_async_flushes: FPF,
        mut accumulate_flushes: FAF,
    ) where
        FSW: FnMut() -> bool,
        FS: FnMut(&F) -> bool,
        FPF: FnMut() + Send + 'static,
        FAF: FnMut(),
    {
        if !self.has_async_check {
            self.try_release_pending_fences(false, |pending_fence, force_wait| {
                try_release_fence(
                    pending_fence,
                    force_wait,
                    &mut should_wait_async_flushes,
                    &mut is_fence_signaled,
                    None::<&mut fn(&F)>,
                    &mut pop_async_flushes,
                )
            });
        }
        accumulate_flushes();
    }

    /// Port of `FenceManager::SignalReference()`.
    pub fn signal_reference<FC, FQ, FSW, FIS, FPF, FSHF, FCAF, FFL, FINV>(
        &mut self,
        create_fence: FC,
        queue_fence: FQ,
        should_wait_async_flushes: FSW,
        is_fence_signaled: FIS,
        pop_async_flushes: FPF,
        should_flush: FSHF,
        commit_async_flushes: FCAF,
        flush_commands: FFL,
        invalidate_gpu_cache: FINV,
    ) -> bool
    where
        FC: FnMut(bool) -> F,
        FQ: FnMut(&mut F),
        FSW: FnMut() -> bool,
        FIS: FnMut(&F) -> bool,
        FPF: FnMut() + Send + 'static,
        FSHF: FnMut() -> bool,
        FCAF: FnMut(),
        FFL: FnMut(),
        FINV: FnMut(),
    {
        let trace_id = next_fence_trace_id();
        trace_fence_flow(trace_id, "signal_reference");
        self.signal_fence(
            Box::new(|| {}),
            create_fence,
            queue_fence,
            should_wait_async_flushes,
            is_fence_signaled,
            pop_async_flushes,
            should_flush,
            commit_async_flushes,
            flush_commands,
            invalidate_gpu_cache,
        )
    }

    /// Port of `FenceManager::SyncOperation()`.
    pub fn sync_operation(&mut self, func: Operation) {
        let mut state = self.shared.state.lock().unwrap();
        state.uncommitted_operations.push_back(func);
    }

    /// Port of `FenceManager::SignalFence()`.
    pub fn signal_fence<FC, FQ, FSW, FIS, FPF, FSHF, FCAF, FFL, FINV>(
        &mut self,
        func: Operation,
        mut create_fence: FC,
        mut queue_fence: FQ,
        mut should_wait_async_flushes: FSW,
        mut is_fence_signaled: FIS,
        mut pop_async_flushes: FPF,
        mut should_flush: FSHF,
        mut commit_async_flushes: FCAF,
        mut flush_commands: FFL,
        mut invalidate_gpu_cache: FINV,
    ) -> bool
    where
        FC: FnMut(bool) -> F,
        FQ: FnMut(&mut F),
        FSW: FnMut() -> bool,
        FIS: FnMut(&F) -> bool,
        FPF: FnMut() + Send + 'static,
        FSHF: FnMut() -> bool,
        FCAF: FnMut(),
        FFL: FnMut(),
        FINV: FnMut(),
    {
        let trace_id = next_fence_trace_id();
        trace_fence_flow(trace_id, "signal_fence begin");
        let delay_fence = settings::is_gpu_level_high(&settings::values());

        if !self.has_async_check {
            self.try_release_pending_fences(false, |pending_fence, force_wait| {
                try_release_fence(
                    pending_fence,
                    force_wait,
                    &mut should_wait_async_flushes,
                    &mut is_fence_signaled,
                    None::<&mut fn(&F)>,
                    &mut pop_async_flushes,
                )
            });
        }

        let should_flush_now = should_flush();
        trace_fence_flow(
            trace_id,
            if should_flush_now {
                "signal_fence should_flush=true"
            } else {
                "signal_fence should_flush=false"
            },
        );
        commit_async_flushes();
        trace_fence_flow(trace_id, "signal_fence commit_async_flushes end");

        let mut new_fence = create_fence(!should_flush_now);
        trace_fence_flow(trace_id, "signal_fence create_fence end");
        let mut maybe_func = Some(func);
        let operations = {
            let mut state = self.shared.state.lock().unwrap();
            if delay_fence {
                state.uncommitted_operations.push_back(
                    maybe_func
                        .take()
                        .expect("fence callback must be consumed once"),
                );
            }
            std::mem::take(&mut state.uncommitted_operations)
        };

        let mut pre_operations = VecDeque::new();
        if self.has_async_check {
            pre_operations.push_back(Box::new(move || pop_async_flushes()) as Operation);
        }

        {
            let mut state = self.shared.state.lock().unwrap();
            trace_fence_flow(trace_id, "signal_fence queue_fence begin");
            queue_fence(&mut new_fence);
            trace_fence_flow(trace_id, "signal_fence queue_fence end");
            if !delay_fence {
                maybe_func
                    .take()
                    .expect("fence callback must be consumed once")();
                trace_fence_flow(trace_id, "signal_fence callback end");
            }
            state.fences.push_back(PendingFence {
                trace_id,
                fence: new_fence,
                pre_operations,
                operations,
            });
            if should_flush_now {
                trace_fence_flow(trace_id, "signal_fence flush_commands begin");
                flush_commands();
                trace_fence_flow(trace_id, "signal_fence flush_commands end");
            }
        }
        if self.has_async_check {
            self.shared.cv.notify_all();
        }
        trace_fence_flow(trace_id, "signal_fence invalidate_gpu_cache begin");
        invalidate_gpu_cache();
        trace_fence_flow(trace_id, "signal_fence invalidate_gpu_cache end");

        should_flush_now
    }

    /// Port of `FenceManager::SignalSyncPoint()`.
    pub fn signal_sync_point<FG, FH, FC, FQ, FSW, FIS, FPF, FSHF, FCAF, FFL, FINV>(
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
        flush_commands: FFL,
        invalidate_gpu_cache: FINV,
    ) -> bool
    where
        FG: FnMut(u32),
        FH: FnMut(u32) + Send + 'static,
        FC: FnMut(bool) -> F,
        FQ: FnMut(&mut F),
        FSW: FnMut() -> bool,
        FIS: FnMut(&F) -> bool,
        FPF: FnMut() + Send + 'static,
        FSHF: FnMut() -> bool,
        FCAF: FnMut(),
        FFL: FnMut(),
        FINV: FnMut(),
    {
        let trace_id = next_fence_trace_id();
        trace_fence_flow(trace_id, "signal_sync_point begin");
        increment_guest(value);
        trace_fence_flow(trace_id, "signal_sync_point increment_guest end");
        self.signal_fence(
            Box::new(move || increment_host(value)),
            create_fence,
            queue_fence,
            should_wait_async_flushes,
            is_fence_signaled,
            pop_async_flushes,
            should_flush,
            commit_async_flushes,
            flush_commands,
            invalidate_gpu_cache,
        )
    }

    /// Port of `FenceManager::WaitPendingFences()`.
    pub fn wait_pending_fences<FC, FQ, FS, FW, FSW, FPF, FSHF, FCAF, FFL, FINV>(
        &mut self,
        force: bool,
        create_fence: FC,
        queue_fence: FQ,
        mut should_wait_async_flushes: FSW,
        mut is_fence_signaled: FS,
        mut wait_fence: FW,
        mut pop_async_flushes: FPF,
        should_flush: FSHF,
        commit_async_flushes: FCAF,
        flush_commands: FFL,
        invalidate_gpu_cache: FINV,
    ) where
        FC: FnMut(bool) -> F,
        FQ: FnMut(&mut F),
        FS: FnMut(&F) -> bool,
        FW: FnMut(&F),
        FSW: FnMut() -> bool,
        FPF: FnMut() + Send + 'static,
        FSHF: FnMut() -> bool,
        FCAF: FnMut(),
        FFL: FnMut(),
        FINV: FnMut(),
    {
        if !self.has_async_check {
            self.try_release_pending_fences(force, |pending_fence, force_wait| {
                try_release_fence(
                    pending_fence,
                    force_wait,
                    &mut should_wait_async_flushes,
                    &mut is_fence_signaled,
                    Some(&mut wait_fence),
                    &mut pop_async_flushes,
                )
            });
            return;
        }

        if !force {
            return;
        }

        let wait_pair = Arc::new((Mutex::new(false), Condvar::new()));
        let wait_pair_for_callback = Arc::clone(&wait_pair);
        let trace_id = next_fence_trace_id();
        trace_fence_flow(trace_id, "wait_pending_fences force drain begin");
        self.signal_fence(
            Box::new(move || {
                let (lock, cv) = &*wait_pair_for_callback;
                let mut finished = lock.lock().unwrap();
                *finished = true;
                cv.notify_all();
            }),
            create_fence,
            queue_fence,
            should_wait_async_flushes,
            is_fence_signaled,
            pop_async_flushes,
            should_flush,
            commit_async_flushes,
            flush_commands,
            invalidate_gpu_cache,
        );

        let (lock, cv) = &*wait_pair;
        let mut finished = lock.lock().unwrap();
        while !*finished {
            finished = cv.wait(finished).unwrap();
        }
    }

    #[cfg(test)]
    pub(crate) fn queued_fence_count(&self) -> usize {
        self.shared.state.lock().unwrap().fences.len()
    }

    #[cfg(test)]
    pub(crate) fn pending_operation_batch_count(&self) -> usize {
        let state = self.shared.state.lock().unwrap();
        let queued_operations = usize::from(!state.uncommitted_operations.is_empty());
        state.fences.len() + queued_operations
    }
}

impl<F: FenceBase + Send + 'static> Drop for FenceManager<F> {
    fn drop(&mut self) {
        if let Some(fence_thread) = self.fence_thread.take() {
            self.shared.stop_requested.store(true, Ordering::Relaxed);
            self.shared.cv.notify_all();
            let _ = fence_thread.join();
        }
    }
}

impl<F: FenceBase + Send + 'static> FenceManager<F> {
    fn try_release_pending_fences<FR>(&mut self, force_wait: bool, mut should_release: FR)
    where
        FR: FnMut(&PendingFence<F>, bool) -> bool,
    {
        loop {
            let should_pop = {
                let state = self.shared.state.lock().unwrap();
                let Some(pending_fence) = state.fences.front() else {
                    return;
                };
                should_release(pending_fence, force_wait)
            };
            if !should_pop {
                return;
            }

            let pending_fence = {
                let mut state = self.shared.state.lock().unwrap();
                state
                    .fences
                    .pop_front()
                    .expect("pending fence must exist while releasing")
            };
            trace_fence_flow(pending_fence.trace_id, "release_pending popped");
            for operation in pending_fence.pre_operations {
                operation();
            }
            for operation in pending_fence.operations {
                operation();
            }
            trace_fence_flow(pending_fence.trace_id, "release_pending operations end");
            let mut ring = self.shared.ring_guard.lock().unwrap();
            ring.push(pending_fence.fence);
        }
    }
}

fn next_fence_trace_id() -> u64 {
    FENCE_TRACE_ID.fetch_add(1, Ordering::Relaxed)
}

fn trace_fence_flow(trace_id: u64, stage: &str) {
    if std::env::var_os("RUZU_TRACE_GL_FENCE_FLOW").is_some() {
        log::info!("[GL_FENCE_FLOW] id={} {}", trace_id, stage);
    }
}

fn try_release_fence<F, FSW, FIS, FW, FPF>(
    pending_fence: &PendingFence<F>,
    force_wait: bool,
    should_wait_async_flushes: &mut FSW,
    is_fence_signaled: &mut FIS,
    mut wait_fence: Option<&mut FW>,
    pop_async_flushes: &mut FPF,
) -> bool
where
    F: FenceBase + Send + 'static,
    FSW: FnMut() -> bool,
    FIS: FnMut(&F) -> bool,
    FW: FnMut(&F),
    FPF: FnMut(),
{
    if should_wait_async_flushes() && !is_fence_signaled(&pending_fence.fence) {
        if force_wait {
            if let Some(wait_fence) = wait_fence.as_mut() {
                wait_fence(&pending_fence.fence);
            }
        } else {
            return false;
        }
    }
    pop_async_flushes();
    true
}

fn release_thread_func<F: FenceBase + Send + 'static>(shared: Arc<FenceManagerShared<F>>) {
    loop {
        let mut state = shared.state.lock().unwrap();
        while !shared.stop_requested.load(Ordering::Relaxed) && state.fences.is_empty() {
            state = shared.cv.wait(state).unwrap();
        }
        if shared.stop_requested.load(Ordering::Relaxed) {
            return;
        }
        let pending_fence = state
            .fences
            .pop_front()
            .expect("fence queue must contain the signaled fence");
        drop(state);
        trace_fence_flow(pending_fence.trace_id, "release_thread popped");

        if !pending_fence.fence.is_stubbed() {
            pending_fence.fence.wait_for_fence();
        }
        for operation in pending_fence.pre_operations {
            operation();
        }
        for operation in pending_fence.operations {
            operation();
        }
        trace_fence_flow(pending_fence.trace_id, "release_thread operations end");
        let mut ring = shared.ring_guard.lock().unwrap();
        ring.push(pending_fence.fence);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use common::settings_enums::GpuAccuracy;
    use std::sync::atomic::{AtomicBool, AtomicU32, Ordering};
    use std::time::{Duration, Instant};

    #[derive(Default)]
    struct TestFence {
        stubbed: bool,
    }

    impl FenceBase for TestFence {
        fn is_stubbed(&self) -> bool {
            self.stubbed
        }

        fn wait_for_fence(&self) {}
    }

    fn wait_until(flag: &AtomicBool) {
        let deadline = Instant::now() + Duration::from_secs(1);
        while !flag.load(Ordering::Relaxed) {
            assert!(
                Instant::now() < deadline,
                "timed out waiting for async callback"
            );
            std::thread::sleep(Duration::from_millis(5));
        }
    }

    #[test]
    fn signal_ordering_accumulates_flushes_after_releasing_signaled_fences() {
        let mut manager = FenceManager::<TestFence>::new(false);
        let released = Arc::new(AtomicBool::new(false));
        let accumulated = Arc::new(AtomicBool::new(false));
        let popped = Arc::new(AtomicBool::new(false));

        {
            let mut state = manager.shared.state.lock().unwrap();
            state.fences.push_back(PendingFence {
                trace_id: 0,
                fence: TestFence { stubbed: false },
                pre_operations: VecDeque::new(),
                operations: VecDeque::from([Box::new({
                    let released = Arc::clone(&released);
                    move || {
                        released.store(true, Ordering::Relaxed);
                    }
                }) as Operation]),
            });
        }

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

        let mut manager = FenceManager::<TestFence>::new(false);
        let committed = Arc::new(AtomicBool::new(false));
        let callback_hit = Arc::new(AtomicBool::new(false));
        let flushed = Arc::new(AtomicBool::new(false));
        let invalidated = Arc::new(AtomicBool::new(false));

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
            {
                let flushed = Arc::clone(&flushed);
                move || flushed.store(true, Ordering::Relaxed)
            },
            {
                let invalidated = Arc::clone(&invalidated);
                move || invalidated.store(true, Ordering::Relaxed)
            },
        );

        assert!(committed.load(Ordering::Relaxed));
        assert!(callback_hit.load(Ordering::Relaxed));
        assert!(flushed.load(Ordering::Relaxed));
        assert!(invalidated.load(Ordering::Relaxed));
        assert_eq!(manager.queued_fence_count(), 1);

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

        let mut manager = FenceManager::<TestFence>::new(false);
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
            || {},
            || {},
        );

        assert_eq!(guest.load(Ordering::Relaxed), 7);
        assert_eq!(host.load(Ordering::Relaxed), 7);

        settings::values_mut().current_gpu_accuracy = previous_gpu_accuracy;
    }

    #[test]
    fn stubbed_fence_still_runs_async_flush_waiter_path() {
        let mut manager = FenceManager::<TestFence>::new(false);
        let popped = Arc::new(AtomicBool::new(false));

        {
            let mut state = manager.shared.state.lock().unwrap();
            state.fences.push_back(PendingFence {
                trace_id: 0,
                fence: TestFence { stubbed: true },
                pre_operations: VecDeque::new(),
                operations: VecDeque::new(),
            });
        }

        manager.wait_pending_fences(
            false,
            |is_stubbed| TestFence {
                stubbed: is_stubbed,
            },
            |_| {},
            || false,
            |_| true,
            |_| {},
            {
                let popped = Arc::clone(&popped);
                move || popped.store(true, Ordering::Relaxed)
            },
            || false,
            || {},
            || {},
            || {},
        );

        assert!(popped.load(Ordering::Relaxed));
        assert_eq!(manager.queued_fence_count(), 0);
    }

    #[test]
    fn async_signal_fence_executes_delayed_callback_without_manual_release() {
        let previous_gpu_accuracy = {
            let mut values = settings::values_mut();
            let previous = values.current_gpu_accuracy;
            values.current_gpu_accuracy = GpuAccuracy::High;
            previous
        };

        let mut manager = FenceManager::<TestFence>::new(true);
        let callback_hit = Arc::new(AtomicBool::new(false));
        let popped = Arc::new(AtomicBool::new(false));

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
            {
                let popped = Arc::clone(&popped);
                move || popped.store(true, Ordering::Relaxed)
            },
            || false,
            || {},
            || {},
            || {},
        );

        wait_until(&popped);
        wait_until(&callback_hit);
        settings::values_mut().current_gpu_accuracy = previous_gpu_accuracy;
    }

    #[test]
    fn async_wait_pending_fences_force_signals_drain_fence_and_waits_for_callback() {
        let previous_gpu_accuracy = {
            let mut values = settings::values_mut();
            let previous = values.current_gpu_accuracy;
            values.current_gpu_accuracy = GpuAccuracy::High;
            previous
        };

        let mut manager = FenceManager::<TestFence>::new(true);
        let created = Arc::new(AtomicU32::new(0));
        let queued = Arc::new(AtomicU32::new(0));

        manager.wait_pending_fences(
            true,
            {
                let created = Arc::clone(&created);
                move |is_stubbed| {
                    created.fetch_add(1, Ordering::Relaxed);
                    TestFence {
                        stubbed: is_stubbed,
                    }
                }
            },
            {
                let queued = Arc::clone(&queued);
                move |_| {
                    queued.fetch_add(1, Ordering::Relaxed);
                }
            },
            || false,
            |_| true,
            |_| {},
            || {},
            || false,
            || {},
            || {},
            || {},
        );

        assert_eq!(created.load(Ordering::Relaxed), 1);
        assert_eq!(queued.load(Ordering::Relaxed), 1);
        assert_eq!(manager.queued_fence_count(), 0);

        settings::values_mut().current_gpu_accuracy = previous_gpu_accuracy;
    }
}
