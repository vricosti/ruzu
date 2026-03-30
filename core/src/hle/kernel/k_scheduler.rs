//! Port of zuyu/src/core/hle/kernel/k_scheduler.h / k_scheduler.cpp
//! Status: Partial (structural port, complex methods stubbed)
//! Derniere synchro: 2026-03-11
//!
//! KScheduler: per-core scheduler managing thread dispatch and context switching.

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, Weak};
use std::thread;
use std::time::{Duration, Instant};

use common::fiber::Fiber;

use super::k_priority_queue::KPriorityQueue;
use super::k_process::KProcess;
use super::k_thread::KThread;
use super::k_thread::ThreadState;

/// Scheduling state held per-core.
/// Matches upstream `KScheduler::SchedulingState` (k_scheduler.h).
pub struct SchedulingState {
    pub needs_scheduling: AtomicBool,
    pub interrupt_task_runnable: bool,
    pub should_count_idle: bool,
    pub idle_count: u64,
    pub highest_priority_thread_id: Option<u64>,
    pub idle_thread_stack: usize, // void* — opaque
    pub prev_thread_id: Option<u64>,
    // interrupt_task_manager — opaque
}

impl Default for SchedulingState {
    fn default() -> Self {
        Self {
            needs_scheduling: AtomicBool::new(false),
            interrupt_task_runnable: false,
            should_count_idle: false,
            idle_count: 0,
            highest_priority_thread_id: None,
            idle_thread_stack: 0,
            prev_thread_id: None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn thread_context_guard_stays_locked_until_explicit_unlock() {
        let thread = Arc::new(Mutex::new(KThread::new()));

        assert!(KScheduler::try_lock_thread_context(&thread));
        assert!(thread.lock().unwrap().context_guard.try_lock().is_none());

        KScheduler::unlock_thread_context(&thread);
        assert!(thread.lock().unwrap().context_guard.try_lock().is_some());
    }

    #[test]
    fn schedule_impl_fiber_keeps_idle_handoff_when_highest_is_none() {
        let current_thread = Arc::new(Mutex::new(KThread::new()));
        current_thread.lock().unwrap().thread_id = 42;

        let mut scheduler = KScheduler::new(0);
        scheduler.current_thread = Some(Arc::downgrade(&current_thread));
        scheduler.current_thread_id = Some(42);
        scheduler.state.highest_priority_thread_id = None;
        scheduler.state.interrupt_task_runnable = false;

        scheduler.schedule_impl_fiber();

        assert!(scheduler.switch_cur_thread.is_some());
        assert!(scheduler.switch_highest_priority_thread.is_none());
        assert!(scheduler.switch_from_schedule);
    }
}

/// The per-core kernel scheduler.
/// Matches upstream `KScheduler` class (k_scheduler.h).
pub struct KScheduler {
    pub state: SchedulingState,
    pub is_active: bool,
    pub core_id: i32,
    pub last_context_switch_time: i64,
    pub idle_thread_id: Option<u64>,
    pub idle_thread: Option<Weak<Mutex<KThread>>>,
    pub current_thread_id: Option<u64>,
    pub current_thread: Option<Weak<Mutex<KThread>>>,
    pub yielded_thread_id: Option<u64>,

    // Kernel references — upstream stores KernelCore& m_kernel
    pub global_scheduler_context: Option<Arc<Mutex<super::global_scheduler_context::GlobalSchedulerContext>>>,
    pub physical_cores: Vec<Arc<super::physical_core::PhysicalCore>>,
    pub core_timing: Option<Arc<std::sync::Mutex<crate::core_timing::CoreTiming>>>,

    // Fiber fields for host-thread switching
    /// Upstream: `std::shared_ptr<Common::Fiber> m_switch_fiber`
    pub switch_fiber: Option<Arc<Fiber>>,
    pub switch_cur_thread: Option<Weak<Mutex<KThread>>>,
    pub switch_highest_priority_thread: Option<Weak<Mutex<KThread>>>,
    pub switch_from_schedule: bool,
}

impl KScheduler {
    fn try_lock_thread_context(thread: &Arc<Mutex<KThread>>) -> bool {
        let thread_guard = thread.lock().unwrap();
        let Some(context_guard) = thread_guard.context_guard.try_lock() else {
            return false;
        };

        // Match upstream KThread::m_context_guard semantics: keep the lock
        // held across the fiber switch until Unload() explicitly releases it.
        std::mem::forget(context_guard);
        true
    }

    fn unlock_thread_context(thread: &Arc<Mutex<KThread>>) {
        let thread_guard = thread.lock().unwrap();
        unsafe {
            thread_guard.context_guard.force_unlock();
        }
    }

    pub(crate) fn lock_thread_context_for_runtime(thread: &Arc<Mutex<KThread>>) -> bool {
        Self::try_lock_thread_context(thread)
    }

    pub(crate) fn unlock_thread_context_for_runtime(thread: &Arc<Mutex<KThread>>) {
        Self::unlock_thread_context(thread);
    }

    fn exit_thread_if_termination_requested(
        &self,
        process: &Arc<Mutex<KProcess>>,
        thread_id: u64,
    ) -> bool {
        let thread = {
            let process = process.lock().unwrap();
            process.get_thread_by_thread_id(thread_id)
        };
        let Some(thread) = thread else {
            return false;
        };

        if {
            let thread = thread.lock().unwrap();
            thread.is_termination_requested() && !thread.is_signaled()
        } {
            thread.lock().unwrap().exit();
            return true;
        }

        false
    }

    /// Create a new scheduler for the given core.
    pub fn new(core_id: i32) -> Self {
        Self {
            state: SchedulingState::default(),
            is_active: false,
            core_id,
            last_context_switch_time: 0,
            idle_thread_id: None,
            idle_thread: None,
            current_thread_id: None,
            current_thread: None,
            yielded_thread_id: None,
            global_scheduler_context: None,
            physical_cores: Vec::new(),
            core_timing: None,
            switch_fiber: None,
            switch_cur_thread: None,
            switch_highest_priority_thread: None,
            switch_from_schedule: false,
        }
    }

    /// Initialize the scheduler with main and idle threads (ID-only variant).
    /// Used by tests that don't need full thread references.
    pub fn initialize(
        &mut self,
        main_thread_id: u64,
        idle_thread_id: u64,
        core_id: i32,
    ) {
        self.core_id = core_id;
        self.idle_thread_id = Some(idle_thread_id);
        self.current_thread_id = Some(main_thread_id);
    }

    /// Initialize the scheduler with main and idle thread references.
    /// Matches upstream `KScheduler::Initialize(main_thread, idle_thread, core_id)`
    /// (k_scheduler.cpp:147-168).
    ///
    /// Sets `m_current_thread = main_thread` so that `get_scheduler_current_thread()`
    /// returns a valid thread with a host context for fiber switching.
    pub fn initialize_with_threads(
        &mut self,
        main_thread: &Arc<Mutex<KThread>>,
        idle_thread: &Arc<Mutex<KThread>>,
        core_id: i32,
    ) {
        self.core_id = core_id;

        let main_thread_id = main_thread.lock().unwrap().get_thread_id();
        let idle_thread_id = idle_thread.lock().unwrap().get_thread_id();

        self.idle_thread_id = Some(idle_thread_id);
        self.idle_thread = Some(Arc::downgrade(idle_thread));

        self.current_thread_id = Some(main_thread_id);
        self.current_thread = Some(Arc::downgrade(main_thread));
    }

    /// Activate the scheduler.
    /// Matches upstream `KScheduler::Activate()`.
    pub fn activate(&mut self) {
        self.is_active = true;
        self.reschedule_current_core();
    }

    /// Get the idle thread count.
    pub fn get_idle_count(&self) -> u64 {
        self.state.idle_count
    }

    /// Is the scheduler currently idle?
    pub fn is_idle(&self) -> bool {
        self.current_thread_id == self.idle_thread_id
    }

    /// Get the previous thread id.
    pub fn get_previous_thread_id(&self) -> Option<u64> {
        self.state.prev_thread_id
    }

    /// Get the current thread id.
    pub fn get_scheduler_current_thread_id(&self) -> Option<u64> {
        self.current_thread_id
    }

    /// Get the scheduler's current thread (Arc).
    /// Upstream: `KScheduler::GetSchedulerCurrentThread()`.
    pub fn get_scheduler_current_thread(&self) -> Option<Arc<Mutex<KThread>>> {
        self.current_thread.as_ref().and_then(Weak::upgrade)
    }

    /// Get the last context switch time.
    pub fn get_last_context_switch_time(&self) -> i64 {
        self.last_context_switch_time
    }

    /// Set the interrupt task as runnable.
    /// Matches upstream: sets flag and needs_scheduling.
    pub fn set_interrupt_task_runnable(&mut self) {
        self.state.interrupt_task_runnable = true;
        self.state.needs_scheduling.store(true, Ordering::Relaxed);
    }

    /// Request schedule on interrupt.
    /// Matches upstream: sets needs_scheduling, calls ScheduleOnInterrupt
    /// if dispatch is allowed.
    pub fn request_schedule_on_interrupt(&mut self) {
        self.state.needs_scheduling.store(true, Ordering::Relaxed);
        // Full upstream path: if (CanSchedule()) { ScheduleOnInterrupt(); }
        // We defer the actual fiber switch to the caller (cpu_manager) via
        // schedule_raw_if_needed(), so that the switch never happens while
        // the scheduler Mutex is held (holding a Mutex across a fiber yield
        // causes deadlock when the next fiber tries to lock the same Mutex).
    }

    /// Activate the scheduler and schedule without holding the Mutex.
    ///
    /// Matches upstream `KScheduler::Activate()` (k_scheduler.cpp:138-141):
    ///   m_is_active = true;
    ///   RescheduleCurrentCore();
    ///
    /// Called via raw pointer from `CpuManager::guest_activate` so that the
    /// fiber switch inside `schedule_impl_fiber` never occurs while the
    /// per-core scheduler Mutex is held.
    ///
    /// # Safety
    /// Must be called on the core OS thread.  The caller must have already
    /// dropped the Mutex guard before invoking this function.  The scheduler
    /// object must remain alive for the duration of the call (guaranteed by
    /// the Arc kept in KernelCore).
    pub unsafe fn activate_and_schedule_raw(sched: *mut KScheduler) {
        (*sched).is_active = true;
        // Upstream: RescheduleCurrentCore() → EnableDispatch + Schedule if needed.
        if let Some(cur_thread) = super::kernel::get_current_thread_pointer() {
            cur_thread.lock().unwrap().enable_dispatch();
        }
        if (*sched).state.needs_scheduling.load(Ordering::SeqCst) {
            (*sched).reschedule_current_core_impl();
        }
    }

    /// Perform a scheduling fiber switch without holding the Mutex.
    ///
    /// Matches the scheduling half of upstream `KScheduler::ScheduleOnInterrupt()`:
    ///   DisableDispatch(); Schedule(); EnableDispatch();
    ///
    /// Called via raw pointer from `CpuManager` after `handle_interrupt()` so that
    /// the fiber switch inside `schedule_impl_fiber` never occurs while the
    /// per-core scheduler Mutex is held.
    ///
    /// # Safety
    /// Same requirements as `activate_and_schedule_raw`.
    pub unsafe fn schedule_raw_if_needed(sched: *mut KScheduler) {
        // Trigger UpdateHighestPriorityThreads to sync PQ → highest_priority_thread_id.
        // Upstream: this happens in KScopedSchedulerLock::Unlock() before ScheduleImpl.
        // We call it here because our dispatch loop doesn't use KScopedSchedulerLock.
        if let Some(gsc_arc) = &(*sched).global_scheduler_context {
            // Get callback fn pointer while holding GSC lock, then release
            // GSC before calling (the callback re-locks GSC internally).
            let update_fn = {
                let gsc = gsc_arc.lock().unwrap();
                gsc.scheduler_lock().get_update_callback()
            };
            if let Some(f) = update_fn {
                f();
            }
        }

        if (*sched).state.needs_scheduling.load(Ordering::SeqCst) {
            if let Some(cur_thread) = super::kernel::get_current_thread_pointer() {
                cur_thread.lock().unwrap().disable_dispatch();
            }
            (*sched).schedule_impl_fiber();
            if let Some(cur_thread) = super::kernel::get_current_thread_pointer() {
                cur_thread.lock().unwrap().enable_dispatch();
            }
        }
    }

    /// Initialize the switch fiber for this scheduler.
    /// Must be called after the scheduler is wrapped in Arc<Mutex<>>.
    ///
    /// Upstream creates this in the KScheduler constructor:
    /// ```cpp
    /// m_switch_fiber = std::make_shared<Common::Fiber>([this] {
    ///     while (true) { ScheduleImplFiber(); }
    /// });
    /// ```
    /// In Rust we can't capture `self` in the constructor before the Arc exists,
    /// so this is a separate initialization step.
    pub fn init_switch_fiber(scheduler: &Arc<Mutex<KScheduler>>) {
        // The switch fiber runs on the same OS thread as the code that yields
        // to it. Only one fiber runs at a time, so there's no data race.
        // We use a raw pointer because the scheduler's Mutex is already held
        // by the caller when it yields to this fiber (YieldTo suspends the
        // caller's fiber with the MutexGuard still on its stack).
        // This matches upstream where ScheduleImplFiber accesses `this`
        // directly without re-locking.
        let sched_ptr = {
            let mut guard = scheduler.lock().unwrap();
            &mut *guard as *mut KScheduler as usize
        };
        let fiber = Fiber::new(Box::new(move || {
            loop {
                // Safety: only one fiber runs at a time on this OS thread,
                // and the scheduler outlives the switch fiber. We use usize
                // to bypass Send requirement (same pattern as CpuManager threads).
                let sched = unsafe { &mut *(sched_ptr as *mut KScheduler) };
                sched.schedule_impl_fiber_loop();
            }
        }));
        scheduler.lock().unwrap().switch_fiber = Some(fiber);
    }

    /// Preempt single core.
    /// Matches upstream `KScheduler::PreemptSingleCore()`:
    /// disables dispatch, unloads thread, yields to switch fiber, enables dispatch.
    pub fn preempt_single_core(&mut self) {
        // Upstream:
        //   GetCurrentThread(m_kernel).DisableDispatch();
        //   auto* thread = GetCurrentThreadPointer(m_kernel);
        //   auto& previous_scheduler = m_kernel.Scheduler(thread->GetCurrentCore());
        //   previous_scheduler.Unload(thread);
        //   Common::Fiber::YieldTo(thread->GetHostContext(), *m_switch_fiber);
        //   GetCurrentThread(m_kernel).EnableDispatch();

        let cur_thread = super::kernel::get_current_thread_pointer();
        if let Some(ref cur_thread) = cur_thread {
            cur_thread.lock().unwrap().disable_dispatch();

            // Unload the current thread
            self.unload(cur_thread);

            // Yield to the switch fiber
            if let Some(ref switch_fiber) = &self.switch_fiber {
                if let Some(ref host_ctx) = cur_thread.lock().unwrap().host_context {
                    Fiber::yield_to(Arc::downgrade(host_ctx), switch_fiber);
                }
            }

            cur_thread.lock().unwrap().enable_dispatch();
        }
    }

    /// Called when a thread first starts executing on this core.
    /// Matches upstream `KScheduler::OnThreadStart()`.
    pub fn on_thread_start(&self, current_thread: &Arc<Mutex<KThread>>) {
        current_thread.lock().unwrap().enable_dispatch();
    }

    /// Unload a thread's context (save guest state).
    /// Matches upstream `KScheduler::Unload(KThread*)`.
    pub fn unload(&self, thread: &Arc<Mutex<KThread>>) {
        // Upstream: m_kernel.PhysicalCore(m_core_id).SaveContext(thread)
        if let Some(core) = self.physical_cores.get(self.core_id as usize) {
            core.save_context(&mut thread.lock().unwrap());
        }

        // Check if the thread is terminated by checking the DPC flags.
        let thread_guard = thread.lock().unwrap();
        let dpc_flags = thread_guard.get_dpc();
        drop(thread_guard);
        if (dpc_flags & super::k_thread::DpcFlag::TERMINATED.bits() as u8) == 0 {
            // Upstream: thread->m_context_guard.unlock()
            Self::unlock_thread_context(thread);
        }
    }

    /// Reload a thread's context (restore guest state).
    /// Matches upstream `KScheduler::Reload(KThread*)`.
    /// Upstream: `m_kernel.PhysicalCore(m_core_id).LoadContext(thread)`.
    pub fn reload(&self, thread: &Arc<Mutex<KThread>>) {
        // Inline PhysicalCore::LoadContext since the scheduler doesn't hold
        // a reference to the kernel's physical cores.
        let thread_guard = thread.lock().unwrap();
        let parent = match thread_guard.parent.as_ref().and_then(|w| w.upgrade()) {
            Some(p) => p,
            None => return,
        };
        let mut process = parent.lock().unwrap();
        if let Some(jit) = process.get_arm_interface_mut(self.core_id as usize) {
            let k_ctx = &thread_guard.thread_context;
            let arm_ctx: &crate::arm::arm_interface::ThreadContext =
                unsafe { &*(k_ctx as *const super::k_thread::ThreadContext
                    as *const crate::arm::arm_interface::ThreadContext) };
            jit.set_context(arm_ctx);
            jit.set_tpidrro_el0(thread_guard.get_tls_address().get());
            log::info!(
                "KScheduler::Reload: core={} r15/PC=0x{:X} r13/SP=0x{:X}",
                self.core_id, k_ctx.r[15], k_ctx.r[13],
            );
        }
    }

    /// Reschedule other cores by sending IPI.
    /// Matches upstream `KScheduler::RescheduleOtherCores(u64)`.
    pub fn reschedule_other_cores(&self, cores_needing_scheduling: u64) {
        let core_mask = cores_needing_scheduling & !(1u64 << self.core_id);
        if core_mask != 0 {
            self.reschedule_cores_impl(core_mask);
        }
    }

    /// Send IPI to cores that need rescheduling.
    /// Matches upstream `KScheduler::RescheduleCores(kernel, core_mask)`.
    fn reschedule_cores_impl(&self, core_mask: u64) {
        for i in 0..crate::hardware_properties::NUM_CPU_CORES as usize {
            if core_mask & (1u64 << i) != 0 {
                if let Some(core) = self.physical_cores.get(i) {
                    core.interrupt();
                }
            }
        }
    }

    /// Static version of RescheduleCores (when no scheduler instance available).
    /// Upstream: `KScheduler::RescheduleCores(kernel, core_mask)` sends IPIs.
    /// Without kernel reference, logs and returns (IPIs require physical core access).
    pub fn reschedule_cores(core_mask: u64) {
        if core_mask != 0 {
            log::trace!("KScheduler::reschedule_cores: core_mask={:#x} (no physical core access)", core_mask);
        }
    }

    /// Matches upstream `KScheduler::RescheduleCurrentHLEThread(kernel)`.
    /// Called when no scheduler is available (non-core threads, phantom mode).
    pub fn reschedule_current_hle_thread() {
        if let Some(cur_thread) = super::kernel::get_current_thread_pointer() {
            let mut t = cur_thread.lock().unwrap();
            debug_assert!(t.get_disable_dispatch_count() == 1);

            // Upstream: GetCurrentThread(kernel).DummyThreadBeginWait();
            // Ensure dummy threads that are waiting block.
            if t.is_dummy_thread() {
                t.dummy_thread_begin_wait();
            }

            debug_assert!(t.get_state() != ThreadState::WAITING);
            t.enable_dispatch();
        }
    }

    /// Reschedule the current core.
    /// Matches upstream `KScheduler::RescheduleCurrentCore()`.
    pub fn reschedule_current_core(&mut self) {
        // Upstream: ASSERT(!m_kernel.IsPhantomModeForSingleCore());
        // Upstream: ASSERT(GetCurrentThread(m_kernel).GetDisableDispatchCount() == 1);

        // Upstream: GetCurrentThread(m_kernel).EnableDispatch();
        if let Some(cur_thread) = super::kernel::get_current_thread_pointer() {
            cur_thread.lock().unwrap().enable_dispatch();
        }

        if self.state.needs_scheduling.load(Ordering::SeqCst) {
            self.reschedule_current_core_impl();
        }
    }

    fn reschedule_current_core_impl(&mut self) {
        // Upstream: if (m_state.needs_scheduling.load()) [[likely]] {
        //     GetCurrentThread(m_kernel).DisableDispatch();
        //     Schedule();
        //     GetCurrentThread(m_kernel).EnableDispatch();
        // }
        if self.state.needs_scheduling.load(Ordering::SeqCst) {
            if let Some(cur_thread) = super::kernel::get_current_thread_pointer() {
                cur_thread.lock().unwrap().disable_dispatch();
            }
            self.schedule();
            if let Some(cur_thread) = super::kernel::get_current_thread_pointer() {
                cur_thread.lock().unwrap().enable_dispatch();
            }
        }
    }

    /// Matches upstream `KScheduler::Schedule()`.
    fn schedule(&mut self) {
        // Upstream: ScheduleImpl() which yields to the switch fiber.
        self.schedule_impl_fiber();
    }

    /// Clear previous thread across all schedulers.
    /// Matches upstream `KScheduler::ClearPreviousThread(kernel, thread)`.
    pub fn clear_previous_thread(schedulers: &mut [KScheduler], thread_id: u64) {
        for scheduler in schedulers.iter_mut() {
            if scheduler.state.prev_thread_id == Some(thread_id) {
                scheduler.state.prev_thread_id = None;
            }
        }
    }

    // -- Static methods --
    // In upstream these take `KernelCore&` and access global state via
    // GetCurrentThread(kernel). Here they take the current thread directly.

    /// Matches upstream `KScheduler::DisableScheduling(kernel)`.
    /// Increments the current thread's disable_dispatch_count.
    pub fn disable_scheduling(current_thread: &Arc<Mutex<KThread>>) {
        let mut t = current_thread.lock().unwrap();
        debug_assert!(t.get_disable_dispatch_count() >= 0);
        t.disable_dispatch();
    }

    /// Matches upstream `KScheduler::EnableScheduling(kernel, cores_needing_scheduling)`.
    /// Decrements dispatch count. If it reaches 0, triggers rescheduling.
    pub fn enable_scheduling_with_scheduler(
        current_thread: &Arc<Mutex<KThread>>,
        cores_needing_scheduling: u64,
        scheduler: Option<&Arc<Mutex<KScheduler>>>,
        is_phantom_mode: bool,
    ) {
        debug_assert!(current_thread.lock().unwrap().get_disable_dispatch_count() >= 1);

        if scheduler.is_none() || is_phantom_mode {
            // Upstream: KScheduler::RescheduleCores(kernel, cores_needing_scheduling);
            //           KScheduler::RescheduleCurrentHLEThread(kernel);
            Self::reschedule_cores(cores_needing_scheduling);
            Self::reschedule_current_hle_thread();
            return;
        }

        let scheduler = scheduler.unwrap();
        {
            let sched = scheduler.lock().unwrap();
            sched.reschedule_other_cores(cores_needing_scheduling);
        }

        if current_thread.lock().unwrap().get_disable_dispatch_count() > 1 {
            current_thread.lock().unwrap().enable_dispatch();
        } else {
            scheduler.lock().unwrap().reschedule_current_core();
        }
    }

    /// Matches upstream `KScheduler::UpdateHighestPriorityThreads(kernel)`.
    /// Called by KAbstractSchedulerLock::Unlock.
    ///
    /// Upstream checks IsSchedulerUpdateNeeded and if set, calls
    /// UpdateHighestPriorityThreadsImpl. Returns bitmask of cores needing rescheduling.
    ///
    /// Note: The static version without kernel context returns 0.
    /// The full implementation requires GlobalSchedulerContext access and is
    /// invoked through the scheduler callbacks wired by the kernel.
    pub fn update_highest_priority_threads() -> u64 {
        // Without kernel context, return 0. When wired through SchedulerCallbacks,
        // the kernel provides the actual implementation via
        // update_highest_priority_threads_with_context().
        0
    }

    /// Full implementation of UpdateHighestPriorityThreads with GSC access.
    /// Called from wired scheduler callbacks that have kernel context.
    pub fn update_highest_priority_threads_with_context(
        gsc: &mut super::global_scheduler_context::GlobalSchedulerContext,
        schedulers: &mut [std::sync::MutexGuard<'_, KScheduler>],
    ) -> (u64, Vec<(u64, i32)>) {
        if gsc.m_scheduler_update_needed.load(std::sync::atomic::Ordering::Relaxed) {
            Self::update_highest_priority_threads_impl(schedulers, gsc)
        } else {
            (0, Vec::new())
        }
    }

    /// Matches upstream `KScheduler::UpdateHighestPriorityThreadsImpl(kernel)`.
    /// Full implementation with pinned thread handling and idle core migration.
    ///
    /// Returns (cores_needing_scheduling, migrations) where migrations is a list
    /// of (thread_id, new_core) pairs. The caller must update KThread.core_id
    /// for each migration AFTER releasing the GSC lock.
    pub fn update_highest_priority_threads_impl(
        schedulers: &mut [std::sync::MutexGuard<'_, KScheduler>],
        gsc: &mut super::global_scheduler_context::GlobalSchedulerContext,
    ) -> (u64, Vec<(u64, i32)>) {
        use super::global_scheduler_context::HIGHEST_CORE_MIGRATION_ALLOWED_PRIORITY;
        use crate::hardware_properties::NUM_CPU_CORES;

        // Clear scheduler update needed.
        gsc.m_scheduler_update_needed
            .store(false, std::sync::atomic::Ordering::Relaxed);

        let mut cores_needing_scheduling = 0u64;
        let mut idle_cores = 0u64;
        let mut migrations: Vec<(u64, i32)> = Vec::new();
        let mut top_threads: [Option<u64>; NUM_CPU_CORES as usize] = [None; NUM_CPU_CORES as usize];

        // Select top thread per core from PQ.
        for core_id in 0..NUM_CPU_CORES as usize {
            let top_thread_id = gsc.m_priority_queue.get_scheduled_front(core_id as i32);

            // Upstream: check pinned thread for the process.
            // If the top thread's process has a pinned thread for this core,
            // and it's different from the top thread, prefer the pinned one
            // (unless top thread has kernel waiters).
            // Simplified: no pinned thread support yet.

            if top_thread_id.is_none() {
                idle_cores |= 1u64 << core_id;
            }

            top_threads[core_id] = top_thread_id;
            if core_id < schedulers.len() {
                let (mask, prev_id) =
                    schedulers[core_id].update_highest_priority_thread(top_threads[core_id]);
                cores_needing_scheduling |= mask;
                // Upstream: IncrementScheduledCount(prev_thread)
                if let Some(pid) = prev_id {
                    gsc.m_priority_queue.increment_scheduled_count(pid);
                }
            }
        }

        // Upstream migrates runnable threads toward idle cores here.
        //
        // The current Rust `common::fiber` backend still does not provide
        // trustworthy cross-host-thread fiber exchange parity with the upstream
        // `boost::context` implementation. Allowing the migration step here can
        // move a runnable guest thread to another core scheduler while its host
        // fiber continues executing on the previous host core, which corrupts
        // per-thread runtime state such as TLS-visible IPC buffers.
        //
        // Keep ownership and all non-migration scheduling logic in this file,
        // but skip the actual migration pass until the backend can faithfully
        // exchange fibers between host threads.
        let _ = (
            &mut idle_cores,
            &mut migrations,
            HIGHEST_CORE_MIGRATION_ALLOWED_PRIORITY,
        );

        // Wake up waiting dummy threads.
        gsc.wakeup_waiting_dummy_threads();

        (cores_needing_scheduling, migrations)
    }

    /// Update the highest priority thread for this core.
    /// Matches upstream `KScheduler::UpdateHighestPriorityThread(KThread*)`.
    /// Returns (cores_needing_scheduling bitmask, previous_highest_thread_id).
    pub fn update_highest_priority_thread(&mut self, highest_thread_id: Option<u64>) -> (u64, Option<u64>) {
        let prev = self.state.highest_priority_thread_id;
        if prev != highest_thread_id {
            self.state.highest_priority_thread_id = highest_thread_id;
            self.state.needs_scheduling.store(true, Ordering::Relaxed);
            (1u64 << self.core_id, prev)
        } else {
            (0, None)
        }
    }

    /// On thread state changed.
    /// Matches upstream `KScheduler::OnThreadStateChanged(kernel, thread, old_state)`.
    ///
    /// Updates the priority queue when a thread transitions to/from RUNNABLE.
    pub fn on_thread_state_changed(
        &mut self,
        thread_id: u64,
        old_state: ThreadState,
        new_state: ThreadState,
    ) {
        if old_state == new_state {
            return;
        }

        // Always request scheduling on state change for cooperative dispatch.
        self.state.prev_thread_id = Some(thread_id);
        self.request_schedule();

        // PQ updates happen at the call sites that transition thread state
        // while holding the process lock (condvar wait/signal, sync wait,
        // thread exit, etc.). The dispatch loop's scan_runnable_threads
        // fallback catches any threads that bypass PQ updates.
    }

    /// On thread priority changed.
    /// Matches upstream `KScheduler::OnThreadPriorityChanged(kernel, thread, old_priority)`.
    pub fn on_thread_priority_changed(&mut self, thread_id: u64, _old_priority: i32) {
        self.state.prev_thread_id = Some(thread_id);
        self.request_schedule();

        // Upstream: if thread is RUNNABLE, call
        // priority_queue.ChangePriority(old_priority, is_running, thread).
        // Same as above: PQ update deferred until PQ-based dispatch.
    }

    /// Yield without core migration.
    /// Matches upstream `KScheduler::YieldWithoutCoreMigration(kernel)`.
    pub fn yield_without_core_migration(
        &mut self,
        process: &Arc<Mutex<KProcess>>,
        current_thread_id: u64,
    ) {
        // Cooperative runtime approximation of a thread observing its pending
        // termination and exiting at the next yield boundary.
        if self.exit_thread_if_termination_requested(process, current_thread_id) {
            self.request_schedule();
            return;
        }

        let current_thread = {
            let process = process.lock().unwrap();
            process.get_thread_by_thread_id(current_thread_id)
        };
        let Some(current_thread) = current_thread else {
            return;
        };

        let mut process = process.lock().unwrap();
        {
            let current_thread = current_thread.lock().unwrap();
            if current_thread.get_state() != ThreadState::RUNNABLE {
                return;
            }
            if current_thread.get_yield_schedule_count() == process.get_scheduled_count() {
                return;
            }
        }

        // Move current thread to the back of its priority level in the PQ.
        // Upstream: next_thread = priority_queue.MoveToScheduledBack(cur_thread)
        let next_thread_id = if let Some(ref gsc) = process.global_scheduler_context {
            let ct = current_thread.lock().unwrap();
            let pri = ct.priority;
            let core = ct.core_id;
            let is_dummy = ct.thread_type == super::k_thread::ThreadType::Dummy;
            drop(ct);
            gsc.lock().unwrap().move_to_scheduled_back(current_thread_id, pri, core, is_dummy)
        } else {
            None
        };
        process.increment_scheduled_count();

        if let Some(next_id) = next_thread_id {
            if next_id != current_thread_id {
                // A different thread is now at the front — schedule update needed.
                self.yielded_thread_id = Some(current_thread_id);
                self.request_schedule();
            } else {
                // No other thread at this priority — set yield count.
                current_thread
                    .lock()
                    .unwrap()
                    .set_yield_schedule_count(process.get_scheduled_count());
            }
        } else {
            // PQ was empty (thread not in PQ) — fall back to old behavior.
            self.yielded_thread_id = Some(current_thread_id);
            self.request_schedule();
        }
    }

    /// Yield with core migration.
    pub fn yield_with_core_migration(
        &mut self,
        process: &Arc<Mutex<KProcess>>,
        current_thread_id: u64,
    ) {
        // Single-core bring-up: match upstream control flow entry point, but keep the same
        // local-core behavior until cross-core migration exists.
        self.yield_without_core_migration(process, current_thread_id);
    }

    /// Yield to any thread.
    pub fn yield_to_any_thread(
        &mut self,
        process: &Arc<Mutex<KProcess>>,
        current_thread_id: u64,
    ) {
        // Single-core bring-up: preserve the SVC ownership in KScheduler while deferring
        // real inter-core migration until the full priority queue exists.
        self.yield_without_core_migration(process, current_thread_id);
    }

    pub fn set_scheduler_current_thread_id(&mut self, thread_id: u64) {
        self.current_thread_id = Some(thread_id);
        if self.yielded_thread_id == Some(thread_id) {
            self.yielded_thread_id = None;
        }
        self.state.needs_scheduling.store(false, Ordering::Relaxed);
    }

    pub fn set_scheduler_current_thread(&mut self, thread: &Arc<Mutex<KThread>>) {
        let thread_id = thread.lock().unwrap().get_thread_id();
        self.current_thread_id = Some(thread_id);
        self.current_thread = Some(Arc::downgrade(thread));
        if self.yielded_thread_id == Some(thread_id) {
            self.yielded_thread_id = None;
        }
        self.state.needs_scheduling.store(false, Ordering::Relaxed);
        super::kernel::set_current_emu_thread(Some(thread));
    }

    pub fn request_schedule(&self) {
        self.state.needs_scheduling.store(true, Ordering::Relaxed);
    }

    /// Rotate the scheduled queue at a given priority for a core.
    /// Matches upstream `KScheduler::RotateScheduledQueue(kernel, core_id, priority)`.
    ///
    /// Moves the front thread at `priority` to the back, then tries to
    /// migrate a suggested thread to fill the gap.
    /// Rotate the scheduled queue at a given priority for a core.
    /// Operates on the GlobalSchedulerContext's PQ.
    pub fn rotate_scheduled_queue(
        gsc: &mut super::global_scheduler_context::GlobalSchedulerContext,
        core_id: i32,
        priority: i32,
    ) {
        let top_thread_id = gsc.m_priority_queue.get_scheduled_front_at_priority(core_id, priority);
        if let Some(top_id) = top_thread_id {
            let (t_priority, t_core, t_dummy) = gsc.m_priority_queue
                .get_thread_props(top_id)
                .map(|p| (p.priority, p.active_core, p.is_dummy))
                .unwrap_or((priority, core_id, false));
            let next = gsc.m_priority_queue.move_to_scheduled_back(top_id, t_priority, t_core, t_dummy);
            // Upstream: IncrementScheduledCount on top_thread and next_thread if different.
            if next != Some(top_id) {
                gsc.m_priority_queue.increment_scheduled_count(top_id);
                if let Some(next_id) = next {
                    gsc.m_priority_queue.increment_scheduled_count(next_id);
                }
            }
        }

        gsc.m_scheduler_update_needed
            .store(true, std::sync::atomic::Ordering::Release);
    }

    pub fn needs_scheduling(&self) -> bool {
        self.state.needs_scheduling.load(Ordering::Relaxed)
    }

    pub fn wake_expired_sleeping_threads(&mut self, process: &Arc<Mutex<KProcess>>) -> bool {
        let now = Instant::now();
        let mut process = process.lock().unwrap();
        let mut woke_any = false;
        let mut woke_ids = Vec::new();

        for thread_id in &process.thread_list {
            let Some(thread) = process.get_thread_by_thread_id(*thread_id) else {
                continue;
            };

            let mut thread = thread.lock().unwrap();
            let Some(deadline) = thread.get_sleep_deadline() else {
                continue;
            };
            if deadline > now {
                continue;
            }

            let tid = thread.get_thread_id();
            thread.on_timer();
            woke_ids.push(tid);
            woke_any = true;
        }

        // Push woken threads to PQ (they're now RUNNABLE).
        for tid in woke_ids {
            process.push_back_to_priority_queue(tid);
        }

        if woke_any {
            self.request_schedule();
        }

        woke_any
    }

    pub fn wake_signaled_synchronization_threads(
        &mut self,
        process: &Arc<Mutex<KProcess>>,
    ) -> bool {
        let mut process = process.lock().unwrap();
        let mut woke_any = false;
        let mut woke_ids = Vec::new();

        for thread_id in &process.thread_list {
            let Some(thread) = process.get_thread_by_thread_id(*thread_id) else {
                continue;
            };

            let mut thread = thread.lock().unwrap();
            if !thread.is_waiting_on_synchronization() || thread.get_state() != ThreadState::WAITING {
                continue;
            }

            let Some(synced_index) = thread.check_synchronization_ready(&process) else {
                continue;
            };

            let tid = thread.get_thread_id();
            thread.complete_synchronization_wait(synced_index, crate::hle::result::RESULT_SUCCESS.get_inner_value());
            woke_ids.push(tid);
            woke_any = true;
        }

        // Push woken threads to PQ (they're now RUNNABLE).
        for tid in woke_ids {
            process.push_back_to_priority_queue(tid);
        }

        if woke_any {
            self.request_schedule();
        }

        woke_any
    }

    fn next_sleep_deadline(&self, process: &Arc<Mutex<KProcess>>) -> Option<Instant> {
        let process = process.lock().unwrap();
        let mut next_deadline = None;

        for thread_id in &process.thread_list {
            let Some(thread) = process.get_thread_by_thread_id(*thread_id) else {
                continue;
            };

            let thread = thread.lock().unwrap();
            let Some(deadline) = thread.get_sleep_deadline() else {
                continue;
            };

            next_deadline = Some(match next_deadline {
                Some(current) if current <= deadline => current,
                _ => deadline,
            });
        }

        next_deadline
    }

    pub fn wait_for_next_runnable_thread(
        &mut self,
        process: &Arc<Mutex<KProcess>>,
        current_thread_id: u64,
    ) -> u64 {
        loop {
            self.wake_expired_sleeping_threads(process);
            self.wake_signaled_synchronization_threads(process);

            // PQ-based selection (O(1) for highest priority thread).
            if let Some(next) = self.select_next_thread_from_pq(process) {
                return next;
            }

            // PQ empty — fallback to linear scan for RUNNABLE threads that
            // aren't in the PQ (timer/cancel_wait wakeups, early init).
            // This is a safety net; once all wakeup paths push to PQ, this
            // becomes unreachable.
            if let Some(next) = self.scan_runnable_threads(process) {
                return next;
            }

            if let Some(deadline) = self.next_sleep_deadline(process) {
                let now = Instant::now();
                if deadline > now {
                    thread::sleep(deadline.duration_since(now));
                }
                continue;
            }

            thread::sleep(Duration::from_millis(1));
        }
    }

    /// Fallback: scan for the highest-priority RUNNABLE thread.
    /// Used when PQ is empty (threads woken via timer/cancel_wait that
    /// bypass PQ). Returns the thread_id and pushes it to PQ for future use.
    fn scan_runnable_threads(&self, process: &Arc<Mutex<KProcess>>) -> Option<u64> {
        let process = process.lock().unwrap();
        let mut best_id = None;
        let mut best_priority = i32::MAX;

        for thread_id in &process.thread_list {
            let Some(thread) = process.get_thread_by_thread_id(*thread_id) else {
                continue;
            };
            let thread = thread.lock().unwrap();
            if thread.get_state() != ThreadState::RUNNABLE {
                continue;
            }
            if thread.get_priority() < best_priority {
                best_priority = thread.get_priority();
                best_id = Some(thread.get_thread_id());
            }
        }

        // Push the found thread to PQ so future lookups are O(1).
        if let Some(tid) = best_id {
            process.push_back_to_priority_queue(tid);
        }
        best_id
    }

    /// Select the next thread using the priority queue.
    /// This is the upstream-matching dispatch path: O(1) lookup of the
    /// highest priority RUNNABLE thread for our core.
    fn select_next_thread_from_pq(
        &mut self,
        process: &Arc<Mutex<KProcess>>,
    ) -> Option<u64> {
        let process_guard = process.lock().unwrap();
        let gsc = process_guard.global_scheduler_context.as_ref()?;
        let gsc_guard = gsc.lock().unwrap();
        let next_thread_id = gsc_guard.get_scheduled_front(self.core_id)?;

        // Handle yield: if the current thread yielded, try the next one.
        if let Some(yielded) = self.yielded_thread_id {
            if next_thread_id == yielded {
                let priority = process_guard
                    .get_thread_by_thread_id(next_thread_id)
                    .map(|t| t.lock().unwrap().get_priority())
                    .unwrap_or(63);
                let next = gsc_guard.get_scheduled_next(
                    self.core_id,
                    next_thread_id,
                    priority,
                );
                if let Some(alternative) = next {
                    self.yielded_thread_id = None;
                    return Some(alternative);
                }
                if let Some(thread) = process_guard.get_thread_by_thread_id(yielded) {
                    thread
                        .lock()
                        .unwrap()
                        .set_yield_schedule_count(process_guard.get_scheduled_count());
                }
                self.yielded_thread_id = None;
            }
        }

        Some(next_thread_id)
    }

    /// Matches upstream `KScheduler::ScheduleImpl()`.
    /// Clears needs_scheduling, selects the highest priority thread.
    ///
    /// Upstream yields to a fiber for context switching via ScheduleImplFiber.
    /// We provide both cooperative (no fiber) and fiber-based paths.
    fn schedule_impl_cooperative(&mut self) {
        self.state.needs_scheduling.store(false, Ordering::Relaxed);
        std::sync::atomic::fence(Ordering::SeqCst);

        let highest = self.state.highest_priority_thread_id;

        // If the interrupt task is runnable, switch to idle.
        let target = if self.state.interrupt_task_runnable {
            self.idle_thread_id
        } else {
            highest
        };

        // If same as current, nothing to do.
        if target == self.current_thread_id {
            std::sync::atomic::fence(Ordering::SeqCst);
            return;
        }

        // Switch to the target thread.
        let next_id = target.or(self.idle_thread_id);
        if let Some(next_id) = next_id {
            self.switch_thread_impl(next_id);
        }
    }

    /// Matches upstream `KScheduler::ScheduleImpl()` — fiber-based path.
    /// In upstream, this yields to m_switch_fiber which runs ScheduleImplFiber.
    fn schedule_impl_fiber(&mut self) {
        self.state.needs_scheduling.store(false, Ordering::Relaxed);
        std::sync::atomic::fence(Ordering::SeqCst);

        let cur_thread = self.current_thread.as_ref().and_then(Weak::upgrade);
        let highest = self.state.highest_priority_thread_id;

        log::info!(
            "schedule_impl_fiber: core={} cur_thread={} highest={:?} has_gsc={} has_switch_fiber={}",
            self.core_id,
            cur_thread.as_ref().map(|t| t.lock().unwrap().get_thread_id()).unwrap_or(u64::MAX),
            highest,
            self.global_scheduler_context.is_some(),
            self.switch_fiber.is_some(),
        );

        let target = if self.state.interrupt_task_runnable {
            self.idle_thread.as_ref().and_then(Weak::upgrade)
        } else {
            highest.and_then(|id| {
                let gsc = self.global_scheduler_context.as_ref()?;
                gsc.lock().unwrap().get_thread_by_thread_id(id)
            })
        };

        log::info!(
            "schedule_impl_fiber: target={}",
            target.as_ref().map(|t| t.lock().unwrap().get_thread_id()).unwrap_or(u64::MAX),
        );

        // If same as current, nothing to do.
        if let (Some(ref cur), Some(ref tgt)) = (&cur_thread, &target) {
            if Arc::ptr_eq(cur, tgt) {
                log::info!("schedule_impl_fiber: target == current, returning");
                std::sync::atomic::fence(Ordering::SeqCst);
                return;
            }
        }

        // Store switch state for the fiber.
        self.switch_cur_thread = cur_thread.as_ref().map(Arc::downgrade);
        self.switch_highest_priority_thread = target.as_ref().map(Arc::downgrade);
        self.switch_from_schedule = true;

        // Upstream: Common::Fiber::YieldTo(cur_thread->m_host_context, *m_switch_fiber)
        if let Some(ref cur) = cur_thread {
            let cur_lock = cur.lock().unwrap();
            let has_ctx = cur_lock.host_context.is_some();
            log::info!("schedule_impl_fiber: cur thread {} has host_context={}", cur_lock.get_thread_id(), has_ctx);
            if let Some(ref host_ctx) = cur_lock.host_context {
                if let Some(ref switch_fiber) = self.switch_fiber {
                    let host_weak = Arc::downgrade(host_ctx);
                    drop(cur_lock); // release thread lock before yield
                    log::info!("schedule_impl_fiber: yielding to switch_fiber NOW");
                    Fiber::yield_to(host_weak, switch_fiber);
                    log::info!("schedule_impl_fiber: returned from switch_fiber");
                } else {
                    log::warn!("schedule_impl_fiber: no switch_fiber");
                }
            } else {
                log::warn!("schedule_impl_fiber: current thread has no host_context");
            }
        } else {
            log::warn!("schedule_impl_fiber: no current thread");
        }
    }

    /// Matches upstream `KScheduler::ScheduleImplFiber()` (k_scheduler.cpp:420-494).
    /// This runs inside the switch fiber and handles the actual context switch:
    /// unloads old thread, spins to acquire new thread's context_guard,
    /// calls SwitchThread, reloads new thread, then yields back.
    fn schedule_impl_fiber_loop(&mut self) {

        // Trigger UpdateHighestPriorityThreads to sync PQ → highest_priority_thread_id.
        // This is needed when re-entering after a service thread fiber returned,
        // because the thread's TERMINATED transition removed it from PQ but
        // highest_priority_thread_id still points to the old thread.
        if let Some(gsc_arc) = &self.global_scheduler_context {
            let update_fn = {
                let gsc = gsc_arc.lock().unwrap();
                gsc.scheduler_lock().get_update_callback()
            };
            if let Some(f) = update_fn {
                f();
            }
        }

        let cur_thread = self.switch_cur_thread.as_ref().and_then(Weak::upgrade);
        let mut highest_priority_thread = self.switch_highest_priority_thread.as_ref().and_then(Weak::upgrade);

        // If we're not coming from scheduling (i.e., we came from SC preemption),
        // skip the unload and jump straight to retry.
        let mut need_retry = !self.switch_from_schedule;

        if self.switch_from_schedule {
            self.switch_from_schedule = false;

            // Save the original thread context.
            if let Some(ref cur) = cur_thread {
                self.unload(cur);
            }
        }

        // Loop until we successfully switch the thread context.
        loop {
            if need_retry {
                // Clear needs_scheduling and refresh highest priority thread.
                self.state.needs_scheduling.store(false, Ordering::Relaxed);
                std::sync::atomic::fence(Ordering::SeqCst);

                highest_priority_thread = if let Some(id) = self.state.highest_priority_thread_id {
                    if let Some(ref gsc) = self.global_scheduler_context {
                        gsc.lock().unwrap().get_thread_by_thread_id(id)
                    } else {
                        None
                    }
                } else {
                    None
                };
            }
            need_retry = false;

            // If highest_priority_thread is null, switch to idle thread.
            if highest_priority_thread.is_none() {
                highest_priority_thread = self.idle_thread.as_ref().and_then(Weak::upgrade);
            }

            let Some(ref hpt) = highest_priority_thread else {
                // No thread available at all — retry.
                need_retry = true;
                continue;
            };

            // Try to lock the highest priority thread's context_guard.
            loop {
                if Self::try_lock_thread_context(hpt) {
                    break;
                }
                // Context is locked by another core. Check if we need rescheduling.
                if self.state.needs_scheduling.load(Ordering::SeqCst) {
                    need_retry = true;
                    break;
                }
            }
            if need_retry {
                continue;
            }

            // Switch to the highest priority thread.
            let hpt_id = hpt.lock().unwrap().thread_id;
            self.switch_thread_impl(hpt_id);

            // Check if we need scheduling again. If so, unlock and retry.
            if self.state.needs_scheduling.load(Ordering::SeqCst) {
                Self::unlock_thread_context(hpt);
                need_retry = true;
                continue;
            }

            // Success — break out of the loop.
            break;
        }

        // Reload the guest thread context.
        if let Some(ref hpt) = highest_priority_thread {
            let hpt_id = hpt.lock().unwrap().get_thread_id();
            log::info!("schedule_impl_fiber_loop: Reload + YieldTo thread {}", hpt_id);
            self.reload(hpt);

            // Yield back from switch_fiber to the newly scheduled thread's host_context.
            // Upstream: Common::Fiber::YieldTo(m_switch_fiber, *highest_priority_thread->m_host_context)
            if let Some(ref switch_fiber) = self.switch_fiber {
                let host_ctx = hpt.lock().unwrap().host_context.clone();
                if let Some(ref ctx) = host_ctx {
                    log::info!("schedule_impl_fiber_loop: yielding from switch_fiber to thread {} fiber", hpt_id);
                    Fiber::yield_to(Arc::downgrade(switch_fiber), ctx);
                } else {
                    log::warn!("schedule_impl_fiber_loop: thread {} has no host_context", hpt_id);
                }
            }
        }
    }

    /// Matches upstream `KScheduler::SwitchThread(KThread* next_thread)`.
    /// Updates CPU time tracking, previous thread, current thread.
    fn switch_thread_impl(&mut self, next_thread_id: u64) {
        let cur_thread_id = self.current_thread_id;

        log::info!(
            "switch_thread_impl: cur={:?} next={} has_gsc={}",
            cur_thread_id, next_thread_id, self.global_scheduler_context.is_some()
        );

        // If same thread, nothing to do.
        if Some(next_thread_id) == cur_thread_id {
            log::info!("switch_thread_impl: same thread, skipping");
            return;
        }

        // Update CPU time tracking.
        let prev_tick = self.last_context_switch_time;
        let cur_tick = if let Some(ref ct) = self.core_timing {
            ct.lock().unwrap().get_global_time_ns().as_nanos() as i64
        } else {
            prev_tick + 1
        };
        let tick_diff = cur_tick - prev_tick;
        self.last_context_switch_time = cur_tick;

        // Add CPU time to current thread.
        if let Some(ref cur) = self.current_thread.as_ref().and_then(Weak::upgrade) {
            cur.lock().unwrap().add_cpu_time(self.core_id, tick_diff);
        }

        // Update previous thread.
        if let Some(ref cur) = self.current_thread.as_ref().and_then(Weak::upgrade) {
            let cur_guard = cur.lock().unwrap();
            if !cur_guard.is_termination_requested()
                && cur_guard.get_active_core() == self.core_id
            {
                self.state.prev_thread_id = cur_thread_id;
            } else {
                self.state.prev_thread_id = None;
            }
        }

        // Set the new current thread.
        self.current_thread_id = Some(next_thread_id);
        if let Some(ref gsc) = self.global_scheduler_context {
            if let Some(next) = gsc.lock().unwrap().get_thread_by_thread_id(next_thread_id) {
                // Ensure the thread is on our core.
                {
                    let mut next_guard = next.lock().unwrap();
                    if next_guard.get_current_core() != self.core_id {
                        next_guard.set_current_core(self.core_id);
                    }
                }
                // Upstream: SetCurrentThread(m_kernel, next_thread);
                super::kernel::set_current_emu_thread(Some(&next));
                self.current_thread = Some(Arc::downgrade(&next));
            }
        }
    }

    /// Matches upstream `KScheduler::Unload(KThread*)`.
    /// Saves guest context and unlocks the thread's context guard.
    pub fn unload_thread(&self, thread: &Arc<Mutex<KThread>>) {
        // Upstream: m_kernel.PhysicalCore(m_core_id).SaveContext(thread)
        if let Some(core) = self.physical_cores.get(self.core_id as usize) {
            core.save_context(&mut thread.lock().unwrap());
        }

        // Unlock context guard if thread is not terminated.
        let thread_guard = thread.lock().unwrap();
        let dpc_flags = thread_guard.get_dpc();
        drop(thread_guard);
        if (dpc_flags & super::k_thread::DpcFlag::TERMINATED.bits() as u8) == 0 {
            // Upstream: thread->m_context_guard.unlock()
            Self::unlock_thread_context(thread);
        }
    }

    /// Matches upstream `KScheduler::Reload(KThread*)`.
    /// Restores guest context.
    pub fn reload_thread(&self, thread: &Arc<Mutex<KThread>>) {
        // Upstream: m_kernel.PhysicalCore(m_core_id).LoadContext(thread)
        if let Some(core) = self.physical_cores.get(self.core_id as usize) {
            core.load_context(&thread.lock().unwrap());
        }
    }

    pub fn wait_for_next_thread(
        &mut self,
        process: &Arc<Mutex<KProcess>>,
        current_thread_id: u64,
    ) -> Option<Arc<Mutex<KThread>>> {
        if self.exit_thread_if_termination_requested(process, current_thread_id) {
            self.request_schedule();
        }
        let next_thread_id = self.wait_for_next_runnable_thread(process, current_thread_id);
        let next_thread = process
            .lock()
            .unwrap()
            .get_thread_by_thread_id(next_thread_id);
        if next_thread.is_some() {
            if let Some(ref thread) = next_thread {
                self.set_scheduler_current_thread(thread);
            } else {
                self.set_scheduler_current_thread_id(next_thread_id);
            }
        }
        next_thread
    }

    /// Deprecated: linear scan for next thread. Replaced by PQ-based dispatch.
    /// Kept only for test compatibility (svc_thread tests use it directly).
    #[cfg(test)]
    pub fn select_next_thread_id(
        &mut self,
        process: &Arc<Mutex<KProcess>>,
        current_thread_id: u64,
    ) -> Option<u64> {
        if self.exit_thread_if_termination_requested(process, current_thread_id) {
            self.request_schedule();
        }
        let mut process = process.lock().unwrap();

        let mut best_thread_id = None;
        let mut best_priority = i32::MAX;
        let mut yielded_alternative_thread_id = None;
        let mut yielded_priority = i32::MAX;
        let yielded_thread_id = self.yielded_thread_id;

        for thread_id in &process.thread_list {
            let Some(thread) = process.get_thread_by_thread_id(*thread_id) else {
                continue;
            };

            let thread = thread.lock().unwrap();
            if thread.get_state() != ThreadState::RUNNABLE {
                continue;
            }

            if thread.get_priority() < best_priority {
                best_priority = thread.get_priority();
                best_thread_id = Some(thread.get_thread_id());
            } else if thread.get_priority() == best_priority {
                let replace = match best_thread_id {
                    None => true,
                    Some(best) => thread.get_thread_id() == current_thread_id && best != current_thread_id,
                };
                if replace {
                    best_thread_id = Some(thread.get_thread_id());
                }
            }

            if yielded_thread_id == Some(current_thread_id)
                && thread.get_thread_id() != current_thread_id
                && thread.get_priority() <= yielded_priority
            {
                yielded_priority = thread.get_priority();
                yielded_alternative_thread_id = Some(thread.get_thread_id());
            }
        }

        let next_thread_id = if yielded_thread_id == Some(current_thread_id) {
            match yielded_alternative_thread_id {
                Some(candidate) if yielded_priority == best_priority => Some(candidate),
                _ => best_thread_id,
            }
        } else {
            best_thread_id
        };

        if let Some(yielded_thread_id) = yielded_thread_id {
            if next_thread_id == Some(yielded_thread_id) {
                if let Some(current_thread) = process.get_thread_by_thread_id(yielded_thread_id) {
                    current_thread
                        .lock()
                        .unwrap()
                        .set_yield_schedule_count(process.get_scheduled_count());
                }
            }
        }

        self.yielded_thread_id = None;
        next_thread_id
    }
}
