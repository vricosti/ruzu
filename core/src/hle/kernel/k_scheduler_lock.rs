//! Port of zuyu/src/core/hle/kernel/k_scheduler_lock.h
//! Status: EN COURS
//! Derniere synchro: 2026-03-16
//!
//! KAbstractSchedulerLock — recursive lock that disables/enables scheduling.
//! Parameterized by a SchedulerType that provides DisableScheduling,
//! EnableScheduling, and UpdateHighestPriorityThreads static methods.
//!
//! Upstream uses a template `KAbstractSchedulerLock<SchedulerType>`.
//! Since there's only one SchedulerType (KScheduler), we use function
//! pointers set at initialization to break the circular dependency.

use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::OnceLock;

use super::k_spin_lock::KAlignedSpinLock;

fn should_trace_scheduler_lock() -> bool {
    static ENABLED: OnceLock<bool> = OnceLock::new();
    *ENABLED.get_or_init(|| {
        std::env::var_os("RUZU_TRACE_SCHED_LOCK").is_some()
            || std::env::var_os("RUZU_TRACE_SLEEP").is_some()
    })
}

fn should_trace_scheduler_lock_strict() -> bool {
    static ENABLED: OnceLock<bool> = OnceLock::new();
    *ENABLED.get_or_init(|| std::env::var_os("RUZU_TRACE_SCHED_LOCK").is_some())
}

fn should_trace_scheduler_lock_owner(owner: u64) -> bool {
    if !should_trace_scheduler_lock_strict() {
        return false;
    }
    static OWNERS: OnceLock<Option<Vec<u64>>> = OnceLock::new();
    OWNERS
        .get_or_init(|| {
            std::env::var("RUZU_TRACE_SCHED_LOCK_OWNER")
                .ok()
                .map(|spec| {
                    spec.split(',')
                        .filter_map(|raw| raw.trim().parse::<u64>().ok())
                        .collect()
                })
        })
        .as_ref()
        .is_some_and(|owners| owners.iter().copied().any(|id| id == owner))
}

fn should_trace_scheduler_lock_ring() -> bool {
    static ENABLED: OnceLock<bool> = OnceLock::new();
    *ENABLED.get_or_init(|| std::env::var_os("RUZU_TRACE_SCHED_LOCK_RING").is_some())
}

fn trace_scheduler_lock_ring(stage: u64, owner_sched_id: u64, cores: u64, lock_count: i32) {
    if !should_trace_scheduler_lock_ring() {
        return;
    }
    if !common::trace::is_enabled(common::trace::cat::SCHED_STATE) {
        return;
    }
    let guest_tid = super::kernel::get_current_thread_id_fast().unwrap_or(u64::MAX);
    common::trace::emit_raw(
        common::trace::cat::SCHED_STATE,
        &[
            stage,
            owner_sched_id,
            guest_tid,
            cores,
            lock_count as u32 as u64,
        ],
    );
}

fn trace_scheduler_lock_context(label: &str, current_id: u64, owner: u64, elapsed_us: u128) {
    let current_tid = super::kernel::get_current_thread_id_fast();
    let current_core = super::kernel::with_current_thread_fast_mut(|t| t.get_current_core());
    let svc: Vec<String> = (0..super::kernel::SVC_IN_PROGRESS.len())
        .map(|core| {
            let packed = super::kernel::SVC_IN_PROGRESS[core].load(Ordering::Acquire);
            let tid = packed >> 32;
            let imm = packed & 0xFFFF_FFFF;
            let pc = super::kernel::GUEST_PC[core].load(Ordering::Acquire);
            let lr = super::kernel::GUEST_LR[core].load(Ordering::Acquire);
            let sp = super::kernel::GUEST_SP[core].load(Ordering::Acquire);
            format!(
                "c{}:tid={} svc=0x{:X} pc=0x{:X} lr=0x{:X} sp=0x{:X}",
                core, tid, imm, pc, lr, sp
            )
        })
        .collect();

    eprintln!(
        "[SCHED_LOCK] {} elapsed_us={} current_tid={:?} current_core={:?} current_sched_id={} owner={} svc=[{}]",
        label,
        elapsed_us,
        current_tid,
        current_core,
        current_id,
        owner,
        svc.join(" | "),
    );
}
/// Counter for synthetic host-thread IDs. Reserved range: `>= 2^63` so it
/// never collides with guest KThread IDs (those are allocated starting at
/// small positive values by the kernel thread-id allocator).
static NEXT_HOST_SCHED_ID: AtomicU64 = AtomicU64::new(1u64 << 63);

std::thread_local! {
    /// Per-OS-thread synthetic scheduler ID, allocated lazily on first
    /// `current_sched_thread_id()` call from a host thread. Stays stable
    /// for the thread's lifetime.
    static HOST_SCHED_ID: std::cell::Cell<u64> = const { std::cell::Cell::new(0) };
}

/// Returns a unique thread identifier for scheduler-lock ownership.
///
/// On guest (emu) threads: returns the guest KThread ID via the
/// `get_current_thread_id_fast` thread-local cache (matching upstream's
/// `KThread*` comparison).
///
/// On host threads (audio HLE, SDL, CoreTiming, event-signal callers,
/// etc.): returns a synthetic ID from a reserved `>= 2^63` range,
/// allocated lazily per OS thread. Each host thread gets a unique ID so
/// `is_locked_by_current_thread()` works correctly for them too.
///
/// This fixes a classic false positive where multiple host threads all
/// landed in the same fallback branch (`m_lock_count > 0`) and each
/// incorrectly believed it owned the scheduler lock, bypassing the
/// spin-lock acquire and racing on the owner's state.
fn current_sched_thread_id() -> u64 {
    if let Some(id) = super::kernel::get_current_thread_id_fast() {
        return id;
    }
    HOST_SCHED_ID.with(|cell| {
        let existing = cell.get();
        if existing != 0 {
            existing
        } else {
            let id = NEXT_HOST_SCHED_ID.fetch_add(1, Ordering::Relaxed);
            cell.set(id);
            id
        }
    })
}

/// Callbacks matching upstream SchedulerType template parameter static methods.
/// Set during initialization to break the KScheduler <-> GlobalSchedulerContext cycle.
pub struct SchedulerCallbacks {
    pub disable_scheduling: fn(),
    pub enable_scheduling: fn(u64),
    pub update_highest_priority_threads: fn() -> u64,
}

/// DisableScheduling callback matching upstream `KScheduler::DisableScheduling(kernel)`.
/// Increments the current thread's disable_dispatch_count via the thread-local.
fn default_disable_scheduling() {
    super::kernel::with_current_thread_fast_mut(|t| {
        debug_assert!(t.get_disable_dispatch_count() >= 0);
        t.disable_dispatch();
    });
}

/// EnableScheduling callback matching upstream `KScheduler::EnableScheduling(kernel, cores_needing_scheduling)`.
/// Decrements dispatch count. If it reaches 0, triggers rescheduling.
/// Port of upstream `KScheduler::EnableScheduling(kernel, cores_needing_scheduling)`.
///
/// When the scheduler lock's outermost scope exits (lock_count goes to 0),
/// this callback runs. If the current thread's dispatch count is about to
/// reach 0, upstream calls `scheduler->RescheduleCurrentCore()` which
/// triggers `ScheduleImpl()` → fiber switch. This is the mechanism that
/// suspends a WAITING thread after `KScopedSchedulerLockAndSleep` drops.
fn default_enable_scheduling(cores_needing_scheduling: u64) {
    let dispatch_count =
        super::kernel::with_current_thread_fast_mut(|t| t.get_disable_dispatch_count());

    match dispatch_count {
        Some(count) if count > 1 => {
            // Nested lock — just decrement.
            super::kernel::with_current_thread_fast_mut(|t| {
                t.enable_dispatch();
            });
        }
        Some(1) => {
            // Outermost unlock — port upstream
            // `KScheduler::EnableScheduling(kernel, cores_needing_scheduling)`
            // literally enough to preserve the no-scheduler/host-thread path.
            if let Some(kernel) = super::kernel::get_kernel_ref() {
                let scheduler_arc = kernel.current_scheduler().cloned();
                super::k_scheduler::KScheduler::enable_scheduling_with_scheduler(
                    cores_needing_scheduling,
                    scheduler_arc.as_ref(),
                    kernel.is_phantom_mode_for_single_core(),
                );
            } else {
                // No kernel available in local test harnesses.
                super::kernel::with_current_thread_fast_mut(|t| {
                    t.enable_dispatch();
                });
            }
        }
        _ => {
            // No current thread (host thread) — just a no-op.
        }
    }
}

/// UpdateHighestPriorityThreads callback matching upstream
/// `KScheduler::UpdateHighestPriorityThreads(kernel)`.
/// Checks IsSchedulerUpdateNeeded and calls UpdateHighestPriorityThreadsImpl.
/// Returns bitmask of cores needing rescheduling.
fn default_update_highest_priority_threads() -> u64 {
    // This callback is called from within the scheduler lock's Unlock().
    // It needs access to the GlobalSchedulerContext to check the update flag
    // and run UpdateHighestPriorityThreadsImpl.
    // The actual implementation is wired through the SchedulerCallbacks
    // set by the kernel when it has the necessary context.
    // Default: return 0 (no cores need rescheduling).
    0
}

static DEFAULT_CALLBACKS: SchedulerCallbacks = SchedulerCallbacks {
    disable_scheduling: default_disable_scheduling,
    enable_scheduling: default_enable_scheduling,
    update_highest_priority_threads: default_update_highest_priority_threads,
};

/// KAbstractSchedulerLock — recursive scheduler lock.
///
/// Mirrors upstream `Kernel::KAbstractSchedulerLock<SchedulerType>`.
/// Uses a spinlock for the underlying mutual exclusion, with a recursive
/// lock count and owner thread tracking.
///
/// Uses interior mutability: lock()/unlock() take &self to allow
/// the scoped guard (KScopedSchedulerLock) to work without &mut.
pub struct KAbstractSchedulerLock {
    m_spin_lock: KAlignedSpinLock,
    /// Mutable state protected by the spin lock.
    m_lock_count: std::cell::Cell<i32>,
    /// Owner thread id (0 = no owner).
    /// Upstream: std::atomic<KThread*> m_owner_thread
    m_owner_thread: AtomicU64,
    /// Callbacks to KScheduler static methods.
    callbacks: &'static SchedulerCallbacks,
}

// SAFETY: KAbstractSchedulerLock uses a spinlock for synchronization.
// The Cell<i32> is only accessed while the spinlock is held.
unsafe impl Send for KAbstractSchedulerLock {}
unsafe impl Sync for KAbstractSchedulerLock {}

impl KAbstractSchedulerLock {
    pub fn new() -> Self {
        Self {
            m_spin_lock: KAlignedSpinLock::new(),
            m_lock_count: std::cell::Cell::new(0),
            m_owner_thread: AtomicU64::new(0),
            callbacks: &DEFAULT_CALLBACKS,
        }
    }

    /// Set the scheduler callbacks. Called once KScheduler static methods are ready.
    pub fn set_callbacks(&mut self, callbacks: &'static SchedulerCallbacks) {
        self.callbacks = callbacks;
    }

    /// Check if the lock is held by the current thread.
    /// Upstream: compares `m_owner_thread` with `GetCurrentThreadPointer(m_kernel)`.
    ///
    /// Ruzu uses the guest KThread ID for guest threads and a synthetic ID
    /// for host threads (see `current_sched_thread_id()`). Both are
    /// compared against the atomic `m_owner_thread` — never reads
    /// `m_lock_count` (which is `Cell<i32>` and only safe on the owner).
    pub fn is_locked_by_current_thread(&self) -> bool {
        let owner = self.m_owner_thread.load(Ordering::Relaxed);
        if owner == 0 {
            return false;
        }
        owner == current_sched_thread_id()
    }

    /// Lock the scheduler lock.
    /// Matches upstream `KAbstractSchedulerLock::Lock()`.
    pub fn lock(&self) {
        let current_tid = super::kernel::get_current_thread_id_fast();
        let current_sched_id = current_sched_thread_id();
        log::trace!(
            "KAbstractSchedulerLock::lock enter current_tid={:?} owner={} count={}",
            current_tid,
            self.m_owner_thread.load(Ordering::Relaxed),
            self.m_lock_count.get()
        );
        if self.m_owner_thread.load(Ordering::Relaxed) == current_sched_id {
            debug_assert!(self.m_lock_count.get() > 0);
            if should_trace_scheduler_lock_owner(current_sched_id) {
                trace_scheduler_lock_context("recursive", current_sched_id, current_sched_id, 0);
            }
        } else {
            log::trace!(
                "KAbstractSchedulerLock::lock current_tid={:?} before disable_scheduling",
                current_tid
            );
            (self.callbacks.disable_scheduling)();
            log::trace!(
                "KAbstractSchedulerLock::lock current_tid={:?} before spin_lock",
                current_tid
            );
            if should_trace_scheduler_lock() {
                let start = std::time::Instant::now();
                let mut next_log_us = 1_000u128;
                loop {
                    if self.m_spin_lock.try_lock() {
                        let elapsed_us = start.elapsed().as_micros();
                        if elapsed_us >= 1_000 {
                            trace_scheduler_lock_context(
                                "acquired-after-wait",
                                current_sched_id,
                                self.m_owner_thread.load(Ordering::Acquire),
                                elapsed_us,
                            );
                        }
                        break;
                    }

                    let elapsed_us = start.elapsed().as_micros();
                    if elapsed_us >= next_log_us {
                        trace_scheduler_lock_context(
                            "waiting",
                            current_sched_id,
                            self.m_owner_thread.load(Ordering::Acquire),
                            elapsed_us,
                        );
                        next_log_us = elapsed_us + 1_000_000;
                    }
                    std::thread::yield_now();
                }
            } else {
                self.m_spin_lock.lock();
            }
            log::trace!(
                "KAbstractSchedulerLock::lock current_tid={:?} acquired spin_lock",
                current_tid
            );

            debug_assert!(self.m_lock_count.get() == 0);
            debug_assert!(self.m_owner_thread.load(Ordering::Relaxed) == 0);

            // Upstream: m_owner_thread = GetCurrentThreadPointer(m_kernel).
            // Use `current_sched_thread_id()` — returns the guest tid or a
            // unique per-OS-thread synthetic ID for host threads.
            self.m_owner_thread
                .store(current_sched_id, Ordering::Relaxed);
            if should_trace_scheduler_lock_owner(current_sched_id) {
                trace_scheduler_lock_context("acquire", current_sched_id, current_sched_id, 0);
            }
        }

        self.m_lock_count.set(self.m_lock_count.get() + 1);
        log::trace!(
            "KAbstractSchedulerLock::lock exit current_tid={:?} owner={} count={}",
            current_tid,
            self.m_owner_thread.load(Ordering::Relaxed),
            self.m_lock_count.get()
        );
    }

    /// Unlock the scheduler lock.
    /// Matches upstream `KAbstractSchedulerLock::Unlock()`.
    pub fn unlock(&self) {
        debug_assert!(self.is_locked_by_current_thread());
        debug_assert!(self.m_lock_count.get() > 0);

        log::trace!(
            "KAbstractSchedulerLock::unlock enter owner={} count={}",
            self.m_owner_thread.load(Ordering::Relaxed),
            self.m_lock_count.get()
        );
        let new_count = self.m_lock_count.get() - 1;
        self.m_lock_count.set(new_count);

        if new_count == 0 {
            std::sync::atomic::fence(Ordering::SeqCst);

            let cores_needing_scheduling = (self.callbacks.update_highest_priority_threads)();
            let previous_owner = current_sched_thread_id();
            trace_scheduler_lock_ring(6, previous_owner, cores_needing_scheduling, new_count);
            log::trace!(
                "KAbstractSchedulerLock::unlock zero-count cores_needing_scheduling=0x{:x}",
                cores_needing_scheduling
            );

            self.m_owner_thread.store(0, Ordering::Relaxed);
            if should_trace_scheduler_lock_owner(previous_owner) {
                trace_scheduler_lock_context(
                    "release-before-spin-unlock",
                    previous_owner,
                    previous_owner,
                    0,
                );
            }
            self.m_spin_lock.unlock();
            log::trace!("KAbstractSchedulerLock::unlock before enable_scheduling");
            trace_scheduler_lock_ring(7, previous_owner, cores_needing_scheduling, new_count);
            (self.callbacks.enable_scheduling)(cores_needing_scheduling);
            log::trace!("KAbstractSchedulerLock::unlock after enable_scheduling");
        }
    }

    pub fn get_lock_count(&self) -> i32 {
        self.m_lock_count.get()
    }

    /// Get the update_highest_priority_threads callback, if wired.
    /// Returns None if callbacks use the default (noop).
    pub fn get_update_callback(&self) -> Option<fn() -> u64> {
        let f = self.callbacks.update_highest_priority_threads;
        // Check if it's the default noop (returns 0 always).
        // We can't distinguish at runtime, so always return it.
        Some(f)
    }
}

impl Default for KAbstractSchedulerLock {
    fn default() -> Self {
        Self::new()
    }
}

/// KScopedSchedulerLock — RAII wrapper for the scheduler lock.
/// Matches upstream `KScopedSchedulerLock` (typedef of KScopedLock<KSchedulerLockType>).
pub struct KScopedSchedulerLock<'a> {
    lock: &'a KAbstractSchedulerLock,
    _lo: common::lock_order::LockOrderGuard,
}

impl<'a> KScopedSchedulerLock<'a> {
    pub fn new(lock: &'a KAbstractSchedulerLock) -> Self {
        let _lo = common::lock_order::guard("scheduler");
        lock.lock();
        Self { lock, _lo }
    }
}

impl Drop for KScopedSchedulerLock<'_> {
    fn drop(&mut self) {
        self.lock.unlock();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scheduler_lock_basic() {
        let lock = KAbstractSchedulerLock::new();
        assert_eq!(lock.get_lock_count(), 0);
        lock.lock();
        assert_eq!(lock.get_lock_count(), 1);
        lock.unlock();
        assert_eq!(lock.get_lock_count(), 0);
    }

    #[test]
    fn test_scheduler_lock_recursive() {
        let lock = KAbstractSchedulerLock::new();
        lock.lock();
        assert_eq!(lock.get_lock_count(), 1);
        lock.lock();
        assert_eq!(lock.get_lock_count(), 2);
        lock.unlock();
        assert_eq!(lock.get_lock_count(), 1);
        lock.unlock();
        assert_eq!(lock.get_lock_count(), 0);
    }

    #[test]
    fn test_scoped_scheduler_lock() {
        let lock = KAbstractSchedulerLock::new();
        assert_eq!(lock.get_lock_count(), 0);
        {
            let _guard = KScopedSchedulerLock::new(&lock);
            assert_eq!(lock.get_lock_count(), 1);
        }
        assert_eq!(lock.get_lock_count(), 0);
    }
}
