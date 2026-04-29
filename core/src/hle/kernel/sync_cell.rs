//! Upstream-faithful kernel-object cell — `UnsafeCell<T>` serialized by
//! `KScopedSchedulerLock` instead of a sleeping `Mutex<T>`.
//!
//! ## Why this exists
//!
//! Upstream zuyu's kernel protects `KThread` / `KProcess` fields with a
//! single scheduler spin-lock (`KScopedSchedulerLock`), accessed through
//! raw references (`KThread*`, `KProcess&`). No per-object sleeping mutex.
//!
//! The Rust port originally wrapped these objects in
//! `KThreadLock` / `TrackedMutex<KProcess>`. Combined with
//! cooperative fibers (which can yield while holding a sleeping mutex),
//! this creates the entire class of AB-BA / self-lock deadlocks documented
//! in `project_mk8d_arbitrate_unlock_deadlock.md` and
//! `project_mk8d_lock_refactor_attempts_all_regressed.md`.
//!
//! `SyncCell<T>` is the upstream-faithful replacement: `UnsafeCell<T>`
//! with two access paths:
//!
//!  1. `with(&scheduler_guard, |t| ...)` — safe, takes a
//!     `KScopedSchedulerLock` as type-level witness that the scheduler
//!     spin-lock is held (which serializes all access across cores).
//!  2. `get_mut_unchecked()` — raw `unsafe fn` for hot paths in the
//!     scheduler / interrupt-handler that already know the spin-lock is
//!     held and don't want to thread a guard through deeply nested calls.
//!
//! ## When to use
//!
//! **Phase 3+ of the lock refactor.** Until all sites are migrated in one
//! commit, `SyncCell<T>` remains scaffolding — defined but unused.
//! Partial migration races against unconverted `Mutex<T>` sites because
//! `KScopedSchedulerLock` and `parking_lot::Mutex` don't know about each
//! other.
//!
//! ## Type aliases
//!
//! `KProcessCell` and `KThreadCell` are the concrete instantiations used
//! by the kernel. Keeping them named so call sites read naturally
//! (`KProcessCell::new(...)` matches upstream's `KProcess` construction).

use std::cell::UnsafeCell;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use std::sync::{LockResult, TryLockResult};

use super::k_scheduler_lock::KScopedSchedulerLock;

/// Generic upstream-faithful cell. See module-level docs.
pub struct SyncCell<T: ?Sized> {
    inner: UnsafeCell<T>,
}

// SAFETY: All access to `inner` is gated on the scheduler spin-lock being
// held (either via `with` taking a `&KScopedSchedulerLock` as witness, or
// via `unsafe get_mut_unchecked` whose safety contract is the same). The
// scheduler spin-lock provides mutual exclusion across all host threads,
// matching upstream C++'s `KScopedSchedulerLock` semantics. `T` itself
// must be `Send` for the Arc<SyncCell<T>> to be shared across threads —
// enforced by the `Send` bound below.
unsafe impl<T: ?Sized + Send> Send for SyncCell<T> {}
unsafe impl<T: ?Sized + Send> Sync for SyncCell<T> {}

impl<T> SyncCell<T> {
    /// Construct a new cell wrapping `value`. Not const-friendly because
    /// `KProcess` / `KThread` constructors aren't; upstream allocates them
    /// with `new` at runtime too.
    #[inline]
    pub fn new(value: T) -> Self {
        Self {
            inner: UnsafeCell::new(value),
        }
    }

    /// Safe access under a scheduler-lock witness. The `_guard` parameter
    /// isn't touched at runtime — its sole purpose is to encode, in the
    /// type system, that the caller currently holds `KScopedSchedulerLock`.
    /// Since the scheduler lock is the emulator's single serialization
    /// primitive (mirroring upstream), holding it implies exclusive access
    /// to ALL cells that contain kernel objects.
    ///
    /// Returns whatever `f` returns.
    #[inline]
    pub fn with<R>(&self, _guard: &KScopedSchedulerLock<'_>, f: impl FnOnce(&mut T) -> R) -> R {
        // SAFETY: `_guard`'s existence proves the scheduler spin-lock is
        // held (it was constructed by locking and cannot be forged without
        // unsafe). Therefore no other host thread can be accessing this
        // cell through `with` / `get_mut_unchecked` concurrently.
        //
        // The `&mut T` lifetime is bound by the closure, so it cannot
        // leak past the lock-held window: when `f` returns, the &mut T
        // is dropped before `with` returns.
        unsafe { f(&mut *self.inner.get()) }
    }

    /// Raw access. The caller asserts the scheduler spin-lock is held.
    /// Intended for tight, performance-sensitive spots in the scheduler
    /// itself where threading a `&KScopedSchedulerLock` through nested
    /// helpers would be awkward.
    ///
    /// # Safety
    ///
    /// Caller must ensure the scheduler spin-lock is held for the entire
    /// duration of the returned `&mut T`. Violating this is undefined
    /// behaviour (data race with any other host thread accessing the cell).
    #[inline]
    pub unsafe fn get_mut_unchecked(&self) -> &mut T {
        &mut *self.inner.get()
    }

    /// Raw access without any lock requirement. **Test-only.** Exists so
    /// unit tests can construct isolated `SyncCell<T>` without wiring up
    /// a full kernel + scheduler lock. Not gated on `#[cfg(test)]` so
    /// integration/doc tests can use it, but the name is deliberate.
    ///
    /// # Safety
    ///
    /// Caller must ensure no concurrent access to this cell. The typical
    /// use is a single test thread that constructed the cell locally and
    /// never shared it.
    #[inline]
    pub unsafe fn test_access<R>(&self, f: impl FnOnce(&mut T) -> R) -> R {
        f(&mut *self.inner.get())
    }

    /// Consume the cell and return its inner value. Useful for teardown
    /// in tests and for moving ownership during late-kernel-shutdown.
    #[inline]
    pub fn into_inner(self) -> T {
        self.inner.into_inner()
    }

    /// Return a raw pointer to the contained value. Mirrors
    /// `parking_lot::Mutex::data_ptr` and upstream's `KThread*` /
    /// `KProcess*` used by the scheduler for bulk cross-thread work that
    /// never dereferences without holding the scheduler lock.
    ///
    /// The pointer is valid for the lifetime of `self`.
    #[inline]
    pub fn as_ptr(&self) -> *mut T {
        self.inner.get()
    }

    // ----- API-compatibility shim with `TrackedMutex<T>` / `Mutex<T>` -----
    //
    // These methods exist purely so that the atomic type swap
    // (`Arc<TrackedMutex<KProcess>>` → `Arc<SyncCell<KProcess>>`) doesn't
    // require touching ~313 `.lock().unwrap()` call sites in the same
    // commit. The methods return guards that do **no** runtime locking;
    // they assume the scheduler spin-lock provides serialization
    // externally (matching upstream's raw-reference access model).
    //
    // Until every caller is also wrapped in `KScopedSchedulerLock`,
    // these guards are a contract — broken contracts are UB. Step 5
    // (this commit) accepts that intermediate breakage; later steps
    // tighten coverage.

    /// API-compat with `Mutex::lock`. Returns a guard that derefs to
    /// `&mut T` via the underlying UnsafeCell.
    ///
    /// Semantics: assumes the scheduler spin-lock is held by the caller.
    /// No actual locking happens here.
    #[inline]
    pub fn lock(&self) -> LockResult<SyncCellGuard<'_, T>> {
        Ok(SyncCellGuard {
            ptr: self.inner.get(),
            _marker: PhantomData,
        })
    }

    /// API-compat with `TrackedMutex::lock_with`. The `_site_id` is
    /// unused under the new model (the dump infrastructure rooted in
    /// per-Mutex tracking goes away once `TrackedMutex` is retired).
    /// Kept so call sites that opt into site labels keep compiling.
    #[inline]
    pub fn lock_with(&self, _site_id: u64) -> LockResult<SyncCellGuard<'_, T>> {
        self.lock()
    }

    /// API-compat with `Mutex::try_lock`. Always succeeds — there is no
    /// actual lock to contend on.
    #[inline]
    pub fn try_lock(&self) -> TryLockResult<SyncCellGuard<'_, T>> {
        Ok(SyncCellGuard {
            ptr: self.inner.get(),
            _marker: PhantomData,
        })
    }

    /// API-compat with `TrackedMutex::from_value`. Equivalent to `new`.
    #[inline]
    pub fn from_value(value: T) -> Self {
        Self::new(value)
    }
}

/// Guard returned by `SyncCell::lock` / `try_lock` / `lock_with`.
///
/// Holds no actual lock — see `SyncCell::lock` docs. Exists to
/// preserve the `.lock().unwrap()` syntactic shape of `Mutex` /
/// `TrackedMutex` call sites without runtime overhead.
pub struct SyncCellGuard<'a, T: ?Sized + 'a> {
    ptr: *mut T,
    _marker: PhantomData<&'a mut T>,
}

// SAFETY: the guard is logically a `&mut T` whose validity is gated on
// the scheduler spin-lock (caller's contract). `T: Send` is required for
// the same reason `Arc<SyncCell<T>>` requires it: the underlying value
// can be observed from any host thread under the spin-lock.
unsafe impl<T: ?Sized + Send> Send for SyncCellGuard<'_, T> {}
unsafe impl<T: ?Sized + Sync> Sync for SyncCellGuard<'_, T> {}

impl<T: ?Sized> Deref for SyncCellGuard<'_, T> {
    type Target = T;
    #[inline]
    fn deref(&self) -> &T {
        // SAFETY: the guard contract assumes the scheduler spin-lock is
        // held; under that contract, no concurrent mutation can occur
        // through any other access path (`with`, `get_mut_unchecked`,
        // another `SyncCellGuard`).
        unsafe { &*self.ptr }
    }
}

impl<T: ?Sized> DerefMut for SyncCellGuard<'_, T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut T {
        // SAFETY: see Deref. `&mut self` ensures no other reference
        // exists through this guard; the scheduler spin-lock contract
        // ensures none through any other guard.
        unsafe { &mut *self.ptr }
    }
}

/// Concrete alias for `KProcess` storage. Will replace
/// `Arc<TrackedMutex<KProcess>>` in Phase 3 of the lock refactor.
pub type KProcessCell = SyncCell<super::k_process::KProcess>;

/// Concrete alias for `KThread` storage. Will replace
/// `Arc<KThreadLock>` in Phase 3 of the lock refactor.
pub type KThreadCell = SyncCell<super::k_thread::KThread>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sync_cell_test_access_returns_inner_mut() {
        let cell = SyncCell::new(0u64);
        let doubled = unsafe {
            cell.test_access(|n| {
                *n = 42;
                *n * 2
            })
        };
        assert_eq!(doubled, 84);
        let observed = unsafe { cell.test_access(|n| *n) };
        assert_eq!(observed, 42);
    }

    #[test]
    fn sync_cell_with_under_scheduler_lock_runs_closure() {
        let lock = super::super::k_scheduler_lock::KAbstractSchedulerLock::new();
        let cell = SyncCell::new(10u32);
        let guard = KScopedSchedulerLock::new(&lock);
        let out = cell.with(&guard, |n| {
            *n += 1;
            *n
        });
        assert_eq!(out, 11);
        drop(guard);
    }

    #[test]
    fn sync_cell_into_inner_moves_value() {
        let cell = SyncCell::new(String::from("hello"));
        let s = cell.into_inner();
        assert_eq!(s, "hello");
    }

    #[test]
    fn sync_cell_as_ptr_roundtrips() {
        let cell = SyncCell::new(7i32);
        let p = cell.as_ptr();
        // SAFETY: single-threaded test, cell outlives this scope.
        unsafe {
            *p = 9;
        }
        let v = unsafe { cell.test_access(|n| *n) };
        assert_eq!(v, 9);
    }
}
