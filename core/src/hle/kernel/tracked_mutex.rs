//! Drop-in wrapper around `std::sync::Mutex` that records the current holder
//! (thread id + acquisition site) for diagnostic dumping.
//!
//! Used to track lock ownership for Mutexes suspected of being held across a
//! fiber yield (ruzu's scheduler uses cooperative fibers; standard Mutex has
//! no visibility into which fiber currently owns it).
//!
//! The wrapper preserves the `.lock() -> LockResult<Guard>` /
//! `.try_lock() -> TryLockResult<Guard>` shape so existing call sites that do
//! `mutex.lock().unwrap()` compile unchanged.
//!
//! The global registry is enumerated by the SIGUSR1 thread dumper in
//! `kernel.rs`.

use std::cell::UnsafeCell;
use std::ops::{Deref, DerefMut};
use std::sync::atomic::{AtomicU64, Ordering};

/// One held lock entry — (thread id, site id, mutex address).  Fixed-size
/// array keeps the registry lock-free and async-signal-safe for the dumper.
#[derive(Clone, Copy)]
pub struct HeldEntry {
    pub thread_id: u64,
    pub site_id: u64,
    pub mutex_addr: u64,
}

const REGISTRY_SLOTS: usize = 64;

/// Global registry of currently held TrackedMutex instances.  Each slot holds
/// one (tid, site, addr) triple packed into 3 atomics; the `thread_id` being
/// non-zero marks the slot as in-use.
pub struct Registry {
    slots: [Slot; REGISTRY_SLOTS],
}

pub struct Slot {
    pub thread_id: AtomicU64,
    pub site_id: AtomicU64,
    pub mutex_addr: AtomicU64,
}

impl Slot {
    const fn new() -> Self {
        Self {
            thread_id: AtomicU64::new(0),
            site_id: AtomicU64::new(0),
            mutex_addr: AtomicU64::new(0),
        }
    }
}

pub static REGISTRY: Registry = Registry {
    slots: [
        Slot::new(), Slot::new(), Slot::new(), Slot::new(),
        Slot::new(), Slot::new(), Slot::new(), Slot::new(),
        Slot::new(), Slot::new(), Slot::new(), Slot::new(),
        Slot::new(), Slot::new(), Slot::new(), Slot::new(),
        Slot::new(), Slot::new(), Slot::new(), Slot::new(),
        Slot::new(), Slot::new(), Slot::new(), Slot::new(),
        Slot::new(), Slot::new(), Slot::new(), Slot::new(),
        Slot::new(), Slot::new(), Slot::new(), Slot::new(),
        Slot::new(), Slot::new(), Slot::new(), Slot::new(),
        Slot::new(), Slot::new(), Slot::new(), Slot::new(),
        Slot::new(), Slot::new(), Slot::new(), Slot::new(),
        Slot::new(), Slot::new(), Slot::new(), Slot::new(),
        Slot::new(), Slot::new(), Slot::new(), Slot::new(),
        Slot::new(), Slot::new(), Slot::new(), Slot::new(),
        Slot::new(), Slot::new(), Slot::new(), Slot::new(),
        Slot::new(), Slot::new(), Slot::new(), Slot::new(),
    ],
};

fn get_host_thread_id() -> u64 {
    // Use gettid() on Linux for a stable numeric id.
    #[cfg(target_os = "linux")]
    unsafe {
        libc::gettid() as u64
    }
    #[cfg(not(target_os = "linux"))]
    {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::Hasher;
        let mut h = DefaultHasher::new();
        h.write_u64(unsafe {
            std::mem::transmute::<_, u64>(std::thread::current().id())
        });
        h.finish()
    }
}

/// Record that a TrackedMutex at `mutex_addr` was just acquired.
/// Returns the slot index used (for fast release).
fn register_acquire(mutex_addr: u64, site_id: u64) -> usize {
    let tid = get_host_thread_id();
    for (i, slot) in REGISTRY.slots.iter().enumerate() {
        if slot
            .thread_id
            .compare_exchange(0, tid, Ordering::Acquire, Ordering::Relaxed)
            .is_ok()
        {
            slot.site_id.store(site_id, Ordering::Relaxed);
            slot.mutex_addr.store(mutex_addr, Ordering::Release);
            return i;
        }
    }
    // Registry full — return sentinel; release is a no-op.
    usize::MAX
}

fn register_release(slot_index: usize) {
    if slot_index >= REGISTRY.slots.len() {
        return;
    }
    let slot = &REGISTRY.slots[slot_index];
    slot.mutex_addr.store(0, Ordering::Relaxed);
    slot.site_id.store(0, Ordering::Relaxed);
    slot.thread_id.store(0, Ordering::Release);
}

/// Dump the full registry to stderr.  Called from the SIGUSR1 dumper.
/// Async-signal-safe (only atomic loads + write(2)).
pub fn dump_registry() {
    eprintln!("[DUMP] TrackedMutex holders:");
    let mut any = false;
    for (i, slot) in REGISTRY.slots.iter().enumerate() {
        let tid = slot.thread_id.load(Ordering::Acquire);
        if tid == 0 {
            continue;
        }
        let site = slot.site_id.load(Ordering::Relaxed);
        let addr = slot.mutex_addr.load(Ordering::Relaxed);
        // Render site as ASCII if it looks like 4-byte ASCII text; else hex.
        let site_bytes = (site as u32).to_be_bytes();
        let site_disp = if site_bytes.iter().all(|b| b.is_ascii_graphic() || *b == b'_') {
            format!(
                "{:?}",
                std::str::from_utf8(&site_bytes).unwrap_or("?")
            )
        } else {
            format!("0x{:08X}", site as u32)
        };
        eprintln!(
            "[DUMP]   slot[{}] host_tid={} site={} mutex_addr=0x{:X}",
            i, tid, site_disp, addr,
        );
        any = true;
    }
    if !any {
        eprintln!("[DUMP]   (no TrackedMutex currently held)");
    }
}

/// A Mutex wrapper that records its holder in the global REGISTRY.
pub struct TrackedMutex<T: ?Sized> {
    inner: parking_lot::Mutex<TrackedInner<T>>,
}

struct TrackedInner<T: ?Sized> {
    value: UnsafeCell<T>,
}

impl<T> TrackedMutex<T> {
    pub const fn new(value: T) -> Self {
        Self {
            inner: parking_lot::const_mutex(TrackedInner {
                value: UnsafeCell::new(value),
            }),
        }
    }

    pub fn from_value(value: T) -> Self {
        Self {
            inner: parking_lot::Mutex::new(TrackedInner {
                value: UnsafeCell::new(value),
            }),
        }
    }
}

impl<T: ?Sized> TrackedMutex<T> {
    pub fn lock(&self) -> std::sync::LockResult<TrackedMutexGuard<'_, T>> {
        // Associate the lock with the *current site id*, where "current site"
        // is ALWAYS the default for unlabelled call sites.  Call sites that
        // want a specific ID can call `lock_with(site_id)` instead.
        self.lock_with(0)
    }

    pub fn lock_with(&self, site_id: u64) -> std::sync::LockResult<TrackedMutexGuard<'_, T>> {
        let mutex_addr = self as *const Self as *const () as usize as u64;
        let guard = self.inner.lock();
        let slot = register_acquire(mutex_addr, site_id);
        Ok(TrackedMutexGuard {
            guard: std::mem::ManuallyDrop::new(guard),
            slot,
        })
    }

    pub fn try_lock(&self) -> std::sync::TryLockResult<TrackedMutexGuard<'_, T>> {
        let mutex_addr = self as *const Self as *const () as usize as u64;
        match self.inner.try_lock() {
            Some(guard) => {
                let slot = register_acquire(mutex_addr, 0);
                Ok(TrackedMutexGuard {
                    guard: std::mem::ManuallyDrop::new(guard),
                    slot,
                })
            }
            None => Err(std::sync::TryLockError::WouldBlock),
        }
    }
}

// SAFETY: TrackedInner<T> only contains an UnsafeCell<T>, and all access to the
// inner T is mediated by the parking_lot::Mutex.
unsafe impl<T: Send> Send for TrackedInner<T> {}
unsafe impl<T: Send> Sync for TrackedInner<T> {}

pub struct TrackedMutexGuard<'a, T: ?Sized + 'a> {
    guard: std::mem::ManuallyDrop<parking_lot::MutexGuard<'a, TrackedInner<T>>>,
    slot: usize,
}

impl<'a, T: ?Sized> Deref for TrackedMutexGuard<'a, T> {
    type Target = T;
    fn deref(&self) -> &T {
        unsafe { &*self.guard.value.get() }
    }
}

impl<'a, T: ?Sized> DerefMut for TrackedMutexGuard<'a, T> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe { &mut *self.guard.value.get() }
    }
}

impl<'a, T: ?Sized> Drop for TrackedMutexGuard<'a, T> {
    fn drop(&mut self) {
        // Release registry BEFORE parking_lot guard drops — so a dump running
        // immediately after we relinquish the lock sees a clean registry.
        register_release(self.slot);
        unsafe { std::mem::ManuallyDrop::drop(&mut self.guard) };
    }
}

impl<T: Default> Default for TrackedMutex<T> {
    fn default() -> Self {
        Self::from_value(T::default())
    }
}
