//! Process-global registry for the JIT fastmem arena base address.
//!
//! Both `core` (arm_dynarmic) and `audio_core` need access to the fastmem
//! base to translate between host pointers and guest vaddrs. `core`
//! depends on `common` and `audio_core` depends on `common`, but
//! `core` doesn't depend on `audio_core` (and vice-versa); putting the
//! tiny registry here keeps the dependency graph clean.
//!
//! The base is set once by `ArmDynarmic32::new` after the JIT resolves
//! its fastmem pointer; subsequent cores call `set` with the same value
//! (idempotent — the first non-zero base wins).

use std::sync::atomic::{AtomicUsize, Ordering};

static FASTMEM_BASE: AtomicUsize = AtomicUsize::new(0);

/// Set the fastmem base. Idempotent: only the first non-zero call wins.
pub fn set(base: usize) {
    let _ = FASTMEM_BASE.compare_exchange(0, base, Ordering::Release, Ordering::Relaxed);
}

/// Get the fastmem base. Returns 0 if not yet set.
pub fn base() -> usize {
    FASTMEM_BASE.load(Ordering::Acquire)
}
