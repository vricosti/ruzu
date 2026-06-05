//! Audio-core direct-fastmem-write tracing.
//!
//! Many audio_core sites bypass `Memory::write_raw` by computing
//! `addr as *mut T` from a `CpuAddr` (already host-pointer post-translation)
//! and writing through that raw pointer. These writes are invisible to the
//! `RUZU_TRACE_RAW_WRITE_AT` hook in `core::memory::memory`. This module
//! exposes a single helper, `maybe_trace_write_at`, that each direct-write
//! site calls to log a backtrace when the address covers the configured
//! GUEST vaddr.
//!
//! Address translation: the helper compares against a guest vaddr; the
//! fastmem base is registered once by `ArmDynarmic32::new` via
//! `set_fastmem_base`, after which `host_addr - fastmem_base` yields the
//! guest vaddr. Using guest vaddrs keeps the env var stable across runs
//! despite host-side ASLR shifting the fastmem mmap base.
//!
//! Usage:
//!
//!   * `RUZU_TRACE_AUDIO_GUEST_VADDR=0x70F3E828` — the guest vaddr to watch
//!   * `RUZU_FASTMEM_TRAP_PAGE=0x70F3E000` — keeps the page mapped so the
//!     host fastmem region actually exists when audio runs
//!
//! On match, a backtrace is dumped that identifies the audio_core site
//! that touched the address. Off by default. One env-var read cached in
//! a `OnceLock`; the hot path is a single atomic load + range check.

use std::sync::OnceLock;

fn target_guest() -> Option<usize> {
    static TARGET: OnceLock<Option<usize>> = OnceLock::new();
    *TARGET.get_or_init(|| {
        std::env::var("RUZU_TRACE_AUDIO_GUEST_VADDR")
            .ok()
            .and_then(|s| usize::from_str_radix(s.trim().trim_start_matches("0x"), 16).ok())
    })
}

#[inline]
pub fn maybe_trace_write_at(site: &'static str, host_addr: usize, size: usize) {
    let Some(target_guest) = target_guest() else {
        return;
    };
    let base = common::fastmem_registry::base();
    if base == 0 {
        return; // not yet registered
    }
    // host_addr might be below fastmem_base (e.g. an owned-reference site
    // we accidentally caught). In that case there's no valid guest addr.
    if host_addr < base {
        return;
    }
    let guest_addr = host_addr - base;
    if guest_addr <= target_guest && target_guest < guest_addr.saturating_add(size) {
        let bt = std::backtrace::Backtrace::force_capture();
        eprintln!(
            "[AUDIO_WRITE_AT] site={} guest=0x{:X} host=0x{:X} size={}\n{}",
            site, guest_addr, host_addr, size, bt
        );
    }
}
