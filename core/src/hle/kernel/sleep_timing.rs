//! Diagnostic-only: measure actual wake vs requested ns for SleepThread SVCs.
//!
//! Enabled when env var `RUZU_TRACE_SLEEP_TIMING` is set. Has no upstream
//! counterpart; used to disambiguate "sleep-poll code path" vs "timer fires
//! too fast" as the cause of excessive SleepThread rates.

use std::collections::HashMap;
use std::sync::{Mutex, OnceLock};
use std::time::Instant;

fn map() -> &'static Mutex<HashMap<u64, (Instant, i64)>> {
    static M: OnceLock<Mutex<HashMap<u64, (Instant, i64)>>> = OnceLock::new();
    M.get_or_init(|| Mutex::new(HashMap::new()))
}

fn enabled() -> bool {
    static ENABLED: OnceLock<bool> = OnceLock::new();
    *ENABLED.get_or_init(|| std::env::var_os("RUZU_TRACE_SLEEP_TIMING").is_some())
}

/// Record that `tid` entered `SleepThread(ns)`. Overwrites any prior pending
/// entry for the same `tid`.
pub fn record_sleep_start(tid: u64, requested_ns: i64) {
    if !enabled() {
        return;
    }
    let mut m = map().lock().unwrap();
    m.insert(tid, (Instant::now(), requested_ns));
}

/// If `tid` had a pending SleepThread entry, remove it and log the delta
/// between request and wake-up. No-op if no entry (including when the wake
/// path isn't a SleepThread wake).
pub fn observe_wake(tid: u64) {
    if !enabled() {
        return;
    }
    let entry = {
        let mut m = map().lock().unwrap();
        m.remove(&tid)
    };
    if let Some((start, requested_ns)) = entry {
        let actual_ns = start.elapsed().as_nanos() as i64;
        let ratio = if requested_ns > 0 {
            actual_ns as f64 / requested_ns as f64
        } else {
            0.0
        };
        log::info!(
            "sleep_timing tid={} requested_ns={} actual_ns={} ratio={:.3}",
            tid,
            requested_ns,
            actual_ns,
            ratio
        );
    }
}
