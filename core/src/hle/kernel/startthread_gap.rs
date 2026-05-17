//! Per-tid StartThread → first SVC gap measurement.
//!
//! Enabled by `RUZU_PROFILE_STARTTHREAD_GAP=1`.
//!
//! When `svc::StartThread` is called, the parent records a wall-clock
//! timestamp keyed by the child's thread_id. The first time `svc_dispatch`
//! sees an SVC from that tid, it logs the elapsed time and clears the entry.
//!
//! Used to investigate the JIT-startup latency race that causes MK8D to skip
//! `svcArbitrateUnlock` at SVC #745 vs zuyu — see
//! `project_mk8d_thread_id_order_fix_2026_05_17` and
//! `project_mk8d_root_cause_rdynarmic_9x_slow_2026_05_16`.

use std::collections::HashMap;
use std::sync::Mutex;
use std::sync::OnceLock;
use std::time::Instant;

fn map() -> &'static Mutex<HashMap<u64, Instant>> {
    static MAP: OnceLock<Mutex<HashMap<u64, Instant>>> = OnceLock::new();
    MAP.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Record the StartThread timestamp for `child_tid`.
pub fn set_start(child_tid: u64, t: Instant) {
    map().lock().unwrap().insert(child_tid, t);
}

/// Take and clear the recorded StartThread timestamp. Returns `Some(elapsed_us)`
/// if the entry existed, else `None`.
pub fn take_elapsed_us(tid: u64) -> Option<u128> {
    let mut guard = map().lock().unwrap();
    guard.remove(&tid).map(|t| t.elapsed().as_micros())
}
