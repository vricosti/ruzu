//! Process-lifetime monotonic anchor for race-window timing.
//!
//! Used by `RUZU_TRACE_TID_SVC` (and similar env-gated tracers) to stamp
//! every logged event with a nanosecond offset from a fixed origin. Lets us
//! diff SVC timelines across threads to find parent/worker dispatch races
//! at sub-100 µs granularity, instead of the second-precision timestamps
//! that env_logger produces by default.
//!
//! The anchor is set on first call to `elapsed_ns()`. All subsequent calls
//! return ns offsets from that first call. Cheap: one atomic load plus an
//! Instant::now() call on the hot path.

use std::sync::OnceLock;
use std::time::Instant;

fn anchor() -> Instant {
    static ANCHOR: OnceLock<Instant> = OnceLock::new();
    *ANCHOR.get_or_init(Instant::now)
}

/// Nanoseconds elapsed since the first call to `elapsed_ns` in this process.
#[inline]
pub fn elapsed_ns() -> u64 {
    anchor().elapsed().as_nanos() as u64
}
