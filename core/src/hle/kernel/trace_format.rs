/// Zuyu-compatible trace format helpers.
/// Activated by RUZU_SVC_TRACE=1 environment variable.
/// Produces lines matching the zuyu trace format:
///   [  T.TTTTTT] SCHED core=C from_tid=F to_tid=T
///   [  T.TTTTTT] SVC_IN  imm=0xNN core=C tid=T args=[...]
///   [  T.TTTTTT] SVC_OUT imm=0xNN args=[...]

use std::sync::atomic::{AtomicU8, Ordering};
use std::sync::OnceLock;
use std::time::Instant;

static TRACE_START: OnceLock<Instant> = OnceLock::new();

/// Check if SVC tracing is enabled (RUZU_SVC_TRACE=1).
pub fn is_svc_trace_enabled() -> bool {
    static INIT: AtomicU8 = AtomicU8::new(0); // 0=unknown, 1=off, 2=on
    match INIT.load(Ordering::Relaxed) {
        2 => true,
        1 => false,
        _ => {
            let on = std::env::var("RUZU_SVC_TRACE").is_ok();
            INIT.store(if on { 2 } else { 1 }, Ordering::Relaxed);
            if on {
                TRACE_START.get_or_init(Instant::now);
                eprintln!("# ruzu trace capture started");
            }
            on
        }
    }
}

/// Returns seconds elapsed since trace start.
pub fn elapsed_secs() -> f64 {
    let start = TRACE_START.get_or_init(Instant::now);
    start.elapsed().as_secs_f64()
}

/// Emit a SCHED trace line. Called from k_scheduler on context switch.
pub fn trace_sched(core_id: i32, from_tid: Option<u64>, to_tid: u64) {
    if !is_svc_trace_enabled() {
        return;
    }
    let from = from_tid.map_or(-1i64, |v| v as i64);
    eprintln!(
        "[{:>10.6}] SCHED core={} from_tid={} to_tid={}",
        elapsed_secs(),
        core_id,
        from,
        to_tid
    );
}
