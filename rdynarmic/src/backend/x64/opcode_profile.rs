//! Per-IR-opcode emit time accounting.
//!
//! Activated by `RDYNARMIC_PROFILE_OPCODES=1`. Records cumulative emit
//! nanoseconds per Opcode variant. Dumps top consumers on Drop (via the
//! `Reporter` struct registered at jit creation).

use crate::ir::Opcode;
use std::collections::HashMap;
use std::sync::Mutex;
use std::sync::OnceLock;

#[derive(Default, Clone, Copy)]
struct Bucket {
    count: u64,
    total_ns: u64,
}

fn buckets() -> &'static Mutex<HashMap<Opcode, Bucket>> {
    static M: OnceLock<Mutex<HashMap<Opcode, Bucket>>> = OnceLock::new();
    M.get_or_init(|| Mutex::new(HashMap::new()))
}

pub fn record(opcode: Opcode, ns: u64) {
    use std::sync::atomic::{AtomicU64, Ordering};
    static TOTAL: AtomicU64 = AtomicU64::new(0);
    let mut g = buckets().lock().unwrap();
    let e = g.entry(opcode).or_default();
    e.count += 1;
    e.total_ns += ns;
    drop(g);
    let n = TOTAL.fetch_add(1, Ordering::Relaxed) + 1;
    // Dump periodically (every 200k records) so we have data even if
    // process is killed by timeout.
    if n % 200_000 == 0 {
        dump_top(30);
    }
}

/// Dump top-N opcodes by total emit time to stderr.
pub fn dump_top(n: usize) {
    let g = buckets().lock().unwrap();
    let mut v: Vec<_> = g.iter().collect();
    v.sort_by(|a, b| b.1.total_ns.cmp(&a.1.total_ns));
    eprintln!(
        "[OPCODE_PROFILE] top {} opcodes by total emit_ns:",
        n.min(v.len())
    );
    eprintln!(
        "  {:35} {:>10} {:>14} {:>10}",
        "opcode", "count", "total_ns", "avg_ns"
    );
    for (op, b) in v.iter().take(n) {
        let avg = if b.count > 0 { b.total_ns / b.count } else { 0 };
        eprintln!(
            "  {:35} {:>10} {:>14} {:>10}",
            format!("{:?}", op),
            b.count,
            b.total_ns,
            avg
        );
    }
}

/// Register an atexit dump.
pub fn install_atexit() {
    use std::sync::Once;
    static ONCE: Once = Once::new();
    ONCE.call_once(|| {
        let _ = std::panic::catch_unwind(|| {
            // Best-effort: leak a Drop guard.
            struct Guard;
            impl Drop for Guard {
                fn drop(&mut self) {
                    dump_top(30);
                }
            }
            let g = Box::leak(Box::new(Guard));
            // Ensure the leak isn't optimized away.
            let _ = g as *const _;
        });
    });
}
