//! Lightweight nvnflinger history for cross-subsystem diagnostics.
//!
//! This is intentionally not an upstream-owned behavior port. It keeps a small
//! in-memory history that can be dumped by nvmap failure diagnostics without
//! enabling high-volume `log::info!` traces during MK8D boot.

use std::collections::VecDeque;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Mutex, OnceLock};

#[derive(Clone, Copy)]
struct NvnflingerEvent {
    sequence: u64,
    kind: &'static str,
    a: u64,
    b: u64,
    c: u64,
    d: u64,
    e: u64,
    f: u64,
}

static HISTORY: OnceLock<Mutex<VecDeque<NvnflingerEvent>>> = OnceLock::new();
static SEQUENCE: AtomicU64 = AtomicU64::new(0);
const HISTORY_CAPACITY: usize = 4096;

fn record(kind: &'static str, args: [u64; 6]) {
    let mut history = HISTORY
        .get_or_init(|| Mutex::new(VecDeque::with_capacity(HISTORY_CAPACITY)))
        .lock()
        .unwrap();
    if history.len() == HISTORY_CAPACITY {
        history.pop_front();
    }
    history.push_back(NvnflingerEvent {
        sequence: SEQUENCE.fetch_add(1, Ordering::Relaxed) + 1,
        kind,
        a: args[0],
        b: args[1],
        c: args[2],
        d: args[3],
        e: args[4],
        f: args[5],
    });
}

pub(crate) fn record_bqp(kind: &'static str, args: [u64; 6]) {
    record(kind, args);
}

pub(crate) fn record_surface_flinger(kind: &'static str, args: [u64; 6]) {
    record(kind, args);
}

pub(crate) fn record_hwc(kind: &'static str, args: [u64; 6]) {
    record(kind, args);
}

pub fn dump(reason: &str) {
    let Some(history) = HISTORY.get() else {
        return;
    };
    let history = history.lock().unwrap();
    log::info!(
        "[NVNFLINGER_HISTORY] reason={} events={}",
        reason,
        history.len()
    );
    for event in history.iter() {
        log::info!(
            "[NVNFLINGER_HISTORY] #{:05} {} a=0x{:X} b=0x{:X} c=0x{:X} d=0x{:X} e=0x{:X} f=0x{:X}",
            event.sequence,
            event.kind,
            event.a,
            event.b,
            event.c,
            event.d,
            event.e,
            event.f,
        );
    }
}
