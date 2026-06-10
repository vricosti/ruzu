//! Silent in-memory forensics for the MK8D boot-time InvalidHandle abort
//! (task #123). Synchronous eprintln tracing of CloseHandle perturbs timing
//! enough to make the race vanish (5/5 boots with tracing vs abort without),
//! so this module records handle-related SVCs into a bounded in-memory ring
//! and only prints when the failing SVC actually fires
//! (`RUZU_TRACE_SVC_ERRVAL` hit in svc_dispatch).
//!
//! Enabled implicitly whenever `RUZU_TRACE_SVC_ERRVAL` is set.

use std::collections::{HashMap, VecDeque};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Mutex, OnceLock};

const RING_CAP: usize = 8192;

static SEQ: AtomicU64 = AtomicU64::new(0);

#[derive(Clone)]
struct SvcEvent {
    seq: u64,
    tid: u64,
    imm: u32,
    args: [u64; 4],
    ret0: u64,
}

fn ring() -> &'static Mutex<VecDeque<SvcEvent>> {
    static RING: OnceLock<Mutex<VecDeque<SvcEvent>>> = OnceLock::new();
    RING.get_or_init(|| Mutex::new(VecDeque::with_capacity(RING_CAP)))
}

fn handle_map() -> &'static Mutex<HashMap<u32, (u64, String)>> {
    static MAP: OnceLock<Mutex<HashMap<u32, (u64, String)>>> = OnceLock::new();
    MAP.get_or_init(|| Mutex::new(HashMap::new()))
}

pub fn enabled() -> bool {
    static CACHED: OnceLock<bool> = OnceLock::new();
    *CACHED.get_or_init(|| std::env::var_os("RUZU_TRACE_SVC_ERRVAL").is_some())
}

/// Record a handle-related SVC (CloseHandle, SendSyncRequest, WaitSynchronization,
/// ResetSignal, ...) into the ring. Cheap: one short mutex push, no I/O.
pub fn record_svc(tid: u64, imm: u32, args: [u64; 4], ret0: u64) {
    let seq = SEQ.fetch_add(1, Ordering::Relaxed);
    let mut r = ring().lock().unwrap();
    if r.len() == RING_CAP {
        r.pop_front();
    }
    r.push_back(SvcEvent { seq, tid, imm, args, ret0 });
}

/// Record where an IPC-output handle came from (service name + cmd).
/// Latest writer wins — handle values are reused after close.
pub fn record_handle_out(handle: u32, desc: String) {
    let seq = SEQ.fetch_add(1, Ordering::Relaxed);
    handle_map().lock().unwrap().insert(handle, (seq, desc));
}

/// Dump everything we know about `handle`: its recorded provenance and every
/// ring event whose args mention it, plus the ring tail for context.
pub fn dump_for_handle(handle: u64) {
    if let Some((seq, desc)) = handle_map().lock().unwrap().get(&(handle as u32)) {
        eprintln!(
            "[HANDLE_FORENSICS] provenance handle=0x{:X} seq={} desc={}",
            handle, seq, desc
        );
    } else {
        eprintln!(
            "[HANDLE_FORENSICS] provenance handle=0x{:X} UNKNOWN (never recorded by IPC handle-out)",
            handle
        );
    }
    let r = ring().lock().unwrap();
    for ev in r.iter() {
        if ev.args.contains(&handle) {
            eprintln!(
                "[HANDLE_FORENSICS] match seq={} tid={} svc=0x{:02X} args=[0x{:X},0x{:X},0x{:X},0x{:X}] ret0=0x{:X}",
                ev.seq, ev.tid, ev.imm, ev.args[0], ev.args[1], ev.args[2], ev.args[3], ev.ret0
            );
        }
    }
    let tail_start = r.len().saturating_sub(64);
    for ev in r.iter().skip(tail_start) {
        eprintln!(
            "[HANDLE_FORENSICS] tail seq={} tid={} svc=0x{:02X} args=[0x{:X},0x{:X},0x{:X},0x{:X}] ret0=0x{:X}",
            ev.seq, ev.tid, ev.imm, ev.args[0], ev.args[1], ev.args[2], ev.args[3], ev.ret0
        );
    }
}
