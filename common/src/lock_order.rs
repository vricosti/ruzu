//! Lock-acquisition-order tracer (debug-only, env-gated).
//!
//! ruzu has several coarse subsystem mutexes (the per-process kernel mutex,
//! the singleton scheduler lock, the GMMU `MemoryManager` mutex, the OpenGL
//! buffer/texture cache mutexes, …). When two of them are ever acquired in
//! opposite orders on different code paths, a timing-sensitive AB-BA deadlock
//! becomes possible — observed as the MK8D "black screen" wedge, which
//! surfaces at different lock pairs run-to-run.
//!
//! This module records, per acquisition, an edge `held -> acquired` for every
//! lock currently held by the calling thread. The first time an edge is seen
//! in the *opposite* direction of one already recorded, it logs a one-shot
//! `[LOCKORDER] INVERSION` line with a backtrace pointing at the offending
//! acquisition. Running MK8D with `RUZU_LOCK_ORDER=1` therefore enumerates
//! every lock-order inversion deterministically — no need to win the wedge
//! race — so each can be fixed (consistent global order / reduced scope).
//!
//! Zero cost when `RUZU_LOCK_ORDER` is unset: `enabled()` is checked once and
//! the instrumentation early-returns.

use std::cell::RefCell;
use std::collections::HashSet;
use std::sync::{Mutex, OnceLock};

fn enabled() -> bool {
    static ON: OnceLock<bool> = OnceLock::new();
    *ON.get_or_init(|| std::env::var_os("RUZU_LOCK_ORDER").is_some())
}

thread_local! {
    /// Names of the coarse locks currently held by this thread, innermost last.
    static HELD: RefCell<Vec<&'static str>> = const { RefCell::new(Vec::new()) };
}

/// Directed edges `(outer, inner)` meaning: `inner` was acquired while `outer`
/// was already held by some thread.
fn edges() -> &'static Mutex<HashSet<(&'static str, &'static str)>> {
    static EDGES: OnceLock<Mutex<HashSet<(&'static str, &'static str)>>> = OnceLock::new();
    EDGES.get_or_init(|| Mutex::new(HashSet::new()))
}

/// Pairs already reported, so each inversion logs only once.
fn reported() -> &'static Mutex<HashSet<(&'static str, &'static str)>> {
    static REPORTED: OnceLock<Mutex<HashSet<(&'static str, &'static str)>>> = OnceLock::new();
    REPORTED.get_or_init(|| Mutex::new(HashSet::new()))
}

/// Current owners per coarse lock name: `name -> [host_tid, …]` (a Vec because
/// the same coarse name can be held re-entrantly or by several instances).
type Owners = std::collections::HashMap<&'static str, Vec<i64>>;
fn owners() -> &'static Mutex<Owners> {
    static OWNERS: OnceLock<Mutex<Owners>> = OnceLock::new();
    OWNERS.get_or_init(|| Mutex::new(Owners::new()))
}

fn host_tid() -> i64 {
    unsafe { libc::syscall(libc::SYS_gettid) as i64 }
}

// ── Per-object wait-for graph ───────────────────────────────────────────────
// Keyed by the actual Mutex address so per-object locks (client_session,
// process, …) are distinguished. The OS thread is the key for "who is
// blocked" (a thread blocks on at most one lock); the guest thread id (stable
// across fiber migration; 0 for host-only threads) is carried for display.
type WaitMap =
    std::collections::HashMap<i64 /*host_tid*/, (usize, &'static str, i64 /*guest_tid*/)>;
type HeldMap = std::collections::HashMap<
    usize, /*addr*/
    (i64 /*host_tid*/, &'static str, i64 /*guest_tid*/),
>;
fn wait_for() -> &'static Mutex<WaitMap> {
    static W: OnceLock<Mutex<WaitMap>> = OnceLock::new();
    W.get_or_init(|| Mutex::new(WaitMap::new()))
}
fn held_by() -> &'static Mutex<HeldMap> {
    static H: OnceLock<Mutex<HeldMap>> = OnceLock::new();
    H.get_or_init(|| Mutex::new(HeldMap::new()))
}

/// Stable logical-actor id: the guest thread id for guest fibers (stable across
/// fiber migration), or `-host_tid` for pure host threads (guest_tid==0). Keying
/// the wait/held maps by this — rather than the raw host_tid — avoids stale
/// entries when a guest fiber migrates host threads.
fn actor_id(guest_tid: i64) -> i64 {
    if guest_tid != 0 {
        guest_tid
    } else {
        -host_tid()
    }
}

/// Record that the calling actor is ABOUT to block acquiring `name`@`addr`.
/// Call immediately before `.lock()`.
pub fn obj_wait(name: &'static str, addr: usize, guest_tid: i64) {
    if !enabled() {
        return;
    }
    wait_for()
        .lock()
        .unwrap()
        .insert(actor_id(guest_tid), (addr, name, guest_tid));
}

#[must_use]
pub struct ObjGuard {
    addr: usize,
    active: bool,
}
impl Drop for ObjGuard {
    fn drop(&mut self) {
        if self.active {
            held_by().lock().unwrap().remove(&self.addr);
        }
    }
}

/// Record that `.lock()` returned: the OS thread now HOLDS `name`@`addr`.
/// Returns a guard that records the release on drop.
pub fn obj_held(name: &'static str, addr: usize, guest_tid: i64) -> ObjGuard {
    if !enabled() {
        return ObjGuard {
            addr,
            active: false,
        };
    }
    wait_for().lock().unwrap().remove(&actor_id(guest_tid));
    held_by()
        .lock()
        .unwrap()
        .insert(addr, (host_tid(), name, guest_tid));
    ObjGuard { addr, active: true }
}

/// Dump the wait-for graph: every blocked acquisition and who holds the lock.
/// A holder that is itself in the wait list reveals a cycle (deadlock); a
/// holder that is NOT waiting (running/parked elsewhere) reveals hold-while-blocked.
pub fn dump_wait_for() {
    if !enabled() {
        return;
    }
    let w = wait_for().lock().unwrap();
    let h = held_by().lock().unwrap();
    eprintln!("[WAITFOR] === blocked lock acquisitions ({}) ===", w.len());
    for (actor, (addr, name, gtid)) in w.iter() {
        match h.get(addr) {
            Some((h_htid, _, h_gtid)) => eprintln!(
                "[WAITFOR]   actor={} guest_tid={} BLOCKED on {}@{:#x}  -->  held by host_tid={} guest_tid={}",
                actor, gtid, name, addr, h_htid, h_gtid
            ),
            None => eprintln!(
                "[WAITFOR]   actor={} guest_tid={} BLOCKED on {}@{:#x}  -->  holder UNKNOWN (not instrumented or released)",
                actor, gtid, name, addr
            ),
        }
    }
    eprintln!(
        "[WAITFOR] currently-held instrumented per-object locks: {}",
        h.len()
    );
    for (addr, (h_htid, name, h_gtid)) in h.iter() {
        eprintln!(
            "[WAITFOR]   {}@{:#x} held by host_tid={} guest_tid={}",
            name, addr, h_htid, h_gtid
        );
    }
}

/// Print, to stderr, who currently holds each coarse lock. Called from the
/// SIGUSR1 thread dumper so a wedge snapshot shows the lock-owner graph.
pub fn dump_owners() {
    if !enabled() {
        return;
    }
    let o = owners().lock().unwrap();
    eprintln!("[LOCKORDER] === current coarse-lock owners ===");
    for (name, tids) in o.iter() {
        if !tids.is_empty() {
            eprintln!("[LOCKORDER]   '{}' held by host_tid(s) {:?}", name, tids);
        }
    }
}

/// Print, to stderr, the COMPLETE observed nesting graph: every edge
/// `outer -> inner` seen so far (i.e. `inner` was acquired while `outer` was
/// held). From this a global lock hierarchy can be derived: a topological sort
/// is possible iff there is no cycle, and any back-edge is an inversion to fix.
pub fn dump_graph() {
    if !enabled() {
        return;
    }
    let e = edges().lock().unwrap();
    let mut v: Vec<_> = e.iter().copied().collect();
    v.sort();
    eprintln!(
        "[LOCKGRAPH] === observed lock-nesting edges (outer -> inner): {} ===",
        v.len()
    );
    for (outer, inner) in &v {
        let back = e.contains(&(*inner, *outer));
        eprintln!(
            "[LOCKGRAPH]   {} -> {}{}",
            outer,
            inner,
            if back {
                "   *** INVERSION (both directions seen) ***"
            } else {
                ""
            }
        );
    }
}

/// RAII guard: drop records the release. Create it in the same scope as the
/// real lock guard (declared just before locking) so its lifetime tracks the
/// hold window closely enough for ordering analysis.
#[must_use]
pub struct LockOrderGuard {
    name: &'static str,
    active: bool,
}

impl Drop for LockOrderGuard {
    fn drop(&mut self) {
        if !self.active {
            return;
        }
        HELD.with(|h| {
            let mut h = h.borrow_mut();
            if let Some(pos) = h.iter().rposition(|n| *n == self.name) {
                h.remove(pos);
            }
        });
        let tid = host_tid();
        if let Ok(mut o) = owners().lock() {
            if let Some(v) = o.get_mut(self.name) {
                if let Some(pos) = v.iter().rposition(|t| *t == tid) {
                    v.remove(pos);
                }
            }
        }
    }
}

/// Record that the calling thread is about to acquire the coarse lock `name`
/// while already holding everything currently on its `HELD` stack. Returns a
/// guard that records the matching release on drop.
pub fn guard(name: &'static str) -> LockOrderGuard {
    if !enabled() {
        return LockOrderGuard {
            name,
            active: false,
        };
    }
    HELD.with(|h| {
        let held = h.borrow();
        if !held.is_empty() {
            let mut e = edges().lock().unwrap();
            for &outer in held.iter() {
                if outer == name {
                    continue; // re-entrant acquire of the same coarse lock
                }
                // Inversion: we are taking `name` while holding `outer`, but
                // `outer` has previously been taken while holding `name`.
                if e.contains(&(name, outer)) {
                    let mut rep = reported().lock().unwrap();
                    if rep.insert((outer, name)) {
                        drop(rep);
                        eprintln!(
                            "[LOCKORDER] INVERSION: acquiring '{}' while holding '{}'; \
                             opposite order ('{}' while holding '{}') seen earlier. held={:?}",
                            name, outer, outer, name, *held
                        );
                        print_backtrace();
                    }
                }
                e.insert((outer, name));
            }
        }
        drop(held);
        h.borrow_mut().push(name);
    });
    owners()
        .lock()
        .unwrap()
        .entry(name)
        .or_default()
        .push(host_tid());
    LockOrderGuard { name, active: true }
}

fn print_backtrace() {
    // glibc backtrace -> stderr (async-signal-safe, matches the SIGURG dumper
    // so addr2line resolves the same `ruzu-cmd(+0xOFF)` offsets).
    const MAX: usize = 48;
    let mut frames = [std::ptr::null_mut::<libc::c_void>(); MAX];
    unsafe {
        extern "C" {
            fn backtrace(buffer: *mut *mut libc::c_void, size: libc::c_int) -> libc::c_int;
            fn backtrace_symbols_fd(
                buffer: *const *mut libc::c_void,
                size: libc::c_int,
                fd: libc::c_int,
            );
        }
        let n = backtrace(frames.as_mut_ptr(), MAX as libc::c_int);
        let marker = b"[LOCKORDER] --- backtrace ---\n";
        libc::write(2, marker.as_ptr() as *const libc::c_void, marker.len());
        backtrace_symbols_fd(frames.as_ptr(), n, 2);
    }
}
