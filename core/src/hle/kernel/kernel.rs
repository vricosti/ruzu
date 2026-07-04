//! Port of zuyu/src/core/hle/kernel/kernel.h/.cpp
//! Status: COMPLET (stub — runtime dependencies not yet available)
//! Derniere synchro: 2026-03-11
//!
//! KernelCore: the main kernel class, managing all kernel subsystems
//! including schedulers, physical cores, memory layout, slab heaps,
//! shared memory objects, and the global scheduler context.
//!
//! Full implementation requires KProcess, KThread, KScheduler,
//! KMemoryManager, KMemoryLayout, KHardwareTimer, KHandleTable,
//! KResourceLimit, KWorkerTaskManager, and many other subsystems.

use std::cell::RefCell;
use std::sync::atomic::{AtomicBool, AtomicU32, AtomicU64, Ordering};
use std::sync::{Arc, Mutex, Weak};

use super::super::service::os::event::Event;
use super::super::service::server_manager::ServerManager;
use super::k_memory_manager::KMemoryManager;
use super::k_port::KPort;
use super::k_process::KProcess;
use super::k_scheduler::KScheduler;
use super::k_thread::{KThread, KThreadLock, ThreadState};

use super::global_scheduler_context::GlobalSchedulerContext;
use super::init::init_slab_setup::KSlabResourceCounts;
use super::k_auto_object_container::KAutoObjectWithListContainer;
use super::k_hardware_timer::KHardwareTimer;
use super::k_object_name::KObjectNameGlobalData;
use super::k_process::ProcessLock;
use super::k_thread::SuspendType;
use super::k_worker_task_manager::KWorkerTaskManager;
use super::physical_core::PhysicalCore;
use crate::core_timing::CoreTiming;
use crate::hardware_properties;

// Thread-local host thread ID.
// Upstream: `static inline thread_local u8 host_thread_id = UINT8_MAX` in KernelCore::Impl.
// Core threads get IDs 0..NUM_CPU_CORES-1. Other host threads get IDs >= NUM_CPU_CORES.
// UINT8_MAX (255) means "not yet registered".
std::thread_local! {
    static HOST_THREAD_ID: std::cell::Cell<u32> = const { std::cell::Cell::new(u32::MAX) };
}

// Global kernel pointer for scheduler callbacks.
// Set during kernel initialization, cleared on shutdown.
// The callbacks (fn pointers) need access to GSC + schedulers but can't capture state.
static KERNEL_PTR: std::sync::atomic::AtomicPtr<KernelCore> =
    std::sync::atomic::AtomicPtr::new(std::ptr::null_mut());

// Raw pointer to the GSC's `m_scheduler_lock` field. Cached at kernel
// initialization so any site can open a `KScopedSchedulerLock` without
// depending on a per-thread scheduler_lock_ptr cache.
//
// Upstream assumes `KScheduler::GetSchedulerLock(kernel)` is always valid
// once the kernel is constructed. Ruzu's previous scheme cached the pointer
// on `KThread::scheduler_lock_ptr`, which is zero until the thread is
// attached — forcing condvar/arbiter entry points to silently no-op the
// scheduler lock. Cache it on the kernel singleton so the "always valid"
// assumption actually holds.
static SCHEDULER_LOCK_PTR: std::sync::atomic::AtomicPtr<
    super::k_scheduler_lock::KAbstractSchedulerLock,
> = std::sync::atomic::AtomicPtr::new(std::ptr::null_mut());

/// Deferred `KThread::SetActiveCore()` updates that could not be applied
/// immediately because the target thread mutex was still held.
///
/// Upstream applies active-core migration while still under the scheduler lock.
/// Rust cannot safely block on a `KThreadLock` there, so preserve the
/// migration request here and retry it from later scheduler callbacks until it
/// succeeds. Dropping the migration outright leaves a runnable thread tagged to
/// the wrong core and can strand all guest cores in idle.
static PENDING_ACTIVE_CORE_UPDATES: Mutex<Vec<(u64, i32)>> = Mutex::new(Vec::new());

struct TrackedServerManager {
    manager: Arc<Mutex<ServerManager>>,
    stop_requested: Arc<AtomicBool>,
    wakeup_event: Arc<Event>,
}

/// Public accessor for KERNEL_PTR — used by GSC to interrupt cores on thread state changes.
pub fn get_kernel_ref() -> Option<&'static KernelCore> {
    let ptr = KERNEL_PTR.load(Ordering::Acquire);
    if ptr.is_null() {
        None
    } else {
        Some(unsafe { &*ptr })
    }
}

/// Mutable accessor for KERNEL_PTR — used by code paths (e.g. KPageTableBase
/// allocation paths) that need `&mut KernelCore` to call `memory_manager_mut()`.
/// The kernel pointer is set once at startup and never reassigned, so the
/// returned reference is valid for the duration of the program.
#[allow(clippy::mut_from_ref)]
pub fn get_kernel_mut() -> Option<&'static mut KernelCore> {
    let ptr = KERNEL_PTR.load(Ordering::Acquire);
    if ptr.is_null() {
        None
    } else {
        Some(unsafe { &mut *ptr })
    }
}

/// Test-only owner that installs a minimal `KernelCore` in the global kernel
/// pointer without running full kernel initialization.
///
/// Unit tests for page-table allocation paths need upstream-shaped access to
/// `KernelCore::MemoryManager()`, but `KernelCore::initialize()` starts broad
/// scheduler/core state that is inappropriate for small native tests.
#[cfg(test)]
pub struct ScopedKernelForTest {
    kernel: Box<KernelCore>,
    previous: *mut KernelCore,
    previous_scheduler_lock: *mut super::k_scheduler_lock::KAbstractSchedulerLock,
}

#[cfg(test)]
impl ScopedKernelForTest {
    pub fn new() -> Self {
        let mut kernel = Box::new(KernelCore::new());
        let ptr = &mut *kernel as *mut KernelCore;
        let previous = KERNEL_PTR.swap(ptr, Ordering::AcqRel);
        let previous_scheduler_lock =
            SCHEDULER_LOCK_PTR.swap(std::ptr::null_mut(), Ordering::AcqRel);
        Self {
            kernel,
            previous,
            previous_scheduler_lock,
        }
    }

    pub fn memory_manager_mut(&mut self) -> &mut KMemoryManager {
        self.kernel.memory_manager_mut()
    }

    pub fn kernel_mut(&mut self) -> &mut KernelCore {
        &mut self.kernel
    }
}

#[cfg(test)]
impl Drop for ScopedKernelForTest {
    fn drop(&mut self) {
        SCHEDULER_LOCK_PTR.store(self.previous_scheduler_lock, Ordering::Release);
        KERNEL_PTR.store(self.previous, Ordering::Release);
    }
}

/// Returns the kernel's global `KAbstractSchedulerLock`, if the kernel has
/// been initialized. Matches upstream `KScheduler::GetSchedulerLock(kernel)`.
///
/// Safe to call from any site (SVC handlers, HLE threads, hardware timer
/// callbacks). Returns `None` only before `KernelCore::initialize()` has
/// run — i.e., unit-test harness entry paths before a kernel exists.
pub fn scheduler_lock() -> Option<&'static super::k_scheduler_lock::KAbstractSchedulerLock> {
    let ptr = SCHEDULER_LOCK_PTR.load(Ordering::Acquire);
    if ptr.is_null() {
        None
    } else {
        Some(unsafe { &*ptr })
    }
}

/// SIGUSR1 flag set by the async-signal-safe handler; polled by the preemption
/// thread so the dump runs outside signal context (where Rust's Mutex is unsafe).
static DUMP_REQUESTED: AtomicBool = AtomicBool::new(false);

/// Per-core SVC-entry tracker.  Each entry is packed as (tid:u32, svc:u32).
/// Updated by `svc_dispatch::call` at entry; cleared at exit.  Used by the
/// SIGUSR1 dumper to identify which thread/svc is currently executing on each
/// core.
pub static SVC_IN_PROGRESS: [std::sync::atomic::AtomicU64;
    hardware_properties::NUM_CPU_CORES as usize] = [
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
];

/// Per-core last known guest PC. Updated by `PhysicalCore::handoff_after_svc`
/// at every SVC return (cheapest hook that already touches the thread context
/// where the PC lives). The value lags the JIT's current PC by however many
/// guest instructions have executed since the last SVC — fine for
/// post-freeze diagnostics where the spin has no SVCs at all and we want the
/// PC at which the spin started.
pub static GUEST_PC: [std::sync::atomic::AtomicU64; hardware_properties::NUM_CPU_CORES as usize] = [
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
];

/// Per-core last known guest LR (link register). When a guest thread calls
/// an nnSdk SVC stub like `svc 0x18; bx lr`, LR points back into the caller
/// — usually the game's code that invoked WaitSynchronization or similar.
/// Essential for identifying the actual hot spot in a spin loop where the
/// game only ever calls one kind of SVC (the SVC address drowns out the
/// real work PC).
pub static GUEST_LR: [std::sync::atomic::AtomicU64; hardware_properties::NUM_CPU_CORES as usize] = [
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
];

/// Per-core last known guest SP. Lets the SIGUSR1 dumper walk a few stack
/// frames above the current SVC/halt — the nnSdk SVC stub caller sits right
/// above and its caller (the game-level function driving the loop) is
/// typically within 1-2 frames up.
pub static GUEST_SP: [std::sync::atomic::AtomicU64; hardware_properties::NUM_CPU_CORES as usize] = [
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
];

/// Per-core snapshot of guest general-purpose registers. AArch32 uses the
/// low words; AArch64 uses x0..x28. Updated alongside GUEST_{PC,LR,SP}.
pub static GUEST_REGS: [[std::sync::atomic::AtomicU64; 29];
    hardware_properties::NUM_CPU_CORES as usize] = {
    const NEW: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
    const ROW: [std::sync::atomic::AtomicU64; 29] = [NEW; 29];
    [ROW, ROW, ROW, ROW]
};

/// RUZU_PC_SAMPLE=1 — background guest-PC/LR sampling profiler. Every
/// `RUZU_PC_SAMPLE_INTERVAL_US` (default 200µs) it reads each core's last
/// guest (PC,LR) and increments a histogram keyed by LR (the game code that
/// called the nnSdk SVC stub — the real hot loop, vs the stub PC which drowns
/// it out). Dumped on SIGUSR1 via `dump_pc_sample_hist`. Fills the missing
/// "where is the wedge hot loop" tool: the candidate-PC hooks all turned out
/// cold, so we need to discover the hot LR empirically.
static PC_SAMPLE_HIST: std::sync::Mutex<Option<std::collections::HashMap<(u32, u64), u64>>> =
    std::sync::Mutex::new(None);
static PC_SAMPLE_STARTED: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

fn maybe_spawn_pc_sampler() {
    if std::env::var_os("RUZU_PC_SAMPLE").is_none() {
        return;
    }
    if PC_SAMPLE_STARTED.swap(true, Ordering::SeqCst) {
        return;
    }
    let interval_us = std::env::var("RUZU_PC_SAMPLE_INTERVAL_US")
        .ok()
        .and_then(|s| s.parse::<u64>().ok())
        .unwrap_or(200);
    *PC_SAMPLE_HIST.lock().unwrap() = Some(std::collections::HashMap::new());
    std::thread::Builder::new()
        .name("ruzu-pc-sample".into())
        .spawn(move || {
            let dur = std::time::Duration::from_micros(interval_us.max(20));
            loop {
                {
                    let mut guard = PC_SAMPLE_HIST.lock().unwrap();
                    if let Some(hist) = guard.as_mut() {
                        for core in 0..GUEST_LR.len() {
                            let lr = GUEST_LR[core].load(Ordering::Acquire);
                            let pc = GUEST_PC[core].load(Ordering::Acquire);
                            if lr == 0 && pc == 0 {
                                continue;
                            }
                            // tid running on this core = last SVC's tid (SVC_IN_PROGRESS
                            // is set on every svc-enter and persists until the next svc
                            // on this core ≈ the current thread). Lets us isolate one
                            // thread's hot loop (e.g. tid=75 Main render thread).
                            let tid = (SVC_IN_PROGRESS[core].load(Ordering::Acquire) >> 32) as u32;
                            // Key by (tid, lr<<32 | pc&0xFFFFFFFF) so we keep the SVC
                            // stub PC and the game caller LR distinguished, per thread.
                            let key = (tid, (lr << 32) | (pc & 0xFFFF_FFFF));
                            *hist.entry(key).or_insert(0) += 1;
                        }
                    }
                }
                std::thread::sleep(dur);
            }
        })
        .ok();
    eprintln!(
        "[PC_SAMPLE] sampler started (interval {}µs); dump on SIGUSR1",
        interval_us
    );
}

pub fn dump_pc_sample_hist() {
    let guard = PC_SAMPLE_HIST.lock().unwrap();
    let Some(hist) = guard.as_ref() else {
        return;
    };
    let total: u64 = hist.values().sum();
    let mut v: Vec<((u32, u64), u64)> = hist.iter().map(|(k, c)| (*k, *c)).collect();
    v.sort_by(|a, b| b.1.cmp(&a.1));
    eprintln!(
        "[PC_SAMPLE] === top guest (tid,LR,PC) by sample count (total={}) ===",
        total
    );
    for ((tid, lrpc), count) in v.iter().take(40) {
        let lr = lrpc >> 32;
        let pc = lrpc & 0xFFFF_FFFF;
        let pct = if total > 0 {
            (*count as f64) * 100.0 / total as f64
        } else {
            0.0
        };
        eprintln!(
            "[PC_SAMPLE] tid={} lr=0x{:08X} pc=0x{:08X} count={} ({:.1}%)",
            tid, lr, pc, count, pct
        );
    }
}

#[inline]
pub fn mark_svc_enter(core_id: usize, tid: u64, svc: u32) {
    if core_id >= SVC_IN_PROGRESS.len() {
        return;
    }
    let packed = ((tid & 0xFFFF_FFFF) << 32) | (svc as u64 & 0xFFFF_FFFF);
    SVC_IN_PROGRESS[core_id].store(packed, Ordering::Release);
}

/// Record the guest PC observed after an SVC returns. Called from
/// `PhysicalCore::handoff_after_svc` once `jit.get_context()` has populated
/// the ThreadContext for this core.
#[inline]
pub fn record_guest_pc(core_id: usize, pc: u64) {
    if core_id >= GUEST_PC.len() {
        return;
    }
    GUEST_PC[core_id].store(pc, Ordering::Release);
}

/// Record the guest PC + LR observed after an SVC / Halt. LR typically
/// points into the game code that called the nnSdk SVC stub, which is
/// more diagnostic than PC (which sits in the stub itself).
#[inline]
pub fn record_guest_pc_lr(core_id: usize, pc: u64, lr: u64) {
    if core_id >= GUEST_PC.len() {
        return;
    }
    GUEST_PC[core_id].store(pc, Ordering::Release);
    GUEST_LR[core_id].store(lr, Ordering::Release);
}

/// Record the guest PC + LR + SP observed after an SVC / Halt.
#[inline]
pub fn record_guest_pc_lr_sp(core_id: usize, pc: u64, lr: u64, sp: u64) {
    if core_id >= GUEST_PC.len() {
        return;
    }
    GUEST_PC[core_id].store(pc, Ordering::Release);
    GUEST_LR[core_id].store(lr, Ordering::Release);
    GUEST_SP[core_id].store(sp, Ordering::Release);
}

/// Record the guest PC + LR + SP + r0..r11 observed after an SVC / Halt.
/// Used by the SIGUSR1 dumper to see live register values during a spin
/// loop (e.g., the `r8` object pointer and `r10` target count that drive
/// the loop at game PC 0x015DFE30).
#[inline]
pub fn record_guest_full(core_id: usize, pc: u64, lr: u64, sp: u64, regs: &[u64]) {
    if core_id >= GUEST_PC.len() {
        return;
    }
    GUEST_PC[core_id].store(pc, Ordering::Release);
    GUEST_LR[core_id].store(lr, Ordering::Release);
    GUEST_SP[core_id].store(sp, Ordering::Release);
    for (i, slot) in GUEST_REGS[core_id].iter().enumerate() {
        let v = regs.get(i).copied().unwrap_or(0);
        slot.store(v, Ordering::Release);
    }
}

#[inline]
pub fn mark_svc_exit(core_id: usize) {
    if core_id >= SVC_IN_PROGRESS.len() {
        return;
    }
    SVC_IN_PROGRESS[core_id].store(0, Ordering::Release);
}

extern "C" fn sigusr1_handler(_signum: libc::c_int) {
    // Only async-signal-safe code here.
    DUMP_REQUESTED.store(true, Ordering::Relaxed);
}

extern "C" fn sigurg_handler(_signum: libc::c_int) {
    // Async-signal-safe: use libc::backtrace + libc::backtrace_symbols_fd.
    // glibc documents these as thread-safe and signal-safe.
    const MAX_FRAMES: usize = 32;
    let mut frames: [*mut libc::c_void; MAX_FRAMES] = [std::ptr::null_mut(); MAX_FRAMES];
    // Use raw libc symbols via direct FFI — neither is in the libc crate by default on all
    // platforms, so declare them here.
    extern "C" {
        fn backtrace(buffer: *mut *mut libc::c_void, size: libc::c_int) -> libc::c_int;
        fn backtrace_symbols_fd(
            buffer: *const *mut libc::c_void,
            size: libc::c_int,
            fd: libc::c_int,
        );
    }
    unsafe {
        let n = backtrace(frames.as_mut_ptr(), MAX_FRAMES as libc::c_int);
        // Write marker + backtrace to stderr.
        let marker = b"[SIGURG] --- backtrace ---\n";
        let _ = libc::write(2, marker.as_ptr() as *const _, marker.len());
        backtrace_symbols_fd(frames.as_ptr(), n, 2);
    }
}

fn install_sigusr1_handler() {
    unsafe {
        let mut sa: libc::sigaction = std::mem::zeroed();
        sa.sa_sigaction = sigusr1_handler as usize;
        libc::sigemptyset(&mut sa.sa_mask);
        sa.sa_flags = libc::SA_RESTART;
        let _ = libc::sigaction(libc::SIGUSR1, &sa, std::ptr::null_mut());

        // Also install SIGURG handler for per-thread backtrace dump.
        let mut sa2: libc::sigaction = std::mem::zeroed();
        sa2.sa_sigaction = sigurg_handler as usize;
        libc::sigemptyset(&mut sa2.sa_mask);
        // Don't set SA_RESTART — we want the blocked futex to return EINTR so
        // the signal handler runs. After the handler the thread returns to the
        // same wait.
        sa2.sa_flags = 0;
        let _ = libc::sigaction(libc::SIGURG, &sa2, std::ptr::null_mut());
    }
    eprintln!(
        "[SIGUSR1] handler installed for pid={}: send `kill -USR1 <pid>` to dump thread state",
        std::process::id(),
    );
    maybe_spawn_pc_sampler();
}

/// Dump all per-core and per-thread state to stderr.
/// Called from the preemption thread once DUMP_REQUESTED is set.
/// The preemption thread is a normal host thread (not a fiber) so locking is
/// safe.
fn dump_thread_state(kernel: &KernelCore) {
    eprintln!("=========================================");
    eprintln!("[DUMP] === ruzu kernel thread dump ===");
    dump_pc_sample_hist();
    crate::hle::kernel::svc_dispatch::dump_svc_ring_profile();
    crate::hle::kernel::svc_dispatch::dump_svc_summary_profile();
    crate::hle::kernel::svc_dispatch::dump_svc_profile();
    crate::hle::kernel::svc::svc_memory_history::dump("sigusr1_thread_dump");
    crate::hle::service::nvnflinger::buffer_queue_core::dump_bqp_wait_profile();
    crate::hle::service::nvnflinger::buffer_queue_producer::dump_bqp_slot_profile();
    crate::hle::service::nvnflinger::hardware_composer::dump_hwc_cache_profile();
    crate::hle::service::nvnflinger::hos_binder_driver::dump_binder_txn_profile();
    crate::hle::service::nvnflinger::diagnostics::dump("sigusr1_thread_dump");
    // Who holds each coarse lock right now + the full observed nesting graph
    // (RUZU_LOCK_ORDER=1).
    common::lock_order::dump_owners();
    common::lock_order::dump_graph();
    common::lock_order::dump_wait_for();

    fn parse_u64_auto(raw: &str) -> Option<u64> {
        let raw = raw.trim();
        let hex = raw.strip_prefix("0x").or_else(|| raw.strip_prefix("0X"));
        match hex {
            Some(hex) => u64::from_str_radix(hex, 16).ok(),
            None => raw.parse().ok(),
        }
    }

    // Per-core running thread + interrupt flag + in-progress SVC + last guest PC.
    let mut pcs_to_dump: Vec<u64> = Vec::new();
    let mut stacks_to_dump: Vec<(usize, u64)> = Vec::new();
    let mut svc21_messages_to_dump: Vec<(usize, u32, u32, u32, u32)> = Vec::new();
    for core_id in 0..hardware_properties::NUM_CPU_CORES as usize {
        if let Some(core) = kernel.physical_core(core_id) {
            let interrupted = core.is_interrupted();
            let packed = SVC_IN_PROGRESS[core_id].load(Ordering::Acquire);
            let svc_tid = (packed >> 32) as u32;
            let svc_num = (packed & 0xFFFF_FFFF) as u32;
            let last_pc = GUEST_PC[core_id].load(Ordering::Acquire);
            let last_lr = GUEST_LR[core_id].load(Ordering::Acquire);
            let last_sp = GUEST_SP[core_id].load(Ordering::Acquire);
            eprintln!(
                "[DUMP] core={} is_interrupted={} in_svc={{tid={}, imm=0x{:X}}} last_guest_pc=0x{:X} last_guest_lr=0x{:X} last_guest_sp=0x{:X}",
                core_id, interrupted, svc_tid, svc_num, last_pc, last_lr, last_sp,
            );
            let regs: [u64; 29] =
                std::array::from_fn(|i| GUEST_REGS[core_id][i].load(Ordering::Acquire));
            eprintln!(
                "[DUMP]        regs x0-x3:   {:016X} {:016X} {:016X} {:016X}",
                regs[0], regs[1], regs[2], regs[3],
            );
            eprintln!(
                "[DUMP]        regs x4-x7:   {:016X} {:016X} {:016X} {:016X}",
                regs[4], regs[5], regs[6], regs[7],
            );
            eprintln!(
                "[DUMP]        regs x8-x11:  {:016X} {:016X} {:016X} {:016X}",
                regs[8], regs[9], regs[10], regs[11],
            );
            eprintln!(
                "[DUMP]        regs x12-x18: {:016X} {:016X} {:016X} {:016X} {:016X} {:016X} {:016X}",
                regs[12], regs[13], regs[14], regs[15], regs[16], regs[17], regs[18],
            );
            eprintln!(
                "[DUMP]        regs x19-x24: {:016X} {:016X} {:016X} {:016X} {:016X} {:016X}",
                regs[19], regs[20], regs[21], regs[22], regs[23], regs[24],
            );
            eprintln!(
                "[DUMP]        regs x25-x28: {:016X} {:016X} {:016X} {:016X}",
                regs[25], regs[26], regs[27], regs[28],
            );
            if svc_num == 0x21 && regs[1] != 0 && regs[2] != 0 {
                svc21_messages_to_dump.push((
                    core_id,
                    svc_tid,
                    regs[0] as u32,
                    regs[1] as u32,
                    regs[2] as u32,
                ));
            }
            if last_sp != 0 {
                stacks_to_dump.push((core_id, last_sp));
            }
            if last_pc != 0 && !pcs_to_dump.contains(&last_pc) {
                pcs_to_dump.push(last_pc);
            }
            // LR usually points AFTER the `BL <stub>` call in the caller
            // (i.e., into game code). Subtract 4 to get the `BL` itself
            // and its surrounding context — where the spin actually
            // happens.
            if last_lr != 0 && last_lr != last_pc {
                let caller_pc = last_lr.saturating_sub(4);
                if !pcs_to_dump.contains(&caller_pc) {
                    pcs_to_dump.push(caller_pc);
                }
            }
        }
    }
    // Allow operator to inject extra addresses via RUZU_DUMP_ADDRS=0xAAA,0xBBB
    // — the SIGUSR1 dumper will print memory around each. Useful for
    // inspecting known init-write PCs or state-handler targets.
    if let Ok(raw) = std::env::var("RUZU_DUMP_ADDRS") {
        for s in raw.split(',') {
            let s = s.trim().trim_start_matches("0x").trim_start_matches("0X");
            if let Ok(addr) = u64::from_str_radix(s, 16) {
                if addr != 0 && !pcs_to_dump.contains(&addr) {
                    pcs_to_dump.push(addr);
                }
            }
        }
    }
    // RUZU_POKE_ADDR=0x40037000:4:1 (addr:size_bytes:value_hex) — write a
    // test value into guest memory on SIGUSR1. Experimental harness for
    // empirically unblocking the MK8D wedge. Size must be 4 (u32 write).
    let pokes: Vec<(u64, u32)> = std::env::var("RUZU_POKE_ADDR")
        .ok()
        .iter()
        .flat_map(|raw| {
            raw.split(',')
                .map(|s| s.trim().to_string())
                .collect::<Vec<_>>()
        })
        .filter_map(|tok| {
            let parts: Vec<&str> = tok.split(':').collect();
            if parts.len() != 3 {
                return None;
            }
            let addr = parse_u64_auto(parts[0])?;
            let size = parse_u64_auto(parts[1])?;
            if size != 4 {
                return None;
            }
            let value = parse_u64_auto(parts[2])? as u32;
            Some((addr, value))
        })
        .collect();

    // RUZU_DUMP_REGION=0x22C0000:24576 or 0x22C0000:0x6000 dumps a
    // contiguous u32 range as raw hex (no per-PC context windows) — useful
    // for whole-vtable snapshots.
    let region_dumps: Vec<(u64, u64)> = std::env::var("RUZU_DUMP_REGION")
        .ok()
        .iter()
        .flat_map(|raw| {
            raw.split(',')
                .map(|s| s.trim().to_string())
                .collect::<Vec<_>>()
        })
        .filter_map(|tok| {
            let (a, n) = tok.split_once(':')?;
            let addr = parse_u64_auto(a)?;
            let bytes = parse_u64_auto(n)?;
            Some((addr, bytes))
        })
        .collect();
    // Walk the application process (if any) and dump each thread.
    let system = kernel.system();
    if system.is_null() {
        eprintln!("[DUMP] no system ref — skipping thread walk");
        eprintln!("=========================================");
        DUMP_REQUESTED.store(false, Ordering::Relaxed);
        return;
    }
    let Some(process_arc) = system.get().current_process_arc.as_ref().cloned() else {
        eprintln!("[DUMP] no current process");
        eprintln!("=========================================");
        DUMP_REQUESTED.store(false, Ordering::Relaxed);
        return;
    };

    // RUZU_DUMP_MEM=0xADDR:LEN[,0xADDR:LEN...] — dump guest memory words at the
    // wedge instant (e.g. the barrier-object region to read the never-satisfied
    // join predicate). LEN is in BYTES (decimal or 0xhex).
    if let Ok(spec) = std::env::var("RUZU_DUMP_MEM") {
        for part in spec.split(',') {
            let mut it = part.split(':');
            if let (Some(a), Some(l)) = (it.next(), it.next()) {
                if let (Some(addr), Some(len)) = (parse_u64_auto(a), parse_u64_auto(l)) {
                    let nwords = ((len as usize) / 4).min(64);
                    // Prefer the fastmem arena (host base + guest vaddr). If
                    // it is not registered, fall back to the page-table memory
                    // handle used by the stack dumper below.
                    let fb = common::fastmem_registry::base();
                    if fb != 0 {
                        let mut w = vec![0u32; nwords];
                        for (i, slot) in w.iter_mut().enumerate() {
                            let host = (fb as u64 + addr + (i as u64) * 4) as *const u32;
                            *slot = unsafe { std::ptr::read_volatile(host) };
                        }
                        eprintln!("[DUMP] MEM 0x{:08X} ({} words) = {:08X?}", addr, nwords, w);
                    } else {
                        match process_arc.try_lock() {
                            Ok(process_guard) => {
                                if let Some(memory) =
                                    process_guard.page_table.get_base().m_memory.as_ref()
                                {
                                    match memory.try_lock() {
                                        Ok(m) => {
                                            let mut w = vec![0u32; nwords];
                                            for (i, slot) in w.iter_mut().enumerate() {
                                                *slot = m.read_32(addr + (i as u64) * 4);
                                            }
                                            eprintln!(
                                                "[DUMP] MEM 0x{:08X} ({} words,page_table) = {:08X?}",
                                                addr, nwords, w
                                            );
                                        }
                                        Err(_) => {
                                            eprintln!(
                                                "[DUMP] MEM 0x{:08X} <guest-memory-lock-busy>",
                                                addr
                                            );
                                        }
                                    }
                                } else {
                                    eprintln!("[DUMP] MEM 0x{:08X} <no-memory-source>", addr);
                                }
                            }
                            Err(_) => {
                                eprintln!("[DUMP] MEM 0x{:08X} <process-lock-busy>", addr);
                            }
                        }
                    }
                }
            }
        }
    }

    let pq_fronts = kernel.global_scheduler_context().and_then(|gsc| {
        gsc.try_lock().ok().map(|gsc| {
            [
                gsc.get_scheduled_front(0),
                gsc.get_scheduled_front(1),
                gsc.get_scheduled_front(2),
                gsc.get_scheduled_front(3),
            ]
        })
    });
    eprintln!("[DUMP] scheduler pq_fronts={:?}", pq_fronts);
    for core_id in 0..crate::hardware_properties::NUM_CPU_CORES as usize {
        let Some(scheduler) = kernel.scheduler(core_id) else {
            eprintln!("[DUMP] scheduler core={} missing", core_id);
            continue;
        };
        match scheduler.try_lock() {
            Ok(scheduler) => {
                eprintln!(
                    "[DUMP] scheduler core={} current={:?} highest={:?} prev={:?} needs={} interrupt_task={} idle={}",
                    core_id,
                    scheduler.get_scheduler_current_thread_id(),
                    scheduler.state.highest_priority_thread_id,
                    scheduler.state.prev_thread_id,
                    scheduler.needs_scheduling(),
                    scheduler.state.interrupt_task_runnable,
                    scheduler.is_idle(),
                );
            }
            Err(_) => {
                eprintln!(
                    "[DUMP] scheduler core={} <scheduler-lock-contended>",
                    core_id
                );
            }
        }
    }

    // Attempt the process lock with multi-second polling so a briefly-held
    // Mutex passes; if still contended after the timeout, it's a real hold.
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(3);
    let mut acquired = None;
    let mut poll_count = 0u64;
    while std::time::Instant::now() < deadline {
        match process_arc.try_lock() {
            Ok(guard) => {
                acquired = Some(guard);
                break;
            }
            Err(_) => {
                poll_count += 1;
                std::thread::sleep(std::time::Duration::from_millis(5));
            }
        }
    }
    let thread_entries: Vec<(u64, std::sync::Arc<super::k_thread::KThreadLock>)> = match acquired {
        Some(mut guard) => {
            eprintln!(
                "[DUMP] process threads: {} (process.lock() acquired after {} polls)",
                guard.thread_list.len(),
                poll_count,
            );
            // RUZU_DUMP_JIT_MAP=prefix — write each core's JIT block map
            // (host entry -> guest descriptor) to `prefix.coreN.map` so host
            // profiler samples inside JIT code can be attributed to guest
            // locations offline.
            if let Ok(prefix) = std::env::var("RUZU_DUMP_JIT_MAP") {
                for core in 0..4 {
                    if let Some(arm) = guard.get_arm_interface_mut(core) {
                        let path = format!("{prefix}.core{core}.map");
                        arm.dump_jit_block_map(&path);
                        eprintln!("[DUMP] JIT block map core {core} -> {path}");
                    }
                }
            }
            for (core_id, tid, handle, message_addr, size) in &svc21_messages_to_dump {
                if let Some(object_id) = guard.handle_table.get_object(*handle) {
                    let Some(client_session) = guard.get_client_session_by_object_id(object_id)
                    else {
                        eprintln!(
                            "[DUMP]   SVC21_HANDLE core={} tid={} handle=0x{:08X} object_id=0x{:X} <not-client-session>",
                            core_id, tid, handle, object_id
                        );
                        continue;
                    };
                    let handle_line = match client_session.try_lock() {
                        Ok(client) => {
                            let parent_id = client.get_parent_id();
                            let server_session = parent_id
                                .and_then(|parent_id| guard.get_session_by_object_id(parent_id))
                                .and_then(|session| {
                                    session
                                        .try_lock()
                                        .ok()
                                        .map(|session| session.get_server_session().clone())
                                });
                            let manager_name = server_session
                                .as_ref()
                                .and_then(|server_session| {
                                    server_session.try_lock().ok().and_then(|server_session| {
                                        server_session.get_manager().cloned()
                                    })
                                })
                                .and_then(|manager| {
                                    manager.lock().ok().and_then(|manager| {
                                        manager
                                            .session_handler()
                                            .map(|handler| handler.service_name().to_string())
                                    })
                                })
                                .unwrap_or_else(|| "<no-manager-handler>".to_string());
                            let server_state = parent_id
                                .and_then(|parent_id| guard.get_session_by_object_id(parent_id))
                                .and_then(|session| {
                                    session
                                        .try_lock()
                                        .ok()
                                        .map(|session| session.get_server_session().clone())
                                })
                                .and_then(|server_session| {
                                    server_session.try_lock().ok().map(|server| {
                                        let server_manager_name = server
                                            .get_manager()
                                            .and_then(|manager| {
                                                manager.lock().ok().and_then(|manager| {
                                                    manager.session_handler().map(|handler| {
                                                        handler.service_name().to_string()
                                                    })
                                                })
                                            })
                                            .unwrap_or_else(|| "<no-server-manager>".to_string());
                                        format!(
                                            "server_manager={} request_list={} current_request={}",
                                            server_manager_name,
                                            server.request_list.len(),
                                            server.current_request.is_some()
                                        )
                                    })
                                })
                                .unwrap_or_else(|| {
                                    "<server-session-lock-busy-or-missing>".to_string()
                                });
                            format!(
                                "parent_id={:?} manager={} {}",
                                parent_id, manager_name, server_state
                            )
                        }
                        Err(_) => "<client-session-lock-busy>".to_string(),
                    };
                    eprintln!(
                        "[DUMP]   SVC21_HANDLE core={} tid={} handle=0x{:08X} object_id=0x{:X} {}",
                        core_id, tid, handle, object_id, handle_line
                    );
                } else {
                    eprintln!(
                        "[DUMP]   SVC21_HANDLE core={} tid={} handle=0x{:08X} <invalid-handle>",
                        core_id, tid, handle
                    );
                }

                let word_count = ((*size as usize).min(0x40) / 4).min(16);
                let words: Option<Vec<u32>> =
                    if let Some(memory) = guard.page_table.get_base().m_memory.as_ref() {
                        match memory.try_lock() {
                            Ok(m) => {
                                let mut w = vec![0u32; word_count];
                                for (i, slot) in w.iter_mut().enumerate() {
                                    *slot = m.read_32(*message_addr as u64 + (i as u64) * 4);
                                }
                                Some(w)
                            }
                            Err(_) => None,
                        }
                    } else {
                        let mem = guard.process_memory.read().unwrap();
                        let bytes = mem.read_block(*message_addr as u64, word_count * 4);
                        if bytes.len() >= word_count * 4 {
                            Some(
                                bytes
                                    .chunks_exact(4)
                                    .take(word_count)
                                    .map(|chunk| {
                                        u32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]])
                                    })
                                    .collect(),
                            )
                        } else {
                            None
                        }
                    };
                match words {
                    Some(words) => eprintln!(
                        "[DUMP]   SVC21_MSG core={} tid={} handle=0x{:08X} msg=0x{:08X} size=0x{:X} words={:08X?}",
                        core_id, tid, handle, message_addr, size, words
                    ),
                    None => eprintln!(
                        "[DUMP]   SVC21_MSG core={} tid={} handle=0x{:08X} msg=0x{:08X} size=0x{:X} <guest-memory-lock-busy>",
                        core_id, tid, handle, message_addr, size
                    ),
                }
            }
            // RUZU_DUMP_HANDLE=0xNNN[,0xMMM...] — resolve each handle in the
            // main process handle table and identify the kernel object kind
            // (thread/session/event/...) plus thread state when applicable.
            if let Ok(spec) = std::env::var("RUZU_DUMP_HANDLE") {
                for s in spec.split(',') {
                    let s = s.trim().trim_start_matches("0x").trim_start_matches("0X");
                    let Ok(handle) = u32::from_str_radix(s, 16) else {
                        continue;
                    };
                    let Some(object_id) = guard.handle_table.get_object(handle) else {
                        eprintln!("[DUMP]   HANDLE 0x{:08X} <not-in-handle-table>", handle);
                        continue;
                    };
                    let mut kinds: Vec<String> = Vec::new();
                    if let Some(thread) = guard.get_thread_by_object_id(object_id) {
                        let state = match thread.try_lock() {
                            Ok(t) => format!("tid={} state={:?}", t.thread_id, t.thread_state),
                            Err(_) => "<thread-lock-busy>".to_string(),
                        };
                        kinds.push(format!("thread({})", state));
                    }
                    if guard.get_session_by_object_id(object_id).is_some() {
                        kinds.push("session".to_string());
                    }
                    if guard.get_client_session_by_object_id(object_id).is_some() {
                        kinds.push("client_session".to_string());
                    }
                    if guard.get_server_session_by_object_id(object_id).is_some() {
                        kinds.push("server_session".to_string());
                    }
                    if let Some(event) = guard.get_event_by_object_id(object_id) {
                        let state = match event.try_lock() {
                            Ok(e) => format!("readable_event_id={}", e.readable_event_id),
                            Err(_) => "<event-lock-busy>".to_string(),
                        };
                        kinds.push(format!("event({})", state));
                    }
                    if let Some(revent) = guard.get_readable_event_by_object_id(object_id) {
                        let state = match revent.try_lock() {
                            Ok(e) => format!("{}", e.is_signaled()),
                            Err(_) => "<revent-lock-busy>".to_string(),
                        };
                        kinds.push(format!("readable_event(signaled={})", state));
                    }
                    if guard.get_shared_memory_by_object_id(object_id).is_some() {
                        kinds.push("shared_memory".to_string());
                    }
                    if guard.get_transfer_memory_by_object_id(object_id).is_some() {
                        kinds.push("transfer_memory".to_string());
                    }
                    eprintln!(
                        "[DUMP]   HANDLE 0x{:08X} object_id=0x{:X} kinds=[{}]",
                        handle,
                        object_id,
                        kinds.join(", ")
                    );
                }
            }
            // Dump 32 bytes (8 ARM32 insns) around each interesting PC so the
            // SIGUSR1 operator can see exactly what's spinning. ARM32 insns
            // are 4 bytes; Thumb insns are 2 or 4. We print raw little-endian
            // u32s starting 8 bytes before the PC.
            for pc in &pcs_to_dump {
                const WORDS_BEFORE: u64 = 32; // 128 bytes before PC (catches BNE-72 target + loop preamble)
                const WORDS_AFTER: u64 = 16; // 64 bytes after PC
                const TOTAL: usize = (WORDS_BEFORE + WORDS_AFTER + 1) as usize;
                let start = pc.saturating_sub(WORDS_BEFORE * 4);
                // Prefer the page_table's guest memory (virtual addresses
                // mapped by the loader). Fall back to process_memory if
                // not wired.
                let words: Option<Vec<u32>> =
                    if let Some(memory) = guard.page_table.get_base().m_memory.as_ref() {
                        // try_lock, never block: under a wedge the guest memory
                        // Mutex may be held forever by a stuck thread. Blocking
                        // here would hang the SIGUSR1 dumper before it reaches
                        // the per-thread wait-state walk (the CV/owner data).
                        match memory.try_lock() {
                            Ok(m) => {
                                let mut w = vec![0u32; TOTAL];
                                for (i, slot) in w.iter_mut().enumerate() {
                                    *slot = m.read_32(start + (i as u64) * 4);
                                }
                                Some(w)
                            }
                            Err(_) => None,
                        }
                    } else {
                        let mem = guard.process_memory.read().unwrap();
                        let len = (TOTAL as u64) * 4;
                        if !mem.is_valid_range(start, len as usize) {
                            None
                        } else {
                            let mut w = vec![0u32; TOTAL];
                            for (i, slot) in w.iter_mut().enumerate() {
                                *slot = mem.read_32(start + (i as u64) * 4);
                            }
                            Some(w)
                        }
                    };
                match words {
                    Some(w) => {
                        eprint!("[DUMP]   pc=0x{:X} insns:", pc);
                        for (i, insn) in w.iter().enumerate() {
                            if i as u64 == WORDS_BEFORE {
                                eprint!(" [{:08X}]", insn);
                            } else {
                                eprint!(" {:08X}", insn);
                            }
                        }
                        eprintln!();
                    }
                    None => eprintln!("[DUMP]   pc=0x{:X}: memory range not mapped", pc),
                }
            }
            // Per-core guest stack dump — 128 words (512 bytes) starting at SP.
            // The nnSdk SVC wrapper (e.g., `0x1D314F4`) is only one frame up
            // from the SVC stub; its caller's LR sits a few words deeper in
            // the stack. Walking these words reveals the game-level function
            // driving the spin loop. See `project_mk8d_post_rng_wedge_2026_04_24.md`.
            for (core_id, sp) in &stacks_to_dump {
                const STACK_WORDS: usize = 128;
                let mut stack_words: Vec<u32> = Vec::with_capacity(STACK_WORDS);
                if let Some(memory) = guard.page_table.get_base().m_memory.as_ref() {
                    // try_lock, never block (see pcs_to_dump rationale above).
                    match memory.try_lock() {
                        Ok(m) => {
                            for i in 0..STACK_WORDS {
                                let addr = sp + (i as u64) * 4;
                                if !m.is_valid_virtual_address_range(addr, 4) {
                                    break;
                                }
                                stack_words.push(m.read_32(addr));
                            }
                        }
                        Err(_) => {
                            eprintln!(
                                "[DUMP]   core={} sp=0x{:X}: <guest-memory-lock-busy>",
                                core_id, sp
                            );
                            continue;
                        }
                    }
                } else {
                    let mem = guard.process_memory.read().unwrap();
                    for i in 0..STACK_WORDS {
                        let addr = sp + (i as u64) * 4;
                        if !mem.is_valid_range(addr, 4) {
                            break;
                        }
                        stack_words.push(mem.read_32(addr));
                    }
                }
                if stack_words.is_empty() {
                    eprintln!("[DUMP]   core={} sp=0x{:X}: stack not mapped", core_id, sp);
                    continue;
                }
                eprint!("[DUMP]   core={} stack@0x{:X}:", core_id, sp);
                for (i, w) in stack_words.iter().enumerate() {
                    if i > 0 && (i & 7) == 0 {
                        eprint!("\n[DUMP]        +0x{:02X}:", i * 4);
                    }
                    eprint!(" {:08X}", w);
                }
                eprintln!();
            }
            // RUZU_DUMP_REGION raw u32 dumps — 8 words per line.
            for (start, bytes) in &region_dumps {
                let nwords = (bytes / 4) as usize;
                eprintln!(
                    "[DUMP] === REGION 0x{:X}..0x{:X} ({} u32 words) ===",
                    start,
                    start + bytes,
                    nwords
                );
                let mut all_words: Vec<u32> = Vec::with_capacity(nwords);
                if let Some(memory) = guard.page_table.get_base().m_memory.as_ref() {
                    let m = memory.lock().unwrap();
                    for i in 0..nwords {
                        all_words.push(m.read_32(start + (i as u64) * 4));
                    }
                } else {
                    let mem = guard.process_memory.read().unwrap();
                    if !mem.is_valid_range(*start, *bytes as usize) {
                        eprintln!("[DUMP]   region not mapped");
                        continue;
                    }
                    for i in 0..nwords {
                        all_words.push(mem.read_32(start + (i as u64) * 4));
                    }
                }
                for chunk_idx in 0..nwords.div_ceil(8) {
                    let off = chunk_idx * 8;
                    eprint!("[DUMP] 0x{:08X}:", start + (off as u64) * 4);
                    for w in &all_words[off..(off + 8).min(nwords)] {
                        eprint!(" {:08X}", w);
                    }
                    eprintln!();
                }
            }
            // RUZU_POKE_ADDR — experimental write into guest memory to test
            // whether a missing HLE signal is the root cause of a spin.
            for (addr, value) in &pokes {
                if let Some(memory) = guard.page_table.get_base().m_memory.as_ref() {
                    let m = memory.lock().unwrap();
                    let old = m.read_32(*addr);
                    m.write_32(*addr, *value);
                    let readback = m.read_32(*addr);
                    eprintln!(
                        "[POKE] addr=0x{:X} old=0x{:08X} wrote=0x{:08X} readback=0x{:08X}",
                        addr, old, value, readback
                    );
                } else {
                    eprintln!("[POKE] addr=0x{:X}: no page_table memory — skipping", addr);
                }
            }
            guard
                .thread_list
                .iter()
                .filter_map(|tid| {
                    guard
                        .get_thread_by_thread_id(*tid)
                        .map(|thread| (*tid, thread))
                })
                .collect()
        }
        None => {
            eprintln!(
                "[DUMP] process.lock() is CONTENDED after 3s poll ({} tries) — \
                 Mutex held continuously by some thread.",
                poll_count,
            );
            // Send SIGURG to every CPUCore_* and HLE:* host thread. Each will
            // invoke the SIGURG handler (libc::backtrace -> fd 2) which prints
            // its Rust stack. glibc backtrace is async-signal-safe.
            eprintln!("[DUMP] Triggering SIGURG backtrace on every worker thread...");
            #[cfg(target_os = "linux")]
            if let Ok(entries) = std::fs::read_dir("/proc/self/task") {
                for ent in entries.flatten() {
                    let Ok(tid_str) = ent.file_name().into_string() else {
                        continue;
                    };
                    let comm = std::fs::read_to_string(format!("/proc/self/task/{}/comm", tid_str))
                        .unwrap_or_default()
                        .trim()
                        .to_string();
                    if comm.starts_with("CPUCore_")
                        || comm.starts_with("HLE:")
                        || comm == "CoreTiming"
                    {
                        let tid: i32 = tid_str.parse().unwrap_or(-1);
                        if tid > 0 {
                            eprintln!("[DUMP] SIGURG -> host_tid={} comm={}", tid, comm);
                            unsafe {
                                libc::syscall(
                                    libc::SYS_tgkill,
                                    std::process::id() as i32,
                                    tid,
                                    libc::SIGURG,
                                );
                            }
                            // Sleep briefly so each thread's output doesn't
                            // interleave chaotically.
                            std::thread::sleep(std::time::Duration::from_millis(30));
                        }
                    }
                }
            }
            eprintln!("[DUMP] Host threads currently blocked in futex:");
            #[cfg(target_os = "linux")]
            if let Ok(entries) = std::fs::read_dir("/proc/self/task") {
                for ent in entries.flatten() {
                    let Ok(tid_str) = ent.file_name().into_string() else {
                        continue;
                    };
                    let comm = std::fs::read_to_string(format!("/proc/self/task/{}/comm", tid_str))
                        .unwrap_or_default()
                        .trim()
                        .to_string();
                    // Only interesting threads.
                    if !(comm.starts_with("CPUCore_")
                        || comm == "CoreTiming"
                        || comm.starts_with("DSP_")
                        || comm.starts_with("HLE:")
                        || comm == "ruzu-cmd")
                    {
                        continue;
                    }
                    let wchan =
                        std::fs::read_to_string(format!("/proc/self/task/{}/wchan", tid_str))
                            .unwrap_or_default()
                            .trim()
                            .to_string();
                    let state =
                        std::fs::read_to_string(format!("/proc/self/task/{}/stat", tid_str))
                            .ok()
                            .and_then(|s| s.split_whitespace().nth(2).map(|x| x.to_string()))
                            .unwrap_or_default();
                    let stack =
                        std::fs::read_to_string(format!("/proc/self/task/{}/stack", tid_str))
                            .unwrap_or_else(|_| "<stack unavailable>".into());
                    eprintln!(
                        "[DUMP]   host_tid={} comm={} state={} wchan={}",
                        tid_str, comm, state, wchan
                    );
                    for line in stack.lines().take(6) {
                        eprintln!("[DUMP]     {}", line.trim());
                    }
                }
            }
            eprintln!("=========================================");
            DUMP_REQUESTED.store(false, Ordering::Relaxed);
            return;
        }
    };

    // Walk each thread. Thread Arcs were captured while holding the process lock
    // above, so the dump does not reacquire process.lock() per-thread and hide
    // useful state behind false `<process-lock-contended>` rows.
    for (tid, thread_arc) in thread_entries {
        let try_result = thread_arc.try_lock();
        if let Ok(t) = try_result {
            let state = t.get_state();
            let thread_type = t.thread_type;
            let priority = t.get_priority();
            let current_core = t.get_current_core();
            let active_core = t.get_active_core();
            let wait_reason = t.get_wait_reason_for_debugging();
            let addr_key = t.get_address_key();
            let addr_key_val = t.get_address_key_value();
            let cv_key = t.get_condition_variable_key();
            let waiting_lock = t.get_waiting_lock_info().is_some();
            let lock_owner_tid = t.get_lock_owner_thread_id();
            let pc = t.thread_context.pc as u32;
            let lr = t.thread_context.lr as u32;
            let sp = t.thread_context.sp as u32;
            eprintln!(
                "[DUMP]   tid={} type={:?} state={:?} prio={} core={} active_core={} wait={:?} \
                 addr_key=0x{:X} addr_key_val=0x{:X} cv_key=0x{:X} \
                 waiting_lock={} lock_owner_tid={:?} pc=0x{:08X} lr=0x{:08X} sp=0x{:08X}",
                tid,
                thread_type,
                state,
                priority,
                current_core,
                active_core,
                wait_reason,
                addr_key.get(),
                addr_key_val,
                cv_key,
                waiting_lock,
                lock_owner_tid,
                pc,
                lr,
                sp,
            );
            // AArch32 GPRs r0-r12 (the join loop's subtask-array base is r7).
            let r = &t.thread_context.r;
            eprintln!(
                "[DUMP]        tid={} r0-r12: {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X} {:08X}",
                tid,
                r[0] as u32, r[1] as u32, r[2] as u32, r[3] as u32,
                r[4] as u32, r[5] as u32, r[6] as u32, r[7] as u32,
                r[8] as u32, r[9] as u32, r[10] as u32, r[11] as u32, r[12] as u32,
            );
        } else {
            eprintln!(
                "[DUMP]   tid={} <thread-lock-contended — held by someone>",
                tid,
            );
        }
    }
    // RUZU_DUMP_HOST_BT=1 — send SIGURG to every host worker thread so each
    // prints its native Rust backtrace (libc::backtrace -> fd 2). Unlike the
    // process-lock-contended branch this runs unconditionally, so we can see
    // the Rust stack of a host thread blocked INSIDE a synchronous HLE IPC
    // handler (the Sig-A SendSyncRequest wedge: guest tid RUNNABLE-but-frozen
    // in SVC 0x21 while its host fiber is stuck in the handler).
    if std::env::var_os("RUZU_DUMP_HOST_BT").is_some() {
        eprintln!("[DUMP] RUZU_DUMP_HOST_BT: SIGURG backtrace on every worker thread...");
        #[cfg(target_os = "linux")]
        if let Ok(entries) = std::fs::read_dir("/proc/self/task") {
            for ent in entries.flatten() {
                let Ok(tid_str) = ent.file_name().into_string() else {
                    continue;
                };
                let comm = std::fs::read_to_string(format!("/proc/self/task/{}/comm", tid_str))
                    .unwrap_or_default()
                    .trim()
                    .to_string();
                if comm.starts_with("CPUCore_")
                    || comm.starts_with("HLE:")
                    || comm.starts_with("DSP_")
                    || comm == "CoreTiming"
                    || comm == "GPU"
                    || comm == "VSyncThread"
                    || comm.starts_with("AudioRender")
                {
                    let host_tid: i32 = tid_str.parse().unwrap_or(-1);
                    if host_tid > 0 {
                        eprintln!("[DUMP] SIGURG -> host_tid={} comm={}", host_tid, comm);
                        unsafe {
                            libc::syscall(
                                libc::SYS_tgkill,
                                std::process::id() as i32,
                                host_tid,
                                libc::SIGURG,
                            );
                        }
                        std::thread::sleep(std::time::Duration::from_millis(40));
                    }
                }
            }
        }
    }
    crate::hle::kernel::svc::svc_thread::dump_thread_lifecycle_profile();
    eprintln!("=========================================");
    DUMP_REQUESTED.store(false, Ordering::Relaxed);
}

/// Real scheduler callbacks that access the kernel via KERNEL_PTR.
/// Wired to the scheduler lock during kernel initialization.
static SCHEDULER_CALLBACKS: super::k_scheduler_lock::SchedulerCallbacks =
    super::k_scheduler_lock::SchedulerCallbacks {
        disable_scheduling: real_disable_scheduling,
        enable_scheduling: real_enable_scheduling,
        update_highest_priority_threads: real_update_highest_priority_threads,
    };

fn real_disable_scheduling() {
    apply_pending_active_core_updates();

    if let Some(current_thread) = get_current_emu_thread() {
        let mut thread = current_thread.lock().unwrap();
        debug_assert!(thread.get_disable_dispatch_count() >= 0);
        thread.disable_dispatch();
    }
}

fn real_enable_scheduling(cores_needing_scheduling: u64) {
    apply_pending_active_core_updates();

    let kernel_ptr = KERNEL_PTR.load(Ordering::Acquire);
    if kernel_ptr.is_null() {
        return;
    }

    let kernel = unsafe { &*kernel_ptr };
    let current_thread = if get_current_thread_id_fast().is_some() {
        get_current_emu_thread()
    } else {
        get_current_emu_thread()
    };
    if current_thread.is_none() {
        KScheduler::reschedule_cores(cores_needing_scheduling);
        return;
    }
    let current_scheduler = kernel.current_scheduler();

    let current_tid = get_current_thread_id_fast();
    let disable_dispatch =
        with_current_thread_fast_mut(|t| t.get_disable_dispatch_count()).unwrap_or(-1);
    let state = with_current_thread_fast_mut(|t| t.get_state()).unwrap_or(ThreadState::INITIALIZED);
    log::trace!(
        "real_enable_scheduling: tid={:?} disable_dispatch={} state={:?} cores=0x{:x} has_scheduler={}",
        current_tid,
        disable_dispatch,
        state,
        cores_needing_scheduling,
        current_scheduler.is_some()
    );

    KScheduler::enable_scheduling_with_scheduler(
        cores_needing_scheduling,
        current_scheduler,
        kernel
            .is_phantom_mode_for_singlecore
            .load(Ordering::Relaxed),
    );
}

fn real_update_highest_priority_threads() -> u64 {
    use super::k_scheduler::KScheduler;

    let kernel_ptr = KERNEL_PTR.load(Ordering::Acquire);
    if kernel_ptr.is_null() {
        return 0;
    }
    let kernel = unsafe { &*kernel_ptr };

    let gsc_arc = match kernel.global_scheduler_context() {
        Some(gsc) => gsc.clone(),
        None => return 0,
    };

    // Collect scheduler arcs before locking GSC (lock order: GSC before schedulers).
    let sched_arcs: Vec<_> = (0..hardware_properties::NUM_CPU_CORES as usize)
        .filter_map(|i| kernel.scheduler(i).cloned())
        .collect();

    let migrations;
    let cores_needing_scheduling;
    {
        let mut gsc = gsc_arc.lock().unwrap();

        if !gsc.m_scheduler_update_needed.load(Ordering::Relaxed) {
            return 0;
        }

        let mut sched_guards: Vec<_> = sched_arcs.iter().map(|s| s.lock().unwrap()).collect();

        // Delegate to full implementation with idle core migration.
        let result = KScheduler::update_highest_priority_threads_impl(&mut sched_guards, &mut gsc);
        cores_needing_scheduling = result.0;
        migrations = result.1;
        // GSC lock released here.
    }

    if !migrations.is_empty() {
        enqueue_pending_active_core_updates(migrations);
    }

    apply_pending_active_core_updates();

    cores_needing_scheduling
}

fn enqueue_pending_active_core_updates(migrations: Vec<(u64, i32)>) {
    let mut pending = PENDING_ACTIVE_CORE_UPDATES.lock().unwrap();
    for (thread_id, new_core) in migrations {
        if let Some(existing) = pending
            .iter_mut()
            .find(|(pending_tid, _)| *pending_tid == thread_id)
        {
            existing.1 = new_core;
        } else {
            pending.push((thread_id, new_core));
        }
    }
}

fn apply_pending_active_core_updates() {
    let kernel_ptr = KERNEL_PTR.load(Ordering::Acquire);
    if kernel_ptr.is_null() {
        return;
    }
    let kernel = unsafe { &*kernel_ptr };
    let Some(gsc_arc) = kernel.global_scheduler_context() else {
        return;
    };

    let pending_work = {
        let mut pending = PENDING_ACTIVE_CORE_UPDATES.lock().unwrap();
        if pending.is_empty() {
            return;
        }
        std::mem::take(&mut *pending)
    };

    let gsc = gsc_arc.lock().unwrap();
    let mut still_pending = Vec::new();

    for (thread_id, new_core) in pending_work {
        let Some(thread) = gsc.get_thread_by_thread_id(thread_id) else {
            continue;
        };

        let try_lock_result = thread.try_lock();
        if let Ok(mut thread_guard) = try_lock_result {
            thread_guard.set_active_core(new_core);
        } else {
            still_pending.push((thread_id, new_core));
        }
    }

    drop(gsc);

    if !still_pending.is_empty() {
        let mut pending = PENDING_ACTIVE_CORE_UPDATES.lock().unwrap();
        for (thread_id, new_core) in still_pending {
            if let Some(existing) = pending
                .iter_mut()
                .find(|(pending_tid, _)| *pending_tid == thread_id)
            {
                existing.1 = new_core;
            } else {
                pending.push((thread_id, new_core));
            }
        }
    }
}

// Thread-local current thread pointer.
// Upstream: `static inline thread_local KThread* current_thread{nullptr}` in KernelCore::Impl.
// Each physical core host thread (and any other host thread) stores its own current KThread.
std::thread_local! {
    static CURRENT_THREAD: RefCell<Option<Weak<KThreadLock>>> = RefCell::new(None);
    static CURRENT_THREAD_ID: std::cell::Cell<u64> = const { std::cell::Cell::new(0) };
    static CURRENT_THREAD_PTR: std::cell::Cell<*mut KThread> = const { std::cell::Cell::new(std::ptr::null_mut()) };
    static HOST_DUMMY_THREAD: RefCell<Option<Arc<KThreadLock>>> = const { RefCell::new(None) };
}

fn get_or_create_host_dummy_thread(kernel: &KernelCore) -> Arc<KThreadLock> {
    HOST_DUMMY_THREAD.with(|cell| {
        if let Some(thread) = cell.borrow().as_ref() {
            return Arc::clone(thread);
        }

        let thread = Arc::new(KThreadLock::new(KThread::new()));
        {
            let thread_id = kernel.create_new_thread_id();
            let object_id = kernel.create_new_object_id() as u64;
            let mut guard = thread.lock().unwrap();
            let rc = guard.initialize_dummy_thread(None, thread_id, object_id);
            assert_eq!(rc, crate::hle::result::RESULT_SUCCESS.get_inner_value());
            guard.bind_self_reference(&thread);
        }

        *cell.borrow_mut() = Some(Arc::clone(&thread));
        thread
    })
}

/// Get the current emulation thread for the calling host thread.
/// Upstream: `KernelCore::Impl::GetCurrentEmuThread()`.
pub fn get_current_emu_thread() -> Option<Arc<KThreadLock>> {
    let current = CURRENT_THREAD.with(|cell| cell.borrow().as_ref().and_then(Weak::upgrade));
    if current.is_some() {
        return current;
    }

    let kernel_ptr = KERNEL_PTR.load(Ordering::Acquire);
    if kernel_ptr.is_null() {
        return None;
    }

    let kernel = unsafe { &*kernel_ptr };
    let dummy = get_or_create_host_dummy_thread(kernel);
    set_current_emu_thread(Some(&dummy));
    Some(dummy)
}

/// Set the current emulation thread for the calling host thread.
/// Upstream: `KernelCore::Impl::SetCurrentEmuThread(KThread*)`.
pub fn set_current_emu_thread(thread: Option<&Arc<KThreadLock>>) {
    CURRENT_THREAD.with(|cell| {
        *cell.borrow_mut() = thread.map(Arc::downgrade);
    });
    CURRENT_THREAD_ID.with(|cell| {
        cell.set(
            thread
                .map(|thread| thread.lock().unwrap().get_thread_id())
                .unwrap_or(0),
        );
    });
    CURRENT_THREAD_PTR.with(|cell| {
        let ptr = thread
            .map(|thread| {
                let mut guard = thread.lock().unwrap();
                (&mut *guard) as *mut KThread
            })
            .unwrap_or(std::ptr::null_mut());
        cell.set(ptr);
    });
}

/// Ensure the current host thread has a `CURRENT_THREAD` populated —
/// either a real emu thread set by `set_current_emu_thread`, or the
/// lazily-created per-host-thread dummy KThread. After this returns,
/// `CURRENT_THREAD_ID` is non-zero and `CURRENT_THREAD_PTR` is non-null,
/// matching upstream's invariant that `GetCurrentThreadPointer(kernel)`
/// is total.
///
/// Returns `false` only when the kernel itself has not been initialized
/// (`KERNEL_PTR` is null, e.g., in unit tests with no kernel).
fn ensure_current_thread_populated() -> bool {
    if CURRENT_THREAD_ID.with(|cell| cell.get()) != 0 {
        return true;
    }
    // get_current_emu_thread lazily creates the dummy and calls
    // set_current_emu_thread, which populates all three thread-local
    // fields (CURRENT_THREAD / _ID / _PTR).
    get_current_emu_thread().is_some()
}

pub fn get_current_thread_id_fast() -> Option<u64> {
    // Upstream totality: GetCurrentThreadPointer is always valid during
    // CPU execution. Populate lazily via the dummy-thread fallback if
    // the thread-local hasn't been set yet on this host thread.
    let thread_id = CURRENT_THREAD_ID.with(|cell| cell.get());
    if thread_id != 0 {
        return Some(thread_id);
    }
    if !ensure_current_thread_populated() {
        return None;
    }
    let id = CURRENT_THREAD_ID.with(|cell| cell.get());
    if id == 0 {
        None
    } else {
        Some(id)
    }
}

pub fn with_current_thread_fast_mut<R>(f: impl FnOnce(&mut KThread) -> R) -> Option<R> {
    // Same totality semantics as get_current_thread_id_fast.
    let ptr = CURRENT_THREAD_PTR.with(|cell| cell.get());
    if !ptr.is_null() {
        return Some(unsafe { f(&mut *ptr) });
    }
    if !ensure_current_thread_populated() {
        return None;
    }
    CURRENT_THREAD_PTR.with(|cell| {
        let ptr = cell.get();
        if ptr.is_null() {
            None
        } else {
            Some(unsafe { f(&mut *ptr) })
        }
    })
}

/// Get the current thread pointer for the calling host thread.
/// Upstream: `GetCurrentThreadPointer(kernel)`.
/// Returns None if no thread is set.
pub fn get_current_thread_pointer() -> Option<Arc<KThreadLock>> {
    get_current_emu_thread()
}

/// Get the current hardware timer tick for the active kernel instance.
/// Returns `None` when no kernel or hardware timer is initialized.
pub fn get_current_hardware_tick() -> Option<i64> {
    let kernel_ptr = KERNEL_PTR.load(Ordering::Acquire);
    if kernel_ptr.is_null() {
        return None;
    }

    let kernel = unsafe { &*kernel_ptr };
    if let Some(core_timing) = kernel.core_timing() {
        return Some(core_timing.get_global_time_ns().as_nanos() as i64);
    }

    kernel.hardware_timer().map(|timer| timer.get_tick())
}

/// Get the global hardware timer Arc for the active kernel instance.
pub fn get_hardware_timer_arc() -> Option<Arc<KHardwareTimer>> {
    let kernel_ptr = KERNEL_PTR.load(Ordering::Acquire);
    if kernel_ptr.is_null() {
        return None;
    }

    let kernel = unsafe { &*kernel_ptr };
    kernel.hardware_timer().cloned()
}

/// Constants from the upstream KernelCore::Impl.
const APPLICATION_MEMORY_BLOCK_SLAB_HEAP_SIZE: usize = 20000;
const SYSTEM_MEMORY_BLOCK_SLAB_HEAP_SIZE: usize = 10000;
const BLOCK_INFO_SLAB_HEAP_SIZE: usize = 4000;
const RESERVED_DYNAMIC_PAGE_COUNT: usize = 64;

/// Represents a single instance of the kernel.
///
/// Maps to upstream KernelCore and its inner Impl struct.
pub struct KernelCore {
    // -- Initialization state --
    is_multicore: bool,
    is_shutting_down: AtomicBool,
    is_phantom_mode_for_singlecore: AtomicBool,
    exception_exited: bool,

    // -- ID counters --
    next_object_id: AtomicU32,
    next_kernel_process_id: AtomicU64,
    next_user_process_id: AtomicU64,
    next_thread_id: AtomicU64,

    // -- Subsystems --
    hardware_timer: Option<Arc<KHardwareTimer>>,
    global_object_list_container: Option<KAutoObjectWithListContainer>,
    global_scheduler_context: Option<Arc<Mutex<GlobalSchedulerContext>>>,
    object_name_global_data: Option<KObjectNameGlobalData>,

    // -- Physical cores and schedulers --
    /// Per-core KScheduler instances.
    /// Upstream: `std::array<std::unique_ptr<Kernel::KScheduler>, NUM_CPU_CORES> schedulers`.
    schedulers: Vec<Arc<Mutex<KScheduler>>>,
    /// Per-core PhysicalCore instances.
    /// Upstream: `std::array<std::unique_ptr<Kernel::PhysicalCore>, NUM_CPU_CORES> cores`.
    cores: Vec<Arc<PhysicalCore>>,

    /// Per-core shutdown threads.
    /// Upstream: created in `InitializeShutdownThreads()` via `KThread::InitializeHighPriorityThread`.
    shutdown_threads: Vec<Arc<KThreadLock>>,
    /// Per-core main threads.
    /// Upstream: created in `InitializePhysicalCores()` via `KThread::InitializeMainThread`.
    main_threads: Vec<Arc<KThreadLock>>,
    /// Per-core idle threads.
    /// Upstream: created in `InitializePhysicalCores()` via `KThread::InitializeIdleThread`.
    idle_threads: Vec<Arc<KThreadLock>>,

    /// The application's main thread (created by KProcess::run).
    /// Used to set as the current thread when entering guest dispatch.
    application_thread: Option<Arc<KThreadLock>>,

    // -- Slab resource counts --
    slab_resource_counts: KSlabResourceCounts,

    // -- Process tracking --
    /// Kernel process list.
    ///
    /// Upstream: `KernelCore::Impl::process_list`, populated by `KProcess::Register`.
    /// Rust stores the stable `Arc<ProcessLock>` owners instead of raw `KProcess*`.
    process_list: Mutex<Vec<Arc<ProcessLock>>>,
    process_list_lock: Mutex<()>,

    // -- Registered objects for leak tracking --
    registered_objects: Mutex<Vec<u64>>,
    registered_in_use_objects: Mutex<Vec<u64>>,

    // -- Host thread management --
    next_host_thread_id: AtomicU32,
    /// In single-core mode, the host thread ID of the single core thread.
    /// Upstream: `u32 single_core_thread_id{}` in Impl.
    single_core_thread_id: AtomicU32,

    // -- Memory management --
    /// Physical memory manager. Upstream: `Impl::memory_manager`.
    memory_manager: KMemoryManager,

    /// Kernel-wide resource limit. Upstream:
    /// `KernelCore::Impl::system_resource_limit` set by
    /// `InitializeSystemResourceLimit` at boot. Holds PhysicalMemoryMax,
    /// ThreadCountMax, EventCountMax, TransferMemoryCountMax, SessionCountMax.
    /// Used by services like KSystemControl::GetInsecureMemoryResourceLimit.
    system_resource_limit: Option<Arc<Mutex<super::k_resource_limit::KResourceLimit>>>,

    /// Kernel-wide slab manager for KMemoryBlock nodes used by
    /// `KMemoryBlockManagerUpdateAllocator`. Upstream:
    /// `KernelCore::Impl::application_memory_block_manager` /
    /// `system_memory_block_manager`. ruzu uses a single slab for all
    /// processes since application/system distinction isn't enforced yet.
    memory_block_slab_manager:
        Option<Arc<super::k_dynamic_resource_manager::KMemoryBlockSlabManager>>,

    /// Kernel-wide page-table-page allocator. Upstream:
    /// `KernelCore::Impl::page_table_manager` initialized from
    /// `KernelPageTableHeapSize` at boot. Used by `KPageTableBase::Operate`
    /// to allocate L1/L2/L3 page-table pages and by `FinalizeUpdate` to
    /// free them. ruzu's flat `common::page_table::PageTable` doesn't
    /// produce freeable pages today, but the manager is wired so the
    /// future port of multi-level guest page tables lands cleanly.
    page_table_manager: Option<Arc<super::k_page_table_manager::KPageTableManager>>,

    /// Kernel-wide physical memory layout. Upstream:
    /// `KernelCore::Impl::memory_layout` populated at boot by
    /// `KMemoryLayoutInit` from the SoC region tree. ruzu populates with
    /// `populate_default_dram_user_pools` at boot — the same data
    /// `core.rs` previously hardcoded inline for `initialize_pool` calls.
    memory_layout: Option<Arc<Mutex<super::k_memory_layout::KMemoryLayout>>>,

    // -- Core timing --
    /// Reference to the system's CoreTiming.
    /// Upstream: accessed via `system.CoreTiming()` through `System& system` reference.
    /// Stored here so fiber closures (guest_activate, idle thread) can access it
    /// without needing a System reference.
    core_timing: Option<Arc<CoreTiming>>,

    /// Reference to the owning System.
    /// Upstream: `Core::System& system` stored in KernelCore::Impl.
    /// Used by SVC dispatch (`Svc::Call(system, svc_number)`) and other
    /// kernel operations that need access to System-level state.
    system_ref: crate::core::SystemRef,

    /// Preemption timer event (10ms interval).
    /// Upstream: `std::shared_ptr<Core::Timing::EventType> preemption_event`.
    preemption_event: Option<Arc<parking_lot::Mutex<crate::core_timing::EventType>>>,

    /// Dedicated preemption thread that fires every 10ms independently of
    /// CoreTiming. Works around the CoreTiming event-starvation bug where
    /// looping events stop being collected from the priority queue after
    /// the initial burst.
    preemption_thread: Mutex<Option<std::thread::JoinHandle<()>>>,
    preemption_stop: Arc<std::sync::atomic::AtomicBool>,

    /// Active service server managers.
    /// Upstream: `Impl::server_managers`.
    server_managers: Mutex<Vec<TrackedServerManager>>,

    /// Guest service processes created by `RunOnGuestCoreProcess`.
    /// Upstream keeps them alive after `KProcess::Register(*this, process)`.
    service_processes: Mutex<Vec<Arc<ProcessLock>>>,

    /// Host service processes created by `RunOnHostCoreProcess`.
    /// Upstream keeps them alive after `KProcess::Register(*this, process)` too.
    host_service_processes: Mutex<Vec<Arc<ProcessLock>>>,
}

// KProcess initial ID constants (matching upstream).
const INITIAL_PROCESS_ID_MIN: u64 = 1;
const PROCESS_ID_MIN: u64 = 81;

impl KernelCore {
    /// Construct a new kernel instance.
    pub fn new() -> Self {
        Self {
            is_multicore: true,
            is_shutting_down: AtomicBool::new(false),
            is_phantom_mode_for_singlecore: AtomicBool::new(false),
            exception_exited: false,

            next_object_id: AtomicU32::new(0),
            next_kernel_process_id: AtomicU64::new(INITIAL_PROCESS_ID_MIN),
            next_user_process_id: AtomicU64::new(PROCESS_ID_MIN),
            next_thread_id: AtomicU64::new(1),

            hardware_timer: None,
            global_object_list_container: None,
            global_scheduler_context: None,
            object_name_global_data: None,

            schedulers: Vec::new(),
            cores: Vec::new(),

            shutdown_threads: Vec::new(),
            main_threads: Vec::new(),
            idle_threads: Vec::new(),
            application_thread: None,

            slab_resource_counts: KSlabResourceCounts::create_default(),

            process_list: Mutex::new(Vec::new()),
            process_list_lock: Mutex::new(()),
            registered_objects: Mutex::new(Vec::new()),
            registered_in_use_objects: Mutex::new(Vec::new()),

            memory_manager: KMemoryManager::new(),
            system_resource_limit: None,
            memory_block_slab_manager: None,
            page_table_manager: None,
            memory_layout: None,
            next_host_thread_id: AtomicU32::new(hardware_properties::NUM_CPU_CORES),
            single_core_thread_id: AtomicU32::new(0),
            core_timing: None,
            system_ref: crate::core::SystemRef::null(),
            preemption_event: None,
            preemption_thread: Mutex::new(None),
            preemption_stop: Arc::new(std::sync::atomic::AtomicBool::new(false)),
            server_managers: Mutex::new(Vec::new()),
            service_processes: Mutex::new(Vec::new()),
            host_service_processes: Mutex::new(Vec::new()),
        }
    }

    /// Set whether emulation is multicore or single core.
    /// Must be called before Initialize.
    pub fn set_multicore(&mut self, is_multicore: bool) {
        self.is_multicore = is_multicore;
    }

    /// Initialize the kernel.
    pub fn initialize(&mut self) {
        self.hardware_timer = Some(Arc::new(KHardwareTimer::new()));

        self.global_object_list_container = Some(KAutoObjectWithListContainer::new());
        self.global_scheduler_context = Some(Arc::new(Mutex::new(GlobalSchedulerContext::new())));

        // Cache the GSC's `m_scheduler_lock` raw pointer so any site can
        // acquire `KScopedSchedulerLock` via `kernel::scheduler_lock()`.
        // The GSC sits behind an Arc held by the kernel for its entire
        // lifetime — the address of `m_scheduler_lock` is stable.
        if let Some(ref gsc_arc) = self.global_scheduler_context {
            let gsc_guard = gsc_arc.lock().unwrap();
            let sl_ptr = &gsc_guard.m_scheduler_lock
                as *const super::k_scheduler_lock::KAbstractSchedulerLock
                as *mut super::k_scheduler_lock::KAbstractSchedulerLock;
            SCHEDULER_LOCK_PTR.store(sl_ptr, Ordering::Release);
        }

        self.is_phantom_mode_for_singlecore
            .store(false, Ordering::Relaxed);

        // Initialize slab resource counts.
        super::init::init_slab_setup::initialize_slab_resource_counts(
            &mut self.slab_resource_counts,
        );

        // Initialize shutdown threads (before physical cores, matching upstream order).
        self.initialize_shutdown_threads();

        // Initialize physical cores.
        self.initialize_physical_cores();

        // Initialize global data.
        self.object_name_global_data = Some(KObjectNameGlobalData::new());

        // Wire up scheduler lock callbacks.
        // The callbacks need kernel access but are plain fn pointers.
        // Store a raw pointer to self in a static for the callbacks to use.
        KERNEL_PTR.store(self as *mut KernelCore, Ordering::Release);
        if let Some(ref gsc) = self.global_scheduler_context {
            gsc.lock()
                .unwrap()
                .m_scheduler_lock
                .set_callbacks(&SCHEDULER_CALLBACKS);
        }

        // Initialize preemption event.
        // Upstream: InitializePreemption schedules a looping CoreTiming event every 10ms
        // that calls PreemptThreads + Interrupt on each core, forcing the JIT out of
        // tight loops so the scheduler can reschedule threads.
        // Upstream: InitializePreemption creates a looping CoreTiming event.
        // The callback takes KScopedSchedulerLock then calls PreemptThreads.
        // Additionally, we interrupt all cores to force the JIT to exit
        // linked block loops and check is_interrupted in the outer loop.
        self.preemption_event = Some(crate::core_timing::create_event(
            "PreemptionCallback".to_string(),
            Box::new(move |_time, _ns| {
                let kernel_ptr = KERNEL_PTR.load(Ordering::Acquire);
                if kernel_ptr.is_null() {
                    return None;
                }
                let kernel = unsafe { &*kernel_ptr };

                // PreemptThreads (rotate priority queue).
                if let Some(gsc_arc) = kernel.global_scheduler_context() {
                    let mut gsc = gsc_arc.lock().unwrap();
                    gsc.preempt_threads();
                    drop(gsc);
                }

                // Local ruzu addition: interrupt cores to force JIT exits.
                // Upstream only rotates scheduled queues here; keep this
                // diagnostic gate to isolate preemption-vs-JIT-exit effects.
                if std::env::var_os("RUZU_NO_PREEMPT_INTERRUPTS").is_none() {
                    for core_id in 0..hardware_properties::NUM_CPU_CORES as usize {
                        if let Some(core) = kernel.physical_core(core_id) {
                            core.interrupt();
                        }
                    }
                }

                None
            }),
        ));

        // Upstream: RegisterHostThread(nullptr) at the end of Kernel initialization.
        self.register_host_thread();
    }

    /// Run a service function on a guest core as a KThread with fiber context.
    ///
    /// Port of upstream `KernelCore::RunOnGuestCoreProcess` (kernel.cpp:1105-1139).
    /// Creates a new KProcess and KThread, initializes the thread as a service
    /// thread (HighPriority, core 3, priority 16), and makes it schedulable.
    /// The scheduler will pick it up and run the fiber on guest core 3.
    pub fn run_on_guest_core_process(&self, name: &str, func: Box<dyn FnOnce() + Send>) {
        const SERVICE_THREAD_PRIORITY: i32 = 16;
        const SERVICE_THREAD_CORE: i32 = 3;

        // Create a service process for tracking.
        let process = Arc::new(ProcessLock::from_value(super::k_process::KProcess::new()));
        self.register_process(Arc::clone(&process));
        // Minimal init — service processes don't need page tables or user memory.
        self.service_processes
            .lock()
            .unwrap()
            .push(Arc::clone(&process));

        // Create the service thread.
        let thread = Arc::new(KThreadLock::new(super::k_thread::KThread::new()));
        let thread_id = self.create_new_thread_id();
        let object_id = self.create_new_object_id() as u64;
        if std::env::var_os("RUZU_TRACE_THREAD_ID").is_some() {
            log::info!("[THREAD_ID_NAME] tid={} name=service:{}", thread_id, name);
        }

        // Give the process a scheduler reference for the target core.
        // Upstream: service threads run on core 3, so use core 3's scheduler.
        if let Some(scheduler) = self.scheduler(SERVICE_THREAD_CORE as usize) {
            process.lock().unwrap().scheduler = Some(Arc::downgrade(scheduler));
        }
        // Wire GSC so the thread's notify_state_transition can update PQ directly.
        if let Some(gsc) = self.global_scheduler_context() {
            process
                .lock()
                .unwrap()
                .set_global_scheduler_context(gsc.clone());
        }

        {
            let mut t = thread.lock().unwrap();
            t.initialize_service_thread(
                self.system_ref,
                &thread,
                func,
                SERVICE_THREAD_PRIORITY,
                SERVICE_THREAD_CORE,
                &process,
                thread_id,
                object_id,
            );
        }

        // Upstream registers the thread before making it runnable.
        process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&thread));
        self.register_kernel_object(thread.lock().unwrap().get_object_id());

        // Make the thread runnable. KThread::run_thread() → set_state(RUNNABLE) →
        // notify_state_transition pushes to PQ via the GSC reference
        // (wired during initialize_service_thread from the process).
        // Must be called OUTSIDE GSC lock scope to avoid deadlock.
        super::k_thread::KThread::run_thread(&thread);

        // Request reschedule so the scheduler picks up the new thread.
        // Upstream: SetSchedulerUpdateNeeded + KScopedSchedulerLock release triggers reschedule.
        // Request reschedule on ALL cores (upstream does this via SetSchedulerUpdateNeeded
        // which marks a global flag checked by all cores).
        for core_id in 0..crate::hardware_properties::NUM_CPU_CORES as usize {
            if let Some(scheduler) = self.scheduler(core_id) {
                scheduler.lock().unwrap().request_schedule();
            }
        }
        // Verify the thread is in the priority queue.
        if let Some(gsc) = self.global_scheduler_context() {
            let gsc = gsc.lock().unwrap();
            let front = gsc
                .m_priority_queue
                .get_scheduled_front(SERVICE_THREAD_CORE);
            log::info!(
                "KernelCore::run_on_guest_core_process: '{}' thread_id={} core={} priority={} pq_front={:?}",
                name, thread_id, SERVICE_THREAD_CORE, SERVICE_THREAD_PRIORITY, front
            );
        }

        // Match zuyu: just after spawning the FIRST TWO guest services (sm and
        // account), zuyu also allocates a Dummy thread per service. This comes
        // from host service threads (audio/nvservices/etc.) that were spawned
        // earlier finally getting OS-scheduled and registering child host
        // threads. The 2 extras land at zuyu tids 24 and 26. Without these,
        // ruzu's MK8D main thread is +2 lower than zuyu's.
        // Allocate matching dummies for the first 2 services only.
        let is_sm_or_account = name == "sm" || name == "account";
        if is_sm_or_account {
            let dummy = Arc::new(KThreadLock::new(super::k_thread::KThread::new()));
            let dummy_tid = self.create_new_thread_id();
            let dummy_oid = self.create_new_object_id() as u64;
            {
                let mut g = dummy.lock().unwrap();
                let rc = g.initialize_dummy_thread(Some(&process), dummy_tid, dummy_oid);
                assert_eq!(rc, crate::hle::result::RESULT_SUCCESS.get_inner_value());
                g.bind_self_reference(&dummy);
            }
            process
                .lock()
                .unwrap()
                .register_thread_object(Arc::clone(&dummy));
            log::info!(
                "[THREAD_ID_NAME] tid={} name=process_dummy:{}",
                dummy_tid,
                name
            );
        }
    }

    /// Run a service function on a host thread with a dummy KThread for tracking.
    ///
    /// Shared helper for spawning a host OS thread with a dummy KThread.
    ///
    /// Port of upstream `RunHostThreadFunc(kernel, process, thread_name, func)`
    /// (kernel.cpp:1044-1075).
    ///
    /// Creates a dummy KThread owned by `process`, registers it, then spawns
    /// a host thread that sets the dummy as the current emulation thread and
    /// runs `func`.
    fn run_host_thread_func(
        &self,
        process: &Arc<ProcessLock>,
        thread_name: String,
        func: Box<dyn FnOnce() + Send>,
    ) -> std::thread::JoinHandle<()> {
        let kernel_ptr = self as *const KernelCore as usize;

        let thread = Arc::new(KThreadLock::new(KThread::new()));
        {
            let thread_id = self.create_new_thread_id();
            let object_id = self.create_new_object_id() as u64;
            if std::env::var_os("RUZU_TRACE_THREAD_ID").is_some() {
                log::info!(
                    "[THREAD_ID_NAME] tid={} name=host:{}",
                    thread_id,
                    thread_name
                );
            }
            let mut thread_guard = thread.lock().unwrap();
            let rc = thread_guard.initialize_dummy_thread(Some(process), thread_id, object_id);
            assert_eq!(rc, crate::hle::result::RESULT_SUCCESS.get_inner_value());
            thread_guard.bind_self_reference(&thread);
        }
        process
            .lock()
            .unwrap()
            .register_thread_object(Arc::clone(&thread));

        std::thread::Builder::new()
            .name(thread_name.clone())
            .spawn({
                let thread = Arc::clone(&thread);
                move || {
                    let kernel = unsafe { &*(kernel_ptr as *const KernelCore) };
                    kernel.register_host_thread_with_existing(Some(&thread));
                    // Per-thread alternate signal stack so the rdynarmic SIGSEGV
                    // handler (SA_ONSTACK; sigaltstack is per-thread) runs on a
                    // dedicated 2MB stack rather than this host thread's stack —
                    // otherwise a fault that reaches the handler's
                    // FastmemPatchTable HashMap/SipHash lookup can overflow the
                    // stack into a secondary SIGSEGV (silent exit 139). Mirrors
                    // the CPU-core registration in cpu_manager.rs.
                    rdynarmic::backend::x64::exception_handler::register_thread_signal_stack();
                    log::info!("Host service thread '{}' started", thread_name);
                    func();
                    log::info!("Host service thread '{}' exited", thread_name);
                }
            })
            .expect("Failed to spawn host service thread")
    }

    /// Port of upstream `KernelCore::RunOnHostCoreProcess` (kernel.cpp:1077-1094).
    ///
    /// Creates a new KProcess, then delegates to `run_host_thread_func`.
    /// Used for CPU-intensive services (audio, filesystem, etc.) that benefit
    /// from running on host hardware.
    pub fn run_on_host_core_process(
        &self,
        name: &str,
        func: Box<dyn FnOnce() + Send>,
    ) -> std::thread::JoinHandle<()> {
        let process = Arc::new(ProcessLock::from_value(KProcess::new()));
        {
            let mut process_guard = process.lock().unwrap();
            let rc = process_guard.initialize(&[], 0, 0, 0, 0, 0, None, false);
            assert_eq!(rc, crate::hle::result::RESULT_SUCCESS.get_inner_value());
            process_guard.bind_self_reference(&process);
        }

        self.register_process(Arc::clone(&process));
        self.host_service_processes
            .lock()
            .unwrap()
            .push(Arc::clone(&process));

        self.run_host_thread_func(&process, format!("HLE:{}", name), func)
    }

    /// Port of upstream `KernelCore::RunOnHostCoreThread` (kernel.cpp:1096-1103).
    ///
    /// Reuses the current emulation thread's process, then delegates to
    /// `run_host_thread_func`. Used by `ServerManager::start_additional_host_threads`.
    pub fn run_on_host_core_thread(
        &self,
        name: &str,
        func: Box<dyn FnOnce() + Send>,
    ) -> std::thread::JoinHandle<()> {
        let process = self
            .get_current_emu_thread()
            .and_then(|t| t.lock().unwrap().parent.as_ref().and_then(Weak::upgrade))
            .expect("run_on_host_core_thread: no current process");

        self.run_host_thread_func(&process, name.to_string(), func)
    }

    /// Wire the hardware timer to CoreTiming.
    /// Must be called after initialize() when System has CoreTiming available.
    pub fn wire_hardware_timer(&self, core_timing: Arc<CoreTiming>) {
        if let Some(ref timer) = self.hardware_timer {
            KHardwareTimer::wire_callback(timer, core_timing);
        }
    }

    /// Store a reference to CoreTiming so that fiber closures (guest_activate,
    /// idle thread) can access it without needing a System reference.
    /// Must be called after System creates CoreTiming and before CPU threads start.
    pub fn set_core_timing(&mut self, core_timing: Arc<CoreTiming>) {
        self.core_timing = Some(core_timing);
    }

    /// Get the CoreTiming reference.
    /// Upstream: accessed via `system.CoreTiming()`.
    pub fn core_timing(&self) -> Option<&Arc<CoreTiming>> {
        self.core_timing.as_ref()
    }

    /// Schedule the preemption timer event (10ms interval).
    /// Matches upstream `InitializePreemption(kernel)` in kernel.cpp.
    /// Must be called after set_core_timing().
    pub fn schedule_preemption_event(&self, core_timing: &Arc<CoreTiming>) {
        if let Some(ref event) = self.preemption_event {
            let interval = std::time::Duration::from_millis(10);
            core_timing.schedule_looping_event(interval, interval, event, false);
            log::info!("KernelCore: preemption event scheduled (10ms interval)");
        }

        // Also start a dedicated preemption thread that fires independently
        // of CoreTiming. This works around the CoreTiming event-starvation
        // bug where looping events stop being collected after the initial
        // burst, causing the JIT to run unpreempted forever.
        //
        // The same thread also services SIGUSR1 dump requests — the signal
        // handler sets DUMP_REQUESTED; this thread polls it every 10ms and
        // runs `dump_thread_state` outside signal context.
        install_sigusr1_handler();

        let stop = Arc::clone(&self.preemption_stop);
        let kernel_ptr_val = KERNEL_PTR.load(Ordering::Acquire);
        if std::env::var_os("RUZU_DISABLE_PREEMPTION_THREAD").is_some() {
            log::warn!("RUZU_DISABLE_PREEMPTION_THREAD=1 -> skipping local preemption thread");
        } else if !kernel_ptr_val.is_null() {
            let thread = std::thread::Builder::new()
                .name("PreemptionThread".to_string())
                .spawn(move || {
                    while !stop.load(Ordering::Relaxed) {
                        std::thread::sleep(std::time::Duration::from_millis(10));
                        let kp = KERNEL_PTR.load(Ordering::Acquire);
                        if kp.is_null() {
                            break;
                        }
                        let kernel = unsafe { &*kp };
                        if std::env::var_os("RUZU_NO_PREEMPT_INTERRUPTS").is_none() {
                            for core_id in 0..hardware_properties::NUM_CPU_CORES as usize {
                                if let Some(core) = kernel.physical_core(core_id) {
                                    core.interrupt();
                                }
                            }
                        }
                        if DUMP_REQUESTED.load(Ordering::Relaxed) {
                            dump_thread_state(kernel);
                        }
                    }
                })
                .expect("failed to spawn preemption thread");
            *self.preemption_thread.lock().unwrap() = Some(thread);
            log::info!("KernelCore: dedicated preemption thread started (10ms interval)");
        }
    }

    /// Set the System reference.
    /// Upstream: `KernelCore(System& system)` stores it at construction.
    pub fn set_system_ref(&mut self, system_ref: crate::core::SystemRef) {
        self.system_ref = system_ref;
    }

    /// Get the System reference.
    /// Upstream: `KernelCore::System()`.
    pub fn system(&self) -> crate::core::SystemRef {
        self.system_ref
    }

    /// Set the application's main thread.
    pub fn set_application_thread(&mut self, thread: Arc<KThreadLock>) {
        self.application_thread = Some(thread);
    }

    /// Get the application's main thread.
    pub fn get_application_thread(&self) -> Option<Arc<KThreadLock>> {
        self.application_thread.clone()
    }

    /// Shutdown the kernel.
    pub fn shutdown(&mut self) {
        self.is_shutting_down.store(true, Ordering::Relaxed);

        let _ = self.close_services();

        self.next_object_id.store(0, Ordering::Relaxed);
        self.next_kernel_process_id
            .store(INITIAL_PROCESS_ID_MIN, Ordering::Relaxed);
        self.next_user_process_id
            .store(PROCESS_ID_MIN, Ordering::Relaxed);
        self.next_thread_id.store(1, Ordering::Relaxed);

        // Clean up registered objects.
        {
            let mut in_use = self.registered_in_use_objects.lock().unwrap();
            in_use.clear();
        }
        {
            let mut registered = self.registered_objects.lock().unwrap();
            if !registered.is_empty() {
                log::debug!(
                    "{} kernel objects were dangling on shutdown!",
                    registered.len()
                );
                registered.clear();
            }
        }

        self.object_name_global_data = None;

        // Upstream: schedulers[core_id].reset() per core.
        self.schedulers.clear();
        self.shutdown_threads.clear();
        self.main_threads.clear();
        self.idle_threads.clear();
        self.service_processes.lock().unwrap().clear();
        self.host_service_processes.lock().unwrap().clear();

        if let Some(ref container) = self.global_object_list_container {
            container.finalize();
        }
        self.global_object_list_container = None;

        if let Some(timer) = self.hardware_timer.as_mut() {
            Arc::get_mut(timer)
                .expect("hardware timer still shared at shutdown")
                .finalize();
        }
        self.hardware_timer = None;

        self.is_shutting_down.store(false, Ordering::Relaxed);
    }

    /// Close all active services.
    /// Upstream `KernelCore::CloseServices()` clears the tracked
    /// `ServerManager` owners; `ServerManager::~ServerManager` requests stop,
    /// signals its wakeup event, waits for the loop to stop, and clears extra
    /// `jthread`s. Rust stores service managers behind `Arc<Mutex<_>>`, and
    /// the service loop may hold that mutex while blocked in `WaitSignaled`.
    /// Keep stop/wakeup handles outside the mutex so the destructor ordering
    /// can be reproduced without ABBA-deadlocking the close path.
    pub fn close_services(&self) -> bool {
        let server_managers = {
            let mut managers = self.server_managers.lock().unwrap();
            std::mem::take(&mut *managers)
        };

        for tracked in &server_managers {
            tracked.stop_requested.store(true, Ordering::Release);
            tracked.wakeup_event.signal();
        }

        let mut closed_all = true;
        for tracked in server_managers {
            let manager = tracked.manager;
            let should_wait = {
                match manager.try_lock() {
                    Ok(guard) => guard.loop_started(),
                    Err(_) => {
                        log::warn!(
                            "KernelCore::close_services: skipping locked ServerManager during shutdown"
                        );
                        closed_all = false;
                        continue;
                    }
                }
            };

            if should_wait {
                let deadline = std::time::Instant::now() + std::time::Duration::from_millis(500);
                while !manager
                    .try_lock()
                    .map(|guard| guard.is_stopped())
                    .unwrap_or(false)
                {
                    if std::time::Instant::now() >= deadline {
                        log::warn!(
                            "KernelCore::close_services: timed out waiting for ServerManager stop"
                        );
                        closed_all = false;
                        break;
                    }
                    std::thread::sleep(std::time::Duration::from_millis(1));
                }
            }

            {
                let join_guard = manager.try_lock();
                if let Ok(mut guard) = join_guard {
                    guard.join_host_threads();
                }
            }
        }
        closed_all
    }

    /// Run a service manager that already lives in its final shared Rust owner.
    pub fn run_server_shared(&self, manager: Arc<Mutex<ServerManager>>) {
        {
            let mut managers = self.server_managers.lock().unwrap();
            if self.is_shutting_down.load(Ordering::Relaxed) {
                return;
            }
            let (stop_requested, wakeup_event) = {
                let guard = manager.lock().unwrap();
                (guard.stop_requested_arc(), guard.wakeup_event_arc())
            };
            managers.push(TrackedServerManager {
                manager: Arc::clone(&manager),
                stop_requested,
                wakeup_event,
            });
        }

        crate::hle::service::server_manager::ServerManager::loop_process_shared(&manager);
    }

    pub(crate) fn track_server_manager_for_test(&self, server_manager: Arc<Mutex<ServerManager>>) {
        let (stop_requested, wakeup_event) = {
            let guard = server_manager.lock().unwrap();
            (guard.stop_requested_arc(), guard.wakeup_event_arc())
        };
        self.server_managers
            .lock()
            .unwrap()
            .push(TrackedServerManager {
                manager: server_manager,
                stop_requested,
                wakeup_event,
            });
    }

    pub(crate) fn ensure_tracked_server_manager_port_registrations(
        &self,
        process: Arc<ProcessLock>,
    ) {
        let managers = self.server_managers.lock().unwrap();
        for tracked in managers.iter() {
            tracked
                .manager
                .lock()
                .unwrap()
                .ensure_kernel_port_registrations_for_process(Arc::clone(&process));
        }
    }

    /// Register a process in the kernel-global process list.
    ///
    /// Upstream performs this from `KProcess::Register`. Keeping the owner in
    /// `KernelCore` lets services such as PM query the live process list instead
    /// of carrying per-service snapshots.
    pub fn register_process(&self, process: Arc<ProcessLock>) {
        let _guard = self.process_list_lock.lock().unwrap();
        let mut process_list = self.process_list.lock().unwrap();
        if !process_list
            .iter()
            .any(|registered| Arc::ptr_eq(registered, &process))
        {
            process_list.push(process);
        }
    }

    /// Return the live kernel process list.
    ///
    /// Upstream returns a `std::list<KScopedAutoObject<KProcess>>` copy. Cloning
    /// the `Arc` owners gives Rust callers the same snapshot semantics.
    pub fn get_process_list(&self) -> Vec<Arc<ProcessLock>> {
        let _guard = self.process_list_lock.lock().unwrap();
        self.process_list.lock().unwrap().clone()
    }

    /// Suspend or resume emulation threads for the current application process.
    ///
    /// Upstream: `KernelCore::SuspendEmulation(bool)`.
    /// This port currently tracks only the frontend-loaded application process.
    pub fn suspend_emulation(&self, suspended: bool) {
        let should_suspend = self.exception_exited || suspended;
        let Some(process) = self.system_ref.get().current_process_arc.as_ref().cloned() else {
            return;
        };

        let threads: Vec<Arc<KThreadLock>> = {
            let process_guard = process.lock().unwrap();
            process_guard.thread_objects.values().cloned().collect()
        };

        for thread in threads {
            let mut thread_guard = thread.lock().unwrap();
            if should_suspend {
                thread_guard.request_suspend(SuspendType::System);
            } else {
                thread_guard.resume(SuspendType::System);
            }
        }

        if should_suspend {
            self.interrupt_all_cores();
        }
    }

    /// Begin kernel-side shutdown for the current application process.
    ///
    /// Upstream: `KernelCore::ShutdownCores()`.
    /// The current Rust port uses process termination plus per-core interrupts
    /// to drive CpuManager guest fibers into their shutdown yield path.
    pub fn shutdown_cores(&self) {
        if let Some(process) = self.system_ref.get().current_process_arc.as_ref().cloned() {
            let _ = process.lock().unwrap().terminate();
            KWorkerTaskManager::wait_for_global_idle();
        }

        self.interrupt_all_cores();
    }

    /// Rust helper for owner lookups that upstream performs through the kernel
    /// process list. Returns the frontend application process or a guest
    /// service process with the matching process id.
    pub fn get_process_by_id(&self, process_id: u64) -> Option<Arc<ProcessLock>> {
        self.get_process_list()
            .into_iter()
            .find(|process| process.lock().unwrap().get_process_id() == process_id)
    }

    /// Rust counterpart to upstream `KernelCore::GetProcessList()` scans that
    /// identify a process by comparing `GetPageTable().GetBasePageTable()`.
    pub fn get_process_by_page_table_base(
        &self,
        table: *const super::k_page_table_base::KPageTableBase,
    ) -> Option<Arc<ProcessLock>> {
        if table.is_null() {
            return None;
        }

        if let Some(process) = (!self.system_ref.is_null())
            .then(|| self.system_ref.get().current_process_arc.as_ref().cloned())
            .flatten()
        {
            let process_guard = process.lock().unwrap();
            let process_table = process_guard.page_table.get_base()
                as *const super::k_page_table_base::KPageTableBase;
            if std::ptr::eq(process_table, table) {
                drop(process_guard);
                return Some(process);
            }
        }

        self.service_processes
            .lock()
            .unwrap()
            .iter()
            .find(|process| {
                let process_guard = process.lock().unwrap();
                let process_table = process_guard.page_table.get_base()
                    as *const super::k_page_table_base::KPageTableBase;
                std::ptr::eq(process_table, table)
            })
            .cloned()
            .or_else(|| {
                self.host_service_processes
                    .lock()
                    .unwrap()
                    .iter()
                    .find(|process| {
                        let process_guard = process.lock().unwrap();
                        let process_table = process_guard.page_table.get_base()
                            as *const super::k_page_table_base::KPageTableBase;
                        std::ptr::eq(process_table, table)
                    })
                    .cloned()
            })
    }

    /// Rust helper for event owner lookup via the kernel process list.
    pub fn get_event_owner_process_id(&self, event_object_id: u64) -> Option<u64> {
        if let Some(process) = self.system_ref.get().current_process_arc.as_ref().cloned() {
            let process_guard = process.lock().unwrap();
            if process_guard
                .get_event_by_object_id(event_object_id)
                .is_some()
            {
                return Some(process_guard.get_process_id());
            }
        }

        self.service_processes
            .lock()
            .unwrap()
            .iter()
            .find_map(|process| {
                let process_guard = process.lock().unwrap();
                process_guard
                    .get_event_by_object_id(event_object_id)
                    .map(|_| process_guard.get_process_id())
            })
            .or_else(|| {
                self.host_service_processes
                    .lock()
                    .unwrap()
                    .iter()
                    .find_map(|process| {
                        let process_guard = process.lock().unwrap();
                        process_guard
                            .get_event_by_object_id(event_object_id)
                            .map(|_| process_guard.get_process_id())
                    })
            })
    }

    /// Rust helper for server-session owner lookup via the kernel process list.
    pub fn get_session_owner_process_id(&self, session_object_id: u64) -> Option<u64> {
        if let Some(process) = self.system_ref.get().current_process_arc.as_ref().cloned() {
            let process_guard = process.lock().unwrap();
            if process_guard
                .get_server_session_by_object_id(session_object_id)
                .is_some()
            {
                return Some(process_guard.get_process_id());
            }
        }

        self.service_processes
            .lock()
            .unwrap()
            .iter()
            .find_map(|process| {
                let process_guard = process.lock().unwrap();
                process_guard
                    .get_server_session_by_object_id(session_object_id)
                    .map(|_| process_guard.get_process_id())
            })
            .or_else(|| {
                self.host_service_processes
                    .lock()
                    .unwrap()
                    .iter()
                    .find_map(|process| {
                        let process_guard = process.lock().unwrap();
                        process_guard
                            .get_server_session_by_object_id(session_object_id)
                            .map(|_| process_guard.get_process_id())
                    })
            })
    }

    /// Rust helper for named client-port lookup via the kernel process list.
    ///
    /// This is the Rust counterpart to upstream `KObjectName::Find<KClientPort>(kernel, name)`.
    /// `KObjectName` already stores the named client-port object id; this helper
    /// resolves that object id back to the owning `KPort` by scanning the kernel
    /// process registries that currently own client-port objects.
    pub fn get_client_port_by_object_id(
        &self,
        client_port_object_id: u64,
    ) -> Option<Arc<Mutex<KPort>>> {
        if let Some(process) = self.system_ref.get().current_process_arc.as_ref().cloned() {
            let process_guard = process.lock().unwrap();
            if let Some(port) = process_guard.get_client_port_by_object_id(client_port_object_id) {
                return Some(port);
            }
        }

        if let Some(port) = self
            .process_list
            .lock()
            .unwrap()
            .iter()
            .find_map(|process| {
                process
                    .lock()
                    .unwrap()
                    .get_client_port_by_object_id(client_port_object_id)
            })
        {
            return Some(port);
        }

        self.service_processes
            .lock()
            .unwrap()
            .iter()
            .find_map(|process| {
                process
                    .lock()
                    .unwrap()
                    .get_client_port_by_object_id(client_port_object_id)
            })
            .or_else(|| {
                self.host_service_processes
                    .lock()
                    .unwrap()
                    .iter()
                    .find_map(|process| {
                        process
                            .lock()
                            .unwrap()
                            .get_client_port_by_object_id(client_port_object_id)
                    })
            })
    }

    /// Get the global scheduler context (Arc reference).
    pub fn global_scheduler_context(&self) -> Option<&Arc<Mutex<GlobalSchedulerContext>>> {
        self.global_scheduler_context.as_ref()
    }

    /// Get a physical core by index.
    /// Upstream: `KernelCore::PhysicalCore(id)`.
    pub fn physical_core(&self, id: usize) -> Option<&PhysicalCore> {
        self.cores.get(id).map(Arc::as_ref)
    }

    /// Get a physical core mutably by index.
    pub fn physical_core_mut(&mut self, id: usize) -> Option<&mut PhysicalCore> {
        self.cores.get_mut(id).and_then(Arc::get_mut)
    }

    /// Get a per-core scheduler by index.
    /// Upstream: `KernelCore::Scheduler(id)` (kernel.cpp:924).
    pub fn scheduler(&self, id: usize) -> Option<&Arc<Mutex<KScheduler>>> {
        self.schedulers.get(id)
    }

    fn interrupt_all_cores(&self) {
        for core_id in 0..self.cores.len() {
            if let Some(core) = self.physical_core(core_id) {
                core.interrupt();
            }
        }
    }

    /// Get the scheduler for the calling host thread's core.
    /// Upstream: `KernelCore::CurrentScheduler()` (kernel.cpp:956-963).
    /// Returns None if called from a non-core thread.
    pub fn current_scheduler(&self) -> Option<&Arc<Mutex<KScheduler>>> {
        let core_id = self.get_current_host_thread_id();
        if core_id >= hardware_properties::NUM_CPU_CORES {
            return None;
        }
        self.schedulers.get(core_id as usize)
    }

    /// Temporary workaround for the current fiber-return-to-originating-thread
    /// behavior: returns true only for the `NUM_CPU_CORES` dedicated guest
    /// core OS threads and false for HLE host service threads, the main
    /// thread, etc.
    ///
    /// TODO: remove this once host service waits no longer need the
    /// guest-core/host-thread split workaround in `ServerManager`.
    pub fn is_current_thread_guest_core(&self) -> bool {
        self.get_current_host_thread_id() < hardware_properties::NUM_CPU_CORES
    }

    /// Get the physical core index for the calling host thread.
    /// Upstream: `KernelCore::CurrentPhysicalCoreIndex()` (kernel.cpp:940-946).
    pub fn current_physical_core_index(&self) -> usize {
        let core_id = self.get_current_host_thread_id();
        if core_id >= hardware_properties::NUM_CPU_CORES {
            return (hardware_properties::NUM_CPU_CORES - 1) as usize;
        }
        core_id as usize
    }

    /// Get the physical core for the calling host thread.
    /// Upstream: `KernelCore::CurrentPhysicalCore()` (kernel.cpp:948).
    pub fn current_physical_core(&self) -> &PhysicalCore {
        self.cores[self.current_physical_core_index()].as_ref()
    }

    /// Get the physical core for the calling host thread (mutable).
    pub fn current_physical_core_mut(&mut self) -> &mut PhysicalCore {
        let idx = self.current_physical_core_index();
        Arc::get_mut(&mut self.cores[idx])
            .expect("current_physical_core_mut requires exclusive PhysicalCore ownership")
    }

    /// Register a CPU core thread by setting the thread-local host thread ID.
    /// Upstream: `KernelCore::RegisterCoreThread(core_id)` (kernel.cpp:1032).
    /// Must be called from the host thread that will run this core.
    pub fn register_core_thread(&self, core_id: usize) {
        assert!(core_id < hardware_properties::NUM_CPU_CORES as usize);
        let this_id = core_id as u32;
        HOST_THREAD_ID.with(|id| {
            assert_eq!(id.get(), u32::MAX, "host thread already registered");
            id.set(this_id);
        });
        if !self.is_multicore {
            self.single_core_thread_id.store(this_id, Ordering::Relaxed);
        }
    }

    /// Register a host thread (non-core) by allocating the next host thread ID.
    /// Upstream: `KernelCore::RegisterHostThread(existing_thread)` (kernel.cpp:1036).
    pub fn register_host_thread_with_existing(&self, existing_thread: Option<&Arc<KThreadLock>>) {
        HOST_THREAD_ID.with(|id| {
            if id.get() == u32::MAX {
                let new_id = self.next_host_thread_id.fetch_add(1, Ordering::Relaxed);
                id.set(new_id);
            }
        });

        if let Some(thread) = existing_thread {
            set_current_emu_thread(Some(thread));
        } else {
            let dummy = get_or_create_host_dummy_thread(self);
            set_current_emu_thread(Some(&dummy));
        }
    }

    /// Register a host thread (non-core) by allocating the next host thread ID.
    /// Upstream: `KernelCore::RegisterHostThread(existing_thread)` (kernel.cpp:1036).
    pub fn register_host_thread(&self) {
        self.register_host_thread_with_existing(None);
    }

    /// Get the host thread ID for the calling thread.
    /// Upstream: `Impl::GetCurrentHostThreadID()` (kernel.cpp:403-409).
    /// In single-core mode, if the calling thread is the single core thread,
    /// returns the current core index from CpuManager instead of the raw ID.
    fn get_current_host_thread_id(&self) -> u32 {
        HOST_THREAD_ID.with(|id| {
            let this_id = id.get();
            if !self.is_multicore && this_id == self.single_core_thread_id.load(Ordering::Relaxed) {
                // In single-core mode, the single core thread ID maps to the
                // current core index. Upstream reads system.GetCpuManager().CurrentCore().
                // We return 0 as default; CpuManager.current_core rotates this.
                // Upstream: system.GetCpuManager().CurrentCore(). Defaults to 0 until wired.
                return 0;
            }
            this_id
        })
    }

    /// Get the hardware timer (Arc reference).
    pub fn hardware_timer(&self) -> Option<&Arc<KHardwareTimer>> {
        self.hardware_timer.as_ref()
    }

    /// Get the object list container.
    pub fn object_list_container(&self) -> Option<&KAutoObjectWithListContainer> {
        self.global_object_list_container.as_ref()
    }

    /// Get the object name global data.
    pub fn object_name_global_data(&self) -> Option<&KObjectNameGlobalData> {
        self.object_name_global_data.as_ref()
    }

    pub fn ensure_object_name_global_data_for_test(&mut self) {
        if self.object_name_global_data.is_none() {
            self.object_name_global_data = Some(KObjectNameGlobalData::new());
        }
    }

    /// Get the current emulation thread for the calling host thread.
    /// Matches upstream `KernelCore::GetCurrentEmuThread()`.
    /// Delegates to the thread-local `get_current_emu_thread()` free function.
    pub fn get_current_emu_thread(&self) -> Option<Arc<KThreadLock>> {
        get_current_emu_thread()
    }

    /// Set the current emulation thread for the calling host thread.
    /// Matches upstream `KernelCore::SetCurrentEmuThread(KThread*)`.
    /// Delegates to the thread-local `set_current_emu_thread()` free function.
    pub fn set_current_emu_thread(&self, thread: Option<&Arc<KThreadLock>>) {
        set_current_emu_thread(thread);
    }

    /// Register a kernel object for leak tracking.
    pub fn register_kernel_object(&self, object_id: u64) {
        self.registered_objects.lock().unwrap().push(object_id);
    }

    /// Unregister a kernel object from leak tracking.
    pub fn unregister_kernel_object(&self, object_id: u64) {
        self.registered_objects
            .lock()
            .unwrap()
            .retain(|&id| id != object_id);
    }

    /// Register a kernel object as in-use.
    pub fn register_in_use_object(&self, object_id: u64) {
        self.registered_in_use_objects
            .lock()
            .unwrap()
            .push(object_id);
    }

    /// Unregister an in-use kernel object.
    pub fn unregister_in_use_object(&self, object_id: u64) {
        self.registered_in_use_objects
            .lock()
            .unwrap()
            .retain(|&id| id != object_id);
    }

    /// Whether the kernel is in multicore mode.
    pub fn is_multicore(&self) -> bool {
        self.is_multicore
    }

    /// Whether the kernel is shutting down.
    pub fn is_shutting_down(&self) -> bool {
        self.is_shutting_down.load(Ordering::Relaxed)
    }

    /// Workaround for single-core mode phantom mode.
    pub fn is_phantom_mode_for_single_core(&self) -> bool {
        self.is_phantom_mode_for_singlecore.load(Ordering::Relaxed)
    }

    /// Set the phantom mode for single core.
    pub fn set_is_phantom_mode_for_single_core(&self, value: bool) {
        self.is_phantom_mode_for_singlecore
            .store(value, Ordering::Relaxed);
    }

    /// Get the slab resource counts.
    pub fn slab_resource_counts(&self) -> &KSlabResourceCounts {
        &self.slab_resource_counts
    }

    /// Get the slab resource counts (mutable).
    pub fn slab_resource_counts_mut(&mut self) -> &mut KSlabResourceCounts {
        &mut self.slab_resource_counts
    }

    // -- Private methods --

    /// Create a new object ID.
    pub fn create_new_object_id(&self) -> u32 {
        self.next_object_id.fetch_add(1, Ordering::Relaxed)
    }

    /// Create a new kernel process ID.
    pub fn create_new_kernel_process_id(&self) -> u64 {
        self.next_kernel_process_id.fetch_add(1, Ordering::Relaxed)
    }

    /// Create a new user process ID.
    pub fn create_new_user_process_id(&self) -> u64 {
        self.next_user_process_id.fetch_add(1, Ordering::Relaxed)
    }

    /// Create a new thread ID.
    #[track_caller]
    pub fn create_new_thread_id(&self) -> u64 {
        let id = self.next_thread_id.fetch_add(1, Ordering::Relaxed);
        // RUZU_TRACE_THREAD_ID=1 — log every thread id allocation with caller
        // location. Used to find why ruzu's MK8D main thread gets tid=73 vs
        // zuyu's tid=75 (delta +2). See project_mk8d_ruzu_det_wedge_vs_zuyu_2026_05_17.
        if std::env::var_os("RUZU_TRACE_THREAD_ID").is_some() {
            let caller = std::panic::Location::caller();
            let bt = std::backtrace::Backtrace::force_capture();
            log::info!(
                "[THREAD_ID] alloc tid={} caller={}:{}\nBacktrace:\n{}",
                id,
                caller.file(),
                caller.line(),
                bt
            );
        }
        id
    }

    /// Get the memory manager.
    /// Upstream: `KernelCore::MemoryManager()`.
    pub fn memory_manager(&self) -> &KMemoryManager {
        &self.memory_manager
    }

    /// Get the memory manager (mutable).
    pub fn memory_manager_mut(&mut self) -> &mut KMemoryManager {
        &mut self.memory_manager
    }

    /// Get the kernel-wide resource limit. Upstream:
    /// `KernelCore::GetSystemResourceLimit()`.
    pub fn get_system_resource_limit(
        &self,
    ) -> Option<Arc<Mutex<super::k_resource_limit::KResourceLimit>>> {
        self.system_resource_limit.clone()
    }

    /// Get the kernel-wide KMemoryBlock slab manager. Upstream stores this
    /// alongside the application/system memory block managers; ruzu uses
    /// one shared slab.
    pub fn get_memory_block_slab_manager(
        &self,
    ) -> Option<Arc<super::k_dynamic_resource_manager::KMemoryBlockSlabManager>> {
        self.memory_block_slab_manager.clone()
    }

    /// Initialize the kernel-wide KMemoryBlock slab. Upstream creates
    /// separate application / system slabs sized from
    /// `KernelApplicationMemoryBlockSlabHeapSize` and
    /// `KernelSystemMemoryBlockSlabHeapSize` in kernel.cpp:1070-71. ruzu
    /// uses a single slab; capacity covers worst-case concurrent block
    /// updates (4096 entries is far above the two-per-update upper bound
    /// for any practical guest workload).
    pub fn initialize_memory_block_slab_manager(&mut self, capacity: usize) {
        let mut slab = super::k_dynamic_resource_manager::KMemoryBlockSlabManager::new();
        slab.initialize(capacity);
        self.memory_block_slab_manager = Some(Arc::new(slab));
    }

    /// Get the kernel-wide page-table-page allocator. Upstream:
    /// `KernelCore::PageTableManager()` accessor.
    pub fn get_page_table_manager(
        &self,
    ) -> Option<Arc<super::k_page_table_manager::KPageTableManager>> {
        self.page_table_manager.clone()
    }

    /// Get the kernel-wide physical memory layout. Upstream:
    /// `KernelCore::MemoryLayout()`. Populated by
    /// `initialize_memory_layout` at boot.
    pub fn get_memory_layout(&self) -> Option<Arc<Mutex<super::k_memory_layout::KMemoryLayout>>> {
        self.memory_layout.clone()
    }

    /// Populate the kernel-wide physical memory layout with the three
    /// Switch DRAM user pools (Application / Applet / SystemNonSecure).
    /// Mirrors the upstream init pass that walks the SoC region tree.
    pub fn initialize_memory_layout(
        &mut self,
        application: (u64, usize),
        applet: (u64, usize),
        system: (u64, usize),
    ) {
        let mut layout = super::k_memory_layout::KMemoryLayout::new();
        layout.populate_default_dram_user_pools(
            application.0,
            application.1,
            applet.0,
            applet.1,
            system.0,
            system.1,
        );
        self.memory_layout = Some(Arc::new(Mutex::new(layout)));
    }

    /// Initialize the kernel-wide page-table-page slab. Upstream sizes
    /// this from `KernelPageTableHeapSize` (kernel.cpp:1067) — typically
    /// several thousand page-sized entries. ruzu doesn't currently spend
    /// any of this allocation pressure (flat page table) so the capacity
    /// is set conservatively.
    ///
    /// Wires a dedicated `KDynamicPageManager` as the page allocator
    /// behind the slab so `is_in_range` queries return real (non-empty)
    /// ranges and refcount accounting is anchored to a stable address
    /// region. Mirrors upstream's
    /// `Initialize(KDynamicPageManager*, capacity, rc_table)`.
    pub fn initialize_page_table_manager(&mut self, capacity: usize) {
        use super::k_dynamic_page_manager::KDynamicPageManager;
        use super::k_memory_block::PAGE_SIZE;

        // Reserve a dedicated page-manager region for the slab. Pick a
        // base address well above any guest VA so `is_in_range` queries
        // never collide with mapped guest memory.
        let region_size = capacity * PAGE_SIZE;
        let pa = Arc::new(Mutex::new(KDynamicPageManager::new()));
        pa.lock()
            .unwrap()
            .initialize(0xFFFF_E000_0000_0000, region_size, PAGE_SIZE)
            .expect("KPageTableSlabHeap page allocator init");

        let slab = super::k_page_table_slab_heap::KPageTableSlabHeap::new();
        slab.initialize(pa, capacity);
        let slab = Arc::new(slab);
        self.page_table_manager = Some(Arc::new(
            super::k_page_table_manager::KPageTableManager::new(slab),
        ));
    }

    /// Initialize the kernel-wide resource limit. Upstream:
    /// `KernelCore::Impl::InitializeSystemResourceLimit` (kernel.cpp:214):
    ///
    /// ```cpp
    /// system_resource_limit = KResourceLimit::Create(...);
    /// system_resource_limit->Initialize();
    /// SetLimitValue(PhysicalMemoryMax, total_size);
    /// SetLimitValue(ThreadCountMax, 800);
    /// SetLimitValue(EventCountMax, 900);
    /// SetLimitValue(TransferMemoryCountMax, 200);
    /// SetLimitValue(SessionCountMax, 1133);
    /// Reserve(PhysicalMemoryMax, kernel_size);
    /// Reserve(PhysicalMemoryMax, secure_applet_memory_size /* 4 MiB */);
    /// ```
    pub fn initialize_system_resource_limit(&mut self, total_size: i64, kernel_size: i64) {
        use super::k_resource_limit::{KResourceLimit, LimitableResource};
        let mut rl = KResourceLimit::new();
        let _ = rl.set_limit_value(LimitableResource::PhysicalMemoryMax, total_size);
        let _ = rl.set_limit_value(LimitableResource::ThreadCountMax, 800);
        let _ = rl.set_limit_value(LimitableResource::EventCountMax, 900);
        let _ = rl.set_limit_value(LimitableResource::TransferMemoryCountMax, 200);
        let _ = rl.set_limit_value(LimitableResource::SessionCountMax, 1133);
        let _ = rl.reserve(LimitableResource::PhysicalMemoryMax, kernel_size);
        // Reserve secure applet memory introduced in firmware 5.0.0.
        let secure_applet_memory_size: i64 = 4 * 1024 * 1024;
        let _ = rl.reserve(
            LimitableResource::PhysicalMemoryMax,
            secure_applet_memory_size,
        );
        self.system_resource_limit = Some(Arc::new(Mutex::new(rl)));
    }

    /// Initialize shutdown threads (one per core).
    ///
    /// Upstream: `Impl::InitializeShutdownThreads()` (kernel.cpp:340-348).
    /// Creates 4 high-priority kernel threads used for graceful shutdown.
    /// Must be called before `initialize_physical_cores()` so that thread IDs
    /// match upstream (shutdown threads get IDs 1-4, physical core threads 5-12).
    fn initialize_shutdown_threads(&mut self) {
        self.shutdown_threads.clear();

        let kernel_ptr = self as *const KernelCore as usize;

        for core_id in 0..hardware_properties::NUM_CPU_CORES {
            let thread = Arc::new(KThreadLock::new(KThread::new()));
            {
                let thread_id = self.create_new_thread_id();
                let object_id = self.create_new_object_id() as u64;
                let mut t = thread.lock().unwrap();
                t.set_current_core(core_id as i32);

                // Upstream: InitializeHighPriorityThread passes GetShutdownThreadStartFunc().
                let kp = kernel_ptr;
                let shutdown_func: Box<dyn FnOnce() + Send> = Box::new(move || {
                    let kernel = unsafe { &*(kp as *const KernelCore) };
                    crate::cpu_manager::CpuManager::shutdown_thread_function(kernel);
                });

                t.initialize_high_priority_thread(
                    core_id as i32,
                    thread_id,
                    object_id,
                    Some(shutdown_func),
                );
                t.bind_self_reference(&thread);
            }
            self.register_kernel_object(thread.lock().unwrap().get_object_id());
            self.shutdown_threads.push(thread);
        }
    }

    /// Initialize physical cores and per-core schedulers.
    ///
    /// Upstream: `Impl::InitializePhysicalCores()` (kernel.cpp:192-211).
    /// For each core, creates a KScheduler and PhysicalCore, then creates
    /// main and idle threads (ownerless, `ThreadType::Main`) and initializes
    /// the scheduler with them so that `get_scheduler_current_thread()` returns
    /// the main thread with a valid host fiber context.
    fn initialize_physical_cores(&mut self) {
        self.schedulers.clear();
        self.cores.clear();
        self.main_threads.clear();
        self.idle_threads.clear();

        // Capture a raw pointer to self for use in fiber closures.
        // Safety: KernelCore outlives all fibers — fibers are destroyed during
        // kernel shutdown which happens before KernelCore is dropped.
        let kernel_ptr = self as *const KernelCore as usize;

        for i in 0..hardware_properties::NUM_CPU_CORES as usize {
            let core_id = i as i32;

            // Create scheduler and physical core.
            let scheduler = Arc::new(Mutex::new(KScheduler::new(core_id)));
            // Wire the global scheduler context so the scheduler can find threads.
            if let Some(ref gsc) = self.global_scheduler_context {
                scheduler.lock().unwrap().global_scheduler_context = Some(gsc.clone());
            }
            self.schedulers.push(scheduler.clone());
            self.cores
                .push(Arc::new(PhysicalCore::new(i, self.is_multicore)));

            // Create main thread.
            // Upstream: auto* main_thread = KThread::Create(kernel);
            //           main_thread->SetCurrentCore(core);
            //           KThread::InitializeMainThread(system, main_thread, core);
            //           KThread::Register(kernel, main_thread);
            //
            // Upstream passes system.GetCpuManager().GetGuestActivateFunc() as the
            // fiber entry point. GuestActivate calls scheduler->Activate().
            let main_thread = Arc::new(KThreadLock::new(KThread::new()));
            {
                let thread_id = self.create_new_thread_id();
                let object_id = self.create_new_object_id() as u64;
                let mut t = main_thread.lock().unwrap();
                t.set_current_core(core_id);

                // Upstream: InitializeMainThread passes GetGuestActivateFunc().
                // GuestActivate() calls kernel.CurrentScheduler()->Activate().
                let kp = kernel_ptr;
                let guest_activate_func: Box<dyn FnOnce() + Send> = Box::new(move || {
                    // Safety: kernel_ptr is valid for the lifetime of this fiber.
                    let kernel = unsafe { &*(kp as *const KernelCore) };
                    crate::cpu_manager::CpuManager::guest_activate(kernel);
                });

                t.initialize_kernel_main_thread(
                    core_id,
                    thread_id,
                    object_id,
                    Some(guest_activate_func),
                );
                t.bind_self_reference(&main_thread);
            }
            // Upstream: KThread::Register(kernel, main_thread) → adds to object list container.
            self.register_kernel_object(main_thread.lock().unwrap().get_object_id());

            // Create idle thread.
            // Upstream: auto* idle_thread = KThread::Create(kernel);
            //           idle_thread->SetCurrentCore(core);
            //           KThread::InitializeIdleThread(system, idle_thread, core);
            //           KThread::Register(kernel, idle_thread);
            //
            // Upstream passes system.GetCpuManager().GetIdleThreadStartFunc() as the
            // fiber entry point. IdleThreadFunction dispatches to MultiCoreRunIdleThread
            // or SingleCoreRunIdleThread.
            let idle_thread = Arc::new(KThreadLock::new(KThread::new()));
            {
                let thread_id = self.create_new_thread_id();
                let object_id = self.create_new_object_id() as u64;
                let mut t = idle_thread.lock().unwrap();
                t.set_current_core(core_id);

                // Upstream: InitializeIdleThread passes GetIdleThreadStartFunc().
                // IdleThreadFunction calls MultiCoreRunIdleThread or SingleCoreRunIdleThread.
                let kp = kernel_ptr;
                let is_mc = self.is_multicore;
                let idle_func: Box<dyn FnOnce() + Send> = Box::new(move || {
                    // Safety: kernel_ptr is valid for the lifetime of this fiber.
                    let kernel = unsafe { &*(kp as *const KernelCore) };
                    if is_mc {
                        crate::cpu_manager::CpuManager::multi_core_run_idle_thread_entry(kernel);
                    } else {
                        // Single-core idle requires CoreTiming and additional state
                        // that are not available from the fiber context yet.
                        // For now, run the multicore idle path which is functionally
                        // equivalent (idle + handle interrupt loop).
                        crate::cpu_manager::CpuManager::multi_core_run_idle_thread_entry(kernel);
                    }
                });

                t.initialize_kernel_idle_thread(core_id, thread_id, object_id, Some(idle_func));
                t.bind_self_reference(&idle_thread);
            }
            self.register_kernel_object(idle_thread.lock().unwrap().get_object_id());

            // Initialize the scheduler with the main and idle threads.
            // Upstream: schedulers[i]->Initialize(main_thread, idle_thread, core);
            // This sets m_current_thread = main_thread so get_scheduler_current_thread()
            // returns a valid thread.
            scheduler
                .lock()
                .unwrap()
                .initialize_with_threads(&main_thread, &idle_thread, core_id);

            self.main_threads.push(main_thread);
            self.idle_threads.push(idle_thread);
        }

        // Rust adaptation: schedulers do not hold the upstream `KernelCore&`
        // owner, so wire the created PhysicalCore set into every scheduler for
        // save/load/interrupt paths that would otherwise go through `m_kernel`.
        for scheduler in &self.schedulers {
            scheduler.lock().unwrap().physical_cores = self.cores.clone();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::SystemRef;

    #[test]
    fn close_services_requests_stop_on_tracked_server_managers() {
        let kernel = KernelCore::new();
        let manager = ServerManager::new_shared(SystemRef::null());

        kernel.track_server_manager_for_test(Arc::clone(&manager));
        kernel.close_services();

        assert!(manager.lock().unwrap().stop_requested_for_test());
    }

    #[test]
    fn process_list_registration_is_idempotent_and_queryable() {
        let kernel = KernelCore::new();
        let process = Arc::new(ProcessLock::from_value(KProcess::new()));
        process.lock().unwrap().process_id = 0x1234;

        kernel.register_process(Arc::clone(&process));
        kernel.register_process(Arc::clone(&process));

        assert_eq!(kernel.get_process_list().len(), 1);
        assert!(kernel
            .get_process_by_id(0x1234)
            .is_some_and(|found| Arc::ptr_eq(&found, &process)));
    }

    #[test]
    fn run_on_guest_core_process_retains_service_process_owner() {
        let mut kernel = KernelCore::new();
        kernel.initialize();

        kernel.run_on_guest_core_process("svc-test", Box::new(|| {}));

        assert_eq!(kernel.service_processes.lock().unwrap().len(), 1);

        let service_process = kernel.service_processes.lock().unwrap()[0].clone();
        let thread = {
            let process = service_process.lock().unwrap();
            process.thread_objects.values().next().cloned()
        }
        .expect("service process should keep its thread object");

        assert!(thread
            .lock()
            .unwrap()
            .parent
            .as_ref()
            .and_then(Weak::upgrade)
            .is_some());
    }

    #[test]
    fn register_host_thread_sets_dummy_current_thread() {
        let mut kernel = KernelCore::new();
        kernel.initialize();

        set_current_emu_thread(None);
        kernel.register_host_thread();

        let current = kernel
            .get_current_emu_thread()
            .expect("host thread should have a current dummy thread");
        assert!(current.lock().unwrap().is_dummy_thread());
    }

    #[test]
    fn register_host_thread_with_existing_keeps_existing_thread_as_current() {
        let mut kernel = KernelCore::new();
        kernel.initialize();

        let thread = Arc::new(KThreadLock::new(KThread::new()));
        {
            let thread_id = kernel.create_new_thread_id();
            let object_id = kernel.create_new_object_id() as u64;
            let mut guard = thread.lock().unwrap();
            let rc = guard.initialize_dummy_thread(None, thread_id, object_id);
            assert_eq!(rc, crate::hle::result::RESULT_SUCCESS.get_inner_value());
            guard.bind_self_reference(&thread);
        }

        set_current_emu_thread(None);
        kernel.register_host_thread_with_existing(Some(&thread));

        let current = kernel
            .get_current_emu_thread()
            .expect("existing thread should remain current");
        assert!(Arc::ptr_eq(&current, &thread));
    }

    #[test]
    fn initialize_physical_cores_wires_physical_cores_into_each_scheduler() {
        let mut kernel = KernelCore::new();
        kernel.initialize();

        for scheduler in &kernel.schedulers {
            let scheduler = scheduler.lock().unwrap();
            assert_eq!(scheduler.physical_cores.len(), kernel.cores.len());
        }
    }
}

impl Default for KernelCore {
    fn default() -> Self {
        Self::new()
    }
}
