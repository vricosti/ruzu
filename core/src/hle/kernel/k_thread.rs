//! Port of zuyu/src/core/hle/kernel/k_thread.h / k_thread.cpp
//! Status: Partial (structural port, complex methods stubbed)
//! Derniere synchro: 2026-03-11
//!
//! KThread: The kernel thread object. Preserves all thread states, enums,
//! StackParameters, QueueEntry, NativeExecutionParameters, and all field
//! ownership matching upstream.

use bitflags::bitflags;
use std::sync::atomic::{AtomicBool, AtomicI64, AtomicU16, AtomicU8, Ordering};
use std::sync::{Arc, Condvar, Mutex, Weak};
use std::time::{Duration, Instant};

use super::k_process::KProcess;
use super::k_scheduler::KScheduler;
use super::k_synchronization_object;
use super::k_thread_queue::KThreadQueue;
use super::k_worker_task_manager::{KWorkerTaskManager, WorkerType};
use super::k_synchronization_object::SynchronizationWaitSet;
use super::k_synchronization_object::SynchronizationObjectState;
use super::k_typed_address::{KProcessAddress, KVirtualAddress};
use crate::arm::arm_interface::ThreadContext as ArmThreadContext;
use crate::hardware_properties::NUM_CPU_CORES;
use crate::hle::kernel::svc::svc_results::{
    RESULT_CANCELLED, RESULT_INVALID_STATE, RESULT_NO_SYNCHRONIZATION_OBJECT,
    RESULT_OUT_OF_RESOURCE, RESULT_TERMINATION_REQUESTED, RESULT_TIMED_OUT,
};
use crate::hle::result::RESULT_SUCCESS;
// RBEntry kept for structural parity with upstream m_condvar_arbiter_tree_node.
// Currently unused: we use BTreeSet externally instead of an intrusive tree.
use common::tree::RBEntry;

// ---------------------------------------------------------------------------
// Enums matching upstream k_thread.h
// ---------------------------------------------------------------------------

/// Thread type.
/// Matches upstream `ThreadType` enum (k_thread.h).
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ThreadType {
    Main = 0,
    Kernel = 1,
    HighPriority = 2,
    User = 3,
    /// Special thread type for emulation purposes only.
    Dummy = 100,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ConditionVariableTreeState {
    #[default]
    None,
    ConditionVariable,
    AddressArbiter,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ConditionVariableThreadKey {
    pub cv_key: u64,
    pub priority: i32,
    pub thread_id: u64,
}

/// Suspend type.
/// Matches upstream `SuspendType` enum (k_thread.h).
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SuspendType {
    Process = 0,
    Thread = 1,
    Debug = 2,
    Backtrace = 3,
    Init = 4,
    System = 5,
    // Count = 6, // not represented as a variant
}

impl SuspendType {
    pub const COUNT: u32 = 6;
}

bitflags! {
    /// Thread state flags.
    /// Matches upstream `ThreadState` enum (k_thread.h).
    /// Uses bitflags to support the combined suspend flags pattern.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct ThreadState: u16 {
        const INITIALIZED = 0;
        const WAITING = 1;
        const RUNNABLE = 2;
        const TERMINATED = 3;

        /// Mask for the base state (lower 4 bits).
        const MASK = (1 << 4) - 1;

        const PROCESS_SUSPENDED  = 1 << (0 + 4);
        const THREAD_SUSPENDED   = 1 << (1 + 4);
        const DEBUG_SUSPENDED    = 1 << (2 + 4);
        const BACKTRACE_SUSPENDED = 1 << (3 + 4);
        const INIT_SUSPENDED     = 1 << (4 + 4);
        const SYSTEM_SUSPENDED   = 1 << (5 + 4);

        const SUSPEND_FLAG_MASK = ((1 << 6) - 1) << 4;
    }
}

impl ThreadState {
    pub const SUSPEND_SHIFT: u16 = 4;
}

bitflags! {
    /// DPC (Deferred Procedure Call) flags.
    /// Matches upstream `DpcFlag` enum (k_thread.h).
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct DpcFlag: u32 {
        const TERMINATING = 1 << 0;
        const TERMINATED  = 1 << 1;
    }
}

/// Reason a thread is waiting, for debugging purposes.
/// Matches upstream `ThreadWaitReasonForDebugging` enum (k_thread.h).
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ThreadWaitReasonForDebugging {
    None = 0,
    Sleep = 1,
    Ipc = 2,
    Synchronization = 3,
    ConditionVar = 4,
    Arbitration = 5,
    Suspended = 6,
}

impl Default for ThreadWaitReasonForDebugging {
    fn default() -> Self {
        Self::None
    }
}

/// Step state for debugging single-step.
/// Matches upstream `StepState` enum (k_thread.h).
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StepState {
    NotStepping = 0,
    StepPending = 1,
    StepPerformed = 2,
}

impl Default for StepState {
    fn default() -> Self {
        Self::NotStepping
    }
}

// ---------------------------------------------------------------------------
// Constants matching upstream KThread statics
// ---------------------------------------------------------------------------

/// Lowest thread priority from Svc.
pub const SVC_LOWEST_THREAD_PRIORITY: i32 = 63;
/// Highest thread priority from Svc.
pub const SVC_HIGHEST_THREAD_PRIORITY: i32 = 0;

/// Default thread priority.
pub const DEFAULT_THREAD_PRIORITY: i32 = 44;
/// Idle thread priority.
pub const IDLE_THREAD_PRIORITY: i32 = SVC_LOWEST_THREAD_PRIORITY + 1;
/// Dummy thread priority.
pub const DUMMY_THREAD_PRIORITY: i32 = SVC_LOWEST_THREAD_PRIORITY + 2;

/// Maximum count for priority inheritance.
pub const PRIORITY_INHERITANCE_COUNT_MAX: usize = 10;

// ---------------------------------------------------------------------------
// StackParameters — matches upstream KThread::StackParameters
// ---------------------------------------------------------------------------

/// Stack parameters stored per-thread.
/// Matches upstream `KThread::StackParameters` (k_thread.h).
pub struct StackParameters {
    pub svc_permission: [u8; 0x10],
    pub dpc_flags: AtomicU8,
    pub current_svc_id: u8,
    pub is_calling_svc: bool,
    pub is_in_exception_handler: bool,
    pub is_pinned: bool,
    pub disable_count: i32,
    // In C++ this is `KThread* cur_thread;`
    // We skip storing a self-pointer here; it serves no purpose in Rust.
}

impl Default for StackParameters {
    fn default() -> Self {
        Self {
            svc_permission: [0u8; 0x10],
            dpc_flags: AtomicU8::new(0),
            current_svc_id: 0,
            is_calling_svc: false,
            is_in_exception_handler: false,
            is_pinned: false,
            disable_count: 0,
        }
    }
}

// ---------------------------------------------------------------------------
// QueueEntry — re-exported from k_priority_queue for KThread field.
// Matches upstream `KThread::QueueEntry` (k_thread.h).
pub use super::k_priority_queue::QueueEntry;

// ---------------------------------------------------------------------------
// NativeExecutionParameters
// ---------------------------------------------------------------------------

/// Native execution parameters.
/// Matches upstream `KThread::NativeExecutionParameters` (k_thread.h).
pub struct NativeExecutionParameters {
    pub tpidr_el0: u64,
    pub tpidrro_el0: u64,
    // native_context omitted (void* — opaque platform-specific pointer)
    pub lock: std::sync::atomic::AtomicU32,
    pub is_running: bool,
    pub magic: u32,
}

impl Default for NativeExecutionParameters {
    fn default() -> Self {
        Self {
            tpidr_el0: 0,
            tpidrro_el0: 0,
            lock: std::sync::atomic::AtomicU32::new(1),
            is_running: false,
            // 'YUZU' in little-endian bytes: Y=0x59 U=0x55 Z=0x5A U=0x55
            magic: u32::from_le_bytes([b'Y', b'U', b'Z', b'U']),
        }
    }
}

// ---------------------------------------------------------------------------
// SyncObjectBuffer — matches upstream KThread::SyncObjectBuffer
// ---------------------------------------------------------------------------

/// Argument handle count max from Svc.
pub const SVC_ARGUMENT_HANDLE_COUNT_MAX: usize = 0x40;

// ---------------------------------------------------------------------------
// ThreadContext placeholder
// Upstream is Svc::ThreadContext with 31 GPRs, FP/SIMD regs, etc.
// ---------------------------------------------------------------------------

/// Placeholder for Svc::ThreadContext.
/// Mirrors the layout currently used by `arm_interface::ThreadContext`.
#[derive(Clone, Default)]
#[repr(C)]
pub struct ThreadContext {
    pub r: [u64; 31],
    pub fp: u64,
    pub lr: u64,
    pub sp: u64,
    pub pc: u64,
    pub pstate: u32,
    pub v: [u128; 32],
    pub fpcr: u32,
    pub fpsr: u32,
    pub tpidr: u64,
}

// ---------------------------------------------------------------------------
// KAffinityMask placeholder
// ---------------------------------------------------------------------------

/// Placeholder for KAffinityMask.
/// Upstream: k_affinity_mask.h. Simplified to a single u64 mask.
#[derive(Clone, Default)]
pub struct KAffinityMask {
    pub mask: u64,
}

impl KAffinityMask {
    pub fn get_affinity_mask(&self) -> u64 {
        self.mask
    }
    pub fn set_affinity_mask(&mut self, mask: u64) {
        self.mask = mask;
    }
}

// ---------------------------------------------------------------------------
// LockWithPriorityInheritanceInfo
// Matches upstream KThread::LockWithPriorityInheritanceInfo (k_thread.h:771-845).
// ---------------------------------------------------------------------------

/// Key for ordering waiters in the lock's thread tree.
/// Upstream uses LockWithPriorityInheritanceComparator which orders by
/// (condvar_key, priority, thread_id) — same as ConditionVariableComparator.
/// For lock waiters, condvar_key is effectively the address key, so we
/// order by (priority, thread_id) which is the meaningful ordering for
/// selecting the highest-priority waiter.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct LockWaiterKey {
    pub priority: i32,
    pub thread_id: u64,
}

/// Per-address lock tracking structure.
/// Upstream: nested class inside KThread, slab-allocated, intrusive-list node.
/// In Rust: owned in a Vec on the holding thread.
///
/// Each instance tracks one address key (mutex/lock address) and the set
/// of threads waiting to acquire that lock.
pub struct LockWithPriorityInheritanceInfo {
    /// Waiters ordered by (priority, thread_id) — matches upstream red-black tree.
    tree: std::collections::BTreeSet<LockWaiterKey>,
    /// The address being locked.
    address_key: KProcessAddress,
    /// Owner thread ID (upstream: raw pointer to KThread).
    owner_thread_id: u64,
    /// Number of waiters.
    waiter_count: u32,
    /// Whether this is a kernel address key.
    is_kernel_address_key: bool,
}

impl LockWithPriorityInheritanceInfo {
    /// Create a new lock info for the given address key.
    /// Matches upstream `LockWithPriorityInheritanceInfo::Create()`.
    pub fn new(address_key: KProcessAddress, is_kernel_address_key: bool) -> Self {
        Self {
            tree: std::collections::BTreeSet::new(),
            address_key,
            owner_thread_id: 0,
            waiter_count: 0,
            is_kernel_address_key,
        }
    }

    pub fn set_owner(&mut self, owner_thread_id: u64) {
        self.owner_thread_id = owner_thread_id;
    }

    /// Add a waiter thread. The caller must provide the waiter's priority and thread_id.
    /// Matches upstream `AddWaiter(KThread*)`.
    pub fn add_waiter(&mut self, priority: i32, thread_id: u64) {
        self.tree.insert(LockWaiterKey {
            priority,
            thread_id,
        });
        self.waiter_count += 1;
    }

    /// Remove a waiter thread. Returns true if the lock has no more waiters.
    /// Matches upstream `RemoveWaiter(KThread*)`.
    pub fn remove_waiter(&mut self, priority: i32, thread_id: u64) -> bool {
        self.tree.remove(&LockWaiterKey {
            priority,
            thread_id,
        });
        self.waiter_count -= 1;
        self.waiter_count == 0
    }

    /// Get the highest priority waiter's key.
    /// Matches upstream `GetHighestPriorityWaiter()` — front of tree = lowest
    /// (priority, thread_id) = highest priority.
    pub fn get_highest_priority_waiter(&self) -> Option<LockWaiterKey> {
        self.tree.iter().next().copied()
    }

    pub fn get_address_key(&self) -> KProcessAddress {
        self.address_key
    }

    pub fn get_is_kernel_address_key(&self) -> bool {
        self.is_kernel_address_key
    }

    pub fn get_owner_thread_id(&self) -> u64 {
        self.owner_thread_id
    }

    pub fn get_waiter_count(&self) -> u32 {
        self.waiter_count
    }
}

/// Reference to a LockWithPriorityInheritanceInfo on another thread.
/// Upstream uses a raw pointer; we store enough info to find it.
#[derive(Clone, Debug)]
pub struct WaitingLockRef {
    /// The thread that owns the lock info.
    pub owner_thread_id: u64,
    /// The address key identifying the lock.
    pub address_key: KProcessAddress,
    /// Whether it's a kernel address key.
    pub is_kernel_address_key: bool,
}

// ---------------------------------------------------------------------------
// KThread — the main thread structure
// ---------------------------------------------------------------------------

/// The kernel thread object.
/// Matches upstream `KThread` class (k_thread.h).
///
/// Uses indices/IDs instead of raw pointers for references to other kernel
/// objects. Arc/Weak used where upstream uses shared_ptr/weak_ptr.
pub struct KThread {
    // -- Core KThread fields --
    pub object_id: u64,
    pub self_reference: Option<Weak<Mutex<KThread>>>,
    pub thread_context: ThreadContext,
    pub condvar_arbiter_tree_node: RBEntry,
    pub priority: i32,

    // Condition variable / arbiter tree membership
    pub condvar_tree_state: ConditionVariableTreeState,
    pub condvar_key: u64,
    pub virtual_affinity_mask: u64,
    pub physical_affinity_mask: KAffinityMask,
    pub thread_id: u64,
    pub cpu_time: AtomicI64,
    pub address_key: KProcessAddress,
    // parent process — Weak reference matching upstream raw pointer + ref counting
    pub parent: Option<Weak<Mutex<KProcess>>>,
    pub scheduler: Option<Weak<Mutex<KScheduler>>>,
    pub kernel_stack_top: KVirtualAddress,
    pub light_ipc_data: Option<Vec<u32>>,
    pub tls_address: KProcessAddress,
    // m_activity_pause_lock — KLightLock; stubbed
    pub schedule_count: i64,
    pub last_scheduled_tick: i64,
    pub per_core_priority_queue_entry: [QueueEntry; NUM_CPU_CORES as usize],

    // Wait queue — upstream `m_wait_queue`
    pub wait_queue: Option<KThreadQueue>,

    // Lock with priority inheritance — matches upstream fields:
    // LockWithPriorityInheritanceInfoList m_held_lock_info_list{};
    // LockWithPriorityInheritanceInfo* m_waiting_lock_info{};
    pub held_lock_info_list: Vec<LockWithPriorityInheritanceInfo>,
    /// Index into the *owner* thread's held_lock_info_list that this thread
    /// is waiting on. None if not waiting on any lock.
    /// Upstream: raw pointer `m_waiting_lock_info`.
    /// We store (owner_thread_id, address_key, is_kernel_address_key) so we
    /// can find the lock info on the owner thread.
    pub waiting_lock_info: Option<WaitingLockRef>,

    pub address_key_value: u32,
    pub suspend_request_flags: u32,
    pub suspend_allowed_flags: u32,
    pub synced_index: i32,
    pub wait_result: u32, // Result code
    pub base_priority: i32,
    pub physical_ideal_core_id: i32,
    pub virtual_ideal_core_id: i32,
    pub num_kernel_waiters: i32,
    pub current_core_id: i32,
    pub core_id: i32,
    pub original_physical_affinity_mask: KAffinityMask,
    pub original_physical_ideal_core_id: i32,
    pub num_core_migration_disables: i32,
    pub thread_state: AtomicU16,
    pub termination_requested: AtomicBool,
    pub wait_cancelled: bool,
    pub cancellable: bool,
    pub signaled: bool,
    pub termination_wait_pair: Arc<(Mutex<bool>, Condvar)>,
    pub initialized: bool,
    pub debug_attached: bool,
    pub priority_inheritance_count: i8,
    pub resource_limit_release_hint: bool,
    pub is_kernel_address_key: bool,
    pub stack_parameters: StackParameters,

    // Emulation fields
    /// Host fiber context for this thread.
    /// Upstream: `std::shared_ptr<Common::Fiber> m_host_context`
    pub host_context: Option<Arc<common::fiber::Fiber>>,
    /// Context guard for fiber switching.
    /// Upstream: `KSpinLock m_context_guard`
    pub context_guard: parking_lot::Mutex<()>,
    pub thread_type: ThreadType,
    pub step_state: StepState,
    pub dummy_thread_runnable: AtomicBool,
    pub dummy_thread_mutex: Mutex<()>,
    pub dummy_thread_cv: Condvar,
    /// Host-thread parking mechanism for kernel wait operations.
    /// When begin_wait is called, the host thread parks on this condvar.
    /// When end_wait/cancel_wait is called, the condvar is notified.
    pub wait_park_mutex: Mutex<bool>,
    pub wait_park_cv: Condvar,

    // Debugging fields
    pub wait_reason_for_debugging: ThreadWaitReasonForDebugging,
    pub argument: usize,
    pub stack_top: KProcessAddress,
    pub native_execution_parameters: NativeExecutionParameters,
    pub sleep_deadline: Option<Instant>,
    /// Upstream: KTimerTask::m_time — absolute time in nanoseconds for
    /// the hardware timer. Set by KHardwareTimer::RegisterAbsoluteTask,
    /// cleared to 0 when the task fires or is cancelled.
    pub timer_task_time: i64,
    pub synchronization_wait: SynchronizationWaitSet,
    pub sync_object: SynchronizationObjectState,
}

impl KThread {
    pub fn restore_guest_context(&self, ctx: &mut ArmThreadContext) {
        ctx.r = self.thread_context.r;
        ctx.fp = self.thread_context.fp;
        ctx.lr = self.thread_context.lr;
        ctx.sp = self.thread_context.sp;
        ctx.pc = self.thread_context.pc;
        ctx.pstate = self.thread_context.pstate;
        ctx.v = self.thread_context.v;
        ctx.fpcr = self.thread_context.fpcr;
        ctx.fpsr = self.thread_context.fpsr;
        ctx.tpidr = self.thread_context.tpidr;
    }

    pub fn capture_guest_context(&mut self, ctx: &ArmThreadContext) {
        self.thread_context.r = ctx.r;
        self.thread_context.fp = ctx.fp;
        self.thread_context.lr = ctx.lr;
        self.thread_context.sp = ctx.sp;
        self.thread_context.pc = ctx.pc;
        self.thread_context.pstate = ctx.pstate;
        self.thread_context.v = ctx.v;
        self.thread_context.fpcr = ctx.fpcr;
        self.thread_context.fpsr = ctx.fpsr;
        self.thread_context.tpidr = ctx.tpidr;
    }

    /// Create a new KThread with default/zero-initialized state.
    pub fn new() -> Self {
        Self {
            object_id: 0,
            self_reference: None,
            thread_context: ThreadContext::default(),
            condvar_arbiter_tree_node: RBEntry::default(),
            priority: 0,
            condvar_tree_state: ConditionVariableTreeState::None,
            condvar_key: 0,
            virtual_affinity_mask: 0,
            physical_affinity_mask: KAffinityMask::default(),
            thread_id: 0,
            cpu_time: AtomicI64::new(0),
            address_key: KProcessAddress::default(),
            parent: None,
            scheduler: None,
            kernel_stack_top: KVirtualAddress::default(),
            light_ipc_data: None,
            tls_address: KProcessAddress::default(),
            schedule_count: 0,
            last_scheduled_tick: 0,
            per_core_priority_queue_entry: Default::default(),
            wait_queue: None,
            held_lock_info_list: Vec::new(),
            waiting_lock_info: None,
            address_key_value: 0,
            suspend_request_flags: 0,
            suspend_allowed_flags: ThreadState::SUSPEND_FLAG_MASK.bits() as u32,
            synced_index: 0,
            wait_result: RESULT_NO_SYNCHRONIZATION_OBJECT.get_inner_value(),
            base_priority: 0,
            physical_ideal_core_id: 0,
            virtual_ideal_core_id: 0,
            num_kernel_waiters: 0,
            current_core_id: 0,
            core_id: 0,
            original_physical_affinity_mask: KAffinityMask::default(),
            original_physical_ideal_core_id: 0,
            num_core_migration_disables: 0,
            thread_state: AtomicU16::new(0),
            termination_requested: AtomicBool::new(false),
            wait_cancelled: false,
            cancellable: false,
            signaled: false,
            termination_wait_pair: Arc::new((Mutex::new(false), Condvar::new())),
            initialized: false,
            debug_attached: false,
            priority_inheritance_count: 0,
            resource_limit_release_hint: false,
            is_kernel_address_key: false,
            stack_parameters: StackParameters::default(),
            host_context: None,
            context_guard: parking_lot::Mutex::new(()),
            thread_type: ThreadType::User,
            step_state: StepState::default(),
            dummy_thread_runnable: AtomicBool::new(true),
            dummy_thread_mutex: Mutex::new(()),
            dummy_thread_cv: Condvar::new(),
            wait_park_mutex: Mutex::new(false),
            wait_park_cv: Condvar::new(),
            wait_reason_for_debugging: ThreadWaitReasonForDebugging::default(),
            argument: 0,
            stack_top: KProcessAddress::default(),
            native_execution_parameters: NativeExecutionParameters::default(),
            sleep_deadline: None,
            timer_task_time: 0,
            synchronization_wait: SynchronizationWaitSet::new(),
            sync_object: SynchronizationObjectState::new(),
        }
    }

    // -- Getters / setters matching upstream --

    pub fn get_priority(&self) -> i32 {
        self.priority
    }

    pub fn get_object_id(&self) -> u64 {
        self.object_id
    }

    pub fn bind_self_reference(&mut self, thread: &Arc<Mutex<KThread>>) {
        self.self_reference = Some(Arc::downgrade(thread));
    }

    pub fn set_priority(&mut self, value: i32) {
        self.priority = value;
    }

    pub fn get_base_priority(&self) -> i32 {
        self.base_priority
    }

    pub fn get_thread_id(&self) -> u64 {
        self.thread_id
    }

    pub fn get_tls_address(&self) -> KProcessAddress {
        self.tls_address
    }

    pub fn get_tpidr_el0(&self) -> u64 {
        self.thread_context.tpidr
    }

    pub fn set_tpidr_el0(&mut self, value: u64) {
        self.thread_context.tpidr = value;
    }

    pub fn get_state(&self) -> ThreadState {
        let raw = self.thread_state.load(Ordering::Relaxed);
        ThreadState::from_bits_truncate(raw) & ThreadState::MASK
    }

    pub fn get_raw_state(&self) -> ThreadState {
        ThreadState::from_bits_truncate(self.thread_state.load(Ordering::Relaxed))
    }

    pub fn get_step_state(&self) -> StepState {
        self.step_state
    }

    pub fn set_step_state(&mut self, state: StepState) {
        self.step_state = state;
    }

    pub fn get_last_scheduled_tick(&self) -> i64 {
        self.last_scheduled_tick
    }

    pub fn set_last_scheduled_tick(&mut self, tick: i64) {
        self.last_scheduled_tick = tick;
    }

    pub fn add_cpu_time(&self, _core_id: i32, amount: i64) {
        self.cpu_time.fetch_add(amount, Ordering::Relaxed);
    }

    pub fn get_cpu_time(&self) -> i64 {
        self.cpu_time.load(Ordering::Relaxed)
    }

    pub fn get_active_core(&self) -> i32 {
        self.core_id
    }

    pub fn set_active_core(&mut self, core: i32) {
        self.core_id = core;
    }

    pub fn get_current_core(&self) -> i32 {
        self.current_core_id
    }

    pub fn set_current_core(&mut self, core: i32) {
        self.current_core_id = core;
    }

    pub fn is_user_thread(&self) -> bool {
        self.thread_type == ThreadType::User || self.parent.is_some()
    }

    /// Get the host fiber context for this thread.
    /// Upstream: `KThread::GetHostContext()` (k_thread.h:266).
    /// Used by CpuManager::RunThread and ShutdownThread for fiber switching.
    pub fn get_host_context(&self) -> Option<&Arc<common::fiber::Fiber>> {
        self.host_context.as_ref()
    }

    /// Set the host fiber context for this thread.
    pub fn set_host_context(&mut self, ctx: Arc<common::fiber::Fiber>) {
        self.host_context = Some(ctx);
    }

    /// Read the user-mode disable count from the thread's TLS region in guest memory.
    ///
    /// Upstream: `KThread::GetUserDisableCount()` (k_thread.cpp:552-560).
    /// The ThreadLocalRegion layout:
    ///   offset 0x000: message_buffer[0x100]
    ///   offset 0x100: disable_count (u16)
    ///   offset 0x102: interrupt_flag (u16)
    pub fn get_user_disable_count(&self) -> u16 {
        if !self.is_user_thread() {
            return 0;
        }
        let tls_addr = self.tls_address.get();
        if tls_addr == 0 {
            return 0;
        }
        // ThreadLocalRegion::disable_count is at offset 0x100.
        const DISABLE_COUNT_OFFSET: u64 = 0x100;
        let addr = tls_addr + DISABLE_COUNT_OFFSET;

        if let Some(parent) = self.parent.as_ref().and_then(|w| w.upgrade()) {
            let process = parent.lock().unwrap();
            let memory = process.get_shared_memory();
            let mem = memory.read().unwrap();
            mem.read_16(addr)
        } else {
            0
        }
    }

    /// Set the interrupt flag in the thread's TLS region in guest memory.
    ///
    /// Upstream: `KThread::SetInterruptFlag()` (k_thread.cpp:562-570).
    pub fn set_interrupt_flag(&self) {
        if !self.is_user_thread() {
            return;
        }
        let tls_addr = self.tls_address.get();
        if tls_addr == 0 {
            return;
        }
        // ThreadLocalRegion::interrupt_flag is at offset 0x102.
        const INTERRUPT_FLAG_OFFSET: u64 = 0x102;
        let addr = tls_addr + INTERRUPT_FLAG_OFFSET;

        if let Some(parent) = self.parent.as_ref().and_then(|w| w.upgrade()) {
            let mut process = parent.lock().unwrap();
            let memory = process.get_shared_memory();
            let mut mem = memory.write().unwrap();
            mem.write_16(addr, 1);
        }
    }

    /// Clear the interrupt flag in the thread's TLS region in guest memory.
    ///
    /// Upstream: `KThread::ClearInterruptFlag()` (k_thread.cpp:572-580).
    pub fn clear_interrupt_flag(&self) {
        if !self.is_user_thread() {
            return;
        }
        let tls_addr = self.tls_address.get();
        if tls_addr == 0 {
            return;
        }
        const INTERRUPT_FLAG_OFFSET: u64 = 0x102;
        let addr = tls_addr + INTERRUPT_FLAG_OFFSET;

        if let Some(parent) = self.parent.as_ref().and_then(|w| w.upgrade()) {
            let mut process = parent.lock().unwrap();
            let memory = process.get_shared_memory();
            let mut mem = memory.write().unwrap();
            mem.write_16(addr, 0);
        }
    }

    pub fn get_suspend_flags(&self) -> u32 {
        self.suspend_allowed_flags & self.suspend_request_flags
    }

    pub fn is_suspended(&self) -> bool {
        self.get_suspend_flags() != 0
    }

    pub fn is_suspend_requested_type(&self, suspend_type: SuspendType) -> bool {
        (self.suspend_request_flags
            & (1u32 << (ThreadState::SUSPEND_SHIFT as u32 + suspend_type as u32)))
            != 0
    }

    pub fn is_suspend_requested(&self) -> bool {
        self.suspend_request_flags != 0
    }

    pub fn get_synced_index(&self) -> i32 {
        self.synced_index
    }

    pub fn set_synced_index(&mut self, index: i32) {
        self.synced_index = index;
    }

    pub fn get_wait_result(&self) -> u32 {
        self.wait_result
    }

    pub fn set_wait_result(&mut self, result: u32) {
        self.wait_result = result;
    }

    pub fn get_yield_schedule_count(&self) -> i64 {
        self.schedule_count
    }

    pub fn set_yield_schedule_count(&mut self, count: i64) {
        self.schedule_count = count;
    }

    pub fn is_wait_cancelled(&self) -> bool {
        self.wait_cancelled
    }

    pub fn clear_wait_cancelled(&mut self) {
        self.wait_cancelled = false;
    }

    pub fn is_cancellable(&self) -> bool {
        self.cancellable
    }

    pub fn set_cancellable(&mut self) {
        self.cancellable = true;
    }

    pub fn clear_cancellable(&mut self) {
        self.cancellable = false;
    }

    pub fn is_termination_requested(&self) -> bool {
        self.termination_requested.load(Ordering::Relaxed)
            || self.get_raw_state() == ThreadState::TERMINATED
    }

    pub fn is_initialized(&self) -> bool {
        self.initialized
    }

    pub fn get_thread_type(&self) -> ThreadType {
        self.thread_type
    }

    pub fn is_dummy_thread(&self) -> bool {
        self.thread_type == ThreadType::Dummy
    }

    pub fn get_disable_dispatch_count(&self) -> i32 {
        self.stack_parameters.disable_count
    }

    pub fn disable_dispatch(&mut self) {
        self.stack_parameters.disable_count += 1;
    }

    pub fn enable_dispatch(&mut self) {
        assert!(self.stack_parameters.disable_count > 0);
        self.stack_parameters.disable_count -= 1;
    }

    pub fn set_in_exception_handler(&mut self) {
        self.stack_parameters.is_in_exception_handler = true;
    }

    pub fn clear_in_exception_handler(&mut self) {
        self.stack_parameters.is_in_exception_handler = false;
    }

    pub fn is_in_exception_handler(&self) -> bool {
        self.stack_parameters.is_in_exception_handler
    }

    pub fn set_is_calling_svc(&mut self) {
        self.stack_parameters.is_calling_svc = true;
    }

    pub fn clear_is_calling_svc(&mut self) {
        self.stack_parameters.is_calling_svc = false;
    }

    pub fn is_calling_svc(&self) -> bool {
        self.stack_parameters.is_calling_svc
    }

    pub fn get_svc_id(&self) -> u8 {
        self.stack_parameters.current_svc_id
    }

    pub fn register_dpc(&self, flag: DpcFlag) {
        self.stack_parameters
            .dpc_flags
            .fetch_or(flag.bits() as u8, Ordering::Relaxed);
    }

    pub fn clear_dpc(&self, flag: DpcFlag) {
        self.stack_parameters
            .dpc_flags
            .fetch_and(!(flag.bits() as u8), Ordering::Relaxed);
    }

    pub fn get_dpc(&self) -> u8 {
        self.stack_parameters.dpc_flags.load(Ordering::Relaxed)
    }

    pub fn has_dpc(&self) -> bool {
        self.get_dpc() != 0
    }

    pub fn set_wait_reason_for_debugging(&mut self, reason: ThreadWaitReasonForDebugging) {
        self.wait_reason_for_debugging = reason;
    }

    pub fn get_wait_reason_for_debugging(&self) -> ThreadWaitReasonForDebugging {
        self.wait_reason_for_debugging
    }

    pub fn get_num_kernel_waiters(&self) -> i32 {
        self.num_kernel_waiters
    }

    pub fn get_condition_variable_key(&self) -> u64 {
        self.condvar_key
    }

    pub fn get_condition_variable_tree(&self) -> Option<ConditionVariableTreeState> {
        match self.condvar_tree_state {
            ConditionVariableTreeState::None => None,
            state => Some(state),
        }
    }

    pub fn get_address_arbiter_key(&self) -> u64 {
        self.condvar_key
    }

    pub fn get_address_key(&self) -> KProcessAddress {
        self.address_key
    }

    pub fn get_address_key_value(&self) -> u32 {
        self.address_key_value
    }

    // -- LockWithPriorityInheritanceInfo methods --

    /// Find a held lock info by address key.
    /// Matches upstream `KThread::FindHeldLock()`.
    pub fn find_held_lock_index(
        &self,
        address_key: KProcessAddress,
        is_kernel_address_key: bool,
    ) -> Option<usize> {
        self.held_lock_info_list.iter().position(|info| {
            info.get_address_key() == address_key
                && info.get_is_kernel_address_key() == is_kernel_address_key
        })
    }

    /// Add a lock info to our held list and set ourselves as owner.
    /// Matches upstream `KThread::AddHeldLock()`.
    pub fn add_held_lock(&mut self, mut lock_info: LockWithPriorityInheritanceInfo) {
        if lock_info.get_is_kernel_address_key() {
            self.num_kernel_waiters += lock_info.get_waiter_count() as i32;
        }
        lock_info.set_owner(self.thread_id);
        self.held_lock_info_list.push(lock_info);
    }

    /// Set the waiting lock info reference.
    /// Matches upstream `KThread::SetWaitingLockInfo()`.
    pub fn set_waiting_lock_info(&mut self, lock_ref: Option<WaitingLockRef>) {
        self.waiting_lock_info = lock_ref;
    }

    /// Get the waiting lock info reference.
    /// Matches upstream `KThread::GetWaitingLockInfo()`.
    pub fn get_waiting_lock_info(&self) -> Option<&WaitingLockRef> {
        self.waiting_lock_info.as_ref()
    }

    /// Add a waiter thread to the appropriate lock info.
    /// Matches upstream `KThread::AddWaiterImpl()` (k_thread.cpp:962-989).
    ///
    /// The waiter's address_key and is_kernel_address_key must already be set.
    pub fn add_waiter_impl(
        &mut self,
        waiter_thread_id: u64,
        waiter_priority: i32,
        waiter_address_key: KProcessAddress,
        waiter_is_kernel_address_key: bool,
    ) {
        // Keep track of kernel waiters.
        if waiter_is_kernel_address_key {
            self.num_kernel_waiters += 1;
        }

        // Find or create the lock info for this address.
        let lock_idx = self.find_held_lock_index(waiter_address_key, waiter_is_kernel_address_key);
        let lock_idx = match lock_idx {
            Some(idx) => idx,
            None => {
                let lock_info =
                    LockWithPriorityInheritanceInfo::new(waiter_address_key, waiter_is_kernel_address_key);
                self.add_held_lock(lock_info);
                self.held_lock_info_list.len() - 1
            }
        };

        // Add the waiter.
        self.held_lock_info_list[lock_idx].add_waiter(waiter_priority, waiter_thread_id);
    }

    /// Remove a waiter thread from its lock info.
    /// Matches upstream `KThread::RemoveWaiterImpl()` (k_thread.cpp:991-1009).
    ///
    /// `waiter_lock_ref` identifies which lock the waiter is on.
    pub fn remove_waiter_impl(
        &mut self,
        waiter_thread_id: u64,
        waiter_priority: i32,
        waiter_is_kernel_address_key: bool,
        waiter_address_key: KProcessAddress,
    ) {
        // Keep track of kernel waiters.
        if waiter_is_kernel_address_key {
            self.num_kernel_waiters -= 1;
        }

        // Find the lock info.
        let lock_idx = self
            .find_held_lock_index(waiter_address_key, waiter_is_kernel_address_key)
            .expect("RemoveWaiterImpl: lock info not found");

        // Remove the waiter; if the lock is now empty, remove it.
        let is_empty = self.held_lock_info_list[lock_idx].remove_waiter(waiter_priority, waiter_thread_id);
        if is_empty {
            self.held_lock_info_list.remove(lock_idx);
        }
    }

    /// Public AddWaiter: adds waiter then triggers priority inheritance.
    /// Matches upstream `KThread::AddWaiter()` (k_thread.cpp:1063-1070).
    ///
    /// NOTE: Priority inheritance (RestorePriority) is simplified here —
    /// full chain-walking requires access to other threads' mutexes.
    /// Callers that need full priority inheritance should call
    /// `restore_priority_simplified()` after this.
    pub fn add_waiter(
        &mut self,
        waiter_thread_id: u64,
        waiter_priority: i32,
        waiter_address_key: KProcessAddress,
        waiter_is_kernel_address_key: bool,
    ) {
        self.add_waiter_impl(
            waiter_thread_id,
            waiter_priority,
            waiter_address_key,
            waiter_is_kernel_address_key,
        );

        // If the waiter has higher priority than us, inherit it.
        if waiter_priority < self.priority {
            self.restore_priority_simplified();
        }
    }

    /// Public RemoveWaiter: removes waiter then may restore priority.
    /// Matches upstream `KThread::RemoveWaiter()` (k_thread.cpp:1072-1081).
    pub fn remove_waiter(
        &mut self,
        waiter_thread_id: u64,
        waiter_priority: i32,
        waiter_is_kernel_address_key: bool,
        waiter_address_key: KProcessAddress,
    ) {
        self.remove_waiter_impl(
            waiter_thread_id,
            waiter_priority,
            waiter_is_kernel_address_key,
            waiter_address_key,
        );

        // If our priority equals the removed waiter's and we've inherited,
        // we may need to drop back.
        if self.priority == waiter_priority && self.priority < self.base_priority {
            self.restore_priority_simplified();
        }
    }

    /// Remove the highest priority waiter for a given address key and transfer
    /// lock ownership to it.
    /// Matches upstream `KThread::RemoveWaiterByKey()` (k_thread.cpp:1083-1142).
    pub fn remove_waiter_by_key(
        &mut self,
        address_key: KProcessAddress,
        is_kernel_address_key: bool,
        has_waiters: &mut bool,
    ) -> Option<(u64, i32)> {
        // Find the lock info for this address.
        let lock_idx = self.find_held_lock_index(address_key, is_kernel_address_key)?;

        // Remove the lock info from our held list.
        let mut lock_info = self.held_lock_info_list.remove(lock_idx);

        // Adjust kernel waiter count.
        if lock_info.get_is_kernel_address_key() {
            self.num_kernel_waiters -= lock_info.get_waiter_count() as i32;
            assert!(self.num_kernel_waiters >= 0);
        }

        assert!(lock_info.get_waiter_count() > 0);

        // Remove the highest priority waiter to become the next owner.
        let next_owner_key = lock_info.get_highest_priority_waiter()
            .expect("RemoveWaiterByKey: lock has waiters but tree is empty");

        let next_owner_thread_id = next_owner_key.thread_id;
        let next_owner_priority = next_owner_key.priority;

        if lock_info.remove_waiter(next_owner_key.priority, next_owner_key.thread_id) {
            // The new owner was the only waiter — lock info is freed (dropped).
            *has_waiters = false;
        } else {
            // There are additional waiters — transfer to new owner.
            *has_waiters = true;

            // Track kernel waiters for the new owner.
            // NOTE: The caller must call `next_owner.add_held_lock(lock_info)`
            // after locking the next owner thread. We return the lock_info
            // via a separate method to avoid borrow issues.
            // For now, we store it temporarily — the caller retrieves it.
            self.held_lock_info_list.push(lock_info);
            // Mark the last element as the "transfer" slot.
            // The caller must pop it and give it to the new owner.
        }

        // If our priority matched the next owner's and we've inherited, restore.
        if self.priority == next_owner_priority && self.priority < self.base_priority {
            self.restore_priority_simplified();
        }

        Some((next_owner_thread_id, next_owner_priority))
    }

    /// Take the last lock info from the held list (used for lock transfer
    /// after `remove_waiter_by_key` when has_waiters is true).
    pub fn take_transfer_lock_info(&mut self) -> Option<LockWithPriorityInheritanceInfo> {
        self.held_lock_info_list.pop()
    }

    /// Simplified RestorePriority — computes new priority from base priority
    /// and all held locks' highest-priority waiters.
    /// Matches upstream `KThread::RestorePriority()` (k_thread.cpp:1011-1061)
    /// but without the chain-walking (which requires locking other threads).
    pub fn restore_priority_simplified(&mut self) {
        let mut new_priority = self.base_priority;
        for lock_info in &self.held_lock_info_list {
            if let Some(highest) = lock_info.get_highest_priority_waiter() {
                new_priority = new_priority.min(highest.priority);
            }
        }

        if new_priority != self.priority {
            let old_priority = self.priority;
            self.priority = new_priority;
            self.notify_priority_change(old_priority);
        }
    }

    /// Collect all waiter thread IDs across all held locks.
    /// Replaces the old `waiter_thread_ids()` that returned the flat Vec.
    pub fn waiter_thread_ids(&self) -> Vec<u64> {
        let mut ids = Vec::new();
        for lock_info in &self.held_lock_info_list {
            for key in lock_info.tree.iter() {
                ids.push(key.thread_id);
            }
        }
        ids
    }

    /// Get waiter thread IDs for a specific address key.
    pub fn waiter_thread_ids_for_address(&self, address_key: KProcessAddress) -> Vec<u64> {
        for lock_info in &self.held_lock_info_list {
            if lock_info.get_address_key() == address_key {
                return lock_info.tree.iter().map(|k| k.thread_id).collect();
            }
        }
        Vec::new()
    }

    /// Legacy compatibility: add_waiter with just a thread_id.
    /// Used by callers that set the waiter's address key separately.
    /// The waiter_priority must be provided. For callers that don't have it,
    /// use a default of 0 (highest) — they should be updated to pass the real priority.
    pub fn add_waiter_by_id(
        &mut self,
        waiter_thread_id: u64,
        waiter_priority: i32,
        waiter_address_key: KProcessAddress,
        waiter_is_kernel_address_key: bool,
    ) {
        self.add_waiter(
            waiter_thread_id,
            waiter_priority,
            waiter_address_key,
            waiter_is_kernel_address_key,
        );
    }

    /// Legacy compatibility: remove_waiter with a thread_id.
    /// Searches all held locks for the given thread_id.
    pub fn remove_waiter_by_thread_id(&mut self, waiter_thread_id: u64) {
        for i in 0..self.held_lock_info_list.len() {
            let found = self.held_lock_info_list[i]
                .tree
                .iter()
                .find(|k| k.thread_id == waiter_thread_id)
                .copied();
            if let Some(key) = found {
                let is_kernel = self.held_lock_info_list[i].get_is_kernel_address_key();
                let is_empty = self.held_lock_info_list[i].remove_waiter(key.priority, key.thread_id);
                if is_kernel {
                    self.num_kernel_waiters -= 1;
                }
                if is_empty {
                    self.held_lock_info_list.remove(i);
                }
                if self.priority == key.priority && self.priority < self.base_priority {
                    self.restore_priority_simplified();
                }
                return;
            }
        }
    }

    pub fn get_is_kernel_address_key(&self) -> bool {
        self.is_kernel_address_key
    }

    pub fn set_user_address_key(&mut self, key: KProcessAddress, val: u32) {
        self.address_key = key;
        self.address_key_value = val;
        self.is_kernel_address_key = false;
    }

    pub fn set_kernel_address_key(&mut self, key: KProcessAddress) {
        self.address_key = key;
        self.is_kernel_address_key = true;
    }

    pub fn get_argument(&self) -> usize {
        self.argument
    }

    pub fn get_user_stack_top(&self) -> KProcessAddress {
        self.stack_top
    }

    pub fn get_affinity_mask(&self) -> &KAffinityMask {
        &self.physical_affinity_mask
    }

    pub fn get_native_execution_parameters(&mut self) -> &mut NativeExecutionParameters {
        &mut self.native_execution_parameters
    }

    pub fn get_sleep_deadline(&self) -> Option<Instant> {
        self.sleep_deadline
    }

    pub fn is_waiting_on_synchronization(&self) -> bool {
        self.synchronization_wait.is_active()
    }

    pub fn begin_wait_synchronization(&mut self, wait_set: SynchronizationWaitSet, timeout: i64) {
        self.synchronization_wait = wait_set;
        self.synced_index = -1;
        self.wait_result = RESULT_SUCCESS.get_inner_value();
        self.set_cancellable();
        self.set_wait_reason_for_debugging(ThreadWaitReasonForDebugging::Synchronization);

        if timeout > 0 {
            let timeout_ns = u64::try_from(timeout).unwrap_or(u64::MAX);
            self.sleep_deadline = Some(
                Instant::now()
                    .checked_add(Duration::from_nanos(timeout_ns))
                    .unwrap_or_else(|| Instant::now() + Duration::from_secs(365 * 24 * 60 * 60)),
            );
        } else {
            self.sleep_deadline = None;
        }

        self.begin_wait_with_queue(
            k_synchronization_object::ThreadQueueImplForKSynchronizationObjectWait::queue(),
        );
    }

    pub fn check_synchronization_ready(&self, process: &KProcess) -> Option<i32> {
        k_synchronization_object::check_wait_ready(process, self)
    }

    pub fn complete_synchronization_wait(&mut self, synced_index: i32, result: u32) {
        self.synced_index = synced_index;
        self.end_wait(result);
    }

    pub fn link_waiter(&mut self, process: &mut KProcess, thread_id: u64) {
        self.sync_object
            .link_waiter(process, super::k_synchronization_object::SynchronizationWaitNode {
                object_id: self.object_id,
                handle: super::k_synchronization_object::SynchronizationWaitNodeHandle {
                    thread_id,
                    wait_index: 0,
                },
            });
    }

    pub fn unlink_waiter(&mut self, process: &mut KProcess, thread_id: u64) {
        self.sync_object
            .unlink_waiter(process, thread_id, self.object_id, 0);
    }

    pub(crate) fn clear_wait_synchronization(&mut self) {
        if !self.synchronization_wait.is_active() {
            self.clear_cancellable();
            return;
        }

        if let Some(parent) = self.parent.as_ref().and_then(Weak::upgrade) {
            let mut process_guard = parent.lock().unwrap();
            k_synchronization_object::clear_wait_set(
                Some(&mut process_guard),
                self.thread_id,
                &mut self.synchronization_wait,
            );
        } else {
            k_synchronization_object::clear_wait_set(
                None,
                self.thread_id,
                &mut self.synchronization_wait,
            );
        }
        self.clear_cancellable();
    }

    pub(crate) fn apply_wait_result_to_context(&mut self) {
        self.thread_context.r[0] = self.wait_result as u64;
        self.thread_context.r[1] = (self.synced_index as u32) as u64;
    }

    // -- Complex methods stubbed --

    fn reset_thread_context32(&mut self, stack_top: u64, entry_point: u64, arg: u64) {
        // Upstream: ctx = {}; ctx.r[0]=arg; ctx.r[15]=entry; ctx.r[13]=sp; ctx.fpcr=0; ctx.fpsr=0;
        self.thread_context = ThreadContext::default();
        self.thread_context.r[0] = arg;
        self.thread_context.r[15] = entry_point;
        self.thread_context.r[13] = stack_top;
        self.thread_context.fpcr = 0;
        self.thread_context.fpsr = 0;
    }

    fn reset_thread_context64(&mut self, stack_top: u64, entry_point: u64, arg: u64) {
        self.thread_context = ThreadContext::default();
        self.thread_context.r[0] = arg;
        self.thread_context.r[18] = 1;
        self.thread_context.sp = stack_top;
        self.thread_context.pc = entry_point;
        self.thread_context.fpcr = 0;
        self.thread_context.fpsr = 0;
    }

    pub fn initialize_main_thread(
        &mut self,
        entry_point: u64,
        stack_top: u64,
        virt_core: i32,
        tls_address: u64,
        owner: &Arc<Mutex<KProcess>>,
        thread_id: u64,
        object_id: u64,
        is_64bit: bool,
    ) {
        self.initialize_main_thread_with_func(
            entry_point, stack_top, virt_core, tls_address,
            owner, thread_id, object_id, is_64bit, None,
        );
    }

    /// Initialize as a main thread with an optional host fiber init function.
    /// Upstream: `InitializeThread(thread, {}, {}, {}, IdleThreadPriority, virt_core, {},
    ///           ThreadType::Main, system.GetCpuManager().GetGuestActivateFunc())`
    pub fn initialize_main_thread_with_func(
        &mut self,
        entry_point: u64,
        stack_top: u64,
        virt_core: i32,
        tls_address: u64,
        owner: &Arc<Mutex<KProcess>>,
        thread_id: u64,
        object_id: u64,
        is_64bit: bool,
        init_func: Option<Box<dyn FnOnce() + Send>>,
    ) {
        let phys_core = virt_core;
        self.object_id = object_id;
        self.thread_type = ThreadType::Main;
        self.thread_id = thread_id;
        self.priority = IDLE_THREAD_PRIORITY;
        self.base_priority = IDLE_THREAD_PRIORITY;
        self.virtual_ideal_core_id = virt_core;
        self.physical_ideal_core_id = phys_core;
        self.virtual_affinity_mask = 1u64 << virt_core;
        self.physical_affinity_mask
            .set_affinity_mask(1u64 << phys_core);
        self.tls_address = KProcessAddress::new(tls_address);
        self.parent = Some(Arc::downgrade(owner));
        self.scheduler = owner.lock().unwrap().scheduler.clone();
        self.stack_top = KProcessAddress::new(stack_top);
        self.argument = 0;
        self.core_id = phys_core;
        self.current_core_id = phys_core;
        self.thread_state
            .store(ThreadState::RUNNABLE.bits(), Ordering::Relaxed);
        self.suspend_allowed_flags = ThreadState::SUSPEND_FLAG_MASK.bits() as u32;
        self.suspend_request_flags = 0;
        self.wait_result = RESULT_NO_SYNCHRONIZATION_OBJECT.get_inner_value();
        self.schedule_count = -1;
        self.initialized = true;
        self.sleep_deadline = None;
        self.synchronization_wait.clear();
        self.stack_parameters.disable_count = 1;
        self.stack_parameters.is_in_exception_handler = true;

        if is_64bit {
            self.reset_thread_context64(stack_top, entry_point, 0);
        } else {
            self.reset_thread_context32(stack_top, entry_point, 0);
        }
        // Upstream does NOT set thread_context.tpidr here.
        // The TLS address is stored in m_tls_address and passed to the JIT
        // via SetTpidrroEl0 (CP15 URO) during LoadContext, not via ctx.tpidr (UPRW).

        // Initialize emulation parameters — create host fiber context.
        // Upstream: thread->m_host_context = std::make_shared<Common::Fiber>(std::move(init_func));
        if let Some(func) = init_func {
            self.host_context = Some(common::fiber::Fiber::new(func));
        }
    }

    pub fn initialize_user_thread(
        &mut self,
        entry_point: u64,
        arg: u64,
        stack_top: u64,
        prio: i32,
        virt_core: i32,
        owner: &Arc<Mutex<KProcess>>,
        thread_id: u64,
        object_id: u64,
        is_64bit: bool,
    ) -> u32 {
        self.initialize_user_thread_with_init_func(
            entry_point, arg, stack_top, prio, virt_core,
            owner, thread_id, object_id, is_64bit, None,
        )
    }

    /// Initialize as a user thread with an optional host fiber init function.
    /// Upstream: `InitializeUserThread(system, thread, func, arg, user_stack_top, prio,
    ///           virt_core, owner)` passes `system.GetCpuManager().GetGuestThreadFunc()`.
    pub fn initialize_user_thread_with_init_func(
        &mut self,
        entry_point: u64,
        arg: u64,
        stack_top: u64,
        prio: i32,
        virt_core: i32,
        owner: &Arc<Mutex<KProcess>>,
        thread_id: u64,
        object_id: u64,
        is_64bit: bool,
        init_func: Option<Box<dyn FnOnce() + Send>>,
    ) -> u32 {
        let tls_address = {
            let mut process = owner.lock().unwrap();
            match process.create_thread_local_region() {
                Some(address) => address,
                None => return RESULT_OUT_OF_RESOURCE.get_inner_value(),
            }
        };
        let owner_weak = Arc::downgrade(owner);
        let scheduler = owner.lock().unwrap().scheduler.clone();

        self.initialize_user_thread_with_tls(
            entry_point,
            arg,
            stack_top,
            prio,
            virt_core,
            owner_weak,
            scheduler,
            tls_address,
            thread_id,
            object_id,
            is_64bit,
            init_func,
        )
    }

    pub fn initialize_user_thread_with_tls(
        &mut self,
        entry_point: u64,
        arg: u64,
        stack_top: u64,
        prio: i32,
        virt_core: i32,
        owner: Weak<Mutex<KProcess>>,
        scheduler: Option<Weak<Mutex<KScheduler>>>,
        tls_address: KProcessAddress,
        thread_id: u64,
        object_id: u64,
        is_64bit: bool,
        init_func: Option<Box<dyn FnOnce() + Send>>,
    ) -> u32 {
        
        let phys_core = virt_core;
        self.object_id = object_id;
        self.thread_type = ThreadType::User;
        self.thread_id = thread_id;
        self.priority = prio;
        self.base_priority = prio;
        self.virtual_ideal_core_id = virt_core;
        self.physical_ideal_core_id = phys_core;
        self.virtual_affinity_mask = 1u64 << virt_core;
        self.physical_affinity_mask
            .set_affinity_mask(1u64 << phys_core);
        self.thread_state
            .store(ThreadState::INITIALIZED.bits(), Ordering::Relaxed);
        self.suspend_allowed_flags = ThreadState::SUSPEND_FLAG_MASK.bits() as u32;
        self.suspend_request_flags = 0;
        self.tls_address = tls_address;
        self.parent = Some(owner);
        self.scheduler = scheduler;
        self.signaled = false;
        let (termination_lock, _) = &*self.termination_wait_pair;
        *termination_lock.lock().unwrap() = false;
        self.termination_requested.store(false, Ordering::Relaxed);
        self.wait_cancelled = false;
        self.cancellable = false;
        self.core_id = phys_core;
        self.current_core_id = phys_core;
        self.wait_result = RESULT_NO_SYNCHRONIZATION_OBJECT.get_inner_value();
        self.schedule_count = -1;
        self.last_scheduled_tick = 0;
        self.num_kernel_waiters = 0;
        self.resource_limit_release_hint = false;
        self.sleep_deadline = None;
        self.synchronization_wait.clear();
        self.stack_top = KProcessAddress::new(stack_top);
        self.argument = arg as usize;
        self.stack_parameters = StackParameters::default();
        self.stack_parameters.disable_count = 1;
        self.stack_parameters.is_in_exception_handler = true;
        self.initialized = true;

        if is_64bit {
            self.reset_thread_context64(stack_top, entry_point, arg);
        } else {
            self.reset_thread_context32(stack_top, entry_point, arg);
        }
        // Upstream does NOT set thread_context.tpidr here.
        // The TLS address is passed via SetTpidrroEl0 (CP15 URO) during LoadContext.

        // Initialize emulation parameters — create host fiber context.
        // Upstream: thread->m_host_context = std::make_shared<Common::Fiber>(std::move(init_func));
        if let Some(func) = init_func {
            self.host_context = Some(common::fiber::Fiber::new(func));
        }

        RESULT_SUCCESS.get_inner_value()
    }

    pub fn clone_fpu_status_from(&mut self, current: &KThread) {
        self.thread_context.fpcr = current.thread_context.fpcr;
        self.thread_context.fpsr = current.thread_context.fpsr;
    }

    /// Set the thread's base priority with priority inheritance handling.
    /// Upstream: full priority inheritance chain update in k_thread.cpp.
    pub fn set_base_priority(&mut self, value: i32) {
        let old_priority = self.priority;
        let waiting_on_condition_variable = matches!(
            self.get_condition_variable_tree(),
            Some(ConditionVariableTreeState::ConditionVariable)
        );
        let parent = if waiting_on_condition_variable {
            self.parent.as_ref().and_then(Weak::upgrade)
        } else {
            None
        };

        if old_priority != value {
            if let Some(parent) = parent.as_ref() {
                parent
                    .lock()
                    .unwrap()
                    .before_update_condition_variable_priority(self.thread_id);
            }
        }

        self.base_priority = value;
        self.priority = value;
        if old_priority != value {
            let updated_thread_key = self.condition_variable_tree_key();
            if let Some(parent) = parent.as_ref() {
                parent
                    .lock()
                    .unwrap()
                    .after_update_condition_variable_priority(updated_thread_key);
            }
            self.notify_priority_change(old_priority);
        }
    }

    /// Run the thread.
    /// Matches upstream `KThread::Run()`.
    pub fn run(&mut self) -> u32 {
        // Check termination.
        if self.termination_requested.load(Ordering::Relaxed) {
            return RESULT_TERMINATION_REQUESTED.get_inner_value();
        }
        // Upstream also checks current thread termination — we skip as single-thread.

        // Validate state.
        if self.get_state() != ThreadState::INITIALIZED {
            return RESULT_INVALID_STATE.get_inner_value();
        }

        // If this is a user thread that is suspended, update its state.
        if self.is_user_thread() && self.is_suspended() {
            self.update_state();
        }

        // Increment parent's running thread count.
        // NOTE: We do NOT lock the parent here — the caller (KProcess::Run)
        // typically already holds the process lock. The caller is responsible
        // for calling process.increment_running_thread_count() after thread.run().
        // This avoids a deadlock (thread.run called while process lock is held).

        // Set state to Runnable.
        self.set_state(ThreadState::RUNNABLE);
        RESULT_SUCCESS.get_inner_value()
    }

    /// Start the termination sequence.
    /// Matches upstream `KThread::StartTermination()` (k_thread.cpp:402-426).
    ///
    /// Upstream requires KScheduler::IsSchedulerLockedByCurrentThread().
    fn start_termination(&mut self) {
        // Release user exception and unpin, if relevant.
        if let Some(parent) = self.parent.as_ref().and_then(Weak::upgrade) {
            let mut process = parent.lock().unwrap();

            // Upstream: m_parent->ReleaseUserException(this)
            // Releases the exception thread if this thread holds it,
            // then wakes the next waiter via RemoveKernelWaiterByKey.
            if process.exception_thread_id == Some(self.thread_id) {
                process.exception_thread_id = None;
                // Wake the next kernel waiter that was blocked on the exception thread.
                // Upstream uses RemoveKernelWaiterByKey with the exception_thread
                // address | 1 as the key. We wake the first user waiter as a
                // simplified equivalent (kernel waiters are tracked only by count).
                if self.num_kernel_waiters > 0 {
                    self.num_kernel_waiters -= 1;
                    // The waiter would be woken via EndWait — in our model,
                    // the process-level sync will handle notification via
                    // FinishTermination's NotifyAvailable.
                }
            }

            // Upstream: if (m_parent->GetPinnedThread(GetCurrentCoreId(m_kernel)) == this)
            //               m_parent->UnpinCurrentThread();
            for core_id in 0..NUM_CPU_CORES as usize {
                if process.pinned_threads[core_id] == Some(self.thread_id) {
                    process.pinned_threads[core_id] = None;
                }
            }

            // Set state to terminated.
            self.set_state(ThreadState::TERMINATED);

            // Clear the thread's status as running in parent.
            // Upstream: m_parent->ClearRunningThread(this)
            process.clear_running_thread(self.thread_id);

            // Remove from priority queue.
            process.remove_from_priority_queue(self.thread_id);
        } else {
            // No parent — just set state.
            self.set_state(ThreadState::TERMINATED);
        }

        // Clear previous thread in KScheduler.
        // Upstream: KScheduler::ClearPreviousThread(m_kernel, this)
        // Upstream: KScheduler::ClearPreviousThread(m_kernel, this).

        // Register terminated DPC flag.
        self.register_dpc(DpcFlag::TERMINATED);
    }

    /// Finish the termination (called from worker task or inline).
    /// Matches upstream `KThread::FinishTermination()` (k_thread.cpp:428-448).
    ///
    /// Upstream spins until the thread is not executing on any core, then
    /// acquires the scheduler lock, signals the synchronization object,
    /// and closes the thread reference.
    pub fn finish_termination(&mut self) {
        // Upstream: Ensure the thread is not executing on any core.
        // Upstream spin-waits checking each core's scheduler current thread.
        // We check via the process's scheduler references if available.
        if self.parent.is_some() {
            // Spin-wait: upstream does a tight loop per core checking
            // scheduler.GetSchedulerCurrentThread() != this.
            // In our model, the fiber-based context switching ensures
            // that by the time the worker task runs, the thread has
            // already been unloaded from its core. The spin is a safety check.
            // We yield briefly to let any in-progress context switch complete.
            std::thread::yield_now();
        }

        // Upstream: KScopedSchedulerLock sl{m_kernel};
        // The scheduler lock ensures atomicity with thread state changes.
        // The caller (do_worker_task_impl or exit) should acquire this.

        // Signal.
        // Upstream: m_signaled = true; KSynchronizationObject::NotifyAvailable();
        self.signaled = true;
        {
            let (lock, cv) = &*self.termination_wait_pair;
            let mut termination_completed = lock.lock().unwrap();
            *termination_completed = true;
            cv.notify_all();
        }

        // Notify any waiters (matches KSynchronizationObject::NotifyAvailable).
        if let Some(parent) = self.parent.as_ref().and_then(Weak::upgrade) {
            let mut process = parent.lock().unwrap();
            let waiter_snapshot = self.sync_object.waiter_snapshot(&process);
            let outcome = k_synchronization_object::process_waiter_snapshot(
                &mut process,
                self.object_id,
                &waiter_snapshot,
                RESULT_SUCCESS.get_inner_value(),
            );
            for waiter_thread_id in outcome.unlink_thread_ids {
                self.unlink_waiter(&mut process, waiter_thread_id);
            }
        }

        // Upstream: this->Close() — decrements reference count.
        // Reference counting is not yet implemented; the Arc<Mutex<KThread>>
        // will be dropped when all references are released.
    }

    /// Exit the thread (called by the thread itself).
    /// Matches upstream `KThread::Exit()` (k_thread.cpp:1179-1208).
    ///
    /// Upstream flow:
    /// 1. Release resource limit hint (ThreadCountMax, 0, 1)
    /// 2. Decrement parent's running thread count
    /// 3. Under scheduler lock: disallow suspension, UpdateState, StartTermination
    /// 4. Register with KWorkerTaskManager::WorkerType::Exit
    /// 5. UNREACHABLE — the thread never returns from Exit()
    ///
    /// Our port queues `DoWorkerTaskImpl()` on KWorkerTaskManager like upstream,
    /// but still returns normally because guest thread exit is cooperative here.
    pub fn exit(&mut self) {
        // Release resource hint and decrement running count from parent.
        if let Some(parent) = self.parent.as_ref().and_then(Weak::upgrade) {
            let mut process = parent.lock().unwrap();
            // Upstream: m_parent->GetResourceLimit()->Release(ThreadCountMax, 0, 1)
            if let Some(ref rl) = process.resource_limit {
                rl.lock().unwrap().release_with_hint(
                    super::k_resource_limit::LimitableResource::ThreadCountMax,
                    0,
                    1,
                );
            }
            self.resource_limit_release_hint = true;
            let should_terminate_process = process.decrement_running_thread_count();
            drop(process);

            // If the running thread count reached zero, terminate the process.
            // We must do this after dropping the process lock to avoid deadlock.
            if should_terminate_process {
                if let Some(parent) = self.parent.as_ref().and_then(Weak::upgrade) {
                    parent.lock().unwrap().terminate();
                }
            }
        }

        // Perform termination under (simulated) scheduler lock.
        {
            // Disallow all suspension.
            self.suspend_allowed_flags = 0;
            self.update_state();

            // Disallow all suspension (upstream sets this twice).
            self.suspend_allowed_flags = 0;

            // Start termination.
            self.start_termination();

            let worker_thread = self
                .self_reference
                .as_ref()
                .and_then(Weak::upgrade)
                .or_else(|| {
                    self.parent.as_ref().and_then(Weak::upgrade).and_then(|parent| {
                        parent
                            .lock()
                            .unwrap()
                            .get_thread_by_object_id(self.object_id)
                    })
                });

            if let Some(worker_thread) = worker_thread {
                KWorkerTaskManager::add_task_static(
                    0,
                    WorkerType::Exit,
                    Box::new(move || {
                        worker_thread.lock().unwrap().do_worker_task_impl();
                    }),
                );
            } else {
                self.finish_termination();
            }
        }

        // Upstream: UNREACHABLE_MSG("KThread::Exit() would return")
        // In the full fiber-based execution model, the scheduler context-switches
        // away and this point is never reached.
        log::error!("KThread::Exit() returned — upstream marks this as unreachable");
    }

    /// Terminate the thread (called by another thread).
    /// Matches upstream `KThread::Terminate()` (k_thread.cpp:1210-1223).
    ///
    /// Upstream flow:
    /// 1. ASSERT(this != GetCurrentThreadPointer(m_kernel))
    /// 2. RequestTerminate() — if already Terminated, succeed immediately
    /// 3. If not yet terminated: KSynchronizationObject::Wait(kernel, &index,
    ///    &[this], 1, WaitInfinite) — blocks until the thread signals
    ///
    /// Our `&mut self` variant cannot block safely because callers commonly
    /// hold the thread mutex while invoking it. Use `terminate_thread()` when
    /// a caller owns `Arc<Mutex<KThread>>` and needs upstream-style waiting.
    pub fn terminate(&mut self) -> u32 {
        // Request termination.
        let new_state = self.request_terminate();
        if new_state == ThreadState::TERMINATED {
            return RESULT_SUCCESS.get_inner_value();
        }

        // Upstream: Wait on this thread as a synchronization object until it
        // signals (i.e., until FinishTermination sets m_signaled = true).
        //   s32 index;
        //   KSynchronizationObject* objects[] = {this};
        //   R_TRY(KSynchronizationObject::Wait(m_kernel, &index, objects, 1,
        //                                      Svc::WaitInfinite));
        //
        // Wait using the termination condvar. This blocks until
        // FinishTermination() sets the completion flag.
        let wait_pair = self.termination_wait_pair.clone();
        let (lock, cv) = &*wait_pair;
        let mut completed = lock.lock().unwrap();
        while !*completed {
            completed = cv.wait(completed).unwrap();
        }

        RESULT_SUCCESS.get_inner_value()
    }

    /// Rust-side helper for upstream-style `KThread::Terminate()` semantics.
    ///
    /// This variant can safely wait for `FinishTermination()` because it drops
    /// the thread mutex before blocking on the termination condition variable.
    pub fn terminate_thread(thread: &Arc<Mutex<KThread>>) -> u32 {
        let wait_pair = {
            let mut guard = thread.lock().unwrap();
            let new_state = guard.request_terminate();
            if new_state == ThreadState::TERMINATED || guard.is_signaled() {
                return RESULT_SUCCESS.get_inner_value();
            }
            guard.termination_wait_pair.clone()
        };

        let (lock, cv) = &*wait_pair;
        let mut termination_completed = lock.lock().unwrap();
        while !*termination_completed {
            termination_completed = cv.wait(termination_completed).unwrap();
        }

        RESULT_SUCCESS.get_inner_value()
    }

    /// Request termination of the thread.
    /// Matches upstream `KThread::RequestTerminate()`.
    pub fn request_terminate(&mut self) -> ThreadState {
        // Atomic CAS: only proceed if this is the first request.
        let mut expected = false;
        let first_request = self.termination_requested.compare_exchange(
            expected, true, Ordering::SeqCst, Ordering::SeqCst,
        ).is_ok();

        if first_request {
            // Fast path: if INITIALIZED, directly terminate.
            if self.get_state() == ThreadState::INITIALIZED {
                self.thread_state.store(ThreadState::TERMINATED.bits(), Ordering::Relaxed);
                return ThreadState::TERMINATED;
            }

            // Register terminating DPC.
            self.register_dpc(DpcFlag::TERMINATING);

            // Unpin if pinned.
            if self.stack_parameters.is_pinned {
                // Upstream: self.GetOwnerProcess().UnpinThread(self)
                self.stack_parameters.is_pinned = false;
            }

            // Clear suspension.
            if self.is_suspended() {
                self.suspend_allowed_flags = 0;
                self.update_state();
            }

            // Raise priority to terminating priority.
            const TERMINATING_THREAD_PRIORITY: i32 = -1; // SystemThreadPriorityHighest - 1
            self.increase_base_priority(TERMINATING_THREAD_PRIORITY);

            // If RUNNABLE, request an interrupt-driven reschedule like upstream
            // sending a termination IPI to the candidate cores.
            if self.get_state() == ThreadState::RUNNABLE {
                if let Some(scheduler) = self.scheduler.as_ref().and_then(Weak::upgrade) {
                    scheduler.lock().unwrap().request_schedule_on_interrupt();
                } else {
                    self.request_schedule();
                }
            }

            // If WAITING, cancel the wait.
            if self.get_state() == ThreadState::WAITING {
                if let Some(wq) = self.wait_queue {
                    wq.cancel_wait(self, RESULT_TERMINATION_REQUESTED.get_inner_value(), true);
                }
            }
        }

        self.get_state()
    }

    /// Increase base priority (only if the new priority is higher).
    /// Matches upstream `KThread::IncreaseBasePriority`.
    pub fn increase_base_priority(&mut self, priority: i32) {
        if self.base_priority > priority {
            self.base_priority = priority;
            // Upstream: RestorePriority(kernel, this)
            self.restore_priority_simplified();
        }
    }

    /// Request suspend of the given type.
    /// Matches upstream `KThread::RequestSuspend()`.
    pub fn request_suspend(&mut self, suspend_type: SuspendType) {
        let bit = 1u32 << (ThreadState::SUSPEND_SHIFT as u32 + suspend_type as u32);
        self.suspend_request_flags |= bit;
        self.try_suspend();
    }

    /// Resume from the given suspend type.
    /// Matches upstream `KThread::Resume()`.
    pub fn resume(&mut self, suspend_type: SuspendType) {
        let bit = 1u32 << (ThreadState::SUSPEND_SHIFT as u32 + suspend_type as u32);
        self.suspend_request_flags &= !bit;
        self.update_state();
    }

    /// Try to suspend the thread.
    /// Matches upstream `KThread::TrySuspend()`.
    pub fn try_suspend(&mut self) {
        if !self.is_suspend_requested() {
            return;
        }
        if self.get_num_kernel_waiters() > 0 {
            return;
        }
        self.update_state();
    }

    /// Update the thread state.
    /// Matches upstream `KThread::UpdateState()`.
    pub fn update_state(&mut self) {
        let old_state = self.get_raw_state();
        let base_state = old_state & ThreadState::MASK;
        let suspend_bits = ThreadState::from_bits_truncate(self.get_suspend_flags() as u16);
        let new_state = suspend_bits | base_state;
        self.thread_state.store(new_state.bits(), Ordering::Relaxed);
        if new_state != old_state && self.is_suspended() && base_state == ThreadState::WAITING {
            self.set_wait_reason_for_debugging(ThreadWaitReasonForDebugging::Suspended);
        }
        self.notify_state_transition(old_state, new_state);
    }

    /// Continue the thread.
    /// Matches upstream `KThread::ContinueThread()`.
    pub fn continue_thread(&mut self) {
        let old_state = self.get_raw_state();
        let continued_state = old_state & ThreadState::MASK;
        self.thread_state
            .store(continued_state.bits(), Ordering::Relaxed);
        if continued_state != ThreadState::WAITING {
            self.set_wait_reason_for_debugging(ThreadWaitReasonForDebugging::None);
        }
        self.notify_state_transition(old_state, continued_state);
    }

    /// Set the thread state.
    /// Matches upstream `KThread::SetState()`.
    pub fn set_state(&mut self, state: ThreadState) {
        let old_state = self.get_raw_state();
        self.set_wait_reason_for_debugging(ThreadWaitReasonForDebugging::None);
        let new_state = (old_state & !ThreadState::MASK) | (state & ThreadState::MASK);
        self.thread_state.store(new_state.bits(), Ordering::Relaxed);
        if new_state != old_state {
            self.schedule_count = -1;
        }
        self.notify_state_transition(old_state, new_state);
    }

    fn notify_state_transition(&self, old_state: ThreadState, new_state: ThreadState) {
        if old_state == new_state {
            return;
        }

        let old_base = old_state & ThreadState::MASK;
        let new_base = new_state & ThreadState::MASK;

        // NOTE: PQ updates happen here in upstream (via KScheduler::OnThreadStateChanged
        // which calls PQ.Remove/PushBack). We cannot do it here because this is called
        // while the thread's own mutex is held, and locking the parent process would
        // create a lock-ordering deadlock (thread→process vs process→thread).
        //
        // Instead, callers that transition thread state while holding the process lock
        // should call process.push_back_to_priority_queue / remove_from_priority_queue
        // directly. The dispatch loop's scan_runnable_threads fallback handles
        // the case where the PQ is not up to date.

        // Notify the scheduler for dispatch.
        if let Some(scheduler) = self.scheduler.as_ref().and_then(Weak::upgrade) {
            scheduler.lock().unwrap().on_thread_state_changed(
                self.thread_id,
                old_base,
                new_base,
            );
        }
    }

    fn notify_priority_change(&self, old_priority: i32) {
        // Same note as notify_state_transition: PQ updates cannot happen here
        // due to lock ordering. Callers holding the process lock should call
        // process.change_priority_in_queue directly.

        if let Some(scheduler) = self.scheduler.as_ref().and_then(Weak::upgrade) {
            scheduler
                .lock()
                .unwrap()
                .on_thread_priority_changed(self.thread_id, old_priority);
        }
    }

    fn request_schedule(&self) {
        let Some(scheduler) = self.scheduler.as_ref().and_then(Weak::upgrade) else {
            return;
        };
        scheduler.lock().unwrap().request_schedule();
    }

    /// Pin the thread to a core.
    /// Matches upstream `KThread::Pin(s32 current_core)`.
    pub fn pin(&mut self, current_core: i32) {
        // Set pinned flag.
        self.stack_parameters.is_pinned = true;

        // Disable core migration.
        debug_assert!(self.num_core_migration_disables == 0);
        self.num_core_migration_disables += 1;

        // Save original state for unpinning.
        self.original_physical_ideal_core_id = self.physical_ideal_core_id;
        self.original_physical_affinity_mask = self.physical_affinity_mask.clone();

        // Bind to current core.
        let _active_core = self.get_active_core();
        self.set_active_core(current_core);
        self.physical_ideal_core_id = current_core;
        self.physical_affinity_mask.set_affinity_mask(1u64 << current_core);

        // Upstream: notify scheduler of affinity change if needed.

        // Disallow thread suspension.
        self.suspend_allowed_flags &=
            !(1u32 << (ThreadState::SUSPEND_SHIFT as u32 + SuspendType::Thread as u32));
        self.update_state();
    }

    /// Unpin the thread.
    /// Matches upstream `KThread::Unpin()`.
    pub fn unpin(&mut self) {
        // Clear pinned flag.
        self.stack_parameters.is_pinned = false;

        // Enable core migration.
        debug_assert!(self.num_core_migration_disables == 1);
        self.num_core_migration_disables -= 1;

        // Restore original affinity.
        let old_mask = self.physical_affinity_mask.clone();
        self.physical_ideal_core_id = self.original_physical_ideal_core_id;
        self.physical_affinity_mask = self.original_physical_affinity_mask.clone();

        if self.physical_affinity_mask.get_affinity_mask() != old_mask.get_affinity_mask() {
            let active_core = self.get_active_core();
            // Check if current core is still valid.
            if (self.physical_affinity_mask.get_affinity_mask() & (1u64 << active_core)) == 0 {
                if self.physical_ideal_core_id >= 0 {
                    self.set_active_core(self.physical_ideal_core_id);
                } else {
                    // Pick highest valid core.
                    let mask = self.physical_affinity_mask.get_affinity_mask();
                    if mask != 0 {
                        self.set_active_core((63 - mask.leading_zeros()) as i32);
                    }
                }
            }
            // Upstream: notify scheduler of affinity change.
        }

        // Allow thread suspension (if termination not requested).
        if !self.is_termination_requested() {
            self.suspend_allowed_flags |=
                1u32 << (ThreadState::SUSPEND_SHIFT as u32 + SuspendType::Thread as u32);
            self.update_state();
        }

        // Upstream: resume pinned waiter list threads.
    }

    /// Wait cancel.
    /// Matches upstream `KThread::WaitCancel()`.
    pub fn wait_cancel(&mut self) {
        if self.get_state() == ThreadState::WAITING && self.cancellable {
            self.wait_cancelled = false;
            self.synced_index = -1;
            self.cancel_wait(RESULT_CANCELLED.get_inner_value(), true);
        } else {
            self.wait_cancelled = true;
        }
    }

    /// Begin wait on a thread queue.
    ///
    /// Matches upstream `KThread::BeginWait(KThreadQueue* queue)`:
    /// sets state to Waiting, assigns the wait queue, and parks the host thread.
    ///
    /// The host thread blocks on `wait_park_cv` until `end_wait` or `cancel_wait`
    /// sets the parked flag to false and notifies.
    pub fn begin_wait_with_queue(&mut self, wait_queue: KThreadQueue) {
        self.set_state(ThreadState::WAITING);
        self.wait_queue = Some(wait_queue);

        // Park the host thread: set parked=true, then wait until unparked.
        {
            let mut parked = self.wait_park_mutex.lock().unwrap();
            *parked = true;
            while *parked {
                parked = self.wait_park_cv.wait(parked).unwrap();
            }
        }
    }

    /// Begin wait without a specialized queue implementation.
    pub fn begin_wait(&mut self) {
        self.begin_wait_with_queue(KThreadQueue::default());
    }

    /// Unpark the host thread that is blocked in begin_wait.
    /// Called by end_wait and cancel_wait after updating state.
    pub fn unpark_wait(&self) {
        let mut parked = self.wait_park_mutex.lock().unwrap();
        *parked = false;
        self.wait_park_cv.notify_one();
    }

    /// Clear the thread's active wait queue.
    ///
    /// Matches upstream `KThread::ClearWaitQueue()` ownership.
    pub fn clear_wait_queue(&mut self) {
        self.wait_queue = None;
    }

    pub fn notify_available(
        &mut self,
        process: &mut KProcess,
        signaled_object_id: u64,
        result: u32,
    ) -> bool {
        let Some(wait_queue) = self.wait_queue else {
            return false;
        };

        wait_queue.notify_available(self, process, signaled_object_id, result)
    }

    /// End wait with a result.
    /// Matches upstream `KThread::EndWait()`.
    pub fn end_wait(&mut self, _wait_result: u32) {
        self.sleep_deadline = None;
        self.waiting_lock_info = None;
        self.clear_wait_synchronization();
        self.wait_result = _wait_result;
        self.apply_wait_result_to_context();

        let wait_queue = self
            .wait_queue
            .expect("KThread::end_wait requires wait_queue while waiting");
        wait_queue.end_wait(self, _wait_result);
    }

    /// Cancel wait.
    /// Matches upstream `KThread::CancelWait()`.
    pub fn cancel_wait(&mut self, _wait_result: u32, _cancel_timer_task: bool) {
        self.sleep_deadline = None;
        self.waiting_lock_info = None;
        self.clear_wait_synchronization();
        self.wait_result = _wait_result;
        self.apply_wait_result_to_context();

        let wait_queue = self
            .wait_queue
            .expect("KThread::cancel_wait requires wait_queue while waiting");
        wait_queue.cancel_wait(self, _wait_result, _cancel_timer_task);
    }

    /// Set the thread's activity (pause/resume).
    /// Matches upstream `KThread::SetActivity()`.
    pub fn set_activity(&mut self, activity: u32) -> u32 {
        let result = match activity {
            0 => {
                if !self.is_suspend_requested_type(SuspendType::Thread) {
                    return RESULT_INVALID_STATE.get_inner_value();
                }
                self.resume(SuspendType::Thread);
                RESULT_SUCCESS.get_inner_value()
            }
            1 => {
                let cur_state = self.get_state();
                if cur_state != ThreadState::WAITING && cur_state != ThreadState::RUNNABLE {
                    return RESULT_INVALID_STATE.get_inner_value();
                }
                if self.is_suspend_requested_type(SuspendType::Thread) {
                    return RESULT_INVALID_STATE.get_inner_value();
                }
                self.request_suspend(SuspendType::Thread);
                RESULT_SUCCESS.get_inner_value()
            }
            _ => RESULT_INVALID_STATE.get_inner_value(),
        };

        if result == RESULT_SUCCESS.get_inner_value() {
            self.request_schedule();
        }

        result
    }

    /// Sleep for the given timeout.
    /// Matches upstream `KThread::Sleep()`.
    pub fn sleep(&mut self, timeout: i64) -> u32 {
        if timeout <= 0 {
            return RESULT_INVALID_STATE.get_inner_value();
        }
        if self.is_termination_requested() {
            return RESULT_TERMINATION_REQUESTED.get_inner_value();
        }

        self.wait_result = RESULT_SUCCESS.get_inner_value();
        let timeout_ns = u64::try_from(timeout).unwrap_or(u64::MAX);
        self.sleep_deadline = Some(
            Instant::now()
                .checked_add(Duration::from_nanos(timeout_ns))
                .unwrap_or_else(|| Instant::now() + Duration::from_secs(365 * 24 * 60 * 60)),
        );
        self.set_wait_reason_for_debugging(ThreadWaitReasonForDebugging::Sleep);
        self.begin_wait();
        RESULT_SUCCESS.get_inner_value()
    }

    /// Get core mask.
    /// Matches upstream `KThread::GetCoreMask()`.
    pub fn get_core_mask(&self) -> (i32, u64) {
        (self.virtual_ideal_core_id, self.virtual_affinity_mask)
    }

    /// Set core mask.
    /// Matches upstream `KThread::SetCoreMask()`.
    pub fn set_core_mask(&mut self, cpu_core_id: i32, affinity_mask: u64) -> u32 {
        let old_virtual_ideal_core_id = self.virtual_ideal_core_id;
        let old_virtual_affinity_mask = self.virtual_affinity_mask;

        if cpu_core_id >= 0 {
            self.virtual_ideal_core_id = cpu_core_id;
            self.physical_ideal_core_id = cpu_core_id;
            self.core_id = cpu_core_id;
            self.current_core_id = cpu_core_id;
        }
        self.virtual_affinity_mask = affinity_mask;
        self.physical_affinity_mask.set_affinity_mask(affinity_mask);

        if self.virtual_ideal_core_id != old_virtual_ideal_core_id
            || self.virtual_affinity_mask != old_virtual_affinity_mask
        {
            self.request_schedule();
        }

        RESULT_SUCCESS.get_inner_value()
    }

    /// Finalize the thread.
    /// Matches upstream `KThread::Finalize()` (k_thread.cpp:333-387).
    pub fn finalize(&mut self) {
        // If the thread has an owner process, unregister it.
        if let Some(parent) = self.parent.as_ref().and_then(Weak::upgrade) {
            let mut process = parent.lock().unwrap();
            process.thread_objects.remove(&self.object_id);
            process.unregister_thread(self.thread_id);

            // If the thread has a local region, delete it.
            if self.tls_address.get() != 0 {
                process.delete_thread_local_region(self.tls_address);
            }
        }

        // Release any waiters.
        // Matches upstream KThread::Finalize() (k_thread.cpp:344-380).
        {
            debug_assert!(
                self.waiting_lock_info.is_none(),
                "thread {} has waiting_lock_info at finalize",
                self.thread_id
            );
            assert_eq!(
                self.num_kernel_waiters, 0,
                "thread {} has kernel waiters at finalize",
                self.thread_id
            );

            // Walk held_lock_info_list, cancel all waiters, free lock infos.
            while let Some(mut lock_info) = self.held_lock_info_list.pop() {
                debug_assert!(
                    !lock_info.get_is_kernel_address_key(),
                    "finalize: lock info should not have kernel address key"
                );

                // Remove all waiters from this lock.
                while lock_info.get_waiter_count() != 0 {
                    let Some(waiter_key) = lock_info.get_highest_priority_waiter() else {
                        break;
                    };
                    lock_info.remove_waiter(waiter_key.priority, waiter_key.thread_id);

                    // Cancel the waiter's wait.
                    if let Some(parent) = self.parent.as_ref().and_then(Weak::upgrade) {
                        let process = parent.lock().unwrap();
                        if let Some(waiter) = process.get_thread_by_thread_id(waiter_key.thread_id)
                        {
                            let mut waiter_guard = waiter.lock().unwrap();
                            waiter_guard.set_waiting_lock_info(None);
                            waiter_guard
                                .cancel_wait(RESULT_INVALID_STATE.get_inner_value(), true);
                        }
                    }
                }
                // lock_info is dropped here (equivalent to upstream Free).
            }
        }

        // Release host emulation members.
        // Upstream: m_host_context.reset()
        self.host_context = None;

        // Perform inherited finalization.
        // Upstream: KSynchronizationObject::Finalize()
        // Clears the synchronization object state so no dangling waiters remain.
        self.sync_object = super::k_synchronization_object::SynchronizationObjectState::new();
    }

    /// Is the thread signaled?
    /// Matches upstream `KThread::IsSignaled()` (k_thread.h).
    /// Returns true when the thread has completed termination (FinishTermination
    /// sets m_signaled = true). Used by KSynchronizationObject::Wait to determine
    /// if waiters should be woken.
    pub fn is_signaled(&self) -> bool {
        self.signaled
    }

    /// Worker task implementation.
    /// Matches upstream `KThread::DoWorkerTaskImpl()` (k_thread.cpp:450-453).
    /// Called by KWorkerTaskManager after Exit() registers the thread as a task.
    pub fn do_worker_task_impl(&mut self) {
        // Finish the termination that was begun by Exit().
        self.finish_termination();
    }

    /// OnTimer callback.
    pub fn on_timer(&mut self) {
        if self.get_state() == ThreadState::WAITING {
            self.synced_index = -1;
            self.wait_result = RESULT_TIMED_OUT.get_inner_value();
            self.cancel_wait(RESULT_TIMED_OUT.get_inner_value(), false);
        }
    }

    /// Request that this dummy thread block on next DummyThreadBeginWait.
    /// Port of upstream `KThread::RequestDummyThreadWait`.
    pub fn request_dummy_thread_wait(&self) {
        let _guard = self.dummy_thread_mutex.lock().unwrap();
        self.dummy_thread_runnable.store(false, Ordering::Relaxed);
    }

    /// Block the dummy thread until DummyThreadEndWait is called.
    /// Port of upstream `KThread::DummyThreadBeginWait`.
    pub fn dummy_thread_begin_wait(&self) {
        if !self.is_dummy_thread() {
            return;
        }

        // Block until dummy_thread_runnable becomes true.
        let guard = self.dummy_thread_mutex.lock().unwrap();
        let _guard = self
            .dummy_thread_cv
            .wait_while(guard, |_| {
                !self.dummy_thread_runnable.load(Ordering::Relaxed)
            })
            .unwrap();
    }

    /// Wake the dummy thread from DummyThreadBeginWait.
    /// Port of upstream `KThread::DummyThreadEndWait`.
    pub fn dummy_thread_end_wait(&self) {
        {
            let _guard = self.dummy_thread_mutex.lock().unwrap();
            self.dummy_thread_runnable.store(true, Ordering::Relaxed);
        }
        self.dummy_thread_cv.notify_one();
    }

    /// Set condition variable state.
    /// Upstream: ASSERT(m_waiting_lock_info == nullptr).
    pub fn set_condition_variable(
        &mut self,
        address: KProcessAddress,
        cv_key: u64,
        value: u32,
    ) {
        debug_assert!(
            self.waiting_lock_info.is_none(),
            "set_condition_variable: m_waiting_lock_info must be null"
        );
        self.condvar_tree_state = ConditionVariableTreeState::ConditionVariable;
        self.condvar_key = cv_key;
        self.address_key = address;
        self.address_key_value = value;
        self.is_kernel_address_key = false;
    }

    /// Clear condition variable state.
    pub fn clear_condition_variable(&mut self) {
        self.condvar_tree_state = ConditionVariableTreeState::None;
    }

    pub fn is_waiting_for_condition_variable(&self) -> bool {
        self.condvar_tree_state == ConditionVariableTreeState::ConditionVariable
    }

    pub fn is_waiting_for_address_arbiter(&self) -> bool {
        self.condvar_tree_state == ConditionVariableTreeState::AddressArbiter
    }

    /// Set address arbiter state.
    /// Upstream: ASSERT(m_waiting_lock_info == nullptr).
    pub fn set_address_arbiter(&mut self, address: u64) {
        debug_assert!(
            self.waiting_lock_info.is_none(),
            "set_address_arbiter: m_waiting_lock_info must be null"
        );
        self.condvar_tree_state = ConditionVariableTreeState::AddressArbiter;
        self.condvar_key = address;
    }

    /// Clear address arbiter state.
    pub fn clear_address_arbiter(&mut self) {
        self.condvar_tree_state = ConditionVariableTreeState::None;
    }

    pub fn condition_variable_tree_key(&self) -> ConditionVariableThreadKey {
        ConditionVariableThreadKey {
            cv_key: self.condvar_key,
            priority: self.priority,
            thread_id: self.thread_id,
        }
    }

    /// Set the waiting lock info (which lock this thread is blocked on).
    /// Replaces the old `set_waiting_lock_owner_thread_id`.
    /// Pass `None` to clear (thread is no longer waiting on a lock).
    pub fn set_waiting_lock_owner_thread_id(&mut self, owner_thread_id: Option<u64>) {
        match owner_thread_id {
            Some(id) => {
                self.waiting_lock_info = Some(WaitingLockRef {
                    owner_thread_id: id,
                    address_key: self.address_key,
                    is_kernel_address_key: self.is_kernel_address_key,
                });
            }
            None => {
                self.waiting_lock_info = None;
            }
        }
    }

    /// Get the lock owner thread.
    /// Matches upstream `KThread::GetLockOwner()` (k_thread.cpp:732-734).
    pub fn get_lock_owner(&self) -> Option<Arc<Mutex<KThread>>> {
        let lock_ref = self.waiting_lock_info.as_ref()?;
        let parent = self.parent.as_ref()?.upgrade()?;
        let process = parent.lock().unwrap();
        process.get_thread_by_thread_id(lock_ref.owner_thread_id)
    }

    /// Get the lock owner thread ID (without looking up the thread).
    pub fn get_lock_owner_thread_id(&self) -> Option<u64> {
        self.waiting_lock_info.as_ref().map(|r| r.owner_thread_id)
    }

    pub fn has_wait_queue(&self) -> bool {
        self.wait_queue.is_some()
    }

    /// Get the timer task time (upstream KTimerTask::GetTime()).
    pub fn get_timer_task_time(&self) -> i64 {
        self.timer_task_time
    }

    /// Set the timer task time (upstream KTimerTask::SetTime()).
    pub fn set_timer_task_time(&mut self, time: i64) {
        self.timer_task_time = time;
    }

    /// Continue if has kernel waiters.
    pub fn continue_if_has_kernel_waiters(&mut self) {
        if self.get_num_kernel_waiters() > 0 {
            self.continue_thread();
        }
    }
}

impl Default for KThread {
    fn default() -> Self {
        Self::new()
    }
}

impl super::k_priority_queue::KPriorityQueueMember for KThread {
    fn get_priority_queue_entry(&self, core: i32) -> &QueueEntry {
        &self.per_core_priority_queue_entry[core as usize]
    }

    fn get_priority_queue_entry_mut(&mut self, core: i32) -> &mut QueueEntry {
        &mut self.per_core_priority_queue_entry[core as usize]
    }

    fn get_affinity_mask_value(&self) -> u64 {
        self.physical_affinity_mask.get_affinity_mask()
    }

    fn get_active_core(&self) -> i32 {
        self.core_id
    }

    fn get_priority(&self) -> i32 {
        self.priority
    }

    fn is_dummy_thread(&self) -> bool {
        self.thread_type == ThreadType::Dummy
    }
}

// HasRBEntry impl removed: condvar_arbiter_tree_node field is kept for
// structural parity with upstream m_condvar_arbiter_tree_node, but we use
// BTreeSet<ConditionVariableThreadKey> externally rather than an intrusive
// red-black tree through this node. The impl can be restored if/when we
// switch to an intrusive tree.

/// RAII guard that disables dispatch on construction and enables on destruction.
/// Matches upstream `KScopedDisableDispatch` (k_thread.h).
///
/// On construction: increments current thread's disable_dispatch_count.
/// On destruction: if count would reach 0, triggers RescheduleCurrentCore
/// (or RescheduleCurrentHLEThread for phantom/single-core mode).
/// Otherwise just decrements.
pub struct KScopedDisableDispatch {
    thread: Arc<Mutex<KThread>>,
}

impl KScopedDisableDispatch {
    /// Create a new scoped disable dispatch guard.
    /// Upstream takes `KernelCore&` and uses `GetCurrentThread(kernel)`.
    /// We take the thread directly.
    pub fn new(thread: &Arc<Mutex<KThread>>) -> Self {
        thread.lock().unwrap().disable_dispatch();
        Self {
            thread: thread.clone(),
        }
    }
}

impl Drop for KScopedDisableDispatch {
    fn drop(&mut self) {
        // Upstream: ~KScopedDisableDispatch() (k_thread.cpp:1429-1446)
        // If shutting down, do nothing.
        // Otherwise, if dispatch count is 1, reschedule; if > 1, just enable.

        let thread = self.thread.lock().unwrap();
        if thread.get_disable_dispatch_count() <= 1 {
            drop(thread);
            // Upstream: scheduler->RescheduleCurrentCore() if scheduler exists
            // and not phantom mode; otherwise RescheduleCurrentHLEThread.
            // Use the HLE reschedule path since we don't have kernel context here.
            KScheduler::reschedule_current_hle_thread();
        } else {
            drop(thread);
            self.thread.lock().unwrap().enable_dispatch();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hle::kernel::k_process::KProcess;
    use crate::hle::kernel::k_scheduler::KScheduler;
    use crate::hle::result::RESULT_SUCCESS;
    use std::sync::{Arc, Mutex};

    #[test]
    fn test_thread_state_values() {
        assert_eq!(ThreadState::WAITING.bits(), 1);
        assert_eq!(ThreadState::RUNNABLE.bits(), 2);
        assert_eq!(ThreadState::TERMINATED.bits(), 3);
        assert_eq!(ThreadState::MASK.bits(), 0xF);
        assert_eq!(ThreadState::PROCESS_SUSPENDED.bits(), 1 << 4);
        assert_eq!(ThreadState::THREAD_SUSPENDED.bits(), 1 << 5);
        assert_eq!(ThreadState::DEBUG_SUSPENDED.bits(), 1 << 6);
        assert_eq!(ThreadState::BACKTRACE_SUSPENDED.bits(), 1 << 7);
        assert_eq!(ThreadState::INIT_SUSPENDED.bits(), 1 << 8);
        assert_eq!(ThreadState::SYSTEM_SUSPENDED.bits(), 1 << 9);
    }

    #[test]
    fn test_thread_type_values() {
        assert_eq!(ThreadType::Main as u32, 0);
        assert_eq!(ThreadType::Kernel as u32, 1);
        assert_eq!(ThreadType::HighPriority as u32, 2);
        assert_eq!(ThreadType::User as u32, 3);
        assert_eq!(ThreadType::Dummy as u32, 100);
    }

    #[test]
    fn test_suspend_type_values() {
        assert_eq!(SuspendType::Process as u32, 0);
        assert_eq!(SuspendType::Thread as u32, 1);
        assert_eq!(SuspendType::Debug as u32, 2);
        assert_eq!(SuspendType::Backtrace as u32, 3);
        assert_eq!(SuspendType::Init as u32, 4);
        assert_eq!(SuspendType::System as u32, 5);
    }

    #[test]
    fn test_dpc_flag_values() {
        assert_eq!(DpcFlag::TERMINATING.bits(), 1);
        assert_eq!(DpcFlag::TERMINATED.bits(), 2);
    }

    #[test]
    fn test_default_thread() {
        let thread = KThread::new();
        assert_eq!(thread.get_priority(), 0);
        assert_eq!(thread.get_thread_id(), 0);
        assert!(!thread.is_initialized());
        assert!(!thread.is_dummy_thread());
    }

    #[test]
    fn test_request_suspend_sets_suspend_bits_without_changing_base_state() {
        let mut thread = KThread::new();
        thread.set_state(ThreadState::RUNNABLE);

        thread.request_suspend(SuspendType::Thread);

        assert_eq!(thread.get_state(), ThreadState::RUNNABLE);
        assert!(thread.is_suspend_requested_type(SuspendType::Thread));
        assert!(thread.is_suspended());
        assert!(thread.get_raw_state().contains(ThreadState::THREAD_SUSPENDED));
    }

    #[test]
    fn test_wait_cancel_marks_wait_cancelled_when_not_cancellable() {
        let mut thread = KThread::new();
        thread.begin_wait();

        thread.wait_cancel();

        assert!(thread.is_wait_cancelled());
        assert_eq!(thread.get_state(), ThreadState::WAITING);
    }

    #[test]
    fn test_wait_cancel_resumes_cancellable_waiting_thread() {
        let mut thread = KThread::new();
        thread.begin_wait();
        thread.set_cancellable();

        thread.wait_cancel();

        assert_eq!(thread.get_state(), ThreadState::RUNNABLE);
        assert!(!thread.is_wait_cancelled());
    }

    #[test]
    fn test_on_timer_wakes_sleeping_thread() {
        let mut thread = KThread::new();

        assert_eq!(thread.sleep(1), RESULT_SUCCESS.get_inner_value());
        assert_eq!(thread.get_state(), ThreadState::WAITING);
        assert!(thread.get_sleep_deadline().is_some());

        thread.on_timer();

        assert_eq!(thread.get_state(), ThreadState::RUNNABLE);
        assert!(thread.get_sleep_deadline().is_none());
        assert_eq!(thread.get_wait_result(), RESULT_TIMED_OUT.get_inner_value());
    }

    #[test]
    fn test_state_transition_requests_schedule_via_parent_process() {
        let process = Arc::new(Mutex::new(KProcess::new()));
        let scheduler = Arc::new(Mutex::new(KScheduler::new(0)));
        process.lock().unwrap().attach_scheduler(&scheduler);

        let mut thread = KThread::new();
        thread.thread_id = 1;
        thread.parent = Some(Arc::downgrade(&process));
        thread.scheduler = Some(Arc::downgrade(&scheduler));
        thread.set_state(ThreadState::RUNNABLE);
        scheduler
            .lock()
            .unwrap()
            .state
            .needs_scheduling
            .store(false, Ordering::Relaxed);

        thread.begin_wait();

        assert!(scheduler.lock().unwrap().needs_scheduling());
    }

    #[test]
    fn test_priority_change_requests_schedule_via_parent_process() {
        let process = Arc::new(Mutex::new(KProcess::new()));
        let scheduler = Arc::new(Mutex::new(KScheduler::new(0)));
        process.lock().unwrap().attach_scheduler(&scheduler);

        let mut thread = KThread::new();
        thread.thread_id = 1;
        thread.parent = Some(Arc::downgrade(&process));
        thread.scheduler = Some(Arc::downgrade(&scheduler));
        thread.priority = 44;
        thread.base_priority = 44;
        scheduler
            .lock()
            .unwrap()
            .state
            .needs_scheduling
            .store(false, Ordering::Relaxed);

        thread.set_base_priority(30);

        assert!(scheduler.lock().unwrap().needs_scheduling());
    }

    #[test]
    fn test_add_held_lock_transfers_kernel_waiter_count() {
        let mut thread = KThread::new();
        thread.thread_id = 7;

        let mut lock_info = LockWithPriorityInheritanceInfo::new(KProcessAddress::new(0x4000), true);
        lock_info.add_waiter(3, 10);
        lock_info.add_waiter(5, 11);

        thread.add_held_lock(lock_info);

        assert_eq!(thread.get_num_kernel_waiters(), 2);
        assert_eq!(thread.held_lock_info_list.len(), 1);
        assert_eq!(thread.held_lock_info_list[0].get_owner_thread_id(), 7);
    }

    #[test]
    fn test_remove_waiter_by_thread_id_restores_priority() {
        let mut thread = KThread::new();
        thread.thread_id = 1;
        thread.base_priority = 10;
        thread.priority = 3;

        let mut lock_info =
            LockWithPriorityInheritanceInfo::new(KProcessAddress::new(0x5000), false);
        lock_info.add_waiter(3, 2);
        thread.add_held_lock(lock_info);

        thread.remove_waiter_by_thread_id(2);

        assert_eq!(thread.priority, 10);
        assert!(thread.held_lock_info_list.is_empty());
    }

    #[test]
    fn test_finalize_unregisters_thread_object_from_owner_process() {
        let process = Arc::new(Mutex::new(KProcess::new()));
        let thread = Arc::new(Mutex::new(KThread::new()));

        {
            let mut guard = thread.lock().unwrap();
            guard.thread_id = 11;
            guard.object_id = 22;
            guard.parent = Some(Arc::downgrade(&process));
        }
        process.lock().unwrap().register_thread_object(thread.clone());

        thread.lock().unwrap().finalize();

        let process_guard = process.lock().unwrap();
        assert!(process_guard.get_thread_by_object_id(22).is_none());
        assert!(!process_guard.thread_list.contains(&11));
    }

    #[test]
    fn terminate_thread_waits_for_finish_termination_signal() {
        use std::sync::atomic::{AtomicBool, Ordering};
        use std::thread;
        use std::time::Duration;

        let target = Arc::new(Mutex::new(KThread::new()));
        {
            let mut guard = target.lock().unwrap();
            guard.object_id = 44;
            guard.thread_id = 7;
            guard.bind_self_reference(&target);
            guard.set_state(ThreadState::RUNNABLE);
        }

        let completed = Arc::new(AtomicBool::new(false));
        let completed_clone = completed.clone();
        let target_clone = target.clone();
        let waiter = thread::spawn(move || {
            let result = KThread::terminate_thread(&target_clone);
            assert_eq!(result, RESULT_SUCCESS.get_inner_value());
            completed_clone.store(true, Ordering::SeqCst);
        });

        thread::sleep(Duration::from_millis(10));
        assert!(!completed.load(Ordering::SeqCst));

        target.lock().unwrap().exit();

        waiter.join().unwrap();
        assert!(completed.load(Ordering::SeqCst));
        assert!(target.lock().unwrap().is_signaled());
    }

    #[test]
    fn request_terminate_runnable_thread_requests_interrupt_reschedule() {
        let scheduler = Arc::new(Mutex::new(KScheduler::new(0)));
        let mut thread = KThread::new();
        thread.thread_id = 9;
        thread.scheduler = Some(Arc::downgrade(&scheduler));
        thread.set_state(ThreadState::RUNNABLE);

        scheduler
            .lock()
            .unwrap()
            .state
            .needs_scheduling
            .store(false, Ordering::Relaxed);

        thread.request_terminate();

        assert!(scheduler.lock().unwrap().needs_scheduling());
    }

    #[test]
    fn test_core_mask_change_requests_schedule_via_thread_scheduler() {
        let process = Arc::new(Mutex::new(KProcess::new()));
        let scheduler = Arc::new(Mutex::new(KScheduler::new(0)));
        process.lock().unwrap().attach_scheduler(&scheduler);

        let mut thread = KThread::new();
        thread.thread_id = 1;
        thread.parent = Some(Arc::downgrade(&process));
        thread.scheduler = Some(Arc::downgrade(&scheduler));
        thread.virtual_ideal_core_id = 0;
        thread.virtual_affinity_mask = 0x1;
        scheduler
            .lock()
            .unwrap()
            .state
            .needs_scheduling
            .store(false, Ordering::Relaxed);

        assert_eq!(thread.set_core_mask(1, 0x2), RESULT_SUCCESS.get_inner_value());
        assert!(scheduler.lock().unwrap().needs_scheduling());
    }

    #[test]
    fn test_activity_change_requests_schedule_via_thread_scheduler() {
        let process = Arc::new(Mutex::new(KProcess::new()));
        let scheduler = Arc::new(Mutex::new(KScheduler::new(0)));
        process.lock().unwrap().attach_scheduler(&scheduler);

        let mut thread = KThread::new();
        thread.thread_id = 1;
        thread.parent = Some(Arc::downgrade(&process));
        thread.scheduler = Some(Arc::downgrade(&scheduler));
        thread.set_state(ThreadState::RUNNABLE);
        scheduler
            .lock()
            .unwrap()
            .state
            .needs_scheduling
            .store(false, Ordering::Relaxed);

        assert_eq!(thread.set_activity(1), RESULT_SUCCESS.get_inner_value());
        assert!(scheduler.lock().unwrap().needs_scheduling());
    }
}
