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
/// TODO: Port from k_affinity_mask.h.
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

    // Lock with priority inheritance
    // m_held_lock_info_list — stubbed
    // m_waiting_lock_info — stubbed

    pub address_key_value: u32,
    pub waiting_lock_owner_thread_id: Option<u64>,
    pub user_waiter_thread_ids: Vec<u64>,
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
    pub initialized: bool,
    pub debug_attached: bool,
    pub priority_inheritance_count: i8,
    pub resource_limit_release_hint: bool,
    pub is_kernel_address_key: bool,
    pub stack_parameters: StackParameters,

    // Emulation fields
    // m_host_context — fiber; stubbed
    pub thread_type: ThreadType,
    pub step_state: StepState,
    pub dummy_thread_runnable: bool,
    pub dummy_thread_mutex: Mutex<()>,
    pub dummy_thread_cv: Condvar,

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
            address_key_value: 0,
            waiting_lock_owner_thread_id: None,
            user_waiter_thread_ids: Vec::new(),
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
            initialized: false,
            debug_attached: false,
            priority_inheritance_count: 0,
            resource_limit_release_hint: false,
            is_kernel_address_key: false,
            stack_parameters: StackParameters::default(),
            thread_type: ThreadType::User,
            step_state: StepState::default(),
            dummy_thread_runnable: true,
            dummy_thread_mutex: Mutex::new(()),
            dummy_thread_cv: Condvar::new(),
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

    pub fn add_waiter(&mut self, thread_id: u64) {
        if self.user_waiter_thread_ids.contains(&thread_id) {
            return;
        }
        self.user_waiter_thread_ids.push(thread_id);
        self.num_kernel_waiters += 1;
    }

    pub fn remove_waiter(&mut self, thread_id: u64) {
        if let Some(index) = self
            .user_waiter_thread_ids
            .iter()
            .position(|candidate| *candidate == thread_id)
        {
            self.user_waiter_thread_ids.remove(index);
            self.num_kernel_waiters = self.num_kernel_waiters.saturating_sub(1);
        }
    }

    pub fn remove_user_waiter_by_key(
        &mut self,
        process: &KProcess,
        address: KProcessAddress,
        has_waiters: &mut bool,
    ) -> Option<u64> {
        let mut index = 0usize;
        while index < self.user_waiter_thread_ids.len() {
            let waiter_thread_id = self.user_waiter_thread_ids[index];
            let Some(waiter_thread) = process.get_thread_by_thread_id(waiter_thread_id) else {
                self.user_waiter_thread_ids.remove(index);
                self.num_kernel_waiters = self.num_kernel_waiters.saturating_sub(1);
                continue;
            };

            if waiter_thread.lock().unwrap().get_address_key() != address {
                index += 1;
                continue;
            }

            self.user_waiter_thread_ids.remove(index);
            self.num_kernel_waiters = self.num_kernel_waiters.saturating_sub(1);
            *has_waiters = self.user_waiter_thread_ids.iter().filter_map(|candidate_id| {
                process.get_thread_by_thread_id(*candidate_id)
            }).any(|thread| thread.lock().unwrap().get_address_key() == address);
            return Some(waiter_thread_id);
        }

        *has_waiters = false;
        None
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
        self.thread_context = ThreadContext::default();
        self.thread_context.r[0] = arg;
        self.thread_context.r[13] = stack_top;
        self.thread_context.r[15] = entry_point;
        self.thread_context.sp = stack_top;
        self.thread_context.pc = entry_point;
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
        self.thread_context.tpidr = tls_address;
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
        self.thread_context.tpidr = tls_address.get();

        RESULT_SUCCESS.get_inner_value()
    }

    pub fn clone_fpu_status_from(&mut self, current: &KThread) {
        self.thread_context.fpcr = current.thread_context.fpcr;
        self.thread_context.fpsr = current.thread_context.fpsr;
    }

    /// Set the thread's base priority with priority inheritance handling.
    /// TODO: Port full implementation from k_thread.cpp.
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
    /// TODO: Port from k_thread.cpp.
    pub fn run(&mut self) -> u32 {
        if self.termination_requested.load(Ordering::Relaxed) {
            return RESULT_TERMINATION_REQUESTED.get_inner_value();
        }
        if self.get_state() != ThreadState::INITIALIZED {
            return RESULT_INVALID_STATE.get_inner_value();
        }
        self.set_state(ThreadState::RUNNABLE);
        RESULT_SUCCESS.get_inner_value()
    }

    /// Exit the thread.
    /// TODO: Port from k_thread.cpp.
    pub fn exit(&mut self) {
        self.termination_requested.store(true, Ordering::Relaxed);
        self.sleep_deadline = None;
        self.clear_wait_synchronization();
        self.set_state(ThreadState::TERMINATED);
        self.signaled = true;
        self.wait_queue = None;

        if let Some(parent) = self.parent.as_ref().and_then(Weak::upgrade) {
            let mut process = parent.lock().unwrap();
            // Thread leaving RUNNABLE (or any state) → remove from PQ.
            process.remove_from_priority_queue(self.thread_id);
            process.clear_running_thread(self.thread_id);
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
    }

    /// Terminate the thread.
    /// TODO: Port from k_thread.cpp.
    pub fn terminate(&mut self) -> u32 {
        // TODO: Full implementation
        0
    }

    /// Request termination of the thread.
    /// TODO: Port from k_thread.cpp.
    pub fn request_terminate(&mut self) -> ThreadState {
        // TODO: Full implementation
        self.get_state()
    }

    /// Request suspend of the given type.
    /// TODO: Port from k_thread.cpp.
    pub fn request_suspend(&mut self, suspend_type: SuspendType) {
        let bit = 1u32 << (ThreadState::SUSPEND_SHIFT as u32 + suspend_type as u32);
        self.suspend_request_flags |= bit;
        self.try_suspend();
    }

    /// Resume from the given suspend type.
    /// TODO: Port from k_thread.cpp.
    pub fn resume(&mut self, suspend_type: SuspendType) {
        let bit = 1u32 << (ThreadState::SUSPEND_SHIFT as u32 + suspend_type as u32);
        self.suspend_request_flags &= !bit;
        self.update_state();
    }

    /// Try to suspend the thread.
    /// TODO: Port from k_thread.cpp.
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
    /// TODO: Port from k_thread.cpp.
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
    /// TODO: Port from k_thread.cpp.
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
    /// TODO: Port from k_thread.cpp.
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
    /// TODO: Port from k_thread.cpp.
    pub fn pin(&mut self, _current_core: i32) {
        // TODO: Full implementation
    }

    /// Unpin the thread.
    /// TODO: Port from k_thread.cpp.
    pub fn unpin(&mut self) {
        // TODO: Full implementation
    }

    /// Wait cancel.
    /// TODO: Port from k_thread.cpp.
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
    /// sets state to Waiting, then assigns the wait queue.
    pub fn begin_wait_with_queue(&mut self, wait_queue: KThreadQueue) {
        self.set_state(ThreadState::WAITING);
        self.wait_queue = Some(wait_queue);
    }

    /// Begin wait without a specialized queue implementation.
    pub fn begin_wait(&mut self) {
        self.begin_wait_with_queue(KThreadQueue::default());
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
    /// TODO: Port from k_thread.cpp.
    pub fn end_wait(&mut self, _wait_result: u32) {
        self.sleep_deadline = None;
        self.waiting_lock_owner_thread_id = None;
        self.clear_wait_synchronization();
        self.wait_result = _wait_result;
        self.apply_wait_result_to_context();

        let wait_queue = self
            .wait_queue
            .expect("KThread::end_wait requires wait_queue while waiting");
        wait_queue.end_wait(self, _wait_result);
    }

    /// Cancel wait.
    /// TODO: Port from k_thread.cpp.
    pub fn cancel_wait(&mut self, _wait_result: u32, _cancel_timer_task: bool) {
        self.sleep_deadline = None;
        self.waiting_lock_owner_thread_id = None;
        self.clear_wait_synchronization();
        self.wait_result = _wait_result;
        self.apply_wait_result_to_context();

        let wait_queue = self
            .wait_queue
            .expect("KThread::cancel_wait requires wait_queue while waiting");
        wait_queue.cancel_wait(self, _wait_result, _cancel_timer_task);
    }

    /// Set the thread's activity (pause/resume).
    /// TODO: Port from k_thread.cpp.
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
    /// TODO: Port from k_thread.cpp.
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
    /// TODO: Port from k_thread.cpp.
    pub fn get_core_mask(&self) -> (i32, u64) {
        (self.virtual_ideal_core_id, self.virtual_affinity_mask)
    }

    /// Set core mask.
    /// TODO: Port from k_thread.cpp.
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
    /// TODO: Port from k_thread.cpp.
    pub fn finalize(&mut self) {
        // TODO: Full implementation
    }

    /// Is the thread signaled?
    /// TODO: Port from k_thread.cpp.
    pub fn is_signaled(&self) -> bool {
        self.signaled
    }

    /// OnTimer callback.
    pub fn on_timer(&mut self) {
        if self.get_state() == ThreadState::WAITING {
            self.synced_index = -1;
            self.wait_result = RESULT_TIMED_OUT.get_inner_value();
            self.cancel_wait(RESULT_TIMED_OUT.get_inner_value(), false);
        }
    }

    /// Dummy thread request wait.
    pub fn request_dummy_thread_wait(&mut self) {
        // TODO: Full implementation
    }

    /// Dummy thread begin wait.
    pub fn dummy_thread_begin_wait(&self) {
        // TODO: Full implementation with condvar
    }

    /// Dummy thread end wait.
    pub fn dummy_thread_end_wait(&self) {
        // TODO: Full implementation with condvar
    }

    /// Set condition variable state.
    /// Upstream: ASSERT(m_waiting_lock_info == nullptr) — checks
    /// LockWithPriorityInheritanceInfo, not waiting_lock_owner_thread_id.
    /// TODO: add assertion once LockWithPriorityInheritanceInfo is ported.
    pub fn set_condition_variable(
        &mut self,
        address: KProcessAddress,
        cv_key: u64,
        value: u32,
    ) {
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
    /// Upstream: ASSERT(m_waiting_lock_info == nullptr) — checks
    /// LockWithPriorityInheritanceInfo, not waiting_lock_owner_thread_id.
    /// TODO: add assertion once LockWithPriorityInheritanceInfo is ported.
    pub fn set_address_arbiter(&mut self, address: u64) {
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

    pub fn set_waiting_lock_owner_thread_id(&mut self, owner_thread_id: Option<u64>) {
        self.waiting_lock_owner_thread_id = owner_thread_id;
    }

    pub fn get_lock_owner(&self) -> Option<Arc<Mutex<KThread>>> {
        let owner_thread_id = self.waiting_lock_owner_thread_id?;
        let parent = self.parent.as_ref()?.upgrade()?;
        let process = parent.lock().unwrap();
        process.get_thread_by_thread_id(owner_thread_id)
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

    pub fn waiter_thread_ids(&self) -> &[u64] {
        &self.user_waiter_thread_ids
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
