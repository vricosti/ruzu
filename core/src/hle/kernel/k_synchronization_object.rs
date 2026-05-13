//! Port of zuyu/src/core/hle/kernel/k_synchronization_object.{h,cpp}
//! Status: COMPLET (upstream-faithful raw-pointer intrusive list)
//! Derniere synchro: 2026-04-19
//!
//! KSynchronizationObject — base class for kernel objects that threads can wait
//! on. Extends KAutoObjectWithList.
//!
//! Waiter storage mirrors upstream: an intrusive linked list of
//! `ThreadListNode` structures is stored per sync object; each node is owned by
//! the waiting thread's `Wait()` stack frame. Traversal/signal paths only hold
//! the scheduler lock — they do NOT need `&KProcess`.

use std::marker::PhantomPinned;
use std::ptr;
use std::sync::{Arc, Mutex, Weak};

use super::k_auto_object::{KAutoObjectBase, KAutoObjectWithList, TypeObj};
use super::k_class_token;
use super::k_port::KPort;
use super::k_process::{KProcess, ProcessLock};
use super::k_readable_event::KReadableEvent;
use super::k_scheduler::KScheduler;
use super::k_scoped_scheduler_lock_and_sleep::KScopedSchedulerLockAndSleep;
use super::k_server_session::KServerSession;
use super::k_thread::{KThread, KThreadLock};
use super::k_thread_queue::{KThreadQueue, KThreadQueueWithoutEndWait};
use crate::hle::kernel::svc::svc_results::{
    RESULT_CANCELLED, RESULT_INVALID_HANDLE, RESULT_TERMINATION_REQUESTED, RESULT_TIMED_OUT,
};
use crate::hle::result::ResultCode;

/// Maximum number of sync objects a single Wait() can target.
/// Upstream: `Svc::ArgumentHandleCountMax == 64`.
pub const ARGUMENT_HANDLE_COUNT_MAX: usize = 64;

/// Intrusive linked-list node used by `KSynchronizationObject` to track waiting
/// threads. Mirrors upstream `KSynchronizationObject::ThreadListNode`.
///
/// Each node is stored in a per-wait buffer owned by `KThread::wait_nodes`
/// (allocated by `wait()` before linking, cleared on wake). The signal path
/// dereferences `thread` as `Weak<KThreadLock>` and upgrades under the
/// scheduler lock.
pub struct ThreadListNode {
    pub next: *mut ThreadListNode,
    /// Weak ref to the waiter. Upgraded on signal to call notify_available.
    pub thread: Weak<KThreadLock>,
    /// Object id this node is linked into — used to compute the synced_index
    /// in the queue callback when the thread is notified.
    pub object_id: u64,
    _pin: PhantomPinned,
}

impl ThreadListNode {
    pub fn new() -> Self {
        Self {
            next: ptr::null_mut(),
            thread: Weak::new(),
            object_id: 0,
            _pin: PhantomPinned,
        }
    }
}

// Safety: raw next pointer and object_id are only read/written under the
// scheduler lock; Weak<KThreadLock> is Send+Sync. Nodes are otherwise
// owned by a single waiter thread.
unsafe impl Send for ThreadListNode {}
unsafe impl Sync for ThreadListNode {}

/// Trait for objects that can be signaled and waited on.
/// Mirrors the pure virtual `IsSignaled()` in upstream.
pub trait KSynchronizable {
    fn is_signaled(&self) -> bool;
}

/// Waiter list state embedded in every waitable kernel object.
/// Upstream: the raw pointer fields on `KSynchronizationObject` itself.
///
/// Every mutation and traversal must happen with the scheduler lock held.
pub struct SynchronizationObjectState {
    head: *mut ThreadListNode,
    tail: *mut ThreadListNode,
}

impl SynchronizationObjectState {
    pub const fn new() -> Self {
        Self {
            head: ptr::null_mut(),
            tail: ptr::null_mut(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.head.is_null()
    }

    /// Diagnostic — pointer value of head, for cross-referencing wait/notify
    /// pairs in trace logs.
    pub fn head_addr(&self) -> *const ThreadListNode {
        self.head
    }

    /// Diagnostic — pointer value of tail.
    pub fn tail_addr(&self) -> *const ThreadListNode {
        self.tail
    }

    /// Link a ThreadListNode to the tail. Mirrors upstream `LinkNode`.
    ///
    /// # Safety
    /// - Caller holds the scheduler lock for the owning kernel.
    /// - `node` is a valid pointer that remains alive until `unlink_node`.
    pub unsafe fn link_node(&mut self, node: *mut ThreadListNode) {
        debug_assert!(!node.is_null());
        (*node).next = ptr::null_mut();
        if self.tail.is_null() {
            self.head = node;
        } else {
            (*self.tail).next = node;
        }
        self.tail = node;
    }

    /// Unlink a ThreadListNode. Mirrors upstream `UnlinkNode`.
    ///
    /// # Safety
    /// - Caller holds the scheduler lock for the owning kernel.
    /// - `node` is in this list.
    pub unsafe fn unlink_node(&mut self, node: *mut ThreadListNode) {
        debug_assert!(!node.is_null());
        let mut prev: *mut ThreadListNode = ptr::null_mut();
        let mut cur = self.head;
        while !cur.is_null() && cur != node {
            prev = cur;
            cur = (*cur).next;
        }
        if cur.is_null() {
            // Not found — upstream would UB, we no-op to stay safe.
            debug_assert!(false, "unlink_node: node not found in list");
            return;
        }
        let next = (*cur).next;
        if prev.is_null() {
            self.head = next;
        } else {
            (*prev).next = next;
        }
        if self.tail == cur {
            self.tail = prev;
        }
        (*cur).next = ptr::null_mut();
    }

    /// Walk the list under the scheduler lock and collect strong refs to the
    /// waiters. Dead (dropped) threads are filtered out.
    ///
    /// # Safety
    /// Caller must hold the scheduler lock.
    pub unsafe fn waiter_snapshot(&self) -> Vec<Arc<KThreadLock>> {
        let mut v = Vec::new();
        let mut cur = self.head;
        while !cur.is_null() {
            if let Some(t) = (*cur).thread.upgrade() {
                v.push(t);
            }
            cur = (*cur).next;
        }
        v
    }
}

// Safety: list pointers are only mutated under the scheduler lock; nodes
// themselves are Send+Sync.
unsafe impl Send for SynchronizationObjectState {}
unsafe impl Sync for SynchronizationObjectState {}

impl Default for SynchronizationObjectState {
    fn default() -> Self {
        Self::new()
    }
}

/// Per-thread record of which objects a Wait() is targeting plus the raw
/// pointers needed by the wake callback to unlink nodes from other objects.
///
/// The `nodes` buffer owns the `ThreadListNode`s linked into sync objects;
/// pointers into it must remain stable, so it's a `Box<[ThreadListNode]>`.
///
/// `object_states` holds raw pointers to each `SynchronizationObjectState` the
/// thread is linked into. The referenced states live inside kernel objects
/// held by `Arc`s elsewhere; the pointers are valid only while the scheduler
/// lock is held AND the wait is active (i.e. thread is in WAITING state).
pub struct SynchronizationWaitContext {
    pub nodes: Box<[ThreadListNode]>,
    pub object_ids: Vec<u64>,
    /// Raw pointers to the SynchronizationObjectState each node is linked into.
    /// SAFETY: only dereferenced under the scheduler lock.
    pub object_states: Vec<*mut SynchronizationObjectState>,
    pub active: bool,
}

impl SynchronizationWaitContext {
    pub fn new() -> Self {
        Self {
            nodes: Box::new([]),
            object_ids: Vec::new(),
            object_states: Vec::new(),
            active: false,
        }
    }

    pub fn is_active(&self) -> bool {
        self.active
    }

    pub fn object_ids(&self) -> &[u64] {
        &self.object_ids
    }

    pub fn clear(&mut self) {
        self.nodes = Box::new([]);
        self.object_ids.clear();
        self.object_states.clear();
        self.active = false;
    }

    /// Match a signaled object_id against the object list; return wait_index.
    pub fn synced_index_for(&self, signaled_object_id: u64) -> Option<usize> {
        self.object_ids
            .iter()
            .position(|&oid| oid == signaled_object_id)
    }
}

// Safety: the raw pointers are only dereferenced under the scheduler lock;
// the struct is otherwise owned by one KThread.
unsafe impl Send for SynchronizationWaitContext {}
unsafe impl Sync for SynchronizationWaitContext {}

impl Default for SynchronizationWaitContext {
    fn default() -> Self {
        Self::new()
    }
}

/// The queue callback that runs when any of the wait-targeted sync objects
/// signals the waiting thread. Mirrors upstream
/// `ThreadQueueImplForKSynchronizationObjectWait`.
pub(crate) struct ThreadQueueImplForKSynchronizationObjectWait;

impl ThreadQueueImplForKSynchronizationObjectWait {
    pub(crate) fn queue() -> KThreadQueue {
        KThreadQueueWithoutEndWait::with_callbacks(
            Some(Self::notify_available),
            Some(Self::cancel_wait),
        )
        .base
    }

    /// Upstream: iterate the wait's nodes, find synced_index, unlink all nodes,
    /// set synced_index on thread, clear cancellable, base EndWait.
    fn notify_available(
        wait_queue: &KThreadQueue,
        thread: &mut KThread,
        signaled_object_id: u64,
        wait_result: u32,
    ) -> bool {
        if !thread.sync_wait_context.is_active() {
            return false;
        }

        // Compute synced_index.
        let synced_index = thread
            .sync_wait_context
            .synced_index_for(signaled_object_id)
            .map(|i| i as i32)
            .unwrap_or(-1);

        // Unlink every node from its object, under scheduler lock.
        unsafe {
            let ctx = &mut thread.sync_wait_context;
            debug_assert_eq!(ctx.nodes.len(), ctx.object_states.len());
            for i in 0..ctx.nodes.len() {
                let state_ptr = ctx.object_states[i];
                if state_ptr.is_null() {
                    continue;
                }
                let node_ptr = &mut ctx.nodes[i] as *mut ThreadListNode;
                (*state_ptr).unlink_node(node_ptr);
            }
            ctx.clear();
        }

        thread.synced_index = synced_index;
        thread.clear_cancellable();
        wait_queue.base_end_wait(thread, wait_result);
        true
    }

    fn cancel_wait(thread: &mut KThread) {
        if !thread.sync_wait_context.is_active() {
            thread.clear_cancellable();
            return;
        }
        unsafe {
            let ctx = &mut thread.sync_wait_context;
            for i in 0..ctx.nodes.len() {
                let state_ptr = ctx.object_states[i];
                if state_ptr.is_null() {
                    continue;
                }
                let node_ptr = &mut ctx.nodes[i] as *mut ThreadListNode;
                (*state_ptr).unlink_node(node_ptr);
            }
            ctx.clear();
        }
        thread.clear_cancellable();
    }
}

/// Enum wrapping the possible sync-object sources so wait() can query the
/// right state_ptr + signaled check per object_id.
enum WaitableObject {
    ReadableEvent(Arc<Mutex<KReadableEvent>>),
    ServerPort(Arc<Mutex<KPort>>),
    ServerSession(Arc<Mutex<KServerSession>>),
    Thread(Arc<KThreadLock>),
    Process,
}

impl WaitableObject {
    fn is_signaled(&self, process: &KProcess) -> bool {
        match self {
            Self::ReadableEvent(event) => event.lock().unwrap().is_signaled(),
            Self::ServerPort(port) => port.lock().unwrap().server.is_signaled(),
            Self::ServerSession(session) => session.lock().unwrap().is_signaled(),
            Self::Thread(thread) => thread.lock().unwrap().is_signaled(),
            Self::Process => process.is_signaled(),
        }
    }

    /// Acquire a raw pointer to the object's `SynchronizationObjectState`.
    ///
    /// # Safety
    /// The caller must guarantee the state lives at least as long as any node
    /// that is linked into it — i.e. the pointer is used while the underlying
    /// `Arc` is held and the scheduler lock is acquired on touch.
    fn sync_state_ptr(&self, process: &mut KProcess) -> *mut SynchronizationObjectState {
        match self {
            Self::ReadableEvent(event) => {
                let mut guard = event.lock().unwrap();
                &mut guard.sync_object as *mut SynchronizationObjectState
            }
            Self::ServerPort(port) => {
                let mut guard = port.lock().unwrap();
                &mut guard.server.sync_object as *mut SynchronizationObjectState
            }
            Self::ServerSession(session) => {
                let mut guard = session.lock().unwrap();
                &mut guard.sync_object as *mut SynchronizationObjectState
            }
            Self::Thread(thread) => {
                let mut guard = thread.lock().unwrap();
                &mut guard.sync_object as *mut SynchronizationObjectState
            }
            Self::Process => &mut process.sync_object as *mut SynchronizationObjectState,
        }
    }
}

fn resolve_waitable_object(process: &KProcess, object_id: u64) -> Option<WaitableObject> {
    if let Some(port) = process.get_server_port_by_object_id(object_id) {
        return Some(WaitableObject::ServerPort(port));
    }
    if let Some(event) = process.get_readable_event_by_object_id(object_id) {
        return Some(WaitableObject::ReadableEvent(event));
    }
    if let Some(session) = process.get_server_session_by_object_id(object_id) {
        return Some(WaitableObject::ServerSession(session));
    }
    if let Some(thread) = process.get_thread_by_object_id(object_id) {
        return Some(WaitableObject::Thread(thread));
    }
    if process.process_id == object_id {
        return Some(WaitableObject::Process);
    }
    None
}

pub fn is_object_signaled(process: &KProcess, object_id: u64) -> bool {
    resolve_waitable_object(process, object_id)
        .map(|object| object.is_signaled(process))
        .unwrap_or(false)
}

fn is_known_object(process: &KProcess, object_id: u64) -> bool {
    resolve_waitable_object(process, object_id).is_some()
}

fn all_objects_known(process: &KProcess, object_ids: &[u64]) -> bool {
    object_ids.iter().all(|&oid| is_known_object(process, oid))
}

fn first_signaled_index(process: &KProcess, object_ids: &[u64]) -> Option<usize> {
    object_ids
        .iter()
        .position(|&oid| is_object_signaled(process, oid))
}

/// KSynchronizationObject — kernel object that threads can wait on.
///
/// Mirrors upstream `KSynchronizationObject : public KAutoObjectWithList`.
///
/// In ruzu each waitable kernel type keeps its own embedded
/// `SynchronizationObjectState` field instead of inheriting this base directly
/// (Rust has no multiple inheritance). This type is retained for the
/// type-token plumbing expected by KAutoObject.
pub struct KSynchronizationObject {
    pub base: KAutoObjectWithList,
    pub sync_object: SynchronizationObjectState,
}

impl KSynchronizationObject {
    pub fn new(kernel: usize) -> Self {
        Self {
            base: KAutoObjectWithList::new(kernel),
            sync_object: SynchronizationObjectState::new(),
        }
    }

    pub fn finalize(&self) {
        self.on_finalize_synchronization_object();
    }

    pub fn on_finalize_synchronization_object(&self) {}

    /// Mirror of upstream `GetWaitingThreadsForDebugging()`. Caller is
    /// responsible for scheduler-lock scoping.
    ///
    /// # Safety
    /// Call under scheduler lock only.
    pub unsafe fn get_waiting_threads_for_debugging(&self) -> Vec<Arc<KThreadLock>> {
        self.sync_object.waiter_snapshot()
    }
}

impl KAutoObjectBase for KSynchronizationObject {
    fn get_type_obj(&self) -> TypeObj {
        Self::get_static_type_obj()
    }

    fn get_type_name(&self) -> &'static str {
        Self::get_static_type_name()
    }
}

impl KSynchronizationObject {
    pub fn get_static_type_obj() -> TypeObj {
        TypeObj::new(
            "KSynchronizationObject",
            k_class_token::class_token(k_class_token::ObjectType::KSynchronizationObject),
        )
    }

    pub fn get_static_type_name() -> &'static str {
        "KSynchronizationObject"
    }
}

/// Walk a sync object's waiter list and call `notify_available` on each
/// thread. Mirrors upstream `KSynchronizationObject::NotifyAvailable`.
///
/// Collects waiters first, then drops the scheduler lock before calling
/// `thread.notify_available` on each (because that method re-acquires the
/// scheduler lock internally — upstream uses a recursive scoped lock).
///
/// # Safety
/// The caller must guarantee `state` remains live for the duration of this
/// call (it's behind an Arc held by the signaler).
pub unsafe fn notify_waiters_on_state(
    state: &SynchronizationObjectState,
    signaled_object_id: u64,
    result: u32,
) -> bool {
    let waiters = state.waiter_snapshot();
    if std::env::var_os("RUZU_TRACE_NOTIFY_WAITERS").is_some() {
        let n = waiters.len();
        log::info!(
            "[NOTIFY] object_id={} waiters={} state_addr={:p} head={:?} tail={:?}",
            signaled_object_id,
            n,
            state as *const _,
            state.head_addr(),
            state.tail_addr(),
        );
    }
    let mut woke_any = false;
    for thread in waiters {
        let mut guard = thread.lock().unwrap();
        if guard.notify_available(signaled_object_id, result) {
            woke_any = true;
        }
    }
    woke_any
}

/// Wait on a set of synchronization objects identified by object_id.
///
/// Mirrors upstream `KSynchronizationObject::Wait`. Key differences:
/// - Ruzu resolves object_ids through the process's object tables (upstream
///   passes `KSynchronizationObject**` directly).
/// - The `ThreadListNode` buffer lives in the wait() function's stack frame
///   (via a `Box<[ThreadListNode]>` owned by the thread's `sync_wait_context`).
/// - The fiber switch driven by `KScopedSchedulerLockAndSleep` matches upstream.
pub fn wait(
    process: &Arc<ProcessLock>,
    current_thread: &Arc<KThreadLock>,
    _scheduler: &Arc<Mutex<KScheduler>>,
    out_index: &mut i32,
    object_ids: Vec<u64>,
    timeout_ns: i64,
) -> ResultCode {
    {
        let mut guard = current_thread.lock().unwrap();
        if guard.is_termination_requested() {
            return RESULT_TERMINATION_REQUESTED;
        }
        if guard.is_wait_cancelled() {
            guard.clear_wait_cancelled();
            *out_index = -1;
            return RESULT_CANCELLED;
        }
    }

    let current_thread_id = current_thread.lock().unwrap().thread_id;
    // Upstream's `KSynchronizationObject::Wait` opens
    // `KScopedSchedulerLockAndSleep slp(kernel, ...)` unconditionally.
    // Use the kernel singleton's scheduler_lock so the lock scope matches
    // upstream even at lifecycle points where the per-thread cache was zero.
    let scheduler_lock = super::kernel::scheduler_lock()
        .expect("scheduler_lock must exist — kernel not initialized?");
    let hardware_timer = super::kernel::get_hardware_timer_arc();
    let thread_ptr = {
        let guard = current_thread.lock().unwrap();
        &*guard as *const KThread as usize
    };

    let result = {
        let (mut sleep_guard, timer) = KScopedSchedulerLockAndSleep::new(
            scheduler_lock,
            hardware_timer.as_ref(),
            current_thread_id,
            thread_ptr,
            timeout_ns,
        );

        let mut process_guard = process.lock().unwrap();

        if !all_objects_known(&process_guard, &object_ids) {
            sleep_guard.cancel_sleep();
            return RESULT_INVALID_HANDLE;
        }

        if let Some(index) = first_signaled_index(&process_guard, &object_ids) {
            if std::env::var_os("RUZU_TRACE_NOTIFY_WAITERS").is_some() {
                log::info!(
                    "[WAIT_EARLY] tid={} object_ids={:?} signaled_index={} short-circuit (no link)",
                    current_thread_id,
                    object_ids,
                    index,
                );
            }
            *out_index = index as i32;
            sleep_guard.cancel_sleep();
            return crate::hle::result::RESULT_SUCCESS;
        }

        *out_index = -1;
        if timeout_ns == 0 {
            sleep_guard.cancel_sleep();
            return RESULT_TIMED_OUT;
        }

        // Allocate the node buffer and resolve sync-state pointers for every
        // object. Once linked these pointers must stay valid until wake.
        let n = object_ids.len();
        let mut nodes: Box<[ThreadListNode]> = (0..n)
            .map(|_| ThreadListNode::new())
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let mut state_ptrs: Vec<*mut SynchronizationObjectState> = Vec::with_capacity(n);

        let current_thread_weak = Arc::downgrade(current_thread);
        for (i, oid) in object_ids.iter().enumerate() {
            let Some(object) = resolve_waitable_object(&process_guard, *oid) else {
                sleep_guard.cancel_sleep();
                return RESULT_INVALID_HANDLE;
            };
            let state_ptr = object.sync_state_ptr(&mut process_guard);
            nodes[i].thread = current_thread_weak.clone();
            nodes[i].object_id = *oid;
            unsafe {
                (*state_ptr).link_node(&mut nodes[i] as *mut ThreadListNode);
            }
            state_ptrs.push(state_ptr);
        }

        // Stash the node buffer + state_ptrs on the thread so the queue
        // callback can unlink on wake.
        {
            let mut guard = current_thread.lock().unwrap();
            guard.sync_wait_context = SynchronizationWaitContext {
                nodes,
                object_ids: object_ids.clone(),
                object_states: state_ptrs,
                active: true,
            };
            guard.synced_index = -1;
            guard.wait_result = crate::hle::result::RESULT_SUCCESS.get_inner_value();
            guard.set_cancellable();
            guard.set_wait_reason_for_debugging(
                super::k_thread::ThreadWaitReasonForDebugging::Synchronization,
            );

            if timeout_ns > 0 {
                let current_tick = super::kernel::get_current_hardware_tick();
                guard.sleep_deadline =
                    super::k_thread::deadline_from_timeout_tick(timeout_ns, current_tick);
            } else {
                guard.sleep_deadline = None;
            }

            let mut wait_queue = ThreadQueueImplForKSynchronizationObjectWait::queue();
            if let Some(timer) = timer {
                wait_queue.set_hardware_timer(timer);
            }
            guard.begin_wait_with_queue(wait_queue);
        }

        crate::hle::result::RESULT_SUCCESS
    };
    // KScopedSchedulerLockAndSleep drops here → fiber switch. When the
    // waiter resumes, sync_wait_context has been cleared by the notify
    // callback and synced_index holds the fired index.

    let thread = current_thread.lock().unwrap();
    *out_index = thread.get_synced_index();
    ResultCode::new(thread.get_wait_result())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn link_and_unlink_single_node() {
        let mut state = SynchronizationObjectState::new();
        let mut node = ThreadListNode::new();
        unsafe {
            state.link_node(&mut node);
            assert!(!state.is_empty());
            state.unlink_node(&mut node);
            assert!(state.is_empty());
        }
    }

    #[test]
    fn link_and_unlink_preserves_order() {
        let mut state = SynchronizationObjectState::new();
        let mut a = ThreadListNode::new();
        let mut b = ThreadListNode::new();
        let mut c = ThreadListNode::new();
        a.object_id = 1;
        b.object_id = 2;
        c.object_id = 3;
        unsafe {
            state.link_node(&mut a);
            state.link_node(&mut b);
            state.link_node(&mut c);
            state.unlink_node(&mut b);
            // head → a → c
            let mut ids = Vec::new();
            let mut cur = state.head;
            while !cur.is_null() {
                ids.push((*cur).object_id);
                cur = (*cur).next;
            }
            assert_eq!(ids, vec![1, 3]);

            state.unlink_node(&mut a);
            state.unlink_node(&mut c);
            assert!(state.is_empty());
        }
    }
}
