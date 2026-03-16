//! Port of zuyu/src/core/hle/kernel/k_synchronization_object.h and k_synchronization_object.cpp
//! Status: EN COURS
//! Derniere synchro: 2026-03-11
//!
//! KSynchronizationObject — base class for kernel objects that threads can wait on.
//! Extends KAutoObjectWithList.
//!
//! The Wait() static method and thread queue integration are stubbed until
//! KThread, KScheduler, and KThreadQueue are ported.

use std::sync::{Arc, Mutex, Weak};

use super::k_auto_object::{KAutoObjectBase, KAutoObjectWithList, TypeObj};
use super::k_class_token;
use super::k_process::KProcess;
use super::k_readable_event::KReadableEvent;
use super::k_scheduler::KScheduler;
use super::k_thread::KThread;
use super::k_thread_queue::{KThreadQueue, KThreadQueueWithoutEndWait};
use crate::hle::kernel::svc::svc_results::{
    RESULT_CANCELLED, RESULT_INVALID_HANDLE, RESULT_TERMINATION_REQUESTED, RESULT_TIMED_OUT,
};
use crate::hle::result::ResultCode;

/// ThreadListNode — intrusive linked list node for threads waiting on a sync object.
/// Mirrors upstream `KSynchronizationObject::ThreadListNode`.
pub struct ThreadListNode {
    pub next: Option<*mut ThreadListNode>,
    /// Opaque thread handle until KThread is ported.
    // TODO: Replace with *mut KThread when KThread is ported.
    pub thread: usize,
}

impl ThreadListNode {
    pub fn new() -> Self {
        Self {
            next: None,
            thread: 0,
        }
    }
}

/// Trait for objects that can be signaled and waited on.
/// Mirrors the pure virtual `IsSignaled()` in upstream.
pub trait KSynchronizable {
    fn is_signaled(&self) -> bool;
}

#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct SynchronizationWaiters {
    head: Option<SynchronizationWaitNodeHandle>,
    tail: Option<SynchronizationWaitNodeHandle>,
}

#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct SynchronizationObjectState {
    pub waiters: SynchronizationWaiters,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SynchronizationWaitNode {
    pub object_id: u64,
    pub handle: SynchronizationWaitNodeHandle,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SynchronizationWaitNodeHandle {
    pub thread_id: u64,
    pub wait_index: usize,
}

impl SynchronizationWaiters {
    pub fn new() -> Self {
        Self { head: None, tail: None }
    }

    pub fn link(&mut self, process: &mut KProcess, handle: SynchronizationWaitNodeHandle) {
        if wait_object_linked(process, handle) {
            return;
        }

        set_wait_object_links(process, handle, self.tail, None, true);

        if let Some(tail) = self.tail {
            update_wait_object_next(process, tail, Some(handle));
        } else {
            self.head = Some(handle);
        }
        self.tail = Some(handle);
    }

    pub fn unlink(&mut self, process: &mut KProcess, thread_id: u64, wait_index: usize) {
        self.unlink_handle(
            process,
            SynchronizationWaitNodeHandle {
                thread_id,
                wait_index,
            },
        );
    }

    pub fn unlink_all_for_thread(&mut self, process: &mut KProcess, thread_id: u64) {
        let handles: Vec<SynchronizationWaitNodeHandle> = self
            .handles(process)
            .into_iter()
            .filter(|handle| handle.thread_id == thread_id)
            .collect();
        for handle in handles {
            self.unlink_handle(process, handle);
        }
    }

    pub fn clear(&mut self) {
        self.head = None;
        self.tail = None;
    }

    pub fn snapshot(&self, process: &KProcess) -> Vec<u64> {
        self.handles(process)
            .into_iter()
            .map(|handle| handle.thread_id)
            .collect()
    }

    pub fn handles(&self, process: &KProcess) -> Vec<SynchronizationWaitNodeHandle> {
        let mut handles = Vec::new();
        let mut current = self.head;
        while let Some(handle) = current {
            handles.push(handle);
            current = wait_object_next(process, handle);
        }
        handles
    }

    pub fn is_empty(&self) -> bool {
        self.head.is_none()
    }

    fn unlink_handle(&mut self, process: &mut KProcess, handle: SynchronizationWaitNodeHandle) {
        let Some((prev, next, linked)) = wait_object_links(process, handle) else {
            return;
        };
        if !linked {
            return;
        }

        if let Some(prev_handle) = prev {
            update_wait_object_next(process, prev_handle, next);
        } else {
            self.head = next;
        }

        if let Some(next_handle) = next {
            update_wait_object_prev(process, next_handle, prev);
        } else {
            self.tail = prev;
        }

        set_wait_object_links(process, handle, None, None, false);
    }
}

pub(crate) struct ThreadQueueImplForKSynchronizationObjectWait;

impl SynchronizationObjectState {
    pub fn new() -> Self {
        Self {
            waiters: SynchronizationWaiters::new(),
        }
    }

    pub fn link_waiter(&mut self, process: &mut KProcess, node: SynchronizationWaitNode) {
        self.waiters.link(process, node.handle);
    }

    pub fn unlink_waiter(
        &mut self,
        process: &mut KProcess,
        thread_id: u64,
        object_id: u64,
        wait_index: usize,
    ) {
        let _ = object_id;
        self.waiters.unlink(process, thread_id, wait_index);
    }

    pub fn clear_waiters(&mut self) {
        self.waiters.clear();
    }

    pub fn unlink_all_waiters_for_thread(
        &mut self,
        process: &mut KProcess,
        thread_id: u64,
        object_id: u64,
    ) {
        let _ = object_id;
        self.waiters.unlink_all_for_thread(process, thread_id);
    }

    pub fn waiter_snapshot(&self, process: &KProcess) -> Vec<u64> {
        self.waiters.snapshot(process)
    }

    pub fn waiters_are_empty(&self) -> bool {
        self.waiters.is_empty()
    }

    pub fn waiter_node_handles(&self, process: &KProcess) -> Vec<SynchronizationWaitNodeHandle> {
        self.waiters.handles(process)
    }
}

#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct SynchronizationWaitSet {
    objects: Vec<SynchronizationWaitObject>,
    active: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SynchronizationWaitObject {
    pub object_id: u64,
    pub wait_index: usize,
    pub wait_node: Option<SynchronizationWaitNode>,
    pub prev_waiter: Option<SynchronizationWaitNodeHandle>,
    pub next_waiter: Option<SynchronizationWaitNodeHandle>,
    pub linked: bool,
}

impl SynchronizationWaitSet {
    pub fn new() -> Self {
        Self {
            objects: Vec::new(),
            active: false,
        }
    }

    pub fn begin(&mut self, object_ids: Vec<u64>) {
        self.objects = object_ids
            .into_iter()
            .enumerate()
            .map(|(wait_index, object_id)| SynchronizationWaitObject {
                object_id,
                wait_index,
                wait_node: None,
                prev_waiter: None,
                next_waiter: None,
                linked: false,
            })
            .collect();
        self.active = true;
    }

    pub fn bind_thread(&mut self, thread_id: u64) {
        for object in &mut self.objects {
            object.wait_node = Some(SynchronizationWaitNode {
                object_id: object.object_id,
                handle: SynchronizationWaitNodeHandle {
                    thread_id,
                    wait_index: object.wait_index,
                },
            });
        }
    }

    pub fn clear(&mut self) {
        self.objects.clear();
        self.active = false;
    }

    pub fn is_active(&self) -> bool {
        self.active
    }

    pub fn snapshot(&self) -> Vec<u64> {
        self.objects.iter().map(|object| object.object_id).collect()
    }

    pub fn objects(&self) -> &[SynchronizationWaitObject] {
        &self.objects
    }

    pub fn iter(&self) -> impl Iterator<Item = &u64> {
        self.objects.iter().map(|object| &object.object_id)
    }

    pub fn position(&self, object_id: u64) -> Option<usize> {
        self.objects
            .iter()
            .position(|candidate| candidate.object_id == object_id)
    }

    pub fn object_by_id(&self, object_id: u64) -> Option<&SynchronizationWaitObject> {
        self.objects
            .iter()
            .find(|candidate| candidate.object_id == object_id)
    }

    pub fn object_by_wait_index(&self, wait_index: usize) -> Option<&SynchronizationWaitObject> {
        self.objects
            .iter()
            .find(|candidate| candidate.wait_index == wait_index)
    }

    pub fn object_by_wait_index_mut(
        &mut self,
        wait_index: usize,
    ) -> Option<&mut SynchronizationWaitObject> {
        self.objects
            .iter_mut()
            .find(|candidate| candidate.wait_index == wait_index)
    }
}

enum WaitableObject {
    ReadableEvent(Arc<Mutex<KReadableEvent>>),
    Thread(Arc<Mutex<KThread>>),
    Process,
}

impl WaitableObject {
    fn is_signaled(&self, process: &KProcess) -> bool {
        match self {
            Self::ReadableEvent(event) => event.lock().unwrap().is_signaled(),
            Self::Thread(thread) => thread.lock().unwrap().is_signaled(),
            Self::Process => process.is_signaled(),
        }
    }

    fn link_waiter(&self, process: &mut KProcess, node: SynchronizationWaitNode) {
        match self {
            Self::ReadableEvent(event) => event.lock().unwrap().sync_object.link_waiter(process, node),
            Self::Thread(thread) => thread.lock().unwrap().sync_object.link_waiter(process, node),
            Self::Process => process.link_waiter(node.handle.thread_id),
        }
    }

    fn unlink_waiter(
        &self,
        process: &mut KProcess,
        thread_id: u64,
        object_id: u64,
        wait_index: usize,
    ) {
        match self {
            Self::ReadableEvent(event) => event
                .lock()
                .unwrap()
                .sync_object
                .unlink_waiter(process, thread_id, object_id, wait_index),
            Self::Thread(thread) => thread
                .lock()
                .unwrap()
                .sync_object
                .unlink_waiter(process, thread_id, object_id, wait_index),
            Self::Process => process.unlink_waiter(thread_id),
        }
    }

    fn waiter_thread_ids(&self, process: &KProcess) -> Vec<u64> {
        match self {
            Self::ReadableEvent(event) => event.lock().unwrap().sync_object.waiter_snapshot(process),
            Self::Thread(thread) => thread.lock().unwrap().sync_object.waiter_snapshot(process),
            Self::Process => process.sync_object.waiter_snapshot(process),
        }
    }

    fn unlink_all_waiters_for_thread(&self, process: &mut KProcess, thread_id: u64, object_id: u64) {
        match self {
            Self::ReadableEvent(event) => event
                .lock()
                .unwrap()
                .sync_object
                .unlink_all_waiters_for_thread(process, thread_id, object_id),
            Self::Thread(thread) => thread
                .lock()
                .unwrap()
                .sync_object
                .unlink_all_waiters_for_thread(process, thread_id, object_id),
            Self::Process => {
                let _ = object_id;
                let mut waiters = std::mem::take(&mut process.sync_object.waiters);
                waiters.unlink_all_for_thread(process, thread_id);
                process.sync_object.waiters = waiters;
            }
        }
    }
}

fn with_wait_object<R>(
    process: &KProcess,
    handle: SynchronizationWaitNodeHandle,
    f: impl FnOnce(&SynchronizationWaitObject) -> R,
) -> Option<R> {
    let thread = process.get_thread_by_thread_id(handle.thread_id)?;
    let thread = thread.lock().unwrap();
    let wait_object = thread.synchronization_wait.object_by_wait_index(handle.wait_index)?;
    Some(f(wait_object))
}

fn with_wait_object_mut<R>(
    process: &mut KProcess,
    handle: SynchronizationWaitNodeHandle,
    f: impl FnOnce(&mut SynchronizationWaitObject) -> R,
) -> Option<R> {
    let thread = process.get_thread_by_thread_id(handle.thread_id)?;
    let thread = Arc::clone(&thread);
    let mut thread = thread.lock().unwrap();
    let wait_object = thread
        .synchronization_wait
        .object_by_wait_index_mut(handle.wait_index)?;
    Some(f(wait_object))
}

fn wait_object_linked(process: &KProcess, handle: SynchronizationWaitNodeHandle) -> bool {
    with_wait_object(process, handle, |wait_object| wait_object.linked).unwrap_or(false)
}

fn wait_object_links(
    process: &KProcess,
    handle: SynchronizationWaitNodeHandle,
) -> Option<(
    Option<SynchronizationWaitNodeHandle>,
    Option<SynchronizationWaitNodeHandle>,
    bool,
)> {
    with_wait_object(process, handle, |wait_object| {
        (wait_object.prev_waiter, wait_object.next_waiter, wait_object.linked)
    })
}

fn set_wait_object_links(
    process: &mut KProcess,
    handle: SynchronizationWaitNodeHandle,
    prev: Option<SynchronizationWaitNodeHandle>,
    next: Option<SynchronizationWaitNodeHandle>,
    linked: bool,
) {
    let _ = with_wait_object_mut(process, handle, |wait_object| {
        wait_object.prev_waiter = prev;
        wait_object.next_waiter = next;
        wait_object.linked = linked;
    });
}

fn wait_object_next(
    process: &KProcess,
    handle: SynchronizationWaitNodeHandle,
) -> Option<SynchronizationWaitNodeHandle> {
    with_wait_object(process, handle, |wait_object| wait_object.next_waiter).unwrap_or(None)
}

fn update_wait_object_next(
    process: &mut KProcess,
    handle: SynchronizationWaitNodeHandle,
    next: Option<SynchronizationWaitNodeHandle>,
) {
    let _ = with_wait_object_mut(process, handle, |wait_object| {
        wait_object.next_waiter = next;
    });
}

fn update_wait_object_prev(
    process: &mut KProcess,
    handle: SynchronizationWaitNodeHandle,
    prev: Option<SynchronizationWaitNodeHandle>,
) {
    let _ = with_wait_object_mut(process, handle, |wait_object| {
        wait_object.prev_waiter = prev;
    });
}

/// KSynchronizationObject — kernel object that threads can wait on.
///
/// Mirrors upstream `KSynchronizationObject : public KAutoObjectWithList`.
pub struct KSynchronizationObject {
    pub base: KAutoObjectWithList,
    m_thread_list_head: Option<*mut ThreadListNode>,
    m_thread_list_tail: Option<*mut ThreadListNode>,
}

impl KSynchronizationObject {
    pub fn new(kernel: usize) -> Self {
        Self {
            base: KAutoObjectWithList::new(kernel),
            m_thread_list_head: None,
            m_thread_list_tail: None,
        }
    }

    /// Link a ThreadListNode to the end of the wait list.
    /// Mirrors upstream `LinkNode`.
    ///
    /// # Safety
    /// The caller must ensure `node` is a valid pointer and remains alive while linked.
    pub unsafe fn link_node(&mut self, node: *mut ThreadListNode) {
        if let Some(tail) = self.m_thread_list_tail {
            (*tail).next = Some(node);
        } else {
            self.m_thread_list_head = Some(node);
        }
        self.m_thread_list_tail = Some(node);
    }

    /// Unlink a ThreadListNode from the wait list.
    /// Mirrors upstream `UnlinkNode`.
    ///
    /// # Safety
    /// The caller must ensure `node` is in this list and all pointers are valid.
    pub unsafe fn unlink_node(&mut self, node: *mut ThreadListNode) {
        // Walk the list to find the predecessor of `node`.
        let mut prev: *mut ThreadListNode = std::ptr::null_mut();
        let mut tail_prev: *mut ThreadListNode = std::ptr::null_mut();
        let mut cur = self.m_thread_list_head;

        // The upstream implementation uses a trick where it casts &m_thread_list_head
        // to a ThreadListNode* and walks from there. We replicate the logic more safely.
        loop {
            match cur {
                Some(cur_ptr) if cur_ptr == node => break,
                Some(cur_ptr) => {
                    tail_prev = prev;
                    prev = cur_ptr;
                    cur = (*cur_ptr).next;
                }
                None => {
                    // Node not found in list — should not happen.
                    debug_assert!(false, "UnlinkNode: node not found in list");
                    return;
                }
            }
        }

        // Update tail if we're removing the tail node.
        if self.m_thread_list_tail == Some(node) {
            if tail_prev.is_null() && prev.is_null() {
                // Removing the only node
                self.m_thread_list_tail = None;
            } else {
                self.m_thread_list_tail = if prev.is_null() { None } else { Some(prev) };
            }
        }

        // Unlink: prev->next = node->next
        let next = (*node).next;
        if prev.is_null() {
            // node was head
            self.m_thread_list_head = next;
        } else {
            (*prev).next = next;
        }
    }

    /// Finalize the synchronization object.
    /// Mirrors upstream `KSynchronizationObject::Finalize()`.
    pub fn finalize(&self) {
        self.on_finalize_synchronization_object();
        // TODO: Call base KAutoObject::Finalize() when fully wired up.
    }

    /// Hook for derived classes to perform cleanup on finalization.
    /// Mirrors upstream `OnFinalizeSynchronizationObject()` (empty default).
    pub fn on_finalize_synchronization_object(&self) {}

    /// Notify all waiting threads that this object is available.
    /// Mirrors upstream `NotifyAvailable(Result result)`.
    pub fn notify_available(&self, _result: ResultCode) {
        // TODO: Requires KScopedSchedulerLock and KThread.
        // KScopedSchedulerLock sl(m_kernel);
        // if (!self.is_signaled()) { return; }
        // for cur_node in self.m_thread_list_head.. {
        //     cur_node.thread.notify_available(self, result);
        // }
    }

    /// Notify with success result. Mirrors the no-arg overload.
    pub fn notify_available_success(&self) {
        self.notify_available(crate::hle::result::RESULT_SUCCESS);
    }

    /// Get list of waiting threads (for debugging).
    /// Mirrors upstream `GetWaitingThreadsForDebugging()`.
    pub fn get_waiting_threads_for_debugging(&self) -> Vec<usize> {
        // TODO: Return Vec<&KThread> once KThread is ported.
        let mut threads = Vec::new();
        unsafe {
            let mut cur = self.m_thread_list_head;
            while let Some(node) = cur {
                threads.push((*node).thread);
                cur = (*node).next;
            }
        }
        threads
    }

    /// Wait on multiple synchronization objects.
    /// Mirrors upstream static `KSynchronizationObject::Wait(...)`.
    ///
    /// TODO: Requires KThread, KScheduler, KScopedSchedulerLockAndSleep, KThreadQueue.
    pub fn wait(
        _kernel: usize,
        _out_index: &mut i32,
        _objects: &mut [&mut KSynchronizationObject],
        _timeout: i64,
    ) -> ResultCode {
        // Upstream: acquires KScopedSchedulerLockAndSleep, iterates objects,
        // checks IsSignaled(), enqueues thread onto waiting list, then blocks.
        // TODO: Implement once KThread and KScheduler are ported.
        log::warn!("KSynchronizationObject::wait: KThread/KScheduler not yet ported, returning error");
        crate::hle::result::RESULT_UNKNOWN
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

pub fn is_object_signaled(process: &KProcess, object_id: u64) -> bool {
    resolve_waitable_object(process, object_id)
        .map(|object| object.is_signaled(process))
        .unwrap_or(false)
}

pub fn link_waiter(process: &mut KProcess, wait_node: SynchronizationWaitNode) {
    let object_id = wait_node.object_id;
    if let Some(object) = resolve_waitable_object(process, object_id) {
        object.link_waiter(process, wait_node);
    }
}

pub fn link_wait_set(process: &mut KProcess, wait_set: &SynchronizationWaitSet) {
    for wait_object in wait_set.objects() {
        if let Some(wait_node) = wait_object.wait_node.clone() {
            link_waiter(process, wait_node);
        }
    }
}

pub fn unlink_waiter(process: &mut KProcess, wait_node: &SynchronizationWaitNode) {
    if let Some(object) = resolve_waitable_object(process, wait_node.object_id) {
        object.unlink_waiter(
            process,
            wait_node.handle.thread_id,
            wait_node.object_id,
            wait_node.handle.wait_index,
        );
    }
}

pub fn unlink_all_waiters_for_thread(
    process: &mut KProcess,
    object_id: u64,
    thread_id: u64,
) {
    if let Some(object) = resolve_waitable_object(process, object_id) {
        object.unlink_all_waiters_for_thread(process, thread_id, object_id);
    }
}

pub fn unlink_wait_set(
    process: &mut KProcess,
    thread_id: u64,
    wait_set: &SynchronizationWaitSet,
    skip_object_id: Option<u64>,
) {
    for wait_object in wait_set.objects() {
        if skip_object_id == Some(wait_object.object_id) {
            continue;
        }
        let Some(wait_node) = wait_object.wait_node.as_ref() else {
            continue;
        };
        debug_assert_eq!(wait_node.handle.thread_id, thread_id);
        unlink_waiter(process, wait_node);
    }
}

pub fn consume_signaled_wait_set(
    process: &mut KProcess,
    thread_id: u64,
    wait_set: &SynchronizationWaitSet,
    signaled_object_id: u64,
) -> Option<usize> {
    let signaled_wait_object = wait_set.object_by_id(signaled_object_id)?;
    let synced_index = signaled_wait_object.wait_index;
    unlink_wait_set(process, thread_id, wait_set, Some(signaled_object_id));
    Some(synced_index)
}

pub fn clear_wait_set(
    process: Option<&mut KProcess>,
    thread_id: u64,
    wait_set: &mut SynchronizationWaitSet,
) {
    if !wait_set.is_active() {
        return;
    }

    if let Some(process) = process {
        unlink_wait_set(process, thread_id, wait_set, None);
    }

    wait_set.clear();
}

pub fn check_wait_ready(process: &KProcess, thread: &KThread) -> Option<i32> {
    if !thread.synchronization_wait.is_active() {
        return None;
    }

    first_signaled_index(process, &thread.synchronization_wait).map(|index| index as i32)
}

pub fn notify_waiter_available(
    thread: &mut KThread,
    process: &mut KProcess,
    signaled_object_id: u64,
    _result: u32,
) -> bool {
    if !thread.synchronization_wait.is_active()
        || thread.get_state() != super::k_thread::ThreadState::WAITING
    {
        return false;
    }

    let Some(synced_index) = consume_signaled_wait_set(
        process,
        thread.thread_id,
        &thread.synchronization_wait,
        signaled_object_id,
    ) else {
        return false;
    };

    // The process lock is already held by the caller in the object-notify path,
    // so consume the active wait registration before the base wait queue cleanup runs.
    thread.synchronization_wait.clear();
    thread.synced_index = synced_index as i32;
    thread.clear_cancellable();
    true
}

impl ThreadQueueImplForKSynchronizationObjectWait {
    pub(crate) fn queue() -> KThreadQueue {
        KThreadQueueWithoutEndWait::with_callbacks(
            Some(Self::notify_available),
            Some(Self::cancel_wait),
        )
        .base
    }

    fn notify_available(
        wait_queue: &KThreadQueue,
        thread: &mut KThread,
        process: &mut KProcess,
        signaled_object_id: u64,
        result: u32,
    ) -> bool {
        if notify_waiter_available(thread, process, signaled_object_id, result) {
            wait_queue.base_end_wait(thread, result);
            true
        } else {
            false
        }
    }

    fn cancel_wait(thread: &mut KThread) {
        if !thread.synchronization_wait.is_active() {
            thread.clear_cancellable();
            return;
        }

        if let Some(parent) = thread.parent.as_ref().and_then(Weak::upgrade) {
            let mut process_guard = parent.lock().unwrap();
            clear_wait_set(
                Some(&mut process_guard),
                thread.thread_id,
                &mut thread.synchronization_wait,
            );
        } else {
            clear_wait_set(None, thread.thread_id, &mut thread.synchronization_wait);
        }
        thread.clear_cancellable();
    }
}

pub fn first_signaled_index(process: &KProcess, wait_set: &SynchronizationWaitSet) -> Option<usize> {
    wait_set
        .objects()
        .iter()
        .position(|object| is_object_signaled(process, object.object_id))
}

pub fn wait_set_position(wait_set: &SynchronizationWaitSet, object_id: u64) -> Option<usize> {
    wait_set.position(object_id)
}

pub fn all_objects_known(process: &KProcess, wait_set: &SynchronizationWaitSet) -> bool {
    wait_set
        .objects()
        .iter()
        .all(|object| is_known_object(process, object.object_id))
}

pub fn notify_available(process: &mut KProcess, signaled_object_id: u64, result: u32) -> bool {
    if !is_object_signaled(process, signaled_object_id) {
        return false;
    }

    let outcome = process_waiter_snapshot(
        process,
        signaled_object_id,
        &waiter_thread_ids(process, signaled_object_id),
        result,
    );
    for waiter_thread_id in outcome.unlink_thread_ids {
        unlink_all_waiters_for_thread(process, signaled_object_id, waiter_thread_id);
    }
    outcome.woke_any
}

pub struct NotifyWaitersOutcome {
    pub woke_any: bool,
    pub unlink_thread_ids: Vec<u64>,
}

pub fn process_waiter_snapshot(
    process: &mut KProcess,
    signaled_object_id: u64,
    waiter_thread_ids: &[u64],
    result: u32,
) -> NotifyWaitersOutcome {
    let mut woke_any = false;
    let mut unlink_thread_ids = Vec::new();

    for waiter_thread_id in waiter_thread_ids.iter().copied() {
        let Some(waiter_thread) = process.get_thread_by_thread_id(waiter_thread_id) else {
            unlink_thread_ids.push(waiter_thread_id);
            continue;
        };

        let mut waiter_thread = waiter_thread.lock().unwrap();
        if waiter_thread.notify_available(process, signaled_object_id, result) {
            unlink_thread_ids.push(waiter_thread_id);
            woke_any = true;
        } else if waiter_thread.get_state() != super::k_thread::ThreadState::WAITING {
            unlink_thread_ids.push(waiter_thread_id);
        }
    }

    NotifyWaitersOutcome {
        woke_any,
        unlink_thread_ids,
    }
}

pub fn wait(
    process: &mut KProcess,
    current_thread: &Arc<Mutex<KThread>>,
    scheduler: &Arc<Mutex<KScheduler>>,
    out_index: &mut i32,
    object_ids: Vec<u64>,
    timeout_ns: i64,
) -> ResultCode {
    {
        let mut current_thread = current_thread.lock().unwrap();
        if current_thread.is_termination_requested() {
            return RESULT_TERMINATION_REQUESTED;
        }
        if current_thread.is_wait_cancelled() {
            current_thread.clear_wait_cancelled();
            *out_index = -1;
            return RESULT_CANCELLED;
        }
    }

    let mut wait_set = SynchronizationWaitSet::new();
    wait_set.begin(object_ids);

    if !all_objects_known(process, &wait_set) {
        return RESULT_INVALID_HANDLE;
    }

    if let Some(index) = first_signaled_index(process, &wait_set) {
        *out_index = index as i32;
        return crate::hle::result::RESULT_SUCCESS;
    }

    *out_index = -1;
    if timeout_ns == 0 {
        return RESULT_TIMED_OUT;
    }

    let current_thread_id = current_thread.lock().unwrap().thread_id;
    wait_set.bind_thread(current_thread_id);
    link_wait_set(process, &wait_set);

    current_thread
        .lock()
        .unwrap()
        .begin_wait_synchronization(wait_set, timeout_ns);
    scheduler.lock().unwrap().request_schedule();
    crate::hle::result::RESULT_SUCCESS
}

fn is_known_object(process: &KProcess, object_id: u64) -> bool {
    resolve_waitable_object(process, object_id).is_some()
}

fn waiter_thread_ids(process: &KProcess, object_id: u64) -> Vec<u64> {
    resolve_waitable_object(process, object_id)
        .map(|object| object.waiter_thread_ids(process))
        .unwrap_or_default()
}

fn resolve_waitable_object(process: &KProcess, object_id: u64) -> Option<WaitableObject> {
    if let Some(event) = process.get_readable_event_by_object_id(object_id) {
        return Some(WaitableObject::ReadableEvent(event));
    }
    if let Some(thread) = process.get_thread_by_object_id(object_id) {
        return Some(WaitableObject::Thread(thread));
    }
    if process.process_id == object_id {
        return Some(WaitableObject::Process);
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hle::kernel::k_process::KProcess;
    use crate::hle::kernel::k_readable_event::KReadableEvent;
    use crate::hle::kernel::k_scheduler::KScheduler;
    use crate::hle::kernel::k_thread::KThread;
    use crate::hle::kernel::svc::svc_results::RESULT_TIMED_OUT;
    use crate::hle::result::RESULT_SUCCESS;
    use std::sync::atomic::Ordering;
    use std::sync::{Arc, Mutex};

    #[test]
    fn test_sync_object_creation() {
        let obj = KSynchronizationObject::new(0);
        assert!(obj.m_thread_list_head.is_none());
        assert!(obj.m_thread_list_tail.is_none());
    }

    #[test]
    fn test_sync_object_type_obj() {
        let ty = KSynchronizationObject::get_static_type_obj();
        assert_eq!(ty.get_name(), "KSynchronizationObject");
    }

    #[test]
    fn test_link_unlink_node() {
        let mut obj = KSynchronizationObject::new(0);
        let mut node1 = ThreadListNode::new();
        let mut node2 = ThreadListNode::new();

        unsafe {
            obj.link_node(&mut node1 as *mut _);
            obj.link_node(&mut node2 as *mut _);
            assert_eq!(obj.get_waiting_threads_for_debugging().len(), 2);

            obj.unlink_node(&mut node1 as *mut _);
            assert_eq!(obj.get_waiting_threads_for_debugging().len(), 1);

            obj.unlink_node(&mut node2 as *mut _);
            assert_eq!(obj.get_waiting_threads_for_debugging().len(), 0);
        }
    }

    #[test]
    fn helper_resolves_waiter_operations() {
        let mut process = KProcess::new();
        process.process_id = 55;

        let readable = Arc::new(Mutex::new(KReadableEvent::new()));
        readable.lock().unwrap().initialize(1, 2);
        process.register_readable_event_object(2, Arc::clone(&readable));

        let thread = Arc::new(Mutex::new(KThread::new()));
        {
            let mut thread_guard = thread.lock().unwrap();
            thread_guard.thread_id = 7;
            thread_guard.object_id = 3;
            thread_guard.signaled = true;
        }
        process.register_thread_object(Arc::clone(&thread));

        assert!(!is_object_signaled(&process, 2));
        readable.lock().unwrap().is_signaled = true;
        assert!(is_object_signaled(&process, 2));
        assert!(is_object_signaled(&process, 3));

        link_waiter(
            &mut process,
            SynchronizationWaitNode {
                object_id: 2,
                handle: SynchronizationWaitNodeHandle {
                    thread_id: 99,
                    wait_index: 0,
                },
            },
        );
        assert_eq!(readable.lock().unwrap().sync_object.waiter_snapshot(&process), vec![99]);
        unlink_waiter(
            &mut process,
            &SynchronizationWaitNode {
                object_id: 2,
                handle: SynchronizationWaitNodeHandle {
                    thread_id: 99,
                    wait_index: 0,
                },
            },
        );
        assert!(readable.lock().unwrap().sync_object.waiters_are_empty());

        link_waiter(
            &mut process,
            SynchronizationWaitNode {
                object_id: 55,
                handle: SynchronizationWaitNodeHandle {
                    thread_id: 77,
                    wait_index: 0,
                },
            },
        );
        assert_eq!(process.sync_object.waiter_snapshot(&process), vec![77]);
        unlink_waiter(
            &mut process,
            &SynchronizationWaitNode {
                object_id: 55,
                handle: SynchronizationWaitNodeHandle {
                    thread_id: 77,
                    wait_index: 0,
                },
            },
        );
        assert!(process.sync_object.waiters_are_empty());
    }

    #[test]
    fn link_wait_set_registers_all_objects() {
        let mut process = KProcess::new();
        process.process_id = 55;

        let readable = Arc::new(Mutex::new(KReadableEvent::new()));
        readable.lock().unwrap().initialize(1, 2);
        process.register_readable_event_object(2, Arc::clone(&readable));

        let thread = Arc::new(Mutex::new(KThread::new()));
        {
            let mut thread_guard = thread.lock().unwrap();
            thread_guard.thread_id = 7;
            thread_guard.object_id = 3;
        }
        process.register_thread_object(Arc::clone(&thread));

        let mut wait_set = SynchronizationWaitSet::new();
        wait_set.begin(vec![2, 3, 55]);
        wait_set.bind_thread(99);
        link_wait_set(&mut process, &wait_set);

        assert_eq!(readable.lock().unwrap().sync_object.waiter_snapshot(&process), vec![99]);
        assert_eq!(thread.lock().unwrap().sync_object.waiter_snapshot(&process), vec![99]);
        assert_eq!(process.sync_object.waiter_snapshot(&process), vec![99]);
    }

    #[test]
    fn wait_set_preserves_object_order() {
        let mut wait_set = SynchronizationWaitSet::new();
        wait_set.begin(vec![7, 3, 9]);

        assert!(wait_set.is_active());
        assert_eq!(wait_set.snapshot(), vec![7, 3, 9]);
        assert_eq!(wait_set.objects()[0].object_id, 7);
        assert_eq!(wait_set.objects()[0].wait_index, 0);
        assert!(wait_set.objects()[0].wait_node.is_none());
        assert_eq!(wait_set.objects()[1].object_id, 3);
        assert_eq!(wait_set.objects()[1].wait_index, 1);
        assert_eq!(wait_set.position(9), Some(2));

        wait_set.clear();
        assert!(!wait_set.is_active());
        assert!(wait_set.snapshot().is_empty());
    }

    #[test]
    fn first_signaled_index_uses_wait_set_order() {
        let mut process = KProcess::new();
        process.process_id = 55;

        let readable_a = Arc::new(Mutex::new(KReadableEvent::new()));
        readable_a.lock().unwrap().initialize(1, 2);
        process.register_readable_event_object(2, Arc::clone(&readable_a));

        let readable_b = Arc::new(Mutex::new(KReadableEvent::new()));
        readable_b.lock().unwrap().initialize(1, 3);
        readable_b.lock().unwrap().is_signaled = true;
        process.register_readable_event_object(3, Arc::clone(&readable_b));

        let mut wait_set = SynchronizationWaitSet::new();
        wait_set.begin(vec![2, 3]);

        assert_eq!(first_signaled_index(&process, &wait_set), Some(1));
        assert!(all_objects_known(&process, &wait_set));
    }

    #[test]
    fn wait_set_position_uses_wait_set_entries() {
        let mut wait_set = SynchronizationWaitSet::new();
        wait_set.begin(vec![11, 22, 33]);

        assert_eq!(wait_set_position(&wait_set, 11), Some(0));
        assert_eq!(wait_set_position(&wait_set, 33), Some(2));
        assert_eq!(wait_set_position(&wait_set, 44), None);
    }

    #[test]
    fn wait_nodes_preserve_object_registration_identity() {
        let mut wait_set = SynchronizationWaitSet::new();
        wait_set.begin(vec![11, 22, 33]);

        let node = SynchronizationWaitNode {
            object_id: wait_set.objects()[1].object_id,
            handle: SynchronizationWaitNodeHandle {
                thread_id: 9,
                wait_index: wait_set.objects()[1].wait_index,
            },
        };

        assert_eq!(node.object_id, 22);
        assert_eq!(node.handle.wait_index, 1);
    }

    #[test]
    fn wait_set_owns_wait_nodes_after_binding_thread() {
        let mut wait_set = SynchronizationWaitSet::new();
        wait_set.begin(vec![11, 22]);
        wait_set.bind_thread(7);

        assert_eq!(
            wait_set.objects()[0].wait_node,
            Some(SynchronizationWaitNode {
                object_id: 11,
                handle: SynchronizationWaitNodeHandle {
                    thread_id: 7,
                    wait_index: 0,
                },
            })
        );
        assert_eq!(
            wait_set.objects()[1].wait_node,
            Some(SynchronizationWaitNode {
                object_id: 22,
                handle: SynchronizationWaitNodeHandle {
                    thread_id: 7,
                    wait_index: 1,
                },
            })
        );
    }

    #[test]
    fn waiter_storage_preserves_link_order_after_unlink() {
        let mut process = KProcess::new();
        for (thread_id, wait_index) in [(1, 0usize), (2, 1usize), (3, 2usize)] {
            let thread = Arc::new(Mutex::new(KThread::new()));
            {
                let mut thread = thread.lock().unwrap();
                thread.thread_id = thread_id;
                thread.object_id = 100 + thread_id;
                thread.synchronization_wait.begin(vec![20, 21, 22]);
                thread.synchronization_wait.bind_thread(thread_id);
            }
            process.register_thread_object(thread);
            let mut waiters = std::mem::take(&mut process.sync_object.waiters);
            waiters.link(
                &mut process,
                SynchronizationWaitNodeHandle {
                    thread_id,
                    wait_index,
                },
            );
            process.sync_object.waiters = waiters;
        }

        let mut waiters = std::mem::take(&mut process.sync_object.waiters);
        waiters.unlink(&mut process, 2, 1);
        process.sync_object.waiters = waiters;

        assert_eq!(
            process
                .sync_object
                .waiter_node_handles(&process)
                .into_iter()
                .map(|handle| handle.thread_id)
                .collect::<Vec<_>>(),
            vec![1, 3]
        );
    }

    #[test]
    fn consume_signaled_wait_set_unlinks_other_objects() {
        let mut process = KProcess::new();
        process.process_id = 55;

        let readable = Arc::new(Mutex::new(KReadableEvent::new()));
        readable.lock().unwrap().initialize(1, 2);
        process.register_readable_event_object(2, Arc::clone(&readable));

        let thread = Arc::new(Mutex::new(KThread::new()));
        {
            let mut thread_guard = thread.lock().unwrap();
            thread_guard.thread_id = 7;
            thread_guard.object_id = 3;
        }
        process.register_thread_object(Arc::clone(&thread));

        let mut wait_set = SynchronizationWaitSet::new();
        wait_set.begin(vec![2, 3, 55]);
        wait_set.bind_thread(99);
        link_wait_set(&mut process, &wait_set);

        assert_eq!(
            consume_signaled_wait_set(&mut process, 99, &wait_set, 3),
            Some(1)
        );
        assert!(readable.lock().unwrap().sync_object.waiters_are_empty());
        assert_eq!(thread.lock().unwrap().sync_object.waiter_snapshot(&process), vec![99]);
        assert!(process.sync_object.waiters_are_empty());
    }

    #[test]
    fn clear_wait_set_unlinks_all_objects() {
        let mut process = KProcess::new();
        process.process_id = 55;

        let readable = Arc::new(Mutex::new(KReadableEvent::new()));
        readable.lock().unwrap().initialize(1, 2);
        process.register_readable_event_object(2, Arc::clone(&readable));

        let thread = Arc::new(Mutex::new(KThread::new()));
        {
            let mut thread_guard = thread.lock().unwrap();
            thread_guard.thread_id = 7;
            thread_guard.object_id = 3;
        }
        process.register_thread_object(Arc::clone(&thread));

        let mut wait_set = SynchronizationWaitSet::new();
        wait_set.begin(vec![2, 3, 55]);
        wait_set.bind_thread(99);
        link_wait_set(&mut process, &wait_set);

        clear_wait_set(Some(&mut process), 99, &mut wait_set);

        assert!(!wait_set.is_active());
        assert!(readable.lock().unwrap().sync_object.waiters_are_empty());
        assert!(thread.lock().unwrap().sync_object.waiters_are_empty());
        assert!(process.sync_object.waiters_are_empty());
    }

    #[test]
    fn check_wait_ready_uses_thread_wait_set() {
        let mut process = KProcess::new();
        process.process_id = 55;

        let readable = Arc::new(Mutex::new(KReadableEvent::new()));
        readable.lock().unwrap().initialize(1, 2);
        readable.lock().unwrap().is_signaled = true;
        process.register_readable_event_object(2, Arc::clone(&readable));

        let mut thread = KThread::new();
        thread.synchronization_wait.begin(vec![2, 55]);
        thread.synchronization_wait.bind_thread(99);

        assert_eq!(check_wait_ready(&process, &thread), Some(0));
    }

    #[test]
    fn wait_returns_signaled_index() {
        let mut process = KProcess::new();
        process.process_id = 100;
        let scheduler = Arc::new(Mutex::new(KScheduler::new(0)));

        let current_thread = Arc::new(Mutex::new(KThread::new()));
        {
            let mut thread = current_thread.lock().unwrap();
            thread.thread_id = 1;
            thread.object_id = 1;
        }

        let readable = Arc::new(Mutex::new(KReadableEvent::new()));
        readable.lock().unwrap().initialize(7, 2);
        readable.lock().unwrap().is_signaled = true;
        process.register_readable_event_object(2, readable);

        let mut out_index = -1;
        let result = wait(
            &mut process,
            &current_thread,
            &scheduler,
            &mut out_index,
            vec![2],
            0,
        );
        assert_eq!(result, RESULT_SUCCESS);
        assert_eq!(out_index, 0);
    }

    #[test]
    #[should_panic(expected = "KThreadQueueWithoutEndWait::end_wait should never be called")]
    fn synchronization_wait_queue_disallows_direct_end_wait() {
        let queue = ThreadQueueImplForKSynchronizationObjectWait::queue();
        assert!(!queue.end_wait_allowed);

        let mut thread = KThread::new();
        queue.end_wait(&mut thread, RESULT_SUCCESS.get_inner_value());
    }

    #[test]
    fn wait_links_and_blocks_thread() {
        let process = Arc::new(Mutex::new(KProcess::new()));
        {
            let mut guard = process.lock().unwrap();
            guard.process_id = 100;
            let readable = Arc::new(Mutex::new(KReadableEvent::new()));
            readable.lock().unwrap().initialize(7, 2);
            guard.register_readable_event_object(2, readable);
        }

        let scheduler = Arc::new(Mutex::new(KScheduler::new(0)));
        let current_thread = Arc::new(Mutex::new(KThread::new()));
        {
            let mut thread = current_thread.lock().unwrap();
            thread.thread_id = 1;
            thread.object_id = 1;
            thread.parent = Some(Arc::downgrade(&process));
            thread.thread_state.store(
                crate::hle::kernel::k_thread::ThreadState::RUNNABLE.bits(),
                Ordering::Relaxed,
            );
        }
        process
            .lock()
            .unwrap()
            .register_thread_object(current_thread.clone());

        let mut process_guard = process.lock().unwrap();
        let mut out_index = -1;
        let result = wait(
            &mut process_guard,
            &current_thread,
            &scheduler,
            &mut out_index,
            vec![2],
            -1,
        );
        drop(process_guard);

        assert_eq!(result, RESULT_SUCCESS);
        assert_eq!(out_index, -1);
        assert_eq!(
            current_thread.lock().unwrap().get_state(),
            crate::hle::kernel::k_thread::ThreadState::WAITING
        );
        assert_eq!(
            process
                .lock()
                .unwrap()
                .get_readable_event_by_object_id(2)
                .unwrap()
                .lock()
                .unwrap()
                .sync_object
                .waiter_snapshot(&process.lock().unwrap()),
            vec![1]
        );
    }

    #[test]
    fn wait_timeout_zero_returns_timed_out() {
        let mut process = KProcess::new();
        process.process_id = 100;
        let scheduler = Arc::new(Mutex::new(KScheduler::new(0)));
        let current_thread = Arc::new(Mutex::new(KThread::new()));
        {
            let mut thread = current_thread.lock().unwrap();
            thread.thread_id = 1;
            thread.object_id = 1;
        }

        let readable = Arc::new(Mutex::new(KReadableEvent::new()));
        readable.lock().unwrap().initialize(7, 2);
        process.register_readable_event_object(2, readable);

        let mut out_index = 123;
        let result = wait(
            &mut process,
            &current_thread,
            &scheduler,
            &mut out_index,
            vec![2],
            0,
        );
        assert_eq!(result, RESULT_TIMED_OUT);
        assert_eq!(out_index, -1);
    }
}
