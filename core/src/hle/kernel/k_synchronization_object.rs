//! Port of zuyu/src/core/hle/kernel/k_synchronization_object.h and k_synchronization_object.cpp
//! Status: EN COURS
//! Derniere synchro: 2026-03-11
//!
//! KSynchronizationObject — base class for kernel objects that threads can wait on.
//! Extends KAutoObjectWithList.
//!
//! The Wait() static method and thread queue integration are stubbed until
//! KThread, KScheduler, and KThreadQueue are ported.

use crate::hle::result::ResultCode;
use super::k_auto_object::{KAutoObjectBase, KAutoObjectWithList, TypeObj};
use super::k_class_token;

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

#[cfg(test)]
mod tests {
    use super::*;

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
}
