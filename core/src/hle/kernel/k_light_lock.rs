//! Port of zuyu/src/core/hle/kernel/k_light_lock.h and k_light_lock.cpp
//! Status: Ported
//! Last synchronized: 2026-07-10

use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Condvar, Mutex};

use super::k_scheduler_lock::KScopedSchedulerLock;
use super::k_thread::{KThread, KThreadLock};
use super::k_thread_queue::KThreadQueue;
use super::k_typed_address::KProcessAddress;
use crate::hle::result::RESULT_SUCCESS;

const WAITERS_FLAG: usize = 1;
const HOST_TAG_FLAG: usize = 1usize << (usize::BITS - 1);

/// Lightweight kernel lock with priority inheritance.
///
/// The tag is the stable address of the owning `KThread`; bit zero records
/// whether the owner has waiters. This is the same representation upstream
/// uses with `KThread*`.
pub struct KLightLock {
    m_tag: AtomicUsize,

    // A lock may be exercised before KernelCore has installed a current
    // KThread (notably isolated unit tests). This native fallback is never
    // used by an emulated CPU core and therefore cannot park a guest core.
    host_wait_mutex: Mutex<()>,
    host_wait_cv: Condvar,
}

impl KLightLock {
    pub fn new() -> Self {
        Self {
            m_tag: AtomicUsize::new(0),
            host_wait_mutex: Mutex::new(()),
            host_wait_cv: Condvar::new(),
        }
    }

    /// Matches upstream `KLightLock::Lock()`.
    pub fn lock(&self) {
        let current = super::kernel::get_current_thread_pointer();
        let cur_thread = current
            .as_ref()
            .map_or_else(current_host_tag, |thread| thread.as_ref().as_ptr() as usize);

        loop {
            let mut old_tag = self.m_tag.load(Ordering::Relaxed);

            loop {
                let new_tag = if old_tag == 0 {
                    cur_thread
                } else {
                    old_tag | WAITERS_FLAG
                };

                match self.m_tag.compare_exchange_weak(
                    old_tag,
                    new_tag,
                    Ordering::Acquire,
                    Ordering::Relaxed,
                ) {
                    Ok(_) => break,
                    Err(actual) => old_tag = actual,
                }
            }

            if old_tag == 0
                || self.lock_slow_path(old_tag | WAITERS_FLAG, cur_thread, current.as_ref())
            {
                break;
            }
        }
    }

    /// Matches upstream `KLightLock::Unlock()`.
    pub fn unlock(&self) {
        let current = super::kernel::get_current_thread_pointer();
        let cur_thread = current
            .as_ref()
            .map_or_else(current_host_tag, |thread| thread.as_ref().as_ptr() as usize);

        if self
            .m_tag
            .compare_exchange(cur_thread, 0, Ordering::Release, Ordering::Relaxed)
            .is_err()
        {
            self.unlock_slow_path(cur_thread, current.as_ref());
        }
    }

    /// Matches upstream `KLightLock::LockSlowPath()`.
    fn lock_slow_path(
        &self,
        owner: usize,
        cur_thread: usize,
        current: Option<&Arc<KThreadLock>>,
    ) -> bool {
        let Some(current) = current else {
            return self.lock_host_slow_path(owner);
        };
        if owner & HOST_TAG_FLAG != 0 {
            return self.lock_host_slow_path(owner);
        }

        let scheduler_lock = super::kernel::scheduler_lock()
            .expect("KLightLock slow path requires an initialized scheduler lock");
        let _scheduler_guard = KScopedSchedulerLock::new(scheduler_lock);

        if self.m_tag.load(Ordering::Relaxed) != owner {
            return false;
        }

        let owner_ptr = (owner & !WAITERS_FLAG) as *mut KThread;
        let key = KProcessAddress::new((&self.m_tag as *const AtomicUsize) as u64);
        let (waiter_id, waiter_priority) = {
            let mut waiter = current.lock().unwrap();
            waiter.set_kernel_address_key(key);
            (waiter.get_thread_id(), waiter.get_priority())
        };

        // SAFETY: KThread storage is stable inside SyncCell and both owner and
        // waiter accesses are serialized by the scheduler lock, as upstream.
        let owner_thread = unsafe { &mut *owner_ptr };
        owner_thread.add_waiter(current, waiter_id, waiter_priority, key, true);
        current
            .lock()
            .unwrap()
            .begin_wait_with_queue(light_lock_wait_queue());

        if owner_thread.is_suspended() {
            owner_thread.continue_if_has_kernel_waiters();
        }

        // Dropping the scheduler lock switches away from this WAITING fiber.
        // This call resumes only after UnlockSlowPath transfers ownership.
        let _ = cur_thread;
        true
    }

    fn lock_host_slow_path(&self, owner: usize) -> bool {
        if self.m_tag.load(Ordering::Relaxed) != owner {
            return false;
        }
        let guard = self.host_wait_mutex.lock().unwrap();
        let _guard = self
            .host_wait_cv
            .wait_while(guard, |_| self.m_tag.load(Ordering::Relaxed) != 0)
            .unwrap();
        false
    }

    /// Matches upstream `KLightLock::UnlockSlowPath()`.
    fn unlock_slow_path(&self, cur_thread: usize, current: Option<&Arc<KThreadLock>>) {
        if current.is_none() || cur_thread & HOST_TAG_FLAG != 0 {
            self.m_tag.store(0, Ordering::Release);
            self.host_wait_cv.notify_all();
            return;
        }

        let scheduler_lock = super::kernel::scheduler_lock()
            .expect("KLightLock slow path requires an initialized scheduler lock");
        let _scheduler_guard = KScopedSchedulerLock::new(scheduler_lock);
        let owner_ptr = cur_thread as *mut KThread;
        let key = KProcessAddress::new((&self.m_tag as *const AtomicUsize) as u64);

        // SAFETY: `cur_thread` is the stable SyncCell address of the current
        // KThread, and the scheduler lock serializes this access.
        let owner_thread = unsafe { &mut *owner_ptr };
        let mut has_waiters = false;
        let next_owner = owner_thread.remove_waiter_by_key(key, true, &mut has_waiters);

        let mut next_tag = 0;
        if let Some((next_owner_id, _priority, transfer_lock_info)) = next_owner {
            let next_owner = find_thread(next_owner_id)
                .expect("KLightLock waiter disappeared while scheduler lock was held");
            let next_owner_ptr = next_owner.as_ref().as_ptr() as usize;

            if let Some(lock_info) = transfer_lock_info {
                let remaining_waiters = lock_info.waiter_keys();
                let waiter_count = lock_info.get_waiter_count();
                {
                    let mut next = next_owner.lock().unwrap();
                    next.add_held_lock(lock_info);
                    next.add_transferred_kernel_waiters(waiter_count);
                }
                for waiter in remaining_waiters {
                    if let Some(waiter_thread) = find_thread(waiter.thread_id) {
                        waiter_thread
                            .lock()
                            .unwrap()
                            .set_waiting_lock_owner_thread_id(
                                Some(next_owner_id),
                                next_owner_ptr,
                            );
                    }
                }
            }

            {
                let mut next = next_owner.lock().unwrap();
                next.end_wait(RESULT_SUCCESS.get_inner_value());
                if next.is_suspended() {
                    next.continue_if_has_kernel_waiters();
                }
                if next.is_dummy_thread() {
                    next.dummy_thread_end_wait();
                }
            }

            next_tag = next_owner_ptr | usize::from(has_waiters);
        }

        if owner_thread.is_suspended() {
            owner_thread.try_suspend();
        }
        self.m_tag.store(next_tag, Ordering::Release);
    }

    pub fn is_locked(&self) -> bool {
        self.m_tag.load(Ordering::Relaxed) != 0
    }

    pub fn is_locked_by_current_thread(&self) -> bool {
        let current = super::kernel::get_current_thread_pointer();
        let cur_thread = current
            .as_ref()
            .map_or_else(current_host_tag, |thread| thread.as_ref().as_ptr() as usize);
        (self.m_tag.load(Ordering::Relaxed) | WAITERS_FLAG) == (cur_thread | WAITERS_FLAG)
    }
}

impl Default for KLightLock {
    fn default() -> Self {
        Self::new()
    }
}

fn light_lock_wait_queue() -> KThreadQueue {
    KThreadQueue::with_callbacks(None, Some(cancel_light_lock_wait))
}

fn cancel_light_lock_wait(waiting_thread: &mut KThread) {
    let Some(owner_ptr) = waiting_thread.get_lock_owner_raw() else {
        return;
    };
    let waiter_id = waiting_thread.get_thread_id();
    let waiter_priority = waiting_thread.get_priority();
    let address_key = waiting_thread.get_address_key();

    // SAFETY: KThreadQueue cancellation runs under the scheduler lock and the
    // waiting-lock reference retains the stable owner pointer.
    unsafe {
        (*owner_ptr).remove_waiter(waiter_id, waiter_priority, true, address_key);
    }
}

fn find_thread(thread_id: u64) -> Option<Arc<KThreadLock>> {
    let gsc = super::kernel::get_kernel_ref()?
        .global_scheduler_context()?
        .clone();
    let thread = gsc.lock().unwrap().get_thread_by_thread_id(thread_id);
    thread
}

fn current_host_tag() -> usize {
    thread_local! {
        static HOST_TAG: usize = {
            static NEXT_TAG: AtomicUsize = AtomicUsize::new(2);
            HOST_TAG_FLAG | NEXT_TAG.fetch_add(2, Ordering::Relaxed)
        };
    }
    HOST_TAG.with(|tag| *tag)
}

pub struct KScopedLightLock<'a> {
    lock: &'a KLightLock,
}

impl<'a> KScopedLightLock<'a> {
    pub fn new(lock: &'a KLightLock) -> Self {
        lock.lock();
        Self { lock }
    }
}

impl Drop for KScopedLightLock<'_> {
    fn drop(&mut self) {
        self.lock.unlock();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn light_lock_basic() {
        let lock = KLightLock::new();
        assert!(!lock.is_locked());
        lock.lock();
        assert!(lock.is_locked());
        assert!(lock.is_locked_by_current_thread());
        lock.unlock();
        assert!(!lock.is_locked());
    }

    #[test]
    fn scoped_light_lock_unlocks_on_drop() {
        let lock = KLightLock::new();
        {
            let _guard = KScopedLightLock::new(&lock);
            assert!(lock.is_locked());
        }
        assert!(!lock.is_locked());
    }
}
