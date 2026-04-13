//! Port of zuyu/src/common/thread.h and zuyu/src/common/thread.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-05

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Condvar, Mutex};
use std::time::Duration;

/// Thread priority levels, matching the C++ enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum ThreadPriority {
    Low = 0,
    Normal = 1,
    High = 2,
    VeryHigh = 3,
    Critical = 4,
}

/// Set the current thread's scheduling priority.
/// On Linux, uses pthread_setschedparam with SCHED_OTHER.
pub fn set_current_thread_priority(new_priority: ThreadPriority) {
    #[cfg(target_os = "linux")]
    {
        unsafe {
            let this_thread = libc::pthread_self();
            let scheduling_type = libc::SCHED_OTHER;
            let max_prio = libc::sched_get_priority_max(scheduling_type);
            let min_prio = libc::sched_get_priority_min(scheduling_type);
            let level = std::cmp::max(new_priority as u32 + 1, 4);

            let priority = if max_prio > min_prio {
                min_prio + ((max_prio - min_prio) * level as i32) / 4
            } else {
                min_prio - ((min_prio - max_prio) * level as i32) / 4
            };

            let params = libc::sched_param {
                sched_priority: priority,
            };
            libc::pthread_setschedparam(this_thread, scheduling_type, &params);
        }
    }

    #[cfg(not(target_os = "linux"))]
    {
        let _ = new_priority;
        // No-op on unsupported platforms
    }
}

/// Set the current thread's name (visible in debuggers).
/// On Linux, truncates to 15 characters as required by pthread_setname_np.
pub fn set_current_thread_name(name: &str) {
    #[cfg(target_os = "linux")]
    {
        use std::ffi::CString;

        // Linux limits thread names to 15 characters
        let truncated: String = name.chars().take(15).collect();
        if let Ok(c_name) = CString::new(truncated) {
            unsafe {
                libc::pthread_setname_np(libc::pthread_self(), c_name.as_ptr());
            }
        }
    }

    #[cfg(target_os = "macos")]
    {
        use std::ffi::CString;
        if let Ok(c_name) = CString::new(name) {
            unsafe {
                libc::pthread_setname_np(c_name.as_ptr());
            }
        }
    }

    #[cfg(not(any(target_os = "linux", target_os = "macos")))]
    {
        let _ = name;
    }
}

/// An event that can be set and waited on, matching the C++ Common::Event.
pub struct Event {
    mutex: Mutex<()>,
    condvar: Condvar,
    is_set: AtomicBool,
}

impl Event {
    pub fn new() -> Self {
        Self {
            mutex: Mutex::new(()),
            condvar: Condvar::new(),
            is_set: AtomicBool::new(false),
        }
    }

    pub fn is_set_peek(&self) -> bool {
        self.is_set.load(Ordering::SeqCst)
    }

    pub fn set(&self) {
        let _lk = self.mutex.lock().unwrap();
        self.is_set.store(true, Ordering::SeqCst);
        self.condvar.notify_one();
    }

    pub fn wait(&self) {
        let lk = self.mutex.lock().unwrap();
        let _lk = self
            .condvar
            .wait_while(lk, |_| !self.is_set.load(Ordering::SeqCst))
            .unwrap();
        self.is_set.store(false, Ordering::SeqCst);
    }

    pub fn wait_for(&self, duration: Duration) -> bool {
        let lk = self.mutex.lock().unwrap();
        let result = self
            .condvar
            .wait_timeout_while(lk, duration, |_| !self.is_set.load(Ordering::SeqCst));
        match result {
            Ok((_guard, timeout_result)) => {
                if timeout_result.timed_out() {
                    return false;
                }
                self.is_set.store(false, Ordering::SeqCst);
                true
            }
            Err(_) => false,
        }
    }

    pub fn reset(&self) {
        let _lk = self.mutex.lock().unwrap();
        self.is_set.store(false, Ordering::SeqCst);
    }

    pub fn is_set(&self) -> bool {
        self.is_set.load(Ordering::SeqCst)
    }
}

impl Default for Event {
    fn default() -> Self {
        Self::new()
    }
}

/// A barrier that blocks until all `count` threads have called `sync()`.
pub struct Barrier {
    mutex: Mutex<BarrierState>,
    condvar: Condvar,
    count: usize,
}

struct BarrierState {
    waiting: usize,
    generation: usize,
}

impl Barrier {
    pub fn new(count: usize) -> Self {
        Self {
            mutex: Mutex::new(BarrierState {
                waiting: 0,
                generation: 0,
            }),
            condvar: Condvar::new(),
            count,
        }
    }

    /// Blocks until all `count` threads have called sync().
    /// Returns true for all threads when the barrier is reached.
    pub fn sync(&self) -> bool {
        let mut state = self.mutex.lock().unwrap();
        let current_generation = state.generation;

        state.waiting += 1;
        if state.waiting == self.count {
            state.generation += 1;
            state.waiting = 0;
            self.condvar.notify_all();
            true
        } else {
            while current_generation == state.generation {
                state = self.condvar.wait(state).unwrap();
            }
            true
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_event_set_wait() {
        let event = Event::new();
        event.set();
        // Should return immediately since it's already set
        event.wait();
        assert!(!event.is_set());
    }

    #[test]
    fn test_event_wait_for_timeout() {
        let event = Event::new();
        let result = event.wait_for(Duration::from_millis(10));
        assert!(!result);
    }

    #[test]
    fn test_set_thread_name() {
        set_current_thread_name("test_thread");
    }
}
