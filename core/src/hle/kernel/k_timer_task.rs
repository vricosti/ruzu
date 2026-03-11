//! Port of zuyu/src/core/hle/kernel/k_timer_task.h
//! Status: EN COURS
//! Derniere synchro: 2026-03-11
//!
//! KTimerTask — base for objects that can be registered as timed tasks.
//! In upstream, this inherits from IntrusiveRedBlackTreeBaseNode for
//! insertion into a red-black tree sorted by time.
//!
//! The `OnTimer()` method is declared here but implemented inline in k_thread.h
//! (it was devirtualized in kernel 13.0.0 since only KThread uses it).

/// KTimerTask — timer task node for the hardware timer's red-black tree.
///
/// Mirrors upstream `Kernel::KTimerTask`.
/// The intrusive red-black tree node is represented by Ord/Eq implementations
/// for use with BTreeSet or similar sorted containers.
#[derive(Debug, Clone)]
pub struct KTimerTask {
    /// Absolute time in nanoseconds.
    m_time: i64,
}

impl KTimerTask {
    pub const fn new() -> Self {
        Self { m_time: 0 }
    }

    /// Set the absolute time for this timer task.
    /// Mirrors upstream `SetTime(s64 t)`.
    pub fn set_time(&mut self, t: i64) {
        self.m_time = t;
    }

    /// Get the absolute time for this timer task.
    /// Mirrors upstream `GetTime() const`.
    pub const fn get_time(&self) -> i64 {
        self.m_time
    }

    /// Compare two timer tasks by time.
    /// Mirrors upstream `static constexpr int Compare(...)`.
    /// Returns -1 if lhs < rhs, 1 otherwise (never returns 0, matching upstream).
    pub fn compare(lhs: &KTimerTask, rhs: &KTimerTask) -> i32 {
        if lhs.m_time < rhs.m_time {
            -1
        } else {
            1
        }
    }

    /// Called when the timer fires.
    /// NOTE: In upstream, this is virtual but was devirtualized since only KThread
    /// uses it. The actual implementation is in KThread.
    ///
    /// TODO: Implement via KThread::OnTimer() when KThread is ported.
    pub fn on_timer(&self) {
        todo!("KTimerTask::OnTimer requires KThread implementation");
    }
}

impl Default for KTimerTask {
    fn default() -> Self {
        Self::new()
    }
}

impl PartialEq for KTimerTask {
    fn eq(&self, other: &Self) -> bool {
        self.m_time == other.m_time
    }
}

impl Eq for KTimerTask {}

impl PartialOrd for KTimerTask {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for KTimerTask {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // Match upstream: strictly less-than, with equal times ordered arbitrarily
        self.m_time.cmp(&other.m_time)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_timer_task_default() {
        let task = KTimerTask::new();
        assert_eq!(task.get_time(), 0);
    }

    #[test]
    fn test_timer_task_set_get_time() {
        let mut task = KTimerTask::new();
        task.set_time(12345);
        assert_eq!(task.get_time(), 12345);
    }

    #[test]
    fn test_timer_task_compare() {
        let a = KTimerTask { m_time: 100 };
        let b = KTimerTask { m_time: 200 };
        assert_eq!(KTimerTask::compare(&a, &b), -1);
        assert_eq!(KTimerTask::compare(&b, &a), 1);
        // Equal times: upstream returns 1 (never 0)
        assert_eq!(KTimerTask::compare(&a, &a), 1);
    }

    #[test]
    fn test_timer_task_ordering() {
        let a = KTimerTask { m_time: 100 };
        let b = KTimerTask { m_time: 200 };
        assert!(a < b);
    }
}
