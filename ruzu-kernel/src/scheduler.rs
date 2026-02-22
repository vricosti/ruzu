// SPDX-FileCopyrightText: 2025 ruzu contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Priority-aware round-robin scheduler.
//! Selects the highest-priority runnable thread, round-robining among
//! threads of equal priority. Also handles timeout-based wakeups.

use ruzu_common::{error, Handle, ResultCode};

use crate::thread::{KThread, ThreadState, WaitReason};

/// Priority-aware round-robin scheduler.
pub struct Scheduler {
    /// Handle of the currently running thread.
    pub current_thread: Option<Handle>,
    /// Index used for round-robin among equal-priority threads.
    last_scheduled_idx: usize,
}

impl Scheduler {
    pub fn new() -> Self {
        Self {
            current_thread: None,
            last_scheduled_idx: 0,
        }
    }

    /// Select the next runnable thread index from thread metadata.
    ///
    /// `threads` is a slice of `(handle, state, priority)` tuples indexed by
    /// thread position in the process's thread vector.
    ///
    /// Returns `Some(index)` for the selected thread, or `None` if no threads
    /// are runnable.
    pub fn schedule_next(&mut self, threads: &[(Handle, ThreadState, u32)]) -> Option<usize> {
        // Collect runnable threads: (original_index, priority)
        let runnable: Vec<(usize, u32)> = threads
            .iter()
            .enumerate()
            .filter(|(_, (_, state, _))| *state == ThreadState::Runnable)
            .map(|(i, (_, _, priority))| (i, *priority))
            .collect();

        if runnable.is_empty() {
            self.current_thread = None;
            return None;
        }

        // Find highest priority (lowest numeric value = highest priority)
        let best_priority = runnable.iter().map(|(_, p)| *p).min().unwrap();

        // Among equal-priority threads, round-robin from last_scheduled_idx
        let candidates: Vec<usize> = runnable
            .iter()
            .filter(|(_, p)| *p == best_priority)
            .map(|(i, _)| *i)
            .collect();

        // Pick the first candidate whose index is greater than last_scheduled_idx,
        // wrapping around if none found.
        let next_idx = candidates
            .iter()
            .find(|&&i| i > self.last_scheduled_idx)
            .copied()
            .unwrap_or(candidates[0]);

        self.last_scheduled_idx = next_idx;
        self.current_thread = Some(threads[next_idx].0);
        Some(next_idx)
    }

    /// Check all waiting threads for timeout expiry and wake them.
    ///
    /// This is an associated function (no &self) to avoid borrow conflicts
    /// with other KernelCore fields.
    pub fn check_timeouts(threads: &mut [KThread], current_tick: u64) {
        for thread in threads.iter_mut() {
            if thread.state != ThreadState::Waiting {
                continue;
            }
            match &thread.wait_reason {
                WaitReason::Synchronization { timeout_ns, .. } => {
                    if *timeout_ns > 0 {
                        // 1 tick ≈ 1 ns (assuming ~1 GHz emulated CPU)
                        let timeout_ticks = *timeout_ns as u64;
                        if current_tick >= thread.wait_start_tick + timeout_ticks {
                            thread.wake(error::TIMEOUT.raw(), -1);
                        }
                    }
                    // timeout_ns == -1 means infinite wait, no timeout
                }
                WaitReason::Sleep { wake_tick } => {
                    if current_tick >= *wake_tick {
                        thread.wake(ResultCode::SUCCESS.raw(), -1);
                    }
                }
                _ => {
                    // ArbitrateLock, CondVar, AddressArbiter: no timeout, woken explicitly
                }
            }
        }
    }
}

impl Default for Scheduler {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::thread::ThreadState;

    #[test]
    fn test_schedule_picks_highest_priority() {
        let mut sched = Scheduler::new();
        let threads = vec![
            (1, ThreadState::Runnable, 44), // lower priority
            (2, ThreadState::Runnable, 20), // higher priority
            (3, ThreadState::Waiting, 10),  // highest but waiting
        ];
        let idx = sched.schedule_next(&threads);
        assert_eq!(idx, Some(1)); // thread at index 1 with priority 20
    }

    #[test]
    fn test_round_robin_equal_priority() {
        let mut sched = Scheduler::new();
        let threads = vec![
            (1, ThreadState::Runnable, 44),
            (2, ThreadState::Runnable, 44),
            (3, ThreadState::Runnable, 44),
        ];

        // First schedule: picks index 0 (first > last_scheduled_idx which starts at 0)
        // Actually, last_scheduled_idx starts at 0, first candidate > 0 is index 1
        let idx1 = sched.schedule_next(&threads);
        assert_eq!(idx1, Some(1));

        let idx2 = sched.schedule_next(&threads);
        assert_eq!(idx2, Some(2));

        // Wrap around
        let idx3 = sched.schedule_next(&threads);
        assert_eq!(idx3, Some(0));
    }

    #[test]
    fn test_no_runnable_threads() {
        let mut sched = Scheduler::new();
        let threads = vec![
            (1, ThreadState::Waiting, 44),
            (2, ThreadState::Terminated, 44),
        ];
        assert_eq!(sched.schedule_next(&threads), None);
    }

    #[test]
    fn test_timeout_wakeup() {
        let mut thread = KThread::new(1, 0x1000, 0x8000, 44, 0);
        thread.start();
        thread.begin_wait(
            WaitReason::Synchronization {
                handles: vec![1],
                timeout_ns: 100_000, // 100µs = 100,000 ticks
            },
            1_000_000, // started at tick 1M
        );

        // Not yet expired
        Scheduler::check_timeouts(std::slice::from_mut(&mut thread), 1_050_000);
        assert_eq!(thread.state, ThreadState::Waiting);

        // Now expired
        Scheduler::check_timeouts(std::slice::from_mut(&mut thread), 1_100_000);
        assert_eq!(thread.state, ThreadState::Runnable);
        assert_eq!(thread.wait_result, error::TIMEOUT.raw());
    }

    #[test]
    fn test_sleep_wakeup() {
        let mut thread = KThread::new(1, 0x1000, 0x8000, 44, 0);
        thread.start();
        thread.begin_wait(WaitReason::Sleep { wake_tick: 500 }, 100);

        Scheduler::check_timeouts(std::slice::from_mut(&mut thread), 400);
        assert_eq!(thread.state, ThreadState::Waiting);

        Scheduler::check_timeouts(std::slice::from_mut(&mut thread), 500);
        assert_eq!(thread.state, ThreadState::Runnable);
    }
}
