//! Port of zuyu/src/common/steady_clock.h and zuyu/src/common/steady_clock.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-05
//!
//! Provides monotonic and real-time clock abstractions matching the C++ implementation.

use std::time::Duration;

#[cfg(not(target_os = "linux"))]
use std::time::Instant;

#[cfg(not(target_os = "linux"))]
use std::time::SystemTime;

/// A monotonic clock similar to C++'s `std::chrono::steady_clock`, but using
/// platform-specific high-resolution timers where available.
///
/// On Linux: uses `CLOCK_MONOTONIC` via `Instant`.
/// On other platforms: falls back to `std::time::Instant`.
pub struct SteadyClock;

impl SteadyClock {
    /// Returns the current time as a duration since an unspecified epoch.
    /// This is monotonic and will not go backwards.
    pub fn now() -> Duration {
        // For absolute monotonic time, we use libc directly on Linux
        #[cfg(target_os = "linux")]
        {
            let mut ts = libc::timespec {
                tv_sec: 0,
                tv_nsec: 0,
            };
            unsafe {
                libc::clock_gettime(libc::CLOCK_MONOTONIC, &mut ts);
            }
            Duration::new(ts.tv_sec as u64, ts.tv_nsec as u32)
        }

        #[cfg(not(target_os = "linux"))]
        {
            // Fallback: use Instant elapsed from process start
            // This is still monotonic but epoch is process start
            static START: std::sync::OnceLock<Instant> = std::sync::OnceLock::new();
            let start = START.get_or_init(Instant::now);
            start.elapsed()
        }
    }
}

/// A real-time clock that returns wall clock time.
/// Unlike `SteadyClock`, this can go backwards if the system clock is adjusted.
pub struct RealTimeClock;

impl RealTimeClock {
    /// Returns the current real (wall clock) time as a duration since the Unix epoch.
    pub fn now() -> Duration {
        #[cfg(target_os = "linux")]
        {
            let mut ts = libc::timespec {
                tv_sec: 0,
                tv_nsec: 0,
            };
            unsafe {
                libc::clock_gettime(libc::CLOCK_REALTIME, &mut ts);
            }
            Duration::new(ts.tv_sec as u64, ts.tv_nsec as u32)
        }

        #[cfg(not(target_os = "linux"))]
        {
            SystemTime::now()
                .duration_since(SystemTime::UNIX_EPOCH)
                .unwrap_or(Duration::ZERO)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_steady_clock_monotonic() {
        let t1 = SteadyClock::now();
        std::thread::sleep(Duration::from_millis(10));
        let t2 = SteadyClock::now();
        assert!(t2 > t1);
    }

    #[test]
    fn test_real_time_clock() {
        let t = RealTimeClock::now();
        // Should be after Unix epoch
        assert!(t.as_secs() > 1_000_000_000);
    }
}
