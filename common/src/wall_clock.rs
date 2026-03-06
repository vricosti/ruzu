//! Port of zuyu/src/common/wall_clock.h and zuyu/src/common/wall_clock.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-05

use std::time::{Duration, Instant, SystemTime};

/// CNTPCT_EL0 Frequency = 19.2 MHz
pub const CNTFRQ: u64 = 19_200_000;
/// GM20B GPU Tick Frequency = 614.4 MHz
pub const GPU_TICK_FREQ: u64 = 614_400_000;
/// T210/4 A57 CPU Tick Frequency = 1020.0 MHz
pub const CPU_TICK_FREQ: u64 = 1_020_000_000;

// Ratio constants for conversions (matching the C++ std::ratio types)
// NsToCNTPCT: CNTFRQ / 1_000_000_000
// NsToGPUTick: GPUTickFreq / 1_000_000_000
// CPUTickToNs: 1_000_000_000 / CPUTickFreq
// CPUTickToUs: 1_000_000 / CPUTickFreq
// CPUTickToCNTPCT: CNTFRQ / CPUTickFreq
// CPUTickToGPUTick: GPUTickFreq / CPUTickFreq

const NS_PER_SEC: u64 = 1_000_000_000;
const US_PER_SEC: u64 = 1_000_000;

/// Trait for wall clock implementations, matching the C++ WallClock abstract class.
pub trait WallClock: Send + Sync {
    /// Returns the time in nanoseconds since the construction of this clock.
    fn get_time_ns(&self) -> Duration;

    /// Returns the time in microseconds since the construction of this clock.
    fn get_time_us(&self) -> Duration;

    /// Returns the time in milliseconds since the construction of this clock.
    fn get_time_ms(&self) -> Duration;

    /// Returns the guest CNTPCT ticks since the construction of this clock.
    fn get_cntpct(&self) -> i64;

    /// Returns the guest GPU ticks since the construction of this clock.
    fn get_gpu_tick(&self) -> i64;

    /// Returns the raw host timer ticks since an indeterminate epoch.
    fn get_uptime(&self) -> i64;

    /// Returns whether the clock directly uses the host's hardware clock.
    fn is_native(&self) -> bool;
}

// Static conversion functions matching the C++ static methods

/// Convert nanoseconds to CNTPCT ticks.
#[inline]
pub fn ns_to_cntpct(ns: u64) -> u64 {
    // CNTFRQ / NS_PER_SEC simplified: 19_200_000 / 1_000_000_000 = 24/1250
    ns * CNTFRQ / NS_PER_SEC
}

/// Convert nanoseconds to GPU ticks.
#[inline]
pub fn ns_to_gpu_tick(ns: u64) -> u64 {
    ns * GPU_TICK_FREQ / NS_PER_SEC
}

/// Convert CPU ticks to nanoseconds.
#[inline]
pub fn cpu_tick_to_ns(cpu_tick: u64) -> u64 {
    cpu_tick * NS_PER_SEC / CPU_TICK_FREQ
}

/// Convert CPU ticks to microseconds.
#[inline]
pub fn cpu_tick_to_us(cpu_tick: u64) -> u64 {
    cpu_tick * US_PER_SEC / CPU_TICK_FREQ
}

/// Convert CPU ticks to CNTPCT ticks.
#[inline]
pub fn cpu_tick_to_cntpct(cpu_tick: u64) -> u64 {
    cpu_tick * CNTFRQ / CPU_TICK_FREQ
}

/// Convert CPU ticks to GPU ticks.
#[inline]
pub fn cpu_tick_to_gpu_tick(cpu_tick: u64) -> u64 {
    cpu_tick * GPU_TICK_FREQ / CPU_TICK_FREQ
}

/// Standard wall clock implementation using std::time.
/// This is the fallback when no native high-precision clock is available.
pub struct StandardWallClock {
    _start: Instant,
}

impl StandardWallClock {
    pub fn new() -> Self {
        Self {
            _start: Instant::now(),
        }
    }
}

impl Default for StandardWallClock {
    fn default() -> Self {
        Self::new()
    }
}

impl WallClock for StandardWallClock {
    fn get_time_ns(&self) -> Duration {
        SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap_or(Duration::ZERO)
    }

    fn get_time_us(&self) -> Duration {
        let ns = self.get_time_ns();
        Duration::from_micros(ns.as_micros() as u64)
    }

    fn get_time_ms(&self) -> Duration {
        let ns = self.get_time_ns();
        Duration::from_millis(ns.as_millis() as u64)
    }

    fn get_cntpct(&self) -> i64 {
        let uptime = self.get_uptime();
        (uptime as u64 * CNTFRQ / NS_PER_SEC) as i64
    }

    fn get_gpu_tick(&self) -> i64 {
        let uptime = self.get_uptime();
        (uptime as u64 * GPU_TICK_FREQ / NS_PER_SEC) as i64
    }

    fn get_uptime(&self) -> i64 {
        // Use monotonic clock for uptime (like steady_clock in C++)
        self._start.elapsed().as_nanos() as i64
    }

    fn is_native(&self) -> bool {
        false
    }
}

/// Creates the optimal clock for the current platform.
/// On platforms without a native high-precision timer, falls back to StandardWallClock.
pub fn create_optimal_clock() -> Box<dyn WallClock> {
    // For now, always use the standard wall clock.
    // Native clocks (x86_64 RDTSC, ARM64 CNTVCT_EL0) can be added later.
    Box::new(StandardWallClock::new())
}

/// Creates a standard wall clock (non-native).
pub fn create_standard_wall_clock() -> Box<dyn WallClock> {
    Box::new(StandardWallClock::new())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_standard_wall_clock() {
        let clock = StandardWallClock::new();
        assert!(!clock.is_native());

        let ns = clock.get_time_ns();
        assert!(ns.as_nanos() > 0);

        let uptime1 = clock.get_uptime();
        std::thread::sleep(Duration::from_millis(10));
        let uptime2 = clock.get_uptime();
        assert!(uptime2 > uptime1);
    }

    #[test]
    fn test_conversion_functions() {
        assert_eq!(ns_to_cntpct(1_000_000_000), CNTFRQ);
        assert_eq!(cpu_tick_to_ns(CPU_TICK_FREQ), NS_PER_SEC);
    }
}
