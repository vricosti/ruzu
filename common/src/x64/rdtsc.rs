//! Port of zuyu/src/common/x64/rdtsc.h and rdtsc.cpp
//!
//! Fenced RDTSC instruction and TSC frequency estimation.

#[cfg(target_arch = "x86_64")]
use std::time::Duration;

#[cfg(target_arch = "x86_64")]
use crate::steady_clock::RealTimeClock;
#[cfg(target_arch = "x86_64")]
use crate::uint128::multiply_and_divide64;

/// Execute a fenced RDTSC instruction.
///
/// Issues LFENCE before and after RDTSC to ensure serialization,
/// preventing out-of-order execution from affecting the timestamp reading.
#[cfg(target_arch = "x86_64")]
#[inline]
pub fn fenced_rdtsc() -> u64 {
    unsafe {
        // lfence; rdtsc; lfence
        std::arch::x86_64::_mm_lfence();
        let result = std::arch::x86_64::_rdtsc();
        std::arch::x86_64::_mm_lfence();
        result
    }
}

/// Round a value to the nearest multiple of `nearest`.
#[cfg(target_arch = "x86_64")]
#[inline]
fn round_to_nearest(value: u64, nearest: u64) -> u64 {
    let modulus = value % nearest;
    if modulus >= (nearest / 2) {
        value - modulus + nearest
    } else {
        value - modulus
    }
}

/// Estimate the TSC (Time Stamp Counter) frequency by measuring over a 100ms interval.
///
/// This is used as a fallback when CPUID leaf 0x15 doesn't provide a crystal frequency.
/// The first RDTSC measurement is discarded to warm up.
#[cfg(target_arch = "x86_64")]
pub fn estimate_rdtsc_frequency() -> u64 {
    // Discard the first result measuring the rdtsc.
    fenced_rdtsc();
    std::thread::sleep(Duration::from_millis(1));
    fenced_rdtsc();

    // Get the current time.
    let start_time = RealTimeClock::now();
    let tsc_start = fenced_rdtsc();
    // Wait for 100 milliseconds.
    std::thread::sleep(Duration::from_millis(100));
    let end_time = RealTimeClock::now();
    let tsc_end = fenced_rdtsc();

    // Calculate differences.
    let timer_diff = end_time
        .checked_sub(start_time)
        .unwrap_or(Duration::ZERO)
        .as_nanos() as u64;
    let tsc_diff = tsc_end - tsc_start;
    let tsc_freq = multiply_and_divide64(tsc_diff, 1_000_000_000u64, timer_diff);
    round_to_nearest(tsc_freq, 100_000)
}

#[cfg(all(test, target_arch = "x86_64"))]
mod tests {
    use super::*;

    #[test]
    fn test_fenced_rdtsc_monotonic() {
        let t1 = fenced_rdtsc();
        let t2 = fenced_rdtsc();
        assert!(t2 >= t1, "RDTSC should be monotonically increasing");
    }

    #[test]
    fn test_round_to_nearest() {
        assert_eq!(round_to_nearest(1_049_999, 100_000), 1_000_000);
        assert_eq!(round_to_nearest(1_050_000, 100_000), 1_100_000);
        assert_eq!(round_to_nearest(1_100_000, 100_000), 1_100_000);
    }
}
