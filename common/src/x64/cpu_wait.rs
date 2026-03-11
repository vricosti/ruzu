//! Port of zuyu/src/common/x64/cpu_wait.h and cpu_wait.cpp
//!
//! Platform-specific micro-sleep using MWAITX (AMD) or TPAUSE (Intel),
//! falling back to thread yield.

#[cfg(target_arch = "x86_64")]
use crate::x64::cpu_detect::get_cpu_caps;
#[cfg(target_arch = "x86_64")]
use crate::x64::rdtsc::fenced_rdtsc;

// 100,000 cycles is a reasonable amount of time to wait to save on CPU resources.
// For reference:
// At 1 GHz, 100K cycles is 100us
// At 2 GHz, 100K cycles is 50us
// At 4 GHz, 100K cycles is 25us
#[cfg(target_arch = "x86_64")]
const PAUSE_CYCLES: u64 = 100_000;

/// Execute the TPAUSE instruction (Intel WAITPKG).
/// Requests C0.2 state and waits until the TSC reaches the target value.
#[cfg(target_arch = "x86_64")]
#[inline]
unsafe fn tpause() {
    const REQUEST_C02_STATE: u32 = 0;
    let tsc = fenced_rdtsc() + PAUSE_CYCLES;
    let eax = (tsc & 0xFFFF_FFFF) as u32;
    let edx = (tsc >> 32) as u32;
    std::arch::asm!(
        "tpause {ctrl:e}",
        ctrl = in(reg) REQUEST_C02_STATE,
        in("edx") edx,
        in("eax") eax,
    );
}

/// Execute the MWAITX instruction (AMD MONITORX/MWAITX).
/// Sets up a monitor on a cache-line-aligned variable, then waits for the
/// specified number of cycles.
#[cfg(target_arch = "x86_64")]
#[inline]
unsafe fn mwaitx() {
    const ENABLE_WAIT_TIME_FLAG: u32 = 1 << 1;
    const REQUEST_C1_STATE: u32 = 0;

    // monitor_var should be aligned to a cache line.
    #[repr(align(64))]
    struct AlignedVar(u64);
    let monitor_var = AlignedVar(0);

    // rbx is reserved by LLVM, so we must save/restore it manually
    // and move the pause cycles value into rbx inside the asm block.
    let pause_cycles_val = PAUSE_CYCLES as u32;
    std::arch::asm!(
        "monitorx",
        in("rax") &monitor_var as *const AlignedVar,
        in("ecx") 0u32,
        in("edx") 0u32,
    );
    std::arch::asm!(
        "push rbx",
        "mov ebx, {pause:e}",
        "mwaitx",
        "pop rbx",
        pause = in(reg) pause_cycles_val,
        in("eax") REQUEST_C1_STATE,
        in("ecx") ENABLE_WAIT_TIME_FLAG,
    );
}

/// Perform a platform-specific micro-sleep.
///
/// Uses TPAUSE (Intel WAITPKG) if available, MWAITX (AMD) if available,
/// otherwise falls back to `std::thread::yield_now()`.
#[cfg(target_arch = "x86_64")]
pub fn micro_sleep() {
    // Cache capability checks in thread-local storage for fast path.
    // OnceLock would also work, but the C++ uses static local bools.
    use std::sync::OnceLock;
    static HAS_WAITPKG: OnceLock<bool> = OnceLock::new();
    static HAS_MONITORX: OnceLock<bool> = OnceLock::new();

    let has_waitpkg = *HAS_WAITPKG.get_or_init(|| get_cpu_caps().waitpkg);
    let has_monitorx = *HAS_MONITORX.get_or_init(|| get_cpu_caps().monitorx);

    if has_waitpkg {
        // Safety: TPAUSE is available (checked via CPUID).
        unsafe { tpause() };
    } else if has_monitorx {
        // Safety: MONITORX/MWAITX is available (checked via CPUID).
        unsafe { mwaitx() };
    } else {
        std::thread::yield_now();
    }
}

#[cfg(all(test, target_arch = "x86_64"))]
mod tests {
    use super::*;

    #[test]
    fn test_micro_sleep_does_not_panic() {
        // Just verify it doesn't crash. The actual wait behavior depends on
        // CPU capabilities.
        micro_sleep();
    }
}
