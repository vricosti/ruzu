//! x86_64 platform-specific modules.
//!
//! These modules are only compiled on x86_64 targets.

#[cfg(target_arch = "x86_64")]
pub mod cpu_detect;

#[cfg(target_arch = "x86_64")]
pub mod cpu_wait;

#[cfg(target_arch = "x86_64")]
pub mod native_clock;

#[cfg(target_arch = "x86_64")]
pub mod rdtsc;
