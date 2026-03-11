//! ARM64 (AArch64) platform-specific modules.
//!
//! These modules are only compiled on aarch64 targets.

#[cfg(target_arch = "aarch64")]
pub mod native_clock;
