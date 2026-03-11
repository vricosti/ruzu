//! Port of zuyu/src/core/hle/kernel/svc/svc_kernel_debug.cpp
//! Status: COMPLET
//! Derniere synchro: 2026-03-11
//!
//! SVC handlers for kernel debug operations.
//! These are intentional no-ops in released kernel binaries.

use crate::hle::kernel::svc::svc_types::*;

/// Kernel debug — intentionally does nothing in released kernel binaries.
pub fn kernel_debug(_kernel_debug_type: KernelDebugType, _arg0: u64, _arg1: u64, _arg2: u64) {
    // Intentionally do nothing.
}

/// Change kernel trace state — intentionally does nothing in released kernel binaries.
pub fn change_kernel_trace_state(_trace_state: KernelTraceState) {
    // Intentionally do nothing.
}
