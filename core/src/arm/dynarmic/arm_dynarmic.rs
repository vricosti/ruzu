// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/arm/dynarmic/arm_dynarmic.h
//! Common dynarmic base: halt reason translation and JIT execution scoping.

use crate::arm::arm_interface::HaltReason;

/// Dynarmic halt reason constants.
/// These map Core::HaltReason values to Dynarmic::HaltReason values.
/// Upstream has static_asserts confirming these match.
pub const STEP_THREAD: u64 = HaltReason::STEP_THREAD.bits();
pub const DATA_ABORT: u64 = HaltReason::DATA_ABORT.bits();
pub const BREAK_LOOP: u64 = HaltReason::BREAK_LOOP.bits();
pub const SUPERVISOR_CALL: u64 = HaltReason::SUPERVISOR_CALL.bits();
pub const INSTRUCTION_BREAKPOINT: u64 = HaltReason::INSTRUCTION_BREAKPOINT.bits();
pub const PREFETCH_ABORT: u64 = HaltReason::PREFETCH_ABORT.bits();

/// Translate a raw dynarmic halt reason value to our HaltReason bitflags.
///
/// Corresponds to upstream `Core::TranslateHaltReason`.
pub fn translate_halt_reason(hr: u64) -> HaltReason {
    HaltReason::from_bits_truncate(hr)
}

/// RAII guard for JIT execution scope.
///
/// On Linux, upstream registers a SIGBUS handler and sets up memory mapping
/// for the JIT code region. On other platforms this is a no-op.
///
/// Corresponds to upstream `Core::ScopedJitExecution`.
pub struct ScopedJitExecution {
    _private: (),
}

impl ScopedJitExecution {
    pub fn new(_process: &dyn std::any::Any) -> Self {
        // TODO: On Linux, set up the JIT execution context.
        // On other platforms this is a no-op.
        Self { _private: () }
    }

    /// Register signal handlers for JIT execution.
    ///
    /// Corresponds to upstream `ScopedJitExecution::RegisterHandler`.
    pub fn register_handler() {
        // TODO: On Linux, register SIGBUS handler.
    }
}

impl Drop for ScopedJitExecution {
    fn drop(&mut self) {
        // TODO: On Linux, tear down the JIT execution context.
    }
}
