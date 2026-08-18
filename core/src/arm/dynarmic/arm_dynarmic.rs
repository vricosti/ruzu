// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/arm/dynarmic/arm_dynarmic.h and arm_dynarmic.cpp
//! Common dynarmic base: halt reason translation.

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
