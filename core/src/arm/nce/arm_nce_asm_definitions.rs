// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/arm/nce/arm_nce_asm_definitions.h
//! Constants used by the NCE assembly code and verified via static assertions
//! against struct offsets.

/// Signal used to return to run code by exception level change.
/// Corresponds to `ReturnToRunCodeByExceptionLevelChangeSignal` (SIGUSR2).
pub const RETURN_TO_RUN_CODE_BY_EXCEPTION_LEVEL_CHANGE_SIGNAL: i32 = libc::SIGUSR2;

/// Signal used to break from run code.
/// Corresponds to `BreakFromRunCodeSignal` (SIGURG).
pub const BREAK_FROM_RUN_CODE_SIGNAL: i32 = libc::SIGURG;

/// Signal used for guest access faults.
/// Corresponds to `GuestAccessFaultSignal` (SIGSEGV).
pub const GUEST_ACCESS_FAULT_SIGNAL: i32 = libc::SIGSEGV;

/// Signal used for guest alignment faults.
/// Corresponds to `GuestAlignmentFaultSignal` (SIGBUS).
pub const GUEST_ALIGNMENT_FAULT_SIGNAL: i32 = libc::SIGBUS;

/// Offset of `sp` field in `GuestContext`.
/// Corresponds to `GuestContextSp` (0xF8).
pub const GUEST_CONTEXT_SP: usize = 0xF8;

/// Offset of `host_ctx` field in `GuestContext`.
/// Corresponds to `GuestContextHostContext` (0x320).
pub const GUEST_CONTEXT_HOST_CONTEXT: usize = 0x320;

/// Offset of `host_sp` within `HostContext` (also covers tpidr_el0 at +8).
/// Corresponds to `HostContextSpTpidrEl0` (0xE0).
pub const HOST_CONTEXT_SP_TPIDR_EL0: usize = 0xE0;

/// Offset of `host_tpidr_el0` within `HostContext`.
/// Corresponds to `HostContextTpidrEl0` (0xE8).
pub const HOST_CONTEXT_TPIDR_EL0: usize = 0xE8;

/// Offset of `host_saved_regs` within `HostContext`.
/// Corresponds to `HostContextRegs` (0x0).
pub const HOST_CONTEXT_REGS: usize = 0x0;

/// Offset of `host_saved_vregs` within `HostContext`.
/// Corresponds to `HostContextVregs` (0x60).
pub const HOST_CONTEXT_VREGS: usize = 0x60;

/// Offset of `native_context` within `NativeExecutionParameters`.
/// Corresponds to `TpidrEl0NativeContext` (0x10).
pub const TPIDR_EL0_NATIVE_CONTEXT: usize = 0x10;

/// Offset of `lock` within `NativeExecutionParameters`.
/// Corresponds to `TpidrEl0Lock` (0x18).
pub const TPIDR_EL0_LOCK: usize = 0x18;

/// Offset of `magic` within `NativeExecutionParameters`.
/// Corresponds to `TpidrEl0TlsMagic` (0x20).
pub const TPIDR_EL0_TLS_MAGIC: usize = 0x20;

/// TLS magic value for identifying native execution parameters.
/// Corresponds to `TlsMagic` (0x555a5559, "YUZU" in little-endian).
pub const TLS_MAGIC: u32 = 0x555a5559;

/// Spin lock locked value.
/// Corresponds to `SpinLockLocked` (0).
pub const SPIN_LOCK_LOCKED: u32 = 0;

/// Spin lock unlocked value.
/// Corresponds to `SpinLockUnlocked` (1).
pub const SPIN_LOCK_UNLOCKED: u32 = 1;

/// Signal stack size.
/// Corresponds to upstream `StackSize` (128 KiB).
pub const STACK_SIZE: usize = 128 * 1024;
