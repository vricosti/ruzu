// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/os/multi_wait_utils.h
//!
//! Utility helpers for MultiWait (variadic wait functions).
//!
//! Upstream uses C++ variadic templates for WaitAny/TryWaitAny helpers.
//! In Rust these are better expressed as slice-based operations on MultiWait.

// The upstream C++ template machinery (AutoMultiWaitHolder, WaitAnyImpl, etc.)
// is a compile-time convenience. In Rust, the equivalent is simply calling
// multi_wait.wait_any() after linking the holders.
