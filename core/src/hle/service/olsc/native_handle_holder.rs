// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/olsc/native_handle_holder.h
//! Port of zuyu/src/core/hle/service/olsc/native_handle_holder.cpp
//!
//! INativeHandleHolder: provides a native handle (KReadableEvent) to callers.

use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// IPC command table for INativeHandleHolder.
///
/// | Cmd | Handler         | Name            |
/// |-----|-----------------|-----------------|
/// | 0   | GetNativeHandle | GetNativeHandle |
pub struct INativeHandleHolder;

impl INativeHandleHolder {
    pub fn new() -> Self {
        INativeHandleHolder
    }

    /// Cmd 0: GetNativeHandle
    ///
    /// Returns a KReadableEvent handle. Upstream returns nullptr (stubbed).
    /// In this port, we return success but the event handle is not yet wired.
    pub fn get_native_handle(&self) -> ResultCode {
        log::warn!("(STUBBED) INativeHandleHolder::get_native_handle called");
        // Upstream: *out_event = nullptr; R_SUCCEED();
        RESULT_SUCCESS
    }
}
