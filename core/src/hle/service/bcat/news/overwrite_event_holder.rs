// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/bcat/news/overwrite_event_holder.h
//! Port of zuyu/src/core/hle/service/bcat/news/overwrite_event_holder.cpp

use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// IPC command IDs for IOverwriteEventHolder
pub mod commands {
    pub const GET: u32 = 0;
}

/// IOverwriteEventHolder corresponds to upstream `News::IOverwriteEventHolder`.
pub struct IOverwriteEventHolder {
    // TODO: overwrite_event: KEvent, service_context
}

impl IOverwriteEventHolder {
    pub fn new() -> Self {
        Self {}
    }

    pub fn get(&self) -> ResultCode {
        log::info!("IOverwriteEventHolder::get called");
        // TODO: return overwrite_event readable event handle
        RESULT_SUCCESS
    }
}
