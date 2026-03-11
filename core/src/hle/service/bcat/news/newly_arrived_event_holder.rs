// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/bcat/news/newly_arrived_event_holder.h
//! Port of zuyu/src/core/hle/service/bcat/news/newly_arrived_event_holder.cpp

use crate::hle::result::{ResultCode, RESULT_SUCCESS};

/// IPC command IDs for INewlyArrivedEventHolder
pub mod commands {
    pub const GET: u32 = 0;
}

/// INewlyArrivedEventHolder corresponds to upstream `News::INewlyArrivedEventHolder`.
pub struct INewlyArrivedEventHolder {
    // TODO: arrived_event: KEvent, service_context
}

impl INewlyArrivedEventHolder {
    pub fn new() -> Self {
        Self {}
    }

    pub fn get(&self) -> ResultCode {
        log::info!("INewlyArrivedEventHolder::get called");
        // TODO: return arrived_event readable event handle
        RESULT_SUCCESS
    }
}
