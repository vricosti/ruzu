// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/bcat/delivery_cache_progress_service.h
//! Port of zuyu/src/core/hle/service/bcat/delivery_cache_progress_service.cpp

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use super::bcat_types::DeliveryCacheProgressImpl;

/// IPC command IDs for IDeliveryCacheProgressService
pub mod commands {
    pub const GET_EVENT: u32 = 0;
    pub const GET_IMPL: u32 = 1;
}

/// IDeliveryCacheProgressService corresponds to upstream `IDeliveryCacheProgressService`.
pub struct IDeliveryCacheProgressService {
    // TODO: event reference
    pub impl_data: DeliveryCacheProgressImpl,
}

impl IDeliveryCacheProgressService {
    pub fn new(impl_data: DeliveryCacheProgressImpl) -> Self {
        Self { impl_data }
    }

    pub fn get_event(&self) -> ResultCode {
        log::debug!("IDeliveryCacheProgressService::get_event called");
        // TODO: return event handle
        RESULT_SUCCESS
    }

    pub fn get_impl(&self) -> (ResultCode, &DeliveryCacheProgressImpl) {
        log::debug!("IDeliveryCacheProgressService::get_impl called");
        (RESULT_SUCCESS, &self.impl_data)
    }
}
