// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/pctl/parental_control_service_factory.h
//! Port of zuyu/src/core/hle/service/pctl/parental_control_service_factory.cpp
//!
//! IParentalControlServiceFactory — creates IParentalControlService sessions.

/// IPC command table for IParentalControlServiceFactory.
pub mod commands {
    pub const CREATE_SERVICE: u32 = 0;
    pub const CREATE_SERVICE_WITHOUT_INITIALIZE: u32 = 1;
}

/// IParentalControlServiceFactory.
///
/// Corresponds to `IParentalControlServiceFactory` in upstream.
pub struct IParentalControlServiceFactory {
    capability: u32,
}

impl IParentalControlServiceFactory {
    pub fn new(capability: u32) -> Self {
        Self { capability }
    }
}
