// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/application_creator.h
//! Port of zuyu/src/core/hle/service/am/service/application_creator.cpp

/// IPC command table for IApplicationCreator:
/// - 0: CreateApplication
/// - 1: PopLaunchRequestedApplication (unimplemented)
/// - 10: CreateSystemApplication (unimplemented)
/// - 100: PopFloatingApplicationForDevelopment (unimplemented)
pub struct IApplicationCreator {
    // TODO: WindowSystem reference
}

impl IApplicationCreator {
    pub fn new() -> Self {
        Self {}
    }

    /// Port of IApplicationCreator::CreateApplication
    pub fn create_application(&self, _application_id: u64) {
        log::info!("(STUBBED) CreateApplication called");
        // TODO: CreateGuestApplication
    }
}
