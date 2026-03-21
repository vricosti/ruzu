// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/application_creator.h
//! Port of zuyu/src/core/hle/service/am/service/application_creator.cpp

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IApplicationCreator:
/// - 0: CreateApplication
/// - 1: PopLaunchRequestedApplication (unimplemented)
/// - 10: CreateSystemApplication (unimplemented)
/// - 100: PopFloatingApplicationForDevelopment (unimplemented)
pub struct IApplicationCreator {
    window_system: Arc<Mutex<crate::hle::service::am::window_system::WindowSystem>>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IApplicationCreator {
    pub fn new(window_system: Arc<Mutex<crate::hle::service::am::window_system::WindowSystem>>) -> Self {
        let handlers = build_handler_map(&[
            (0, None, "CreateApplication"),
            (1, None, "PopLaunchRequestedApplication"),
            (10, None, "CreateSystemApplication"),
            (100, None, "PopFloatingApplicationForDevelopment"),
        ]);
        Self {
            window_system,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// Port of IApplicationCreator::CreateApplication
    pub fn create_application(&self, _application_id: u64) {
        log::info!("(STUBBED) CreateApplication called");
        // TODO: CreateGuestApplication
    }
}

impl SessionRequestHandler for IApplicationCreator {
    fn handle_sync_request(&self, context: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, context)
    }

    fn service_name(&self) -> &str {
        "am::IApplicationCreator"
    }
}

impl ServiceFramework for IApplicationCreator {
    fn get_service_name(&self) -> &str {
        "am::IApplicationCreator"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
