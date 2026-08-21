// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/ldn/system_local_communication_service.h
//! Port of zuyu/src/core/hle/service/ldn/system_local_communication_service.cpp
//!
//! ISystemLocalCommunicationService: system-level LDN communication service.

use std::collections::BTreeMap;

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for ISystemLocalCommunicationService.
///
/// | Cmd | Handler           | Name                              |
/// |-----|------------------|-----------------------------------|
/// | 0   | nullptr          | GetState                          |
/// | 1   | nullptr          | GetNetworkInfo                    |
/// | 2   | nullptr          | GetIpv4Address                    |
/// | 3   | nullptr          | GetDisconnectReason               |
/// | 4   | nullptr          | GetSecurityParameter              |
/// | 5   | nullptr          | GetNetworkConfig                  |
/// | 100 | nullptr          | AttachStateChangeEvent            |
/// | 101 | nullptr          | GetNetworkInfoLatestUpdate        |
/// | 102 | nullptr          | Scan                              |
/// | 103 | nullptr          | ScanPrivate                       |
/// | 104 | nullptr          | SetWirelessControllerRestriction  |
/// | 200 | nullptr          | OpenAccessPoint                   |
/// | 201 | nullptr          | CloseAccessPoint                  |
/// | 202 | nullptr          | CreateNetwork                     |
/// | 203 | nullptr          | CreateNetworkPrivate              |
/// | 204 | nullptr          | DestroyNetwork                    |
/// | 205 | nullptr          | Reject                            |
/// | 206 | nullptr          | SetAdvertiseData                  |
/// | 207 | nullptr          | SetStationAcceptPolicy            |
/// | 208 | nullptr          | AddAcceptFilterEntry              |
/// | 209 | nullptr          | ClearAcceptFilter                 |
/// | 300 | nullptr          | OpenStation                       |
/// | 301 | nullptr          | CloseStation                      |
/// | 302 | nullptr          | Connect                           |
/// | 303 | nullptr          | ConnectPrivate                    |
/// | 304 | nullptr          | Disconnect                        |
/// | 400 | nullptr          | InitializeSystem                  |
/// | 401 | nullptr          | FinalizeSystem                    |
/// | 402 | nullptr          | SetOperationMode                  |
/// | 403 | InitializeSystem2| InitializeSystem2                 |
pub struct ISystemLocalCommunicationService {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ISystemLocalCommunicationService {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (0, None, "GetState"),
                (1, None, "GetNetworkInfo"),
                (2, None, "GetIpv4Address"),
                (3, None, "GetDisconnectReason"),
                (4, None, "GetSecurityParameter"),
                (5, None, "GetNetworkConfig"),
                (100, None, "AttachStateChangeEvent"),
                (101, None, "GetNetworkInfoLatestUpdate"),
                (102, None, "Scan"),
                (103, None, "ScanPrivate"),
                (104, None, "SetWirelessControllerRestriction"),
                (200, None, "OpenAccessPoint"),
                (201, None, "CloseAccessPoint"),
                (202, None, "CreateNetwork"),
                (203, None, "CreateNetworkPrivate"),
                (204, None, "DestroyNetwork"),
                (205, None, "Reject"),
                (206, None, "SetAdvertiseData"),
                (207, None, "SetStationAcceptPolicy"),
                (208, None, "AddAcceptFilterEntry"),
                (209, None, "ClearAcceptFilter"),
                (300, None, "OpenStation"),
                (301, None, "CloseStation"),
                (302, None, "Connect"),
                (303, None, "ConnectPrivate"),
                (304, None, "Disconnect"),
                (400, None, "InitializeSystem"),
                (401, None, "FinalizeSystem"),
                (402, None, "SetOperationMode"),
                (
                    403,
                    Some(Self::initialize_system2_handler),
                    "InitializeSystem2",
                ),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// Cmd 403: InitializeSystem2
    pub fn initialize_system2(&self) -> ResultCode {
        log::warn!("(STUBBED) ISystemLocalCommunicationService::initialize_system2 called");
        RESULT_SUCCESS
    }

    fn initialize_system2_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let service = unsafe {
            &*(this as *const dyn ServiceFramework as *const ISystemLocalCommunicationService)
        };
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(service.initialize_system2());
    }
}

impl SessionRequestHandler for ISystemLocalCommunicationService {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "ISystemLocalCommunicationService"
    }
}

impl ServiceFramework for ISystemLocalCommunicationService {
    fn get_service_name(&self) -> &str {
        "ISystemLocalCommunicationService"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
