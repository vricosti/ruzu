// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/ldn/sf_service.h
//! Port of zuyu/src/core/hle/service/ldn/sf_service.cpp
//!
//! ISfService: LP2P network service.

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for ISfService.
///
/// | Cmd  | Handler | Name                              |
/// |------|---------|-----------------------------------|
/// | 0    | nullptr | Initialize                        |
/// | 256  | nullptr | AttachNetworkInterfaceStateChangeEvent |
/// | 264  | nullptr | GetNetworkInterfaceLastError       |
/// | 272  | nullptr | GetRole                           |
/// | 280  | nullptr | GetAdvertiseData                  |
/// | 288  | nullptr | GetGroupInfo                      |
/// | 296  | nullptr | GetGroupInfo2                     |
/// | 304  | nullptr | GetGroupOwner                     |
/// | 312  | nullptr | GetIpConfig                       |
/// | 320  | nullptr | GetLinkLevel                      |
/// | 512  | nullptr | Scan                              |
/// | 768  | nullptr | CreateGroup                       |
/// | 776  | nullptr | DestroyGroup                      |
/// | 784  | nullptr | SetAdvertiseData                  |
/// | 1536 | nullptr | SendToOtherGroup                  |
/// | 1544 | nullptr | RecvFromOtherGroup                |
/// | 1552 | nullptr | AddAcceptableGroupId              |
/// | 1560 | nullptr | ClearAcceptableGroupId            |
///
/// All commands are nullptr (unimplemented stubs) in upstream.
pub struct ISfService {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl ISfService {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (0, None, "Initialize"),
                (256, None, "AttachNetworkInterfaceStateChangeEvent"),
                (264, None, "GetNetworkInterfaceLastError"),
                (272, None, "GetRole"),
                (280, None, "GetAdvertiseData"),
                (288, None, "GetGroupInfo"),
                (296, None, "GetGroupInfo2"),
                (304, None, "GetGroupOwner"),
                (312, None, "GetIpConfig"),
                (320, None, "GetLinkLevel"),
                (512, None, "Scan"),
                (768, None, "CreateGroup"),
                (776, None, "DestroyGroup"),
                (784, None, "SetAdvertiseData"),
                (1536, None, "SendToOtherGroup"),
                (1544, None, "RecvFromOtherGroup"),
                (1552, None, "AddAcceptableGroupId"),
                (1560, None, "ClearAcceptableGroupId"),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl SessionRequestHandler for ISfService {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "ISfService"
    }
}

impl ServiceFramework for ISfService {
    fn get_service_name(&self) -> &str {
        "ISfService"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
