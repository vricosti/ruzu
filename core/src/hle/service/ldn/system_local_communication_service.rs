// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/ldn/system_local_communication_service.h
//! Port of zuyu/src/core/hle/service/ldn/system_local_communication_service.cpp
//!
//! ISystemLocalCommunicationService: system-level LDN communication service.

use crate::hle::result::{ResultCode, RESULT_SUCCESS};

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
pub struct ISystemLocalCommunicationService;

impl ISystemLocalCommunicationService {
    pub fn new() -> Self {
        ISystemLocalCommunicationService
    }

    /// Cmd 403: InitializeSystem2
    pub fn initialize_system2(&self) -> ResultCode {
        log::warn!("(STUBBED) ISystemLocalCommunicationService::initialize_system2 called");
        RESULT_SUCCESS
    }
}
