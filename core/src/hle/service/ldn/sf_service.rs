// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-3.0-or-later

//! Port of zuyu/src/core/hle/service/ldn/sf_service.h
//! Port of zuyu/src/core/hle/service/ldn/sf_service.cpp
//!
//! ISfService: LP2P network service.

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
pub struct ISfService;

impl ISfService {
    pub fn new() -> Self {
        ISfService
    }
}
