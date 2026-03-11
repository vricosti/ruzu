// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/process_winding_controller.h
//! Port of zuyu/src/core/hle/service/am/service/process_winding_controller.cpp

/// IPC command table for IProcessWindingController:
/// - 0: GetLaunchReason
/// - 11: OpenCallingLibraryApplet
pub struct IProcessWindingController {
    // TODO: Applet reference
}

impl IProcessWindingController {
    pub fn new() -> Self {
        Self {}
    }
}
