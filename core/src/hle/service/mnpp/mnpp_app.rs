// SPDX-FileCopyrightText: Copyright 2022 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/mnpp/mnpp_app.cpp
//!
//! MNPP_APP service ("mnpp:app").

/// IPC command IDs for MNPP_APP
pub mod commands {
    pub const UNKNOWN0: u32 = 0;
    pub const UNKNOWN1: u32 = 1;
}

/// MNPP_APP service ("mnpp:app").
pub struct MnppApp;

impl MnppApp {
    pub fn new() -> Self {
        Self
    }

    pub fn unknown0(&self) {
        log::warn!("(STUBBED) MnppApp::unknown0 called");
    }

    pub fn unknown1(&self) {
        log::warn!("(STUBBED) MnppApp::unknown1 called");
    }
}

/// Registers "mnpp:app" service.
///
/// Corresponds to `LoopProcess` in upstream `mnpp_app.cpp`.
pub fn loop_process() {
    // TODO: register "mnpp:app" -> MnppApp with ServerManager
}
