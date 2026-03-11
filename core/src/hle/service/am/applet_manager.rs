// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/applet_manager.h
//! Port of zuyu/src/core/hle/service/am/applet_manager.cpp

use super::am_types::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LaunchType {
    FrontendInitiated,
    ApplicationInitiated,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct FrontendAppletParameters {
    pub program_id: ProgramId,
    pub applet_id: AppletId,
    pub applet_type: AppletType,
    pub launch_type: Option<LaunchType>,
    pub program_index: i32,
    pub previous_program_index: i32,
}

/// Port of AppletManager
///
/// Manages the lifecycle of applets. In the full implementation this
/// coordinates with WindowSystem via condition variables; here it is
/// stubbed to compile.
pub struct AppletManager {
    // TODO: wire up System, WindowSystem, condition variable, pending process
}

impl AppletManager {
    pub fn new() -> Self {
        Self {}
    }

    pub fn request_exit(&self) {
        // TODO: forward to window_system
        log::warn!("(STUBBED) AppletManager::request_exit called");
    }

    pub fn operation_mode_changed(&self) {
        // TODO: forward to window_system
        log::warn!("(STUBBED) AppletManager::operation_mode_changed called");
    }
}
