// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/window_controller.h
//! Port of zuyu/src/core/hle/service/am/service/window_controller.cpp

/// IPC command table for IWindowController:
/// - 0: CreateWindow (unimplemented)
/// - 1: GetAppletResourceUserId
/// - 2: GetAppletResourceUserIdOfCallerApplet
/// - 10: AcquireForegroundRights
/// - 11: ReleaseForegroundRights
/// - 12: RejectToChangeIntoBackground
/// - 20: SetAppletWindowVisibility
/// - 21: SetAppletGpuTimeSlice
pub struct IWindowController {
    // TODO: WindowSystem reference, Applet reference
}

impl IWindowController {
    pub fn new() -> Self {
        Self {}
    }

    /// Port of IWindowController::GetAppletResourceUserId
    pub fn get_applet_resource_user_id(&self) -> u64 {
        log::info!("GetAppletResourceUserId called");
        0
    }

    /// Port of IWindowController::GetAppletResourceUserIdOfCallerApplet
    pub fn get_applet_resource_user_id_of_caller_applet(&self) -> u64 {
        log::info!("GetAppletResourceUserIdOfCallerApplet called");
        0
    }

    /// Port of IWindowController::AcquireForegroundRights
    pub fn acquire_foreground_rights(&self) {
        log::info!("AcquireForegroundRights called");
    }

    /// Port of IWindowController::ReleaseForegroundRights
    pub fn release_foreground_rights(&self) {
        log::info!("ReleaseForegroundRights called");
    }

    /// Port of IWindowController::RejectToChangeIntoBackground
    pub fn reject_to_change_into_background(&self) {
        log::info!("RejectToChangeIntoBackground called");
    }

    /// Port of IWindowController::SetAppletWindowVisibility
    pub fn set_applet_window_visibility(&self, visible: bool) {
        log::info!("SetAppletWindowVisibility called, visible={}", visible);
    }

    /// Port of IWindowController::SetAppletGpuTimeSlice
    pub fn set_applet_gpu_time_slice(&self, time_slice: i64) {
        log::warn!("(STUBBED) SetAppletGpuTimeSlice called, time_slice={}", time_slice);
    }
}
