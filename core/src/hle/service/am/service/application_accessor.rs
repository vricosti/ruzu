// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/application_accessor.h
//! Port of zuyu/src/core/hle/service/am/service/application_accessor.cpp

/// IPC command table for IApplicationAccessor:
/// - 0: GetAppletStateChangedEvent
/// - 1: IsCompleted (unimplemented)
/// - 10: Start
/// - 20: RequestExit
/// - 25: Terminate
/// - 30: GetResult
/// - 101: RequestForApplicationToGetForeground
/// - 110: TerminateAllLibraryApplets (unimplemented)
/// - 111: AreAnyLibraryAppletsLeft (unimplemented)
/// - 112: GetCurrentLibraryApplet
/// - 120: GetApplicationId (unimplemented)
/// - 121: PushLaunchParameter
/// - 122: GetApplicationControlProperty
/// - 123: GetApplicationLaunchProperty (unimplemented)
/// - 124: GetApplicationLaunchRequestInfo (unimplemented)
/// - 130: SetUsers
/// - 131: CheckRightsEnvironmentAvailable
/// - 132: GetNsRightsEnvironmentHandle
/// - 140: GetDesirableUids (unimplemented)
/// - 150: ReportApplicationExitTimeout
/// - 160: SetApplicationAttribute (unimplemented)
/// - 170: HasSaveDataAccessPermission (unimplemented)
/// - 180: PushToFriendInvitationStorageChannel (unimplemented)
/// - 190: PushToNotificationStorageChannel (unimplemented)
/// - 200: RequestApplicationSoftReset (unimplemented)
/// - 201: RestartApplicationTimer (unimplemented)
pub struct IApplicationAccessor {
    // TODO: WindowSystem reference, Applet reference
}

impl IApplicationAccessor {
    pub fn new() -> Self {
        Self {}
    }

    /// Port of IApplicationAccessor::Start
    pub fn start(&self) {
        log::info!("IApplicationAccessor::Start called");
        // TODO: m_applet->process->Run()
    }

    /// Port of IApplicationAccessor::RequestExit
    pub fn request_exit(&self) {
        log::info!("IApplicationAccessor::RequestExit called");
        // TODO: check exit_locked, lifecycle_manager.RequestExit()
    }

    /// Port of IApplicationAccessor::Terminate
    pub fn terminate(&self) {
        log::info!("IApplicationAccessor::Terminate called");
        // TODO: m_applet->process->Terminate()
    }

    /// Port of IApplicationAccessor::CheckRightsEnvironmentAvailable
    pub fn check_rights_environment_available(&self) -> bool {
        log::warn!("(STUBBED) CheckRightsEnvironmentAvailable called");
        true
    }

    /// Port of IApplicationAccessor::GetNsRightsEnvironmentHandle
    pub fn get_ns_rights_environment_handle(&self) -> u64 {
        log::warn!("(STUBBED) GetNsRightsEnvironmentHandle called");
        0xdeadbeef
    }

    /// Port of IApplicationAccessor::ReportApplicationExitTimeout
    pub fn report_application_exit_timeout(&self) {
        log::error!("ReportApplicationExitTimeout called");
    }
}
