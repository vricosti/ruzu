// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/global_state_controller.h
//! Port of zuyu/src/core/hle/service/am/service/global_state_controller.cpp

/// IPC command table for IGlobalStateController:
/// - 0: RequestToEnterSleep (unimplemented)
/// - 1: EnterSleep (unimplemented)
/// - 2: StartSleepSequence (unimplemented)
/// - 3: StartShutdownSequence (unimplemented)
/// - 4: StartRebootSequence (unimplemented)
/// - 9: IsAutoPowerDownRequested (unimplemented)
/// - 10: LoadAndApplyIdlePolicySettings
/// - 11: NotifyCecSettingsChanged (unimplemented)
/// - 12: SetDefaultHomeButtonLongPressTime (unimplemented)
/// - 13: UpdateDefaultDisplayResolution (unimplemented)
/// - 14: ShouldSleepOnBoot
/// - 15: GetHdcpAuthenticationFailedEvent
/// - 30: OpenCradleFirmwareUpdater
pub struct IGlobalStateController {
    // TODO: ServiceContext, Event fields
}

impl IGlobalStateController {
    pub fn new() -> Self {
        Self {}
    }
}
