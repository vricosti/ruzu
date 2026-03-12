// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/omm/operation_mode_manager.h
//! Port of zuyu/src/core/hle/service/omm/operation_mode_manager.cpp

/// IOperationModeManager service ("omm").
///
/// Command IDs (all currently stubbed/unimplemented in upstream):
///   0  GetOperationMode
///   1  GetOperationModeChangeEvent
///   2  EnableAudioVisual
///   3  DisableAudioVisual
///   4  EnterSleepAndWait
///   5  GetCradleStatus
///   6  FadeInDisplay
///   7  FadeOutDisplay
///   8  GetCradleFwVersion
///   9  NotifyCecSettingsChanged
///  10  SetOperationModePolicy
///  11  GetDefaultDisplayResolution
///  12  GetDefaultDisplayResolutionChangeEvent
///  13  UpdateDefaultDisplayResolution
///  14  ShouldSleepOnBoot
///  15  NotifyHdcpApplicationExecutionStarted
///  16  NotifyHdcpApplicationExecutionFinished
///  17  NotifyHdcpApplicationDrawingStarted
///  18  NotifyHdcpApplicationDrawingFinished
///  19  GetHdcpAuthenticationFailedEvent
///  20  GetHdcpAuthenticationFailedEmulationEnabled
///  21  SetHdcpAuthenticationFailedEmulation
///  22  GetHdcpStateChangeEvent
///  23  GetHdcpState
///  24  ShowCardUpdateProcessing
///  25  SetApplicationCecSettingsAndNotifyChanged
///  26  GetOperationModeSystemInfo
///  27  GetAppletFullAwakingSystemEvent
///  28  CreateCradleFirmwareUpdater
pub struct IOperationModeManager;

impl IOperationModeManager {
    pub fn new() -> Self {
        Self
    }
}
