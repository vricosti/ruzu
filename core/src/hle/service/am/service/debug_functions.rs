// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/debug_functions.h
//! Port of zuyu/src/core/hle/service/am/service/debug_functions.cpp

/// IPC command table for IDebugFunctions:
/// - 0: NotifyMessageToHomeMenuForDebug (unimplemented)
/// - 1: OpenMainApplication (unimplemented)
/// - 10: PerformSystemButtonPressing (unimplemented)
/// - 20: InvalidateTransitionLayer (unimplemented)
/// - 30: RequestLaunchApplicationWithUserAndArgumentForDebug (unimplemented)
/// - 31: RequestLaunchApplicationByApplicationLaunchInfoForDebug (unimplemented)
/// - 40: GetAppletResourceUsageInfo (unimplemented)
/// - 50: AddSystemProgramIdAndAppletIdForDebug (unimplemented)
/// - 51: AddOperationConfirmedLibraryAppletIdForDebug (unimplemented)
/// - 100: SetCpuBoostModeForApplet (unimplemented)
/// - 101: CancelCpuBoostModeForApplet (unimplemented)
/// - 110: PushToAppletBoundChannelForDebug (unimplemented)
/// - 111: TryPopFromAppletBoundChannelForDebug (unimplemented)
/// - 120: AlarmSettingNotificationEnableAppEventReserve (unimplemented)
/// - 121: AlarmSettingNotificationDisableAppEventReserve (unimplemented)
/// - 122: AlarmSettingNotificationPushAppEventNotify (unimplemented)
/// - 130: FriendInvitationSetApplicationParameter (unimplemented)
/// - 131: FriendInvitationClearApplicationParameter (unimplemented)
/// - 132: FriendInvitationPushApplicationParameter (unimplemented)
/// - 140: RestrictPowerOperationForSecureLaunchModeForDebug (unimplemented)
/// - 200: CreateFloatingLibraryAppletAccepterForDebug (unimplemented)
/// - 300: TerminateAllRunningApplicationsForDebug (unimplemented)
/// - 900: GetGrcProcessLaunchedSystemEvent (unimplemented)
pub struct IDebugFunctions {}

impl IDebugFunctions {
    pub fn new() -> Self {
        Self {}
    }
}
