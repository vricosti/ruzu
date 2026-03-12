// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/omm/power_state_interface.h
//! Port of zuyu/src/core/hle/service/omm/power_state_interface.cpp

/// IPowerStateInterface service ("spsm").
///
/// Command IDs (all currently stubbed/unimplemented in upstream):
///   0  GetState
///   1  EnterSleep
///   2  GetLastWakeReason
///   3  Shutdown
///   4  GetNotificationMessageEventHandle
///   5  ReceiveNotificationMessage
///   6  AnalyzeLogForLastSleepWakeSequence
///   7  ResetEventLog
///   8  AnalyzePerformanceLogForLastSleepWakeSequence
///   9  ChangeHomeButtonLongPressingTime
///  10  PutErrorState
///  11  InvalidateCurrentHomeButtonPressing
pub struct IPowerStateInterface;

impl IPowerStateInterface {
    pub fn new() -> Self {
        Self
    }
}
