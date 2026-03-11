// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/time/alarms.h/.cpp
//! Status: Stubbed - interfaces defined, implementations pending.

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum AlarmType {
    WakeupAlarm = 0,
    BackgroundTaskAlarm = 1,
}

pub const IALARM_SERVICE_COMMANDS: &[(u32, &str)] = &[
    (0, "CreateWakeupAlarm"),
    (1, "CreateBackgroundTaskAlarm"),
];

pub const ISTEADY_CLOCK_ALARM_COMMANDS: &[(u32, &str)] = &[
    (0, "GetAlarmEvent"),
    (1, "Enable"),
    (2, "Disable"),
    (3, "IsEnabled"),
];
