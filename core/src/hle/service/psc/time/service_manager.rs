// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/time/service_manager.h/.cpp

pub const SERVICE_MANAGER_COMMANDS: &[(u32, bool, &str)] = &[
    (0, true, "GetStaticServiceAsUser"),
    (5, true, "GetStaticServiceAsAdmin"),
    (6, true, "GetStaticServiceAsRepair"),
    (7, true, "GetStaticServiceAsServiceManager"),
    (10, true, "SetupStandardSteadyClockCore"),
    (11, true, "SetupStandardLocalSystemClockCore"),
    (12, true, "SetupStandardNetworkSystemClockCore"),
    (13, true, "SetupStandardUserSystemClockCore"),
    (14, true, "SetupTimeZoneServiceCore"),
    (15, true, "SetupEphemeralNetworkSystemClockCore"),
    (50, true, "GetStandardLocalClockOperationEvent"),
    (51, true, "GetStandardNetworkClockOperationEventForServiceManager"),
    (52, true, "GetEphemeralNetworkClockOperationEventForServiceManager"),
    (53, true, "GetStandardUserSystemClockAutomaticCorrectionUpdatedEvent"),
    (60, true, "SetStandardSteadyClockBaseTime"),
    (200, true, "GetClosestAlarmUpdatedEvent"),
    (201, true, "CheckAndSignalAlarms"),
    (202, true, "GetClosestAlarmInfo"),
];
