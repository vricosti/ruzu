// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/time/time_zone_service.h/.cpp

pub const TIME_ZONE_SERVICE_COMMANDS: &[(u32, bool, &str)] = &[
    (0, true, "GetDeviceLocationName"),
    (1, true, "SetDeviceLocationName"),
    (2, true, "GetTotalLocationNameCount"),
    (3, true, "LoadLocationNameList"),
    (4, true, "LoadTimeZoneRule"),
    (5, true, "GetTimeZoneRuleVersion"),
    (6, true, "GetDeviceLocationNameAndUpdatedTime"),
    (7, true, "SetDeviceLocationNameWithTimeZoneRule"),
    (8, true, "ParseTimeZoneBinary"),
    (20, true, "GetDeviceLocationNameOperationEventReadableHandle"),
    (100, true, "ToCalendarTime"),
    (101, true, "ToCalendarTimeWithMyRule"),
    (201, true, "ToPosixTime"),
    (202, true, "ToPosixTimeWithMyRule"),
];
