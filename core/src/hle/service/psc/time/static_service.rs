// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/time/static.h/.cpp
//! Note: file named static_service.rs to avoid Rust keyword conflict with "static".

use super::common::StaticServiceSetupInfo;

pub const STATIC_SERVICE_COMMANDS: &[(u32, bool, &str)] = &[
    (0, true, "GetStandardUserSystemClock"),
    (1, true, "GetStandardNetworkSystemClock"),
    (2, true, "GetStandardSteadyClock"),
    (3, true, "GetTimeZoneService"),
    (4, true, "GetStandardLocalSystemClock"),
    (5, true, "GetEphemeralNetworkSystemClock"),
    (20, true, "GetSharedMemoryNativeHandle"),
    (50, true, "SetStandardSteadyClockInternalOffset"),
    (51, true, "GetStandardSteadyClockRtcValue"),
    (100, true, "IsStandardUserSystemClockAutomaticCorrectionEnabled"),
    (101, true, "SetStandardUserSystemClockAutomaticCorrectionEnabled"),
    (102, true, "GetStandardUserSystemClockInitialYear"),
    (200, true, "IsStandardNetworkSystemClockAccuracySufficient"),
    (201, true, "GetStandardUserSystemClockAutomaticCorrectionUpdatedTime"),
    (300, true, "CalculateMonotonicSystemClockBaseTimePoint"),
    (400, true, "GetClockSnapshot"),
    (401, true, "GetClockSnapshotFromSystemClockContext"),
    (500, true, "CalculateStandardUserSystemClockDifferenceByUser"),
    (501, true, "CalculateSpanBetween"),
];

/// Default setup info for time:su
pub const TIME_SU_SETUP_INFO: StaticServiceSetupInfo = StaticServiceSetupInfo {
    can_write_local_clock: false,
    can_write_user_clock: false,
    can_write_network_clock: false,
    can_write_timezone_device_location: false,
    can_write_steady_clock: false,
    can_write_uninitialized_clock: true,
};
