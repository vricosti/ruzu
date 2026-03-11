// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/time/steady_clock.h/.cpp

pub const STEADY_CLOCK_COMMANDS: &[(u32, bool, &str)] = &[
    (0, true, "GetCurrentTimePoint"),
    (2, true, "GetTestOffset"),
    (3, true, "SetTestOffset"),
    (100, true, "GetRtcValue"),
    (101, true, "IsRtcResetDetected"),
    (102, true, "GetSetupResultValue"),
    (200, true, "GetInternalOffset"),
];
