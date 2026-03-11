// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/time/system_clock.h/.cpp

pub const SYSTEM_CLOCK_COMMANDS: &[(u32, bool, &str)] = &[
    (0, true, "GetCurrentTime"),
    (1, true, "SetCurrentTime"),
    (2, true, "GetSystemClockContext"),
    (3, true, "SetSystemClockContext"),
    (4, true, "GetOperationEventReadableHandle"),
];
