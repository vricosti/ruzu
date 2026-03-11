// SPDX-FileCopyrightText: Copyright 2023 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/time/power_state_service.h/.cpp

pub const IPOWER_STATE_REQUEST_HANDLER_COMMANDS: &[(u32, bool, &str)] = &[
    (0, true, "GetPowerStateRequestEventReadableHandle"),
    (1, true, "GetAndClearPowerStateRequest"),
];
