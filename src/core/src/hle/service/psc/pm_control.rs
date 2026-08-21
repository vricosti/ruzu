// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/pm_control.cpp/.h

pub const IPM_CONTROL_COMMANDS: &[(u32, &str)] = &[
    (0, "Initialize"),
    (1, "DispatchRequest"),
    (2, "GetResult"),
    (3, "GetState"),
    (4, "Cancel"),
    (5, "PrintModuleInformation"),
    (6, "GetModuleInformation"),
    (10, "AcquireStateLock"),
    (11, "HasStateLock"),
];
