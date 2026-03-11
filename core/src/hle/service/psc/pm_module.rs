// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/pm_module.cpp/.h

pub const IPM_MODULE_COMMANDS: &[(u32, &str)] = &[
    (0, "Initialize"),
    (1, "GetRequest"),
    (2, "Acknowledge"),
    (3, "Finalize"),
    (4, "AcknowledgeEx"),
];
