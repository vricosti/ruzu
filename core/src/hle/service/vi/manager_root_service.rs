// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/vi/manager_root_service.cpp/.h

pub const IMANAGER_ROOT_SERVICE_COMMANDS: &[(u32, bool, &str)] = &[
    (2, true, "GetDisplayService"),
    (3, false, "GetDisplayServiceWithProxyNameExchange"),
    (100, false, "AllocateProcessHeapBlock"),
    (101, false, "FreeProcessHeapBlock"),
];
