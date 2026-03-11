// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/vi/system_root_service.cpp/.h

pub const ISYSTEM_ROOT_SERVICE_COMMANDS: &[(u32, bool, &str)] = &[
    (1, true, "GetDisplayService"),
    (3, false, "GetDisplayServiceWithProxyNameExchange"),
];
