// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/vi/application_root_service.cpp/.h

pub const IAPPLICATION_ROOT_SERVICE_COMMANDS: &[(u32, bool, &str)] = &[
    (0, true, "GetDisplayService"),
    (1, false, "GetDisplayServiceWithProxyNameExchange"),
];
