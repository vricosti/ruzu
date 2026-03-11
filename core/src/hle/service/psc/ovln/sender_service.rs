// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/ovln/sender_service.cpp/.h

pub const ISENDER_SERVICE_COMMANDS: &[(u32, bool, &str)] = &[
    (0, true, "OpenSender"),
];
