// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/psc/ovln/receiver.cpp/.h

pub const IRECEIVER_COMMANDS: &[(u32, &str)] = &[
    (0, "AddSource"),
    (1, "RemoveSource"),
    (2, "GetReceiveEventHandle"),
    (3, "Receive"),
    (4, "ReceiveWithTick"),
];
