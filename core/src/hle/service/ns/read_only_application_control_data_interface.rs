// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/read_only_application_control_data_interface.cpp/.h

pub const IREAD_ONLY_APPLICATION_CONTROL_DATA_INTERFACE_COMMANDS: &[(u32, bool, &str)] = &[
    (0, true, "GetApplicationControlData"),
    (1, true, "GetApplicationDesiredLanguage"),
    (2, true, "ConvertApplicationLanguageToLanguageCode"),
    (3, false, "ConvertLanguageCodeToApplicationLanguage"),
    (4, false, "SelectApplicationDesiredLanguage"),
];
