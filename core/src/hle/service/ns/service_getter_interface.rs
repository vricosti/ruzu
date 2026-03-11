// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/service_getter_interface.cpp/.h
//!
//! IServiceGetterInterface dispatches to sub-interfaces.

pub const ISERVICE_GETTER_INTERFACE_COMMANDS: &[(u32, &str)] = &[
    (7988, "GetDynamicRightsInterface"),
    (7989, "GetReadOnlyApplicationControlDataInterface"),
    (7991, "GetReadOnlyApplicationRecordInterface"),
    (7992, "GetECommerceInterface"),
    (7993, "GetApplicationVersionInterface"),
    (7994, "GetFactoryResetInterface"),
    (7995, "GetAccountProxyInterface"),
    (7996, "GetApplicationManagerInterface"),
    (7997, "GetDownloadTaskInterface"),
    (7998, "GetContentManagementInterface"),
    (7999, "GetDocumentInterface"),
];
