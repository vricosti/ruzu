// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ns/service_getter_interface.h
//! Port of zuyu/src/core/hle/service/ns/service_getter_interface.cpp
//!
//! IServiceGetterInterface dispatches to sub-interfaces.

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command table for IServiceGetterInterface.
///
/// Corresponds to the function table in upstream service_getter_interface.cpp.
pub mod commands {
    pub const GET_DYNAMIC_RIGHTS_INTERFACE: u32 = 7988;
    pub const GET_READ_ONLY_APPLICATION_CONTROL_DATA_INTERFACE: u32 = 7989;
    pub const GET_READ_ONLY_APPLICATION_RECORD_INTERFACE: u32 = 7991;
    pub const GET_ECOMMERCE_INTERFACE: u32 = 7992;
    pub const GET_APPLICATION_VERSION_INTERFACE: u32 = 7993;
    pub const GET_FACTORY_RESET_INTERFACE: u32 = 7994;
    pub const GET_ACCOUNT_PROXY_INTERFACE: u32 = 7995;
    pub const GET_APPLICATION_MANAGER_INTERFACE: u32 = 7996;
    pub const GET_DOWNLOAD_TASK_INTERFACE: u32 = 7997;
    pub const GET_CONTENT_MANAGEMENT_INTERFACE: u32 = 7998;
    pub const GET_DOCUMENT_INTERFACE: u32 = 7999;
}

/// IServiceGetterInterface — dispatches to sub-interfaces for NS.
///
/// Corresponds to `IServiceGetterInterface` in upstream.
/// Each Get*Interface method creates and returns a new sub-interface object.
pub struct IServiceGetterInterface {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IServiceGetterInterface {
    pub fn new() -> Self {
        let handlers = build_handler_map(&[
            (commands::GET_DYNAMIC_RIGHTS_INTERFACE, None, "GetDynamicRightsInterface"),
            (commands::GET_READ_ONLY_APPLICATION_CONTROL_DATA_INTERFACE, None, "GetReadOnlyApplicationControlDataInterface"),
            (commands::GET_READ_ONLY_APPLICATION_RECORD_INTERFACE, None, "GetReadOnlyApplicationRecordInterface"),
            (commands::GET_ECOMMERCE_INTERFACE, None, "GetECommerceInterface"),
            (commands::GET_APPLICATION_VERSION_INTERFACE, None, "GetApplicationVersionInterface"),
            (commands::GET_FACTORY_RESET_INTERFACE, None, "GetFactoryResetInterface"),
            (commands::GET_ACCOUNT_PROXY_INTERFACE, None, "GetAccountProxyInterface"),
            (commands::GET_APPLICATION_MANAGER_INTERFACE, None, "GetApplicationManagerInterface"),
            (commands::GET_DOWNLOAD_TASK_INTERFACE, None, "GetDownloadTaskInterface"),
            (commands::GET_CONTENT_MANAGEMENT_INTERFACE, None, "GetContentManagementInterface"),
            (commands::GET_DOCUMENT_INTERFACE, None, "GetDocumentInterface"),
        ]);
        Self {
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    /// GetDynamicRightsInterface (cmd 7988).
    pub fn get_dynamic_rights_interface(&self) {
        log::debug!("IServiceGetterInterface::get_dynamic_rights_interface called");
        // Returns IDynamicRightsInterface.
    }

    /// GetReadOnlyApplicationControlDataInterface (cmd 7989).
    pub fn get_read_only_application_control_data_interface(&self) {
        log::debug!(
            "IServiceGetterInterface::get_read_only_application_control_data_interface called"
        );
        // Returns IReadOnlyApplicationControlDataInterface.
    }

    /// GetReadOnlyApplicationRecordInterface (cmd 7991).
    pub fn get_read_only_application_record_interface(&self) {
        log::debug!(
            "IServiceGetterInterface::get_read_only_application_record_interface called"
        );
        // Returns IReadOnlyApplicationRecordInterface.
    }

    /// GetECommerceInterface (cmd 7992).
    pub fn get_ecommerce_interface(&self) {
        log::debug!("IServiceGetterInterface::get_ecommerce_interface called");
    }

    /// GetApplicationVersionInterface (cmd 7993).
    pub fn get_application_version_interface(&self) {
        log::debug!(
            "IServiceGetterInterface::get_application_version_interface called"
        );
    }

    /// GetFactoryResetInterface (cmd 7994).
    pub fn get_factory_reset_interface(&self) {
        log::debug!("IServiceGetterInterface::get_factory_reset_interface called");
    }

    /// GetAccountProxyInterface (cmd 7995).
    pub fn get_account_proxy_interface(&self) {
        log::debug!("IServiceGetterInterface::get_account_proxy_interface called");
    }

    /// GetApplicationManagerInterface (cmd 7996).
    pub fn get_application_manager_interface(&self) {
        log::debug!(
            "IServiceGetterInterface::get_application_manager_interface called"
        );
    }

    /// GetDownloadTaskInterface (cmd 7997).
    pub fn get_download_task_interface(&self) {
        log::debug!("IServiceGetterInterface::get_download_task_interface called");
    }

    /// GetContentManagementInterface (cmd 7998).
    pub fn get_content_management_interface(&self) {
        log::debug!(
            "IServiceGetterInterface::get_content_management_interface called"
        );
    }

    /// GetDocumentInterface (cmd 7999).
    pub fn get_document_interface(&self) {
        log::debug!("IServiceGetterInterface::get_document_interface called");
    }
}

impl SessionRequestHandler for IServiceGetterInterface {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }

    fn service_name(&self) -> &str {
        "ns::IServiceGetterInterface"
    }
}

impl ServiceFramework for IServiceGetterInterface {
    fn get_service_name(&self) -> &str {
        "ns::IServiceGetterInterface"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
