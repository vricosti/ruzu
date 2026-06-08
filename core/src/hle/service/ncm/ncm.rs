// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ncm/ncm.cpp
//!
//! NCM and LR services. All commands are unimplemented stubs.

use std::collections::BTreeMap;

use crate::hle::result::ResultCode;
use crate::hle::service::hle_ipc::{
    HLERequestContext, SessionRequestHandler, SessionRequestHandlerPtr,
};
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

/// IPC command IDs for ILocationResolver
pub mod location_resolver_commands {
    pub const RESOLVE_PROGRAM_PATH: u32 = 0;
    pub const REDIRECT_PROGRAM_PATH: u32 = 1;
    pub const RESOLVE_APPLICATION_CONTROL_PATH: u32 = 2;
    pub const RESOLVE_APPLICATION_HTML_DOCUMENT_PATH: u32 = 3;
    pub const RESOLVE_DATA_PATH: u32 = 4;
    pub const REDIRECT_APPLICATION_CONTROL_PATH: u32 = 5;
    pub const REDIRECT_APPLICATION_HTML_DOCUMENT_PATH: u32 = 6;
    pub const RESOLVE_APPLICATION_LEGAL_INFORMATION_PATH: u32 = 7;
    pub const REDIRECT_APPLICATION_LEGAL_INFORMATION_PATH: u32 = 8;
    pub const REFRESH: u32 = 9;
    pub const REDIRECT_APPLICATION_PROGRAM_PATH: u32 = 10;
    pub const CLEAR_APPLICATION_REDIRECTION: u32 = 11;
    pub const ERASE_PROGRAM_REDIRECTION: u32 = 12;
    pub const ERASE_APPLICATION_CONTROL_REDIRECTION: u32 = 13;
    pub const ERASE_APPLICATION_HTML_DOCUMENT_REDIRECTION: u32 = 14;
    pub const ERASE_APPLICATION_LEGAL_INFORMATION_REDIRECTION: u32 = 15;
    pub const RESOLVE_PROGRAM_PATH_FOR_DEBUG: u32 = 16;
    pub const REDIRECT_PROGRAM_PATH_FOR_DEBUG: u32 = 17;
    pub const REDIRECT_APPLICATION_PROGRAM_PATH_FOR_DEBUG: u32 = 18;
    pub const ERASE_PROGRAM_REDIRECTION_FOR_DEBUG: u32 = 19;
}

/// IPC command IDs for IRegisteredLocationResolver
pub mod registered_location_resolver_commands {
    pub const RESOLVE_PROGRAM_PATH: u32 = 0;
    pub const REGISTER_PROGRAM_PATH: u32 = 1;
    pub const UNREGISTER_PROGRAM_PATH: u32 = 2;
    pub const REDIRECT_PROGRAM_PATH: u32 = 3;
    pub const RESOLVE_HTML_DOCUMENT_PATH: u32 = 4;
    pub const REGISTER_HTML_DOCUMENT_PATH: u32 = 5;
    pub const UNREGISTER_HTML_DOCUMENT_PATH: u32 = 6;
    pub const REDIRECT_HTML_DOCUMENT_PATH: u32 = 7;
    pub const REFRESH: u32 = 8;
    pub const REFRESH_EXCLUDING: u32 = 9;
}

/// IPC command IDs for IAddOnContentLocationResolver
pub mod add_on_content_location_resolver_commands {
    pub const RESOLVE_ADD_ON_CONTENT_PATH: u32 = 0;
    pub const REGISTER_ADD_ON_CONTENT_STORAGE: u32 = 1;
    pub const UNREGISTER_ALL_ADD_ON_CONTENT_PATH: u32 = 2;
    pub const REFRESH_APPLICATION_ADD_ON_CONTENT: u32 = 3;
    pub const UNREGISTER_APPLICATION_ADD_ON_CONTENT: u32 = 4;
}

/// IPC command IDs for LR
pub mod lr_commands {
    pub const OPEN_LOCATION_RESOLVER: u32 = 0;
    pub const OPEN_REGISTERED_LOCATION_RESOLVER: u32 = 1;
    pub const REFRESH_LOCATION_RESOLVER: u32 = 2;
    pub const OPEN_ADD_ON_CONTENT_LOCATION_RESOLVER: u32 = 3;
}

/// IPC command IDs for NCM
pub mod ncm_commands {
    pub const CREATE_CONTENT_STORAGE: u32 = 0;
    pub const CREATE_CONTENT_META_DATABASE: u32 = 1;
    pub const VERIFY_CONTENT_STORAGE: u32 = 2;
    pub const VERIFY_CONTENT_META_DATABASE: u32 = 3;
    pub const OPEN_CONTENT_STORAGE: u32 = 4;
    pub const OPEN_CONTENT_META_DATABASE: u32 = 5;
    pub const CLOSE_CONTENT_STORAGE_FORCIBLY: u32 = 6;
    pub const CLOSE_CONTENT_META_DATABASE_FORCIBLY: u32 = 7;
    pub const CLEAN_UP_CONTENT_META_DATABASE: u32 = 8;
    pub const ACTIVATE_CONTENT_STORAGE: u32 = 9;
    pub const INACTIVATE_CONTENT_STORAGE: u32 = 10;
    pub const ACTIVATE_CONTENT_META_DATABASE: u32 = 11;
    pub const INACTIVATE_CONTENT_META_DATABASE: u32 = 12;
    pub const INVALIDATE_RIGHTS_ID_CACHE: u32 = 13;
    pub const GET_MEMORY_REPORT: u32 = 14;
    pub const ACTIVATE_FS_CONTENT_STORAGE: u32 = 15;
}

macro_rules! impl_service_framework {
    ($ty:ty, $name:expr) => {
        impl SessionRequestHandler for $ty {
            fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
                ServiceFramework::handle_sync_request_impl(self, ctx)
            }

            fn service_name(&self) -> &str {
                $name
            }
        }

        impl ServiceFramework for $ty {
            fn get_service_name(&self) -> &str {
                $name
            }

            fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
                &self.handlers
            }

            fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
                &self.handlers_tipc
            }
        }
    };
}

pub struct ILocationResolver {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    storage: u8,
}

impl ILocationResolver {
    pub fn new(storage: u8) -> Self {
        Self {
            handlers: build_handler_map(&[
                (
                    location_resolver_commands::RESOLVE_PROGRAM_PATH,
                    None,
                    "ResolveProgramPath",
                ),
                (
                    location_resolver_commands::REDIRECT_PROGRAM_PATH,
                    None,
                    "RedirectProgramPath",
                ),
                (
                    location_resolver_commands::RESOLVE_APPLICATION_CONTROL_PATH,
                    None,
                    "ResolveApplicationControlPath",
                ),
                (
                    location_resolver_commands::RESOLVE_APPLICATION_HTML_DOCUMENT_PATH,
                    None,
                    "ResolveApplicationHtmlDocumentPath",
                ),
                (
                    location_resolver_commands::RESOLVE_DATA_PATH,
                    None,
                    "ResolveDataPath",
                ),
                (
                    location_resolver_commands::REDIRECT_APPLICATION_CONTROL_PATH,
                    None,
                    "RedirectApplicationControlPath",
                ),
                (
                    location_resolver_commands::REDIRECT_APPLICATION_HTML_DOCUMENT_PATH,
                    None,
                    "RedirectApplicationHtmlDocumentPath",
                ),
                (
                    location_resolver_commands::RESOLVE_APPLICATION_LEGAL_INFORMATION_PATH,
                    None,
                    "ResolveApplicationLegalInformationPath",
                ),
                (
                    location_resolver_commands::REDIRECT_APPLICATION_LEGAL_INFORMATION_PATH,
                    None,
                    "RedirectApplicationLegalInformationPath",
                ),
                (location_resolver_commands::REFRESH, None, "Refresh"),
                (
                    location_resolver_commands::REDIRECT_APPLICATION_PROGRAM_PATH,
                    None,
                    "RedirectApplicationProgramPath",
                ),
                (
                    location_resolver_commands::CLEAR_APPLICATION_REDIRECTION,
                    None,
                    "ClearApplicationRedirection",
                ),
                (
                    location_resolver_commands::ERASE_PROGRAM_REDIRECTION,
                    None,
                    "EraseProgramRedirection",
                ),
                (
                    location_resolver_commands::ERASE_APPLICATION_CONTROL_REDIRECTION,
                    None,
                    "EraseApplicationControlRedirection",
                ),
                (
                    location_resolver_commands::ERASE_APPLICATION_HTML_DOCUMENT_REDIRECTION,
                    None,
                    "EraseApplicationHtmlDocumentRedirection",
                ),
                (
                    location_resolver_commands::ERASE_APPLICATION_LEGAL_INFORMATION_REDIRECTION,
                    None,
                    "EraseApplicationLegalInformationRedirection",
                ),
                (
                    location_resolver_commands::RESOLVE_PROGRAM_PATH_FOR_DEBUG,
                    None,
                    "ResolveProgramPathForDebug",
                ),
                (
                    location_resolver_commands::REDIRECT_PROGRAM_PATH_FOR_DEBUG,
                    None,
                    "RedirectProgramPathForDebug",
                ),
                (
                    location_resolver_commands::REDIRECT_APPLICATION_PROGRAM_PATH_FOR_DEBUG,
                    None,
                    "RedirectApplicationProgramPathForDebug",
                ),
                (
                    location_resolver_commands::ERASE_PROGRAM_REDIRECTION_FOR_DEBUG,
                    None,
                    "EraseProgramRedirectionForDebug",
                ),
            ]),
            handlers_tipc: BTreeMap::new(),
            storage,
        }
    }

    pub fn storage(&self) -> u8 {
        self.storage
    }
}

impl_service_framework!(ILocationResolver, "ILocationResolver");

pub struct IRegisteredLocationResolver {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IRegisteredLocationResolver {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (
                    registered_location_resolver_commands::RESOLVE_PROGRAM_PATH,
                    None,
                    "ResolveProgramPath",
                ),
                (
                    registered_location_resolver_commands::REGISTER_PROGRAM_PATH,
                    None,
                    "RegisterProgramPath",
                ),
                (
                    registered_location_resolver_commands::UNREGISTER_PROGRAM_PATH,
                    None,
                    "UnregisterProgramPath",
                ),
                (
                    registered_location_resolver_commands::REDIRECT_PROGRAM_PATH,
                    None,
                    "RedirectProgramPath",
                ),
                (
                    registered_location_resolver_commands::RESOLVE_HTML_DOCUMENT_PATH,
                    None,
                    "ResolveHtmlDocumentPath",
                ),
                (
                    registered_location_resolver_commands::REGISTER_HTML_DOCUMENT_PATH,
                    None,
                    "RegisterHtmlDocumentPath",
                ),
                (
                    registered_location_resolver_commands::UNREGISTER_HTML_DOCUMENT_PATH,
                    None,
                    "UnregisterHtmlDocumentPath",
                ),
                (
                    registered_location_resolver_commands::REDIRECT_HTML_DOCUMENT_PATH,
                    None,
                    "RedirectHtmlDocumentPath",
                ),
                (
                    registered_location_resolver_commands::REFRESH,
                    None,
                    "Refresh",
                ),
                (
                    registered_location_resolver_commands::REFRESH_EXCLUDING,
                    None,
                    "RefreshExcluding",
                ),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl_service_framework!(IRegisteredLocationResolver, "IRegisteredLocationResolver");

pub struct IAddOnContentLocationResolver {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IAddOnContentLocationResolver {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (
                    add_on_content_location_resolver_commands::RESOLVE_ADD_ON_CONTENT_PATH,
                    None,
                    "ResolveAddOnContentPath",
                ),
                (
                    add_on_content_location_resolver_commands::REGISTER_ADD_ON_CONTENT_STORAGE,
                    None,
                    "RegisterAddOnContentStorage",
                ),
                (
                    add_on_content_location_resolver_commands::UNREGISTER_ALL_ADD_ON_CONTENT_PATH,
                    None,
                    "UnregisterAllAddOnContentPath",
                ),
                (
                    add_on_content_location_resolver_commands::REFRESH_APPLICATION_ADD_ON_CONTENT,
                    None,
                    "RefreshApplicationAddOnContent",
                ),
                (
                    add_on_content_location_resolver_commands::UNREGISTER_APPLICATION_ADD_ON_CONTENT,
                    None,
                    "UnregisterApplicationAddOnContent",
                ),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl_service_framework!(
    IAddOnContentLocationResolver,
    "IAddOnContentLocationResolver"
);

pub struct LR {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl LR {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (
                    lr_commands::OPEN_LOCATION_RESOLVER,
                    None,
                    "OpenLocationResolver",
                ),
                (
                    lr_commands::OPEN_REGISTERED_LOCATION_RESOLVER,
                    None,
                    "OpenRegisteredLocationResolver",
                ),
                (
                    lr_commands::REFRESH_LOCATION_RESOLVER,
                    None,
                    "RefreshLocationResolver",
                ),
                (
                    lr_commands::OPEN_ADD_ON_CONTENT_LOCATION_RESOLVER,
                    None,
                    "OpenAddOnContentLocationResolver",
                ),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl_service_framework!(LR, "lr");

pub struct NCM {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl NCM {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[
                (
                    ncm_commands::CREATE_CONTENT_STORAGE,
                    None,
                    "CreateContentStorage",
                ),
                (
                    ncm_commands::CREATE_CONTENT_META_DATABASE,
                    None,
                    "CreateContentMetaDatabase",
                ),
                (
                    ncm_commands::VERIFY_CONTENT_STORAGE,
                    None,
                    "VerifyContentStorage",
                ),
                (
                    ncm_commands::VERIFY_CONTENT_META_DATABASE,
                    None,
                    "VerifyContentMetaDatabase",
                ),
                (
                    ncm_commands::OPEN_CONTENT_STORAGE,
                    None,
                    "OpenContentStorage",
                ),
                (
                    ncm_commands::OPEN_CONTENT_META_DATABASE,
                    None,
                    "OpenContentMetaDatabase",
                ),
                (
                    ncm_commands::CLOSE_CONTENT_STORAGE_FORCIBLY,
                    None,
                    "CloseContentStorageForcibly",
                ),
                (
                    ncm_commands::CLOSE_CONTENT_META_DATABASE_FORCIBLY,
                    None,
                    "CloseContentMetaDatabaseForcibly",
                ),
                (
                    ncm_commands::CLEAN_UP_CONTENT_META_DATABASE,
                    None,
                    "CleanupContentMetaDatabase",
                ),
                (
                    ncm_commands::ACTIVATE_CONTENT_STORAGE,
                    None,
                    "ActivateContentStorage",
                ),
                (
                    ncm_commands::INACTIVATE_CONTENT_STORAGE,
                    None,
                    "InactivateContentStorage",
                ),
                (
                    ncm_commands::ACTIVATE_CONTENT_META_DATABASE,
                    None,
                    "ActivateContentMetaDatabase",
                ),
                (
                    ncm_commands::INACTIVATE_CONTENT_META_DATABASE,
                    None,
                    "InactivateContentMetaDatabase",
                ),
                (
                    ncm_commands::INVALIDATE_RIGHTS_ID_CACHE,
                    None,
                    "InvalidateRightsIdCache",
                ),
                (ncm_commands::GET_MEMORY_REPORT, None, "GetMemoryReport"),
                (
                    ncm_commands::ACTIVATE_FS_CONTENT_STORAGE,
                    None,
                    "ActivateFsContentStorage",
                ),
            ]),
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl_service_framework!(NCM, "ncm");

/// Registers "lr" and "ncm" services.
///
/// Corresponds to `LoopProcess` in upstream `ncm.cpp`.
pub fn loop_process(system: crate::core::SystemRef) {
    use crate::hle::service::server_manager::ServerManager;

    let server_manager = ServerManager::new_shared(system);

    {
        let mut server_manager = server_manager.lock().unwrap();
        server_manager.register_named_service(
            "lr",
            Box::new(|| -> SessionRequestHandlerPtr { std::sync::Arc::new(LR::new()) }),
            64,
        );
        server_manager.register_named_service(
            "ncm",
            Box::new(|| -> SessionRequestHandlerPtr { std::sync::Arc::new(NCM::new()) }),
            64,
        );
    }

    ServerManager::run_server_shared(server_manager);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ncm_service_tables_match_upstream_command_counts() {
        assert_eq!(ILocationResolver::new(0).handlers.len(), 20);
        assert_eq!(IRegisteredLocationResolver::new().handlers.len(), 10);
        assert_eq!(IAddOnContentLocationResolver::new().handlers.len(), 5);
        assert_eq!(LR::new().handlers.len(), 4);
        assert_eq!(NCM::new().handlers.len(), 16);
    }

    #[test]
    fn location_resolver_preserves_storage_id() {
        assert_eq!(ILocationResolver::new(3).storage(), 3);
    }
}
