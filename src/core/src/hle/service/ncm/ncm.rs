// SPDX-FileCopyrightText: Copyright 2018 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/ncm/ncm.cpp
//!
//! NCM and LR services.

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use crate::core::SystemRef;
use crate::file_sys::nca_metadata::ContentRecordType;
use crate::file_sys::registered_cache::{ContentProvider, NcaId, PlaceholderCache};
use crate::file_sys::romfs_factory::StorageId;
use crate::hle::result::{ResultCode, RESULT_SUCCESS, RESULT_UNKNOWN};
use crate::hle::service::hle_ipc::{
    HLERequestContext, SessionRequestHandler, SessionRequestHandlerPtr,
};
use crate::hle::service::ipc_helpers::{RequestParser, ResponseBuilder};
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

pub mod content_storage_commands {
    pub const GENERATE_PLACE_HOLDER_ID: u32 = 0;
    pub const CREATE_PLACE_HOLDER: u32 = 1;
    pub const DELETE_PLACE_HOLDER: u32 = 2;
    pub const WRITE_PLACE_HOLDER: u32 = 4;
    pub const REGISTER: u32 = 5;
    pub const DELETE: u32 = 6;
}

pub mod content_meta_database_commands {
    pub const SET: u32 = 0;
    pub const REMOVE: u32 = 2;
    pub const HAS: u32 = 8;
    pub const COMMIT: u32 = 15;
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

pub struct IContentStorage {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    system: SystemRef,
    storage: u8,
}

impl IContentStorage {
    pub fn new(system: SystemRef, storage: u8) -> Self {
        Self {
            handlers: build_handler_map(&[
                (
                    content_storage_commands::GENERATE_PLACE_HOLDER_ID,
                    Some(Self::generate_place_holder_id_handler),
                    "GeneratePlaceHolderId",
                ),
                (
                    content_storage_commands::CREATE_PLACE_HOLDER,
                    Some(Self::create_place_holder_handler),
                    "CreatePlaceHolder",
                ),
                (
                    content_storage_commands::DELETE_PLACE_HOLDER,
                    Some(Self::delete_place_holder_handler),
                    "DeletePlaceHolder",
                ),
                (
                    content_storage_commands::WRITE_PLACE_HOLDER,
                    Some(Self::write_place_holder_handler),
                    "WritePlaceHolder",
                ),
                (
                    content_storage_commands::REGISTER,
                    Some(Self::register_handler),
                    "Register",
                ),
                (
                    content_storage_commands::DELETE,
                    Some(Self::delete_handler),
                    "Delete",
                ),
            ]),
            handlers_tipc: BTreeMap::new(),
            system,
            storage,
        }
    }

    fn storage_id(&self) -> Option<StorageId> {
        match self.storage {
            0 => Some(StorageId::None),
            1 => Some(StorageId::Host),
            2 => Some(StorageId::GameCard),
            3 => Some(StorageId::NandSystem),
            4 => Some(StorageId::NandUser),
            5 => Some(StorageId::SdCard),
            _ => None,
        }
    }

    fn generate_place_holder_id(&self) -> NcaId {
        log::debug!("IContentStorage::GeneratePlaceHolderId called");
        PlaceholderCache::generate()
    }

    fn create_place_holder(&self, placeholder_id: NcaId, size: i64) -> bool {
        let succeeded = self.storage_id().is_some_and(|storage| {
            let controller = self.system.get().get_filesystem_controller();
            let controller = controller.lock().unwrap();
            controller
                .get_placeholder_cache_for_storage(storage)
                .is_some_and(|cache| {
                    size >= 0
                        && (cache.exists(&placeholder_id)
                            || cache.create(&placeholder_id, size as u64))
                })
        });
        if succeeded {
            log::debug!(
                "IContentStorage::CreatePlaceHolder called, storage_id={}, size={}",
                self.storage,
                size
            );
        } else {
            log::warn!(
                "IContentStorage::CreatePlaceHolder failed, storage_id={}, size={}",
                self.storage,
                size
            );
        }
        succeeded
    }

    fn delete_place_holder(&self, placeholder_id: NcaId) -> bool {
        let succeeded = self.storage_id().is_some_and(|storage| {
            let controller = self.system.get().get_filesystem_controller();
            let controller = controller.lock().unwrap();
            controller
                .get_placeholder_cache_for_storage(storage)
                .is_some_and(|cache| {
                    !cache.exists(&placeholder_id) || cache.delete_placeholder(&placeholder_id)
                })
        });
        if succeeded {
            log::debug!(
                "IContentStorage::DeletePlaceHolder called, storage_id={}",
                self.storage
            );
        } else {
            log::warn!(
                "IContentStorage::DeletePlaceHolder failed, storage_id={}",
                self.storage
            );
        }
        succeeded
    }

    fn write_place_holder(&self, placeholder_id: NcaId, offset: u64, data: &[u8]) -> bool {
        let succeeded = self.storage_id().is_some_and(|storage| {
            let controller = self.system.get().get_filesystem_controller();
            let controller = controller.lock().unwrap();
            controller
                .get_placeholder_cache_for_storage(storage)
                .is_some_and(|cache| cache.write(&placeholder_id, offset, data))
        });
        if succeeded {
            log::debug!(
                "IContentStorage::WritePlaceHolder called, storage_id={}, offset={}, size={}",
                self.storage,
                offset,
                data.len()
            );
        } else {
            log::warn!(
                "IContentStorage::WritePlaceHolder failed, storage_id={}, offset={}, size={}",
                self.storage,
                offset,
                data.len()
            );
        }
        succeeded
    }

    fn register(&self, placeholder_id: NcaId, content_id: NcaId) -> bool {
        let succeeded = self.storage_id().is_some_and(|storage| {
            let controller = self.system.get().get_filesystem_controller();
            let mut controller = controller.lock().unwrap();
            let placeholder = controller
                .get_placeholder_cache_for_storage(storage)
                .map(|cache| cache as *const PlaceholderCache);
            let registered = controller.get_registered_cache_for_storage(storage);
            match (placeholder, registered) {
                // SAFETY: for every StorageId, FileSystemController stores the
                // placeholder and registered caches in distinct fields. The raw
                // pointer only keeps the immutable placeholder borrow alive while
                // Rust grants mutable access to the sibling registered cache.
                (Some(placeholder), Some(registered)) => unsafe {
                    (*placeholder).register(registered, &placeholder_id, &content_id)
                },
                _ => false,
            }
        });
        if succeeded {
            log::debug!(
                "IContentStorage::Register called, storage_id={}",
                self.storage
            );
        } else {
            log::warn!(
                "IContentStorage::Register failed, storage_id={}",
                self.storage
            );
        }
        succeeded
    }

    fn delete(&self, content_id: NcaId) -> bool {
        let succeeded = self.storage_id().is_some_and(|storage| {
            let controller = self.system.get().get_filesystem_controller();
            let mut controller = controller.lock().unwrap();
            let Some(cache) = controller.get_registered_cache_for_storage(storage) else {
                return false;
            };
            let succeeded = cache.delete(&content_id);
            if succeeded {
                cache.refresh();
            }
            succeeded
        });
        if succeeded {
            log::debug!(
                "IContentStorage::Delete called, storage_id={}",
                self.storage
            );
        } else {
            log::warn!(
                "IContentStorage::Delete failed, storage_id={}",
                self.storage
            );
        }
        succeeded
    }

    fn generate_place_holder_id_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let id = this.generate_place_holder_id();
        let mut rb = ResponseBuilder::new(ctx, 6, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_raw(&id);
    }

    fn create_place_holder_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let mut rp = RequestParser::new(ctx);
        let (placeholder_id, _content_id) = if crate::hle::api_version::HOS_VERSION_MAJOR >= 16 {
            (rp.pop_raw::<NcaId>(), rp.pop_raw::<NcaId>())
        } else {
            let content_id = rp.pop_raw::<NcaId>();
            (rp.pop_raw::<NcaId>(), content_id)
        };
        let size = rp.pop_i64();
        let result = if this.create_place_holder(placeholder_id, size) {
            RESULT_SUCCESS
        } else {
            RESULT_UNKNOWN
        };
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    fn delete_place_holder_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let mut rp = RequestParser::new(ctx);
        let placeholder_id = rp.pop_raw::<NcaId>();
        let result = if this.delete_place_holder(placeholder_id) {
            RESULT_SUCCESS
        } else {
            RESULT_UNKNOWN
        };
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    fn write_place_holder_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let mut rp = RequestParser::new(ctx);
        let placeholder_id = rp.pop_raw::<NcaId>();
        let offset = rp.pop_u64();
        let data = ctx.read_buffer(0);
        let result = if this.write_place_holder(placeholder_id, offset, &data) {
            RESULT_SUCCESS
        } else {
            RESULT_UNKNOWN
        };
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    fn register_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let mut rp = RequestParser::new(ctx);
        let (placeholder_id, content_id) = if crate::hle::api_version::HOS_VERSION_MAJOR >= 16 {
            (rp.pop_raw::<NcaId>(), rp.pop_raw::<NcaId>())
        } else {
            let content_id = rp.pop_raw::<NcaId>();
            (rp.pop_raw::<NcaId>(), content_id)
        };
        let result = if this.register(placeholder_id, content_id) {
            RESULT_SUCCESS
        } else {
            RESULT_UNKNOWN
        };
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }

    fn delete_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let mut rp = RequestParser::new(ctx);
        let content_id = rp.pop_raw::<NcaId>();
        let result = if this.delete(content_id) {
            RESULT_SUCCESS
        } else {
            RESULT_UNKNOWN
        };
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(result);
    }
}

impl_service_framework!(IContentStorage, "IContentStorage");

#[derive(Clone, Copy, Default)]
#[repr(C)]
struct ContentMetaKey {
    id: u64,
    version: u32,
    type_: u8,
    install_type: u8,
    padding: [u8; 2],
}

impl ContentMetaKey {
    fn matches(&self, other: &Self) -> bool {
        self.id == other.id
            && self.version == other.version
            && self.type_ == other.type_
            && self.install_type == other.install_type
    }
}

pub struct IContentMetaDatabase {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
    system: SystemRef,
    storage: u8,
    entries: Mutex<Vec<ContentMetaKey>>,
}

impl IContentMetaDatabase {
    pub fn new(system: SystemRef, storage: u8) -> Self {
        Self {
            handlers: build_handler_map(&[
                (
                    content_meta_database_commands::SET,
                    Some(Self::set_handler),
                    "Set",
                ),
                (
                    content_meta_database_commands::REMOVE,
                    Some(Self::remove_handler),
                    "Remove",
                ),
                (
                    content_meta_database_commands::HAS,
                    Some(Self::has_handler),
                    "Has",
                ),
                (
                    content_meta_database_commands::COMMIT,
                    Some(Self::commit_handler),
                    "Commit",
                ),
            ]),
            handlers_tipc: BTreeMap::new(),
            system,
            storage,
            entries: Mutex::new(Vec::new()),
        }
    }

    fn storage_id(&self) -> Option<StorageId> {
        match self.storage {
            0 => Some(StorageId::None),
            1 => Some(StorageId::Host),
            2 => Some(StorageId::GameCard),
            3 => Some(StorageId::NandSystem),
            4 => Some(StorageId::NandUser),
            5 => Some(StorageId::SdCard),
            _ => None,
        }
    }

    fn set(&self, key: ContentMetaKey) {
        let mut entries = self.entries.lock().unwrap();
        if !entries.iter().any(|entry| entry.matches(&key)) {
            entries.push(key);
        }
        log::debug!(
            "IContentMetaDatabase::Set called, storage_id={}, title_id={:016X}, version={}, type={}",
            self.storage,
            key.id,
            key.version,
            key.type_
        );
    }

    fn remove(&self, key: ContentMetaKey) {
        self.entries
            .lock()
            .unwrap()
            .retain(|entry| !entry.matches(&key));
        log::debug!(
            "IContentMetaDatabase::Remove called, storage_id={}, title_id={:016X}, version={}, type={}",
            self.storage,
            key.id,
            key.version,
            key.type_
        );
    }

    fn has(&self, key: ContentMetaKey) -> bool {
        let has_pending = self
            .entries
            .lock()
            .unwrap()
            .iter()
            .any(|entry| entry.matches(&key));
        let has_registered = self.storage_id().is_some_and(|storage| {
            let controller = self.system.get().get_filesystem_controller();
            let mut controller = controller.lock().unwrap();
            controller
                .get_registered_cache_for_storage(storage)
                .is_some_and(|cache| cache.has_entry(key.id, ContentRecordType::Meta))
        });
        let has = has_pending || has_registered;
        log::debug!(
            "IContentMetaDatabase::Has called, storage_id={}, title_id={:016X}, version={}, type={}, has={}",
            self.storage,
            key.id,
            key.version,
            key.type_,
            has
        );
        has
    }

    fn commit(&self) {
        if let Some(storage) = self.storage_id() {
            let controller = self.system.get().get_filesystem_controller();
            let mut controller = controller.lock().unwrap();
            if let Some(cache) = controller.get_registered_cache_for_storage(storage) {
                cache.refresh();
            }
        }
        log::debug!(
            "IContentMetaDatabase::Commit called, storage_id={}",
            self.storage
        );
    }

    fn set_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let mut rp = RequestParser::new(ctx);
        this.set(rp.pop_raw::<ContentMetaKey>());
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn remove_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let mut rp = RequestParser::new(ctx);
        this.remove(rp.pop_raw::<ContentMetaKey>());
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }

    fn has_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let mut rp = RequestParser::new(ctx);
        let has = this.has(rp.pop_raw::<ContentMetaKey>());
        let mut rb = ResponseBuilder::new(ctx, 3, 0, 0);
        rb.push_result(RESULT_SUCCESS);
        rb.push_bool(has);
    }

    fn commit_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        this.commit();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
        rb.push_result(RESULT_SUCCESS);
    }
}

impl_service_framework!(IContentMetaDatabase, "IContentMetaDatabase");

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
    system: SystemRef,
}

impl NCM {
    pub fn new(system: SystemRef) -> Self {
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
                    Some(Self::open_content_storage_handler),
                    "OpenContentStorage",
                ),
                (
                    ncm_commands::OPEN_CONTENT_META_DATABASE,
                    Some(Self::open_content_meta_database_handler),
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
            system,
        }
    }

    fn open_content_storage(&self, storage: u8) -> IContentStorage {
        log::debug!("NCM::OpenContentStorage called, storage_id={}", storage);
        IContentStorage::new(self.system, storage)
    }

    fn open_content_meta_database(&self, storage: u8) -> IContentMetaDatabase {
        log::debug!(
            "NCM::OpenContentMetaDatabase called, storage_id={}",
            storage
        );
        IContentMetaDatabase::new(self.system, storage)
    }

    fn open_content_storage_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let mut rp = RequestParser::new(ctx);
        let storage = rp.pop_u8();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        rb.push_ipc_interface(Arc::new(this.open_content_storage(storage)));
    }

    fn open_content_meta_database_handler(
        this: &dyn ServiceFramework,
        ctx: &mut HLERequestContext,
    ) {
        let this = unsafe { &*(this as *const dyn ServiceFramework as *const Self) };
        let mut rp = RequestParser::new(ctx);
        let storage = rp.pop_u8();
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        rb.push_ipc_interface(Arc::new(this.open_content_meta_database(storage)));
    }
}

impl_service_framework!(NCM, "ncm");

pub struct NcmV {
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl NcmV {
    pub fn new() -> Self {
        Self {
            handlers: build_handler_map(&[(0, None, "GetSystemVersion")]),
            handlers_tipc: BTreeMap::new(),
        }
    }
}

impl_service_framework!(NcmV, "ncm:v");

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
            Box::new(move || -> SessionRequestHandlerPtr { std::sync::Arc::new(NCM::new(system)) }),
            64,
        );
        // Upstream deliberately uses `if (1 /* not retail */)` here.
        server_manager.register_named_service(
            "ncm:v",
            Box::new(|| -> SessionRequestHandlerPtr { std::sync::Arc::new(NcmV::new()) }),
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
        assert_eq!(NCM::new(SystemRef::null()).handlers.len(), 16);
        assert_eq!(NcmV::new().handlers.len(), 1);
    }

    #[test]
    fn location_resolver_preserves_storage_id() {
        assert_eq!(ILocationResolver::new(3).storage(), 3);
    }

    #[test]
    fn ncm_content_interfaces_match_upstream_tables_and_layout() {
        let system = SystemRef::null();
        let content_storage = IContentStorage::new(system, 3);
        let content_meta_database = IContentMetaDatabase::new(system, 3);
        let ncm = NCM::new(system);
        assert_eq!(content_storage.handlers.len(), 6);
        assert_eq!(content_meta_database.handlers.len(), 4);
        assert_eq!(core::mem::size_of::<ContentMetaKey>(), 0x10);
        assert!(ncm
            .handlers
            .get(&ncm_commands::OPEN_CONTENT_STORAGE)
            .unwrap()
            .handler_callback
            .is_some());
        assert!(ncm
            .handlers
            .get(&ncm_commands::OPEN_CONTENT_META_DATABASE)
            .unwrap()
            .handler_callback
            .is_some());
        let implemented_handlers = content_storage
            .handlers
            .values()
            .chain(content_meta_database.handlers.values())
            .chain(ncm.handlers.values())
            .filter(|function| function.handler_callback.is_some())
            .count();
        assert_eq!(implemented_handlers, 12);
    }

    #[test]
    fn content_meta_pending_entries_ignore_padding_like_upstream() {
        let service = IContentMetaDatabase::new(SystemRef::null(), 3);
        let first = ContentMetaKey {
            id: 0x0100_0000_0000_1000,
            version: 7,
            type_: 0x80,
            install_type: 1,
            padding: [1, 2],
        };
        let mut same_key = first;
        same_key.padding = [9, 9];
        service.set(first);
        service.set(same_key);
        assert_eq!(service.entries.lock().unwrap().len(), 1);
        service.remove(same_key);
        assert!(service.entries.lock().unwrap().is_empty());
    }
}
