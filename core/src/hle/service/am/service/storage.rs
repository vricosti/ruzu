// SPDX-FileCopyrightText: Copyright 2024 yuzu Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

//! Port of zuyu/src/core/hle/service/am/service/storage.h
//! Port of zuyu/src/core/hle/service/am/service/storage.cpp

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use crate::hle::result::{ResultCode, RESULT_SUCCESS};
use crate::hle::service::am::am_results;
use crate::hle::service::am::library_applet_storage::{
    BufferLibraryAppletStorage, LibraryAppletStorage,
};
use crate::hle::service::hle_ipc::{HLERequestContext, SessionRequestHandler};
use crate::hle::service::ipc_helpers::ResponseBuilder;
use crate::hle::service::service::{build_handler_map, FunctionInfo, ServiceFramework};

use super::storage_accessor::{IStorageAccessor, ITransferStorageAccessor};

/// IPC command table for IStorage:
/// - 0: Open
/// - 1: OpenTransferStorage
pub struct IStorage {
    /// Backing storage implementation.
    /// Matches upstream `std::shared_ptr<LibraryAppletStorage> m_impl`.
    backing: Arc<Mutex<dyn LibraryAppletStorage>>,
    handlers: BTreeMap<u32, FunctionInfo>,
    handlers_tipc: BTreeMap<u32, FunctionInfo>,
}

impl IStorage {
    pub fn new(data: Vec<u8>) -> Self {
        let backing: Arc<Mutex<dyn LibraryAppletStorage>> =
            Arc::new(Mutex::new(BufferLibraryAppletStorage::new(data)));
        let handlers = build_handler_map(&[
            (0, Some(Self::open_handler), "Open"),
            (
                1,
                Some(Self::open_transfer_storage_handler),
                "OpenTransferStorage",
            ),
        ]);
        Self {
            backing,
            handlers,
            handlers_tipc: BTreeMap::new(),
        }
    }

    pub fn get_data(&self) -> Vec<u8> {
        self.backing.lock().unwrap().get_data()
    }

    fn push_interface_response(
        ctx: &mut HLERequestContext,
        object: Arc<dyn SessionRequestHandler>,
    ) {
        let is_domain = ctx
            .get_manager()
            .map_or(false, |manager| manager.lock().unwrap().is_domain());
        let move_handle = if is_domain {
            0
        } else {
            ctx.create_session_for_service(object.clone()).unwrap_or(0)
        };
        let mut rb = ResponseBuilder::new(ctx, 2, 0, 1);
        rb.push_result(RESULT_SUCCESS);
        if is_domain {
            ctx.add_domain_object(object);
        } else {
            rb.push_move_objects(move_handle);
        }
    }

    /// Port of IStorage::Open
    /// Creates an IStorageAccessor from this storage and returns it.
    /// Upstream: `R_UNLESS(m_impl->GetHandle() == nullptr, AM::ResultInvalidStorageType);`
    fn open_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let storage = unsafe { &*(this as *const dyn ServiceFramework as *const IStorage) };
        log::debug!("IStorage::Open called");

        // Check that the backing storage does not have a transfer memory handle.
        // If it does, Open is invalid — use OpenTransferStorage instead.
        let has_handle = storage.backing.lock().unwrap().has_handle();
        if has_handle {
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(am_results::RESULT_INVALID_STORAGE_TYPE);
            return;
        }

        let accessor = Arc::new(IStorageAccessor::new(storage.backing.clone()));
        Self::push_interface_response(ctx, accessor);
    }

    /// Port of IStorage::OpenTransferStorage
    /// Creates an ITransferStorageAccessor and returns it.
    /// Upstream: `R_UNLESS(m_impl->GetHandle() != nullptr, AM::ResultInvalidStorageType);`
    fn open_transfer_storage_handler(this: &dyn ServiceFramework, ctx: &mut HLERequestContext) {
        let storage = unsafe { &*(this as *const dyn ServiceFramework as *const IStorage) };
        log::debug!("IStorage::OpenTransferStorage called");

        // Check that the backing storage has a transfer memory handle.
        let has_handle = storage.backing.lock().unwrap().has_handle();
        if !has_handle {
            let mut rb = ResponseBuilder::new(ctx, 2, 0, 0);
            rb.push_result(am_results::RESULT_INVALID_STORAGE_TYPE);
            return;
        }

        let accessor = Arc::new(ITransferStorageAccessor::new(storage.backing.clone()));
        Self::push_interface_response(ctx, accessor);
    }
}

impl SessionRequestHandler for IStorage {
    fn handle_sync_request(&self, ctx: &mut HLERequestContext) -> ResultCode {
        ServiceFramework::handle_sync_request_impl(self, ctx)
    }
}

impl ServiceFramework for IStorage {
    fn get_service_name(&self) -> &str {
        "am::IStorage"
    }

    fn handlers(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers
    }

    fn handlers_tipc(&self) -> &BTreeMap<u32, FunctionInfo> {
        &self.handlers_tipc
    }
}
